# packages 
library(conflicted)
library(doParallel)
library(foreach)
library(GIFT)
library(lcvplants)
library(sf)
library(stringr)
library(tidyverse)
library(units)

# Note:
# the entire process takes a long time. Save intermediate outputs when necessary
# code lines for saving already provided as comment

# preamble ----------------------------------------------------------------

rm(list = ls())

filter <- dplyr::filter

# source for functions
source("scripts/functions.RData")



# load data ---------------------------------------------------------------

load("data/PaciFLora_species_list.RData")
specs <- species_names$species_changed
rm(species_names)

load("data/occ_cleaned_slim.RData")



# get GIFT names ----------------------------------------------------------

# compare the species from the Paciflora species list (based on LCVP) with the names used for GIFT

# prepare empty df to store info
GIFT_names <- data.frame(searched_name = character(),
                         GIFT_genus = character(),
                         GIFT_species_ep = character(),
                         stringsAsFactors = FALSE)

# run loop over species
for (spec in specs) {
  
  GIFT_names <- bind_rows(GIFT_names,
                          getGiftNames(spec, incl_lcvp_synonyms = TRUE))
  
} # end of for loop over specs

# sometimes, faulty internet connections can cause NA assignments

# identify species with NA and re-run the above code for these
specs_NA <- specs[which(is.na(GIFT_names$GIFT_genus))]

# prepare empty df to store info

GIFT_names_NA <- data.frame(searched_name = character(),
                            GIFT_genus = character(),
                            GIFT_species_ep = character(),
                            stringsAsFactors = FALSE)

# run loop over species
for (spec in specs_NA) {
  
  GIFT_names_NA <- bind_rows(GIFT_names_NA,
                             getGiftNames(spec, incl_lcvp_synonyms = TRUE))
  
} # end of for loop over specs_NA

# bind info together
GIFT_names <- rbind(GIFT_names, GIFT_names_NA) %>%
  na.omit() # removes duplicates and remaining NAs

# save(GIFT_names, file = "data/status_assignment/GIFT_names.RData")


# get GIFT status ---------------------------------------------------------

GIFT_status <- vector("list", length = nrow(GIFT_names))

for (s in 1:nrow(GIFT_names)) {
  print(s)
  GIFT_status[[s]] <- getGiftStatusInf(searched_name = GIFT_names$searched_name[s],
                                       GIFT_spec_genus = GIFT_names$GIFT_genus[s],
                                       GIFT_spec_epithet = GIFT_names$GIFT_species_ep[s])
}

# sometimes, no status information is returned, these empty list elements cause errors and have to be removed
list_index <- NULL
for (s in 1:length(GIFT_status)) {
  
  # collect indices of list element without GIFT information (empty df)
  if (nrow(GIFT_status[[s]]) == 0) {list_index <- c(list_index, s)}
  
} # end of loop over list indices

# remove empty elements from list
GIFT_status <- GIFT_status[-list_index]

GIFT_status_all_details <- bind_rows(GIFT_status)

#rename the statuses based on the distinct combinations
# based on: https://biogeomacro.github.io/GIFT/articles/GIFT_tutorial.html#species-distribution

GIFT_status <- GIFT_status_all_details %>%
  mutate(status = case_when(
    native == "native" & naturalized == "non-naturalized" ~ "native",
    native == "native" & is.na(naturalized) ~ "native",
    native == "non-native" & is.na(naturalized) ~ "non-native",
    native == "non-native" & naturalized == "naturalized" ~ "naturalized",
    native == "non-native" & naturalized == "non-naturalized" ~ "non-native",
    is.na(native) & is.na(naturalized) ~ "unknown"
  )) %>%
  select(species, GIFT_species, entity_ID, status)

# save(GIFT_status, file = "data/status_assignment/GIFT_status.RData")


# load spatial data of the GIFT regions with status information for the considered species:
GIFT_polygons <- GIFT_shape(unique(GIFT_status$entity_ID), GIFT_version = "beta")

# save(GIFT_polygons, file = "data/status_assignment/Gift_polygons.RData")

# merge GIFT status with occurrences --------------------------------------

# get number of species for which GIFT status was available
specs_gift <- unique(GIFT_status$species)

no_cores <- 2
cl <- makeCluster(no_cores)
registerDoParallel(cl)

occ_GIFT_status <- foreach(s = 1:length(specs_gift), .packages = c("dplyr", "sf"),
                           .combine = "rbind", .verbose = TRUE) %dopar% {
                             
                             
                             # GIFT status data for species s:
                             GIFT_status_spec <- GIFT_status %>%
                               filter(species == specs_gift[s])
                             
                             # GIFT polygon IDs:
                             polygon_IDs_spec <- GIFT_status_spec$entity_ID
                             
                             # GIFT polygons as sf:
                             GIFT_polygons_spec <- GIFT_polygons %>%
                               filter(entity_ID %in% polygon_IDs_spec)
                             
                             # occurrences of species s:
                             # subset occurrences for s
                             occ_sf_spec <-  occ_cleaned_slim %>%
                               filter(species == specs_gift[s]) %>%
                               st_as_sf(coords = c("lon", "lat"), crs = st_crs(tdwg))
                             
                             
                             sf_use_s2(FALSE) # switch off spherical geometry package S2
                             
                             # spatially join occurrences and polygons:
                             occ_GIFT_poly_spec <- occ_sf_spec %>%
                               select(occ_id, species) %>%
                               st_join(GIFT_polygons_spec, st_intersects, left = TRUE) %>% # occurrence must be inside the polygon
                               st_drop_geometry()
                             
                             if (nrow(occ_GIFT_poly_spec) == 0) return(occ_GIFT_poly_spec) # no occurrences for the species
                             
                             # additionally match occurrences not inside any GIFT region to the closest GIFT region
                             # with status information, if it is <= 10 km away:
                             
                             # occurrence ID not joined:
                             occ_ID_no_Gift_poly <- occ_GIFT_poly_spec %>%
                               filter(is.na(entity_ID)) %>%
                               pull(occ_id)
                             
                             if (length(occ_ID_no_Gift_poly) != 0) {
                               
                               # occurrences not joined as sf :
                               occ_sf_spec_no_Gift_poly <- occ_sf_spec %>%
                                 filter(occ_id %in% occ_ID_no_Gift_poly)
                               
                               # nearest GIFT region for each of these occurrences:
                               nearest_GIFT_region <- st_nearest_feature(x = occ_sf_spec_no_Gift_poly, y = GIFT_polygons_spec, longlat = TRUE)
                               nearest_GIFT_region_sf <- GIFT_polygons_spec[nearest_GIFT_region,]  %>%
                                 cbind("occ_id" = occ_sf_spec_no_Gift_poly$occ_id)
                               
                               # distance of each occurrence to nearest GIFT region:
                               dist_nearest_GIFT_region <- st_distance(occ_sf_spec_no_Gift_poly,
                                                                       nearest_GIFT_region_sf,
                                                                       by_element = TRUE,
                                                                       tolerance = units::set_units(10000, m))
                               
                               # keep matched regions if distance is <= 10km:
                               occ_GIFT_max10kmdist <- nearest_GIFT_region_sf %>%
                                 mutate(distance_m = dist_nearest_GIFT_region) %>%
                                 filter(distance_m <= units::set_units(10000, m)) %>%
                                 st_drop_geometry() %>%
                                 select(-distance_m)
                               
                               # join to all occurrences:
                               occ_GIFT_poly_spec_max10kmdist <- occ_GIFT_poly_spec %>%
                                 left_join(occ_GIFT_max10kmdist, by = "occ_id") %>%
                                 mutate(entity_ID = ifelse(!is.na(entity_ID.x), entity_ID.x, entity_ID.y)) %>%
                                 mutate(geo_entity = ifelse(!is.na(geo_entity.x), geo_entity.x, geo_entity.y)) %>%
                                 mutate(area = ifelse(!is.na(area.x), area.x, area.y)) %>%
                                 mutate(polygon_source = ifelse(!is.na(polygon_source.x), polygon_source.x, polygon_source.y)) %>%
                                 select(occ_id, species, entity_ID, geo_entity, area, polygon_source)
                               
                             } else {occ_GIFT_poly_spec_max10kmdist <- occ_GIFT_poly_spec %>%
                               select(occ_id, species, entity_ID, geo_entity, area, polygon_source)
                             }
                             
                             # join status information to GIFT regions:
                             occ_GIFT_status_spec <- occ_GIFT_poly_spec_max10kmdist %>%
                               left_join(GIFT_status_spec)
                             
                             # since polygons are nested, more than 1 polygon may be joined to a single occurrence (e.g. Paraguay and Southern South America)
                             # in the following, for each occurrence the status information belonging to the smaller polygon is used:
                             occ_GIFT_status_spec_final <- occ_GIFT_status_spec %>%
                               group_by(occ_id) %>%
                               arrange(area, .by_group = TRUE) %>%
                               slice(1) %>%
                               ungroup
                           }

stopCluster(cl)

save(occ_GIFT_status, file = "data/status_assignment/occ_GIFT_status.RData")
