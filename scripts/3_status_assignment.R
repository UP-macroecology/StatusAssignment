
# preamble ----------------------------------------------------------------

# packages 
library(conflicted)
library(doParallel)
library(foreach)
library(GIFT)
library(lcvplants)
library(rWCVP) # required data: remotes::install_github('matildabrown/rWCVPdata')
library(sf)
library(stringr)
library(tidyverse)
library(units)

filter <- dplyr::filter

# source for functions
source("scripts/functions.RData")


# load data ---------------------------------------------------------------

load("data/PaciFLora_species_list.RData")
specs <- species_names$species_changed
rm(species_names)

load("data/occ_cleaned_slim.RData")
tdwg <- st_read(paste0(path_import, "input/level3.geojson"))



# WCVP species distributions ----------------------------------------------

# get the list of available wcvp names
wcvp_names <- rWCVPdata::wcvp_names

wcvp_matches <- wcvp_match_names(species_names$species_orig, name_col = "species_changed", progress_bar = TRUE) %>%
  filter(!(multiple_matches == TRUE & wcvp_status != "Accepted"))

wcvp_matches_refined <- wcvp_matches %>%
  filter(wcvp_status == "Accepted") %>%
  filter(match_edit_distance < 3) %>% # fuzzy search output should be less than three characters away from the initil input
  distinct(wcvp_name) # duplicates are a result from different authors name, keep only unique names

wcvp_matches_synonyms <- wcvp_matches %>%
  filter(wcvp_status == "Synonym")

# save synonyms for later
save(wcvp_matches_synonyms, file = "data/status_assignment/wcvp_synonyms.RData")

# check whether filtering arguments where enough (should return FALSE)
# it could be that fewer names are returned from the wcvp search, because not all species work for this funtion
nrow(wcvp_matches_refined) + nrow(wcvp_matches_synonyms) > nrow(species_names)

wcvp_specs <- wcvp_matches_refined$wcvp_name


save(wcvp_specs, file = "data/status_assignment/wcvp_specs.RData")


wcvp_status <- foreach(s = 1:length(wcvp_specs), .packages = c("dplyr"), .combine = "rbind", .verbose = TRUE) %do% {
  
  distribution <- wcvp_distribution(taxon = wcvp_specs[s], taxon_rank = "species",
                                    location_doubtful = FALSE, extinct = FALSE) %>%
    mutate(species = wcvp_specs[s]) %>%
    relocate(species)
  
  # calculate area and make sure it is in km^2 so that it is comparable to the gift polygons
  
  distribution <- distribution %>%
    mutate(area = drop_units(set_units(sf::st_area(distribution), "km^2")))
  
  
  distribution
  
} # end of foreach

save(wcvp_status, file = "data/status_assignment/wcvp_distribution.RData")

rm(distribution)
rm(wcvp_specs)
rm(specs)
rm(wcvp_matches_refined)
rm(wcvp_matches_synonyms)
rm(wcvp_matches)


# merge WCVP status with occurrences --------------------------------------

# get species names for which wcvp_status information is available
specs_wcvp <- unique(wcvp_status$species)

no_cores <- 2
cl <- makeCluster(no_cores)
registerDoParallel(cl)

occ_wcvp_status <- foreach(s = 1:length(specs_wcvp), .packages = c("dplyr", "sf", "units"),
                           .combine = "rbind", .verbose = TRUE) %dopar% { 
                             
                             
                             print(s)
                             
                             # WCVP status data for species s:
                             wcvp_status_spec <- wcvp_status %>%
                               filter(species == specs_wcvp[s])
                             
                             # occurrences of species s:
                             occ_sf_spec <-  occ_cleaned_slim %>%
                               filter(species == specs_wcvp[s]) %>%
                               st_as_sf(coords = c("lon", "lat"), crs = st_crs(tdwg))
                             
                             sf_use_s2(FALSE) # switch off spherical geometry package S2
                             
                             # spatially join occurrences and polygons:
                             occ_wcvp_poly_spec <- occ_sf_spec %>%
                               select(occ_id, species) %>%
                               st_join(wcvp_status_spec, st_intersects, left = TRUE) %>% # occurrence must be inside the polygon
                               st_drop_geometry() 
                             
                             # two species columns are kept(haven't figured out why)
                             # only keep and rename species.x if they are identical
                             
                             if (length(setdiff(occ_wcvp_poly_spec$species.x, occ_wcvp_poly_spec$species.y)) == 0) {
                               
                               occ_wcvp_poly_spec <- occ_wcvp_poly_spec %>%
                                 rename("species" = "species.x") %>%
                                 select(-species.y)
                             }
                             
                             
                             if (nrow(occ_wcvp_poly_spec) == 0) return(occ_wcvp_poly_spec) # no occurrences for the species
                             
                             # additionally match occurrences not inside any WCVP region to the closest WCVP region
                             # with status information, if it is <= 10 km away:
                             
                             # occurrence ID not joined:
                             occ_ID_no_wcvp_poly <- occ_wcvp_poly_spec %>%
                               filter(is.na(LEVEL3_NAM)) %>%
                               pull(occ_id)
                             
                             
                             if (length(occ_ID_no_wcvp_poly) != 0) {
                               
                               # occurrences not joined as sf :
                               occ_sf_spec_no_wcvp_poly <- occ_sf_spec %>%
                                 filter(occ_id %in% occ_ID_no_wcvp_poly)
                               
                               # nearest WCVP region for each of these occurrences:
                               nearest_wcvp_region <- st_nearest_feature(x = occ_sf_spec_no_wcvp_poly, y = wcvp_status_spec, longlat = TRUE)
                               nearest_wcvp_region_sf <- wcvp_status_spec[nearest_wcvp_region,]  %>%
                                 cbind("occ_id" = occ_sf_spec_no_wcvp_poly$occ_id)
                               
                               # distance of each occurrence to nearest GIFT region:
                               dist_nearest_wcvp_region <- st_distance(occ_sf_spec_no_wcvp_poly,
                                                                       nearest_wcvp_region_sf,
                                                                       by_element = TRUE,
                                                                       tolerance = units::set_units(10000, m))
                               
                               # keep matched regions if distance is <= 10km:
                               occ_wcvp_max10kmdist <- nearest_wcvp_region_sf %>%
                                 mutate(distance_m = dist_nearest_wcvp_region) %>%
                                 filter(distance_m <= units::set_units(10000, m)) %>%
                                 st_drop_geometry() %>%
                                 select(-distance_m)
                               
                               # join to all occurrences:
                               occ_wcvp_poly_spec_max10kmdist <- occ_wcvp_poly_spec %>%
                                 left_join(occ_wcvp_max10kmdist, by = c("occ_id", "species")) %>%
                                 mutate(LEVEL3_NAM = ifelse(!is.na(LEVEL3_NAM.x), LEVEL3_NAM.x, LEVEL3_NAM.y)) %>%
                                 mutate(LEVEL3_COD = ifelse(!is.na(LEVEL3_COD.x), LEVEL3_COD.x, LEVEL3_COD.y)) %>%
                                 mutate(LEVEL2_COD = ifelse(!is.na(LEVEL2_COD.x), LEVEL2_COD.x, LEVEL2_COD.y)) %>%
                                 mutate(LEVEL1_COD = ifelse(!is.na(LEVEL1_COD.x), LEVEL1_COD.x, LEVEL1_COD.y)) %>%
                                 mutate(occurrence_type = ifelse(!is.na(occurrence_type.x), occurrence_type.x, occurrence_type.y)) %>%
                                 mutate(area = ifelse(!is.na(area.x), area.x, area.y)) %>%
                                 select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD, area)
                               
                             } else {occ_wcvp_poly_spec_max10kmdist <- occ_wcvp_poly_spec %>%
                               select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD, area)
                             }
                             
                             try(
                               
                               # join status information to WCVP regions:
                               occ_wcvp_status_spec <- occ_wcvp_poly_spec_max10kmdist %>%
                                 left_join(wcvp_status_spec)) # end of try
                             
                             # since polygons are nested, more than 1 polygon may be joined to a single occurrence (e.g. Paraguay and Southern South America)
                             # in the following, for each occurrence the status information belonging to the smaller polygon is used:
                             occ_wcvp_status_spec_final <- occ_wcvp_status_spec %>%
                               select(-geometry) %>%
                               group_by(occ_id) %>%
                               arrange(area, .by_group = TRUE) %>%
                               slice(1) %>%
                               ungroup 
                             
                             
                           } # end of foreach


occ_wcvp_status <- occ_wcvp_status %>%
  select(-c(LEVEL2_COD, LEVEL1_COD))

save(occ_wcvp_status, file = "data/status_assignment/occ_WCVP_status.RData")

stopCluster(cl)


#'##############################################################################
#'##############################################################################
#'##############################################################################

# get GIFT names ----------------------------------------------------------

# compare the species from the Paciflora species list (based on LCVP) with the names used for GIFT

# prepare empty df to store info
GIFT_names <- data.frame(searched_name = character(),
                         GIFT_genus = character(),
                         GIFT_species_ep = character(),
                         stringsAsFactors = FALSE)

# load("data/status_assignment/GIFT_names.RData")

# run loop over species
for (spec in specs_left) {
  
  GIFT_names <- bind_rows(GIFT_names,
                          getGiftNames(spec, incl_lcvp_synonyms = TRUE))
  
} # end of for loop over specs_left

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

save(GIFT_names, file = "data/status_assignment/GIFT_names.RData")


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

save(GIFT_status, file = "data/status_assignment/GIFT_status.RData")


# load spatial data of the GIFT regions with status information for the considered species:
GIFT_polygons <- GIFT_shape(unique(GIFT_status$entity_ID), GIFT_version = "beta")

save(GIFT_polygons, file = "data/status_assignment/Gift_polygons.RData")


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


save(occ_GIFT_status, file = "data/status_assignment/occ_GIFT_status.RData")

stopCluster(cl)



#'##############################################################################
#'##############################################################################
#'##############################################################################


# get GloNAf status -------------------------------------------------------

# GloNAF = Global Naturalized Alien Flora: https://glonaf.org/
# GloNAF contains status information based on TDWG regions, as POWO,
# status information is either "alien" or "naturalized"

# reading the taxon csv-file downloaded from the website didn't work for me due to encoding problems,
# converted csv beforehand to UTF8 encoding using notepad++:
species_dt <- read.delim(file = "data/GloNAF/Taxon_x_List_GloNAF_vanKleunenetal2018Ecology_UTF8.csv")
region_dt <- read.delim(file = "data/GloNAF/Region_GloNAF_vanKleunenetal2018Ecology.csv", sep = ",")
list_dt <- read.delim(file = "data/GloNAF/List_GloNAF_vanKleunenetal2018Ecology.csv", sep = ",")

glonaf_dt <- species_dt %>%
  left_join(region_dt) %>%
  left_join(list_dt) %>%
  mutate(species_name = paste(standardized_name, author), .before = "taxon_orig") %>%
  arrange(species_name)

# free memory:
rm(species_dt)
rm(region_dt)
rm(list_dt)

# subset GloNAF for species names that correspond to target species
glonaf_status_info <- glonaf_dt %>%
  filter(tpl_input %in% specs$species_orig | tpl_input %in% specs$species_changed | tpl_input %in% specs$species_no_x |
           standardized_name %in% specs$species_orig | standardized_name %in% specs$species_changed | standardized_name %in% specs$species_no_x) %>%
  left_join(specs[,c("species_orig", "species_changed", "species_no_x")], by = c("tpl_input" = "species_orig"), keep = TRUE) %>%
  distinct() %>%
  left_join(specs[,c("species_orig", "species_changed", "species_no_x")], by = c("tpl_input" = "species_changed"), keep = TRUE) %>%
  distinct() %>%
  left_join(specs[,c("species_orig", "species_changed", "species_no_x")], by = c("tpl_input" = "species_no_x"), keep = TRUE) %>%
  distinct() %>%
  left_join(specs[,c("species_orig", "species_changed", "species_no_x")], by = c("standardized_name" = "species_orig"), keep = TRUE) %>%
  distinct() %>%
  left_join(specs[,c("species_orig", "species_changed", "species_no_x")], by = c("standardized_name" = "species_changed"), keep = TRUE) %>%
  distinct() %>%
  left_join(specs[,c("species_orig", "species_changed", "species_no_x")], by = c("standardized_name" = "species_no_x"), keep = TRUE) %>%
  distinct() %>%
  mutate(species_orig = coalesce(species_orig.x, species_orig.y, species_orig.x.x, species_orig.y.y, species_orig.x.x.x, species_orig.y.y.y)) %>%
  mutate(species_changed = coalesce(species_changed.x, species_changed.y, species_changed.x.x, species_changed.y.y, species_changed.x.x.x, species_changed.y.y.y)) %>%
  mutate(species_no_x = coalesce(species_no_x.x, species_no_x.y, species_no_x.x.x, species_no_x.y.y, species_no_x.x.x.x, species_no_x.y.y.y)) %>%
  relocate(species_orig, species_changed, species_no_x) %>%
  select(-c(species_orig.x, species_orig.y, species_orig.x.x, species_orig.y.y, species_orig.x.x.x, species_orig.y.y.y,
            species_changed.x, species_changed.y, species_changed.x.x, species_changed.y.y, species_changed.x.x.x, species_changed.y.y.y,
            species_no_x.x, species_no_x.y, species_no_x.x.x, species_no_x.y.y, species_no_x.x.x.x, species_no_x.y.y.y)) %>%
  filter(cultivated_included == 0 | cultivated_included == 3) %>% # 0 & 3: cultivated taxa unlikely to be included (for 2 different reasons), 1 = cultivated may be included, 2 = cultivated likely to be included
  select(species_orig, species_changed, species_no_x, standardized_name, status, tdwg3)


save(glonaf_status_info, file = "data/status_assignment/GloNAF_status.RData")


# glonaf provides no polygons, so the status information will be added to the information form the other two status sources and the species occurrences later on
