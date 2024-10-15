#' ---------------------------
#
# Purpose of script: merging BIEN and GBIF occurrence data + data cleaning
# Author: Christian König, Anna Rönnfeldt, Katrin Schifferle
# Date Created: 2021, revised 2023 by Anna Rönnfeldt
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------


# preamble
rm(list = ls())

# required packages ------------------------------------------------------------
# set up to run on HPC

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org', dep = TRUE)
  require(x, character.only = TRUE)
}
package_vec <- c(
  "CoordinateCleaner", "countrycode", "tidyverse"  # names of the packages required placed here as character objects
)

# install.packages(c("pillar", "phangorn")) # install packages that caused namespace errors before running the previously defined function
sapply(package_vec, install.load.package)



# required paths ------------------------------------------------------------------------
path_import <- file.path("/import","ecoc9", "data-zurell", "roennfeldt", "C1")

# load and prep data -----------------------------------------------------------

# BIEN data
files <- list.files(file.path(path_import, "download_bien"), ignore.case = FALSE, full.names = TRUE)
occ_bien <- map_dfr(files, function(file){load(file); return(occ_df)}) # 6,563,816 records
length(unique(occ_bien$species)) #  3655 species
spp_freq_bien <- occ_bien %>%
  group_by(species) %>%
  tally() %>%
  arrange(desc(n)) # number of records per species

# GBIF data
files <- list.files(file.path(path_import, "download_gbif"), ignore.case = FALSE, full.names = TRUE)
occ_gbif <- map_dfr(files, function(file){load(file); return(occ_df)}) # 30,032,210 occurrences (instead of 25,886,049 occurrences)
length(unique(occ_gbif$species))# 3740 unique species
spp_freq_gbif <- occ_gbif %>%
  group_by(species) %>%
  tally() %>%
  arrange(desc(n))


# free up memory
rm(files, spp_freq_bien, spp_freq_gbif)

# harmonise columns
occ_bien_std <- occ_bien %>% 
  select(species = "species",
         lat = "latitude",
         lon = "longitude",
         country = "country",
         year = "date_collected",
         datasource = "datasource",
         dataset = "dataset",
         native = "native_status") %>%
  # "Kosovo" and "Micronesia" can not be matched to an ISO3c country code by countrycode()
  # (ISO3c country codes were also used by TDWG to define political countries; used in 4_status_assignment.R)
  # "Micronesia" is in codelist as "Micronesia (Federated States of)" -> adapt this
  # Kosovo has no ISO3c code -> gets country code NA -> think about fixing this
  mutate(country = replace(country, country == "Micronesia", "Micronesia (Federated States of)")) %>% # not in previous ChrK-version!
  mutate(year = lubridate::year(year), 
         country = countrycode(country, origin = "country.name", destination = "iso3c"))


occ_gbif_std = occ_gbif %>% 
  select(species = "species", 
         species_gbif = "species",
         lat = "decimalLatitude",
         lon = "decimalLongitude",
         country = "country",
         year = "year",
         datasource = "institutionCode",
         dataset = "datasetName",
         native = "establishmentMeans",
         coordinate_uncertainty = "coordinateUncertaintyInMeters") %>% 
  mutate(species = word(species, start = 1, end = 2, sep = " ")) %>% # drop the author information in the species column 
  # Kosovo has no ISO3c code -> gets country code NA -> think about fixing this
  # Problem while computing `country = countrycode(country, origin = "country.name", destination = "iso3c")`.
  # Some values were not matched unambiguously: Kosovo, Türkiye, unknown or invalid 
  mutate(country = replace(country, country == "Türkiye", "Turkey")) %>%
  mutate(country = countrycode(country, origin = "country.name", destination = "iso3c"))

# free up memory
rm(occ_bien, occ_gbif) 


# clean data -------------------------------------------------------------------
occ_cleaned <- bind_rows(occ_bien_std, occ_gbif_std) %>% 
  mutate_at(vars(lon, lat), round, 4) %>%             # round to four digits (corresponds to a maximum of 11.13 m at equator)
  dplyr::filter(!(is.na(lat) | is.na(lon)),           # only records with coords
                !(lat == lon | lat == 0 | lon == 0),  # coords should not be equal
                !(year < 1900 | year > 2023),         # no unrealistic years
                (is.na(coordinate_uncertainty) | coordinate_uncertainty < 10000)) %>%  # coordinate precision < 10km 
  arrange(native, coordinate_uncertainty) %>%                                # sort before distinct() to keep the most informative records 
  distinct(species, lon, lat, year, country, datasource, .keep_all = TRUE) %>%  # remove duplicate or redundant records
  clean_coordinates(lon = "lon", lat = "lat", species = "species", countries = "country", 
                    tests = c("centroids", "capitals", "gbif", "institutions"))


# free up memory 
rm(occ_bien_std, occ_gbif_std)


# Final subset and filtering ----

# 26,704,899 occurrences before this step 

occ_cleaned_slim <- occ_cleaned %>% 
  dplyr::filter(.summary == TRUE) %>% # remove occurrences that were flagged by coordinateCleaner
  rowid_to_column(var = "occ_id") %>% # create unique identifier for each occurrence
  dplyr::select(occ_id, species, lon, lat, country, year, datasource, dataset, native) # select only relevant columns


# 38,211,283 occurrences after this step

save(occ_cleaned_slim, file =  file.path(path_import, "occ_cleaned_slim.RData"))


# save unique species names available in occ_cleaned_slim
spp_initial_list <- sort(unique(occ_cleaned_slim$species))
save(spp_initial_list, file = file.path(path_import, "spp_initial_list.RData"))

