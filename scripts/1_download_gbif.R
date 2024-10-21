#' ---------------------------
#
# Purpose of script: downloading occurrence data from GBIF for the target species list
# Author: Christian König, Anna Rönnfeldt, Katrin Schifferle
# Created: 2021, revised by Anna Rönnfeldt in 2023-03
# Email: roennfeldt@uni-potsdam.de
#
# Notes:
# this script is set up to run on a HPC to run the data download in parallel 
# you can also run the script locally by adapting the path to your 
# folder structure, commenting out the cluster section and by changing the 'dopar' 
# in the foreach loop to 'do'

#' ---------------------------


# required packages ------------------------------------------------------------

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org', dep = TRUE)
  require(x, character.only = TRUE)
}
package_vec <- c(
  "doParallel", "foreach", "taxize", "tidyverse", "rgbif"  # names of the packages required placed here as character objects
)

# install.packages(c("pillar", "phangorn")) # install packages that caused namespace errors before running the previously defined function
sapply(package_vec, install.load.package)


# required paths ------------------------------------------------------------------------
path_import <- file.path("/import","ecoc9", "data-zurell", "roennfeldt", "C1")



# define download function ------------------------------------------------


download_species = function(spec_name){
  
  # find gbif_id, prepare download:
  gbif_id = taxize::get_gbifid(spec_name, rank = "species", rows = 1, phylum = "Tracheophyta") %>% 
    as.character()
  n_occ = rgbif::occ_count(taxonKey = gbif_id, georeferenced = TRUE)
  
  if (n_occ == 0) {
    return(NULL)
  }
  
  # Unfortunately, there is a hard limit on the maximum download size via rgbif: 100.000 occurrences 
  # For species with more occurrences, we need to apply additional filters to reduce the download size
  # I used longitudinal bands as additional subset on the occurrence data
  n_blocks = ceiling(n_occ / 50000) # 50000 instead of 100000, to account for uneven geographical distribution
  long_ranges = str_replace_all(levels(cut_interval(-180:180, n_blocks)), "\\[|\\]|\\(|\\)", "") # create longitudinal bands for blocked download
  
  # download data in longitudinal bands:
  download_list = lapply(long_ranges, FUN = function(long_range){  
    download_block = rgbif::occ_data(scientificName = spec_name, 
                                     hasCoordinate = TRUE, 
                                     limit = 100000, 
                                     decimalLongitude = long_range)$data
    if (is.null(download_block)) {
      return(NULL)
    } else {
      return(download_block[,colnames(download_block) %in% c("scientificName", 
                                                             "species", 
                                                             "institutionCode", 
                                                             "datasetName",
                                                             "decimalLatitude", 
                                                             "decimalLongitude", 
                                                             "year", 
                                                             "coordinateUncertaintyInMeters", 
                                                             "issues",
                                                             "geodeticDatum",
                                                             "establishmentMeans",
                                                             "countryCode", 
                                                             "country")])
    } # end of ifelse
  }) # end of lapply
  
  # bind blocked data together
  occ_df = bind_rows(download_list) %>% 
    distinct() %>%
    mutate(species = spec_name, .before = "decimalLatitude") # add the initial input species name
  return(occ_df)
} # end of function


# loop over species and download  -----------------------------------------


# species data 
load(file.path(path_import, "PaciFLora_species_list.RData")) # object is called "species_names"


# collect names of already downloaded species:
inv_specs_dl = list.files(file.path(path_import, "download_gbif")) %>%
  str_remove(".RData") %>% 
  str_replace("_", " ")

# create list of still to-be-downloaded species:
inv_specs_final = setdiff(species_names$species_orig, inv_specs_dl) 

# set up cluster and download species (may be a good idea to run this on the cluster):
cl = makeCluster(20)
registerDoParallel(cl)

# download data
foreach(spec_name = inv_specs_final, .packages = c("tidyverse", "taxize", "rgbif")) %dopar% {
  download_successful = FALSE
  iter = 0
  occ_df = NULL
  while (!(download_successful) & iter < 5) {
    tryCatch({
      occ_df = download_species(spec_name)
      download_successful = TRUE
      if (!is.null(occ_df)) {
        save(occ_df, file = file.path(path_import, "download_gbif", paste0(str_replace_all(spec_name, " ", "_"), ".RData")))  
      }
    }, error = function(e){
      cat("download error:", spec_name, "\n")
      Sys.sleep(15)
      iter <<- iter + 1
    }) # end of tryCatch
  } # end of while
}  # end of foreach

stopCluster(cl)