#' ---------------------------
#
# Purpose of script: downloading occurrence data from BIEN for the target species list
# Author: Christian König, Anna Rönnfeldt, Katrin Schifferle
# Created: 2021, revised by Anna Rönnfeldt in 2023-03
# Email: roennfeldt@uni-potsdam.de
#
# Notes:
# this script is set up to run on a HPC to run the data download in parallel 
# you can also run the script locally by adapting the file path accordingly, commenting out the cluster section and by changing the 'dopar' 
# in the foreach loop to 'do'

#' ---------------------------


# required packages ------------------------------------------------------------

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org', dep = TRUE)
  require(x, character.only = TRUE)
}
package_vec <- c(
  "BIEN", "doParallel", "fasterize", "foreach", "taxize", "tidyverse", "terra"  # names of the packages required placed here as character objects
)

# install.packages(c("pillar", "phangorn")) # install packages that caused namespace errors before running the previously defined function
sapply(package_vec, install.load.package)



# required paths ------------------------------------------------------------------------
path_import <- file.path("")


# define download function ------------------------------------------------

download_species <- function(spec_name){
  # download BIEN occurrence data for 'spec_name'
  occ_df = BIEN_occurrence_species(spec_name, 
                                   natives.only = FALSE, 
                                   cultivated = FALSE, 
                                   native.status = TRUE, 
                                   political.boundaries = TRUE) 
  if (nrow(occ_df) == 0) { # if no occurrences available --> return NULL
    return(NULL)
  } else { # else --> return only relevant columns
    return(occ_df[,colnames(occ_df) %in% c("latitude", 
                                           "longitude", 
                                           "date_collected",
                                           "country", 
                                           "datasource", 
                                           "dataset",
                                           # note that the info on native/introduced status was not used in for the final status assignment
                                           "is_introduced", 
                                           "native_status", 
                                           "native_status_country")] %>%
             mutate(species = spec_name, .before = "latitude")) # end of return
  } # end of ifelse
} # end of function



# loop over species and download ------------------------------------------


# species data 
load(file.path(path_import, "PaciFLora_species_list.RData")) # object is called "species_names"

# collect names of already downloaded (in the import folder) species:
inv_specs_dl <- list.files(file.path(path_import, "download_bien")) %>% 
  str_remove(".RData") %>% 
  str_replace("_", " ")

# create list of still to-be-downloaded species:
inv_specs_final <- setdiff(species_names$species_orig, inv_specs_dl)


# set up cluster and download species:
cl = makeCluster(15)
registerDoParallel(cl)

# download data, retry if not successful:
foreach(spec_index = 1:length(inv_specs_final), .packages = c("tidyverse", "BIEN")) %dopar% {
  
  spec_name <- inv_specs_final[spec_index]
  
  download_successful = FALSE
  iter = 0
  occ_df = NULL
  while (!(download_successful) & iter < 5) {
    tryCatch({  # sometimes the connection to server is unstable, so wrap occurrence download in tryCatch()
      print(spec_name)
      occ_df = download_species(spec_name) # call above-defined download function with current species
      download_successful = TRUE
      save(occ_df, file = file.path(path_import, "download_bien", paste0(str_replace_all(spec_name, " ", "_"), ".RData"))) # save downloaded object
    }, error = function(e){
      cat("download error:", spec_name, "\n")
      Sys.sleep(15)
      iter <<- iter + 1
    }) # end of tryCatch
  } # end of while
}  # end of foreach

stopCluster(cl)
gc()
