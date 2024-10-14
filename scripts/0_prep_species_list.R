#' ---------------------------
#
# Purpose of script: Preparing the input species list. Here based on the PaciFlora data set
# Author: Anna Rönnfeldt
# Date Created: 2023-03
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------


library(tidyverse)

rm(list = ls())

# Species data ------------------------------------------------------------

# data set on pacific island invaders from: https://bdj.pensoft.net/article/67318/
species_names <- read.table("data/PaciFLora.txt", header = TRUE) %>%
  dplyr::select(Species) %>% 
  arrange(Species) %>%       
  distinct() %>%
  drop_na() %>%
  rename(species_orig = Species) %>%
  mutate(species_id = row_number(), .before = "species_orig") %>% # add id to identify each species later on
  mutate(species_changed = species_orig) %>% # duplicate species column twice in preparation for the next step
  mutate(species_no_x = species_orig)


# reformat hybrids --------------------------------------------------------


for (i in 1:nrow(species_names)) {
  
  print(i)
  print(species_names[i,"species_orig"])

  if (grepl("_x", species_names[i,"species_orig"])) {
    
    # move the "x" from the end of the string to between genus and species in the second column
    species_names[i,"species_changed"] <- gsub(x = species_names[i,"species_changed"], pattern = "_x", replacement = "")
    species_names[i,"species_changed"] <- gsub(x = species_names[i,"species_changed"], pattern = " ", replacement = " x ")
    
    # remove "x" completely from the second column
    species_names[i,"species_no_x"] <- gsub(x = species_names[i,"species_no_x"], pattern = "_x", replacement = "")
  } # end of if 
} # end of for loop over species_names


save(species_names, file = "data/PaciFLora_species_list.RData")

