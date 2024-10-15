#' ---------------------------
#
# Purpose of script: accessing the information on the biogeographic status of the 
# target species from GloNAF and matching them with their occurrences
# Author: Katrin Schifferle, Anna Rönnfeldt
# Date Created: 2022, revised by Anna Rönnfeldt in 2023-05
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------


# preamble ----------------------------------------------------------------


# packages 
library(conflicted)
library(sf)
library(stringr)
library(tidyverse)

filter <- dplyr::filter

rm(lsit = ls())


# load data ---------------------------------------------------------------

load("data/PaciFLora_species_list.RData") # species list, object name: species_names
specs <- species_names$species_changed
rm(species_names)

# get GloNAf status -------------------------------------------------------

# GloNAF = Global Naturalized Alien Flora: https://glonaf.org/
# GloNAF contains status information based on TDWG regions, as POWO,
# status information is either "alien" or "naturalized"

# reading the taxon csv-file downloaded from the website didn't work for me due to encoding problems,
# converted csv beforehand to UTF8 encoding using notepad++:
species_dt <- read.delim(file = "data/GloNAF/Taxon_x_List_GloNAF_vanKleunenetal2018Ecology_UTF8.csv")
region_dt <- read.delim(file = "data/GloNAF/Region_GloNAF_vanKleunenetal2018Ecology.csv", sep = ",")
list_dt <- read.delim(file = "data/GloNAF/List_GloNAF_vanKleunenetal2018Ecology.csv", sep = ",")

# merge the different GloNAF data frames
glonaf_dt <- species_dt %>%
  left_join(region_dt) %>%
  left_join(list_dt) %>%
  mutate(species_name = paste(standardized_name, author), .before = "taxon_orig") %>%
  arrange(species_name)

# free memory:
rm(species_dt, region_dt, list_dt)


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


# glonaf provides no polygons, so the status information will be added to the information from the other two status sources and the species occurrences in script 4