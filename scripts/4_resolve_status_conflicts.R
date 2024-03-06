
# preamble ----------------------------------------------------------------

# packages
library(doParallel)
library(dplyr)
library(foreach)
library(sf)
library(tibble)
library(tidyr)


# load data ---------------------------------------------------------------

# status information from the different sources
load("data/status_assignment/occ_GIFT_status.RData")
load("data/status_assignment/occ_WCVP_status.RData")
load("data/status_assignment/GloNAF_status.RData") # not merged with occurrences


# combine status data frames ----------------------------------------------

# add prefixes to column names so that it can later be distinguished where the information came from
colnames(occ_wcvp_status)[c(3:8)] <- paste("wcvp", colnames(occ_wcvp_status)[c(3:8)], sep = "_")
colnames(occ_GIFT_status)[c(3:8)] <- paste("gift", colnames(occ_GIFT_status)[c(3:8)], sep = "_")
colnames(glonaf_status_info)[c(4:6)] <- paste("glonaf", colnames(glonaf_status_info)[c(4:6)], sep = "_")

# prepare main status df
occ_status_all <- merge(occ_wcvp_status, occ_GIFT_status, by = c("occ_id", "species")) %>% # join the information for WCVP and GIFT together
  arrange(occ_id) %>%
  select(-c(gift_polygon_source, gift_GIFT_species)) %>% # remove columns that are not required for the next step
  rename(wcvp_status = wcvp_occurrence_type) %>%
  add_column(glonaf_status = NA, final_status = NA) %>% # add empty column for glonaf status and the final status
  relocate(wcvp_status, gift_status, glonaf_status, final_status, .after = last_col())


# merge GloNAF stats with occurrences -------------------------------------

# identify unique combinations of species and level 3
unique_sr_long <- unique(occ_status_all[c("species", "wcvp_LEVEL3_COD")]) %>%
  arrange(wcvp_LEVEL3_COD) %>%
  drop_na() %>%
  add_column(included = NA) %>% # add column for the processing in the next step
  add_column(sr_id = 1:nrow(unique_sr_long)) 

# save(unique_sr_long, file = "data/status_assignment/unique_sr.RData")

unique_sr <- unique_sr_long[116500:119814,] # only work with the ones that have not yet been included

counter <- 0

for (i in 1:nrow(unique_sr)) {
  
  specs <- unique_sr[i,1]
  region <- unique_sr[i,2]
  id <- unique_sr[i,4]
  
  counter <- counter + 1
  print(counter)
  
  unique_sr_long[unique_sr_long$sr_id == id, "included"] <- "done"
  
  # get glonaf status for this combination
  status <- glonaf_status_info %>%
    dplyr::filter(glonaf_tdwg3 == region & (species_orig == specs | species_changed == specs | species_no_x == specs | glonaf_standardized_name == specs)) %>%
    distinct() %>%
    pull(glonaf_status)
  
  if (length(status) == 1) {
    occ_status_all[which(occ_status_all$species == specs & occ_status_all$wcvp_LEVEL3_COD == region), "glonaf_status"] <- status
    
    
  } # end of if condition
  
  if (counter >= 10000) {
    break
  } # end of if
  
} # end  for loop

save(occ_status_all, file = "data/status_assignment/occ_status_all.RData")


# check for conflicts -----------------------------------------------------

# the data set is too big for my laptop, so I split it into four parts

occ_stat_1 <- occ_status_all[1:10000000,]
occ_stat_2 <- occ_status_all[10000001:20000000,]
occ_stat_3 <- occ_status_all[20000001:30000000,]
occ_stat_4 <- occ_status_all[30000001:38064344,]

# change number x in occ_status_merged_x and occ_stat_x accordingly
occ_status_merged_4 <- occ_stat_4 %>%
  mutate(status_check = case_when(
    
    # ID 1
    wcvp_status == "native" & gift_status == "native" & is.na(glonaf_status) ~ "native",
    
    # ID 2
    wcvp_status == "native" & gift_status == "native" & glonaf_status == "naturalized" ~ "Glonaf_vs_rest",
    
    # ID 3
    wcvp_status == "native" & gift_status == "native" & glonaf_status == "alien" ~ "Glonaf_vs_rest",
    
    # ID 4
    wcvp_status == "native" & gift_status == "non-native" & is.na(glonaf_status) ~ "W_vs_G_no_Gl",
    
    # ID 5
    wcvp_status == "native" & gift_status == "non-native" & glonaf_status == "naturalized" ~ "Wcvp_vs_rest",
    
    # ID 6
    wcvp_status == "native" & gift_status == "non-native" & glonaf_status == "alien" ~ "Wcvp_vs_rest",
    
    # ID 7
    wcvp_status == "native" & gift_status == "naturalized" & is.na(glonaf_status) ~ "W_vs_G_no_Gl",
    
    # ID 8
    wcvp_status == "native" & gift_status == "naturalized" & glonaf_status == "naturalized" ~ "Wcvp_vs_rest",
    
    # ID 9
    wcvp_status == "native" & gift_status == "naturalized" & glonaf_status == "alien" ~ "Wcvp_vs_rest",
    
    # ID 10
    wcvp_status == "native" & is.na(gift_status) & is.na(glonaf_status) ~ "native",
    
    # ID 11
    wcvp_status == "native" & is.na(gift_status) & glonaf_status == "naturalized" ~ "W_vs_Gl_no_G",
    
    # ID 12
    wcvp_status == "native" & is.na(gift_status) & glonaf_status == "alien" ~ "W_vs_Gl_no_G",
    
    # ID 13
    wcvp_status == "introduced" & gift_status == "native" & is.na(glonaf_status) ~ "W_vs_G_no_Gl",
    
    # ID 14
    wcvp_status == "introduced" & gift_status == "native" & glonaf_status == "naturalized" ~ "Gift_vs_rest",
    
    # ID 15
    wcvp_status == "introduced" & gift_status == "native" & glonaf_status == "alien" ~ "Gift_vs_rest",
    
    # ID 16
    wcvp_status == "introduced" & gift_status == "non-native" & is.na(glonaf_status) ~ "introduced",
    
    # ID 17
    wcvp_status == "introduced" & gift_status == "non-native" & glonaf_status == "naturalized" ~ "introduced",
    
    # ID 18
    wcvp_status == "introduced" & gift_status == "non-native" & glonaf_status == "alien" ~ "introduced",
    
    # ID 19
    wcvp_status == "introduced" & gift_status == "naturalized" & is.na(glonaf_status) ~ "introduced",
    
    # ID 20
    wcvp_status == "introduced" & gift_status == "naturalized" & glonaf_status == "naturalized" ~ "introduced",
    
    # ID 21
    wcvp_status == "introduced" & gift_status == "naturalized" & glonaf_status == "alien" ~ "introduced",
    
    # ID 22
    wcvp_status == "introduced" & is.na(gift_status) & is.na(glonaf_status) ~ "introduced",
    
    # ID 23
    wcvp_status == "introduced" & is.na(gift_status) & glonaf_status == "naturalized" ~ "introduced",
    
    # ID 24
    wcvp_status == "introduced" & is.na(gift_status) & glonaf_status == "alien" ~ "introduced",
    
    # ID 25
    is.na(wcvp_status) & gift_status == "native" & is.na(glonaf_status) ~ "native",
    
    # ID 26
    is.na(wcvp_status) & gift_status == "native" & glonaf_status == "naturalized" ~ "G_vs_Gl_no_W",
    
    # ID 27
    is.na(wcvp_status) & gift_status == "native" & glonaf_status == "alien" ~ "G_vs_Gl_no_W",
    
    # ID 28
    is.na(wcvp_status) & gift_status == "non-native" & is.na(glonaf_status) ~ "introduced",
    
    # ID 29
    is.na(wcvp_status) & gift_status == "non-native" & glonaf_status == "naturalized" ~ "introduced",
    
    # ID 30
    is.na(wcvp_status) & gift_status == "non-native" & glonaf_status == "alien" ~ "introduced",
    
    # ID 31
    is.na(wcvp_status) & gift_status == "naturalized" & is.na(glonaf_status) ~ "introduced",
    
    # ID 32
    is.na(wcvp_status) & gift_status == "naturalized" & glonaf_status == "naturalized" ~ "introduced",
    
    # ID 33
    is.na(wcvp_status) & gift_status == "naturalized" & glonaf_status == "alien" ~ "introduced",
    
    # ID 34
    is.na(wcvp_status) & is.na(gift_status)  & is.na(glonaf_status) ~ NA,
    
    # ID 35
    is.na(wcvp_status) & is.na(gift_status)  & glonaf_status == "naturalized" ~ "introduced",
    
    # ID 36
    is.na(wcvp_status) & is.na(gift_status)  & glonaf_status == "alien" ~ "introduced",
    
    # ID 37
    wcvp_status == "native" & gift_status == "unknown" & is.na(glonaf_status) ~ "native",
    
    # ID 38
    wcvp_status == "native" & gift_status == "unknown"  & glonaf_status == "naturalized" ~ "W_vs_Gl_no_G",
    
    # ID 39
    wcvp_status == "native" & gift_status == "unknown"  & glonaf_status == "alien" ~ "W_vs_Gl_no_G",
    
    # ID 40
    wcvp_status == "introduced" & gift_status == "unknown"  & is.na(glonaf_status) ~ "introduced",
    
    # ID 41
    wcvp_status == "introduced" & gift_status == "unknown"  & glonaf_status == "naturalized" ~ "introduced",
    
    # ID 42
    wcvp_status == "introduced" & gift_status == "unknown"  & glonaf_status == "alien" ~ "introduced",
    
    # ID 43
    is.na(wcvp_status) & gift_status == "unknown"  & is.na(glonaf_status) ~ "unknown",
    
    # ID 44
    is.na(wcvp_status) & gift_status == "unknown"  & glonaf_status == "naturalized" ~ "introduced",
    
    # ID 45
    is.na(wcvp_status) & gift_status == "unknown"  & glonaf_status == "alien" ~ "introduced"
  )) %>%
  mutate(criterion_1 = status_check, criterion_2 = status_check)# copy column content to new columns that will be modified in the next step



occ_status_merged <- rbind(occ_status_merged_1,
                           occ_status_merged_2,
                           occ_status_merged_3,
                           occ_status_merged_4)

rm(occ_status_merged_1,
   occ_status_merged_2,
   occ_status_merged_3,
   occ_status_merged_4)

table(occ_status_merged$status_check)

# save(occ_status_merged, file = "data/status_assignment/occ_status_merged.RData")


# resolve conflicts -------------------------------------------------------

specs <- unique(occ_status_merged$species)

no_cores <- 2
cl <- makeCluster(no_cores)
registerDoParallel(cl)

occ_status_resolved <- foreach(s = 1:length(specs), .packages = "dplyr",
                               .combine = "rbind", .verbose = TRUE) %do% {
                                 
                                 occ_specs <- subset(occ_status_merged, species == specs[s])
                                 
                                 conflict_index <- which(occ_specs$status_check != "native" & occ_specs$status_check != "introduced" &
                                                           occ_specs$status_check != "unknown" & !is.na(occ_specs$status_check))
                                 
                                 for (i in conflict_index) {
                                   
                                   if (occ_specs[i,"status_check"] == "W_vs_G_no_Gl") {
                                     
                                     # get sizes of the two areas to compare them (both in km^2)
                                     areas <- data.frame(source = c("wcvp", "gift"),
                                                         area_km2 = c(occ_specs[i, "wcvp_area"], occ_specs[i, "gift_area"]))
                                     
                                     if (!anyNA(areas)) {
                                       
                                       if (areas[1,2] != areas[2,2]) {
                                         # if areas don't have the same size:
                                         
                                         # identify which status source refers to the smaller region
                                         smallest_source <- areas[which.min(areas$area_km2),1]
                                         
                                         # assign status info for both criteria based on this source
                                         occ_specs[i,"criterion_1"] <- occ_specs[i, paste0(smallest_source, "_status")]
                                         occ_specs[i,"criterion_2"] <- occ_specs[i, paste0(smallest_source, "_status")]
                                         
                                       } else {
                                         # if areas have the same size: assign status info for both criteria based on their rules
                                         
                                         # criterion 1 (no source preferences)
                                         occ_specs[i,"criterion_1"] <- "contradictory"
                                         # criterion 2 (WCVP preferred)
                                         occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                         
                                       } # end of ifelse area comparison
                                       
                                     } else {
                                       
                                       # if area information is missing: assign status info for both criteria based on their rules
                                       
                                       # criterion 1 (no source preferences)
                                       occ_specs[i,"criterion_1"] <- "contradictory"
                                       # criterion 2 (WCVP preferred)
                                       occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                       
                                     } # end of iselse around areas
                                     
                                     
                                   } # end of  if "W_vs_G_no_Gl"
                                   
                                   
                                   #'################# 
                                   
                                   if (occ_specs[i,"status_check"] == "W_vs_Gl_no_G") {
                                     # WCVP and GloNAF both refer to tdwg level 3, so if they contradict each other
                                     # in the status assignment, this conflict can not be resolved based on region size
                                     # assign status info for both criteria based on their rules:
                                     
                                     # criterion 1 (no source preferences)
                                     occ_specs[i,"criterion_1"] <- "contradictory"
                                     # criterion 2 (WCVP preferred)
                                     occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                     
                                   } # end of if "W_vs_Gl_no_G"
                                   
                                   #'################# 
                                   
                                   if (occ_specs[i,"status_check"] == "G_vs_Gl_no_W") {

                                     # get sizes of the two areas to compare them (both in km^2)
                                     areas <- data.frame(source = c("wcvp", "gift"),
                                                         area_km2 = c(occ_specs[i, "wcvp_area"], occ_specs[i, "gift_area"]))


                                     if (!anyNA(areas)) {

                                       if (areas[1,2] != areas[2,2]) {
                                         # if areas don't have the same size:

                                         # identify which status source refers to the smaller region
                                         smallest_source <- areas[which.min(areas$area_km2),1]

                                         if (smallest_source == "gift") {
                                           # assign status info for both criteria based on gift
                                           occ_specs[i,"criterion_1"] <- occ_specs[i, paste0(smallest_source, "_status")]
                                           occ_specs[i,"criterion_2"] <- occ_specs[i, paste0(smallest_source, "_status")]

                                         } else {
                                           # assign status info for both criteria based on glonaf
                                           occ_specs[i,"criterion_1"] <- occ_specs[i,"glonaf_status"]
                                           occ_specs[i,"criterion_2"] <- occ_specs[i,"glonaf_status"]
                                         } # end of else smallest_source

                                       } else {
                                         # if the areas have the same size:
                                         # no preferences between gift and glonaf, assign contradictory for both criteria

                                         # criterion 1 (no source preferences)
                                         occ_specs[i,"criterion_1"] <- "contradictory"
                                         # criterion 2 (WCVP preferred)
                                         occ_specs[i,"criterion_2"] <- "contradictory"

                                       } # end of else around equal area size

                                     } else {

                                       # if the area info is missing:
                                       # no preferences between gift and glonaf, assign contradictory for both criteria

                                       # criterion 1 (no source preferences)
                                       occ_specs[i,"criterion_1"] <- "contradictory"
                                       # criterion 2 (WCVP preferred)
                                       occ_specs[i,"criterion_2"] <- "contradictory"

                                     } # end of ifesle around areas

                                   } # end of if "G_vs_Gl_no_W"

                                   #'################# 
                                   
                                   if (occ_specs[i,"status_check"] == "Wcvp_vs_rest") {
                                     # this conflict can not be resolved based on region size
                                     # assign status info for both criteria based on their rules:
                                     
                                     # criterion 1 (no source preferences)
                                     occ_specs[i,"criterion_1"] <- "contradictory"
                                     # criterion 2 (WCVP preferred)
                                     occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                     
                                   } # end of if "Wcvp_vs_rest"
                                   
                                   #'################# 
                                   
                                   if (occ_specs[i,"status_check"] == "Glonaf_vs_rest") {
                                     # this conflict can not be resolved based on region size
                                     # assign status info for both criteria based on their rules:
                                     
                                     # criterion 1 (no source preferences)
                                     occ_specs[i,"criterion_1"] <- "contradictory"
                                     # criterion 2 (WCVP preferred)
                                     occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                   } # end of if "GloNAF_vs_rest"
                                   
                                   #'################# 
                                   
                                   if (occ_specs[i,"status_check"] == "Gift_vs_rest") {
                                     
                                     # get sizes of the two areas to compare them (both in km^2)
                                     areas <- data.frame(source = c("wcvp", "gift"),
                                                         area_km2 = c(occ_specs[i, "wcvp_area"], occ_specs[i, "gift_area"]))
                                     
                                     
                                     if (!anyNA(areas)) {
                                       if (areas[1,2] != areas[2,2]) {
                                         # if areas don't have the same size:
                                         
                                         # identify which status source refers to the smaller region
                                         smallest_source <- areas[which.min(areas$area_km2),1]
                                         
                                         # assign status info for both criteria based on this source
                                         occ_specs[i,"criterion_1"] <- occ_specs[i, paste0(smallest_source, "_status")]
                                         occ_specs[i,"criterion_2"] <- occ_specs[i, paste0(smallest_source, "_status")]
                                         
                                       } else {
                                         # if areas have the same size: assign status info for both criteria based on their rules
                                         
                                         # criterion 1 (no source preferences)
                                         occ_specs[i,"criterion_1"] <- "contradictory"
                                         # criterion 2 (WCVP preferred)
                                         occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                         
                                       } # end of else
                                       
                                     } else {
                                       # if area info is missing: assign status info for both criteria based on their rules
                                       
                                       # criterion 1 (no source preferences)
                                       occ_specs[i,"criterion_1"] <- "contradictory"
                                       # criterion 2 (WCVP preferred)
                                       occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                       
                                     } # end of ifelse around areas
                                     
                                   } # end of if "GloNAF_vs_rest"
                                   
                                 } # for loop over conflict_index
                                 
                                 occ_specs
                               } # end of foreach

stopCluster(cl)

save(occ_status_resolved, file = "data/status_assignment/occ_status_resolved.RData")
