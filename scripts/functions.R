library(lcvplants)
library(GIFT) # install from GitHub with: devtools::install_github("https://github.com/BioGeoMacro/GIFT")
# library(reticulate) # R interface to Python modules
DIFFLIB <- reticulate::import("difflib") # load Python module



getGiftNames <- function(spec, incl_lcvp_synonyms = FALSE){
  
  # searches for species name in GIFT dataset, checks species name against LCVP (on which blacklist is based),
  # from GIFT extracts WCVP harmonized species name (same taxonomic backbone as in POWO) if species is also included in LCVP
  
  # input:  
  #   - spec: LCVP based species name
  #   - incl_lcvp_synonyms: TRUE = LCVP search results include names that are considered synonyms in LCVP;
  #                         FALSE = only accepted names in LCVP are considered
  # output: 
  #   - df: searched name - GIFT result genus - GIFT result species epithet
  
  print(spec)
  
  # split name in genus and species epithet:
  spec_gen_epi <- unlist(str_split(spec, pattern = " ", n = 2))
  
  # find searched species name in GIFT:
  GIFT_spec_res <- tryCatch({
    
    GIFT_species_lookup(genus = spec_gen_epi[1], 
                        epithet = spec_gen_epi[2], 
                        namesmatched = TRUE, # TRUE = look for the species not only in the standardized names but also in the original names
                        GIFT_version = "beta")
  },
  error = function(e){
    print(paste("Connection to Gift information failed, try again later."))
    return(data.frame("searched_name" = spec,
                      "GIFT_genus" = NA,
                      "GIFT_species_ep" = NA))})
  
  # GIFT results: species names incl. author before harmonization:
  GIFT_spec_org <- paste(GIFT_spec_res$genus, GIFT_spec_res$species_epithet, GIFT_spec_res$author)
  
  # check against LCVP:
  
  # search LCVP entries connected to the searched species name:
  if(incl_lcvp_synonyms){
    status_ok <- c("accepted", "synonym") # exclude unresolved and external results
  } else {status_ok <- "accepted"} # only accepted species names
  lcvp_spec_res <- unique(lcvp_fuzzy_search(spec, status = status_ok)$Output.Taxon) # lcvp_fuzzy_search from package lcvpplants
  
  # GIFT WCVP harmonized names (column work_species) matching LCVP results:
  GIFT_res_harm <- GIFT_spec_res$work_species[which(GIFT_spec_org %in% lcvp_spec_res)]
  
  # there is no exact match (e.g. due to slightly different names; 6 species of the 122 blacklist species)
  if(length(GIFT_res_harm) == 0){
    
    # find most similar name with fuzzy matching:
    print("No exact match between Gift and LCVP name found. Used fuzzy matching instead. Consider checking the results manually.")
    best_match <- DIFFLIB$get_close_matches(word = lcvp_spec_res, 
                                            possibilities = GIFT_spec_org,
                                            n = as.integer(1), cutoff = 0)
    print(paste("LCVP name:", lcvp_spec_res))
    print(paste("Matched Gift name:", best_match))
    GIFT_res_harm <- GIFT_spec_res$work_species[which(GIFT_spec_org == best_match)]
  }
  
  # split in genus and species epithet:
  GIFT_harm_gen_epi <- unlist(str_split(GIFT_res_harm, pattern = " ", n = 2))
  
  if(length(GIFT_harm_gen_epi) != 0){
    return(data.frame("searched_name" = spec,
                      "GIFT_genus" = GIFT_harm_gen_epi[1],
                      "GIFT_species_ep" = GIFT_harm_gen_epi[2]))
  }else{
    print("Matching GIFT and LCVP name didn't work. Check manually.")
    return(data.frame("searched_name" = spec,
                      "GIFT_genus" = NA,
                      "GIFT_species_ep" = NA))
  }
}


getGiftStatusInf <- function(searched_name, GIFT_spec_genus, GIFT_spec_epithet){
  
  # extracts status information from GIFT
  # input:  
  #   - searched_name: original species name from blacklist, used in output to later join occurrences
  #   - GIFT_spec_genus: genus of GIFT species name
  #   - GIFT_spec_epithet: species epithet of GIFT species name
  # output: 
  #   - df: searched_species, GIFT species - entity_ID (= ID of GIFT polygon) - native - naturalized - endemic_list
  
  # attention: GIFT provides status information for nested regions, e.g. status information for Honshu, but also for Japan in general,
  # whether nested regions should all be retained or how information should be dissolved can be defined within
  # GIFT_species_distribution()
  # for us it makes sense to keep all nested regions in the first place and remove nested regions after merging with occurrences
  # (occurrences may be located at different nesting levels, e.g. not on Honshu but in another location in Japan, if we had dropped Japan in the first place,
  # no status information would be assigned)
  
  print(paste(GIFT_spec_genus, GIFT_spec_epithet))
  
  # find GIFT distribution for harmonized species name: 
  GIFT_spec_distr <- tryCatch({
    
    GIFT_species_distribution(genus = GIFT_spec_genus,
                              epithet = GIFT_spec_epithet, 
                              aggregation = TRUE, # TRUE = only one status per polygon
                              namesmatched = FALSE, # TRUE not necessary since harmonized species name is used
                              #remove_overlap = TRUE, # return only one of overlapping polygons, depending on the rules defined below:
                              #overlap_th = 0.1, # default, if polygons overlap by min. 10 % they are treated as overlapping, if they overlap less, both polygons are kept
                              #area_th_mainland = 0, # overlapping mainlands: use smallest mainland (e.g. use Tanzania rather than East Tropical Africa)
                              #area_th_island = 0, # use smallest island (rather than Island Group, e.g. use Honshu rather than Japan)
                              GIFT_version = "beta") # doesn't allow including author in search
  }, error = function(e){
    print(paste("Connection to Gift information failed, try again later."))
    spec_status_inf <- data.frame(species = searched_name,
                                  GIFT_species = paste(GIFT_spec_genus, GIFT_spec_epithet),
                                  entity_ID = NA,
                                  native = NA,
                                  naturalized = NA,
                                  endemic_list = NA)
    return(spec_status_inf)
  })
  
  # extract and re-format information:
  spec_status_inf <- GIFT_spec_distr %>%
    mutate(species = searched_name) %>%
    mutate(GIFT_species = paste(GIFT_spec_genus, GIFT_spec_epithet)) %>%
    mutate(native = ifelse(native == 1, "native", "non-native"),
           naturalized = ifelse(naturalized == 1, "naturalized",
                                "non-naturalized"),
           endemic_list = ifelse(endemic_list == 1, "endemic_list",
                                 "non-endemic_list")) %>%
    select(species, GIFT_species, entity_ID, native, naturalized, endemic_list) %>%
    filter_at(vars(native, naturalized, endemic_list), any_vars(!is.na(.)))  # remove entries without any status information
  
  return(spec_status_inf)
  
}

