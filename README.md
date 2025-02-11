# StatusAssignment

**Purpose:** Download and cleaning of occurrence data from GBIF and BIEN based on the species list from PaciFLora. The cleaned occurrence data are then assigned a biogeographic status indicating whether the species is considered to be _native_ or _introduced_ in that region. </br>
**Authors:** Katrin Schifferle, Anna Rönnfeldt and Valén Holle </br>
**Year:** The main workflow was developed between 2022-2023 </br>
**Funding:** This study was supported by the German Research Foundation DFG (grant no. ZU 361/3-1 to DZ)

## Required data

These data have to be downloaded to reproduced this workflow:
* the species list from the [PaciFLora](https://bdj.pensoft.net/article/67318/) data set. It lists 3963 vascular plant species known to occur as naturalised species on the Pacific Islands
* spatial data corresponding to level 3 [tdwg](https://github.com/tdwg/wgsrpd) regions
* [GloNAF](https://idata.idiv.de/DDM/Data/ShowData/257) data for the status assignment


## Workflow

### 0 - species list
script [0_prep_species_list](https://github.com/UP-macroecology/StatusAssignment/blob/main/scripts/0_prep_species_list.R) 

A species list based on the PaciFLora data set is prepared. In addition to the original PaciFLora names, two additional columns are appendet with alternative ways of writing names of hybrid species. 

### 1 - download species data 
scripts [1_download_bien](https://github.com/UP-macroecology/StatusAssignment/blob/main/scripts/1_download_bien.R), [1_download_gbif](https://github.com/UP-macroecology/StatusAssignment/blob/main/scripts/1_download_gbif.R)

Two scripts to download occurrence data from [BIEN](https://biendata.org/) and [GBIF](https://www.gbif.org/). We downloaded occurrence data in June 2023.

### 2 - data preparation 
script [2_data_preparation](https://github.com/UP-macroecology/StatusAssignment/blob/main/scripts/2_data_preparation.R)

The downloaded occurrence records from the two sources were then cleaned to remove duplicates and occurrences with erroneous time stamps or coordinates, and harmonised to combine them into one data set at a 1 km resolution. 

### 3 - status assignment
scripts [3_status_GIFT](https://github.com/UP-macroecology/StatusAssignment/blob/main/scripts/3_status_GIFT.R), [3_status_GloNAF](https://github.com/UP-macroecology/StatusAssignment/blob/main/scripts/3_status_GloNAF.R), [3_status_WCVP](https://github.com/UP-macroecology/StatusAssignment/blob/main/scripts/3_status_WCVP.R)

The main source for information on the invasion status (native, introduced, contradictory or unknown) of a species occurrence is the [World Checklist of Vascular Plants (WCVP)](http://www.plantsoftheworldonline.org/) at the base resolution of level 3 of tdwg. To close data gaps, two additional sources were used: [Global Inventory of Floras and Traits (GIFT)](https://gift.uni-goettingen.de/home) and [Global Naturalized Alien Flora (GloNAF)](https://glonaf.org/). As the three sources used different terminology to describe the biogeographic invasion status, we harmonised the terminology as _native_ or _introduced_. </br>
There are three scripts, each handling the status assignment based on one of the three data sources. 

### 4 - resolve status conflicts
script [4_resolve_status_conflicts](https://github.com/UP-macroecology/StatusAssignment/blob/main/scripts/4_resolve_status_conflicts.R)

Merging the status information from these three sources resulted in some conflicts. If the sources that caused the conflict referred to areas of different sizes (e.g., Honshu and Japan), the status from the source referring to the smaller area is used. If the sources refer to the same area size, two cirteria can be used to deal with conflicts:

* **Criterion 1**: All sources are weighted equally. Occurrences with conflicts in the status assignment get the status *contradictory*.
* **Criterion 2**: Status information provided by WCVP is preferred, because it has been targeted towards the level 3 resolution of tdwg.

---------------------------------------------------------------
**Required folder structure**
---------------------------------------------------------------

```
scripts

data
├── download_bien
├── download_gbif
├── status_assignment
├── GloNAF 
├── spatial_data

```
## Operating system info

* R version 4.2.2 (2022-10-31 ucrt)
* Platform: x86_64-w64-mingw32/x64 (64-bit)
* Running under: Windows 10 x64 (build 19045)
* Attached packages:  [1] BIEN_1.26 [2] conflicted_1.2.0.9000 [3] CoordinateCleaner_3.0.1 [4] doParallel_1.0.17 [5] fasterize_1.0.4 [6] foreach_1.5.2 [7] GIFT_1.0.0 [8] lcvplants_2.1.0 [9] rgbif_3.7.5 [10] rWCVP_1.2.4 [11] sf_1.0-16 [12] taxize_0.9.100 [13] tibble_3.2.1 [14] tidyverse_2.0.0 [15] terra_1.7-78 [16] units_0.8-5
