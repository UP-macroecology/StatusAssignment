# GlobalOccurrences

**Purpose:** Download and cleaning of occurrence data from GBIF and BIEN based on the species list from PaciFLora. The cleaned occurrence data are then assigned a biogeographic status indicating whether the species is considered to be _native_ or _introduced_ in that region. </br>
**Authors:** Katrin Schifferle, Anna Rönnfeldt and Valén Holle </br>
**Year:** The main workflow was developed between 2022-2023 </br>

## Required data:

Within this repository: 
* (spatial data for the delineation of the Pacific Islands (provided by Michael Wohlwend))

External data one has to download to reproduce the workflow in this repository:
* the species list from the [PaciFLora](https://bdj.pensoft.net/article/67318/) data set. It lists 3963 vascular plant species known to occur as naturlaised species on the Pacific Islands
* (spatial data for level 1 [tdwg](https://github.com/tdwg/wgsrpd) regions)
* [GloNAF](https://idata.idiv.de/DDM/Data/ShowData/257) data for the status assignment

## Required folder structure:
The base folder should be named "data". It should contain the PaciFLora species list and the following subfolders:

* download_bien
* download_gbif
* status_assignment
* (spatial_data)

## Workflow

### 0 - species list

### 1 - download species data 
Two scripts to download occurrence data from [BIEN](https://biendata.org/) and [GBIF](https://www.gbif.org/). We downloaded occurrence data in June 2023.

### 2 - data preparation 
The downloaded occurrence records from the two sources were then cleaned to remove duplicates and occurrences with erroneous time stamps or coordinates, and harmonised to combine them into one data set at a 1 km resolution. 

### 3 - status assignment
The main source for information on the invasion status (native, introduced, contradictory or unknown) of a species occurrence is the [World Checklist of Vascular Plants (WCVP)](http://www.plantsoftheworldonline.org/) at the base resolution of level 3 of tdwg. To close data gaps, two additional sources were used: [Global Inventory of Floras and Traits (GIFT)](https://gift.uni-goettingen.de/home) and [Global Naturalized Alien Flora (GloNAF)](https://glonaf.org/). As the three sources used different terminology to describe the biogeographic invasion status, we harmonised the terminology as _native_ or _introduced_. 

### 4 - resolve status conlficts
Merging the status information from these three sources resulted in some conflicts. If the sources that caused the conflict referred to areas of different sizes (e.g., Honshu and Japan), the status from the source referring to the smaller area is used. If the sources refer to the same area size, two cirteria can be used to deal with conflicts:

* **Criterion 1**: All sources are weighted equally. Occurrences with conflicts in the status assignment get the status *contradictory*.
* **Criterion 2**: Status information provided by WCVP is perferred, because it has been targeted towards the level 3 resolution of tdwg.
