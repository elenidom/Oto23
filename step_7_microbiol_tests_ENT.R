
#######################################################################################################
#                                           *
#                                       Otorrhoea
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                       June 2023
#                                       CPRD Aurum
#                                           *
# 
#######################################################################################################

library(dplyr)
library(lubridate)
library(haven)

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Lookups")

meddic.a <- read_dta(file = "202106_emismedicaldictionary.dta")
head(meddic.a)

# search for certain strings that include ENT, ear/nose/throat, otolaryngology
# search_med_terms <- " ENT| ent |otolar|otorhino|ear, nose, throat"
search_med_terms <- "Swab - ENT, sent|ear swab|pus swab|culture swab|MC&S swab|otorrhoea swab"

my_ENT_test_list <- meddic.a %>% filter(grepl(search_med_terms, term, fixed = F))
my_ENT_test_list$medcodeid <- as.character(my_ENT_test_list$medcodeid)
my_ENT_test_list$snomedctconceptid <- as.character(my_ENT_test_list$snomedctconceptid)
my_ENT_test_list$snomedctdescriptionid <- as.character(my_ENT_test_list$snomedctdescriptionid)
head(my_ENT_test_list)
# write.table(my_ENT_test_list, "medcodes_ENT_list.txt")

# Read observation files and filter them so you keep only records with an ear swab test

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Observation/")

observation_fileslist = list.files(pattern = ".dta")

# ATTENTION!!!
# The following lines takes days to run. Alternatively, split into at least 10 fragments.
# merge every observation file with Elliot's medcodes list so that only records with PO event remain
for(i in 1:length(observation_fileslist)){
  read_dta <- read_dta(file = observation_fileslist[i])
  read_dta$patid <- as.character(read_dta$patid)
  read_dta$medcodeid <- as.character(read_dta$medcodeid)
  merged_observation <- left_join(my_ENT_test_list, read_dta, by = "medcodeid")
  assign(paste0("OBS_Ear_swab_test_", i), merged_observation)
  print(i) # print the iteration number
}

# NOTE: file from aurum_allpatid_set38_extract_observation_026.dta
# was missing from part1

# unite all the merged files
u <- dplyr::union(OBS_Ear_swab_test_1, OBS_Ear_swab_test_2) # files 1+2

# append them manually (e.g. for i 3 to 771)
for(i in 3:length(observation_fileslist)){ # rest of files
  u <- dplyr::union(u, eval(parse(text = paste0("OBS_Ear_swab_test_", i))))
}

# save
# write.csv(u, "/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted/aurum_obs_merged_ear_swabs.csv")

# read data
swabs.data <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted/aurum_obs_merged_ear_swabs.csv")

# count how many medcodeids are missing
sum(!is.na(swabs.data$medcodeid))

# count distinct patids
swabs.data %>% summarise(Patients = n_distinct(patid))

# clear, tidy and tranform into dataframe
swabs.data <- as.data.frame(swabs.data)
str(swabs.data)
swabs.data$patid <- as.character(swabs.data$patid)
swabs.data$staffid <- as.character(swabs.data$staffid)
swabs.data$medcodeid <- as.character(swabs.data$medcodeid)
swabs.data$swabdate <- as_date(swabs.data$obsdate)
swabs.data$X <- NULL
head(swabs.data)
summary(swabs.data)
swabs.data <- swabs.data %>% filter(!is.na(myeventdate))

# ----------------------------------------------------------------------------------
# Keep only those records for patients with a PO event at the date of the swab test
# ----------------------------------------------------------------------------------

# read the final cohort
my_patients <- read.csv("final_PO_IR_master_coh.csv")
my_patients <- my_patients %>% filter(PO_event == 1) %>% select(c(patid, myeventdate))
my_patients$patid <- as.character(my_patients$patid)
my_patients$myeventdate <- as_date(my_patients$myeventdate)

# filter the patids with patients who are in the master cohort
mydata <- left_join(my_patients, swabs.data, by = "patid")
summary(mydata)
mydata <- mydata %>% filter(!is.na(swabdate))

mydata <- mydata %>% filter(myeventdate == swabdate)
