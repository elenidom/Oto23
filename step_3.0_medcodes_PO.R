
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

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/")

# -----------------------------------------------------------------
# read otorrhoea medcodes from Elliot - use only the primary codes
# -----------------------------------------------------------------

my_oto_medcodes_A <- read.table("Aurum_medcodes_otorrhoea.txt", header = T, sep = "\t")

my_oto_medcodes_A$medcodeid <- as.character(my_oto_medcodes_A$medcodeid)
my_oto_medcodes_A$readcode <- as.character(my_oto_medcodes_A$readcode)
my_oto_medcodes_A$snomedctconceptid <- as.character(my_oto_medcodes_A$snomedctdescriptionid)
my_oto_medcodes_A$snomedctdescriptionid <- as.character(my_oto_medcodes_A$snomedctdescriptionid)

# ================================================================================

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Observation/")

observation_fileslist = list.files(pattern = ".dta")

# ATTENTION!!!
# The following lines takes days to run. Alternatively, split into at least 10 fragments.
# merge every observation file with Elliot's medcodes list so that only records with PO event remain
for(i in 1:length(observation_fileslist)){
  read_dta <- read_dta(file = observation_fileslist[i])
  read_dta$patid <- as.character(read_dta$patid)
  read_dta$medcodeid <- as.character(read_dta$medcodeid)
  merged_observation <- left_join(read_dta, my_oto_medcodes_A, by = "medcodeid")
  merged_observation <- merged_observation %>% filter(!is.na(Coding.Selection)) # filter records without PO diagnosis
  assign(paste0(observation_fileslist[i], "_PO_events"), merged_observation)
  print(observation_fileslist[i]) # to know the iteration number
}

# NOTE: file from aurum_allpatid_set38_extract_observation_026.dta
# was missing from part1

# unite all the merged files
u <- dplyr::union(aurum_allpatid_set3_extract_observation_001.dta_PO_events, 
                  aurum_allpatid_set3_extract_observation_002.dta_PO_events) # files 1+2

# append them manually (e.g. for i 3 to 771)
for(i in 301:length(observation_fileslist)){ # rest of files
  u <- dplyr::union(u, eval(parse(text = paste0(observation_fileslist[i], "_PO_events"))))
}

# save
# write.csv(u, "aurum_obs_merged_part2.csv")

# ---------------------------------------
# AFTER MERGING ALL THE SEPARATED PARTS:
# ---------------------------------------

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted")
u1 <- read.csv("aurum_obs_merged_part1.csv")
u2 <- read.csv("aurum_obs_merged_part2.csv")

u_final <- dplyr::union(u1, u2)

# deduplicate records that appear more than once
u_final <-u_final[!duplicated(u_final),] 

# count how many medcodeids are missing
sum(!is.na(u_final$medcodeid))

# count distinct patids
u_final %>% summarise(n_distinct(patid))

# clear, tidy and tranform into dataframe
u_final <- as.data.frame(u_final)
str(u_final)
u_final$patid <- as.character(u_final$patid)
u_final$staffid <- as.character(u_final$staffid)
u_final$medcodeid <- as.character(u_final$medcodeid)
u_final$myeventdate <- ymd(u_final$obsdate)
u_final$myenterdate <- ymd(u_final$enterdate)
u_final$X <- NULL
head(u_final)
summary(u_final)


# save the file as csv
# write.csv(u_final, file = "aurum_obs_PO_final.csv")
