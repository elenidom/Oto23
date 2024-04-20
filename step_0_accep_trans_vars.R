#############################################################################################
#                                           *
#                                    ELENI DOMZARIDOU
#                                   Otorrhoea PROJECT
#                                UNIVERSITY OF MANCHESTER
#                            Summer 2023 - Stopford Building
#                                       CPRD-Aurum  
#                                           *
# ..........................................................................................
#
# [*] The following lines use the CPRD's Aurum denominator file to extract the cohort for
#     the paediatric otorrhoea project.
# 
# [*] The dataframe is called 'my_patients'.
# 
# [*] Cleaning according to acceptable patids, IMD linkage eligibility, migrators.
# 
# [*] Cleaning dates and definition of the obs period undertaken at step_2_[...].R
#
#############################################################################################

setwd("~/Oto23/Data")

library(haven)
library(lubridate)
library(dplyr)
library(data.table)

# ---------------------------------------------
# CLEAN ACCORDING TO ACCEPTABLE PATIDS
# ---------------------------------------------

# read denominator file
all_pat_flat <- read_dta("./Denominators/202106_cprdaurum_allpats.dta")
head(all_pat_flat)

# keep only acceptable patients
my_patients <- all_pat_flat %>% filter(acceptable == 1)
head(my_patients)
str(my_patients)
my_patients$patid <- as.character(my_patients$patid)
my_patients$pracid <- as.character(my_patients$pracid)

# ---------------------------------------------
# CLEAN ACCORDING TO LINKAGE ELIGIBILITY
# ---------------------------------------------

# Eligible patids for linkage with IMD data - set wd
setwd("~/Steroids23/Set22")

# unzip linkage eligibility file Set 21 ()
# unz_set21 <- fread('unzip -p set_21_Source_Aurum.zip', fill = T)

# remove first 12 rows because they contain info about eligibility instead of patient data
# unz_set21 <- unz_set21[-c(1:12),]
# colnames(unz_set21) <- c("patid", "pracid", "linkdate", "hes_e", "death_e", "cr_e", "lsoa_e", "mh_e") # rename columns
# unz_set21 <- unz_set21[-1,]
# unz_set21 <- as.data.frame(unz_set21)


# keep only patids with linkage for IMD
# unz_set21 <- unz_set21 %>%
#   filter(lsoa_e == 1) %>%
#   select(c(patid, pracid, lsoa_e))
# 
# unz_set21$patid <- as.character(unz_set21$patid)
# unz_set21$pracid <- as.character(unz_set21$pracid)

# merge linkage eli for IMD with my_patients data
# my_patients <- left_join(my_patients, unz_set21, by = c("patid", "pracid"))

# keep only patids with linkage for IMD
# my_patients$lsoa_e <- as.numeric(my_patients$lsoa_e)
# my_patients <- my_patients %>% filter(lsoa_e == 1)

# ---------------------------------------------
# CLEAN ACCORDING TO MIGRATORS - run in GOLD
# ---------------------------------------------

# setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data")
# migrators <- read_dta("visiontoemismigrators.dta")
# migrators$pracid <- as.character(migrators$emis_pracid)
# 
# keep to Aurum patients who migrated from GOLD to Aurum:
# temp <- dplyr::anti_join(my_patients, migrators, by = "pracid")

# ---------------------------------------------
# Transform variables
# ---------------------------------------------

# Dates
my_patients$myregstartdate <- as.Date(my_patients$regstartdate)
my_patients$myregenddate <- as.Date(my_patients$regenddate)
my_patients$mycprd_ddate <- as.Date(my_patients$cprd_ddate)
my_patients$myemis_ddate <- as.Date(my_patients$emis_ddate)
my_patients$myuts <- as.Date(my_patients$uts)
my_patients$mylcd <- as.Date(my_patients$lcd)

# Alternatively
#my_patients <- data.table(my_patients)[, myemis_ddate := as.Date(emis_ddate)]

# Factor vars
my_patients$mygender <- as.factor(my_patients$gender) 

# filter patids with "I" for gender (classified as 0; 1; 2; NA)
# 0 = MALE
# 1 = FEMALE
my_patients <- my_patients %>% filter(mygender %in% c(0, 1))

my_patients$myregion <- as.factor(my_patients$region)

str(my_patients)
summary(my_patients)

dput(names(my_patients))

# -----------------------------------------------------
# append practice-level IMD data from Matt (18/7/2023)
# -----------------------------------------------------

prac_IMD.m <- read_dta(file = "/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/practice_imd_from_Matt.dta")
prac_IMD.m <- prac_IMD.m %>% select(c(pracid, e2019_imd_10)) %>% distinct()
prac_IMD.m$pracid <- as.character(prac_IMD.m$pracid)

# merge with master cohort
my_patients <- left_join(my_patients, prac_IMD.m, by = "pracid")

# check
my_patients %>% 
  group_by(e2019_imd_10) %>% 
  summarise(P = n_distinct(patid), Per = P/dim(my_patients)[1]*100)

# remove vars
my_patients <- my_patients %>% 
  select(c("patid", "pracid", "yob", "mob", "patienttypeid",   
           "myregstartdate", "myregenddate", "mycprd_ddate", 
           "mylcd", "mygender", "myregion", "e2019_imd_10"))

head(my_patients)

