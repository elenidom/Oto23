
#######################################################################################################
#                                           *
#                                       Otorrhoea
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                       July 2023
#                                      CPRD Aurum
#                                           *
#######################################################################################################

# read the merged files that contains referral data for the patids in your master cohort
ref_master <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted/final_PO_master_coh_referrals.csv")
head(ref_master)
ref_master$patid <- as.character(ref_master$patid)
ref_master$pracid <- as.character(ref_master$pracid)
ref_master$obsid <- as.character(ref_master$obsid)
ref_master$myref <- 1

# read the merged observation files
obs_merged_part1 <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted/aurum_obs_merged_part1.csv")
obs_merged_part2 <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted/aurum_obs_merged_part2.csv")
obs_merged <- union(obs_merged_part1, obs_merged_part2)
obs_merged$obsid <- as.character(obs_merged$obsid)
obs_merged$patid <- as.character(obs_merged$patid)
obs_merged$pracid <- as.character(obs_merged$pracid)


# merge the observation files with the referrals
ref_obs_merged <- left_join(obs_merged, ref_master, by = c("obsid", "patid", "pracid"))
ref_obs_merged$X.x <- NULL
ref_obs_merged$X.y <- NULL
#ref_obs_merged$refsourceorgid <- NULL # not populated from CPRD
#ref_obs_merged$reftargetorgid <- NULL # not populated from CPRD

# keep only the primary PO codes
ref_obs_merged <- ref_obs_merged %>% filter(Coding.Selection == "Primary")


# -----------------------------------------------------------------
# Search the observation files for referral medcodes
# -----------------------------------------------------------------

library(dplyr)
library(lubridate)
library(haven)

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Lookups/")

# Identify referral medical codes in the CPRD Aurum dictionary (Elliot reviewed them)
my_oto_refcodes_A <- read.table("Aurum_medcodes_ref_for_otorrhoea.txt", header = T, sep = "", fill = T)
my_oto_refcodes_A <- my_oto_refcodes_A %>% filter(Refferal_to_second_Care_EH == "Y")
my_oto_refcodes_A$medcodeid <- as.character(my_oto_refcodes_A$medcodeid)
my_oto_refcodes_A$aa <- NULL
my_oto_refcodes_A$snomedctconceptid <- as.character(my_oto_refcodes_A$snomedctdescriptionid)
my_oto_refcodes_A$snomedctdescriptionid <- as.character(my_oto_refcodes_A$snomedctdescriptionid)

# Read the observation files and keep only those records with referral relevant codes
setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Observation/")

observation_fileslist = list.files(pattern = ".dta")

# ATTENTION!!!
# The following lines takes days to run. Alternatively, split into at least 10 fragments.
# merge every observation file with Elliot's medcodes list so that only records with PO event remain
for(i in 1:length(observation_fileslist)){
  read_dta <- read_dta(file = observation_fileslist[i])
  read_dta$patid <- as.character(read_dta$patid)
  read_dta$medcodeid <- as.character(read_dta$medcodeid)
  merged_observation <- left_join(read_dta, my_oto_refcodes_A, by = "medcodeid")
  merged_observation <- merged_observation %>% filter(!is.na(Refferal_to_second_Care_EH)) # filter records without PO diagnosis
  assign(paste0("Obs_extr_ref_events", i), merged_observation)
  print(observation_fileslist[i]) # to know the iteration number
}

# unite all the merged files
u <- dplyr::union(Obs_extr_ref_events1, Obs_extr_ref_events2) # files 1+2

# append them manually (e.g. for i 3 to 771)
for(i in 3:length(observation_fileslist)){ # rest of files
  u <- dplyr::union(u, eval(parse(text = paste0("Obs_extr_ref_events", i))))
}

# ---------------------------------------
# AFTER MERGING ALL THE SEPARATED PARTS:
# ---------------------------------------

# deduplicate records that appear more than once
u_final <-u[!duplicated(u),] 

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
u_final$obsdate <- ymd(u_final$obsdate)
u_final$X <- NULL
head(u_final)
summary(u_final)

# save
setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted")

write.csv(u_final, "aurum_obs_merged_referrals.csv")

# rm(list = ls(pattern = "^Obs_extr_"))


# -------------------------------------------------------------------------------
# Filter the referral data from the obs files to keep only patids with PO
# incidence codes of :
# -------------------------------------------------------------------------------

obs_merged_ref <- u_final
obs_merged_ref$myeventdate <- NULL

my_patients <- read.csv("~/Oto23/Data/Extracted/final_PO_IR_master_coh.csv")
my_patients <- my_patients %>% filter(PO_event == 1) %>% select(c(patid, myeventdate))
my_patients$patid <- as.character(my_patients$patid)
my_patients$myeventdate <- as_date(my_patients$myeventdate)

obs_merged_ref$patid <- as.character(obs_merged_ref$patid)

# merge with obs file that contains referrals:
my_patients.r <- left_join(my_patients, obs_merged_ref, by = "patid")
my_patients.r <- my_patients.r %>% filter(!is.na(medcodeid))
my_patients.r %>% summarise(P = n_distinct(patid))

# ==> same date event
my_patients.r_sameday <- my_patients.r %>% filter(myeventdate == obsdate)

# ==> referral within 14 days since the PO diagnosis
my_patients.r_within14days <- my_patients.r %>% filter(myeventdate == obsdate)
