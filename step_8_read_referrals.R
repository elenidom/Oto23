
#################################################################################
#                                           *
#                                       Otorrhoea
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                       July 2023
#                                      CPRD Aurum
#                                           *
#
# Aim: 
# -----
# The following lines read all the referral files in Aurum 
# and merge them to the master cohort (patids between 0-16 years). The final merged
# file is saved ( datafrane : u ) as 'final_PO_master_coh_referrals.csv'.
#
#################################################################################

library(dplyr)
library(lubridate)
library(haven)

# read the patids that are included in the master cohort; those are the ones with which
# the referral files will be merged
mypatids_PO <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted/final_PO_master_coh.csv")
mypatids_PO$patid <- as.character(mypatids_PO$patid)
mypatids_PO <- mypatids_PO %>% select(patid) %>% distinct()

# work with the referrals
setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Referral/")

ref_fileslist = list.files(pattern = ".dta")

# Read every referral file and keep only those patids that exist
# in the master file
for(i in 1:length(ref_fileslist)){
  read_dta <- read_dta(file = ref_fileslist[i])
  read_dta$patid <- as.character(read_dta$patid)
  read_dta$obsid <- as.character(read_dta$obsid)
  merged_ref <- left_join(mypatids_PO, read_dta, by = "patid")
  merged_ref <- merged_ref %>% filter(!is.na(obsid))
  assign(paste0("master_coh_ref", i), merged_ref)
  print(i) # print the iteration number
}

# unite all the merged files
u <- dplyr::union(master_coh_ref1, 
                  master_coh_ref2) # files 1+2

for(i in 3:length(ref_fileslist)){ # rest of files
  u <- dplyr::union(u, eval(parse(text = paste0("master_coh_ref", i))))
}

# save
# write.csv(u, "/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted/final_PO_master_coh_referrals.csv")

rm(list = ls(pattern = "^master_"))
rm(list = ls(pattern = "^aurum_"))
rm(read_dta)
