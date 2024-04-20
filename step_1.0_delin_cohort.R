
library(data.table)
library(dplyr)
library(lubridate)

# '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#
#  ==> cohort saved as "init_PO_master_coh.csv"
#  ==> read: 
#        my_patients <- read.csv("~/Oto23/Data/Extracted/init_PO_master_coh.csv")
#
# '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# ---------------------------------------
# NEW VARIABLES: obs_period_start/end
# ---------------------------------------

my_patients$patid <- as.character(my_patients$patid)
my_patients$obs_period_start <- ymd("2005-01-01") # start of follow-up
my_patients$obs_period_end   <- ymd("2021-06-30") # end of follow-up

# define obs period:
my_patients$obs_period <- my_patients$obs_period_start %--% my_patients$obs_period_end

str(my_patients)

# -----------
# FIX DATES
# -----------
#
# If registration end is missing, it means that the patients did not die or did
# not have a tod. Therefore, s/he is currently registered with a GP practice.

my_patients$myregstartdate <- as.Date(my_patients$myregstartdate)
my_patients$myregenddate <- as.Date(my_patients$myregenddate)
my_patients$mycprd_ddate <- as.Date(my_patients$mycprd_ddate)
my_patients$mylcd <- as.Date(my_patients$mylcd)

my_patients <- data.table(my_patients)[ , myregenddate2 := if_else(is.na(myregenddate), 
                                        as.Date("2021-06-30"), myregenddate)] # June 2021 build

summary(my_patients$myregenddate2)


# -------------------------
# Define entry date
# -------------------------

summary(my_patients$myregstartdate)

# If registration start date follows the start of the obs period, 
# then assign the registration start:
my_patients <- my_patients %>%
  mutate(
    entrydate = if_else(myregstartdate > obs_period_start, 
                        myregstartdate, #%m+% months(12), # in case we want to account for 1-year continuous  registration with the GP
                        obs_period_start)
  )

summary(my_patients$entrydate)

# --------------------------------------------------
# NEW VARIABLES = myage, exit/entry date 
# --------------------------------------------------

# define age at obs start:
my_patients <- my_patients %>%
  mutate(
    year_regst = year(entrydate),
    myage_obs.st = if_else(year_regst < yob, yob-year_regst, year_regst-yob)
  )

summary(my_patients$myage_obs.st)

# Exclude patids with age > 16 at obs start:
my_patients <- my_patients %>% filter(myage_obs.st < 16)

# -----------------------
# save at this stage 
# -----------------------

sevenM_under16_cohort <- my_patients
write.csv(sevenM_under16_cohort, "~/Oto23/Data/Extracted/sevenM_under16_cohort.csv")

# -------------------------
# PO events
# -------------------------
#
# Exit date definition requires the diagnosis date of the first PO
# event. We read the medcodes file than contains only PO
# medcodes and we will use the event date to censor the fu.

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted")
mymed_PO <- read.csv("aurum_obs_PO_final.csv")
str(mymed_PO)
mymed_PO$myeventdate <- as.Date(mymed_PO$myeventdate)
mymed_PO$patid <- as.character(mymed_PO$patid)

# keep only some vars
mymed_PO <- mymed_PO %>% select(c(patid, myeventdate, Aurum.Database, readcode, Coding.Selection)) %>%
  rename("PO_code_descr" = "Aurum.Database")

mymed_PO <- mymed_PO %>% filter(Coding.Selection == "Primary")

# keep any recorded event; we then need to exclude patids with history of PO
# because we want incident cases of otorrhoea:
mymed_PO <- mymed_PO %>% 
  group_by(patid) %>%
  arrange(myeventdate) %>%
  ungroup()


# merge medical codes with master cohort:
my_patients <- left_join(my_patients, mymed_PO, by = "patid")


