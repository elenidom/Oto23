
# ---------------------------------------------------------------------------------------------------
#                                  PRESENTATIONS ANALYSIS
# ---------------------------------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(data.table)

my_patients.pre <- read.csv("~/Oto23/Data/Extracted/final_PO_IR_master_coh.csv")

str(my_patients.pre)

dput(names(my_patients.pre))

my_patients.pre <- my_patients.pre %>%
  select(c("patid", "pracid", "yob", "mob", "patienttypeid", "myregstartdate", 
           "myregenddate", "mycprd_ddate", "mylcd", "mygender", "myregion", 
           "e2019_imd_10", "obs_period_start", "obs_period_end", "obs_period", 
           "myregenddate2", "entrydate", "myage_obs.st"))

# transform other vars into dates
my_patients.pre$patid <- as.character(my_patients.pre$patid)
my_patients.pre$entrydate <- as_date(my_patients.pre$entrydate)
my_patients.pre$mylcd <- as_date(my_patients.pre$mylcd)
my_patients.pre$myregstartdate <- as_date(my_patients.pre$myregstartdate)
my_patients.pre$myregenddate <- as_date(my_patients.pre$myregenddate)
my_patients.pre$myregenddate2 <- as_date(my_patients.pre$myregenddate2)
my_patients.pre$mycprd_ddate <- as_date(my_patients.pre$mycprd_ddate)
my_patients.pre$obs_period_start <- as_date(my_patients.pre$obs_period_start)
my_patients.pre$obs_period_end <- as_date(my_patients.pre$obs_period_end)

summary(my_patients.pre)

# -------------------------
# PO events
# -------------------------
#
# Read the medcodes file than contains only PO
# medcodes and we will use the event date to censor the fu.
# The file includes only Primary codes.
# RUN CODE step_3.1_merge_medcode_parts.R

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted")
mymed_PO <- med_dat
str(mymed_PO)
mymed_PO$myeventdate <- as.Date(mymed_PO$myeventdate)
mymed_PO$patid <- as.character(mymed_PO$patid)

# keep only some vars
mymed_PO <- mymed_PO %>% 
  filter(Coding.Selection == "Primary") %>%
  select(c(patid, myeventdate, Aurum.Database, readcode)) %>%
  rename("PO_code_descr" = "Aurum.Database")

# keep any recorded event; we then need to exclude patids with history of PO
# because we want incident cases of otorrhoea:
mymed_PO <- mymed_PO %>% 
  group_by(patid) %>%
  arrange(myeventdate) %>%
  ungroup()

summary(mymed_PO)

# filter records without event date
mymed_PO <- mymed_PO %>% filter(!is.na(myeventdate))

# keep only events that occurred within the obs period (patients with historic codes
# have been excluded from the IR cohort at previous stages):
# mymed_PO <- mymed_PO %>% filter(year(myeventdate) >= 2005)

# merge with master cohort
my_patients.pre <- left_join(my_patients.pre, mymed_PO, by = "patid")

# Remove patids with historic PO events - PO diagnosis within obs period:
# -----------------------------------------------------------------------
# TRUE:   Patient has an event during the obs period
# FALSE:  Patient has an event but not during the obs period (historic or after 2021 July)
# NA:     Patient does not have an event (control group)

my_patients.pre$obs_period <- my_patients.pre$obs_period_start %--% my_patients.pre$obs_period_end

my_patients.pre$PO_within_obs <- with(my_patients.pre, myeventdate %within% obs_period)

summary(my_patients.pre$PO_within_obs)

# keep only the events that have a 6 weeks (42 days) interval between their occurrances
# (decision informed by Elliot's clinicians)

# run this command only for patients with PO otherwise the dataset is large and it takes time;
# we will then merge this back to the master cohort.
my_patients.cut1 <- my_patients.pre %>% filter(is.na(PO_within_obs)) # control group

# delete duplicated  PO events
my_patients.cut2 <- my_patients.pre %>%
  filter(PO_within_obs) %>%
  distinct()

my_patients.cut1 %>% summarise(P = n_distinct(patid))
my_patients.cut2 %>% summarise(P = n_distinct(patid))

# STEP 1: difference between first and current event
my_patients.cut2 <- my_patients.cut2 %>%
  group_by(patid) %>%
  mutate(
    temp1 = time_length(myeventdate - first(myeventdate), unit = "days")
  ) %>% ungroup()

# filter events that occurred in less than 42 days from the first event
my_patients.cut2 <- my_patients.cut2 %>% filter(temp1 >= 42 | temp1 == 0)
my_patients.cut2 %>% summarise(P = n_distinct(patid))

# STEP 2: difference between current and next event
my_patients.cut2 <- my_patients.cut2 %>%
  group_by(patid) %>%
  mutate(
    temp2 = time_length(myeventdate - lag(myeventdate, default = first(myeventdate)), unit = "days"),
  ) %>% ungroup()

# filter events that occurred in less than 42 days from the previous event
my_patients.cut2 <- my_patients.cut2 %>% filter(temp2 >= 42 | temp2 == 0)
my_patients.cut2 %>% summarise(P = n_distinct(patid))

summary(my_patients.cut2$temp2)
sum(my_patients.cut2$temp2 < 42 & my_patients.cut2$temp2 > 0)

my_patients.cut2$temp1 <- NULL
my_patients.cut2$temp2 <- NULL

# Append this sub cohort that contains only patients with POs, with the master cohort 
# which has the remaining patients.
my_patients <- dplyr::union(my_patients.cut2, my_patients.cut1)
my_patients %>% summarise(P = n_distinct(patid))

rm(my_patients.cut1, my_patients.cut2)


# -------------------------
# define exit date
# -------------------------

# if myregenddate is na, assign the last day of follow up (myregenddate2)
my_patients <- data.table(my_patients)[ , exitdate := min(myregenddate2, mycprd_ddate, obs_period_end, na.rm = TRUE), by = "patid"]
summary(my_patients$exitdate)

# check dates
sum(my_patients$exitdate < my_patients$entrydate)

# define age at exit date
my_patients <- my_patients[ , age_at.exit := as.integer(time_length(exitdate - entrydate, unit = "year") + myage_obs.st), ]
summary(my_patients$age_at.exit)

# redefine exit date when patients become 16 years old and 
my_patients <- my_patients[, exitdate2 := ifelse(age_at.exit >= 16, exitdate %m-% years(age_at.exit - 16), exitdate), ]
my_patients$exitdate2 <- as_date(my_patients$exitdate2)

# select the earliest exit date
my_patients <- my_patients[ , exitdate := min(exitdate, exitdate2), by = "patid"]

summary(my_patients$exitdate)
summary(my_patients$entrydate)

# redefine age at exit date
my_patients <- my_patients[ , age_at.exit := as.integer(time_length(exitdate - entrydate, unit = "year") + myage_obs.st), ]

# For kids younger than one year, enter the months instead of having age = 0
my_patients <- my_patients[ , age_at.exit := ifelse(age_at.exit == 0 & !is.na(mob), mob/12, age_at.exit), ]

summary(my_patients$age_at.exit)

# date checks
with(my_patients, sum(age_at.exit < myage_obs.st))
with(my_patients, sum(entrydate < obs_period_start))
with(my_patients, sum(exitdate > obs_period_end))
with(my_patients, sum(entrydate > exitdate))
with(my_patients, sum(myeventdate > exitdate, na.rm = T))
with(my_patients, sum(myeventdate < entrydate, na.rm = T))

# histograms 
hist(my_patients$age_at.exit)
hist(my_patients$myage_obs.st)

# remove events that fall out of the obs period
part1 <- my_patients %>% filter(is.na(myeventdate)) # controls
part1 %>% summarise(P = n_distinct(patid))

part2 <- my_patients %>% filter(!is.na(myeventdate) & myeventdate <= exitdate) # event in obs
part2 %>% summarise(P = n_distinct(patid))

part3 <- my_patients %>% filter(!is.na(myeventdate) & myeventdate > exitdate) # event out of obs
part3 %>% summarise(P = n_distinct(patid))

my_patients <- union(part2, part1)
my_patients <- union(my_patients, part3)
my_patients %>% summarise(P = n_distinct(patid))

# ------------------
# save this cohort
# ------------------

my_patients <- my_patients %>% 
  select(c("patid", "pracid", "yob", "mob", "patienttypeid", "myregstartdate", 
           "myregenddate", "mycprd_ddate", "mylcd", "mygender", "myregion", 
           "e2019_imd_10", "obs_period_start", "obs_period_end", "obs_period", 
           "myregenddate2", "entrydate", "myage_obs.st", "PO_code_descr", 
           "readcode", "PO_within_obs", "exitdate", "myeventdate"))


setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted")
# write.csv(my_patients, "final_PO_ER_master_coh.csv")

