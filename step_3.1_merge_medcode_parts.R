
# The following lines merge the separate parts of the medcode files from Aurum that 
# contain medocdes of PO for all the patients in CPRD Aurum.

library(dplyr)
library(lubridate)
library(data.table)

setwd("~/Oto23/Data/Extracted")

# read files
med_dat <- read.csv("aurum_obs_PO_final.csv")
head(med_dat)
str(med_dat)


# Variable conversion
med_dat$patid <- as.character(med_dat$patid)
med_dat$medcodeid <- as.character(med_dat$medcodeid)
med_dat$myobsdate <- ymd(med_dat$obsdate)
med_dat$myenterdate <- ymd(med_dat$enterdate)

# filter to keep only the primary diagnostic code
med_dat <- med_dat %>% filter(Coding.Selection == "Primary")

# check differences between enter and event date
sum(med_dat$myenterdate == med_dat$myobsdate, na.rm = T)
sum(med_dat$myenterdate != med_dat$myobsdate, na.rm = T)
sum(is.na(med_dat$myobsdate))

# number of patients with PO events (any time)
med_dat %>% summarise(n_distinct(patid))

# keep only the first ever recorded event for every patid,
# because we want incident cases of otorrhoea:
# med_dat <- med_dat %>% 
#   group_by(patid) %>%
#   arrange(myobsdate) %>%
#   filter(row_number() == 1) %>%
#   ungroup()


