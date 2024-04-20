
# ---------------------------------------------------------------------------------------------------
#                                  PRESENTATIONS ANALYSIS
# ---------------------------------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(data.table)
library(Epi)

# change dir and load data
setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted")

my_patients.pr <- read.csv("final_PO_ER_master_coh.csv")

my_patients.pr %>% summarise(P = n_distinct(patid))

my_patients.pr$patid <- as.character(my_patients.pr$patid)
my_patients.pr$entrydate <- as_date(my_patients.pr$entrydate)
my_patients.pr$exitdate <- as_date(my_patients.pr$exitdate)
my_patients.pr$myeventdate <- as_date(my_patients.pr$myeventdate)
my_patients.pr$mylcd <- as_date(my_patients.pr$mylcd)
my_patients.pr$myregstartdate <- as_date(my_patients.pr$myregstartdate)
my_patients.pr$myregenddate <- as_date(my_patients.pr$myregenddate)
my_patients.pr$myregenddate2 <- as_date(my_patients.pr$myregenddate2)
my_patients.pr$mycprd_ddate <- as_date(my_patients.pr$mycprd_ddate)
my_patients.pr$obs_period_start <- as_date(my_patients.pr$obs_period_start)
my_patients.pr$obs_period_end <- as_date(my_patients.pr$obs_period_end)


my_patients.pr <- my_patients.pr %>% 
  dplyr::select(c("patid", "pracid", "mygender", "myregion", 
           "e2019_imd_10", "entrydate", "exitdate", "myeventdate",
           "myage_obs.st", "PO_code_descr"))

my_patients.pr %>% summarise(P = n_distinct(patid))

# ----------------------------
# generate a lexis object
# ----------------------------

temp <- my_patients.pr

temp <- temp %>%
  group_by(patid, year(myeventdate)) %>%
  mutate(
    PO_event_count = sum(!is.na(myeventdate))
  ) %>% ungroup()

# keep only the 1st row per year per patid cause it saves the annual event count 
temp <- temp %>%
  rename("myevent_yr" = `year(myeventdate)`) %>%
  dplyr::select(-PO_code_descr, -myeventdate) %>%
  group_by(patid, myevent_yr) %>%
  arrange(patid, myevent_yr) %>%
  filter(row_number() == 1) %>%
  ungroup()

# count all events
sum(temp$PO_event_count)

temp2 <- Lexis(
  id = patid,
  entry = list(cal = cal.yr(entrydate)),
  exit = list(cal = cal.yr(exitdate)),
  exit.status = PO_event_count,
  data = temp
)

# group by patid and event year and count events per year per patid
sum(temp2$PO_event_count)

# split obs period according to index year
dm2 <- splitLexis(temp2, breaks = seq(2005, 2021, 1), time.scale = "cal")

dm2$cal_year <- as.integer(dm2$cal)

# count PO events and PYFU
sum(dm2$lex.Xst)
sum(dm2$lex.dur)

# --------------------------------------------------------------
# Count the events right
# --------------------------------------------------------------

# split the data and work only with patients who have the PO event; then, append
# them back to the initial dataset

k0 <- dm2 %>% filter(is.na(myevent_yr))
k0$myevents <- 0

k <- dm2 %>% filter(!is.na(myevent_yr))

sum(k$lex.Xst)
sum(k$lex.dur)

k <- k %>% mutate(myevents = if_else(myevent_yr == cal_year, PO_event_count, as.integer(0)))

sum(k$myevents)

k <- k %>% 
  group_by(patid, cal_year) %>% 
  arrange(patid, cal_year, desc(myevents)) %>% 
  filter(row_number() == 1) %>%
  ungroup()

sum(k$myevents)

# unite the slices
kk <- union(k, k0)

kk <- kk %>%
  mutate(
    denom_contr = lex.dur, # fu in years
    numer_contr = myevents 
  )

kk <- kk %>%
  mutate(
    myIMD = case_when(
      e2019_imd_10 %in% c(1, 2) ~ "1 (least deprived)",
      e2019_imd_10 %in% c(3, 4) ~ "2",
      e2019_imd_10 %in% c(5, 6) ~ "3",
      e2019_imd_10 %in% c(7, 8) ~ "4",
      e2019_imd_10 %in% c(9,10) ~ "5 (most deprived)",
      TRUE ~ "Missing"
    )
  )


# append yob to calculate age during fu:
setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted")
my_patients <- read.csv("final_PO_ER_master_coh.csv")

add.yob <- my_patients %>% dplyr::select(c(patid, yob)) %>% distinct()
add.yob$patid <- as.character(add.yob$patid)

kk <- left_join(kk, add.yob, by = "patid")

# same for age
kk <- kk %>% mutate(age.tvc = cal_year - yob)
kk <- kk %>% mutate(age.tvc = ifelse(age.tvc < 0, 0, age.tvc))
summary(kk$age.tvc)

# define agebands
kk <- kk %>%
  mutate(
    myageband = case_when(
      age.tvc >= 0 & age.tvc <= 2 ~  "(0-2]",
      age.tvc >  2 & age.tvc <= 5 ~  "(2-5]",
      age.tvc >  5 & age.tvc <= 8 ~  "(5-8]",
      age.tvc >  8 ~ "(8-16]"
    )
  )

# write.csv(kk, "~/Oto23/Data/Extracted/kk.csv")

# ------------------------------------------------------------------------------

# Presentations summary
presentation_data <- kk %>%
  select(c(cal_year, myageband, mygender, myregion, myIMD, denom_contr, numer_contr)) %>%
  group_by(cal_year, myageband, mygender, myregion, myIMD) %>%
  mutate(
    denom = sum(denom_contr),
    numer = sum(numer_contr)
  ) %>% 
  select(-denom_contr, -numer_contr) %>%
  distinct()



# -------------------
# Overall rates
# -------------------

crude_rates_0 <- presentation_data %>%
  group_by(cal_year) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_PR_1000 = numer/denom*1000) %>%
  ungroup()

# Standardized presentations (overall) 
population_presentations <- presentation_data %>%
  group_by(myregion, mygender, myIMD, myageband) %>%
  mutate(temp1 = sum(denom)) %>%
  ungroup() %>%
  mutate(
    temp2 = sum(denom), 
    prop = temp1/temp2,
    temp3 = prop * numer/denom
  ) 

population_presentations <- population_presentations %>%
  group_by(cal_year) %>%
  mutate(temp = sum(denom)) %>% ungroup() %>%
  select(-denom, -numer) %>%
  rename("denom.s" = "temp") %>%
  group_by(cal_year) %>%
  mutate(
    Stand.rate = sum(temp3)) %>% 
  ungroup() 

population_presentations <- population_presentations %>%
  mutate(
    numer.s = Stand.rate*denom.s
  ) %>%
  select(cal_year, denom.s, numer.s) %>% distinct()

standardised_PR <- population_presentations %>% 
  select(c(cal_year, numer.s, denom.s)) %>% 
  distinct() %>%
  mutate(PR_Stand_per1000 = numer.s/denom.s*1000)


# table to print
Table_PR_overall <- left_join(crude_rates_0, standardised_PR, by = "cal_year")

setwd("~/Oto23/Results")

write.csv(Table_PR_overall, "Table_PR_overall.csv")


# --------------------------
# GG GRAPHS
# --------------------------

library(ggplot2)

standardised_PR %>% 
  filter(cal_year <= 2019) %>%
  ggplot(aes(cal_year, PR_Stand_per1000)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Presentation rate per 1,000 person-years") +
  ylim(0,4)



# -------------------
# Crude ER by gender
# -------------------

crude_rates <- presentation_data %>%
  group_by(mygender, cal_year) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_PR_1000 = numer/denom*1000) %>%
  ungroup()

library(ggplot2)

crude_rates %>%
  mutate(
    gender = if_else(mygender == 0, "Male", "Female"),
    PR_per1000 = numer/denom * 1000
  ) %>% filter(cal_year <= 2019) %>%
  ggplot(aes(cal_year, PR_per1000, color = gender)) +
  geom_line() +
  ylim(0,4)

print(crude_rates)

# Standardized presentations by gender 
population_presentations <- presentation_data %>%
  group_by(myregion, myIMD, myageband) %>%
  mutate(temp1 = sum(denom)) %>%
  ungroup() %>%
  mutate(
    temp2 = sum(denom), 
    prop = temp1/temp2,
    temp3 = prop * numer/denom
  ) 

population_presentations <- population_presentations %>%
  group_by(mygender, cal_year) %>%
  mutate(temp = sum(denom)) %>% ungroup() %>%
  select(-denom, -numer) %>%
  rename("denom.s" = "temp") %>%
  group_by(mygender, cal_year) %>%
  mutate(
    Stand.rate = sum(temp3)) %>% 
  ungroup() 

population_presentations <- population_presentations %>%
  mutate(
    numer.s = Stand.rate*denom.s
  ) %>%
  select(mygender, cal_year, denom.s, numer.s) %>% distinct()

standardised_PR <- population_presentations %>% 
  select(c(mygender, cal_year, numer.s, denom.s)) %>% 
  distinct() %>%
  mutate(PR_Stand_per1000 = numer.s/denom.s*1000)


# table to print
Table_PR_gender <- left_join(crude_rates, standardised_PR, by = c("cal_year", "mygender"))

setwd("~/Oto23/Results")

write.csv(Table_PR_gender, "Table_PR_gender.csv")

# --------------------------
# GG GRAPHS
# --------------------------

library(ggplot2)

standardised_PR %>% 
  filter(cal_year <= 2019) %>%
  mutate(
    Gender = if_else(mygender == 0, "Male", "Female")
  ) %>%
  ggplot(aes(cal_year, PR_Stand_per1000, color = Gender)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Presentation rate per 1,000 person-years") +
  scale_fill_discrete(name = "Gender")+
  ylim(0,4)

