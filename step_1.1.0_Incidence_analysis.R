
# -------------------------------------------------------------------------
# Identify records with diagnostic date of PO out of the obs period limits
# -------------------------------------------------------------------------

# PO diagnosis within obs period:
# -------------------------------
# TRUE:   Patient has an event during the obs period
# FALSE:  Patient has an event but not during the obs period (historic or after 2021 June)
# NA:     Patient does not have an event (control group)
my_patients$PO_within_obs <- with(my_patients, myeventdate %within% obs_period)

# remove patients with historic codes
summary(my_patients$PO_within_obs) 

# FALSE => patients with historic events or after 2021; remove them
# TRUE => event within obs only
# NA => control
my_patients <- my_patients %>% filter(is.na(PO_within_obs) | PO_within_obs == TRUE)


# replace NAs with FALSEs
#my_patients <- data.table(my_patients)[ , PO_within_obs := if_else(is.na(PO_within_obs), FALSE, TRUE)]
#my_patients <- data.table(my_patients)[ , PO_within_obs := if_else(is.na(myeventdate), FALSE, PO_within_obs)]

# filter the first row of every patient. This will either gonna be a patient
# without the event, patients with historic events (exclude), patients with
# first recorded event during the obs period (keep) or patients with events
# after the obs period (keep as unexposed):
my_patients <- my_patients %>% group_by(patid) %>% mutate(rank = row_number()) %>% ungroup()

# ---------------------------------------------------------------
#                       INCIDENCE ANALYSIS
# ---------------------------------------------------------------

my_patients.inc <- my_patients %>% filter(rank == 1)

# -------------------------
# define exit date
# -------------------------

my_patients.inc <- data.table(my_patients.inc)[ , exitdate := min(myregenddate2, mycprd_ddate, obs_period_end, na.rm = TRUE), by = "patid"]
summary(my_patients.inc$exitdate)
summary(my_patients.inc$entrydate)
summary(my_patients.inc$myeventdate)


# clean dates
sum(my_patients.inc$exitdate < my_patients.inc$entrydate)
my_patients.inc <- my_patients.inc %>% filter(my_patients.inc$exitdate > my_patients.inc$entrydate)

# censor at 1st PO event
my_patients.inc <- data.table(my_patients.inc)[, PO_within_obs:= if_else(is.na(PO_within_obs), FALSE, PO_within_obs)]
my_patients.inc <- data.table(my_patients.inc)[ , exitdate := if_else(PO_within_obs == T & myeventdate <= exitdate, myeventdate, exitdate)]

# define age at exit date
my_patients.inc <- my_patients.inc[, age_at.exit := as.integer(time_length(exitdate - entrydate, unit = "year") + myage_obs.st), ]
summary(my_patients.inc$age_at.exit)

# censor the fu when patients become 16 years old and redefine exit date
#my_patients.inc <- my_patients.inc[, exitdate2 := ifelse(age_at.exit > 16, exitdate %m-% years(age_at.exit - 16), exitdate), ]
my_patients.inc <- my_patients.inc %>%
  mutate(
    exitdate2 = if_else(age_at.exit >= 16, exitdate %m-% years(age_at.exit-16), exitdate)
  )

# select the earliest exit date
my_patients.inc <- my_patients.inc[ , exitdate := min(exitdate, exitdate2), by = "patid"]

summary(my_patients.inc$exitdate)
summary(my_patients.inc$entrydate)

# redefine age at exit date
my_patients.inc <- my_patients.inc[ , age_at.exit := as.integer(time_length(exitdate - entrydate, unit = "year") + myage_obs.st), ]

# For kids younger than one year, enter the months instead of having age = 0
my_patients.inc <- my_patients.inc[ , age_at.exit := ifelse(age_at.exit == 0 & !is.na(mob), mob/12, age_at.exit), ]

summary(my_patients.inc$age_at.exit)

# date checks
with(my_patients.inc, sum(age_at.exit < myage_obs.st))
with(my_patients.inc, sum(entrydate < obs_period_start))
with(my_patients.inc, sum(exitdate > obs_period_end))
with(my_patients.inc, sum(entrydate > exitdate))

# clean data
my_patients.inc <- my_patients.inc %>% filter(entrydate < exitdate)

with(my_patients.inc, sum(myeventdate > exitdate, na.rm = T))

my_patients.inc$entrydate <- as_date(my_patients.inc$entrydate)
my_patients.inc$exitdate <- as_date(my_patients.inc$exitdate)
my_patients.inc$myeventdate <- as_date(my_patients.inc$myeventdate)

# remove events that fall out of the obs period
part1 <- my_patients.inc %>% filter(is.na(myeventdate)) # controls
part1 %>% summarise(P = n_distinct(patid))

part2 <- my_patients.inc %>% filter(!is.na(myeventdate) & myeventdate <= exitdate) # event in obs
part2 %>% summarise(P = n_distinct(patid))

part3 <- my_patients.inc %>% filter(!is.na(myeventdate) & myeventdate > exitdate) # event out of obs
part3 %>% summarise(P = n_distinct(patid))

my_patients.inc <- union(part2, part1)
my_patients.inc %>% summarise(P = n_distinct(patid))

# histograms 
hist(my_patients.inc$age_at.exit)
hist(my_patients.inc$myage_obs.st)


# ------------------------------------------------------------------------------------------
#                                  OVERALL IR CALCULATIONS
# ------------------------------------------------------------------------------------------
# 
# 
# # ------
# # PYs
# # ------
# my_patients.inc <- data.table(my_patients.inc)[ , py := time_length(exitdate - entrydate, unit = "years"), ]
# summary(my_patients.inc$py)
# 
# # ----------
# # PO Events
# # ----------
# 
# my_patients.inc <- data.table(my_patients.inc)[ , PO_event := if_else(PO_within_obs == T, 1, 0), ]
# 
# # -------------
# # IR (95% CI)
# # -------------
# 
# PY = sum(my_patients.inc$py) # PY
# Events = sum(my_patients.inc$PO_event) # PO events
# IR = Events / PY * 1000 # IR per 1,000 PY
# 
# # 95% CI
# a = sqrt(Events) * 1.96
# a1 = Events + a
# a2 = Events - a
# b = 1000/PY
# upper_lim = b * a1
# lower_lim = b * a2
# 
# # ---------------------
# # Events per year
# # ---------------------
# 
# my_patients.inc %>% 
#   group_by(year(myeventdate)) %>%
#   summarise(Events_annually = sum(PO_event),
#             PY_annually = sum(py))
# 
# 
# ----------------------
# save this final cohort
# ----------------------

# write.csv(my_patients.inc, "final_PO_IR_master_coh.csv")

my_patients.inc <- read.csv("~/Oto23/Data/Extracted/final_PO_IR_master_coh.csv")


# ------------------------------------------------------------------------------------------
#                        ANNUAL CRUDE IR AND STANDARDISED CALCULATIONS
# ------------------------------------------------------------------------------------------

library(Epi)
library(dplyr)
library(lubridate)

my_patients.inc.ann <- my_patients.inc %>% 
  select(c("patid", "pracid", "yob", "mygender", "myregion", "myage_obs.st",
           "e2019_imd_10", "entrydate", "exitdate", "py", "myeventdate", 
           "PO_code_descr", "PO_event"))


# ----------------------------
# create a lexis object
# ----------------------------

my_lexis <- Lexis(
  id = patid,
  entry = list("cal" = cal.yr(entrydate)),
  exit = list("cal" = cal.yr(exitdate)),
  exit.status = PO_event, 
  data = my_patients.inc.ann
)

head(my_lexis)
summary.Lexis(my_lexis)
# plot(my_lexis)

# split obs period according to index year
dm1 <- splitLexis(my_lexis, breaks = seq(2005, 2021, 1), time.scale = "cal")


# define age during the fu
dm1 <- dm1 %>% group_by(patid) %>% mutate(age = myage_obs.st + cumsum(lex.dur)) %>% ungroup()


# define IMD and age bands
dm1 <- dm1 %>%
  mutate(
    myIMD = case_when(
      e2019_imd_10 %in% c(1, 2) ~ "1 (least deprived)",
      e2019_imd_10 %in% c(3, 4) ~ "2",
      e2019_imd_10 %in% c(5, 6) ~ "3",
      e2019_imd_10 %in% c(7, 8) ~ "4",
      e2019_imd_10 %in% c(9,10) ~ "5 (most deprived)",
      TRUE ~ "Missing"
    ),
    myageband = case_when(
      age >= 0 & age <= 2 ~  "(0-2]",
      age >  2 & age <= 5 ~  "(2-5]",
      age >  5 & age <= 8 ~  "(5-8]",
      age >  8 ~ "(8-16]"
    )
  )

# define numerator/denominator values
dm1 <- dm1 %>%
  mutate(
    denom_contr = lex.dur, # fu in years
    numer_contr = lex.Xst
  )

dm1$index_year <- as.integer(dm1$cal)


# ------------------------------------------------------------------------------
# Calculate denominators and numerators by gender, region, quintile, and ageband 
# ------------------------------------------------------------------------------

library(ggplot2)

incidence_data <- dm1 %>%
  select(c(index_year, mygender, myregion, myIMD, myageband, denom_contr, numer_contr)) %>%
  group_by(index_year, mygender, myregion, myIMD, myageband) %>%
  mutate(
    denom = sum(denom_contr),
    numer = sum(numer_contr)
  ) %>% ungroup() %>%
  select(-denom_contr, -numer_contr) %>%
  distinct()

# -------------------
# Overall rates
# -------------------

crude_rates_0 <- incidence_data %>%
  group_by(index_year) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_IR_1000 = numer/denom*1000) %>%
  ungroup()


# Standardized incidence (overall) 
population_incidence <- incidence_data %>%
  group_by(myregion, mygender, myIMD, myageband) %>%
  mutate(temp1 = sum(denom)) %>%
  ungroup() %>%
  mutate(
    temp2 = sum(denom), 
    prop = temp1/temp2,
    temp3 = prop * numer/denom
  ) 

population_incidence <- population_incidence %>%
  group_by(index_year) %>%
  mutate(temp = sum(denom)) %>% ungroup() %>%
  select(-denom, -numer) %>%
  rename("denom.s" = "temp") %>%
  group_by(index_year) %>%
  mutate(
    Stand.rate = sum(temp3)) %>% 
  ungroup() 

population_incidence <- population_incidence %>%
  mutate(
    numer.s = Stand.rate*denom.s
  ) %>%
  select(index_year, denom.s, numer.s) %>% distinct()

standardised_IR <- population_incidence %>% 
  select(c(index_year, numer.s, denom.s)) %>% 
  distinct() %>%
  mutate(IR_Stand_per1000 = numer.s/denom.s*1000)


# table to print
Table_IR_overall <- left_join(crude_rates_0, standardised_IR, by = "index_year")

setwd("~/Oto23/Results")

write.csv(Table_IR_overall, "Table_IR_overall.csv")

# --------------------------
# GG GRAPHS
# --------------------------

library(ggplot2)

standardised_IR %>% 
  filter(index_year <= 2019) %>%
  ggplot(aes(index_year, IR_Stand_per1000)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +
  ylim(0,4)


# -------------------
# Crude IR by gender
# -------------------

crude_rates <- incidence_data %>%
  group_by(index_year, mygender) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_IR_1000 = numer/denom*1000) %>%
  ungroup()


crude_rates %>%
  mutate(
    gender = if_else(mygender == 0, "Male", "Female"),
    IR_per1000 = numer/denom * 1000
  ) %>% filter(index_year <= 2019) %>%
  ggplot(aes(index_year, IR_per1000, color = as.factor(mygender))) +
  geom_line() +
  ylim(0,4)

print(crude_rates)

# Standardized incidence by gender 
population_incidence <- incidence_data %>%
  group_by(myregion, myIMD, myageband) %>%
  mutate(temp1 = sum(denom)) %>%
  ungroup() %>%
  mutate(
    temp2 = sum(denom), 
    prop = temp1/temp2,
    temp3 = prop * numer/denom
  ) 

population_incidence <- population_incidence %>%
  group_by(mygender, index_year) %>%
  mutate(temp = sum(denom)) %>% ungroup() %>%
  select(-denom, -numer) %>%
  rename("denom.s" = "temp") %>%
  group_by(mygender, index_year) %>%
  mutate(
    Stand.rate = sum(temp3)) %>% 
  ungroup() 

population_incidence <- population_incidence %>%
  mutate(
    numer.s = Stand.rate*denom.s
  ) %>%
  select(mygender, index_year, denom.s, numer.s) %>% distinct()

standardised_IR <- population_incidence %>% 
  select(c(mygender, index_year, numer.s, denom.s)) %>% 
  distinct() %>%
  mutate(IR_Stand_per1000 = numer.s/denom.s*1000)


# table to print
Table_IR_gender <- left_join(crude_rates, standardised_IR, by = c("index_year", "mygender"))

setwd("~/Oto23/Results")

write.csv(Table_IR_gender, "Table_IR_gender.csv")

# --------------------------
# GG GRAPHS
# --------------------------

library(ggplot2)

standardised_IR %>% 
  filter(index_year <= 2019) %>%
  mutate(
    Gender = if_else(mygender == 0, "Male", "Female")
  ) %>%
  ggplot(aes(index_year, IR_Stand_per1000, color = Gender)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +
  ylim(0,4)


