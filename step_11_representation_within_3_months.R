
# --------------------------------------------------------------------
# Look at the proportion of patients in each group who have a 
# representation with otorrhoea within 3 months
# --------------------------------------------------------------------

library(dplyr)
library(lubridate)

# presentations data
mydat <- read.csv("~/Oto23/Data/Extracted/final_PO_ER_master_coh.csv")
mydat$patid <- as.character(mydat$patid)
dput(names(mydat))

mydat <- mydat %>% 
  select(c("patid", "pracid", "entrydate", "exitdate", "myeventdate")) %>% 
  distinct()

mydat$myeventdate <- as_date(mydat$myeventdate)

# keep only patid with the event
mydat <- mydat %>% filter(!is.na(myeventdate))
mydat <- mydat %>% filter(year(myeventdate) <= 2019)

mydat %>% summarise(P = n_distinct(patid))

# rank the events per patid
mydat <- mydat %>%
  group_by(patid) %>%
  arrange(patid, myeventdate) %>%
  mutate(
    ev.rank = row_number()
  ) %>% ungroup()

summary(mydat$ev.rank)

# exclude patients with one and only event
mydat <- mydat %>% group_by(patid) %>%
  mutate(only.one.ev = if_else(max(ev.rank) == 1, T, F)) %>%
  ungroup()

mydat %>% group_by(only.one.ev) %>% summarise(P = n_distinct(patid))

mydat <- mydat %>% filter(only.one.ev == F)

mydat$only.one.ev <- NULL

summary(mydat$ev.rank)

# all events per patient
mydat <- mydat %>%
  group_by(patid) %>%
  arrange(patid) %>%
  mutate(
    all.ev = n()
  ) %>% ungroup()

summary(mydat$all.ev)

# Number of patients in each category
mydat %>% group_by(all.ev) %>% summarise(P = n_distinct(patid))

# histogram
hist(mydat$all.ev, breaks = 100, xlim = c(0, 20), main = "Frequency of representations", xlab = "Events")

# representation within 3 months
mydat <- mydat %>%
  group_by(patid) %>%
  mutate(
    within3m = ifelse(time_length(lead(myeventdate, default = last(myeventdate)) - myeventdate, unit = "months") <= 3, T, F)
  ) %>% ungroup()

summary(mydat$within3m)

head(mydat)

# remove data from the last row because the previous mutate compares this last row with itself
mydat <- mydat %>%
  group_by(patid) %>%
  mutate(
    within3m = ifelse(row_number() == n(), NA, within3m)
  ) %>% ungroup()

mydat %>% group_by(within3m) %>% summarise(P = n_distinct(patid))

mydat %>% summarise(P = n_distinct(patid))

# there are patients with more than 2 events; if more than 2 events had <= 3 months difference
# patients will be included in both categories (double-count). Therefore, we need another var to
# count patients with event within/no within 3 months
mydat2 <- mydat %>% filter(!is.na(within3m))
mydat2 %>% group_by(within3m) %>% summarise(P = n_distinct(patid))

# filter patients without an event within 3 months
mydat2 <- mydat2 %>% filter(within3m)

# patients with ABs exposure (result from step_6 code)
ab_prescr.pats <- read.csv("~/Oto23/Data/Extracted/ab_prescr.pats.csv")
ab_prescr.pats$patid <- as.character(ab_prescr.pats$patid)
head(ab_prescr.pats)
ab_prescr.pats$X <- NULL
ab_prescr.pats <- ab_prescr.pats %>% distinct()

# flag patients who received ABs (1) and those who did not (0)
mydat2 <- left_join(mydat2, ab_prescr.pats, by = "patid")
summary(mydat2$AB)
mydat2 <- mydat2 %>% mutate(AB = ifelse(is.na(AB), 0, AB)) # substitute NAs with 0 

# count ABs just once; on the representation date, not both
# on presentation + representation
mydat2 <- mydat2 %>% mutate(AB = ifelse(is.na(within3m) & AB == 1, 0, AB)) 

mydat2 %>% group_by(AB) %>% summarise(P = n_distinct(patid))

mydat2$represent <- 1

# -------------
# GGPLOT
# -------------
# count events per year
gg.dat <- mydat2 %>% 
  select(c(myeventdate, represent)) %>%
  mutate(index_year = year(myeventdate)) %>%
  group_by(index_year) %>%
  summarise(Representations = sum(represent))
  
library(ggplot2)

gg.dat %>%
  ggplot(aes(index_year, Representations)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Annual representations") +
  ylim(0,2000) 

