
# --------------------------------------------------------------------
# Look at the proportion of patients in each AB group annually
# --------------------------------------------------------------------

library(Epi)
library(dplyr)
library(lubridate)

# The following steps have been copied from step_6
# ------------------------------------------------------
# Read all the prescriptions that refer to
# patients with a PO event
# ------------------------------------------------------

all_presc <- read.csv("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Extracted/DrugIssue_PO_coh_new.csv")

# Read the list with patids that exist in the master cohort
mypatids <- read.csv("~/Oto23/Data/Extracted/final_PO_IR_master_coh.csv")
mypatids <- mypatids %>% filter(PO_event == 1) %>% select(patid)
mypatids$patid <- as.character(mypatids$patid)

# --------------------------------------------------------
# Filter prescriptions:
# Every prescription date has to be the same as the date
# of PO diagnosis.
# --------------------------------------------------------

a <- all_presc %>% filter(!is.na(issuedate))
a %>% summarise(P = n_distinct(patid))
a$prodcodeid <- as.character(a$prodcodeid)
a$patid <- as.character(a$patid)
a <- left_join(mypatids, a, by = "patid") # keep only patids from the master cohort
a <- a %>% select(c(-X, -X.1))

library(haven)

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Data/Lookups")

# Read CPRD Aurum product dictionary to match it with the prescriptions
aurum_prod_dic <- read_dta("202106_emisproductdictionary.dta")
aurum_prod_dic$prodcodeid <- as.character(aurum_prod_dic$prodcodeid)

# --------------------------------------------------------------------
# ==> match with ABs lists to check frequencies
# --------------------------------------------------------------------

my_ABs_list <- read.table(file = "ABs_prodcodes_Aurum_ED_EH_classes_final.txt",
                          sep = "", header = T, fill = T)

my_ABs_list$my_flag <- "Antibiotic"

head(my_ABs_list)
my_ABs_list$prodcodeid <- as.character(my_ABs_list$prodcodeid)

# remove clobetasone and fluconazole products that were included by mistake
my_ABs_list <- my_ABs_list %>% filter(drugsubstancename != "Clobetasone butyrate")
my_ABs_list <- my_ABs_list %>% filter(drugsubstancename != "Fluconazole")

ab_prescr <- left_join(a, my_ABs_list, by = "prodcodeid")
ab_prescr <- ab_prescr %>% filter(my_flag == "Antibiotic") # keep only AB prescriptions
ab_prescr$patid <- as.character(ab_prescr$patid)
ab_prescr$myeventdate <- as_date(ab_prescr$myeventdate)

# filter patients with events until 2019
ab_prescr <- ab_prescr %>% filter(year(myeventdate) <= 2019)
summary(ab_prescr$myeventdate)

ab_prescr %>% summarise(P = n_distinct(patid))

ab_prescr <- ab_prescr %>% 
  select(c("patid", "myeventdate", "issuedate", "prodcodeid", "termfromemis", 
           "productname", "formulation", "routeofadministration", "drugsubstancename", 
           "substancestrength", "bnfchapter", "class_EH")) %>%
  distinct()

# classify ABs
#aa <- ab_prescr %>% group_by(drugsubstancename, class_EH) %>% summarise(P = n_distinct(patid))
#write.csv(aa, "~/Oto23/Data/ABs_groups.csv")

a.groups <- read.csv("~/Oto23/Data/ABs_groups.csv")
a.groups <- a.groups %>% filter(!is.na(X)) %>% select(c("AB_group_ED", "drugsubstancename", "class_EH"))

# merge with ABs dataset
ab_prescr <- left_join(ab_prescr, a.groups, by = c("drugsubstancename", "class_EH"))
head(ab_prescr)
ab_prescr %>% group_by(AB_group_ED, class_EH) %>% summarise(P = n_distinct(patid))
summary(ab_prescr)


# --------------------------------------------
# incident cases of PO / annual data
# --------------------------------------------

my_patients.inc <- read.csv("~/Oto23/Data/Extracted/final_PO_IR_master_coh.csv")

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

dm1$index_year <- as.integer(dm1$cal)

# exclude years after 2019
dm1 <- dm1 %>% filter(index_year <= 2019)

dm1$patid <- as.character(dm1$patid)

dm1 <- dm1 %>% select(-py, -PO_event)

dm1 %>% summarise(P = n_distinct(patid))

sum(dm1$lex.dur)
sum(dm1$lex.Xst)

# ----------------------------------------------
# merge dm1 with ABs dataset
# ----------------------------------------------

ab_prescr$index_year <- year(ab_prescr$issuedate)

ab_ann <- left_join(dm1, ab_prescr, by = c("patid", "index_year"))
ab_ann <- as.data.frame(ab_ann)
ab_ann$myeventdate.y <- NULL
ab_ann$AB_group_ED <- as.factor(ab_ann$AB_group_ED)
ab_ann$class_EH <- as.factor(ab_ann$class_EH)

summary(ab_ann)

ab_ann %>% group_by(AB_group_ED) %>% summarise(P = n_distinct(patid))

ab_ann <- ab_ann %>% mutate(AB_flag = ifelse(!is.na(AB_group_ED), 1, 0))

ab_ann %>% group_by(AB_flag) %>% summarise(P = n_distinct(patid))
ab_ann %>% group_by(AB_flag) %>% summarise(Pr = n())

# new variable that shows the py per patient correctly; there are more rows than
# patients because some of them received more than one prescriptions for PO and therefore,
# the PY appear increased; do it only for patients who received ABs and then append with 
# the rest of the cohort:
ab_ann1 <- ab_ann %>%
  filter(AB_flag == 1) %>%
  group_by(patid, index_year) %>%
  mutate(
    row.num = row_number()
  ) %>% ungroup()

# separate df with patients who were not exposed to ABs
ab_ann2 <- ab_ann %>% filter(AB_flag == 0)
ab_ann2 <- ab_ann2 %>% mutate(row.num = 1)

ab_ann <- union(ab_ann1, ab_ann2)

# for patients with more than one records (therefore more than 1 ABs, count the PY just once)
ab_ann <- ab_ann %>%
  mutate(
    lex.dur.fin = ifelse(row.num == 1, lex.dur, 0)
  )

sum(ab_ann$lex.dur)
sum(ab_ann$lex.dur.fin)

# check the annual number of prescriptions of any AB
a0 <- ab_ann %>% 
  select(c(patid, AB_group_ED, AB_flag, lex.dur.fin, index_year)) %>%
  group_by(index_year, AB_flag) %>%
  summarise(Pr = sum(AB_flag),
            Pat = n_distinct(patid),
            Denom = sum(lex.dur.fin),
            CR = Pat/Denom)

temp2 <- a0 %>%
  group_by(index_year) %>%
  mutate(
    Denom2 = sum(Denom),
    Pat2 = ifelse(AB_flag == 1, Pat, 0)
  ) %>% 
  ungroup() %>% 
  select(c(index_year, Pat2, Denom2)) %>%
  filter(Pat2 > 0) %>%
  mutate(
    IR_pr = Pat2/Denom2 *1000
  )

library(ggplot2)

temp2 %>%
  ggplot(aes(index_year, IR_pr)) +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +  ylim(0,2.5) +
  scale_fill_discrete(name = "Any antibiotic")
    

# Check if there are duplicated prescriptions per patid
ab_ann.noAB <- ab_ann %>% filter(is.na(termfromemis)) # dataframe without AB exposure
  
ab_ann.yesAB <- ab_ann %>% # dataframe without AB exposure
  filter(!is.na(termfromemis)) %>%
  group_by(patid, index_year, termfromemis) %>%
  mutate(
    dupli = n() > 1
  ) %>% ungroup()

summary(ab_ann.yesAB$dupli)

ab_ann.yesAB %>% group_by(dupli) %>% summarise(P = n_distinct(patid))


# check prescribed codes per year
a1 <- ab_ann %>% select(c(patid, AB_group_ED, AB_flag, lex.dur.fin, index_year, termfromemis)) %>%
  group_by(index_year, termfromemis) %>%
  summarise(Pr = sum(AB_flag))


ab_gg_dat <- ab_ann %>% 
  select(c(patid, index_year, lex.dur.fin, AB_flag, AB_group_ED, class_EH)) %>%
  group_by(index_year, AB_group_ED, class_EH) %>%
  summarise(
    denom = sum(lex.dur.fin), 
    numer = sum(AB_flag)) %>% ungroup()


ab_gg_dat.fin <- ab_gg_dat %>%
  select(c("index_year", "AB_group_ED", "class_EH", "numer", "denom")) %>%
  group_by(index_year) %>%
  mutate(
    denom = sum(denom)
  ) %>% ungroup() 


gg.oral <- ab_gg_dat.fin %>%
  filter(class_EH == "Oral") %>%
  filter(AB_group_ED %in% c("Amoxicillin", "Erythromycin", "Clarithromycin")) %>%
  group_by(index_year, AB_group_ED) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_AB_IR = numer/denom*1000) 

# -------------------------------------------------------------------------
# GGPLOT
# -------------------------------------------------------------------------

library(ggplot2)

# keep oral formulations of amoxicillin, erythromycin, clarythromycin
# and topical as:
# 1 - gentamicin, neomycin and framycetin 
# 2 - ciprofloxacin
# 3 - chloramphenicol 
# 4 - fusidic acid

gg.oral %>%
  mutate(
    `Oral antibiotic` = as.factor(AB_group_ED)
  ) %>%
  ggplot(aes(index_year, crude_AB_IR, color = `Oral antibiotic`)) +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +  ylim(0,1.6) +
  scale_fill_discrete(name = "Oral antibiotic")

write.csv(gg.oral, "~/Oto23/Results/Tab_Annual_oral_ABs.csv")


# TOPICAL ABs
# 1 - gentamicin, neomycin and framycetin 
# 2 - ciprofloxacin
# 3 - chloramphenicol 
# 4 - fusidic acid

gg.topical <- ab_gg_dat.fin %>%
  filter(class_EH == "Topical") %>%
  filter(AB_group_ED %in% c("Gentamicin", "Neomycin", "Framycetin", "Ciprofloxacin")) %>%
  group_by(index_year, AB_group_ED) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_AB_IR = numer/denom*1000) 

gg.topical %>%
  mutate(
    `Topical antibiotic` = as.factor(AB_group_ED)
  ) %>%
  ggplot(aes(index_year, crude_AB_IR, color = `Topical antibiotic`)) +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +  ylim(0,0.15) +
  scale_fill_discrete(name = "Topical antibiotic")

write.csv(gg.topical, "~/Oto23/Results/Tab_Annual_topical_ABs.csv")

