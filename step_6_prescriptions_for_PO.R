
library(dplyr)

# ------------------------------------------------------
# Read all the prescriptions that refer to
# patients with a PO event
# ------------------------------------------------------

all_presc <- read.csv("~/Oto23/Data/Extracted/DrugIssue_PO_coh_new.csv")

# Remove all the remaining df
# rm(list = ls(pattern = "^DrugIssue_PO"))

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
# ==> match with prescriptions to check ANY prescription
# --------------------------------------------------------------------

any_prescr <- left_join(a, aurum_prod_dic, by = "prodcodeid")

any_prescr <- any_prescr %>%
  group_by(productname, drugsubstancename) %>%
  summarise(
    Count = n(), # prescriptions
    Perc = round(n()/dim(any_prescr)[1]*100, 1), # % prescriptions
    Patients = n_distinct(patid), # patients
    Perc_pat = round(Patients / 80454 * 100, 1)) # all patients with PO

write.csv(any_prescr, "/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Results/Most_freq_presc_products.csv")

# --------------------------------------------------------------------
# ==> match with ABs/AFs lists to check frequencies
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

# filter patients with events until 2019
library(lubridate)
ab_prescr <- ab_prescr %>% filter(year(myeventdate) <= 2019)

ab_prescr %>% summarise(P = n_distinct(patid))

ab_prescr_freq <- ab_prescr %>%
  group_by(productname, drugsubstancename) %>% 
  summarise(Count = n(),
            Perc = round(n()/dim(ab_prescr)[1]*100, 1),
            Patients = n_distinct(patid), # patients
            Perc_pat = round(Patients / 80454 * 100, 1) # all patients with PO
            )

# group by AB name and class
ab_prescr_freq <- ab_prescr %>%
  group_by(productname, drugsubstancename, class_EH) %>% 
  summarise(Count = n(),
            Perc = round(n()/dim(ab_prescr)[1]*100, 1),
            Patients = n_distinct(patid), # patients
            Perc_pat = round(Patients / 80454 * 100, 1) # all patients with PO
            )

# class and drug substance
ab_prescr_class <- ab_prescr %>%
  group_by(drugsubstancename, class_EH) %>% 
  summarise(Count = n(),
            Perc = round(n()/dim(ab_prescr)[1]*100, 1))

# class only
ab_prescr_class.only <- ab_prescr %>%
  group_by(class_EH) %>% 
  summarise(Count = n(),
            Perc = round(n()/dim(ab_prescr)[1]*100, 1),
            Pats = n_distinct(patid),
            Perc_pat = round(Pats/80454 * 100, 1)
            )

# overlaps: exposure to both oral & topical ABs
temp <- ab_prescr %>% select(c(patid, class_EH)) %>% distinct()
temp %>% group_by(class_EH) %>% summarise(Patients = n_distinct(patid))

temp <- temp %>% mutate(
  ABo.yes = ifelse(class_EH == "Oral", 1, 0),
  ABt.yes = ifelse(class_EH == "Topical", 1, 0)
)

temp <- temp %>% 
  group_by(patid) %>%
  mutate(
    bothABot = ifelse(sum(ABo.yes) < n(), 1, 0),
    bothABot = max(bothABot)
  ) %>% ungroup()

# oral or topical but not both
a2 <- temp %>% group_by(patid) %>% filter(max(n()) == 1) %>% ungroup()
a2 %>% group_by(ABo.yes, ABt.yes) %>% summarise(P = n_distinct(patid))

# both oral and topical
a3 <- temp %>% group_by(patid) %>% mutate(Records = n()) %>% ungroup()
a3 <- a3 %>% filter(Records > 1)
a3 %>% group_by(bothABot) %>% summarise(P = n_distinct(patid))


# --------------------------------------------------------------------
# Look at the proportion of patients in each group who have a 
# representation with otorrhoea within 3 months
# --------------------------------------------------------------------

# patients with ABs exposure
ab_prescr.pats <- ab_prescr %>% 
  select(patid) %>% 
  mutate(AB = 1) %>% 
  distinct() 

# save this dataset
write.csv(ab_prescr.pats, "~/Oto23/Data/Extracted/ab_prescr.pats.csv")

# --------------------------------------------------------------------
# ==> match with the antifungals list
# --------------------------------------------------------------------

my_AFs_list <- read.table(file = "AFs_prodcodes_Aurum_ED_EH_classes_final.txt",
                          sep = "", header = T, fill = T)

head(my_AFs_list)
my_AFs_list$prodcodeid <- as.character(my_AFs_list$prodcodeid)
my_AFs_list <- my_AFs_list %>% rename("class_EH" = "EH_class")
my_AFs_list$my_flag <- "Antifungal"

af_prescr <- left_join(a, my_AFs_list, by = "prodcodeid")
af_prescr <- af_prescr %>% filter(my_flag == "Antifungal") # keep only AF prescriptions

# filter patients with events until 2019
af_prescr <- af_prescr %>% filter(year(myeventdate) <= 2019)

af_prescr_freq <- af_prescr %>%
  group_by(productname, drugsubstancename) %>% 
  summarise(Count = n(),
            Perc = round(n()/dim(af_prescr)[1]*100, 1))

# group by AF name and class
af_prescr_freq <- af_prescr %>%
  group_by(productname, drugsubstancename, class_EH) %>% 
  summarise(Count = n(),
            Perc = round(n()/dim(af_prescr)[1]*100, 1),
            Pats = n_distinct(patid),
            Perc_pat = round(Pats/80454 * 100, 1)
            )

# class and drug substance
af_prescr_class <- af_prescr %>%
  group_by(drugsubstancename, class_EH) %>% 
  summarise(Count = n(),
            Perc = round(n()/dim(af_prescr)[1]*100, 1),
            Pats = n_distinct(patid),
            Perc_pat = round(Pats/80454 * 100, 1)
  )

# class only
af_prescr_class.only <- af_prescr %>%
  group_by(class_EH) %>% 
  summarise(Count = n(),
            Perc = round(n()/dim(af_prescr)[1]*100, 1),
            Pats = n_distinct(patid),
            Perc_pat = round(Pats/80454 * 100, 1)
  )

# overlaps: exposure to both oral & topical AFs
temp.af <- af_prescr %>% select(c(patid, class_EH)) %>% distinct()
temp.af %>% group_by(class_EH) %>% summarise(Patients = n_distinct(patid))

temp.af <- temp.af %>% mutate(
  AFo.yes = ifelse(class_EH == "oral", 1, 0),
  AFt.yes = ifelse(class_EH == "topical", 1, 0)
)

temp.af <- temp.af %>% 
  group_by(patid) %>%
  mutate(
    bothAFot = ifelse(sum(AFo.yes) < n(), 1, 0),
    bothAFot = max(bothAFot)
  ) %>% ungroup()

# oral or topical but not both
a2.1 <- temp.af %>% group_by(patid) %>% filter(max(n()) == 1) %>% ungroup()
a2.1 %>% group_by(AFo.yes, AFt.yes) %>% summarise(P = n_distinct(patid))

# both oral and topical
a3.1 <- temp.af %>% group_by(patid) %>% mutate(Records = n()) %>% ungroup()
a3.1 <- a3.1 %>% filter(Records > 1) 
a3.1 %>% group_by(bothAFot) %>% summarise(P = n_distinct(patid))


# --------------------------------------------------------------------
# save results
# --------------------------------------------------------------------

setwd("/mnt/bmh01-rds/Ashcroft_Domzaridou_PSTRC/Otorrhoea_2023/Results")

# ABs
write.csv(ab_prescr_freq, "ab_prescr_freq.csv")
write.csv(ab_prescr_class, "ab_prescr_freq_class.csv")
write.csv(ab_prescr_class.only, "ab_prescr_class.csv")

# AFs
write.csv(af_prescr_freq, "af_prescr_freq.csv")
write.csv(af_prescr_class, "af_prescr_freq_class.csv")
write.csv(af_prescr_class.only, "af_prescr_class.csv")

