
library(dplyr)

lef_au <- read.table("~/Steroids23/Set22/Aurum_enhanced_eligibility_January_2022.txt", sep = "\t")

lef_au <- lef_au[-1,c(1:3, 6)]
colnames(lef_au) <- c("patid", "pracid", "linkdate", "lsoa_e") # rename columns

head(lef_au)

# patid
lef_au.patid <- lef_au %>%
  filter(lsoa_e == 1) %>%
  select(c(patid, linkdate, lsoa_e)) %>%
  distinct()

lef_au.patid$patid <- as.character(lef_au.patid$patid)

# pracid
lef_au.pracid <- lef_au %>%
  filter(lsoa_e == 1) %>%
  select(c(pracid, linkdate, lsoa_e)) %>%
  distinct()

lef_au.pracid$pracid <- as.character(lef_au.pracid$pracid)

# merge linkage eli for IMD with my_patients data
my_patients.inc$patid <- as.character(my_patients.inc$patid)
my_patients.inc$pracid <- as.character(my_patients.inc$pracid)

# keep only patids with linkage for IMD
# patid
my_patients_lsoe_patid <- left_join(my_patients.inc, lef_au.patid, by = "patid")
my_patients_lsoe_patid <- my_patients_lsoe_patid %>% filter(lsoa_e == 1) %>%
  select(c(patid, lsoa_e)) %>% distinct()
my_patients_lsoe_patid$patid <- as.character(my_patients_lsoe_patid$patid)

# save
write.table(my_patients_lsoe_patid, "~/Oto23/Data/my_patients_lsoe_patid.txt")

# pracid
my_patients_lsoe_pracid <- left_join(my_patients.inc, lef_au.pracid, by = "pracid")
my_patients_lsoe_pracid <- my_patients_lsoe_pracid %>% filter(lsoa_e == 1)
my_patients_lsoe_pracid <- my_patients_lsoe_pracid %>% select(c(pracid, lsoa_e)) %>%
  distinct()

# save
my_patients_lsoe_pracid$pracid <- as.character(my_patients_lsoe_pracid$pracid)

write.table(my_patients_lsoe_pracid, "~/Oto23/Data/my_patients_lsoe_pracid.txt")
