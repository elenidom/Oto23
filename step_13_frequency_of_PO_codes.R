
# --------------------------------------------------------------------
# Frequency of PO codes
# --------------------------------------------------------------------

library(dplyr)
library(lubridate)

# incident cases of PO
mydat.ir <- read.csv("~/Oto23/Data/Extracted/final_PO_IR_master_coh.csv")
mydat.ir$patid <- as.character(mydat.ir$patid)
dput(names(mydat.ir))

mydat.ir <- mydat.ir %>% select(c("patid", "pracid", "mygender", "myregion", 
                                  "e2019_imd_10", "entrydate", "exitdate", "myeventdate",
                                  "PO_code_descr"))

mydat.ir$myeventdate <- as_date(mydat.ir$myeventdate)

# keep only patid with the event
mydat.ir <- mydat.ir %>% filter(!is.na(myeventdate))
mydat.ir <- mydat.ir %>% filter(year(myeventdate) <= 2019)

mydat.ir %>% summarise(P = n_distinct(patid))
head(mydat.ir)

# frequency of PO primary codes (IR analysis)
freq_PO_code_IR <- mydat.ir %>% group_by(PO_code_descr) %>% summarise(P = n_distinct(patid))
write.csv(freq_PO_code_IR, "~/Oto23/Results/freq_PO_code_IR.csv")

