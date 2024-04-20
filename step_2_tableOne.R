
#######################################################################################################
#                                           *
#                                       Otorrhoea
#                                    ELENI DOMZARIDOU
#                                UNIVERSITY OF MANCHESTER
#                                       June 2023
#                                      CPRD  Aurum
#                                           *
# 
# Aims:
# a) The code populates table one 
# 
#######################################################################################################

library(tableone)
library(dplyr)

# restrict cohort until 2019
k <- dm1 %>% 
  filter(index_year <= 2019) %>% 
  select(c("patid", "lex.Xst", "lex.dur", "mygender", "myregion", 
           "myage_obs.st", "PO_event", "myIMD")) %>%
  group_by(patid) %>%
  mutate(PO_event = max(lex.Xst),
         PY = sum(lex.dur)) %>%
  ungroup()

k <- k %>% select(-lex.dur, -lex.Xst) %>% distinct()

k %>% summarise(Np = n_distinct(patid))
sum(k$PO_event)
sum(k$PY)

# assign labels in region var
k <- k %>% mutate(
  region.txt = case_when(
    myregion ==	1 ~ "North East",
    myregion ==	2 ~ "North West",
    myregion ==	3 ~ "Yorkshire & The Humber", 
    myregion ==	4 ~ "East Midlands",
    myregion ==	5 ~ "West Midlands",
    myregion ==	6 ~ "East of England",
    myregion ==	7 ~ "South West",
    myregion ==	8 ~ "South Central",
    myregion ==	9 ~ "London",
    myregion ==	10 ~ "South East Coast",
    myregion ==	11 ~ "Northern Ireland",
    TRUE ~ "Unknown"
  )
)

# 0 = MALE; 1 = FEMALE (from CPRD dictionary)
k <- k %>% mutate(
  mygender = if_else(mygender == 0, "Male", "Female")
)

# age at obs start
k <- k %>% mutate(
    myageband = case_when(
      myage_obs.st >= 0 & myage_obs.st <= 2 ~  "[0-2]",
      myage_obs.st >  2 & myage_obs.st <= 5 ~  "(2-5]",
      myage_obs.st >  5 & myage_obs.st <= 8 ~  "(5-8]",
      myage_obs.st >  8 ~ "(8-16]"
  )
)


# convert all the comos and meds codes into factor vars
mycols <- myvars <- c("region.txt", "mygender", "myageband", "myIMD")

k <- as.data.frame(k)

k[ , mycols] <- lapply(k[,mycols], factor) 

# summary(k)

myvars <- c("region.txt", "mygender", "myageband", "myIMD", "PY")

k %>% summarise(Np = n_distinct(patid))

# Assign strata
factorVars <- "PO_event"

# produce TableOne
TOoutput <- CreateTableOne(vars = myvars, strata = factorVars, data = k)
TOoutput


