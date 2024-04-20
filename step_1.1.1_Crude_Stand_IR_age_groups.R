# -------------------
# Crude IR by ageband
# -------------------

crude_rates <- incidence_data %>%
  group_by(index_year, myageband) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_IR_1000 = numer/denom*1000) %>%
  ungroup()


crude_rates %>%
  mutate(
    IR_per1000 = numer/denom * 1000
  ) %>% filter(index_year <= 2019) %>%
  ggplot(aes(index_year, IR_per1000, color = as.factor(myageband))) +
  geom_line() +
  ylim(0,12)


# Standardized incidence by gender 
population_incidence <- incidence_data %>%
  group_by(myregion, myIMD, mygender) %>%
  mutate(temp1 = sum(denom)) %>%
  ungroup() %>%
  mutate(
    temp2 = sum(denom), 
    prop = temp1/temp2,
    temp3 = prop * numer/denom
  ) 

population_incidence <- population_incidence %>%
  group_by(myageband, index_year) %>%
  mutate(temp = sum(denom)) %>% ungroup() %>%
  select(-denom, -numer) %>%
  rename("denom.s" = "temp") %>%
  group_by(myageband, index_year) %>%
  mutate(
    Stand.rate = sum(temp3)) %>% 
  ungroup() 

population_incidence <- population_incidence %>%
  mutate(
    numer.s = Stand.rate*denom.s
  ) %>%
  select(myageband, index_year, denom.s, numer.s) %>% distinct()

standardised_IR <- population_incidence %>% 
  select(c(myageband, index_year, numer.s, denom.s)) %>% 
  distinct() %>%
  mutate(IR_Stand_per1000 = numer.s/denom.s*1000)


# table to print
Table_IR_ageband <- left_join(crude_rates, standardised_IR, by = c("index_year", "myageband"))

setwd("~/Oto23/Results")

write.csv(Table_IR_ageband, "Table_IR_ageband.csv")

# --------------------------
# GG GRAPHS
# --------------------------

library(ggplot2)

standardised_IR %>%
  mutate(Age = myageband) %>%
  filter(index_year <= 2019) %>%
  ggplot(aes(index_year, IR_Stand_per1000, color = Age)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +
  ylim(0,11) 



