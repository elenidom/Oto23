
# -------------------
# Crude IR by region
# -------------------

crude_rates <- incidence_data %>%
  group_by(index_year, myregion) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_IR_1000 = numer/denom*1000) %>%
  ungroup()


crude_rates %>%
  filter(!is.na(myregion)) %>%
  filter(index_year <= 2019) %>%
  mutate(
    IR_per1000 = numer/denom * 1000
  ) %>% 
  ggplot(aes(index_year, IR_per1000, color = as.factor(myregion))) +
  geom_line() +
  ylim(0, 5)


# Standardized incidence by gender 
population_incidence <- incidence_data %>%
  group_by(myIMD, myageband, mygender) %>%
  mutate(temp1 = sum(denom)) %>%
  ungroup() %>%
  mutate(
    temp2 = sum(denom), 
    prop = temp1/temp2,
    temp3 = prop * numer/denom
  ) 

population_incidence <- population_incidence %>%
  group_by(myregion, index_year) %>%
  mutate(temp = sum(denom)) %>% ungroup() %>%
  select(-denom, -numer) %>%
  rename("denom.s" = "temp") %>%
  group_by(myregion, index_year) %>%
  mutate(
    Stand.rate = sum(temp3)) %>% 
  ungroup() 

population_incidence <- population_incidence %>%
  mutate(
    numer.s = Stand.rate*denom.s
  ) %>%
  select(myregion, index_year, denom.s, numer.s) %>% distinct()

standardised_IR <- population_incidence %>% 
  select(c(myregion, index_year, numer.s, denom.s)) %>% 
  distinct() %>%
  mutate(IR_Stand_per1000 = numer.s/denom.s*1000)


# table to print
Table_IR_Region <- left_join(crude_rates, standardised_IR, by = c("index_year", "myregion"))

setwd("~/Oto23/Results")

write.csv(Table_IR_Region, "Table_IR_Region.csv")

# --------------------------
# GG GRAPHS
# --------------------------

library(ggplot2)

standardised_IR %>% 
  filter(myregion <= 10) %>%
  filter(index_year <= 2019) %>%
  mutate(
    Region = case_when(
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
    )) %>%
  ggplot(aes(index_year, IR_Stand_per1000, color = Region)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +
  scale_fill_discrete(name = "Region")+
  ylim(0, 5)


