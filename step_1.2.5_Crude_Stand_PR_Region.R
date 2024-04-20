
# -------------------
# Crude IR by region
# -------------------

crude_rates <- presentation_data %>%
  group_by(cal_year, myregion) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_IR_1000 = numer/denom*1000) %>%
  ungroup()


crude_rates %>%
  filter(!is.na(myregion)) %>%
  filter(cal_year <= 2019) %>%
  mutate(
    IR_per1000 = numer/denom * 1000
  ) %>% 
  ggplot(aes(cal_year, IR_per1000, color = as.factor(myregion))) +
  geom_line() +
  ylim(0, 5)


# Standardized presentation by gender 
population_presentation <- presentation_data %>%
  group_by(myIMD, myageband, mygender) %>%
  mutate(temp1 = sum(denom)) %>%
  ungroup() %>%
  mutate(
    temp2 = sum(denom), 
    prop = temp1/temp2,
    temp3 = prop * numer/denom
  ) 

population_presentation <- population_presentation %>%
  group_by(myregion, cal_year) %>%
  mutate(temp = sum(denom)) %>% ungroup() %>%
  select(-denom, -numer) %>%
  rename("denom.s" = "temp") %>%
  group_by(myregion, cal_year) %>%
  mutate(
    Stand.rate = sum(temp3)) %>% 
  ungroup() 

population_presentation <- population_presentation %>%
  mutate(
    numer.s = Stand.rate*denom.s
  ) %>%
  select(myregion, cal_year, denom.s, numer.s) %>% distinct()

standardised_PR <- population_presentation %>% 
  select(c(myregion, cal_year, numer.s, denom.s)) %>% 
  distinct() %>%
  mutate(IR_Stand_per1000 = numer.s/denom.s*1000)


# table to print
Table_PR_Region <- left_join(crude_rates, standardised_PR, by = c("cal_year", "myregion"))

setwd("~/Oto23/Results")

write.csv(Table_PR_Region, "Table_PR_Region.csv")

# --------------------------
# GG GRAPHS
# --------------------------

library(ggplot2)

standardised_PR %>% 
  filter(myregion <= 10) %>%
  filter(cal_year <= 2019) %>%
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
  ggplot(aes(cal_year, IR_Stand_per1000, color = Region)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Presentation rate per 1,000 person-years") +
  scale_fill_discrete(name = "Region")+
  ylim(0, 6)


