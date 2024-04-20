
# -------------------
# Crude PR by ageband
# -------------------

crude_rates <- presentation_data %>%
  group_by(cal_year, myageband) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_PR_1000 = numer/denom*1000) %>%
  ungroup()


crude_rates %>%
  mutate(
    PR_per1000 = numer/denom * 1000
  ) %>% filter(cal_year <= 2019) %>%
  ggplot(aes(cal_year, PR_per1000, color = as.factor(myageband))) +
  geom_line() +
  ylim(0,10)


# Standardized presentation by gender 
population_presentation <- presentation_data %>%
  group_by(myregion, myIMD, mygender) %>%
  mutate(temp1 = sum(denom)) %>%
  ungroup() %>%
  mutate(
    temp2 = sum(denom), 
    prop = temp1/temp2,
    temp3 = prop * numer/denom
  ) 

population_presentation <- population_presentation %>%
  group_by(myageband, cal_year) %>%
  mutate(temp = sum(denom)) %>% ungroup() %>%
  select(-denom, -numer) %>%
  rename("denom.s" = "temp") %>%
  group_by(myageband, cal_year) %>%
  mutate(
    Stand.rate = sum(temp3)) %>% 
  ungroup() 

population_presentation <- population_presentation %>%
  mutate(
    numer.s = Stand.rate*denom.s
  ) %>%
  select(myageband, cal_year, denom.s, numer.s) %>% distinct()

standardised_PR <- population_presentation %>% 
  select(c(myageband, cal_year, numer.s, denom.s)) %>% 
  distinct() %>%
  mutate(PR_Stand_per1000 = numer.s/denom.s*1000)


# table to print
Table_PR_ageband <- left_join(crude_rates, standardised_PR, by = c("cal_year", "myageband"))

setwd("~/Oto23/Results")

write.csv(Table_PR_ageband, "Table_PR_ageband.csv")

# --------------------------
# GG GRAPHS
# --------------------------

library(ggplot2)

standardised_PR %>% 
  filter(cal_year <= 2019) %>%
  mutate(Age = as.factor(myageband)) %>%
  ggplot(aes(cal_year, PR_Stand_per1000, color = Age)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Presentation rate per 1,000 person-years") +
  scale_fill_discrete(name = "Age")+
  ylim(0,10)


