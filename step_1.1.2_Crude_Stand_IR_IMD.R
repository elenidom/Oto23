
# -------------------
# Crude IR by IMD q
# -------------------

crude_rates <- incidence_data %>%
  filter(myIMD != "Missing") %>%
  group_by(index_year, myIMD) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_IR_1000 = numer/denom*1000) %>%
  ungroup()


crude_rates %>%
  filter(index_year <= 2019) %>%
  mutate(
    IR_per1000 = numer/denom * 1000
  ) %>% 
  ggplot(aes(index_year, IR_per1000, color = as.factor(myIMD))) +
  geom_line() +
  ylim(0, 4)


# Standardized incidence by gender 
population_incidence <- incidence_data %>%
  group_by(myregion, myageband, mygender) %>%
  mutate(temp1 = sum(denom)) %>%
  ungroup() %>%
  mutate(
    temp2 = sum(denom), 
    prop = temp1/temp2,
    temp3 = prop * numer/denom
  ) 

population_incidence <- population_incidence %>%
  group_by(myIMD, index_year) %>%
  mutate(temp = sum(denom)) %>% ungroup() %>%
  select(-denom, -numer) %>%
  rename("denom.s" = "temp") %>%
  group_by(myIMD, index_year) %>%
  mutate(
    Stand.rate = sum(temp3)) %>% 
  ungroup() 

population_incidence <- population_incidence %>%
  mutate(
    numer.s = Stand.rate*denom.s
  ) %>%
  select(myIMD, index_year, denom.s, numer.s) %>% distinct()

standardised_IR <- population_incidence %>% 
  select(c(myIMD, index_year, numer.s, denom.s)) %>% 
  distinct() %>%
  mutate(IR_Stand_per1000 = numer.s/denom.s*1000)


# table to print
Table_IR_IMD <- left_join(crude_rates, standardised_IR, by = c("index_year", "myIMD"))

setwd("~/Oto23/Results")

write.csv(Table_IR_IMD, "Table_IR_IMD.csv")

# --------------------------
# GG GRAPHS
# --------------------------

library(ggplot2)

standardised_IR %>% 
  filter(myIMD != "Missing") %>%
  filter(index_year <= 2019) %>%
  ggplot(aes(index_year, IR_Stand_per1000, color = myIMD)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +
  scale_fill_discrete(name = "IMD") +
  ylim(0, 4)

# alternatively
standardised_IR %>% 
  filter(myIMD != "Missing") %>%
  filter(index_year <= 2019) %>%
  mutate(
    IMD = case_when(
      myIMD %in% c("1 (least deprived)", "2") ~ "1-2 (least deprived)",
      myIMD %in% c("5 (most deprived)", "4") ~ "4-5 (most deprived)",
      TRUE ~ "3"
    )) %>%
  ggplot(aes(index_year, IR_Stand_per1000, color = IMD)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +
  scale_fill_discrete(name = "IMD") +
  ylim(0, 4)


