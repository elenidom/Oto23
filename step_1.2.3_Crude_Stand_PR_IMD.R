
# -------------------
# Crude PR by IMD q
# -------------------

crude_rates <- presentation_data %>%
  filter(myIMD != "Missing") %>%
  group_by(cal_year, myIMD) %>%
  summarise(
    denom = sum(denom), 
    numer = sum(numer),
    crude_PR_1000 = numer/denom*1000) %>%
  ungroup()


crude_rates %>%
  filter(cal_year <= 2019) %>%
  mutate(
    PR_per1000 = numer/denom * 1000
  ) %>% 
  ggplot(aes(cal_year, PR_per1000, color = as.factor(myIMD))) +
  geom_line() +
  ylim(0, 5)


# Standardized presentation by gender 
population_presentation <- presentation_data %>%
  group_by(myregion, myageband, mygender) %>%
  mutate(temp1 = sum(denom)) %>%
  ungroup() %>%
  mutate(
    temp2 = sum(denom), 
    prop = temp1/temp2,
    temp3 = prop * numer/denom
  ) 

population_presentation <- population_presentation %>%
  group_by(myIMD, cal_year) %>%
  mutate(temp = sum(denom)) %>% ungroup() %>%
  select(-denom, -numer) %>%
  rename("denom.s" = "temp") %>%
  group_by(myIMD, cal_year) %>%
  mutate(
    Stand.rate = sum(temp3)) %>% 
  ungroup() 

population_presentation <- population_presentation %>%
  mutate(
    numer.s = Stand.rate*denom.s
  ) %>%
  select(myIMD, cal_year, denom.s, numer.s) %>% distinct()

standardised_PR <- population_presentation %>% 
  select(c(myIMD, cal_year, numer.s, denom.s)) %>% 
  distinct() %>%
  mutate(PR_Stand_per1000 = numer.s/denom.s*1000)


# table to print
Table_PR_IMD <- left_join(crude_rates, standardised_PR, by = c("cal_year", "myIMD"))

setwd("~/Oto23/Results")

write.csv(Table_PR_IMD, "Table_PR_IMD.csv")

# --------------------------
# GG GRAPHS
# --------------------------

library(ggplot2)

standardised_PR %>% 
  filter(myIMD != "Missing") %>%
  filter(cal_year <= 2019) %>%
  mutate(IMD = as.factor(myIMD)) %>%
  ggplot(aes(cal_year, PR_Stand_per1000, color = IMD)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Presentation rate per 1,000 person-years") +
  scale_fill_discrete(name = "IMD")+
  ylim(0, 5)


# alternatively
standardised_PR %>% 
  filter(myIMD != "Missing") %>%
  filter(cal_year <= 2019) %>%
  mutate(
    IMD = case_when(
      myIMD %in% c("1 (least deprived)", "2") ~ "1-2 (least deprived)",
      myIMD %in% c("5 (most deprived)", "4") ~ "4-5 (most deprived)",
      TRUE ~ "3"
    )) %>%
  ggplot(aes(cal_year, PR_Stand_per1000, color = IMD)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Presentation rate per 1,000 person-years") +
  scale_fill_discrete(name = "IMD") +
  ylim(0, 5)

