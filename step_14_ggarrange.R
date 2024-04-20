
# ----------------------------------------------------------------------------
#                      Prepare figures for submission 
# ----------------------------------------------------------------------------

setwd("~/Oto23/Results") # working dir that contains summaries for tables

# ----------------------------------------------------------------------------
# FIGURE 1: OVERALL INCIDENCE & PRESENTATION RATES 
# ----------------------------------------------------------------------------

Table_IR_overall <- read.csv("Table_IR_overall.csv")

IR_over.figure1a <- Table_IR_overall %>%
  filter(index_year <= 2019) %>%
  ggplot(aes(index_year, IR_Stand_per1000)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +
  ylim(0, 4)

Table_PR_overall <- read.csv("Table_PR_overall.csv")

PR_over.figure1b <- Table_PR_overall %>%
  filter(cal_year <= 2019) %>%
  ggplot(aes(cal_year, PR_Stand_per1000)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Presentation rate per 1,000 person-years") +
  ylim(0, 4)

# -----------------
# VISUAL FIGURE 1
# -----------------

figure1 <- ggarrange(IR_over.figure1a, PR_over.figure1b, labels = c("a", "b"), ncol = 1)
figure1

# ---------------------------------------------------
# ALTERNATIVE FIGURE 1 - PLOTS COMBINED IN ONE GRAPH
# ---------------------------------------------------

visual1 <- Table_IR_overall %>% 
  filter(index_year <= 2019) %>%
  select(c(index_year, IR_Stand_per1000))%>%
  rename("cal_year" = "index_year",
         "Rate_1000" = "IR_Stand_per1000") %>%
  mutate(Rate = "Incidence")

# levels = c("Male", "Female")) # changes the order in the legend

visual2 <- Table_PR_overall %>% 
  filter(cal_year <= 2019) %>%
  select(c(cal_year, PR_Stand_per1000)) %>%
  rename("Rate_1000" = "PR_Stand_per1000") %>%
  mutate(Rate = "Presentation")

visuals = rbind(visual1, visual2)

visuals <- visuals %>%
  mutate(
    Rate = factor(Rate, levels = c("Presentation", "Incidence"))
  )

ggplot(visuals, aes(cal_year, Rate_1000, group = Rate, col = Rate)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Rate per 1,000 person-years") +
  ylim(0, 4)

# ----------------------------------------------------------------------------
# FIGURES 2 & 3: ANNUAL INCIDENCE & PRESENTATION RATES
# ----------------------------------------------------------------------------

# ----------
# INCIDENCE
# ----------

# AGE
Table_IR_ageband <- read.csv("Table_IR_ageband.csv")

gg.ir.age <- Table_IR_ageband %>%
  mutate(Age = myageband) %>%
  filter(index_year <= 2019) %>%
  ggplot(aes(index_year, IR_Stand_per1000, color = Age)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +
  ylim(0,11) 

# GENDERS
Table_IR_gender <- read.csv("Table_IR_gender.csv")

gg.ir.sex <- Table_IR_gender %>% 
  filter(index_year <= 2019) %>%
  mutate(
    Gender = factor(if_else(mygender == 0, "Male", "Female"),
                    levels = c("Male", "Female")) # changes the order in the legend
  ) %>%
  ggplot(aes(index_year, IR_Stand_per1000, color = Gender)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +
  ylim(0,4)

# IMD
Table_IR_IMD <- read.csv("Table_IR_IMD.csv")

gg.ir.imd <- Table_IR_IMD %>% 
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
  ylim(0, 5)


# -------------
# PRESENTATIONS
# -------------

# AGE
Table_PR_ageband <- read.csv("Table_PR_ageband.csv")

gg.pr.age <- Table_PR_ageband %>% 
  filter(cal_year <= 2019) %>%
  mutate(Age = as.factor(myageband)) %>%
  ggplot(aes(cal_year, PR_Stand_per1000, color = Age)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Presentation rate per 1,000 person-years") +
  scale_fill_discrete(name = "Age")+
  ylim(0,11)

# GENDER
Table_PR_gender <- read.csv("Table_PR_gender.csv")

gg.pr.sex <- Table_PR_gender %>% 
  filter(cal_year <= 2019) %>%
  mutate(
    Gender = factor(if_else(mygender == 0, "Male", "Female"),
                    levels = c("Male", "Female")) # changes the order in the legend
  ) %>%
  ggplot(aes(cal_year, PR_Stand_per1000, color = Gender)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Presentation rate per 1,000 person-years") +
  scale_fill_discrete(name = "Gender")+
  ylim(0,4)

# IMD
Table_PR_IMD <- read.csv("Table_PR_IMD.csv")

gg.pr.imd <- Table_PR_IMD %>% 
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

# --------------------
# VISUAL FIGURES 2&3
# --------------------

library(ggpubr)

IR.figure2 <- ggarrange(gg.ir.age, gg.ir.sex, gg.ir.imd, labels = c("a", "b", "c"))
IR.figure2

PR_figure3 <- ggarrange(gg.pr.age, gg.pr.sex, gg.pr.imd, labels = c("a", "b", "c"))
PR_figure3



# ----------------------------------------------------------------------------
# FIGURE 4: ABs
# ----------------------------------------------------------------------------

# ORAL
Tab_Annual_oral <- read.csv("Tab_Annual_oral_ABs.csv")

figure4a <- Tab_Annual_oral %>%
  mutate(
    `Oral antibiotics` = as.factor(AB_group_ED)
  ) %>%
  ggplot(aes(index_year, crude_AB_IR, color = `Oral antibiotics`)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +  ylim(0, 1.6) 

# TOPICAL
Tab_Annual_topical <- read.csv("Tab_Annual_topical_ABs.csv")

figure4b <- Tab_Annual_topical %>%
  mutate(
    `Topical antibiotics` = as.factor(AB_group_ED)
  ) %>%
  ggplot(aes(index_year, crude_AB_IR, color = `Topical antibiotics`)) +
  geom_point() +
  geom_line() +
  labs(x = "year", y = "Incidence rate per 1,000 person-years") +  ylim(0, 0.15) 

# -----------------
# VISUAL FIGURE 4
# -----------------

figure4 <- ggarrange(figure4a, figure4b, labels = c("a", "b"), ncol = 1)
figure4
