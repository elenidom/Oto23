
# ---------------------------------------------------------------------------------------------------
#                                  PRESENTATIONS RR ANALYSIS
# ---------------------------------------------------------------------------------------------------
#
# In this analysis I considered repeated events throughout the obs period;
# I split the data by index year because the code crushes otherwise

library(MASS)
library(dplyr)

kk <- read.csv("~/Oto23/Data/Extracted/kk.csv")

# -------------------------------------
# Prepare dataset for PR analysis
# -------------------------------------

# crude rates and models for all the obs period
mydat <- kk %>% 
  select(c("patid", "pracid", "lex.dur", "age.tvc",
           "mygender", "myregion", "myevent_yr", 
           "cal_year", "myIMD", "myevents")) %>%
  filter(cal_year <= 2019)

mydat$patid <- as.character(mydat$patid)

# mydat <- mydat %>%
#   group_by(patid) %>%
#   mutate(
#     py = sum(lex.dur),
#     all_events = sum(myevents),
#     myage.exit = max(age.tvc)
#   ) %>% ungroup() %>%
#   select(-myevent_yr, -myevents, -cal_year, -age.tvc, -lex.dur) %>% 
#   distinct()
  
head(mydat)

str(mydat)

mydat <- mydat %>% filter(!is.na(myregion))

# agebands
mydat <- mydat %>% 
  mutate(
      myageband = case_when(
        age.tvc >= 0 & age.tvc <= 2 ~  "(0-2]",
        age.tvc >  2 & age.tvc <= 5 ~  "(2-5]",
        age.tvc >  5 & age.tvc <= 8 ~  "(5-8]",
        age.tvc >  8 ~ "(8-16]"
    )
  )

# New var to consider variation due to calendar time
mydat <- mydat %>%
  mutate(
    cal_year2 = case_when(
      cal_year == 2005 & cal_year <= 2008 ~ "2005-2008",
      cal_year  > 2008 & cal_year <= 2012 ~ "2009-2012",
      cal_year  > 2012 & cal_year <= 2016 ~ "2013-2016",
      cal_year  > 2016 & cal_year <= 2019 ~ "2017-2019"
    )
  )

# reduce the size of the dataset by grouping according to year quintiles
mydat <- mydat %>%
  group_by(patid, cal_year2) %>%
  mutate(
    lex.dur2 = sum(lex.dur)
  ) %>% ungroup() %>%
  select(-lex.dur, -age.tvc, -cal_year) %>%
  distinct()

# -------------------------------------
# Poisson models
# -------------------------------------

# adjusted 
m0 <- glm(myevents ~ offset(log(lex.dur2)) + factor(myIMD) + # factor(myregion) +
                 factor(mygender) + factor(myageband) + factor(cal_year2), 
               family = "poisson", data = mydat)

summary(m0)
#est <- cbind(Estimate = coef(m0), confint(m0))
#exp(est)

# another way to get robust CIs
require(sandwich)
require(msm)

cov.m0 <- vcovHC(m0, type = "HC0")
std.err <- sqrt(diag(cov.m0))
r.est <- cbind(Estimate= coef(m0), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m0)/std.err), lower.tail = FALSE),
               LL = coef(m0) - 1.96 * std.err,
               UL = coef(m0) + 1.96 * std.err)
r.est

# delta method
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                 coef(m0), cov.m0)

# exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])

# replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s

rexp.est

# add fructional polynomials for year


# random effects model
library(lme4)

m0.re <- glmer(myevents ~ offset(log(lex.dur)) + factor(myregion) + factor(myIMD) +
                 factor(mygender) + age.tvc + (1|pracid),
               family = "poisson", data = mydat)

est <- cbind(Estimate = coef(m0.re), confint(m0.re))
exp(est)

# ranef(m1.re)
# 
# randoms <- ranef(m1.re, condVar = TRUE)$pracid
# 
# variances <- as.numeric(attr(randoms, "postVar")) # We'll use the variances for estimating CIs
# 
# res <- data.frame(pracid = rownames(randoms), 
#                   mean_effect = randoms$`(Intercept)`+coef(summary(m1.re))[,"Estimate"])
# 
# coef(summary(m1.re))
# 
# res$lower <- res$mean_effect - 2* sqrt(variances) # 2 standard deviation
# res$upper <- res$mean_effect + 2* sqrt(variances)
# 
# exp(res$mean_effect)
# 
# res$mean_effect <- exp(res$mean_effect) 
# res$lower <- exp(res$lower)
# res$upper <- exp(res$upper)
# 
# res$pracid <- reorder(res$pracid, res$mean_effect, mean)
# 
# head(res)

# -------------------------------------
# Negative binomial models
# -------------------------------------

m0.nb <- glm.nb(myevents ~ offset(log(lex.dur2)) + factor(myIMD) + #factor(myregion) +
            factor(mygender) + factor(myageband) + factor(cal_year2), 
           data = mydat)

summary(m0.nb)

# check assumption
pchisq(2* (logLik(m0.nb) - logLik(m0)), df = 1, lower.tail = FALSE)


cov.m0 <- vcovHC(m0.nb, type = "HC0")
std.err <- sqrt(diag(cov.m0))
r.est <- cbind(Estimate= coef(m0.nb), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m0.nb)/std.err), lower.tail = FALSE),
               LL = coef(m0.nb) - 1.96 * std.err,
               UL = coef(m0.nb) + 1.96 * std.err)
r.est

# delta method
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                 coef(m0), cov.m0)

# exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])

# replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s

rexp.est




