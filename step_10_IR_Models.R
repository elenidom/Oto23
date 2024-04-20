
# ---------------------------------------------------------------------------------------------------
#                                  INCIDENCE IR ANALYSIS
# ---------------------------------------------------------------------------------------------------
#
# In this analysis I considered first events throughout the obs period;

library(MASS)
library(dplyr)

# -------------------------------------
# Prepare dataset 
# -------------------------------------

# crude rates and models for all the obs period
mydat <- dm1 %>% 
  select(c("patid", "pracid", "lex.dur", "lex.Xst", "age",
           "mygender", "myregion", "index_year", "myIMD")) %>%
  filter(index_year <= 2019)

mydat$patid <- as.character(mydat$patid)

head(mydat)

str(mydat)

mydat$myIMD <- as.factor(mydat$myIMD)

mydat <- mydat %>% filter(!is.na(myregion))

# agebands
mydat <- mydat %>% 
  mutate(
    myageband = case_when(
      age >= 0 & age <= 2 ~  "(0-2]",
      age >  2 & age <= 5 ~  "(2-5]",
      age >  5 & age <= 8 ~  "(5-8]",
      age >  8 ~ "(8-16]"
    )
  )

# -------------------------------------
# Poisson models
# -------------------------------------

# adjusted
m0 <- glm(lex.Xst ~ offset(log(lex.dur)) + factor(myIMD) + factor(mygender) + factor(myageband) +
            factor(index_year), 
          family = "poisson", 
          data = mydat)

summary(m0)

# deviance goodness-of-fit test (if p > 0.05, namely not statistically significant, it implies a good fit):
with(m0, cbind(res.deviance = deviance, df = df.residual, 
                        p = pchisq(deviance, df.residual, lower.tail = FALSE)))

# alternatively for p-value:
pchisq(deviance(m0), df.residual(m0), lower.tail = FALSE)

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


# adjusted  with fructional polynomials on year
# ----------------------------------------------
mydat.2 <- mydat %>% filter(lex.dur > 0.01)

y.kn <- with(mydat.2, quantile(index_year, (1:5-0.5)/5))

m1 <- glm(lex.Xst ~ Ns(index_year, knots = y.kn) + 
            factor(myIMD) + factor(mygender) + factor(myageband), 
          offset = log(lex.dur),
          family = "poisson", 
          data = mydat.2)

summary(m1)


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

m0.nb <- glm.nb(lex.Xst ~ offset(log(lex.dur)) + factor(myIMD) + factor(mygender) + factor(myageband) +
                     factor(index_year), 
                   data = mydat)

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

