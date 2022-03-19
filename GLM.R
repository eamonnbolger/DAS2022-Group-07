library(tidyverse)
library(kableExtra)
library(skimr)
library(ggcorrplot)
library(gridExtra)
library(countrycode)
library(moderndive)
library(jtools)

coffee <- read.csv('dataset7.csv')
coffee <- as_tibble(coffee)
coffee <- coffee %>% 
  rename(altitude = altitude_mean_meters) %>% 
  rename(defects = category_two_defects) %>% 
  rename(country = country_of_origin)
glimpse(coffee)

# removing the two data errors for altitude
coffee$altitude <- ifelse(coffee$altitude > 8000, NA, coffee$altitude)
#removing na obs
coffee <- coffee %>% na.omit()
coffee <- as.data.frame(coffee)

coffee$continent = countrycode(sourcevar = coffee[,"country"],
                               origin="country.name",
                               destination = "continent")
coffee[c(374,408,764,787),"continent"] <- 'Americas'
coffee <- as_tibble(coffee)


#GLM Model Selection
coffee1 <- as.data.frame(coffee)
head(coffee1)

coffee1[which(coffee1$Qualityclass=="Poor"),]$Qualityclass=0
coffee1[which(coffee1$Qualityclass=="Good"),]$Qualityclass=1

coffee1$Qualityclass = as.factor(coffee1$Qualityclass)

# Logit link 
logit_model = glm(Qualityclass~aroma + flavor + acidity + defects + altitude + harvested + continent, data = coffee1, family = binomial(link = "logit"))
both_logit = step(logit_model,direction="both")
summary(both_logit)
summ(both_logit)
## formula = Qualityclass ~ aroma + flavor + acidity + altitude + harvested
## AIC: 533.47, BIC = 562.23
## Fittness of model
summary(both_logit)$null.deviance - summary(both_logit)$deviance > qchisq(0.95,891-886)
## TRUE. we can reject the null hypothesis, and the terms are all significant


# Probit link
probit_model = glm(Qualityclass~aroma + flavor + acidity + defects + altitude + harvested + continent, data = coffee1, family = binomial(link = "probit"))
both_probit = step(probit_model,direction = "both")
summary(both_probit)
summ(both_probit)
## formula = Qualityclass ~ aroma + flavor + acidity + altitude + harvested
## AIC = 554.03, BIC = 582.79
## Fittness of model
summary(both_probit)$null.deviance - summary(both_probit)$deviance > qchisq(0.95,891-886)
## TRUE. we can reject the null hypothesis, and the terms are all significant


# Cloglog link
clog_model = glm(Qualityclass~aroma + flavor + acidity + defects + altitude + harvested + continent, data = coffee1, family = binomial(link = "cloglog"))
both_clog = step(clog_model,direction = "both")
summary(both_clog)
summ(both_clog)
## formula = Qualityclass ~ aroma + flavor + acidity + harvested
## AIC = 636.96, BIC = 660.93
## Fittness of model
summary(both_clog)$null.deviance - summary(both_clog)$deviance > qchisq(0.95,891-887)
## TRUE. we can reject the null hypothesis, and the terms are all significant

#
mod = glm(Qualityclass~ aroma:flavor:acidity + altitude + harvested, data = coffee1, family = binomial(link = "logit"))
summ(mod)
mod1 = glm(Qualityclass~ aroma + flavor + acidity + aroma:flavor:acidity + altitude + harvested, data = coffee1, family = binomial(link = "logit"))
summary(mod1)
