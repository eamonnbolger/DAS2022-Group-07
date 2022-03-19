library(tidyverse)
library(kableExtra)
library(skimr)
library(ggcorrplot)
library(gridExtra)

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

coffee1$Qualityclass = as.factor(coffee1$Qualityclass)

full_model = glm(Qualityclass~aroma + flavor + acidity + defects + altitude + harvested + continent, data = coffee1, family = binomial(link = "logit"))

backwards = step(model)
summary(backwards)