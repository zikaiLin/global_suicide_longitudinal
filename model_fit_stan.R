library(nlme)
library(sjPlot)
library(lme4)
library(dplyr)
library(fastDummies)
suicide_dat = read.csv("./Exploratory/main_data.csv", as.is = T)


# Factorized variable 
suicide_dat$country = factor(suicide_dat$country)
suicide_dat$sex = factor(suicide_dat$sex)
suicide_dat$continent = factor(suicide_dat$continent)
suicide_dat$age = factor(suicide_dat$age)
#suicide_dat$continent = relevel(suicide_dat$continent, ref = "Oceania")


suicide_dat_continent_year = suicide_dat %>% 
  group_by(continent, year) %>% 
  summarise(mean_edu_idx = mean(edu_idx, na.rm = T),
            mean_life_idx = mean(life_idx, na.rm = T),
            mean_income_idx = mean(income_idx, na.rm = T))%>%
  filter(year >= 1990)

suicide_dat = suicide_dat %>% filter(year>=1990)

# Imputation
for(i in 1:nrow(suicide_dat)){
  if(is.na(suicide_dat$life_idx[i])){
    continent_i = suicide_dat$continent[i]
    year_i = suicide_dat$year[i]
    suicide_dat[i, c("life_idx")] = suicide_dat_continent_year[which(suicide_dat_continent_year$year == year_i &suicide_dat_continent_year$continent == continent_i), c("mean_life_idx")]  
  } 
  
  if(is.na(suicide_dat$edu_idx[i])){
    continent_i = suicide_dat$continent[i]
    year_i = suicide_dat$year[i]
    suicide_dat[i, c("edu_idx")] = suicide_dat_continent_year[which(suicide_dat_continent_year$year == year_i &suicide_dat_continent_year$continent == continent_i), c("mean_edu_idx")]  
  } 
  
  if(is.na(suicide_dat$income_idx[i])){
    continent_i = suicide_dat$continent[i]
    year_i = suicide_dat$year[i]
    suicide_dat[i, c("income_idx")] = suicide_dat_continent_year[which(suicide_dat_continent_year$year == year_i &suicide_dat_continent_year$continent == continent_i), c("mean_income_idx")]  
  } 
}

# model the suicide rate instead of suicide numbers
suicide_dat = suicide_dat %>%
  mutate(suicides_rate = suicides_no/population * 100000) %>%
  select(-c(gdp_for_year, generation)) %>%
  filter(continent != "Oceania")%>%
  mutate(country_num = as.integer(country),
         year = year - min(year))

# Year - 1995 variable
suicide_dat$year_1995 = suicide_dat$year - 5
suicide_dat$year_1995[suicide_dat$year_1995<0] = 0
suicide_dat$suicides_no = suicide_dat$suicides_no+1

library(rstanarm)
library(mgcv)
fit_stan = stan_glmer(formula = suicides_rate ~ year+sex+life_idx+edu_idx+income_idx+ continent+age+
             year_1995+year*continent + year_1995*continent + edu_idx*continent +
             life_idx*continent + income_idx*continent + (1 + year + year_1995|country_num), 
           data = suicide_dat,
           cores = 3,
           chains = 3,
           adapt_delta = 0.8,
           seed = 12345,
           prior = normal(0,1),refresh=1, open_progress = T)


save(list=ls(), file = "./fit_stan_glmer_random_intercept_only.RData")
