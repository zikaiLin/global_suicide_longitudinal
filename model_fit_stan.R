library(nlme)
library(sjPlot)
library(lme4)
library(dplyr)
library(fastDummies)
# suicide_dat = read.csv("./Exploratory/main_data.csv", as.is = T)
# 
# 
# # Factorized variable 
# suicide_dat$country = factor(suicide_dat$country)
# suicide_dat$sex = factor(suicide_dat$sex)
# suicide_dat$continent = factor(suicide_dat$continent)
# suicide_dat$age = factor(suicide_dat$age)
# #suicide_dat$continent = relevel(suicide_dat$continent, ref = "Oceania")
# 
# 
# suicide_dat_continent_year = suicide_dat %>% 
#   group_by(continent, year) %>% 
#   summarise(mean_edu_idx = mean(edu_idx, na.rm = T),
#             mean_life_idx = mean(life_idx, na.rm = T),
#             mean_income_idx = mean(income_idx, na.rm = T))%>%
#   filter(year >= 1990)
# 
# suicide_dat = suicide_dat %>% filter(year>=1990)
# 
# # Imputation
# for(i in 1:nrow(suicide_dat)){
#   if(is.na(suicide_dat$life_idx[i])){
#     continent_i = suicide_dat$continent[i]
#     year_i = suicide_dat$year[i]
#     suicide_dat[i, c("life_idx")] = suicide_dat_continent_year[which(suicide_dat_continent_year$year == year_i &suicide_dat_continent_year$continent == continent_i), c("mean_life_idx")]  
#   } 
#   
#   if(is.na(suicide_dat$edu_idx[i])){
#     continent_i = suicide_dat$continent[i]
#     year_i = suicide_dat$year[i]
#     suicide_dat[i, c("edu_idx")] = suicide_dat_continent_year[which(suicide_dat_continent_year$year == year_i &suicide_dat_continent_year$continent == continent_i), c("mean_edu_idx")]  
#   } 
#   
#   if(is.na(suicide_dat$income_idx[i])){
#     continent_i = suicide_dat$continent[i]
#     year_i = suicide_dat$year[i]
#     suicide_dat[i, c("income_idx")] = suicide_dat_continent_year[which(suicide_dat_continent_year$year == year_i &suicide_dat_continent_year$continent == continent_i), c("mean_income_idx")]  
#   } 
# }
# 
# # model the suicide rate instead of suicide numbers
# suicide_dat = suicide_dat %>%
#   select(-c(gdp_for_year, generation)) %>%
#   filter(continent != "Oceania")%>%
#   mutate(country_num = as.integer(country),
#          year = year - min(year))
# 
# # Year - 1995 variable
# eps=1e-7
# suicide_dat$year_1995 = suicide_dat$year - 5
# suicide_dat$year_1995[suicide_dat$year_1995<0] = 0
# suicide_dat$suicides_no = suicide_dat$suicides_no
# suicide_dat$suicides_rate = suicide_dat$suicides_no/suicide_dat$population
# suicide_dat$log_suicides_rate = log(suicide_dat$suicides_rate+eps)
# suicide_dat$age = plyr::revalue(suicide_dat$age, c("75+" = "old",
#                                                    "55-74" =  "old",
#                                                    "25-34" = "middle_age",
#                                                    "35-54" = "middle_age",
#                                                    "5-14" = "young",
#                                                    "15-24" = "young"))
# suicide_dat$continent = relevel(suicide_dat$continent, ref = "Europe")
# suicide_dat$sex = relevel(suicide_dat$sex, ref = "Male")
#   
library(rstan)
library(rstanarm)
library(mgcv)

suicide_dat = read.csv("./combined_age_cat.csv")
eps=1e-7
suicide_dat$year = suicide_dat$year - 1990
suicide_dat$suicides_rate = suicide_dat$suicides_no/suicide_dat$population
suicide_dat$suicides_rate[suicide_dat$suicides_rate==0] = eps
suicide_dat$log_suicides_rate = log(suicide_dat$suicides_rate)
suicide_dat$continent = relevel(suicide_dat$continent, ref = "Europe")
suicide_dat$sex = relevel(suicide_dat$sex, ref = "Male")
suicide_dat$age_cat = relevel(suicide_dat$age_cat, ref = "55+")


fit_stan = stan_glmer(formula = log_suicides_rate ~ year+sex+life_idx+edu_idx+income_idx+ continent+age_cat
            + sex*age_cat + continent*sex + continent*age_cat  + edu_idx*continent +
             life_idx*continent + income_idx*continent + (1+year|country), 
           data = suicide_dat,
           cores = 2,
           chains = 2,
           adapt_delta = 0.8,
           iter = 1000,
           prior = normal(0,1),refresh=10)


loo_fit_stan_intecept_slope_final = loo(fit_stan, cores = 3)
save(list=c("fit_stan", "suicide_dat", "loo_fit_stan_intecept_slope_final"),
     file = "./fit_stan_glmer_random_intercept_slope_log_final.RData")

shinystan::launch_shinystan(fit_stan)
pp_check(fit_stan, plotfun = "stat_2d", stat = c("mean", "sd"))


new_data = 