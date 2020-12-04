income_index = read.csv("./Income_index.csv", as.is = T)
edu_index = read.csv("./Education_index.csv", as.is = T)
life_index = read.csv("./life_expectancy_index.csv", as.is = T)


head(income_index)

head(edu_index)

data$life_idx = rep(NA, nrow(data))
data$edu_idx = rep(NA, nrow(data))
data$income_idx = rep(NA, nrow(data))

for(i in 1:nrow(data)){
  country_i = data$country[i]
  
  if(!(country_i %in% life_index$Country)){
    next()
  }
  year_i    = data$year[i]
  
  l_idx_i   = life_index[which(life_index$Country == country_i), paste("X",year_i,sep = "")]
  e_idx_i   = edu_index[which(life_index$Country == country_i), paste("X",year_i,sep = "")]
  i_idx_i   = income_index[which(life_index$Country == country_i), paste("X",year_i,sep = "")]
  
  # if null then skip
  if((!is.null(l_idx_i)) ){
    data$life_idx[i]  = life_index[which(life_index$Country == country_i), paste("X",year_i,sep = "")]
  }
  
  if((!is.null(l_idx_i))){
    data$edu_idx[i]  = edu_index[which(life_index$Country == country_i), paste("X",year_i,sep = "")]
  }
  
  if((!is.null(l_idx_i)) ){
    data$income_idx[i]  = income_index[which(life_index$Country == country_i), paste("X",year_i,sep = "")]
  }
  
}


data$life_idx = as.numeric(data$life_idx)
data$edu_idx = as.numeric(data$edu_idx)
data$income_idx = as.numeric(data$income_idx)



cor.test(data$life_idx, data$edu_idx)



country_mean_eduidx <- data %>%
  group_by(country, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, 
            life_index = mean(income_idx, na.rm = T))

model1 <- lm(suicide_per_100k ~ life_index, data = country_mean_eduidx[country_mean_eduidx$continent == "Europe",])
summary(model1)

gdp_suicide_no_outliers <- model1 %>%
  augment() %>%
  #arrange(desc(.cooksd)) %>%
  #filter(.cooksd < 4/nrow(.)) %>% # removes 5/93 countries
  inner_join(country_mean_eduidx, by = c("suicide_per_100k", "life_index")) %>%
  select(country, continent, life_index, suicide_per_100k)

ggplot(gdp_suicide_no_outliers, aes(x = life_index, y = suicide_per_100k, col = continent)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(group = continent), se = F) + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
  labs(title = "Correlation between edu_idx and Suicides per 100k", 
       subtitle = "Plot with high CooksD countries removed (5/93 total)",
       x = "income_idx", 
       y = "Suicides per 100k", 
       col = "Continent")


ggplot(gdp_suicide_no_outliers, aes(x = life_index, y = suicide_per_100k)) + 
  geom_point() + 
  geom_smooth(method = "loess", aes(group = 1)) + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
  labs(title = "Correlation between GDP (per capita) and Suicides per 100k", 
       subtitle = "Plot with high CooksD countries removed (5/93 total)",
       x = "GDP (per capita)", 
       y = "Suicides per 100k", 
       col = "Continent")








continent_income <- data %>%
  group_by(continent) %>%
  summarize(income_idx = (mean(as.numeric(edu_idx), na.rm = T) )) %>%
  arrange(income_idx)

continent_income$continent <- factor(continent_income$continent, ordered = T, levels = continent_income$continent)


continent_time_hdi <- data %>%
  group_by(year, continent) %>%
  filter(year >=1990) %>% 
  summarize(life_idx = mean(as.numeric(life_idx), na.rm = T),
            edu_idx = mean(as.numeric(edu_idx), na.rm = T ),
            income_idx = (mean(as.numeric(income_idx), na.rm = T) )) 

continent_time_hdi$continent <- factor(continent_time_hdi$continent, ordered = T)

continent_time_plot_income <- ggplot(continent_time_hdi, aes(x = year, y = income_idx, col = factor(continent))) + 
  geom_line() + 
  geom_point() + 
  geom_vline(xintercept = 1995, linetype=3)+
  labs(title = "Income Index Trends Over Time, by Continent", 
       x = "Year", 
       y = "Income Index", 
       color = "Continent") + 
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(1990, 2015, 5), minor_breaks = F)
continent_time_plot_income


continent_time_plot_edu <- ggplot(continent_time_hdi, aes(x = year, y = edu_idx, col = factor(continent))) + 
  geom_line() + 
  geom_point() + 
  geom_vline(xintercept = 1995, linetype=3)+
  labs(title = "Education Index Trends Over Time, by Continent", 
       x = "Year", 
       y = "Education Index", 
       color = "Continent") + 
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(1990, 2015, 5), minor_breaks = F)
continent_time_plot_edu


continent_time_plot_life <- ggplot(continent_time_hdi, aes(x = year, y = life_idx, col = factor(continent))) + 
  geom_line() + 
  geom_point() + 
  geom_vline(xintercept = 1995, linetype=3)+
  labs(title = "Life Expectancy Index Trends Over Time, by Continent", 
       x = "Year", 
       y = "Life Expectancy Index", 
       color = "Continent") + 
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(1990, 2015, 5), minor_breaks = F)
continent_time_plot_life

grid.arrange(continent_time_plot_life, continent_time_plot_income, continent_time_plot_edu)

