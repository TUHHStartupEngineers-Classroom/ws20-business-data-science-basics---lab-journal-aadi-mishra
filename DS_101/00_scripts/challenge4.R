library(tidyverse)
library(ggrepel)
library(maps)
library(theme)
covid_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

################# CHALLENGE 1 #################

# Goal: Map the time course of the cumulative Covid-19 cases!
all_countries <- c(covid_tbl$countriesAndTerritories)
countries <- "France|Germany|Spain|India|Turkey|Russia|United_Kingdom|United_States_of_America"

covid_tbl$desired <- ifelse(grepl(countries, all_countries), 1, 0)

covid_csum <- covid_tbl %>%
  mutate(date = dmy(dateRep)) %>%
  # Select relevant columns
  select(date, day, month, year, countriesAndTerritories, continentExp, desired, cases, deaths) %>%
  arrange(countriesAndTerritories, date) %>%
  filter(desired == 1, year == 2020) %>%
  mutate(cum_sum = ave(cases, countriesAndTerritories, FUN=cumsum))
#mutate(max_val = max(cum_sum))

max_cases <- covid_csum %>% slice_max(cum_sum)

covid_csum %>%
  ggplot(aes(x = date, y = cum_sum, color = countriesAndTerritories)) +
  geom_line(size = 0.5) +
  
  
  #geom_smooth(method="auto", se=FALSE, fullrange=FALSE, level=0.95) +
  expand_limits(y = 0) +
  scale_color_brewer(palette = "Set1") +
  # scale_x_date(date_labels = "%b/%d") +
  #scale_x_date(labels = date_format("%b"),breaks = covi) +
  scale_x_date(date_labels = "%b") +
  #scale_x_date(breaks = covid_data_cum_cases$Month %>% unique(),
  #labels = month(covid_data_cum_cases$Month, label = TRUE) %>% unique()) +
  #scale_x_continuous(breaks = sales_by_month_2015$month, 
  #                   labels = month(sales_by_month_2015$month, label = T))
  
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = " Million")) +
  labs(
    title = "COVID-19 confirmed cases worldwide",
    x = "Year 2020",
    y = "Cumulative Cases",
    color = "Countries") +
  #geom_text_repel(data = max_value, aes(x = Date, y = cum_sum, label = Country))
  #geom_label(aes(label = max_cases$cum_sum))
  #geom_point(size = 0.1) +
  #geom_label_repel(data.frame(x = max_cases$Date, y = max_cases$cum_sum), label = max_cases$max_value)
  geom_label_repel(aes(x = date, y = cum_sum, label = cum_sum), 
                   data = max_cases,
                   show.legend = FALSE, 
                   size = 3) 
################# CHALLENGE 2 #################

# Goal: Visualize the distribution of the mortality rate (deaths / population) with geom_map().
# The necessary longitudinal and lateral data can be accessed with this function:
world <- map_data("world")

covid_deaths <- covid_tbl %>%
  filter(year == 2020) %>%
  group_by(countriesAndTerritories) %>%
  summarise(mortality_rate = sum(deaths/popData2019)) %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories)) %>%
  rename(region = countriesAndTerritories)

combined_covid_data <- covid_deaths %>%
  right_join(world, by = "region")

combined_covid_data %>% 
  ggplot() +
  geom_map(aes(long, lat, map_id = region, fill = mortality_rate), map = world) +
  scale_fill_gradient(low = "black", high = "red", labels = scales::percent) +
  #expand_limits(x = combined_covid_data$long, y = combined_covid_data$lat) +
  
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    fill = "Mortality Rate")