library(tidyverse)
covid = read_csv("covid19-1.csv") %>% 
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>% 
  janitor::clean_names() %>% 
  group_by(country_region) %>% 
  mutate(earlist = min(date), 
         days = as.integer(date - earlist))

unique(covid$country_region)

mutate(Date = as.Date(Date, "%m/%d/%Y")) %>% 
  janitor::clean_names() %>% 
  group_by(country_region, date) %>% 
  summarize(confirmed_cases = sum(confirmed_cases)) %>% 
  filter(confirmed_cases > 0) %>% 
  mutate(earliest = min(date), 
         days = as.integer(date - earliest)) 


series_df = 
  read_csv("time_series_covid19_confirmed_global_0429.csv") %>% 
  select(-Lat, -Long) 

date_varname = series_df %>% select(-"Province/State", -"Country/Region") %>% names()

covid_new = 
  series_df %>% 
  rename(province_state = "Province/State",
         country_region = "Country/Region") %>% 
  group_by(country_region) %>%
  summarize_at(date_varname, sum) %>% 
  gather(key = "date", value = "number", "1/22/20":"4/29/20") %>% 
  filter(number != 0) %>% 
  arrange(country_region)
