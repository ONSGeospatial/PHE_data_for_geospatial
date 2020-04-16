#### Script for 
library(tidyverse)
library(janitor)
library(glue)

#### Load data from https://coronavirus.data.gov.uk/
phe_cases <- read_csv("data/coronavirus-cases.csv") %>% clean_names()
phe_deaths <- read_csv("data/coronavirus-deaths.csv") %>% clean_names()

#### Daily cases
clean_phe_cases_daily <- phe_cases %>% 
  select(-cumulative_lab_confirmed_cases) %>%
  group_by(area_type) %>% 
  arrange(specimen_date) %>% 
  pivot_wider(data=., names_from = specimen_date, values_from = daily_lab_confirmed_cases) %>% 
  clean_names() %>% 
  group_split() %>% 
  map(.,arrange,area_code)

names(clean_phe_cases_daily) <- map(clean_phe_cases_daily, `[[`,1,"area_type") %>% str_replace_all(" ","_")

#### Cumulative cases
clean_phe_cases_cumulative <- phe_cases %>% 
  select(-daily_lab_confirmed_cases) %>%
  group_by(area_type) %>% 
  arrange(specimen_date) %>% 
  pivot_wider(data=., names_from = specimen_date, values_from = cumulative_lab_confirmed_cases) %>% 
  clean_names() %>% 
  group_split() %>% 
  map(.,arrange,area_code)

names(clean_phe_cases_cumulative) <- map(clean_phe_cases_cumulative, `[[`,1,"area_type") %>% str_replace_all(" ","_")

#### Export .csvs
map(names(clean_phe_cases_daily), function(x){
  write_csv(clean_phe_cases_daily[[x]], glue("output/phe/{x}_daily_cases.csv"))
  write_csv(clean_phe_cases_cumulative[[x]], glue("output/phe/{x}_cumulative_cases.csv"))
})


#### Daily deaths
clean_phe_deaths_daily <- phe_deaths %>% 
  mutate(area_type = case_when(area_type == "Country - UK" ~ "Country", 
                               TRUE ~ area_type)) %>% 
  select(-cumulative_hospital_deaths) %>%
  arrange(reporting_date) %>%
  pivot_wider(data=., names_from = reporting_date, values_from = daily_hospital_deaths) %>% 
  clean_names() 

#### Cumulative deaths
clean_phe_deaths_cumulative <- phe_deaths %>% 
  mutate(area_type = case_when(area_type == "Country - UK" ~ "Country", 
                               TRUE ~ area_type)) %>% 
  select(-daily_hospital_deaths) %>%
  arrange(reporting_date) %>%
  pivot_wider(data=., names_from = reporting_date, values_from = cumulative_hospital_deaths) %>% 
  clean_names() 

#### Export .csvs
write_csv(clean_phe_deaths_daily, "output/phe/Country_daily_deaths.csv")
write_csv(clean_phe_deaths_cumulative, "output/phe/Country_cumulative_deaths.csv")

