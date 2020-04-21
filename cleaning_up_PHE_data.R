#### Load libraries
library(tidyverse)
library(janitor)
library(glue)

#### date of download
download_date <- Sys.Date()

#### if not already downloaded, download latest files from https://coronavirus.data.gov.uk/
if(!file.exists(glue("daily_download/coronavirus_cases_{download_date}.csv"))){
  download.file("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv", glue("daily_download/coronavirus_cases_{download_date}.csv"))
}
if(!file.exists(glue("daily_download/coronavirus_deaths_{download_date}.csv"))){
  download.file("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-deaths_latest.csv", glue("daily_download/coronavirus_deaths_{download_date}.csv"))
}

#### Load data from https://coronavirus.data.gov.uk/
phe_cases <- read_csv(glue("daily_download/coronavirus_cases_{download_date}.csv")) %>% clean_names()
phe_deaths <- read_csv(glue("daily_download/coronavirus_deaths_{download_date}.csv")) %>% clean_names()

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
  write_csv(clean_phe_cases_daily[[x]], glue("daily_output/{x}_daily_cases_{download_date}.csv"))
  write_csv(clean_phe_cases_cumulative[[x]], glue("daily_output/{x}_cumulative_cases_{download_date}.csv"))
})


#### Daily deaths
clean_phe_deaths_daily <- phe_deaths %>% 
  mutate(area_type = case_when(area_type == "UK" ~ "Nation", 
                               TRUE ~ area_type)) %>% 
  select(-cumulative_hospital_deaths) %>%
  arrange(reporting_date) %>%
  pivot_wider(data=., names_from = reporting_date, values_from = daily_hospital_deaths) %>% 
  clean_names() 

#### Cumulative deaths
clean_phe_deaths_cumulative <- phe_deaths %>% 
  mutate(area_type = case_when(area_type == "UK" ~ "Nation", 
                               TRUE ~ area_type)) %>% 
  select(-daily_hospital_deaths) %>%
  arrange(reporting_date) %>%
  pivot_wider(data=., names_from = reporting_date, values_from = cumulative_hospital_deaths) %>% 
  clean_names() 

#### Export .csvs
write_csv(clean_phe_deaths_daily, glue("daily_output/Country_daily_deaths_{download_date}.csv"))
write_csv(clean_phe_deaths_cumulative, glue("daily_output/Country_cumulative_deaths_{download_date}.csv"))
