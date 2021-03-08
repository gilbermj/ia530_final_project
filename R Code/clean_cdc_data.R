library(tidyverse)
library(lubridate)
library(here)
library(janitor)

cdc_suicide_data_all <- read_delim(here('Data','suicide_data_1999_2019.txt'), delim='\tab') %>%
  clean_names() %>%
  select(-c(population, crude_rate, notes))

cdc_suicide_data <- cdc_suicide_data_all %>%
  filter(!is.na(ten_year_age_groups) & ten_year_age_groups!='Not Stated' & deaths!=0) %>%
  mutate(age_group = factor(ten_year_age_groups_code, levels=c("1","1-4","5-14","15-24","25-34","35-44","45-54","55-64","65-74","75-84","85+"))) %>%
  separate(month_code, sep='/', into=c(NA,'month')) %>%
  mutate(year_month = rollback(ceiling_date(ymd(paste(as.character(year),month, '01')),unit='month')),
         year_cat = as.character(year))













