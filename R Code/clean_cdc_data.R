library(tidyverse)
library(lubridate)
library(here)
library(janitor)

cdc_suicide_data_all <- read_delim(here('Data','cdc_suicide_data.txt'), delim='\tab') %>%
  clean_names() %>%
  select(-c(population, crude_rate, notes))

cdc_suicide_data <- cdc_suicide_data_all %>%
  filter(!is.na(ten_year_age_groups)) %>%
  mutate(age_group = factor(ten_year_age_groups_code, levels=c("5-14","15-24","25-34","35-44","45-54","55-64","65-74","75-84","85+"))) %>%
  separate(month_code, sep='/', into=c(NA,'month')) %>%
  mutate(year_month = rollback(ceiling_date(ymd(paste(as.character(year), '01',month)),unit='month')))

cdc_suicide_data_totals <- cdc_suicide_data_all %>%
  filter(is.na(ten_year_age_groups) & !is.na(census_region))


ggplot(cdc_suicide_data %>% filter(census_region_code=='CENS-R1'), aes(x=year_month,y=deaths, color=age_group)) +
  geom_point()












