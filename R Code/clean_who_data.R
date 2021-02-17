library(tidyverse)
library(here)

mort10_1 <- read_csv(here('Data','who_mortality_data','Morticd10_part1','Morticd10_part1'), col_types = cols(.default = "c"))
mort10_2 <- read_csv(here('Data','who_mortality_data','Morticd10_part2','Morticd10_part2'), col_types = cols(.default = "c"))
mort09_1 <- read_csv(here('Data','who_mortality_data','morticd9','Morticd9'), col_types = cols(.default = "c"))
mort08_1 <- read_csv(here('Data','who_mortality_data','morticd08','Morticd8'), col_types = cols(.default = "c"))
mort07_1 <- read_csv(here('Data','who_mortality_data','morticd07','Morticd7'), col_types = cols(.default = "c"))

us_mort <- mort10_1 %>%
  filter(Country=='2450') %>%
  mutate(Cause_Letter = gsub('[[:digit:]]','',Cause,ignore.case=TRUE),
         Cause_Letter = gsub('[[:digit:]]','',Cause,ignore.case=TRUE) )

