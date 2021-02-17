library(tidyverse)
library(janitor)

cdc_data <- read_csv("~/Grad School/2021-01 Spring/IA 530 - Econometrics-Ind Study/ia530_final_project/Data/CDCSuicideRates_StateYearAge1981-1998.csv")

cdc_data <- cdc_data %>% janitor::clean_names()

ggplot(cdc_data %>% filter(age_group=='10-14 yrs'), aes(x=year, y=crude_rate, color=state)) + geom_line()

cdc_data <- cdc_data %>%
  mutate(calc=deaths/population*100000)


summary <- cdc_data %>%
  filter(age_group!='Ages 10 to 49') %>%
  group_by(year, age_group) %>%
  summarize(avg=mean(crude_rate), sd= sd(crude_rate))

ggplot(summary, aes(x=year, y=avg, color=age_group)) + 
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd), width=.2,
                position=position_dodge(.9))


cdc_data_us <- read_csv("~/Grad School/2021-01 Spring/IA 530 - Econometrics-Ind Study/ia530_final_project/Data/CDCSuicideRates_YearAge1999-2019.csv")
cdc_data_us <- cdc_data_us %>% clean_names()

ggplot(cdc_data_us , aes(x=year, y=crude_rate, color=age_group)) + 
  geom_line() + 
  geom_point()

before_99 <- cdc_data %>%
  group_by(year,age_group) %>%
  summarize(deaths=sum(deaths), population=sum(population))

after_99 <- cdc_data_us %>%
  select(year, age_group, deaths, population)

all_data <- bind_rows(before_99, after_99) %>%
  mutate(crude_rate=deaths/population*100000) %>%
  filter(age_group!='Ages 10 to 49')

ggplot(all_data , aes(x=year, y=crude_rate, color=age_group)) + 
  geom_line() + 
  geom_point()

