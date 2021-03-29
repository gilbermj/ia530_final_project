library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(gridExtra)
library(scales)
library(dynlm)
library(fastDummies)
library(urca)
library(vars)

final_data <- read_csv(here('Data','final_data.csv'))

final_data_2 <- final_data %>%
  select(-c(population_num_million, x0_24_suicides, x25on_suicides))

data_longer <- final_data_2 %>%
  pivot_longer(cols=-month_end, names_to='variable', values_to='values')

ggplot(data_longer, aes(x=month_end,y=values)) +
  geom_line() +
  facet_wrap(~variable,scales="free")

main_theme <- theme(panel.grid = element_blank(),
                    panel.background = element_blank(),
                    plot.title = element_text(size=10),
                    plot.subtitle = element_text(size=8),
                    axis.title.x = element_text(size=8),
                    axis.title.y = element_text(size=8),
                    axis.text.x = element_text(size=8),
                    axis.text.y = element_text(size=8)) 

##### Common Variables

temp_list <- list()

# Apple Stock Graph
temp_plot <- ggplot(final_data_2, aes(x=month_end, y=avg_close_apple_stock)) + 
  geom_line() +
  labs(title='Average Close Price for Apple Stock', x='', y='') +
  main_theme +
  scale_y_continuous(labels = dollar_format())

temp_list <- append(temp_list, list(temp_plot))

# AT & T Stock Graph
temp_plot <- ggplot(final_data_2, aes(x=month_end, y=avg_close_atandt_stock)) + 
  geom_line() +
  labs(title='Average Close Price for AT & T Stock', x='', y='') +
  main_theme +
  scale_y_continuous(labels = dollar_format())

temp_list <- append(temp_list, list(temp_plot))

# Verizon Stock Graph
temp_plot <- ggplot(final_data_2, aes(x=month_end, y=avg_close_verizon_stock)) + 
  geom_line() +
  labs(title='Average Close Price for Verizon Stock', x='', y='') +
  main_theme +
  scale_y_continuous(labels = dollar_format())

temp_list <- append(temp_list, list(temp_plot))

# Divorced/Widowed/Separated Graph
temp_plot <- ggplot(final_data_2, aes(x=month_end, y=divorced_widowed_separated)) + 
  geom_line() +
  labs(title='Percent Divorced/Widowed/Separated \nin BRFSS', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

# Personal Savings Rate Graph
temp_plot <- ggplot(final_data_2, aes(x=month_end, y=personal_save_rate)) + 
  geom_line() +
  labs(title='Personal Savings Rate', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

# Unemployment Rate Graph
temp_plot <- ggplot(final_data_2, aes(x=month_end, y=unemp_per)) + 
  geom_line() +
  labs(title='Unemployment Rate', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

grid <- grid.arrange(grobs=temp_list, nrow=2,top='Common Variables')
ggsave(here('Plots', 'common_variables.png'),plot=grid,dpi=600)

##### Age Variables

temp_list <- list()

# Gen Health No Good
temp_plot <- ggplot(final_data_2, aes(x=month_end, y=genhlth_no_good_18_24)) + 
  geom_line() +
  labs(title='Age 24 and Under',subtitle='Percent General Health Poor' ,x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

# Ment Health No Good
temp_plot <- ggplot(final_data_2, aes(x=month_end, y=ment_health_no_good_18_24)) + 
  geom_line() +
  labs(title='', subtitle='Percent Mental Health Not Good', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

# Per 1000 Suicides Graph
temp_plot <- ggplot(final_data_2, aes(x=month_end, y=x0_24_suicides_per_thous)) + 
  geom_line() +
  labs(title='', subtitle='Suicides Per Thousand', x='', y='') +
  main_theme

temp_list <- append(temp_list, list(temp_plot))

# Gen Health No Good
temp_plot <- ggplot(final_data_2, aes(x=month_end, y=genhlth_no_good_25on)) + 
  geom_line() +
  labs(title='Age 25 and Over',subtitle='Percent General Health Poor' ,x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

# Ment Health No Good
temp_plot <- ggplot(final_data_2, aes(x=month_end, y=ment_health_no_good_25on)) + 
  geom_line() +
  labs(title='', subtitle='Percent Mental Health Not Good', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

# Per 1000 Suicides Graph
temp_plot <- ggplot(final_data_2, aes(x=month_end, y=x25on_suicides_per_thous)) + 
  geom_line() +
  labs(title='', subtitle='Suicides Per Thousand', x='', y='') +
  main_theme

temp_list <- append(temp_list, list(temp_plot))

grid <- grid.arrange(grobs=temp_list, nrow=2)


ggsave(here('Plots', 'age_variables.png'),plot=grid,dpi=600)

# Create month dummy variables

final_data_2 <- final_data_2 %>%
  mutate(month_num=month(month_end),
         month_name=month.abb[month_num])

final_data_2 <- dummy_cols(final_data_2, select_columns = c('month_name'))
  
names(final_data_2)[16:27] <- gsub('month_name_','',names(final_data_2)[16:27])

# Convert data to time-series

ts_final_data <- ts(final_data_2, start=c(1999,1), frequency=12)

x24under_suicide_lm <- lm(x0_24_suicides_per_thous ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=ts_final_data)
summary(x24under_suicide_lm)

x25over_suicide_lm <- lm(x25on_suicides_per_thous ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=ts_final_data)
summary(x25over_suicide_lm)

x24under_menthlth_lm <- lm(ment_health_no_good_18_24 ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=ts_final_data)
summary(x24under_menthlth_lm)

x25over_menthlth_lm <- lm(ment_health_no_good_25on ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=ts_final_data)
summary(x25over_menthlth_lm)


# Remove seasonal component from suicide variables

x24under_suicide_decomp <- decompose(ts_final_data[,'x0_24_suicides_per_thous'])
x24under_suicide_sa <- x24under_suicide_decomp$x - x24under_suicide_decomp$seasonal  

x25over_suicide_decomp <- decompose(ts_final_data[,'x25on_suicides_per_thous'])
x25over_suicide_sa <- x25over_suicide_decomp$x - x25over_suicide_decomp$seasonal  

# Remove seasonal component from mental health variables

x24under_ment_hlth_decomp <- decompose(ts_final_data[,'ment_health_no_good_18_24'])
x24under_ment_hlth_sa <- x24under_ment_hlth_decomp$x - x24under_ment_hlth_decomp$seasonal  

x25over_ment_hlth_decomp <- decompose(ts_final_data[,'ment_health_no_good_25on'])
x25over_ment_hlth_sa <- x25over_ment_hlth_decomp$x - x25over_ment_hlth_decomp$seasonal

final_data_3 <- bind_cols(final_data_2, 
                             x24under_suicide_sa=x24under_suicide_sa, 
                             x25over_suicide_sa=x25over_suicide_sa, 
                             x24under_ment_hlth_sa=x24under_ment_hlth_sa, 
                             x25over_ment_hlth_sa=x25over_ment_hlth_sa)

ts_final_data <- ts(final_data_3, start=c(1999,1), frequency=12)

############################ ADF Tests For Levels ##############################

stationarity_vars <- c('genhlth_no_good_18_24', 
                       'genhlth_no_good_25on', 
                       'divorced_widowed_separated', 
                       'avg_close_apple_stock', 
                       'avg_close_atandt_stock', 
                       'avg_close_verizon_stock',
                       'unemp_per',
                       'personal_save_rate',
                       'x24under_suicide_sa',
                       'x25over_suicide_sa',
                       'x24under_ment_hlth_sa',
                       'x25over_ment_hlth_sa')

df_testStat <- c()
df_critValue <- c()
pp_testStat <- c()
pp_critValue <- c()
adfgls_testStat <- c()
adfgls_critValue <- c()
kpss_testStat <- c()
kpss_critValue <- c()
 

for(v in stationarity_vars){
  temp_df <-ur.df(ts_final_data[,v], type=c("trend"), selectlags="BIC")
  df_testStat <- c(df_testStat, temp_df@teststat[1])
  df_critValue <- c(df_critValue, temp_df@cval[1,2])
  
  temp_pp <-ur.pp(ts_final_data[,v], type=("Z-tau"), model=c("trend"))
  pp_testStat <- c(pp_testStat, temp_pp@teststat[1])
  pp_critValue <- c(pp_critValue, temp_pp@cval[1,2])
  
  temp_adfgls <- ur.ers(ts_final_data[,v], type="DF-GLS", model="trend")
  adfgls_testStat <- c(adfgls_testStat, temp_adfgls@teststat[1])
  adfgls_critValue <- c(adfgls_critValue, temp_adfgls@cval[1,2])
  
  temp_kpss <- ur.kpss(ts_final_data[,v], type=c("tau"))
  kpss_testStat <- c(kpss_testStat, temp_kpss@teststat[1])
  kpss_critValue <- c(kpss_critValue, temp_kpss@cval[1,2])
}


stationarity_stats <- tibble(stationarity_vars,
                             df_testStat,
                             df_critValue,
                             pp_testStat,
                             pp_critValue,
                             adfgls_testStat,
                             adfgls_critValue,
                             kpss_testStat,
                             kpss_critValue)

stationarity_stats <- stationarity_stats %>%
  mutate(df_result = if_else(abs(df_testStat)>abs(df_critValue),1,0),
         pp_result = if_else(abs(pp_testStat)>abs(pp_critValue),1,0),
         adfgls_result = if_else(abs(adfgls_testStat)>abs(adfgls_critValue),1,0),
         kpss_result = if_else(abs(kpss_testStat)>abs(kpss_critValue),0,1),
         total_stationary=df_result+pp_result+adfgls_result+kpss_result)


nvar <- nrow(stationarity_stats)
nobs <- nrow(ts_final_data)

final_data_4 <- final_data_3 %>%
  select(month_end) %>%
  filter(month_end!=ymd('1999-01-31'))

for(i in 1:nvar){
  
  if(stationarity_stats[i,'total_stationary']==4){
    var_name <- stationarity_vars[i]
    
    final_data_4 <- final_data_4 %>% 
      mutate(!!var_name:=ts_final_data[2:nobs,stationarity_vars[i]])
  } else {
    var_name <- paste(stationarity_vars[i],'_noTrend',sep='')
    final_data_4 <- final_data_4 %>% 
      mutate(!!var_name:=diff(ts_final_data[,stationarity_vars[i]]))
  }
}

data_longer <- final_data_4 %>%
  pivot_longer(cols=-month_end, names_to='variable', values_to='values')

ggplot(data_longer, aes(x=month_end,y=values)) +
  geom_line() +
  facet_wrap(~variable,scales="free")

x24_under <- final_data_4 %>%
  dplyr::select(-c(genhlth_no_good_25on_noTrend,
            x25over_suicide_sa_noTrend,
            x25over_ment_hlth_sa_noTrend,
            x24under_suicide_sa_noTrend)) %>%
  mutate(x24under_suicide_sa_noTrend=final_data_4$x24under_suicide_sa_noTrend) %>%
  ts()

fit <- VAR(x24_under, lag.max = 4, ic = "AIC")
summary(fit)

x25_over <- final_data_4 %>%
  dplyr::select(-c(genhlth_no_good_18_24_noTrend,
                   x25over_suicide_sa_noTrend,
                   x25over_ment_hlth_sa_noTrend,
                   x24under_suicide_sa_noTrend)) %>%
  mutate(x24under_suicide_sa_noTrend=final_data_4$x24under_suicide_sa_noTrend) %>%
  ts()

fit <- VAR(x24_under, lag.max = 12, ic = "AIC")
summary(fit)

