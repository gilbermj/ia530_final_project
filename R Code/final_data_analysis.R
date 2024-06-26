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
library(gt)
library(webshot)

final_data <- read_csv(here('Data','final_data.csv'))
all_names_cleaned <- read_csv(here('Data','all_names_cleaned.csv'))


final_data_2 <- final_data %>%
  dplyr::select(-c(population_num_million, x0_24_suicides, x25on_suicides))

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
ggsave(here('Plots', 'common_variables.png'),plot=grid,dpi=600, width = 11.5, height = 9, units='in')

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


ggsave(here('Plots', 'age_variables.png'),plot=grid,dpi=600, width = 11.5, height = 9, units='in')

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
  temp_df <-ur.df(ts_final_data[,v], type=c('trend'), selectlags='BIC')
  df_testStat <- c(df_testStat, temp_df@teststat[1])
  df_critValue <- c(df_critValue, temp_df@cval[1,2])
  
  temp_pp <-ur.pp(ts_final_data[,v], type=('Z-tau'), model=c('trend'))
  pp_testStat <- c(pp_testStat, temp_pp@teststat[1])
  pp_critValue <- c(pp_critValue, temp_pp@cval[1,2])
  
  temp_adfgls <- ur.ers(ts_final_data[,v], type='DF-GLS', model='trend')
  adfgls_testStat <- c(adfgls_testStat, temp_adfgls@teststat[1])
  adfgls_critValue <- c(adfgls_critValue, temp_adfgls@cval[1,2])
  
  temp_kpss <- ur.kpss(ts_final_data[,v], type=c('tau'))
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
  dplyr::select(month_end) %>%
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

write_csv(final_data_4, here('Data','final_data_4.csv'))

#############

main_theme <- theme(panel.grid = element_blank(),
                    panel.background = element_blank(),
                    plot.title = element_text(size=9),
                    plot.subtitle = element_text(size=8),
                    axis.title.x = element_text(size=8),
                    axis.title.y = element_text(size=8),
                    axis.text.x = element_text(size=8),
                    axis.text.y = element_text(size=8)) 

x24_under <- final_data_4 %>%
  dplyr::select(-c(genhlth_no_good_25on_noTrend,
            x25over_suicide_sa_noTrend,
            x25over_ment_hlth_sa_noTrend)) 
x24_under_ts <- ts(x24_under)
x24_under_var <- VAR(x24_under_ts, lag.max = 4, ic = 'AIC')

x24under_names <- names(x24_under)



######################


lower <- tibble(month=1:13)
irf <- tibble(month=1:13)
upper <- tibble(month=1:13)

for(name in x24under_names){
  
  if(name!='month_end'){
  
  temp_irf <- irf(x24_under_var,impulse=c(name), response=c("x24under_suicide_sa_noTrend"), n.ahead=12, cumulative = TRUE,runs=10, ci=0.95)

  lower <- bind_cols(lower, !!name:=temp_irf$Lower[[1]])
  upper <- bind_cols(upper, !!name:=temp_irf$Upper[[1]])
  irf <- bind_cols(irf, !!name:=temp_irf$irf[[1]])
  
  }
}

min_lower <- min(lower[,2:ncol(lower)])
max_upper <- max(upper[,2:ncol(upper)])

temp_list <- list() 

for(name in x24under_names){
  
  clean_name <- as.character(all_names_cleaned %>% filter(all_names==name) %>% dplyr::select(clean_name))
  
  if(name!='month_end'){
    
    temp_plot <- ggplot(data=lower, aes_string(x='month', y=name)) +
      geom_line(color='red', alpha=0.75, linetype = "dashed") + 
      geom_line(data=irf, aes_string(x='month', y=name)) + 
      geom_line(data=upper, aes_string(x='month', y=name), color='red', alpha=0.75, linetype = "dashed") + 
      geom_hline(yintercept=0) +
      scale_x_continuous(breaks=c(2,4,6,8,10,12)) +
      scale_y_continuous(labels = percent, limits=c(min_lower,max_upper)) +
      main_theme +
      labs(x='Month', 
          y='Percent', 
          title=paste('Response of 24 and Under Suicides \nto a ',clean_name,' Shock',sep=''))
    
    temp_list <- append(temp_list, list(temp_plot))
    
  }
}

grid <- grid.arrange(grobs=temp_list, nrow=3)
ggsave(here('Plots', '24_and_under_VAR_shocks.png'),plot=grid,dpi=600, width = 11.5, height = 9, units='in')

x25_over <- final_data_4 %>%
  dplyr::select(-c(genhlth_no_good_18_24_noTrend,
                   x24under_suicide_sa_noTrend,
                   x24under_ment_hlth_sa_noTrend))

x25_over_ts <- ts(x25_over)
x25_over_var <- VAR(x25_over_ts, lag.max = 12, ic = 'AIC')
x25_over_names <- names(x25_over)

######################


lower <- tibble(month=1:13)
irf <- tibble(month=1:13)
upper <- tibble(month=1:13)

for(name in x25_over_names){
  
  if(name!='month_end'){
    
    temp_irf <- irf(x25_over_var,impulse=c(name), response=c("x25over_suicide_sa_noTrend"), n.ahead=12, cumulative = TRUE,runs=10, ci=0.95)
    
    lower <- bind_cols(lower, !!name:=temp_irf$Lower[[1]])
    upper <- bind_cols(upper, !!name:=temp_irf$Upper[[1]])
    irf <- bind_cols(irf, !!name:=temp_irf$irf[[1]])
    
  }
}

min_lower <- min(lower[,2:ncol(lower)])
max_upper <- max(upper[,2:ncol(upper)])

temp_list <- list() 

for(name in x25_over_names){
  
  clean_name <- as.character(all_names_cleaned %>% filter(all_names==name) %>% dplyr::select(clean_name))
  
  if(name!='month_end'){
    
    temp_plot <- ggplot(data=lower, aes_string(x='month', y=name)) +
      geom_line(color='red', alpha=0.75, linetype = "dashed") + 
      geom_line(data=irf, aes_string(x='month', y=name)) + 
      geom_line(data=upper, aes_string(x='month', y=name), color='red', alpha=0.75, linetype = "dashed") + 
      geom_hline(yintercept=0) +
      scale_x_continuous(breaks=c(2,4,6,8,10,12)) +
      scale_y_continuous(labels = percent, limits=c(min_lower,max_upper)) +
      main_theme +
      labs(x='Month', 
           y='Percent', 
           title=paste('Response of 25 and Over Suicides \nto a ',clean_name,' Shock',sep=''))
    
    temp_list <- append(temp_list, list(temp_plot))
    
  }
}

grid <- grid.arrange(grobs=temp_list, nrow=3)
ggsave(here('Plots', '25_and_over_VAR_shocks.png'),plot=grid,dpi=600, width = 11.5, height = 9, units='in')

max_lag <- 24

ardl_aic <- c()
ardl_models <- c()

for(i in 1:max_lag){
  temp_ARDL <- dynlm(x24under_suicide_sa_noTrend ~ 
                      L(x24under_suicide_sa_noTrend, 1:i) +
                      L(genhlth_no_good_18_24_noTrend, 0:i) + 
                      L(divorced_widowed_separated_noTrend, 0:i) + 
                      L(avg_close_apple_stock_noTrend, 0:i) +
                      L(avg_close_atandt_stock_noTrend, 0:i) +
                      L(avg_close_verizon_stock_noTrend, 0:i) +
                      L(unemp_per_noTrend, 0:i) +
                      L(personal_save_rate_noTrend, 0:i) +
                      L(x24under_ment_hlth_sa_noTrend, 0:i)
                    , data = x24_under_ts)
  
  ardl_models <- append(ardl_models, list(temp_ARDL))
  ardl_aic <- append(ardl_aic, abs(AIC(temp_ARDL)))
}

ardl_aic_tibble <- tibble(lag=1:max_lag, aic=ardl_aic)

aic_plot <- ggplot(ardl_aic_tibble, aes(x=lag, y=aic)) +
  geom_line() +
  geom_point() +
  main_theme +
  scale_x_continuous(breaks=seq(from=2, to=max_lag, by =2)) +
  labs(title='AIC For Various Lags for 25 & Over ADRL Models', x='Lag', y='AIC')

ggsave(here('Plots', 'aic_graph_24under.png'),plot=aic_plot,dpi=600, width = 11.5, height = 9, units='in')


best_model_coef <- coef(summary(ardl_models[[which.min(ardl_aic)]]))

best_model_tibble <- bind_cols(var_name=rownames(best_model_coef), as_tibble(best_model_coef)) %>%
  clean_names() %>%
  mutate(Lag =  rep(seq(0,which.min(ardl_aic)),9))

best_model_pretty_table <- best_model_tibble %>% 
  mutate(pretty_var_name =  gsub('L\\(','',gsub(',.*','',var_name))) %>%
  left_join(all_names_cleaned, by=c('pretty_var_name'='all_names')) %>%
  mutate(clean_name = if_else(is.na(clean_name),'Intercept', clean_name)) %>%
  filter(pr_t<=0.05) %>%
  rename('P-value' = pr_t,
         'Estimate'=estimate,
         'Standard Error'=std_error,
         'Test Statistic'=t_value,
         'Variable Name'=clean_name) %>%
  dplyr::select('Variable Name',
                'Lag',
                'Estimate',
                'Test Statistic',
                'Standard Error',
                'P-value') %>%
  gt() 

gt::gtsave(best_model_pretty_table,here('Plots', '24under_armaModel_coef.png'))

max_lag <- 24

ardl_aic <- c()
ardl_models <- c()

for(i in 1:max_lag){
  temp_ARDL <- dynlm(x25over_suicide_sa_noTrend ~ 
                       L(x25over_suicide_sa_noTrend, 1:i) +
                       L(genhlth_no_good_25on_noTrend, 0:i) + 
                       L(divorced_widowed_separated_noTrend, 0:i) + 
                       L(avg_close_apple_stock_noTrend, 0:i) +
                       L(avg_close_atandt_stock_noTrend, 0:i) +
                       L(avg_close_verizon_stock_noTrend, 0:i) +
                       L(unemp_per_noTrend, 0:i) +
                       L(personal_save_rate_noTrend, 0:i) +
                       L(x25over_ment_hlth_sa_noTrend, 0:i)
                     , data = x25_over_ts)
  
  ardl_models <- append(ardl_models, list(temp_ARDL))
  ardl_aic <- append(ardl_aic, abs(AIC(temp_ARDL)))
}

ardl_aic_tibble <- tibble(lag=1:max_lag, aic=ardl_aic)

aic_plot <- ggplot(ardl_aic_tibble, aes(x=lag, y=aic)) +
  geom_line() +
  geom_point() +
  main_theme +
  scale_x_continuous(breaks=seq(from=2, to=max_lag, by =2)) +
  labs(title='AIC For Various Lags for 25 & Over ADRL Models', x='Lag', y='AIC')

ggsave(here('Plots', 'aic_graph_25over.png'),plot=aic_plot,dpi=600, width = 11.5, height = 9, units='in')


best_model_coef <- coef(summary(ardl_models[[which.min(ardl_aic)]]))

best_model_tibble <- bind_cols(var_name=rownames(best_model_coef), as_tibble(best_model_coef)) %>%
  clean_names() %>%
  mutate(Lag =  rep(seq(0,which.min(ardl_aic)),9))

best_model_pretty_table <- best_model_tibble %>% 
  mutate(pretty_var_name =  gsub('L\\(','',gsub(',.*','',var_name))) %>%
  left_join(all_names_cleaned, by=c('pretty_var_name'='all_names')) %>%
  mutate(clean_name = if_else(is.na(clean_name),'Intercept', clean_name)) %>%
  filter(pr_t<=0.05) %>%
  rename('P-value' = pr_t,
         'Estimate'=estimate,
         'Standard Error'=std_error,
         'Test Statistic'=t_value,
         'Variable Name'=clean_name) %>%
  dplyr::select('Variable Name',
                'Lag',
                'Estimate',
                'Test Statistic',
                'Standard Error',
                'P-value') %>%
  gt() 

gt::gtsave(best_model_pretty_table,here('Plots', '25over_armaModel_coef.png'))

## Graph After Seasonal
temp_list <- list()

temp_plot <- ggplot(final_data_3, aes(x=month_end, y=ment_health_no_good_18_24)) + 
  geom_line() +
  labs(title='Age 24 and Under', subtitle='Percent Mental Health Not Good', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

temp_plot <- ggplot(final_data_3, aes(x=month_end, y=x24under_ment_hlth_sa)) + 
  geom_line() +
  labs(title='', subtitle='Percent Mental Health Not Good - Seasonally Adjusted', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

temp_plot <- ggplot(final_data_3, aes(x=month_end, y=x0_24_suicides_per_thous)) + 
  geom_line() +
  labs(title='', subtitle='Suicides Per Thousand', x='', y='') +
  main_theme 

temp_list <- append(temp_list, list(temp_plot))

temp_plot <- ggplot(final_data_3, aes(x=month_end, y=x24under_suicide_sa)) + 
  geom_line() +
  labs(title='', subtitle='Suicides Per Thousand - Seasonally Adjusted', x='', y='') +
  main_theme 

temp_list <- append(temp_list, list(temp_plot))

temp_plot <- ggplot(final_data_3, aes(x=month_end, y=ment_health_no_good_25on)) + 
  geom_line() +
  labs(title='Age 25 and Over', subtitle='Percent Mental Health Not Good', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

temp_plot <- ggplot(final_data_3, aes(x=month_end, y=x25over_ment_hlth_sa)) + 
  geom_line() +
  labs(title='', subtitle='Percent Mental Health Not Good - Seasonally Adjusted', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

temp_plot <- ggplot(final_data_3, aes(x=month_end, y=x25on_suicides_per_thous)) + 
  geom_line() +
  labs(title='', subtitle='Suicides Per Thousand', x='', y='') +
  main_theme 

temp_list <- append(temp_list, list(temp_plot))

temp_plot <- ggplot(final_data_3, aes(x=month_end, y=x25over_suicide_sa)) + 
  geom_line() +
  labs(title='', subtitle='Suicides Per Thousand - Seasonally Adjusted', x='', y='') +
  main_theme 

temp_list <- append(temp_list, list(temp_plot))

grid <- grid.arrange(grobs=temp_list, nrow=4,top='Before & After Seasonal Adjustment')
ggsave(here('Plots', 'ba_season_adjust.png'),plot=grid,dpi=600, width = 8, height = 11, units='in')

## Graph Final Variables

##### Common Variables

temp_list <- list()

# Apple Stock Graph
temp_plot <- ggplot(final_data_4, aes(x=month_end, y=avg_close_apple_stock_noTrend)) + 
  geom_line() +
  labs(title='Average Close Price for Apple Stock', x='', y='') +
  main_theme +
  scale_y_continuous(labels = dollar_format())

temp_list <- append(temp_list, list(temp_plot))

# AT & T Stock Graph
temp_plot <- ggplot(final_data_4, aes(x=month_end, y=avg_close_atandt_stock_noTrend)) + 
  geom_line() +
  labs(title='Average Close Price for AT & T Stock', x='', y='') +
  main_theme +
  scale_y_continuous(labels = dollar_format())

temp_list <- append(temp_list, list(temp_plot))

# Verizon Stock Graph
temp_plot <- ggplot(final_data_4, aes(x=month_end, y=avg_close_verizon_stock_noTrend)) + 
  geom_line() +
  labs(title='Average Close Price for Verizon Stock', x='', y='') +
  main_theme +
  scale_y_continuous(labels = dollar_format())

temp_list <- append(temp_list, list(temp_plot))

# Divorced/Widowed/Separated Graph
temp_plot <- ggplot(final_data_4, aes(x=month_end, y=divorced_widowed_separated_noTrend)) + 
  geom_line() +
  labs(title='Percent Divorced/Widowed/Separated \nin BRFSS', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

# Personal Savings Rate Graph
temp_plot <- ggplot(final_data_4, aes(x=month_end, y=personal_save_rate_noTrend)) + 
  geom_line() +
  labs(title='Personal Savings Rate', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

# Unemployment Rate Graph
temp_plot <- ggplot(final_data_4, aes(x=month_end, y=unemp_per_noTrend)) + 
  geom_line() +
  labs(title='Unemployment Rate', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

grid <- grid.arrange(grobs=temp_list, nrow=2,top='Common Variables - Detrended')
ggsave(here('Plots', 'common_variables_detrended.png'),plot=grid,dpi=600, width = 11.5, height = 9, units='in')

##### Age Variables

temp_list <- list()

# Gen Health No Good
temp_plot <- ggplot(final_data_4, aes(x=month_end, y=genhlth_no_good_18_24_noTrend)) + 
  geom_line() +
  labs(title='Age 24 and Under',subtitle='Percent General Health Poor' ,x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

# Ment Health No Good
temp_plot <- ggplot(final_data_4, aes(x=month_end, y=x24under_ment_hlth_sa_noTrend)) + 
  geom_line() +
  labs(title='', subtitle='Percent Mental Health Not Good', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

# Per 1000 Suicides Graph
temp_plot <- ggplot(final_data_4, aes(x=month_end, y=x24under_suicide_sa_noTrend)) + 
  geom_line() +
  labs(title='', subtitle='Suicides Per Thousand', x='', y='') +
  main_theme

temp_list <- append(temp_list, list(temp_plot))

# Gen Health No Good
temp_plot <- ggplot(final_data_4, aes(x=month_end, y=genhlth_no_good_25on_noTrend)) + 
  geom_line() +
  labs(title='Age 25 and Over',subtitle='Percent General Health Poor' ,x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

# Ment Health No Good
temp_plot <- ggplot(final_data_4, aes(x=month_end, y=x25over_ment_hlth_sa_noTrend)) + 
  geom_line() +
  labs(title='', subtitle='Percent Mental Health Not Good', x='', y='') +
  main_theme +
  scale_y_continuous(labels = percent)

temp_list <- append(temp_list, list(temp_plot))

# Per 1000 Suicides Graph
temp_plot <- ggplot(final_data_4, aes(x=month_end, y=x25over_suicide_sa_noTrend)) + 
  geom_line() +
  labs(title='', subtitle='Suicides Per Thousand', x='', y='') +
  main_theme

temp_list <- append(temp_list, list(temp_plot))

grid <- grid.arrange(grobs=temp_list, nrow=2,top='Age Variables - Detrended')
ggsave(here('Plots', 'age_variables_detrended.png'),plot=grid,dpi=600, width = 11.5, height = 9, units='in')

## Table Seasonality

x25over_suicide_coef <- coef(summary(x25over_suicide_lm))

x25over_suicide_coef_tibble <- bind_cols(var_name=rownames(x25over_suicide_coef), as_tibble(x25over_suicide_coef)) %>%
  clean_names() 

x25over_suicide_coef_pretty_table <- x25over_suicide_coef_tibble %>%
  filter(pr_t<=0.05) %>%
  rename('P-value' = pr_t,
         'Estimate'=estimate,
         'Standard Error'=std_error,
         'Test Statistic'=t_value,
         'Variable Name'=var_name) %>%
  dplyr::select('Variable Name',
                'Estimate',
                'Test Statistic',
                'Standard Error',
                'P-value') %>%
  gt() 

gt::gtsave(x25over_suicide_coef_pretty_table ,here('Plots', 'x25over_suicide_coef_coef.png'))

##  Table Stationarity Tests

alpha_1 <- 0.4

station_pretty <- stationarity_stats %>%
  left_join(all_names_cleaned, by=c('stationarity_vars'='all_names')) %>%
  dplyr::arrange(clean_name) %>%
  dplyr::select('Variable Name'=clean_name,
         'Dickey-Fuller Test Stat'=df_testStat,
         'Phillips-Perron Test Stat'=pp_testStat,
         'Augmented Dickey-Fuller Test Stat'=adfgls_testStat,
         'KPSS Test Stat'=kpss_testStat,
         df_result,
         pp_result,
         adfgls_result,
         kpss_result) %>%
  gt()  %>%
  tab_style(
    style = cell_fill(color = "lightgray", alpha=alpha_1),
    locations = cells_body(
      columns = 'Dickey-Fuller Test Stat',
      rows = df_result==0)
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray", alpha=alpha_1),
    locations = cells_body(
      columns = 'Phillips-Perron Test Stat',
      rows = pp_result==0)
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray", alpha=alpha_1),
    locations = cells_body(
      columns = 'Augmented Dickey-Fuller Test Stat',
      rows = adfgls_result==0)
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgray", alpha=alpha_1),
    locations = cells_body(
      columns = 'KPSS Test Stat',
      rows = kpss_result==0)
  ) %>%
  cols_hide(columns=c('df_result',
                      'pp_result',
                      'adfgls_result',
                      'kpss_result')) %>%
  cols_width(
    'Variable Name' ~ px(400)
  )

gt::gtsave(station_pretty ,here('Plots', 'stationarity_test_results.png'))

## Data Prep Graphic


