library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(gridExtra)
library(scales)
library(dynlm)
library(fastDummies)

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

x24under_sa <- lm(x0_24_suicides_per_thous ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data=ts_final_data)
summary(x24under_sa)

x24under_sa <- lm(x0_24_suicides_per_thous ~ Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data=ts_final_data)
summary(x24under_sa)

x24under_sa <- lm(x0_24_suicides_per_thous ~ Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data=ts_final_data)
summary(x24under_sa)

x24under_sa <- lm(x0_24_suicides_per_thous ~ Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data=ts_final_data)
summary(x24under_sa)

vehicle_sa <- decompose(mydata[,"TOTALNSA"]) ### Decompose monthly oil into various components
vehicle_sa <- vehicle_sa$x - vehicle_sa$seasonal  ## Subtract seasonal and trend








############################ ADF Tests For Levels ##############################

adfteststat_df <-ur.df(Data[,"WTISPLC"], type=c("trend"), selectlags="BIC") #summary(adfteststat)
adfteststat_pp <-ur.pp(Data[,"WTISPLC"], type=("Z-tau"), model=c("trend"))
adfteststat_adfgls <-ur.ers(Data[,"WTISPLC"], type="DF-GLS", model="trend")
adfteststat_kpss <-ur.kpss(Data[,"WTISPLC"], type=c("tau"))

stats <- cbind(adfteststat_df@teststat[1],adfteststat_pp@teststat[1],adfteststat_adfgls@teststat[1], adfteststat_kpss@teststat[1])
cv <- cbind(adfteststat_df@cval[2],adfteststat_pp@cval[2], adfteststat_adfgls@cval[2], adfteststat_kpss@cval[2])
stats <- rbind(stats,cv)
rownames(stats) <- c("Test Statistic", "Critical Value")
colnames(stats) <- c("ADF", "PP", "ADF-GLS", "KPSS")

oil.fd <- diff(Data[,2]) # First Difference
oil.hp <- mFilter(Data[,2],filter="HP")  # Hodrick-Prescott filter
oil.bk <- mFilter(Data[,2],filter="BK")  # Baxter-King filter
oil.cf <- mFilter(Data[,2],filter="CF")  # Christiano-Fitzgerald filter


gdp.fd <- diff(Data[,3]) # First Difference
gdp.hp <- mFilter(Data[,3],filter="HP")  # Hodrick-Prescott filter
gdp.bk <- mFilter(Data[,3],filter="BK")  # Baxter-King filter
gdp.cf <- mFilter(Data[,3],filter="CF")  # Christiano-Fitzgerald filter

plot(oil.fd, main="First Difference of Crude Oil Prices", ylab="", xlab="Years")
plot(gdp.fd, main="First Difference of Real GDP", ylab="", xlab="Years")

plot(oil.hp$cycle, main="HP-Filtered Crude Oil Prices", ylab="", xlab="Years")
plot(gdp.hp$cycle, main="HP-Filtered Real GDP", ylab="", xlab="Years")

plot(oil.bk$cycle, main="BK-Filtered Crude Oil Prices", ylab="", xlab="Years")
plot(gdp.bk$cycle, main="BK-Filtered Real GDP", ylab="", xlab="Years")

plot(oil.cf$cycle, main="CF-Filtered Crude Oil Prices", ylab="", xlab="Years")
plot(gdp.cf$cycle, main="CF-Filtered Real GDP", ylab="", xlab="Years")

### Finite Distributed Lag Models
FDL <- dynlm(UNRATE ~ L(oil, 0:4) + L(FEDFUNDS, 0:4), data = Data)
summary(FDL)

### Autoregressive Distributed Lag Models
ARDL <- dynlm(UNRATE ~ L(UNRATE, 1:4)  + L(oil, 0:4) + L(FEDFUNDS, 0:4), data = Data)
summary(model_1)

x24_under <- final_data_2 %>%
  select(ment_health_no_good_18_24,
         genhlth_no_good_18_24,
         divorced_widowed_separated,
         avg_close_apple_stock,
         avg_close_atandt_stock,
         avg_close_verizon_stock,
         unemp_per,
         personal_save_rate,
         x0_24_suicides_per_thous) %>%
  ts()

fit <- VAR(x24_under, lag.max = 12, ic = "AIC")
summary(fit)

x25_over <- final_data_2 %>%
  select(ment_health_no_good_25on,
         genhlth_no_good_25on,
         divorced_widowed_separated,
         avg_close_apple_stock,
         avg_close_atandt_stock,
         avg_close_verizon_stock,
         unemp_per,
         personal_save_rate,
         x25on_suicides_per_thous) %>%
  ts()

fit <- VAR(x25_over, lag.max = 12, ic = "AIC")
summary(fit)

res_output_y <- irf(fit,impulse=c("y"), response=c("y"), n.ahead=12, cumulative = TRUE,runs=1000, ci=0.95)
res_output_w <- irf(fit,impulse=c("w"), response="y", n.ahead=12, cumulative = TRUE,runs=100, ci=0.95)
res_oil_w <- irf(fit,impulse=c("w"), response=c("w"), n.ahead=12, cumulative = TRUE,runs=1000, ci=0.95)
res_oil_y <- irf(fit,impulse=c("y"), response="w", n.ahead=12, cumulative = TRUE,runs=100, ci=0.95)
