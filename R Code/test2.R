x25_over <- final_data_4 %>%
  dplyr::select(-c(genhlth_no_good_18_24_noTrend,
                   x25over_suicide_sa_noTrend,
                   x25over_ment_hlth_sa_noTrend,
                   x24under_suicide_sa_noTrend)) %>%
  mutate(x24under_suicide_sa_noTrend=final_data_4$x24under_suicide_sa_noTrend)

x25_over_ts <- ts(x25_over)

x25_over_var <- VAR(x25_over_ts, lag.max = 12, ic = 'AIC')

aes(y=lower), color='red', alpha=0.75, linetype = "dashed") + 
  geom_line(aes(y=temp_var$irf[[1]])) + 
  geom_line(aes(y=temp_var$Upper[[1]]), color='red', alpha=0.75, linetype = "dashed") + 
  geom_hline(yintercept=0) +
  scale_x_continuous(breaks=c(2,4,6,8,10,12)) +
  scale_y_continuous(labels = percent) +
  main_theme +
  labs(x='Month', 
       y='Percent', 
       title=paste('Response of 24 and Under Suicides \nto a ',clean_name,' Shock',sep=''))