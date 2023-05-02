# 5_Taylor_Rule_Simulation_Monthly.R
# John Kearns
# Goal: Do-file for running the dynamic simulation
# Date Created: 2021-10-21
# Last Updated: 2021-10-21

# set directories
main_dir = "J:/Datasets12/Kearns/Steve/EME Mon Pol/"
data_folder = paste0(main_dir,"Data/")
do_folder = paste0(main_dir,"Do/")
results_folder = paste0(main_dir,"Results/")
charts_folder = paste0(main_dir,"Charts/")

# install and load packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(ggrepel)
library(Rcpp)
library(estimatr)
library(texreg)
library(GGally)
library(VGAM)
library(gridExtra)
source(paste0(do_folder,"Mode1.R"))

# bring in data
panel_df_monthly = read.csv(paste0(data_folder,"Final/panel_data_monthly.csv"))[,-1] %>%
  mutate(date=as.Date(date,format="%Y-%m-%d"))


# run test regression with out current specification
# first_round_taylor_regressions = data.frame()
# for(a in unique(panel_df_monthly$country)){
#
#   form = as.formula(paste0("rate~",Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),"+",Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a]),"+lag_rate+time_trend"))
#
#   data_filtered = panel_df_monthly %>%
#     filter(country==a&date>="2011-01-01"&date<="2019-12-01") %>%
#     drop_na(rate,lag_rate,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a]))
#
#   reg = lm_robust(form,data=data_filtered)
#
#   b = tidy(reg)
#
#   first_round_taylor_regressions = rbind(first_round_taylor_regressions,c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],b[5,2],b[5,4],reg$nobs,reg$r.squared,a,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a]),as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(first_round_taylor_regressions) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","time_trend_coef","time_trend_tstat","num_obs","r_squared","country","inflation_variable","output_gap_variable","start_date","end_date")
#
#   write.csv(first_round_taylor_regressions,paste0(results_folder,"taylor_results_2011_2019_time_trend.csv"))
# }
#
#
#
# # begin simulation
# # with time trend
# simulation_df = data.frame()
# plot_df = data.frame()
# for(a in c(panel_df_monthly %>% filter(!is.na(neer_12_month_change)&date>="2020-01-01") %>% distinct(country))$country){
#
#   form = as.formula(paste0("rate~",Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),"+",Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a]),"+lag_rate+time_trend"))
#
#   reg_df = panel_df_monthly %>%
#     filter(country==a&date>="2011-01-01"&date<="2019-12-01")
#
#   reg = lm_robust(form,data=reg_df)
#
#   b = tidy(reg)
#
#   constant = b[1,2]
#   inflation_coef = b[2,2]
#   output_coef = b[3,2]
#   lag_rate_coef = b[4,2]
#   time_trend_coef = b[5,2]
#
#   base_df = panel_df_monthly %>%
#     filter(country==a&date>="2020-01-01") %>%
#     select(date,country,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),output_gap,lag_rate,time_trend,inflation_target,time_trend) %>%
#     rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]))
#
#   base_df$lag_rate_sim = ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$rate_rule = NA
#   for(i in 1:nrow(base_df)){
#     base_df$rate_rule[i] = constant+(base_df$inflation[i]*inflation_coef)+(base_df$output_gap[i]*output_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)+(base_df$time_trend[i]*time_trend_coef)
#     if(i < nrow(base_df)){
#       base_df$lag_rate_sim[i+1]= base_df$rate_rule[i]
#     }
#   }
#
#   simulation_df = bind_rows(simulation_df,panel_df_monthly %>%
#                               filter(country==a&date=="2019-12-01") %>%
#                               select(date,country,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),output_gap,lag_rate,time_trend,inflation_target,time_trend,rate) %>%
#                               rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                      rate_rule=rate),
#                             base_df)
# }
#
# pdf(file=paste0(charts_folder,"predicted_rate_timetrend_constant.pdf"),width=15)
# for(a in c(panel_df_monthly %>% filter(!is.na(neer_12_month_change)&date>="2020-01-01") %>% distinct(country))$country){
#   print(
#
#   ggplot() +
#   geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01"),aes(x=date,y=rate,linetype="Actual",colour="Actual"),size=2) +
#   geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=rate_rule,linetype="Predicted",colour="Predicted"),size=2) +
#   labs(x="Month",y="Rate (%)",caption=a) +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#   )
# }
# dev.off()
#
#
# # without time trend
# simulation_df = data.frame()
# plot_df = data.frame()
# for(a in c(panel_df_monthly %>% filter(!is.na(neer_12_month_change)&date>="2020-01-01") %>% distinct(country))$country){
#
#   form = as.formula(paste0("rate~",Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),"+",Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a]),"+lag_rate+time_trend"))
#
#   reg_df = panel_df_monthly %>%
#     filter(country==a&date>="2011-01-01"&date<="2019-12-01")
#
#   reg = lm_robust(form,data=reg_df)
#
#   b = tidy(reg)
#
#   constant = b[1,2]
#   inflation_coef = b[2,2]
#   output_coef = b[3,2]
#   lag_rate_coef = b[4,2]
#   time_trend_coef = b[5,2]
#
#   base_df = panel_df_monthly %>%
#     filter(country==a&date>="2020-01-01") %>%
#     select(date,country,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),output_gap,lag_rate,time_trend,inflation_target,time_trend) %>%
#     rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]))
#
#   base_df$lag_rate_sim = ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$rate_rule = NA
#   for(i in 1:nrow(base_df)){
#     base_df$rate_rule[i] = constant+(base_df$inflation[i]*inflation_coef)+(base_df$output_gap[i]*output_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)
#     if(i < nrow(base_df)){
#       base_df$lag_rate_sim[i+1]= base_df$rate_rule[i]
#     }
#   }
#
#   simulation_df = bind_rows(simulation_df,panel_df_monthly %>%
#                               filter(country==a&date=="2019-12-01") %>%
#                               select(date,country,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),output_gap,lag_rate,time_trend,inflation_target,time_trend,rate) %>%
#                               rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                      rate_rule=rate),
#                             base_df)
# }
#
# pdf(file=paste0(charts_folder,"predicted_rate_constant_notimetrend.pdf"),width=15)
# for(a in c(panel_df_monthly %>% filter(!is.na(neer_12_month_change)&date>="2020-01-01") %>% distinct(country))$country){
#   print(
#
#     ggplot() +
#       geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01"),aes(x=date,y=rate,linetype="Actual",colour="Actual"),size=2) +
#       geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=rate_rule,linetype="Predicted",colour="Predicted"),size=2) +
#       labs(x="Month",y="Rate (%)",caption=a) +
#       theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#   )
# }
# dev.off()
#
#
# # without time trend or constant
# simulation_df = data.frame()
# plot_df = data.frame()
# for(a in c(panel_df_monthly %>% filter(!is.na(neer_12_month_change)&date>="2020-01-01") %>% distinct(country))$country){
#
#   form = as.formula(paste0("rate~",Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),"+",Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a]),"+lag_rate+time_trend"))
#
#   reg_df = panel_df_monthly %>%
#     filter(country==a&date>="2011-01-01"&date<="2019-12-01")
#
#   reg = lm_robust(form,data=reg_df)
#
#   b = tidy(reg)
#
#   constant = b[1,2]
#   inflation_coef = b[2,2]
#   output_coef = b[3,2]
#   lag_rate_coef = b[4,2]
#   time_trend_coef = b[5,2]
#
#   base_df = panel_df_monthly %>%
#     filter(country==a&date>="2020-01-01") %>%
#     select(date,country,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),output_gap,lag_rate,time_trend,inflation_target,time_trend) %>%
#     rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]))
#
#   base_df$lag_rate_sim = ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$rate_rule = NA
#   for(i in 1:nrow(base_df)){
#     base_df$rate_rule[i] = (base_df$inflation[i]*inflation_coef)+(base_df$output_gap[i]*output_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)+(base_df$time_trend[i]*time_trend_coef)
#     if(i < nrow(base_df)){
#       base_df$lag_rate_sim[i+1]= base_df$rate_rule[i]
#     }
#   }
#
#   simulation_df = bind_rows(simulation_df,panel_df_monthly %>%
#                               filter(country==a&date=="2019-12-01") %>%
#                               select(date,country,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),output_gap,lag_rate,time_trend,inflation_target,time_trend,rate) %>%
#                               rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                      rate_rule=rate),
#                             base_df)
# }
#
# pdf(file=paste0(charts_folder,"predicted_rate_notimetrend_noconstant.pdf"),width=15)
# for(a in c(panel_df_monthly %>% filter(!is.na(neer_12_month_change)&date>="2020-01-01") %>% distinct(country))$country){
#   print(
#
#     ggplot() +
#       geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01"),aes(x=date,y=rate,linetype="Actual",colour="Actual"),size=2) +
#       geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=rate_rule,linetype="Predicted",colour="Predicted"),size=2) +
#       labs(x="Month",y="Rate (%)",caption=a) +
#       theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#   )
# }
# dev.off()
#
#
# # with time trend, setting negative gdp coeff to 0, and taylor rule in differences
# simulation_df = data.frame()
# plot_df = data.frame()
# for(a in c(panel_df_monthly %>% filter(!is.na(neer_12_month_change)&date>="2020-01-01") %>% distinct(country))$country){
#
#   form = as.formula(paste0("rate~",Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),"+",Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a]),"+lag_rate+time_trend"))
#
#   reg_df = panel_df_monthly %>%
#     filter(country==a&date>="2011-01-01"&date<="2019-12-01")
#
#   reg = lm_robust(form,data=reg_df)
#
#   b = tidy(reg)
#
#   constant = b[1,2]
#   inflation_coef = b[2,2]
#   output_coef = b[3,2]
#   output_coef = ifelse(output_coef<0,0,output_coef)
#   lag_rate_coef = b[4,2]
#   time_trend_coef = b[5,2]
#
#   base_df = panel_df_monthly %>%
#     filter(country==a) %>%
#     rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a])) %>%
#     mutate(change_inflation=inflation-dplyr::lag(inflation,1),
#            change_output_gap = output_gap-dplyr::lag(output_gap,1)) %>%
#     filter(date>="2020-01-01") %>%
#     select(date,country,inflation,output_gap,lag_rate,time_trend,inflation_target,time_trend,change_inflation,change_output_gap)
#
#
#   base_df$lag_rate_sim = ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$lag_rate_taylor_rule =  ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$rate_rule = NA
#   base_df$taylor_rate_rule = NA
#   for(i in 1:nrow(base_df)){
#     base_df$rate_rule[i] = constant+(base_df$inflation[i]*inflation_coef)+(base_df$output_gap[i]*output_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)+(base_df$time_trend[i]*time_trend_coef)
#     base_df$taylor_rate_rule[i] = base_df$lag_rate_taylor_rule[i]+(1.5*base_df$change_inflation[i])+(.5*base_df$change_output_gap[i])
#     if(i < nrow(base_df)){
#       base_df$lag_rate_sim[i+1]= base_df$rate_rule[i]
#       base_df$lag_rate_taylor_rule[i+1] = base_df$taylor_rate_rule[i]
#     }
#   }
#
#   simulation_df = bind_rows(simulation_df,panel_df_monthly %>%
#                               filter(country==a&date=="2019-12-01") %>%
#                               select(date,country,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),output_gap,lag_rate,time_trend,inflation_target,time_trend,rate) %>%
#                               rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                      rate_rule=rate) %>%
#                               mutate(taylor_rate_rule=rate_rule),
#                             base_df)
# }
#
# pdf(file=paste0(charts_folder,"predicted_rate_timetrend_constant_zerogdp_monthly.pdf"),width=15)
# for(a in c(panel_df_monthly %>% filter(!is.na(neer_12_month_change)&date>="2020-01-01") %>% distinct(country))$country){
#   print(
#
#     ggplot() +
#       geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01"),aes(x=date,y=rate,linetype="Actual",colour="Actual"),size=2) +
#       geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=rate_rule,linetype="Predicted",colour="Predicted"),size=2) +
#       geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=taylor_rate_rule,linetype="Taylor Rule",colour="Taylor Rule"),size=2) +
#       labs(x="Month",y="Rate (%)",caption=a) +
#       theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#   )
# }
# dev.off()
#
#
# # with time trend, setting negative gdp coeff to 0, and taylor rule in differences, 1970 onwards
# simulation_df = data.frame()
# plot_df = data.frame()
# results_df = data.frame()
# for(a in c(panel_df_monthly %>% filter(!is.na(neer_12_month_change)&date>="2020-01-01") %>% distinct(country))$country){
#
#   form = as.formula(paste0("rate~",Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),"+",Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a]),"+lag_rate+time_trend"))
#
#   reg_df = panel_df_monthly %>%
#     filter(country==a&date>="1970-01-01"&date<="2019-12-01")
#
#   reg = lm_robust(form,data=reg_df)
#
#   b = tidy(reg)
#
#   results_df = bind_rows(results_df,b %>% mutate(country=a))
#
#   constant = b[1,2]
#   inflation_coef = b[2,2]
#   output_coef = b[3,2]
#   output_coef = ifelse(output_coef<0,0,output_coef)
#   lag_rate_coef = b[4,2]
#   time_trend_coef = b[5,2]
#
#   base_df = panel_df_monthly %>%
#     filter(country==a) %>%
#     rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a])) %>%
#     mutate(change_inflation=inflation-dplyr::lag(inflation,1),
#            change_output_gap = output_gap-dplyr::lag(output_gap,1)) %>%
#     filter(date>="2020-01-01") %>%
#     select(date,country,inflation,output_gap,lag_rate,time_trend,inflation_target,time_trend,change_inflation,change_output_gap)
#
#
#   base_df$lag_rate_sim = ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$lag_rate_taylor_rule =  ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$rate_rule = NA
#   base_df$taylor_rate_rule = NA
#   for(i in 1:nrow(base_df)){
#     base_df$rate_rule[i] = constant+(base_df$inflation[i]*inflation_coef)+(base_df$output_gap[i]*output_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)+(base_df$time_trend[i]*time_trend_coef)
#     base_df$taylor_rate_rule[i] = base_df$lag_rate_taylor_rule[i]+(1.5*base_df$change_inflation[i])+(.5*base_df$change_output_gap[i])
#     if(i < nrow(base_df)){
#       base_df$lag_rate_sim[i+1]= base_df$rate_rule[i]
#       base_df$lag_rate_taylor_rule[i+1] = base_df$taylor_rate_rule[i]
#     }
#   }
#
#   simulation_df = bind_rows(simulation_df,panel_df_monthly %>%
#                               filter(country==a&date=="2019-12-01") %>%
#                               select(date,country,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),output_gap,lag_rate,time_trend,inflation_target,time_trend,rate) %>%
#                               rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                      rate_rule=rate) %>%
#                               mutate(taylor_rate_rule=rate_rule),
#                             base_df)
# }
#
# pdf(file=paste0(charts_folder,"predicted_rate_timetrend_constant_zeroip_1970_monthly.pdf"),width=15)
# for(a in c(panel_df_monthly %>% filter(!is.na(neer_12_month_change)&date>="2020-01-01") %>% distinct(country))$country){
#
#     plot1 = ggplot() +
#       geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01"),aes(x=date,y=rate,linetype="Actual",colour="Actual"),size=2) +
#       geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=rate_rule,linetype="Predicted",colour="Predicted"),size=2) +
#       geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=taylor_rate_rule,linetype="Taylor Rule",colour="Taylor Rule"),size=2) +
#       labs(x="Month",y="Rate (%)",caption=a) +
#       theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#     plot2 = ggplot() +
#       geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01") %>% rename(inflation=Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                                                                            output_gap=Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a])) %>% mutate(inflation=inflation*3),aes(x=date,y=inflation,colour="Inflation"),size=2) +
#       geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01") %>% rename(inflation=Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                                                                            output_gap_var=Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a])),aes(x=date,y=output_gap_var,colour="Output gap"),size=2) +
#       labs(x="Month",y="Output gap (%)",caption=a) +
#       geom_hline(yintercept=0) +
#       scale_y_continuous(sec.axis = sec_axis(~./3,name="Inflation (%)")) +
#       scale_colour_manual(labels=c("Inflation","Output gap"),values=c("purple","gold")) +
#       theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#     print(grid.arrange(plot1,plot2))
#
#
# }
# dev.off()
#
# # weighted monthly 1970 onwards regression
# simulation_df = data.frame()
# plot_df = data.frame()
# results_df = data.frame()
# for(a in c(panel_df_monthly %>% distinct(country))$country){
#
#   if(a%in%c("China","New Zealand","Iceland","Russia")){next}
#
#   form = as.formula(paste0("rate~",Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),"+",Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a]),"+lag_rate+time_trend"))
#
#   reg_df = panel_df_monthly %>%
#     filter(country==a&date>="1970-01-01"&date<="2019-12-01") %>%
#     group_by(country) %>%
#     mutate(time_weight=1:n()) %>%
#     ungroup()
#
#   reg = lm_robust(form,data=reg_df,weights=time_weight)
#
#   b = tidy(reg)
#
#   results_df = bind_rows(results_df,b %>% mutate(country=a))
#
#   constant = b[1,2]
#   inflation_coef = b[2,2]
#   output_coef = b[3,2]
#   output_coef = ifelse(output_coef<0,0,output_coef)
#   lag_rate_coef = b[4,2]
#   time_trend_coef = b[5,2]
#
#   base_df = panel_df_monthly %>%
#     filter(country==a) %>%
#     rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a])) %>%
#     mutate(change_inflation=inflation-dplyr::lag(inflation,1),
#            change_output_gap = output_gap-dplyr::lag(output_gap,1)) %>%
#     filter(date>="2020-01-01") %>%
#     select(date,country,inflation,output_gap,lag_rate,time_trend,inflation_target,time_trend,change_inflation,change_output_gap)
#
#
#   base_df$lag_rate_sim = ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$lag_rate_taylor_rule =  ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$rate_rule = NA
#   base_df$taylor_rate_rule = NA
#   for(i in 1:nrow(base_df)){
#     base_df$rate_rule[i] = constant+(base_df$inflation[i]*inflation_coef)+(base_df$output_gap[i]*output_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)+(base_df$time_trend[i]*time_trend_coef)
#     base_df$taylor_rate_rule[i] = base_df$lag_rate_taylor_rule[i]+(1.5*base_df$change_inflation[i])+(.5*base_df$change_output_gap[i])
#     if(i < nrow(base_df)){
#       base_df$lag_rate_sim[i+1]= base_df$rate_rule[i]
#       base_df$lag_rate_taylor_rule[i+1] = base_df$taylor_rate_rule[i]
#     }
#   }
#
#   simulation_df = bind_rows(simulation_df,panel_df_monthly %>%
#                               filter(country==a&date=="2019-12-01") %>%
#                               select(date,country,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),output_gap,lag_rate,time_trend,inflation_target,time_trend,rate) %>%
#                               rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                      rate_rule=rate) %>%
#                               mutate(taylor_rate_rule=rate_rule),
#                             base_df)
# }
#
# pdf(file=paste0(charts_folder,"predicted_rate_timetrend_constant_zeroip_1970_monthly_weighted.pdf"),width=15)
# for(a in c(panel_df_monthly %>% arrange(country) %>% distinct(country))$country){
#
#   if(a%in%c("China","New Zealand","Iceland","Russia")){next}
#
#   plot1 = ggplot() +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01"),aes(x=date,y=rate,linetype="Actual",colour="Actual"),size=2) +
#     geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=rate_rule,linetype="Predicted",colour="Predicted"),size=2) +
#     geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=taylor_rate_rule,linetype="Taylor Rule",colour="Taylor Rule"),size=2) +
#     labs(x="Month",y="Rate (%)",caption=a) +
#     theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#   plot2 = ggplot() +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01") %>% rename(inflation=Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                                                                          output_gap=Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a])) %>% mutate(inflation=inflation*3),aes(x=date,y=inflation,colour="Inflation"),size=2) +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01") %>% rename(inflation=Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                                                                          output_gap_var=Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a])),aes(x=date,y=output_gap_var,colour="Output gap"),size=2) +
#     labs(x="Month",y="Output gap (%)",caption=a) +
#     geom_hline(yintercept=0) +
#     scale_y_continuous(sec.axis = sec_axis(~./3,name="Inflation (%)")) +
#     scale_colour_manual(labels=c("Inflation","Output gap"),values=c("purple","gold")) +
#     theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#   print(grid.arrange(plot1,plot2))
#
#
# }
# dev.off()
#
#
# testweight = reg_df %>%
#   mutate(time_weight=1:n(),
#          weight1 = 1 - (max(time_weight)-time_weight)/max(time_weight),
#          weight2 = 1/(.5*exp(-(2*weight1)-1)))
#
#
# # weighted exponential monthly 1970 onwards regression
# simulation_df = data.frame()
# plot_df = data.frame()
# results_df = data.frame()
# for(a in c(panel_df_monthly %>% distinct(country))$country){
#
#   if(a%in%c("China","New Zealand","Iceland","Russia")){next}
#
#   form = as.formula(paste0("rate~",Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),"+",Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a]),"+lag_rate+time_trend"))
#
#   reg_df = panel_df_monthly %>%
#     filter(country==a&date>="1970-01-01"&date<="2019-12-01") %>%
#     group_by(country) %>%
#     mutate(time_weight=1:n(),
#            weight1 = 1 - (max(time_weight)-time_weight)/max(time_weight),
#            weight2 = 1/(.5*exp(-(2*weight1)-1))) %>%
#     ungroup()
#
#   reg = lm_robust(form,data=reg_df,weights=time_weight^2)
#
#   b = tidy(reg)
#
#   results_df = bind_rows(results_df,b %>% mutate(country=a))
#
#   constant = b[1,2]
#   inflation_coef = b[2,2]
#   output_coef = b[3,2]
#   output_coef = ifelse(output_coef<0,0,output_coef)
#   lag_rate_coef = b[4,2]
#   time_trend_coef = b[5,2]
#
#   base_df = panel_df_monthly %>%
#     filter(country==a) %>%
#     rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a])) %>%
#     mutate(change_inflation=inflation-dplyr::lag(inflation,1),
#            change_output_gap = output_gap-dplyr::lag(output_gap,1)) %>%
#     filter(date>="2020-01-01") %>%
#     select(date,country,inflation,output_gap,lag_rate,time_trend,inflation_target,time_trend,change_inflation,change_output_gap)
#
#
#   base_df$lag_rate_sim = ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$lag_rate_taylor_rule =  ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$rate_rule = NA
#   base_df$taylor_rate_rule = NA
#   for(i in 1:nrow(base_df)){
#     base_df$rate_rule[i] = constant+(base_df$inflation[i]*inflation_coef)+(base_df$output_gap[i]*output_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)+(base_df$time_trend[i]*time_trend_coef)
#     base_df$taylor_rate_rule[i] = base_df$lag_rate_taylor_rule[i]+(1.5*base_df$change_inflation[i])+(.5*base_df$change_output_gap[i])
#     if(i < nrow(base_df)){
#       base_df$lag_rate_sim[i+1]= base_df$rate_rule[i]
#       base_df$lag_rate_taylor_rule[i+1] = base_df$taylor_rate_rule[i]
#     }
#   }
#
#   simulation_df = bind_rows(simulation_df,panel_df_monthly %>%
#                               filter(country==a&date=="2019-12-01") %>%
#                               select(date,country,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),output_gap,lag_rate,time_trend,inflation_target,time_trend,rate) %>%
#                               rename(inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                      rate_rule=rate) %>%
#                               mutate(taylor_rate_rule=rate_rule),
#                             base_df)
# }
#
# pdf(file=paste0(charts_folder,"predicted_rate_timetrend_constant_zeroip_1970_monthly_exp_weighted.pdf"),width=15)
# for(a in c(panel_df_monthly %>% arrange(country) %>% distinct(country))$country){
#
#   if(a%in%c("China","New Zealand","Iceland","Russia")){next}
#
#   plot1 = ggplot() +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01"),aes(x=date,y=rate,linetype="Actual",colour="Actual"),size=2) +
#     geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=rate_rule,linetype="Predicted",colour="Predicted"),size=2) +
#     geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=taylor_rate_rule,linetype="Taylor Rule",colour="Taylor Rule"),size=2) +
#     labs(x="Month",y="Rate (%)",caption=a) +
#     theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#   plot2 = ggplot() +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01") %>% rename(inflation=Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                                                                          output_gap=Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a])) %>% mutate(inflation=inflation*3),aes(x=date,y=inflation,colour="Inflation"),size=2) +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01") %>% rename(inflation=Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                                                                          output_gap_var=Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a])),aes(x=date,y=output_gap_var,colour="Output gap"),size=2) +
#     labs(x="Month",y="Output gap (%)",caption=a) +
#     geom_hline(yintercept=0) +
#     scale_y_continuous(sec.axis = sec_axis(~./3,name="Inflation (%)")) +
#     scale_colour_manual(labels=c("Inflation","Output gap"),values=c("purple","gold")) +
#     theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#   print(grid.arrange(plot1,plot2))
#
#
# }
# dev.off()

# run the simulation in differences
panel_df_monthly = panel_df_monthly %>%
  group_by(country) %>%
  mutate(diff_rate=rate-dplyr::lag(rate,1),
         lag_diff_rate=dplyr::lag(diff_rate,1),
         diff_core_inflation=core_inflation-dplyr::lag(core_inflation,1),
         diff_output_gap=output_gap-dplyr::lag(output_gap,1),
         diff_total_inflation = CPI-dplyr::lag(CPI,1),
         diff_inflation_variable=ifelse(inflation_variable=="CPI","diff_total_inflation","diff_core_inflation"),
         diff_output_variable = "diff_output_gap") %>%
  ungroup()

# simulation_df = data.frame()
# plot_df = data.frame()
# results_df = data.frame()
# for(a in c(panel_df_monthly %>% filter(!is.na(neer_12_month_change)&date>="2020-01-01") %>% distinct(country))$country){
#
#   form = as.formula(paste0("diff_rate~",Mode1(panel_df_monthly$diff_inflation_variable[panel_df_monthly$country==a]),"+",Mode1(panel_df_monthly$diff_output_variable[panel_df_monthly$country==a]),"+lag_diff_rate"))
#
#   reg_df = panel_df_monthly %>%
#     filter(country==a&date>="1970-01-01"&date<="2019-12-01")
#
#   reg = lm_robust(form,data=reg_df)
#
#   b = tidy(reg)
#
#   results_df = bind_rows(results_df,b %>% mutate(country=a))
#
#   constant = b[1,2]
#   inflation_coef = b[2,2]
#   output_coef = b[3,2]
#   output_coef = ifelse(output_coef<0,0,output_coef)
#   lag_rate_coef = b[4,2]
#
#   base_df = panel_df_monthly %>%
#     filter(country==a) %>%
#     rename(diff_inflation = Mode1(panel_df_monthly$diff_inflation_variable[panel_df_monthly$country==a]),
#            inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a])) %>%
#     mutate(change_inflation=inflation-dplyr::lag(inflation,1),
#            change_output_gap = output_gap-dplyr::lag(output_gap,1)) %>%
#     filter(date>="2020-01-01") %>%
#     select(date,country,inflation,output_gap,lag_rate,inflation_target,change_inflation,change_output_gap,diff_inflation,diff_inflation_variable,diff_output_variable,lag_diff_rate,diff_rate,diff_output_gap)
#
#
#   base_df$lag_rate_sim = ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$lag_diff_rate_sim = ifelse(base_df$date=="2020-01-01",base_df$lag_diff_rate[1],NA)
#   base_df$lag_rate_taylor_rule =  ifelse(base_df$date=="2020-01-01",base_df$lag_rate[1],NA)
#   base_df$rate_rule = NA
#   base_df$taylor_rate_rule = NA
#   for(i in 1:nrow(base_df)){
#     base_df$rate_rule[i] = base_df$lag_rate_sim[i]+constant+(inflation_coef*base_df$diff_inflation[i])+(output_coef*base_df$diff_output_gap[i])+(lag_rate_coef*base_df$lag_diff_rate_sim[i])
#     base_df$taylor_rate_rule[i] = base_df$lag_rate_taylor_rule[i]+(1.5*base_df$change_inflation[i])+(.5*base_df$change_output_gap[i])
#     if(i < nrow(base_df)){
#       base_df$lag_rate_sim[i+1]= base_df$rate_rule[i]
#       base_df$lag_diff_rate_sim[i+1] = base_df$rate_rule[i]-base_df$lag_rate_sim[i]
#       base_df$lag_rate_taylor_rule[i+1] = base_df$taylor_rate_rule[i]
#     }
#   }
#
#   simulation_df = bind_rows(simulation_df,panel_df_monthly %>%
#                               filter(country==a&date=="2019-12-01") %>%
#                               select(date,country,Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),output_gap,lag_rate,time_trend,inflation_target,rate) %>%
#                               rename(diff_inflation = Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                      rate_rule=rate) %>%
#                               mutate(taylor_rate_rule=rate_rule),
#                             base_df)
# }
#
# pdf(file=paste0(charts_folder,"predicted_rate_diff_monthly_5panel.pdf"),width=25,height=25)
# for(a in c(panel_df_monthly %>% arrange(country) %>% distinct(country))$country){
#
#   if(a%in%c("China","New Zealand","Iceland","Serbia")){next}
#
#   plot1 = ggplot() +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01"),aes(x=date,y=rate,linetype="Actual",colour="Actual"),size=2) +
#     geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=rate_rule,linetype="Predicted",colour="Predicted"),size=2) +
#     geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=taylor_rate_rule,linetype="Taylor Rule",colour="Taylor Rule"),size=2) +
#     labs(x="Month",y="Rate (%)",caption=a) +
#     theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#   plot2 = ggplot() +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01") %>% rename(inflation=Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                                                                          output_gap=Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a])) %>% mutate(inflation=inflation*3),aes(x=date,y=inflation,colour="Inflation"),size=2) +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01") %>% rename(inflation=Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                                                                          output_gap_var=Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a])),aes(x=date,y=output_gap_var,colour="Output gap"),size=2) +
#     labs(x="Month",y="Output gap (%)",caption=a) +
#     geom_hline(yintercept=0) +
#     scale_y_continuous(sec.axis = sec_axis(~./3,name="Inflation (%)")) +
#     scale_colour_manual(labels=c("Inflation","Output gap"),values=c("purple","gold")) +
#     theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#
#   plot3 = ggplot() +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2019-01-01"),aes(x=date,y=rate,linetype="Actual",colour="Actual"),size=2) +
#     geom_line(data=simulation_df %>% filter(country==a&date>="2019-12-01"),aes(x=date,y=rate_rule,linetype="Predicted",colour="Predicted"),size=2) +
#     labs(x="Month",y="Rate (%)",caption=a) +
#     scale_color_manual(labels=c("Actual","Predicted"),values=c("#F8766D","#00BA38")) +
#     theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
#           legend.position="none")
#
#   plot4 = ggplot() +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2014-01-01") %>% rename(inflation=Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                                                                          output_gap=Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a])),aes(x=date,y=inflation,colour="Inflation"),size=2) +
#     labs(x="Month",y="Inflation (%)",caption=a) +
#     geom_hline(yintercept=0) +
#     scale_colour_manual(labels=c("Inflation"),values=c("purple")) +
#     theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
#           legend.position="none")
#
#   plot5 = ggplot() +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2014-01-01") %>% rename(inflation=Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                                                                          output_gap=Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a])) %>% mutate(inflation=inflation*3),aes(x=date,y=ip_index,colour="Actual IP"),size=2) +
#     geom_line(data=panel_df_monthly %>% filter(country==a&date>="2014-01-01") %>% rename(inflation=Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),
#                                                                                          output_gap_var=Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a])),aes(x=date,y=ip_trend,colour="Trend IP"),size=2) +
#     labs(x="Month",y="Industrial Production (Jan 2020 = 100)",caption=a) +
#     scale_colour_manual(labels=c("Actual IP","Trend IP"),values=c("blue","red")) +
#     theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#
#   print(grid.arrange(plot1,plot2,plot3,plot4,plot5),nrow=3)
#
#
# }
# dev.off()
