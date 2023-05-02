# 6_WEO_Scatterplots.R
# John Kearns and Bea Lee 
# Goal: Do-file for running the dynamic simulation
# Date Created: 2021-11-03
# Last Updated: 2022-04-11 \\ bal

# set directories
main_dir = "J:/Datasets12/Lee/Steve/EME Mon Pol/"
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
library(openxlsx)
library(lfe)
library(ggpmisc)
source(paste0(do_folder,"Mode1.R"))

# bring in data
panel_df_monthly = read.csv(paste0(data_folder,"Final/panel_data_monthly_20230411.csv"))[,-1] %>%
  mutate(date=as.Date(date,format="%Y-%m-%d"))

panel_df = read.csv(paste0(data_folder,"Final/quarterly_panel_df_20230411.csv"))[,-1] %>%
  mutate(date=as.Date(date,format="%Y-%m-%d"))

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

panel_df = panel_df %>%
  group_by(country) %>%
  mutate(diff_rate=rate-dplyr::lag(rate,1),
         lag_diff_rate=dplyr::lag(diff_rate,1),
         diff_core_inflation=core_inflation-dplyr::lag(core_inflation,1),
         diff_output_gap=gdp_output_gap-dplyr::lag(gdp_output_gap,1),
         diff_total_inflation = CPI-dplyr::lag(CPI,1),
         diff_inflation_variable=ifelse(inflation_variable=="CPI","diff_total_inflation","diff_core_inflation"),
         diff_output_variable = "diff_output_gap") %>%
  ungroup()

# bring in that sweet weo data
# idea is that the difference in the forecasts for year-ahead inflation is equal to the inflation shock caused by the pandemic
weo_oct2020 = readxl::read_excel(paste0(data_folder,"Raw/weo_oct2020.xlsx"))[1:21,] %>%
  select(ISO,Country,'2021') %>%
  rename(forcast2021='2021') %>%
  mutate(Country=ifelse(Country=="Korea","South Korea",Country))

weo_april2022 = readxl::read_excel(paste0(data_folder,"Raw/weo_april2022.xlsx"))[1:21,] %>%
  select(ISO,Country,'2022') %>%
  rename(forcast2022='2022') %>%
  mutate(Country=ifelse(Country=="Korea","South Korea",Country))

weo_inflation = left_join(weo_oct2020,weo_april2022,by=c("ISO","Country")) %>%
  mutate(weo_change_inflation=forcast2022-forcast2021) %>%
  select(-c(forcast2021,forcast2022,ISO)) %>%
  ungroup()

# join to panel
panel_df_monthly = left_join(panel_df_monthly,weo_inflation,by=c("country"="Country"))

panel_df = left_join(panel_df,weo_inflation,by=c("country"="Country"))

# weo scatterplots
df_scatter_monthly = panel_df_monthly %>%
  filter(date>="2019-12-01") %>%
  group_by(country) %>%
  summarise(change_rate_oct_2020_apr_2022=rate[date=="2022-04-01"]-rate[date=="2020-10-01"],
            weo_change_inflation=weo_change_inflation[1],
            advanced=advanced[1]) %>%
  mutate(advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America","Non-LA"),
         advanced=ifelse(country%in%c("China", "Czech Republic", "Hungary", "India", "Indonesia","Malaysia","Phillippines","Poland",
                                      "Romania","Russia","Serbia","South Africa","South Korea","Thailand","Turkey"),"Non-LA EMEs", advanced)) %>%
  #mutate(advanced=ifelse(country=="Non-LA","Advanced Economies",advanced)) %>%
  ungroup()

df_scatter_monthly$advanced = factor(df_scatter_monthly$advanced,levels=c("Non-LA EMEs","Advanced Economies","Latin America"))

# This makes Figure 7 (exclude russia and turkey bc they are major outliers)
fig7 <- ggplot(df_scatter_monthly %>% filter((advanced=="Non-LA EMEs"|advanced=="Latin America")&country!="Turkey"&country!="Russia"),
       aes(x=weo_change_inflation,y=change_rate_oct_2020_apr_2022,colour=advanced)) +
  stat_smooth(method="lm",inherit.aes=FALSE,aes(x=weo_change_inflation,y=change_rate_oct_2020_apr_2022)) +
  geom_point(size=3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept= 0) +
  geom_label_repel(aes(label = country),size=9) +
  stat_poly_eq(formula=y~x,aes(x=weo_change_inflation,y=change_rate_oct_2020_apr_2022,color=NA,label=paste(..eq.label..,"p-value:",round(..p.value..,2),sep="~~~")),size=9,color="black",parse=TRUE) +
  labs(y="Percentage point change in policy rate",x="Percentage point change in year-ahead inflation forecast",colour="Classification",caption="Sources: International Monetary Fund; Bank for International Settlements") +
  theme(text = element_text(size=30),axis.text.x = element_text(size=25),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))


ggsave(plot=fig7,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/fig7_inflation_forecast_vs_policy_rates_20230411.png"),
       width=17,height=10,units="in")

# save a copy of this dataset
save(panel_df,file=paste0(data_folder,"Final/panel_df_20230411.rda"))






















































#---------------------------------------------------------------------------------------------------

# make scatterplots
df_scatter_monthly = panel_df_monthly %>%
  filter(date>="2019-12-01") %>%
  drop_na(rate,weo_change_inflation) %>%
  group_by(country) %>%
  summarize(change_rate_oct_2020_present=rate[n()]-rate[date=="2020-10-01"],
            weo_change_inflation=weo_change_inflation[1],
            advanced=advanced[1]) %>%
  mutate(advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced)) %>%
  ungroup()

library(ggpmisc)

ggplot(df_scatter_monthly,aes(x=weo_change_inflation,y=change_rate_oct_2020_present,colour=advanced)) +
  stat_smooth(method="lm",inherit.aes=FALSE,aes(x=weo_change_inflation,y=change_rate_oct_2020_present)) +
  geom_point(size=3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept= 0) +
  geom_label_repel(aes(label = country)) +
  stat_poly_eq(formula=y~x,aes(x=weo_change_inflation,y=change_rate_oct_2020_present,color=NA,label=paste(..eq.label..,"p-value:",..p.value..,sep="~~~")),color="black",parse=TRUE) +
  labs(y="Percentage point change in policy rate\nfrom Oct 2020 to Oct 2021",x="Percentage point change in year-ahead inflation forecast from Oct 2020 to Oct 2021") +
  theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

# run panel regression in differences
library(lfe)

# create model
panel_df_monthly = panel_df_monthly %>%
  mutate(reg_inflation_diff = ifelse(diff_inflation_variable=="diff_total_inflation",diff_total_inflation,diff_core_inflation),
         country_group=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced))


panel_df = panel_df %>%
  mutate(reg_inflation_diff = ifelse(diff_inflation_variable=="diff_total_inflation",diff_total_inflation,diff_core_inflation),
         country_group=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced))


# write function to export regression model to excel
export_reg = function(models,file){
  for(model in models){
    a = tidy(model)
    b = data.frame(a=)
  }
}

# four models per group: gdp + total/core, ip + total/core
# all
# model_monthly_ip_total = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"))
# model_monthly_ip_core = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"))
# model_quarterly_gdp_total = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"))
# model_quarterly_gdp_core = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"))
# htmlreg(list(model_monthly_ip_total,model_monthly_ip_core,model_quarterly_gdp_total,model_quarterly_gdp_core),file=paste0(results_folder,"Regression Results/2021-11-03_Panel_Regressions_2010_2019/all_countries_panel_2010_2019.html"),caption="Panel Regression Results: All countries, 2010-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("IP, Total Inflation","IP, Core Inflation","GDP, Total Inflation","GDP, Core Inflation"),stars=c(.01,.05,.1))
#
# model_monthly_ip_total = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_monthly_ip_core = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_quarterly_gdp_total = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_quarterly_gdp_core = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# htmlreg(list(model_monthly_ip_total,model_monthly_ip_core,model_quarterly_gdp_total,model_quarterly_gdp_core),file=paste0(results_folder,"Regression Results/2021-11-03_Panel_Regressions_2010_2019/advanced_econ_panel_2010_2019.html"),caption="Panel Regression Results: Advanced economies, 2010-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("IP, Total Inflation","IP, Core Inflation","GDP, Total Inflation","GDP, Core Inflation"),stars=c(.01,.05,.1))
#
# model_monthly_ip_total = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_monthly_ip_core = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_quarterly_gdp_total = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_quarterly_gdp_core = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# htmlreg(list(model_monthly_ip_total,model_monthly_ip_core,model_quarterly_gdp_total,model_quarterly_gdp_core),file=paste0(results_folder,"Regression Results/2021-11-03_Panel_Regressions_2010_2019/EME_panel_2010_2019.html"),caption="Panel Regression Results: Non-LA Emerging Market Economies, 2010-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("IP, Total Inflation","IP, Core Inflation","GDP, Total Inflation","GDP, Core Inflation"),stars=c(.01,.05,.1))
#
# model_monthly_ip_total = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# model_monthly_ip_core = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# model_quarterly_gdp_total = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# model_quarterly_gdp_core = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_monthly_ip_total,model_monthly_ip_core,model_quarterly_gdp_total,model_quarterly_gdp_core),file=paste0(results_folder,"Regression Results/2021-11-03_Panel_Regressions_2010_2019/LA_panel_2010_2019.html"),caption="Panel Regression Results: Latin America, 2010-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("IP, Total Inflation","IP, Core Inflation","GDP, Total Inflation","GDP, Core Inflation"),stars=c(.01,.05,.1))
#
# monthly_country_info = panel_df_monthly %>%
#   drop_na(diff_rate,diff_output_gap) %>%
#   filter(date<="2019-12-01") %>%
#   group_by(country) %>%
#   summarize(min_total_ip = as.character(min(date[!is.na(diff_total_inflation)])),
#             min_core_ip = as.character(min(date[!is.na(diff_core_inflation)])),
#             max_core_ip = as.character(max(date[!is.na(diff_core_inflation)])))
#
# quarterly_country_info = panel_df %>%
#   drop_na(diff_rate,diff_output_gap) %>%
#   filter(date<="2019-12-01") %>%
#   group_by(country) %>%
#   summarize(min_total_gdp = as.character(min(date[!is.na(diff_total_inflation)])),
#             max_total_gdp = as.character(max(date[!is.na(diff_total_inflation)])),
#             min_core_gdp = as.character(min(date[!is.na(diff_core_inflation)])),
#             max_core_gdp = as.character(max(date[!is.na(diff_core_inflation)])))
#
# country_info = full_join(monthly_country_info,quarterly_country_info,by="country")
# write.csv(country_info,file=paste0(results_folder,"Regression Results/2021-11-03_Panel_Regressions_2010_2019/country_info_panel_reg_2010_2019.csv"))
#
#
# # reorganize regression results
# model_monthly_ip_total_all = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"))
# model_monthly_ip_total_advanced = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_monthly_ip_total_eme = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_monthly_ip_total_lac = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_monthly_ip_total_all,model_monthly_ip_total_advanced,model_monthly_ip_total_eme,model_monthly_ip_total_lac),file=paste0(results_folder,"Regression Results/2021-11-04_Reorganized_Panel_Regs_2010_2019/total_inflation_ip_panel_2010_2019.html"),caption="Panel Regression Results: Total Inflation and IP Output Gap, 2010-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("All countries","Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#
# model_monthly_ip_core_all = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"))
# model_monthly_ip_core_advanced = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_monthly_ip_core_eme = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_monthly_ip_core_lac = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_monthly_ip_core_all,model_monthly_ip_core_advanced,model_monthly_ip_core_eme,model_monthly_ip_core_lac),file=paste0(results_folder,"Regression Results/2021-11-04_Reorganized_Panel_Regs_2010_2019/core_inflation_ip_panel_2010_2019.html"),caption="Panel Regression Results: Core Inflation and IP Output Gap, 2010-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("All countries","Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#
# model_quarterly_gdp_total_all = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"))
# model_quarterly_gdp_total_advanced = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_quarterly_gdp_total_eme = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_quarterly_gdp_total_lac = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_quarterly_gdp_total_all,model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0(results_folder,"Regression Results/2021-11-04_Reorganized_Panel_Regs_2010_2019/total_inflation_gdp_panel_2010_2019.html"),caption="Panel Regression Results: Total Inflation and GDP Output Gap, 2010-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("All countries","Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#
# model_quarterly_gdp_core_all = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"))
# model_quarterly_gdp_core_advanced = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_quarterly_gdp_core_eme = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_quarterly_gdp_core_lac = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_quarterly_gdp_core_all,model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0(results_folder,"Regression Results/2021-11-04_Reorganized_Panel_Regs_2010_2019/core_inflation_gdp_panel_2010_2019.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap, 2010-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("All countries","Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#
# # save individual regressions with time FE
# individ_country_regs = createWorkbook("Individual Country Regressions")
#
# addWorksheet(individ_country_regs,"TotalInflation, IP")
# total_inf_ip_df = data.frame()
# for(a in unique(panel_df_monthly$country)){
#
#   form = as.formula(paste0("diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate+time_trend"))
#
#   data_filtered = panel_df_monthly %>%
#     filter(country==a&date>="2010-01-01"&date<="2019-12-01") %>%
#     drop_na(diff_rate,lag_diff_rate,diff_total_inflation,diff_output_gap)
#
#   reg = lm_robust(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate+time_trend,data =  data_filtered)
#
#   b = tidy(reg)
#
#   total_inf_ip_df = rbind(total_inf_ip_df,c(a,b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],reg$nobs,reg$r.squared,as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(total_inf_ip_df) = c("country","diff_total_inflation_coef","inflation_tstat","diff_output_gap_coef","output_gap_tstat","lag_diff_rate_coef","lag_rate_tstat","num_obs","r_squared","start_date","end_date")
# }
# writeData(individ_country_regs,sheet=1,total_inf_ip_df %>% arrange(country),rowNames=TRUE)
#
#
# addWorksheet(individ_country_regs,"Core Inflation, IP")
# core_inf_ip_df = data.frame()
# for(a in unique(panel_df_monthly$country)){
#
#   form = as.formula(paste0("diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate+time_trend"))
#
#   data_filtered = panel_df_monthly %>%
#     filter(country==a&date>="2010-01-01"&date<="2019-12-01") %>%
#     drop_na(diff_rate,lag_diff_rate,diff_core_inflation,diff_output_gap)
#
#   reg = lm_robust(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate+time_trend,data =  data_filtered)
#
#   b = tidy(reg)
#
#   core_inf_ip_df = rbind(core_inf_ip_df,c(a,b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],reg$nobs,reg$r.squared,as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(core_inf_ip_df) = c("country","diff_core_inflation_coef","inflation_tstat","diff_output_gap_coef","output_gap_tstat","lag_diff_rate_coef","lag_rate_tstat","num_obs","r_squared","start_date","end_date")
# }
# writeData(individ_country_regs,sheet=2,core_inf_ip_df %>% arrange(country),rowNames=TRUE)
#
#
# addWorksheet(individ_country_regs,"Total Inflation, GDP")
# total_inf_gdp_df = data.frame()
# for(a in unique(panel_df$country)){
#
#   form = as.formula(paste0("diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate+time_trend"))
#
#   data_filtered = panel_df %>%
#     filter(country==a&date>="2010-01-01"&date<="2019-12-01") %>%
#     drop_na(diff_rate,lag_diff_rate,diff_total_inflation,diff_output_gap)
#
#   reg = lm_robust(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate+time_trend,data =  data_filtered)
#
#   b = tidy(reg)
#
#   total_inf_gdp_df = rbind(total_inf_gdp_df,c(a,b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],reg$nobs,reg$r.squared,as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(total_inf_gdp_df) = c("country","diff_core_inflation_coef","inflation_tstat","diff_output_gap_coef","output_gap_tstat","lag_diff_rate_coef","lag_rate_tstat","num_obs","r_squared","start_date","end_date")
# }
# writeData(individ_country_regs,sheet=3,total_inf_gdp_df %>% arrange(country),rowNames=TRUE)
#
#
# addWorksheet(individ_country_regs,"Core Inflation, GDP")
# core_inf_gdp_df = data.frame()
# for(a in unique(panel_df$country)){
#
#   form = as.formula(paste0("diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate+time_trend"))
#
#   data_filtered = panel_df %>%
#     filter(country==a&date>="2010-01-01"&date<="2019-12-01") %>%
#     drop_na(diff_rate,lag_diff_rate,diff_core_inflation,diff_output_gap)
#
#   reg = lm_robust(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate+time_trend,data =  data_filtered)
#
#   b = tidy(reg)
#
#   core_inf_gdp_df = rbind(core_inf_gdp_df,c(a,b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],reg$nobs,reg$r.squared,as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(core_inf_gdp_df) = c("country","diff_core_inflation_coef","inflation_tstat","diff_output_gap_coef","output_gap_tstat","lag_diff_rate_coef","lag_rate_tstat","num_obs","r_squared","start_date","end_date")
# }
# writeData(individ_country_regs,sheet=4,core_inf_gdp_df %>% arrange(country),rowNames=TRUE)
#
# #saveWorkbook(individ_country_regs,file=paste0(results_folder,"Regression Results/2021-11-04_Reorganized_Panel_Regs_2010_2019/Individual_Country_Panel_Regressions.xlsx"),overwrite=TRUE)
#
#
#
#
# # run in levels
# model_monthly_ip_total_all = felm(rate~CPI+output_gap+lag_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"))
# model_monthly_ip_total_advanced = felm(rate~CPI+output_gap+lag_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_monthly_ip_total_eme = felm(rate~CPI+output_gap+lag_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_monthly_ip_total_lac = felm(rate~CPI+output_gap+lag_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_monthly_ip_total_all,model_monthly_ip_total_advanced,model_monthly_ip_total_eme,model_monthly_ip_total_lac),file=paste0(results_folder,"Regression Results/2021-11-04_Reorganized_Panel_Regs_2010_2019/total_inflation_ip_panel_levels_2010_2019.html"),caption="Panel Regression Results: Total Inflation and IP Output Gap in Levels, 2010-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("All countries","Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#
# model_monthly_ip_core_all = felm(rate~core_inflation+output_gap+lag_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"))
# model_monthly_ip_core_advanced = felm(rate~core_inflation+output_gap+lag_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_monthly_ip_core_eme = felm(rate~core_inflation+output_gap+lag_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_monthly_ip_core_lac = felm(rate~core_inflation+output_gap+lag_rate|country + date|0|country,data = panel_df_monthly %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_monthly_ip_core_all,model_monthly_ip_core_advanced,model_monthly_ip_core_eme,model_monthly_ip_core_lac),file=paste0(results_folder,"Regression Results/2021-11-04_Reorganized_Panel_Regs_2010_2019/core_inflation_ip_panel_levels_2010_2019.html"),caption="Panel Regression Results: Core Inflation and IP Output Gap in Levels, 2010-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("All countries","Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#
# model_quarterly_gdp_total_all = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"))
# model_quarterly_gdp_total_advanced = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_quarterly_gdp_total_eme = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_quarterly_gdp_total_lac = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_quarterly_gdp_total_all,model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0(results_folder,"Regression Results/2021-11-04_Reorganized_Panel_Regs_2010_2019/total_inflation_gdp_panel_levels_2010_2019.html"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2010-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("All countries","Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#
# model_quarterly_gdp_core_all = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"))
# model_quarterly_gdp_core_advanced = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_quarterly_gdp_core_eme = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_quarterly_gdp_core_lac = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2010-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_quarterly_gdp_core_all,model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0(results_folder,"Regression Results/2021-11-04_Reorganized_Panel_Regs_2010_2019/core_inflation_gdp_panel_levels_2010_2019.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2010-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("All countries","Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#
#
# # regressions from 2007 onwards, only gdp
# model_quarterly_gdp_total_all = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"))
# model_quarterly_gdp_total_advanced = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_quarterly_gdp_total_eme = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_quarterly_gdp_total_lac = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_quarterly_gdp_total_all,model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0(results_folder,"Regression Results/2021-11-05_Panel_Regressions_2007_2019/total_inflation_gdp_panel_levels_2097_2019.html"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("All countries","Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#
# model_quarterly_gdp_core_all = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"))
# model_quarterly_gdp_core_advanced = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_quarterly_gdp_core_eme = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_quarterly_gdp_core_lac = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_quarterly_gdp_core_all,model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0(results_folder,"Regression Results/2021-11-05_Panel_Regressions_2007_2019/core_inflation_gdp_panel_levels_2007_2019.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("All countries","Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#
# model_quarterly_gdp_total_all = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"))
# model_quarterly_gdp_total_advanced = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_quarterly_gdp_total_eme = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_quarterly_gdp_total_lac = felm(diff_rate~diff_total_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_quarterly_gdp_total_all,model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0(results_folder,"Regression Results/2021-11-05_Panel_Regressions_2007_2019/total_inflation_gdp_panel_diffs_2007_2019.html"),caption="Panel Regression Results: Total Inflation and GDP Output Gap, Differences, 2007-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("All countries","Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#
# model_quarterly_gdp_core_all = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"))
# model_quarterly_gdp_core_advanced = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
# model_quarterly_gdp_core_eme = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
# model_quarterly_gdp_core_lac = felm(diff_rate~diff_core_inflation+diff_output_gap+lag_diff_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# htmlreg(list(model_quarterly_gdp_core_all,model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0(results_folder,"Regression Results/2021-11-05_Panel_Regressions_2007_2019/core_inflation_gdp_panel_diffs_2007_2019.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap, Differences, 2007-2019, Time and Country FE",caption.above=TRUE,
#         custom.model.names = c("All countries","Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#
#
