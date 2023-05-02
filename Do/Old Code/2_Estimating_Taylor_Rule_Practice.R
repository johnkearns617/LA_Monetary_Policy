# 2_Estimating_Taylor_Rule.R
# John Kearns
# Goal: Do-file to gather exchange rate and economic activity data in order to estimate a Taylor Rule. THIS DO-FILE IS OLD AND UNNEEDED!!!!!!!!!!
# Date Created: 2021-09-29
# Last Updated: 2021-09-29

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
source(paste0(do_folder,"Mode1.R"))

# load(paste0(data_folder,"Processing/1_Inflation_and_Rates.RData"))
#
#
# # load in exchange rate data
# neer =  read_excel(paste0(data_folder,"Raw/neer_bis.xlsx"))[-c(1:2),]
# colnames(neer)=c("date",neer[1,2:ncol(neer)])
# neer = neer[-c(1:2),]
# neer = neer %>%
#   mutate(date=as.Date(as.numeric(date),format="%Y-%m-%d",origin="1899-12-30"),
#          date=as.Date(paste0(substr(date,1,8),"01"),format="%Y-%m-%d")) %>%
#   mutate_at(vars(`Algeria`:`United States`),as.numeric)
# neer = reshape2::melt(neer,id.vars="date",measure.vars=colnames(neer)[2:ncol(neer)],variable.name="country",value.name="value") %>%
#   mutate(type="NEER")
# neer$country = str_replace_all(neer$country,setNames(c("Hong Kong","Taiwan","South Korea"),c("Hong Kong SAR","Chinese Taipei","Korea")))
# setdiff(df1$country,neer$country)
# # No Serbia and North Macedonia
#
# # add 12-month % change
# neer = neer %>%
#   group_by(country) %>%
#   mutate(neer_12_month_change = (value/dplyr::lag(value,12)-1)*100)
#
# # load in industrial production data
# ip = read_excel(paste0(data_folder,"Raw/Industrial Production_20210929.xlsx"))
# colnames(ip) = c("date",ip[1,2:ncol(ip)])
# ip = ip[-c(1:25),]
# ip = ip %>%
#   mutate(date=as.Date(as.numeric(date),format="%Y-%m-%d",origin="1899-12-31"),
#          date=as.Date(paste0(substr(date,1,8),"01"),format="%Y-%m-%d"))
# ip = reshape2::melt(ip,id.vars="date",measure.vars=colnames(ip)[2:ncol(ip)],variable.name="country",value.name="value") %>%
#   mutate(type="IP") %>%
#   mutate(value=as.numeric(value))
#
# # apply the HP filter to get trend, also put them all in the same base (make January 2020 the base month)
# ip = ip %>%
#   group_by(country) %>%
#   mutate(value_rebase = (value/value[date=="2020-01-01"])*100) %>%
#   ungroup()
#
# ip_dropna = ip %>%
#   filter(!is.na(value_rebase)) %>%
#   group_by(country) %>%
#   arrange(date) %>%
#   mutate(ip_trend = mFilter::hpfilter(value_rebase,type="lambda",freq=100000)$trend) # get the smoothed series for IP
#
# ggplot(ip_dropna %>% filter(country=="United States")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=ip_trend),colour="blue",size=2)
# # does the graph look okay? the trend line shouldnt map *too* closely to the actual line
#
#
# ip = left_join(ip,ip_dropna %>% select(date,country,ip_trend),by=c("date","country")) %>%
#   mutate(country=as.character(country))
# ggplot(ip_dropna %>% filter(country=="United States")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=ip_trend),colour="blue",size=2)
# ggplot(ip_dropna %>% filter(country=="Israel")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=ip_trend),colour="blue",size=2)
# ggplot(ip_dropna %>% filter(country=="Brazil")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=ip_trend),colour="blue",size=2)
# ggplot(ip_dropna %>% filter(country=="India")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=ip_trend),colour="blue",size=2)
# ggplot(ip_dropna %>% filter(country=="Serbia")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=ip_trend),colour="blue",size=2)
#
# ip$country = str_replace_all(ip$country,setNames(c("Euro area","Russia"),c("European Union","Russian Federation")))
#
# # read in the inflation targeting data
# # google to find their inflation targets by year
# # My source, now suspended, is http://www.centralbanknews.info/    ---   RIP
# targets = read_excel(paste0(data_folder,"Raw/Inflation_Targets.xlsx"),sheet = "Sheet2")
# targets = reshape2::melt(targets,id.vars="Country",measure.vars=colnames(targets)[2:ncol(targets)],variable.name="year",value.name="inflation_target") %>%
#   rename(country=Country) %>%
#   mutate(year=as.numeric(as.character(year)),
#          inflation_target=as.numeric(inflation_target))
#
# # construct dataset, joining policy rate, inflation, exchange rates, IP, and target rate
# df2 = df1 %>%
#   select(-avg) %>%
#   pivot_wider(values_from=value,id_cols=c(date,country),names_from=type)%>%
#   mutate(advanced=ifelse(country%in%emes,"Emerging Market Economy","Advanced Economy"),
#          advanced1 = ifelse(country%in%full_countries,"Emerging Market Economy","Advanced Economy"),
#          year=year(date))
#
# panel_df = left_join(df2,targets,by=c("country","year"))
#
# panel_df = left_join(panel_df,ip %>% select(-c(value,type)) %>% rename(ip_index = value_rebase),by=c("country","date")) %>%
#   mutate(inflation_gap = (CPI*100)-inflation_target,
#          output_gap = ip_index/ip_trend*100-100,      # for now, set output gap using industrial production
#          rate=rate*100) %>%
#   group_by(country) %>%
#   mutate(CPI=CPI*100,
#          lag_rate=dplyr::lag(rate,1),
#          lag_output_gap=dplyr::lag(output_gap,1),
#          lag_inflation_gap=dplyr::lag(CPI,1)-inflation_target,
#          lag_CPI=dplyr::lag(CPI,1)) %>%
#   #filter(year>=2011) %>% # make 2011 the beginning year for the analysis
#   mutate(time_trend=order(date))
#
# panel_df = left_join(panel_df,neer %>% select(-type) %>% rename(neer = value),by=c("country","date"))
#
# panel_df = left_join(panel_df,panel_df %>% ungroup() %>% filter(country=="United States") %>% select(date,rate) %>% rename(US_rate=rate),by=c("date"))
# # the above code sets the associated US policy rate that EMEs would be responding to
#
# # select out the countries we do not want
# #panel_df = panel_df %>%
# #  filter(!(country%in%c("Argentina","China","Croatia","Hong Kong","Iceland","Malaysia","New Zealand","North Macedonia","Saudi Arabia","Singapore","Taiwan")))
#
# # add in core inflation for the usa
# core_cpi_usa = read.csv(paste0(data_folder,"Raw/usa_core_inflation.csv")) %>%
#   mutate(date=as.Date(date,format="%m/%d/%Y"),
#          core_cpi=as.numeric(core_cpi)-.5) # roughly the PCE (CPI often above PCE by .5)
#
# panel_df = left_join(panel_df,core_cpi_usa,by=c("date","country"))
#
# # test Taylor Rule on USA
# for(a in c("United States")){
#
#   taylor_rule_simple = lm_robust(rate~CPI+output_gap,data=panel_df %>% filter(country==a&date>="1990-01-01"&date<="2019-12-01"))
#   test = cbind(panel_df %>% filter(country==a&date>="2011-01-01"),fitted_rate_simple=c(taylor_rule_simple$fitted.values[-c(1:252)],rep(NA,30))) # the number 30 refers to the number of months since 2019
#
#   taylor_rule_inflation_gap = lm_robust(rate~CPI+inflation_gap+output_gap,data=panel_df %>% filter(country==a&date>="1990-01-01"&date<="2019-12-01"))
#   test = cbind(test,fitted_rate_inf_gap=c(taylor_rule_inflation_gap$fitted.values,rep(NA,30)))
#
#   taylor_rule_time_trend = lm_robust(rate~CPI+inflation_gap+output_gap+time_trend,data=panel_df %>% filter(country==a&date>="1990-01-01"&date<="2019-12-01"))
#   test = cbind(test,fitted_rate_time_trend=c(taylor_rule_time_trend$fitted.values,rep(NA,30)))
#
#   taylor_rule_neer = lm_robust(rate~CPI+inflation_gap+output_gap+time_trend+neer_12_month_change,data=panel_df %>% filter(country==a&date>="1990-01-01"&date<="2019-12-01"))
#   test = cbind(test,fitted_rate_neer=c(taylor_rule_neer$fitted.values,rep(NA,30)))
#
#   if(Mode1(panel_df$advanced[panel_df$country==a],na.rm=TRUE)=="Emerging Market Economy"){
#     taylor_rule_usa = lm_robust(rate~CPI+inflation_gap+output_gap+lag_rate+time_trend+neer_12_month_change+US_rate,data=panel_df %>% filter(country==a&date>="2011-01-01"&date<="2019-12-01"))
#     test = cbind(test,fitted_rate_usa=c(taylor_rule_usa$fitted.values,rep(NA,30)))
#   }
#
#   test = test %>%
#     mutate(taylors_rule=2+CPI+.5*(CPI-2)+.5*(output_gap)) # this is Taylor's "traditional Taylor rule"
#
#   assign(paste0("rate_paths_",gsub(" ","_",a)),test[,c(1,2,4,21:ncol(test))])
#
#   test1 = reshape2::melt(test[,c(1,2,4,21:ncol(test))],id.vars=c("date"),measure.vars=colnames(test[,c(1,2,4,21:ncol(test))])[3:ncol(test[,c(1,2,4,21:ncol(test))])],variable.name="rate_path",value.name="rate")
#
#   print(ggplot(test1,aes(x=date,y=rate,color=rate_path)) +
#     geom_line(size=2) +
#     labs(caption=a))
#
#   if(Mode1(panel_df$advanced[panel_df$country==a],na.rm=TRUE)=="Emerging Market Economy"){
#     models = c("taylor_rule_simple","taylor_rule_inflation_gap","taylor_rule_time_trend","taylor_rule_neer","taylor_rule_usa")
#   } else{
#     models =  c("taylor_rule_simple","taylor_rule_inflation_gap","taylor_rule_time_trend","taylor_rule_neer")
#   }
#
#   htmlreg(mget(models),include.ci=FALSE,file=paste0(results_folder,"taylor_rules_",gsub(" ","_",a),".html"),caption=paste0("Estimated Taylor Rules, 2011-2019, ",a),caption.above = TRUE,
#           include.rsquared=TRUE,include.adjrs=FALSE,include.rmse=TRUE,include.nobs=TRUE)
# }
#
#
# panel_df = left_join(df2,targets,by=c("country","year")) %>%
#   mutate(quarter=quarter(date)) %>%
#   left_join(core_cpi_usa,by=c("date","country")) %>%
#   group_by(country,quarter,year) %>%
#   arrange(date) %>%
#   mutate(rate=rate[n()], # use rate at end of quarter
#          CPI=mean(CPI,na.rm=TRUE),
#          CPI=ifelse(is.nan(CPI),NA,CPI),
#          inflation_target=inflation_target[n()],
#          core_cpi = mean(core_cpi,na.rm=TRUE),
#          core_cpi = ifelse(is.nan(core_cpi),NA,core_cpi)) %>%
#   ungroup() %>%
#   arrange(date)
#
# # below code is an annoying way to join together the monthly IP data with the quarterly data I just made above
# panel_df = left_join(panel_df,ip %>% select(-c(value,type)) %>% rename(ip_index = value_rebase),by=c("country","date")) %>%
#   group_by(country,year,quarter) %>%
#   summarize(date=date[n()],
#             CPI=CPI[n()],
#             core_cpi=core_cpi[n()],
#             rate=rate[n()],
#             advanced=advanced[n()],
#             advanced1=advanced1[n()],
#             inflation_target=inflation_target[n()],
#             ip_index=mean(ip_index,na.rm=TRUE),
#             ip_trend=mean(ip_trend,na.rm=TRUE),
#             ip_index=ifelse(is.nan(ip_index),NA,ip_index),
#             ip_trend=ifelse(is.nan(ip_trend),NA,ip_trend)) %>%
#   ungroup() %>%
#   mutate(inflation_gap = (CPI*100)-inflation_target,
#          output_gap = ip_index/ip_trend*100-100,
#          rate=rate*100) %>%
#   group_by(country) %>%
#   mutate(CPI=CPI*100,
#          lag_rate=dplyr::lag(rate,1),
#          lag_output_gap=dplyr::lag(output_gap,1),
#          lag_inflation_gap=dplyr::lag(CPI,1)-inflation_target,
#          lag_CPI=dplyr::lag(CPI,1)) %>%
#   #filter(year>=2011) %>% # make 2011 the beginning year for the analysis
#   mutate(time_trend=order(date))
#
# panel_df = left_join(panel_df,neer %>% select(-type) %>% rename(neer = value),by=c("country","date"))
#
# panel_df = left_join(panel_df,panel_df %>% ungroup() %>% filter(country=="United States") %>% select(date,rate) %>% rename(US_rate=rate),by=c("date"))
#
# # join in with quarterly GDP
# gdp = read_excel(paste0(data_folder,"Raw/quarter_real_gdp.xlsx"))
# colnames(gdp) = c("date",gdp[1,2:ncol(gdp)])
# gdp = gdp[-c(1:25),]
# gdp = gdp %>%
#   mutate(date=as.Date(as.numeric(date),format="%Y-%m-%d",origin="1899-12-31"),
#          date=as.Date(paste0(substr(date,1,8),"01"),format="%Y-%m-%d"))
# gdp = reshape2::melt(gdp,id.vars="date",measure.vars=colnames(gdp)[2:ncol(gdp)],variable.name="country",value.name="value") %>%
#   mutate(type="gdp") %>%
#   mutate(value=as.numeric(value),
#          country=as.character(country))
#
# # apply the HP filter to get trend, also put them all in the same base (make Q1 2020 the base month)
# gdp = gdp %>%
#   group_by(country) %>%
#   mutate(value_rebase = (value/value[date=="2020-03-01"])*100) %>%
#   ungroup()
#
# gdp_dropna = gdp %>%
#   filter(!is.na(value_rebase)) %>%
#   group_by(country) %>%
#   arrange(date) %>%
#   mutate(gdp_trend = mFilter::hpfilter(value_rebase,type="lambda",freq=1600)$trend)
#
# ggplot(gdp_dropna %>% filter(country=="United States")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=gdp_trend),colour="blue",size=2)
# # does the graph look okay? the trend line shouldnt map *too* closely to the actual line
#
# # WE MAY WANT TO APPLY THE MORE STRINGENT PROCESS TO THIS OUTPUT GAP THAT WE USE IN THE INFLATION PAPER
#
# gdp = left_join(gdp,gdp_dropna %>% select(date,country,gdp_trend),by=c("date","country")) %>%
#   mutate(country=as.character(country))
# ggplot(gdp_dropna %>% filter(country=="United States")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=gdp_trend),colour="blue",size=2)
# ggplot(gdp_dropna %>% filter(country=="Israel")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=gdp_trend),colour="blue",size=2)
# ggplot(gdp_dropna %>% filter(country=="Brazil")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=gdp_trend),colour="blue",size=2)
# ggplot(gdp_dropna %>% filter(country=="India")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=gdp_trend),colour="blue",size=2)
# ggplot(gdp_dropna %>% filter(country=="Serbia")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=gdp_trend),colour="blue",size=2)
#
# gdp$country = str_replace_all(gdp$country,setNames(c("Euro area","Russia","Hong Kong"),c("Euro Area","Russian Federation","Hong Kong SAR \\(China\\)")))
#
# panel_df = left_join(panel_df,gdp %>% select(-c(value,type)) %>% rename(gdp_index=value_rebase),by=c("date","country")) %>%
#   mutate(gdp_output_gap=(gdp_index/gdp_trend-1)*100)
#
#
# for(a in c("United States")){
#
#   taylor_rule_simple = lm_robust(rate~CPI+output_gap+lag_rate,data=panel_df %>% filter(country==a&date>="1990-01-01"&date<="2007-12-01"))
#   test = cbind(panel_df %>% filter(country==a&date>="1990-01-01"),fitted_rate_simple=c(taylor_rule_simple$fitted.values,rep(NA,58)))
#
#   #taylor_rule_inflation_gap = lm_robust(rate~CPI+inflation_gap+output_gap+lag_rate,data=panel_df %>% filter(country==a&date>="2011-01-01"&date<="2019-12-01"))
#   #test = cbind(test,fitted_rate_inf_gap=c(taylor_rule_inflation_gap$fitted.values,rep(NA,7)))
#
#   #taylor_rule_time_trend = lm_robust(rate~CPI+inflation_gap+output_gap+lag_rate+time_trend,data=panel_df %>% filter(country==a&date>="2011-01-01"&date<="2019-12-01"))
#   #test = cbind(test,fitted_rate_time_trend=c(taylor_rule_time_trend$fitted.values,rep(NA,7)))
#
#   #taylor_rule_neer = lm_robust(rate~CPI+inflation_gap+output_gap+lag_rate+time_trend+neer_12_month_change,data=panel_df %>% filter(country==a&date>="2011-01-01"&date<="2019-12-01"))
#   #test = cbind(test,fitted_rate_neer=c(taylor_rule_neer$fitted.values,rep(NA,7)))
#
#   #if(Mode1(panel_df$advanced[panel_df$country==a],na.rm=TRUE)=="Emerging Market Economy"){
#   #  taylor_rule_usa = lm_robust(rate~CPI+inflation_gap+output_gap+lag_rate+time_trend+neer_12_month_change+US_rate,data=panel_df %>% filter(country==a&date>="2011-01-01"&date<="2019-12-01"))
#   #  test = cbind(test,fitted_rate_usa=c(taylor_rule_usa$fitted.values,rep(NA,7)))
#   #}
#
#   test = test %>%
#     mutate(taylors_rule=2+CPI+.47*(output_gap))
#
#   assign(paste0("q_rate_paths_",gsub(" ","_",a)),test[,c(1,4,6,22:ncol(test))])
#
#   test1 = reshape2::melt(test[,c(1,4,6,22:ncol(test))],id.vars=c("date"),measure.vars=colnames(test[,c(1,4,6,22:ncol(test))])[3:ncol(test[,c(1,4,6,22:ncol(test))])],variable.name="rate_path",value.name="rate")
#
#   print(ggplot(test1,aes(x=date,y=rate,color=rate_path)) +
#           geom_line(size=2) +
#           labs(caption=a))
#
#   #if(Mode1(panel_df$advanced[panel_df$country==a],na.rm=TRUE)=="Emerging Market Economy"){
#   #  models = c("taylor_rule_simple","taylor_rule_inflation_gap","taylor_rule_time_trend","taylor_rule_neer","taylor_rule_usa")
#   #} else{
#   #  models =  c("taylor_rule_simple","taylor_rule_inflation_gap","taylor_rule_time_trend","taylor_rule_neer")
#   #}
#
#   htmlreg(mget("taylor_rule_simple"),include.ci=FALSE,file=paste0(results_folder,"taylor_rules_q_1990_2007_",gsub(" ","_",a),".html"),caption=paste0("Estimated Taylor Rules, Quarterly, 1990-2007, ",a),caption.above = TRUE,
#           include.rsquared=TRUE,include.adjrs=FALSE,include.rmse=TRUE,include.nobs=TRUE)
# }
#
# # add brazil core inflation
# brazil_core_inflation = read.delim(paste0(data_folder,"Raw/brazil_core_inflation.csv"),header=TRUE,sep=";")
# colnames(brazil_core_inflation) = c("date","brazil_core_inflation")
# brazil_core_inflation = brazil_core_inflation %>%
#   mutate(date=as.Date(paste0(gsub("\\/","-",date),"-01"),format="%m-%Y-%d"),
#          brazil_core_inflation=as.numeric(brazil_core_inflation),
#          brazil_core_inflation1 = as.numeric(c(rep(NA,11),zoo::rollapply(1+(brazil_core_inflation/100),12,prod))),
#          brazil_core_inflation=(brazil_core_inflation1-1)*100) %>%
#   select(-brazil_core_inflation1) %>%
#   mutate(country="Brazil")
#
# panel_df = left_join(panel_df,brazil_core_inflation,by=c("date","country"))
#
# # core cpi USA regressions
# taylor_rule_simple_2011_2019 = lm_robust(rate~core_cpi+output_gap+lag_rate,data=panel_df %>% filter(country=="United States"&date>="2011-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2019 = lm_robust(rate~core_cpi+output_gap+lag_rate,data=panel_df %>% filter(country=="United States"&date>="1970-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2007 = lm_robust(rate~core_cpi+output_gap+lag_rate,data=panel_df %>% filter(country=="United States"&date>="1970-01-01"&date<="2007-12-01"))
# taylor_rule_simple_1990_2007 = lm_robust(rate~core_cpi+output_gap+lag_rate,data=panel_df %>% filter(country=="United States"&date>="1990-01-01"&date<="2007-12-01"))
# htmlreg(mget(c("taylor_rule_simple_2011_2019","taylor_rule_simple_1970_2019","taylor_rule_simple_1970_2007","taylor_rule_simple_1990_2007")),include.ci=FALSE,file=paste0(results_folder,"taylor_rules_q_1990_2007_core_cpi_",gsub(" ","_",a),".html"),caption=paste0("Estimated Taylor Rules, Quarterly, Core CPI Inflation - 0.5, ",a),caption.above = TRUE,
#         include.rsquared=TRUE,include.adjrs=FALSE,include.rmse=TRUE,include.nobs=TRUE)
#
#
# taylor_rule_simple_2011_2019 = lm_robust(rate~CPI+output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="2011-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2019 = lm_robust(rate~CPI+output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="1970-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2007 = lm_robust(rate~CPI+output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="1970-01-01"&date<="2007-12-01"))
# taylor_rule_simple_1990_2007 = lm_robust(rate~CPI+output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="1990-01-01"&date<="2007-12-01"))
# htmlreg(mget(c("taylor_rule_simple_2011_2019","taylor_rule_simple_1970_2019","taylor_rule_simple_1970_2007","taylor_rule_simple_1990_2007")),include.ci=FALSE,file=paste0(results_folder,"taylor_rules_q_all_cpi_",gsub(" ","_","Brazil"),".html"),caption=paste0("Estimated Taylor Rules, Quarterly, All CPI Inflation, IP, DATA STARTS IN 2002, ","Brazil"),caption.above = TRUE,
#         include.rsquared=TRUE,include.adjrs=FALSE,include.rmse=TRUE,include.nobs=TRUE)
#
#
# taylor_rule_simple_2011_2019 = lm_robust(rate~brazil_core_inflation+output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="2011-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2019 = lm_robust(rate~brazil_core_inflation+output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="1970-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2007 = lm_robust(rate~brazil_core_inflation+output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="1970-01-01"&date<="2007-12-01"))
# taylor_rule_simple_1990_2007 = lm_robust(rate~brazil_core_inflation+output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="1990-01-01"&date<="2007-12-01"))
# htmlreg(mget(c("taylor_rule_simple_2011_2019","taylor_rule_simple_1970_2019","taylor_rule_simple_1970_2007","taylor_rule_simple_1990_2007")),include.ci=FALSE,file=paste0(results_folder,"taylor_rules_q_core_cpi_",gsub(" ","_","Brazil"),".html"),caption=paste0("Estimated Taylor Rules, Quarterly, Core CPI Inflation, IP, DATA STARTS IN 2002, ","Brazil"),caption.above = TRUE,
#         include.rsquared=TRUE,include.adjrs=FALSE,include.rmse=TRUE,include.nobs=TRUE)
#
# taylor_rule_simple_2011_2019 = lm_robust(rate~core_cpi+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="United States"&date>="2011-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2019 = lm_robust(rate~core_cpi+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="United States"&date>="1970-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2007 = lm_robust(rate~core_cpi+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="United States"&date>="1970-01-01"&date<="2007-12-01"))
# taylor_rule_simple_1990_2007 = lm_robust(rate~core_cpi+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="United States"&date>="1990-01-01"&date<="2007-12-01"))
# htmlreg(mget(c("taylor_rule_simple_2011_2019","taylor_rule_simple_1970_2019","taylor_rule_simple_1970_2007","taylor_rule_simple_1990_2007")),include.ci=FALSE,file=paste0(results_folder,"taylor_rules_q_core_cpi_gdp_",gsub(" ","_","United States"),".html"),caption=paste0("Estimated Taylor Rules, Quarterly, Core CPI Inflation - 0.5, Real GDP, ","United States"),caption.above = TRUE,
#         include.rsquared=TRUE,include.adjrs=FALSE,include.rmse=TRUE,include.nobs=TRUE)
#
# taylor_rule_simple_2011_2019 = lm_robust(rate~CPI+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="United States"&date>="2011-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2019 = lm_robust(rate~CPI+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="United States"&date>="1970-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2007 = lm_robust(rate~CPI+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="United States"&date>="1970-01-01"&date<="2007-12-01"))
# taylor_rule_simple_1990_2007 = lm_robust(rate~CPI+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="United States"&date>="1990-01-01"&date<="2007-12-01"))
# htmlreg(mget(c("taylor_rule_simple_2011_2019","taylor_rule_simple_1970_2019","taylor_rule_simple_1970_2007","taylor_rule_simple_1990_2007")),include.ci=FALSE,file=paste0(results_folder,"taylor_rules_q_all_cpi_gdp_",gsub(" ","_","United States"),".html"),caption=paste0("Estimated Taylor Rules, Quarterly, Total CPI Inflation, Real GDP, ","United States"),caption.above = TRUE,
#         include.rsquared=TRUE,include.adjrs=FALSE,include.rmse=TRUE,include.nobs=TRUE)
#
# taylor_rule_simple_2011_2019 = lm_robust(rate~brazil_core_inflation+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="2011-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2019 = lm_robust(rate~brazil_core_inflation+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="1970-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2007 = lm_robust(rate~brazil_core_inflation+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="1970-01-01"&date<="2007-12-01"))
# taylor_rule_simple_1990_2007 = lm_robust(rate~brazil_core_inflation+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="1990-01-01"&date<="2007-12-01"))
# htmlreg(mget(c("taylor_rule_simple_2011_2019","taylor_rule_simple_1970_2019","taylor_rule_simple_1970_2007","taylor_rule_simple_1990_2007")),include.ci=FALSE,file=paste0(results_folder,"taylor_rules_q_cpi_gdp_",gsub(" ","_","Brazil"),".html"),caption=paste0("Estimated Taylor Rules, Quarterly, Core CPI Inflation, Real GDP, DATA STARTS 1995 ","Brazil"),caption.above = TRUE,
#         include.rsquared=TRUE,include.adjrs=FALSE,include.rmse=TRUE,include.nobs=TRUE)
#
# taylor_rule_simple_2011_2019 = lm_robust(rate~CPI+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="2011-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2019 = lm_robust(rate~CPI+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="1970-01-01"&date<="2019-12-01"))
# taylor_rule_simple_1970_2007 = lm_robust(rate~CPI+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="1970-01-01"&date<="2007-12-01"))
# taylor_rule_simple_1990_2007 = lm_robust(rate~CPI+gdp_output_gap+lag_rate,data=panel_df %>% filter(country=="Brazil"&date>="1990-01-01"&date<="2007-12-01"))
# htmlreg(mget(c("taylor_rule_simple_2011_2019","taylor_rule_simple_1970_2019","taylor_rule_simple_1970_2007","taylor_rule_simple_1990_2007")),include.ci=FALSE,file=paste0(results_folder,"taylor_rules_q_all_cpi_gdp_",gsub(" ","_","Brazil"),".html"),caption=paste0("Estimated Taylor Rules, Quarterly, Total CPI Inflation, Real GDP, DATA STARTS 1995 ","Brazil"),caption.above = TRUE,
#         include.rsquared=TRUE,include.adjrs=FALSE,include.rmse=TRUE,include.nobs=TRUE)
#
#
#
