# 7_Paper_Charts.R
# John Kearns and Bea Lee
# Goal: Do-file for making charts
# Date Created: 2021-11-03
# Last Updated: 2021-07-21

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
library(cowplot)
library(ggpmisc)
library(Hmisc)
source(paste0(do_folder,"Mode1.R"))

#load(paste0(data_folder,"Processing/3_WEO_Scatterplots.RData"))

# fix South Korea and Czech into non-LA EME
panel_df = panel_df %>%
  mutate(advanced=ifelse(country%in%emes|country%in%c("South Korea","Czech Republic"),"Non-LA EMEs",advanced), # make sure this definition is correct
         country_group=ifelse(country%in%emes|country%in%c("South Korea","Czech Republic"),"Non-LA EMEs",advanced))

panel_df_monthly = panel_df_monthly %>%
  mutate(advanced=ifelse(country%in%emes|country%in%c("South Korea","Czech Republic"),"Non-LA EMEs",advanced),
         country_group=ifelse(country%in%emes|country%in%c("South Korea","Czech Republic"),"Non-LA EMEs",advanced))

panel_df$country_group = ifelse(panel_df$country%in%c("Brazil","Chile","Colombia","Mexico","Peru"),"Latin America",panel_df$country_group)
panel_df_monthly$country_group = ifelse(panel_df_monthly$country%in%c("Brazil","Chile","Colombia","Mexico","Peru"),"Latin America",panel_df_monthly$country_group)

# make daily policy rate and exchange rate charts
# policy rates
daily_rates <- BISdata::fetch_dataset(dest.dir = paste0(data_folder,"Raw/BIS_data_folder/"),
                                      "https://www.bis.org/statistics/full_cbpol_d_csv_row.zip")
colnames(daily_rates) = c("date",unlist(sapply(daily_rates[1,2:ncol(daily_rates)],strsplit,split=":"))[seq(from=2,by=2,length.out=38)])
daily_rates = daily_rates[-c(1:2),]

daily_rates = reshape2::melt(daily_rates,id.vars="date",measure.vars=colnames(daily_rates)[2:ncol(daily_rates)],variable.name="country",value.name="rate") %>%
  mutate(rate=as.numeric(rate),
         rate=replace(rate,is.nan(rate),NA)) %>%
  group_by(country) %>%
  mutate(rate=zoo::na.locf(rate,na.rm=FALSE))  %>%
  ungroup() %>%
  mutate(date=as.Date(date,format="%Y-%m-%d")) %>%
  drop_na(date)


# exchange rates
daily_exchange <- BISdata::fetch_dataset(dest.dir = paste0(data_folder,"Raw/BIS_data_folder/"),
                                         "https://www.bis.org/statistics/full_xru_d_csv_row.zip")
colnames(daily_exchange) = c("date",unlist(sapply(daily_exchange[1,2:ncol(daily_exchange)],strsplit,split=":"))[seq(from=2,by=2,length.out=81)])
daily_exchange = daily_exchange[-c(1:4),]

daily_exchange = reshape2::melt(daily_exchange,id.vars="date",measure.vars=colnames(daily_exchange)[2:ncol(daily_exchange)],variable.name="country",value.name="exch_rate") %>%
  mutate(exch_rate=as.numeric(exch_rate),
         exch_rate=replace(exch_rate,is.nan(exch_rate),NA)) %>%
  group_by(country) %>%
  mutate(exch_rate=zoo::na.locf(exch_rate,na.rm=FALSE)) %>%
  ungroup() %>%
  mutate(date=as.Date(date,format="%Y-%m-%d")) %>%
  drop_na(date)

policy_exchange_df = full_join(daily_rates,daily_exchange ,by=c("date","country"))

lac_data_daily = policy_exchange_df %>%
  filter(country %in% c("Brazil","Colombia","Chile","Mexico","Peru","United States")) %>%
  mutate(country=as.character(country))

# scaling the charts is very annoying in this code, takes lots of trial and error with the secondary y-axis
myPlot = function(data){
  
  data = data %>%
    mutate(exch_rate=exch_rate/exch_rate[date==min(date,na.rm=TRUE)]*100)
  
  scaleFactor = min(data$exch_rate,na.rm=TRUE)/max(data$rate,na.rm=TRUE)
  Country = data$country
  minyear = year(data$date[1])
  
  
  p = ggplot(data,aes(x=date)) +
    geom_line(aes(y=rate,colour="black"),size=2) +
    geom_line(aes(y=(exch_rate-90)/10,colour="red"),size=2) +
    scale_x_date(date_minor_break="1 month",date_labels="%Y-%m") +
    scale_y_continuous(name=ifelse(minyear==2020,"Policy rate (%)",""),sec.axis=sec_axis(~(.*10)+90,name=ifelse(minyear==2020,"","FX value of US dollar"))) +
    theme(text = element_text(size=30),axis.title.x=element_blank(),axis.text.x = element_text(size=23),axis.text.y = element_text(size=23),axis.title.y=element_text(size=23),plot.caption = element_text(face="italic",hjust = 0,size=23),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))+
    ggtitle(Country) +
    scale_color_manual(breaks=c("black","red"),labels=c("Policy rate","FX value"),values=c("black","red")) +
    theme(legend.position = "none")
  
  p
  
}

# Code below makes Figure 1 ------------------------------------------------------------------------------------------------------------------------------------------
# 2020 to present
plot_data = lac_data_daily %>% filter(date>="2020-01-01") %>% group_split(country) # creates a list of data frames by splitting on the given variable
plot_list_2020 = lapply(plot_data,myPlot)

p1 = add_sub(plot_grid(plotlist=plot_list_2020,
                       ncol=2),"Source: Bank for International Settlements\nNote: Red plots FX value indexed where first shown date equals 100, black plots policy rate.",x=0,hjust=0)

ggsave(plot=p1,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/daily_policy_exchange_rate_2020.png"),
       width=20,height=10,units="in")

# July 2008 to July 2009
plot_data = lac_data_daily %>% filter(date>="2008-07-01"&date<="2009-07-01") %>% group_split(country)
plot_list_2008 = lapply(plot_data,myPlot)

p1 = add_sub(plot_grid(plotlist=plot_list_2008,
                       nrow=2),"Source: Bank for International Settlements\nNote: Red plots FX value indexed where first shown date equals 100, black plots policy rate.",x=0,hjust=0)

ggsave(plot=p1,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/daily_policy_exchange_rate_2008_2009.png"),
       width=20,height=10,units="in")

p2 = add_sub(plot_grid(plotlist=c(rbind(plot_list_2020,plot_list_2008)),
                       ncol=2),"Source: Bank for International Settlements\nNote: Red plots FX value indexed where first shown date equals 100.\nAn increase indicates depreciation of the Latin American currency.",x=0,hjust=0,size=25)

# this is Figure 1
ggsave(plot=ggdraw(p2),filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/daily_policy_exchange_rate_combined_2008_2020.png"),
       width=20,height=30,units="in")

# below code makes Figure 2 (was figure 4 in previous version) -----------------------------------------------------------------------------------
ggplot(monthly_scatter %>% filter(advanced!="Advanced Economies"),aes(x=change_exchange_rate,y=change_policy_rate,colour=advanced)) +
  stat_smooth(method="lm",inherit.aes = FALSE,aes(x=change_exchange_rate,y=change_policy_rate)) +
  geom_point(size=3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept= 0) +
  geom_label_repel(aes(label = country),size=9) +
  scale_x_continuous(labels=scales::percent) +
  stat_poly_eq(formula=y~x,aes(x=change_exchange_rate,y=change_policy_rate,label=paste(..eq.label..,"p-value:",round(..p.value..,2),sep="~~~")),size=10,colour="black",parse=TRUE,label.y="top",label.x="right") +
  labs(y="Percentage point change in policy rate",x="Percent change in exchange rate (local currency per dollar)",colour="Classification",caption="Source: Bank for International Settlements") +
  theme(text = element_text(size=30),axis.text.x = element_text(size=25),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

# time series of GDP, inflation, and policy rate
# This code makes Figure 3 (was figure 11 in previous version) --------------------------------------------------------------------------------------
myPlot_gdp = function(data){
  
  Country = data$country[1]
  
  rate_data = lac_data_daily %>% filter(country==Country&date>="2019-01-01")
  
  if(Country=="Latin America"){ #if a country's id is latin american, create a region average for it for a given point in time
    rate_data = lac_data_daily %>%
      filter(date>="2019-01-01") %>%
      group_by(date) %>%
      summarise(rate=mean(rate,na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(country="Latin America")
  }
  
  scaleFactor = abs(min(data$gdp_output_gap,na.rm=TRUE))
  minyear = year(data$date[1])
  
  fact1 = ifelse(Country=="Brazil"|Country=="Chile"|Country=="Mexico"|Country=="Colombia",-3,0)[1]
  
  p = ggplot() +
    geom_line(data=data,aes(x=date,
                            y=(gdp_output_gap),

                            colour="blue"),size=2) +
    scale_x_date(breaks=as.Date(c("2019-03-01","2020-03-01","2021-03-01","2022-03-01","2023-03-01"),
                                format="%Y-%m-%d"),
                 limits = as.Date(c("2019-03-01","2022-12-30")),
                 labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
    theme(text = element_text(size=30),
          axis.title.x=element_blank(),
          axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=25),
          axis.title.y = element_text(size=25),
          plot.caption = element_text(face="italic", hjust = 0,size=20),
          panel.background = element_rect(fill = "white",colour="black"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"),
          panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey")) +
    ggtitle(Country) +
    scale_color_manual(breaks=c("black","blue"),
                       labels=c("Policy rate","Real GDP"),
                       values=c("black","blue")) +
    theme(legend.position = "none") 
    #geom_hline(yintercept = 0)
  
  #if(Country=="Brazil"|Country=="Mexico"|Country=="Peru"){
    p = p +
      geom_line(data=rate_data,
                aes(x=date,
                    #y=(rate-5)/.166667,
                    y=(rate-10.625)/.375,
                    #y=rate,
                    colour="black"),
                size=2) +
      scale_y_continuous(name="Output gap (%)",
                         #limits(c-60,60),
                         limits=c(-30,10),
                         #sec.axis=sec_axis(~(.*.166667)+5,
                         sec.axis=sec_axis(~(.*(.375)+10.625),
                                  name=ifelse(minyear==2019,"Policy rate (%)\n","Policy rate (%)\n")))
  # }
  # if(Country!="Brazil"&Country!="Mexico"&Country!="Peru"){
  #   p = p +
  #     geom_line(data=rate_data,
  #               aes(x=date,
  #                   #y=(rate-4)/.2, 
  #                   y=(rate-30)/2.666667,
  #                   colour="black"),
  #               size=2) +
  #     scale_y_continuous(name="Output gap (%)",
  #                        #limits=c(-20,20),
  #                        limits=c(0,10),
  #                        #sec.axis=sec_axis(~(.*.2)+4,
  #                        #sec.axis=sec_axis(~(.*2.66667)+30,
  #                                          labels=scales::number_format(accuracy=.01),name=ifelse(minyear==2019,"Policy rate (%)\n","Policy rate (%)\n")))
  # }
  # 
  p
  
}

myPlot_inflation = function(data){
  
  Country = data$country[1]
  
  rate_data = lac_data_daily %>% filter(country==Country&date>="2019-01-01")
  
  if(Country=="Latin America"){
    rate_data = lac_data_daily %>%
      filter(date>="2019-01-01") %>%
      group_by(date) %>%
      summarise(rate=mean(rate,na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(country="Latin America")
  }
  
  scaleFactor = max(data$CPI,na.rm=TRUE)/max(rate_data$rate,na.rm=TRUE)
  minyear = year(data$date[1])
  
  
  p = ggplot() +
    geom_line(data=rate_data,aes(x=date,
                                 y=rate,
                                 colour="black"),size=2) +
    geom_line(data=data,aes(x=date,
                            y=CPI,
                            colour="red"),size=2) +
    geom_line(data=data,aes(x=date,
                            y=core_inflation,
                            colour="purple"),size=2) +
    scale_x_date(breaks=as.Date(c("2019-03-01","2020-03-01","2021-03-01","2022-03-01","2023-03-01"),
                                format="%Y-%m-%d"),
                 limits = as.Date(c("2019-03-01","2022-12-30")),
                 labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
    scale_y_continuous(name="",
                       #labels=scales::number_format(accuracy=ifelse(Country=="Brazil"|Country=="Mexico"|Country=="Peru",.1,.01)[1]),
                       #limits=c(0,ifelse(Country=="Brazil",10.75,ifelse(Country=="Mexico"|Country=="Peru",10,8))),
                       limits=c(0,15),
                       sec.axis=sec_axis(~.,name=ifelse(minyear==2019,"12-month inflation rate (%)","12-month inflation rate (%)"))) +
    theme(text = element_text(size=30),
          axis.title.x=element_blank(),
          axis.text.x = element_text(size=25),
          axis.text.y = element_text(size=25),
          axis.title.y=element_text(size=25),
          plot.caption = element_text(face="italic",hjust = 0,size=20),
          panel.background = element_rect(fill = "white",colour="black"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
          panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))+
    ggtitle(Country) +
    geom_hline(yintercept=0) +
    scale_color_manual(breaks=c("black","red","purple"),
                       labels=c("Policy rate","Total Inflation","Core Inflation"),
                       values=c("black","red","purple")) +
    theme(legend.position = "none") 

  p
  
}


# pandemic tables
panel_df = panel_df %>%
  mutate(pandemic=ifelse(date>="2020-03-01",1,0), 
         pandemic_total_inf = pandemic*CPI,
         pandemic_core_inf = pandemic*core_inflation,
         pandemic_lag_rate = pandemic*lag_rate,
         pandemic_output_gap = pandemic*gdp_output_gap)

# predict third quarter GDP for those who don't have it in CEIC yet
# update to Q4 2021
countries_missing_gdp = c(panel_df %>% filter(is.na(gdp_index)&date=="2021-12-01") %>% distinct(country))$country

panel_df_prediction_data = panel_df %>%
  group_by(country) %>%
  arrange(date) %>%
  mutate(qoq_change_gdp = gdp_index/dplyr::lag(gdp_index)-1,
         qoq_change_ip = ip_index/dplyr::lag(ip_index)-1) %>%
  ungroup()


panel_df_prediction_data = panel_df_prediction_data %>%
  group_by(country) %>%
  mutate(pandemic_total_inf = pandemic*CPI,
         pandemic_core_inf = pandemic*core_inflation,
         pandemic_lag_rate = pandemic*lag_rate,
         pandemic_output_gap = pandemic*gdp_output_gap)


# regression in levels, 2007-2019, time and country FE, quarterly percent change in nominal effective exchange rate
neer_quarter = neer %>%
  mutate(quarter = lubridate::quarter(date),
         year = lubridate::year(date)) %>%
  group_by(country,year,quarter) %>%
  summarise(neer_index = mean(value,na.rm=TRUE)) %>%
  group_by(country) %>%
  mutate(neer_quarter_perc_change = (neer_index/dplyr::lag(neer_index,1)-1)*100,
         lag_neer_perc_change = dplyr::lag(neer_quarter_perc_change,1)) %>%
  ungroup()

reer_quarter = reer %>%
  mutate(quarter = lubridate::quarter(date),
         year = lubridate::year(date)) %>%
  group_by(country,year,quarter) %>%
  summarise(reer_index = mean(value,na.rm=TRUE)) %>%
  group_by(country) %>%
  mutate(reer_quarter_perc_change = (reer_index/dplyr::lag(reer_index,1)-1)*100,
         lag_reer_perc_change = dplyr::lag(reer_quarter_perc_change,1)) %>%
  ungroup()

panel_df_prediction_data = left_join(panel_df_prediction_data,neer_quarter,by=c("country","year","quarter"))
panel_df_prediction_data = left_join(panel_df_prediction_data,reer_quarter,by=c("country","year","quarter"))


# this makes figure 3
plot_data = panel_df_prediction_data %>% filter(date>="2019-01-01"&date<="2022-12-01"&country%in%c("Brazil","Chile","Peru","Mexico","Colombia")) %>% group_split(country)
plot_list_gdp = lapply(plot_data,myPlot_gdp)

plot_data = panel_df_monthly %>% filter(date>="2019-01-01"&country%in%c("Brazil","Chile","Peru","Mexico","Colombia")) %>% group_split(country)
plot_list_inflation = lapply(plot_data,myPlot_inflation)

p2 = add_sub(plot_grid(plotlist=c(rbind(plot_list_gdp, plot_list_inflation)),
                       ncol=2),"Source: Bank for International Settlements, CEIC",x=0,hjust=0,size=25)

ggsave(plot=ggdraw(p2),filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/fig6_daily_policy_inflation_gdp_20230425.png"),
       width=20,height=30,units="in")

# figure 4 (was figure 6 in previous verserion): chart showing output gap and policy rate during the pandemic ------------------------------------
df_scatter_quarterly = panel_df %>%
  filter(date>="2019-12-01") %>%
  drop_na(rate,core_inflation) %>%
  group_by(country) %>%
  summarise(total_cpi_dec_2020_q3 = (CPI[n()]-CPI[date=="2020-12-01"]),
            core_cpi_dec_2020_q3 = (core_inflation[n()]-core_inflation[date=="2020-12-01"]),
            gdp_output_gap_dec_2020_q3 = (gdp_output_gap[date=="2020-06-01"]-gdp_output_gap[date=="2019-12-01"]),
            policy_rate_dec_2020_q3 = (rate[n()]-rate[date=="2020-12-01"]),
            policy_rate_dec_2019_q2_2020 = (rate[date=="2020-06-01"]-rate[date=="2019-12-01"]),
            advanced=advanced[1]) %>%
  mutate(advanced=ifelse(advanced!="Non-LA EMEs","Advanced Economies",advanced),
         advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced),
         policy_rate_dec_2020_q3 = ifelse(is.infinite(policy_rate_dec_2020_q3),NA,policy_rate_dec_2020_q3),
         policy_rate_dec_2020_q3 = ifelse(is.nan(policy_rate_dec_2020_q3),0,policy_rate_dec_2020_q3),
         policy_rate_dec_2019_q2_2020 = ifelse(is.infinite(policy_rate_dec_2019_q2_2020),NA,policy_rate_dec_2019_q2_2020),
         policy_rate_dec_2019_q2_2020 = ifelse(is.nan(policy_rate_dec_2019_q2_2020),0,policy_rate_dec_2019_q2_2020)) %>%
  ungroup()

df_scatter_quarterly$advanced = factor(df_scatter_quarterly$advanced,levels=c("Advanced Economies","Non-LA EMEs","Latin America"))

# Below makes Figure 4
ggplot(df_scatter_quarterly %>% filter(country!="Philippines"&advanced!="Advanced Economies"),aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020,colour=advanced)) +
  stat_smooth(method="lm",inherit.aes=FALSE,aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020)) +
  geom_point(size=3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept= 0) +
  geom_label_repel(aes(label = country),size=9) +
  #scale_y_continuous(labels=scales::percent) +
  stat_poly_eq(formula=y~x,aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020,color=NA,label=paste(..eq.label..,"p-value:",round(..p.value..,2),sep="~~~")),size=9,color="black",parse=TRUE,label.x="right") +
  labs(y="Percentage point change in policy rate",x="Percentage point change in GDP output gap",colour="Classification",caption="Source: Bank for International Settlements; CEIC") +
  theme(text = element_text(size=30),axis.text.x = element_text(size=25),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

# This is the code for figure 5 (was figure 12 in previous version) ------------------------------------------------------------------------
# current inflation charts
df_scatter_monthly = panel_df_monthly %>%
  filter(date>="2019-12-01") %>%
  group_by(country) %>%
  summarise(total_cpi_change = (dplyr::last(na.omit(CPI))-CPI[date=="2021-06-01"]),
            core_cpi_change = (dplyr::last(na.omit(core_inflation))-core_inflation[date=="2021-06-01"]),
            policy_rate_pp_change = (dplyr::last(na.omit(rate))-rate[date=="2021-06-01"]),
            policy_rate_prop_change = (dplyr::last(na.omit(rate))-rate[date=="2021-06-01"])/abs(rate[date=="2021-06-01"]),
            advanced=advanced[1]) %>%
  mutate(advanced=ifelse(advanced!="Non-LA EMEs","Advanced Economies",advanced),
         advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced),
         policy_rate_prop_change = ifelse(is.infinite(policy_rate_prop_change),NA,policy_rate_prop_change),
         policy_rate_prop_change = ifelse(is.nan(policy_rate_prop_change),0,policy_rate_prop_change)) %>%
  ungroup()

df_scatter_monthly$advanced = factor(df_scatter_monthly$advanced,levels=c("Non-LA EMEs","Advanced Economies","Latin America"))

# Below makes Figure 5 (but you may want to remove Turkey)
fig12 <- ggplot(df_scatter_monthly %>% filter(advanced!="Advanced Economies"&country!="Turkey"),aes(x=total_cpi_change,y=policy_rate_pp_change,colour=advanced)) +
  stat_smooth(method="lm",inherit.aes=FALSE,aes(x=total_cpi_change,y=policy_rate_pp_change)) +
  geom_point(size=3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept= 0) +
  geom_label_repel(aes(label = country),size=9) +
  stat_poly_eq(formula=y~x,aes(x=total_cpi_change,y=policy_rate_pp_change,color=NA,label=paste(..eq.label..,"p-value:",round(..p.value..,2),sep="~~~")),size=9,color="black",parse=TRUE) +
  labs(y="Percentage point change in policy rate (June 2022 - January 2021)",
       x="Percentage point change in 12-month inflation (June 2022 - January 2021)",
       colour="Classification",caption="Source: Bank for International Settlements; CEIC") +
  theme(text = element_text(size=20),axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title.y=element_text(size=15),
        plot.caption = element_text(face="italic",hjust = 0,size=20),
        panel.background = element_rect(fill = "white",colour="black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

ggsave(fig12,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/fig12_12m_change_inflation_and_pr.png"),
       width=15,height=8,units="in")
