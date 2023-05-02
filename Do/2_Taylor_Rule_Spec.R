# 3_Taylor_Rule_Spec.R
# Beatrice Lee
# Goal: Do-file for clean code in making taylor rules and plotting, based on do-file #2
#       Use quaterly data, and IP and core inflation where possible
# Date Created: 2021-10-05
# Last Updated: 2023-04-11 // bal

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
library(forecast)
source(paste0(do_folder,"Mode1.R"))

#load(paste0(data_folder,"Processing/1_Inflation_and_Rates.RData"))

# load in exchange rate data

# neer = nominal effective exchange rate
neer_daily <- BISdata::fetch_dataset(dest.dir = paste0(data_folder,"Raw/BIS_data_folder/"),
                           "https://www.bis.org/statistics/full_eer_d_csv_row.zip") # writes zip file into a destination directory

httr::GET("https://www.bis.org/statistics/eer/broad.xlsx",httr::write_disk(tf <- tempfile(fileext=".xlsx"))) # accesses file and writes it to a temp object
neer = read_excel(tf)[-c(1:2),]
write.csv(neer,paste0(data_folder,"Raw/BIS_data_folder/neer_data_",gsub("-","",Sys.Date()),".csv"))
colnames(neer)=c("date",neer[1,2:ncol(neer)])
neer = neer[-c(1:2),]
neer = neer %>%
  mutate(date=as.Date(as.numeric(date),format="%Y-%m-%d",origin="1899-12-30"),
         date=as.Date(paste0(substr(date,1,8),"01"),format="%Y-%m-%d")) %>%
  mutate_at(vars(`Algeria`:`United States`),as.numeric)
neer = reshape2::melt(neer,id.vars="date",measure.vars=colnames(neer)[2:ncol(neer)],variable.name="country",value.name="value") %>%
  mutate(type="NEER")
neer$country = str_replace_all(neer$country,setNames(c("Hong Kong","Taiwan","South Korea"),c("Hong Kong SAR","Chinese Taipei","Korea")))
setdiff(df1$country,neer$country)
# No Serbia and North Macedonia

# add 12-month % change
neer = neer %>%
  group_by(country) %>%
  mutate(neer_12_month_change = (value/dplyr::lag(value,12)-1)*100)

# reer = real effective exchange rate (also from bis)
httr::GET("https://www.bis.org/statistics/eer/broad.xlsx",httr::write_disk(tf <- tempfile(fileext=".xlsx")))
reer = read_excel(tf,sheet="Real")[-c(1:2),]
write.csv(reer,paste0(data_folder,"Raw/BIS_data_folder/reer_data_",gsub("-","",Sys.Date()),".csv"))
colnames(reer)=c("date",reer[1,2:ncol(reer)])
reer = reer[-c(1:2),]
reer = reer %>%
  mutate(date=as.Date(as.numeric(date),format="%Y-%m-%d",origin="1899-12-30"),
         date=as.Date(paste0(substr(date,1,8),"01"),format="%Y-%m-%d")) %>%
  mutate_at(vars(`Algeria`:`United States`),as.numeric)
reer = reshape2::melt(reer,id.vars="date",measure.vars=colnames(reer)[2:ncol(reer)],variable.name="country",value.name="value") %>%
  mutate(type="REER")
reer$country = str_replace_all(reer$country,setNames(c("Hong Kong","Taiwan","South Korea"),c("Hong Kong SAR","Chinese Taipei","Korea")))
setdiff(df1$country,reer$country)
# No Serbia and North Macedonia

# add 12-month % change
reer = reer %>%
  group_by(country) %>%
  mutate(reer_12_month_change = (value/dplyr::lag(value,12)-1)*100)

# load in industrial production data (from CEIC)
ip = read_excel(paste0(data_folder,"Raw/Industrial Production_04122023.xlsx"))
colnames(ip) = c("date",ip[1,2:ncol(ip)])
ip = ip[-c(1:26),]
ip = ip %>%
  mutate(date=as.Date(as.numeric(date),format="%Y-%m-%d",origin="1899-12-31"),
         date=as.Date(paste0(substr(date,1,8),"01"),format="%Y-%m-%d"))
ip = reshape2::melt(ip,id.vars="date",measure.vars=colnames(ip)[2:ncol(ip)],variable.name="country",value.name="value") %>%
  mutate(type="IP") %>%
  mutate(value=as.numeric(value))

# apply the HP filter to get trend, also put them all in the same base (make January 2020 the base month)
ip = ip %>%
  group_by(country) %>%
  mutate(value_rebase = (value/value[date=="2020-01-01"])*100) %>%
  ungroup()

ip_dropna = ip %>%
  filter(!is.na(value_rebase)) %>%
  group_by(country) %>%
  arrange(date) %>%
  mutate(ip_trend = mFilter::hpfilter(value_rebase,type="lambda",freq=16000)$trend) # typical freq parameter to use is 16000, but Steve wanted to use higher to get a more smoothed line

# we may want to eventually apply the process we use in the inflation paper to get a more accurate output gap

ggplot(ip_dropna %>% filter(country=="United States")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=ip_trend),colour="blue",size=2)
# does the graph look okay? the trend line shouldnt map *too* closely to the actual line


ip = left_join(ip,ip_dropna %>% select(date,country,ip_trend),by=c("date","country")) %>%
  mutate(country=as.character(country))

ggplot(ip_dropna %>% filter(country=="United States")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=ip_trend),colour="blue",size=2)
ggplot(ip_dropna %>% filter(country=="Israel")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=ip_trend),colour="blue",size=2)
ggplot(ip_dropna %>% filter(country=="Brazil")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=ip_trend),colour="blue",size=2)
ggplot(ip_dropna %>% filter(country=="India")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=ip_trend),colour="blue",size=2)
ggplot(ip_dropna %>% filter(country=="Serbia")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=ip_trend),colour="blue",size=2)

ip$country = str_replace_all(ip$country,setNames(c("Euro area","Russia"),c("European Union","Russian Federation")))

# read in the inflation targeting data (updated 4/11/2023 to include 2022 and 2023)
targets = read_excel(paste0(data_folder,"Raw/Inflation_Targets_20230411.xlsx"),sheet = "Sheet2")
targets = reshape2::melt(targets,id.vars="Country",measure.vars=colnames(targets)[2:ncol(targets)],variable.name="year",value.name="inflation_target") %>%
  rename(country=Country) %>%
  mutate(year=as.numeric(as.character(year)),
         inflation_target=as.numeric(inflation_target))

# get core inflation rates by country (some of these were manually updated)
core_inflation = read_excel(paste0(data_folder,"Raw/core_inflation_04142023.xlsx"))
colnames(core_inflation) = c("date",core_inflation[1,2:ncol(core_inflation)])

core_inflation_monthly = read_excel(paste0(data_folder,"Raw/core_inflation_monthly_04102023.xlsx"))
colnames(core_inflation_monthly) = c("date",core_inflation_monthly[1,2:ncol(core_inflation_monthly)])

core_inflation = core_inflation[-c(1:26),]
core_inflation = core_inflation %>%
  mutate(date=as.Date(as.numeric(date),format="%Y-%m-%d",origin="1899-12-30"),
         date=as.Date(paste0(substr(date,1,8),"01"),format="%Y-%m-%d"))

core_inflation_monthly = core_inflation_monthly[-c(1:26),]
core_inflation_monthly = core_inflation_monthly %>%
  mutate(date=as.Date(as.numeric(date),format="%Y-%m-%d",origin="1899-12-31"),
         date=as.Date(paste0(substr(date,1,8),"01"),format="%Y-%m-%d"))

core_inflation = reshape2::melt(core_inflation,id.vars="date",measure.vars=colnames(core_inflation)[2:ncol(core_inflation)],variable.name="country",value.name="core_inflation") %>%
  mutate(core_inflation=as.numeric(core_inflation),
         country=as.character(country),
         date=ifelse(date=="2021-12-01",as.Date("2021-11-01",format="%Y-%m-%d"),as.Date(date,format="%Y-%m-%d")),
         date=as.Date(date,format="%Y-%m-%d",origin="1970-01-01"))

core_inflation_monthly = reshape2::melt(core_inflation_monthly,id.vars="date",measure.vars=colnames(core_inflation_monthly)[2:ncol(core_inflation_monthly)],variable.name="country",value.name="core_inflation") %>%
  mutate(core_inflation=as.numeric(core_inflation),
         country=as.character(country))

core_inflation_monthly = core_inflation_monthly %>%
  group_by(country,quarter(date),year(date)) %>%
  mutate(core_inflation=ifelse(country%in%c("New Zealand","Australia"),Mode1(core_inflation,na.rm=TRUE),core_inflation)) %>%
  ungroup() %>%
  select(-c(`quarter(date)`,`year(date)`))

# construct dataset, joining policy rate, inflation, exchange rates, IP, and target rate
df2 = df1 %>%
  select(-avg) %>%
  pivot_wider(values_from=value,id_cols=c(date,country),names_from=type)%>%
  mutate(advanced=ifelse(country%in%emes,"Emerging Market Economy","Advanced Economy"),
         advanced1 = ifelse(country%in%full_countries,"Emerging Market Economy","Advanced Economy"),
         year=year(date))

# strange thing going on with Taiwan, but it is okay because we end up excluding them

monthly_flag = TRUE # set to false if we do not want to deal with monthly data. Otherwise, keep as TRUE
if(monthly_flag==TRUE){
  panel_df_monthly = left_join(df2,targets,by=c("country","year")) %>%
    left_join(core_inflation_monthly,by=c("date","country"))

    panel_df_monthly = left_join(panel_df_monthly,ip %>% select(-c(value,type)) %>% rename(ip_index = value_rebase),by=c("country","date")) %>%
    mutate(inflation_gap = (CPI*100)-inflation_target,
           output_gap = ip_index/ip_trend*100-100,
           rate=rate*100) %>%
    group_by(country) %>%
    mutate(CPI=CPI*100,
           lag_rate=dplyr::lag(rate,1),
           lag_output_gap=dplyr::lag(output_gap,1),
           lag_inflation_gap=dplyr::lag(CPI,1)-inflation_target,
           lag_CPI=dplyr::lag(CPI,1), # %>% filter(year>=2011) %>% # make 2011 the beginning year for the analysis
           time_trend=order(date))

    panel_df_monthly = left_join(panel_df_monthly,neer %>% select(-type) %>% rename(neer = value),by=c("country","date"))

    panel_df_monthly = left_join(panel_df_monthly,panel_df_monthly %>% ungroup() %>% filter(country=="United States") %>% select(date,rate) %>% rename(US_rate=rate),by=c("date"))

}

panel_df = left_join(df2,targets,by=c("country","year")) %>%
  mutate(quarter=quarter(date)) %>%
  left_join(core_inflation,by=c("date","country")) %>%
  group_by(country,quarter,year) %>%
  arrange(date) %>%
  mutate(rate=dplyr::last(na.omit(rate)),
         CPI=mean(CPI,na.rm=TRUE),
         CPI=ifelse(is.nan(CPI),NA,CPI),
         inflation_target=inflation_target[n()],
         core_inflation = mean(core_inflation,na.rm=TRUE),
         core_inflation = ifelse(is.nan(core_inflation),NA,core_inflation)) %>%
  ungroup() %>%
  arrange(date)

panel_df = left_join(panel_df,ip %>% select(-c(value,type)) %>% rename(ip_index = value_rebase),by=c("country","date")) %>%
  group_by(country,year,quarter) %>%
  summarise(date=date[n()],
            CPI=CPI[n()],
            core_inflation=core_inflation[n()],
            rate=rate[n()],
            advanced=advanced[n()],
            advanced1=advanced1[n()],
            inflation_target=inflation_target[n()],
            ip_index=mean(ip_index,na.rm=TRUE),
            ip_trend=mean(ip_trend,na.rm=TRUE),
            ip_index=ifelse(is.nan(ip_index),NA,ip_index),
            ip_trend=ifelse(is.nan(ip_trend),NA,ip_trend)) %>%
  ungroup() %>%
  mutate(inflation_gap = (CPI*100)-inflation_target,
         output_gap = ip_index/ip_trend*100-100,
         rate=rate*100) %>%
  group_by(country) %>%
  mutate(CPI=CPI*100,
         lag_rate=dplyr::lag(rate,1),
         lag_output_gap=dplyr::lag(output_gap,1),
         lag_inflation_gap=dplyr::lag(CPI,1)-inflation_target,
         lag_CPI=dplyr::lag(CPI,1)) %>%
  #filter(year>=2011) %>% # make 2011 the beginning year for the analysis
  mutate(time_trend=order(date))

panel_df = left_join(panel_df,neer %>% select(-type) %>% rename(neer = value),by=c("country","date"))

panel_df = left_join(panel_df,panel_df %>% ungroup() %>% filter(country=="United States") %>% select(date,rate) %>% rename(US_rate=rate),by=c("date"))

# join in with quarterly GDP 
gdp = read_excel(paste0(data_folder,"Raw/qtr_real_gdp_ceic_20230322.xlsx"))
colnames(gdp) = c("date",gdp[1,2:ncol(gdp)])
gdp = gdp[-c(1:26),] 
gdp = gdp %>%
  mutate(date=as.Date(as.numeric(date),format="%Y-%m-%d",origin="1899-12-31"),
         date=as.Date(paste0(substr(date,1,8),"01"),format="%Y-%m-%d"))
gdp = reshape2::melt(gdp,id.vars="date",measure.vars=colnames(gdp)[2:ncol(gdp)],variable.name="country",value.name="value") %>%
  mutate(type="gdp") %>%
  mutate(value=as.numeric(value),
         country=as.character(country))

# augment with Colombia GDP from 1994 to 2004 Q4
colombia_gdp = readxl::read_excel(paste0(data_folder,"Raw/colombia_real_gdp_1994.xlsx")) %>%
  mutate(date=as.Date(date,format="%Y-%m-%d")) %>%
  mutate(change=gdp/lead(gdp,1))
colombia_gdp = left_join(colombia_gdp,gdp %>% filter(country=="Colombia"),by="date") %>%
  arrange(desc(date))

# if 2005 or later, use the value we get from ceic. Otherwise use the gdp growth series we found to backwards induce the correct series
colombia_gdp$value1=NA
for(i in 1:nrow(colombia_gdp)){
  if(colombia_gdp$date[i]>="2005-03-01"){
    colombia_gdp$value1[i] = colombia_gdp$value[i]
  }
  else{
    colombia_gdp$value1[i]=colombia_gdp$value1[i-1]*colombia_gdp$change[i]
  }
}
colombia_gdp = colombia_gdp %>%
  select(date,country,value1,type) %>%
  rename(value=value1)

gdp = bind_rows(gdp %>% filter(!(country=="Colombia"&date>="1994-03-01"&date<"2005-03-01")),colombia_gdp %>% filter(date<"2005-03-01")) %>%
  arrange(country,date) 

## 8/18/22: pull in China quarterly real gdp.
##  Source: Federal Reserve Bank of Atlanta: https://www.atlantafed.org/cqer/research/china-macroeconomy?panel=3
china_gdp = readxl::read_excel(paste0(data_folder,"Raw/china_outdata2206_hz_monthly.xlsx"), sheet = "MonthlyData")
china_gdp1 = china_gdp %>%
  mutate(date = format(round(Dates, 3), nsmall =3),
         date = str_replace_all(date, c("\\.000" = "-03-01",
                                        "\\.250" = "-06-01",
                                        "\\.500" = "-09-01",
                                        "\\.750" = "-12-01"))) %>%
  filter(str_detect(date, "-")) %>%
  mutate(date = as.Date(date, "%Y-%m-%d"),
         country = "China",
         type = "gdp",
         value = RealGDP) %>%
  select(date,value,country,type) 

# now bind this to the main gdp dataset
gdp = gdp %>%
  filter(country!= "China") %>%
  bind_rows(china_gdp1) %>%
  arrange(country,date) 

# apply the HP filter to get trend, also put them all in the same base (make Q1 2020 the base month)
#the trend gdp is used to calculate the output gap, which is used in the taylor rule regressions
gdp = gdp %>%
  group_by(country) %>%
  mutate(value_rebase = (value/value[date=="2020-03-01"])*100) %>% #this is the version of gdp that you'll be using going forward
  ungroup()

pdf(file=paste0(charts_folder,"Scatterplots/2022-7-25_NewTrendCorrection/new_ARIMA_forecast_hp_filter_20230411.pdf"))
gdp_forecast_filter = data.frame()
for(a in unique(gdp$country)){ # for every country in the gdp dataset

  gdp_a = gdp %>% #create a new dataset called gdp_[countryname]
    filter(!is.na(value_rebase)&country==a&date<="2019-12-31") %>% #for that country, keep only not missing rebased gdp #s from 2019 Q4 onward 
    mutate(gdp_diff = value_rebase-dplyr::lag(value_rebase,1)) #take the diff btw the rebased gdp and the lagged gdp 
  gdp_ts = ts(gdp_a %>% select(value_rebase),frequency=4,start=c(year(min(gdp_a$date,na.rm=TRUE)),quarter(min(gdp_a$date,na.rm=TRUE)))) #make a ts object for each country

  fit = forecast::auto.arima(gdp_ts,seasonal=FALSE) #use an arima filter to fit an estimation? based on the rebased gdp at a given point in time
  gdp_forecast = forecast::forecast(fit,h=12) #then make a projection using this fit
  print(forecast::autoplot(forecast::forecast(fit,h=12)) + xlim(max(min(year(gdp_a$date)),2000),2022) +ylim(min(gdp$value_rebase[gdp$country==a&gdp$date>="2000-01-01"],na.rm=TRUE),max(gdp$value_rebase[gdp$country==a&gdp$date>="2000-01-01"],na.rm=TRUE)+5) +labs(caption=a))
  # forecasts GDP using pre-pandemic trend

  # as we get more data, move end date forward
  vec = data.frame(date=unique(gdp$date[gdp$date>"2019-12-31"&gdp$date<"2022-12-31"]),
                   forecast_gdp = gdp_forecast$mean[1:length(unique(gdp$date[gdp$date>"2019-12-31"&gdp$date<"2022-12-31"&gdp$country==a]))],
                   country=rep(a,length(unique(gdp$date[gdp$date>"2019-12-31"&gdp$date<"2022-12-31"&gdp$country==a]))))

  print(ggplot() +
    geom_line(data=vec,aes(x=date,y=forecast_gdp,colour="Forecast"),size=2) +
    geom_line(data=gdp_a %>% filter(date>"1999-12-31"),aes(x=date,y=value_rebase,colour="Actual"),size=2) +
    geom_line(data=gdp %>% filter(date>"1999-12-31"&country==a),aes(x=date,y=value_rebase,colour="Actual"),size=2) +
    scale_colour_manual(breaks=c("Actual","Forecast"),values=c("blue","green")) +
      labs(caption=a)
  )

  gdp_a = gdp_a %>%
    mutate(forecast_gdp = value_rebase) %>%
    select(date,country,forecast_gdp)
  gdp_a = bind_rows(gdp_a,vec)

  gdp_a = gdp_a %>%
    arrange(date) %>%
    mutate(gdp_trend = mFilter::hpfilter(forecast_gdp,type="lambda",freq=1600)$trend)

  print(
  ggplot() +
    geom_line(data=gdp_a,aes(x=date,y=gdp_trend),colour="red",size=2) +
    geom_line(data=gdp %>% filter(country==a&date>=gdp_a$date[1]),aes(x=date,y=value_rebase),colour="blue",size=2) +
    labs(caption=a,y="Actual and Trend GDP (Q1 2020 = 100)",x="") +
    theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
  )

  gdp_forecast_filter = bind_rows(gdp_forecast_filter,gdp_a)

}

dev.off()
gdp_forecast_filter = gdp_forecast_filter %>%
  mutate(gdp_trend=as.numeric(gdp_trend))


gdp = left_join(gdp,gdp_forecast_filter %>% select(date,country,gdp_trend),by=c("date","country")) %>%
  mutate(country=as.character(country))
ggplot(gdp %>% filter(country=="United States")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=gdp_trend),colour="blue",size=2)
ggplot(gdp %>% filter(country=="Israel")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=gdp_trend),colour="blue",size=2)
ggplot(gdp %>% filter(country=="Brazil")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=gdp_trend),colour="blue",size=2)
ggplot(gdp %>% filter(country=="India")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=gdp_trend),colour="blue",size=2)
ggplot(gdp %>% filter(country=="Serbia")) + geom_line(aes(x=date,y=value_rebase),colour="red",size=2) + geom_line(aes(x=date,y=gdp_trend),colour="blue",size=2)

gdp$country = str_replace_all(gdp$country,setNames(c("Euro area","Russia","Hong Kong"),c("Euro Area","Russian Federation","Hong Kong SAR \\(China\\)")))

gdp = as.data.frame(gdp) %>%
  mutate(gdp_trend=as.numeric(gdp_trend))

panel_df = left_join(panel_df,gdp %>% select(-c(value,type)) %>% rename(gdp_index=value_rebase),by=c("date","country")) %>%
  mutate(gdp_output_gap=(gdp_index/gdp_trend-1)*100)

panel_df = panel_df %>%
  rename(ip_output_gap=output_gap)

# get rid of the countries we do not want (those that do not target inflation or dont have good data)
## 03.28.2023 - TEMP REMOVE SINGAPORE AND TAIWAN FROM THIS LIST AS WE NEED THEM FOR THE KENYON PRESENTATION
panel_df = panel_df %>%
  filter(!(country%in%c("Argentina","Croatia","Hong Kong","North Macedonia","Saudi Arabia", "Singapore","Taiwan"))) 

if(monthly_flag==TRUE){
  panel_df_monthly = panel_df_monthly %>%
    filter(!(country%in%c("Argentina","Croatia","Hong Kong","North Macedonia","Saudi Arabia","Singapore","Taiwan"))) ## 03.28.2023 - TEMP REMOVE SINGAPORE AND TAIWAN FROM THIS LIST AS WE NEED THEM FOR THE KENYON PRESENTATION


  panel_df_monthly = panel_df_monthly %>%
    mutate(inflation_variable="core_inflation",
           inflation_variable=ifelse(country%in%c("Hong Kong","Saudi Arabia","Malaysia","India"),"CPI",inflation_variable), # these are the only two with core inflation totally missing
           output_variable="output_gap") # GDP should be available for everyone

  panel_df_monthly = panel_df_monthly %>%
    group_by(country) %>%
    rowwise() %>%
    mutate(check1 = ifelse(date>=min(panel_df_monthly$date[panel_df_monthly$country==country&!is.na(panel_df_monthly$rate)]),1,0),
           check2 = ifelse(date<=max(panel_df_monthly$date[panel_df_monthly$country==country&!is.na(panel_df_monthly$rate)]),1,0)) %>%
    ungroup() %>%
    mutate(rate=ifelse(check1==1&check2==1&is.na(rate),0,rate)) %>%
    group_by(country) %>%
    mutate(lag_rate=dplyr::lag(rate,1)) %>%
    ungroup()
  panel_df_monthly$rate[panel_df_monthly$country=="Philippines"&panel_df_monthly$date%in%as.Date(c("1988-01-01","1989-08-01","1989-12-01","1990-01-01","1990-02-01","1990-04-01","1990-05-01","1990-10-01","1990-11-01","1992-02-01"),format="%Y-%m-%d")] = NA # fix one small error that should stay NA

  write.csv(panel_df_monthly,paste0(data_folder,"Final/panel_data_monthly_20230411.csv"))
}

# make variables that tell which inflation and output gap variable to use (examine data limitations; countries use total CPI if core does not have enough time before the pandemic)
panel_df = panel_df %>%
  mutate(inflation_variable="core_inflation",
         inflation_variable=ifelse(country%in%c("Hong Kong","Saudi Arabia","Malaysia","India"),"CPI",inflation_variable), # these are the only with core inflation totally missing
         output_variable="gdp_output_gap") # GDP should be available for everyone through 2022 Q1


# fix NA where there shouldnt be (got rid of when values == 0)
panel_df = panel_df %>%
  group_by(country) %>%
  rowwise() %>%
  mutate(check1 = ifelse(date>=min(panel_df$date[panel_df$country==country&!is.na(panel_df$rate)]),1,0),
         check2 = ifelse(date<=max(panel_df$date[panel_df$country==country&!is.na(panel_df$rate)]),1,0)) %>%
  ungroup() %>%
  mutate(rate=ifelse(check1==1&check2==1&is.na(rate),0,rate)) %>%
  group_by(country) %>%
  mutate(lag_rate=dplyr::lag(rate,1)) %>%
  ungroup()
panel_df$rate[panel_df$country=="Philippines"&panel_df$date=="1989-12-01"] = NA # fix one small error that should stay NA

# create ZLB (zero lower bound) dummy
panel_df = panel_df %>%
  mutate(ZLB=ifelse(lag_rate<=.5,1,0))

write.csv(panel_df,paste0(data_folder,"Final/quarterly_panel_df_20230411.csv"))

# create table where rows are coefficients from simple taylor rule regression

# first round is simplest form of taylor rule
# first_round_taylor_regressions = data.frame()
# for(a in unique(panel_df$country)){
#
#   form = as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate"))
#
#   data_filtered = panel_df %>%
#     filter(country==a&date>="2011-01-01"&date<="2019-12-01") %>%
#     drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]))
#
#   reg = lm_robust(form,data=data_filtered)
#
#   b = tidy(reg)
#
#   first_round_taylor_regressions = rbind(first_round_taylor_regressions,c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],reg$nobs,reg$r.squared,a,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]),as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(first_round_taylor_regressions) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","num_obs","r_squared","country","inflation_variable","output_gap_variable","start_date","end_date")
#
#   write.csv(first_round_taylor_regressions,paste0(results_folder,"first_round_taylor_results_2011_2019.csv"))
# }
#
# # second round adds time trend
# second_round_taylor_regressions = data.frame()
# for(a in unique(panel_df$country)){
#
#   form= as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate+time_trend"))
#
#   data_filtered = panel_df %>%
#     filter(country==a&date>="1970-01-01"&date<="2019-12-01") %>%
#     drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),CPI,Mode1(panel_df$output_variable[panel_df$country==a]))
#
#   reg = lm_robust(form,data=data_filtered)
#   AIC_core = AIC(lm(form,data=data_filtered))
#
#   b = tidy(reg)
#
#   form= as.formula(paste0("rate~","CPI","+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate+time_trend"))
#
#   reg = lm_robust(form,data=data_filtered)
#   AIC_total = AIC(lm(form,data=data_filtered))
#
#   c = tidy(reg)
#
#   second_round_taylor_regressions = rbind(second_round_taylor_regressions,c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],b[5,2],b[5,4],c[2,2],c[2,4],c[3,2],c[3,4],c[4,2],c[4,4],c[5,2],c[5,4],AIC_core,AIC_total,reg$nobs,reg$r.squared,a,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]),as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(second_round_taylor_regressions) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","time_trend_coef","time_trend_tstat","total_cpi_inflation_coef","total_cpi_inflation_tstat","total_cpi_output_gap_coef","total_cpi_output_gap_tstat","total_cpi_lag_rate_coef","total_cpi_lag_rate_tstat","total_cpi_time_trend_coef","total_cpi_time_trend_tstat","AIC_core","AIC_total","num_obs","r_squared","country","inflation_variable","output_gap_variable","start_date","end_date")
#
#   write.csv(second_round_taylor_regressions,paste0(results_folder,"second_round_taylor_results_1970_2019.csv"))
# }
#
# # this model adds an interaction term for if the CB is at the zero lower bound
# second_round_taylor_regressions_zlb = data.frame()
# for(a in unique(panel_df$country)){
#
#   form= as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$inflation_variable[panel_df$country==a]),"*ZLB","+",Mode1(panel_df$output_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"*ZLB","+lag_rate+time_trend-ZLB"))
#
#   data_filtered = panel_df %>%
#     filter(country==a&date>="1970-01-01"&date<="2019-12-01") %>%
#     drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),CPI,Mode1(panel_df$output_variable[panel_df$country==a]))
#
#   reg = lm_robust(form,data=data_filtered)
#   AIC_core = AIC(lm(form,data=data_filtered))
#
#   b = tidy(reg)
#
#   form= as.formula(paste0("rate~","CPI","+","CPI","*ZLB","+",Mode1(panel_df$output_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"*ZLB","+lag_rate+time_trend-ZLB"))
#
#   reg = lm_robust(form,data=data_filtered)
#   AIC_total = AIC(lm(form,data=data_filtered))
#
#   c = tidy(reg)
#
#   second_round_taylor_regressions_zlb = rbind(second_round_taylor_regressions_zlb,c(b[2,2],b[2,4],b[6,2],b[6,4],b[3,2],b[3,4],b[7,2],b[7,4],b[4,2],b[4,4],b[5,2],b[5,4],c[2,2],c[2,4],c[6,2],c[6,4],c[3,2],c[3,4],c[7,2],c[7,4],c[4,2],c[4,4],c[5,2],c[5,4],AIC_core,AIC_total,reg$nobs,reg$r.squared,a,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]),as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(second_round_taylor_regressions_zlb) = c("inflation_coef","inflation_tstat","inflation_zlb_coef","inflation_zlb_tstat","output_gap_coef","output_gap_tstat","output_gap_zlb_coef","output_gap_zlb_tstat","lag_rate_coef","lag_rate_tstat","time_trend_coef","time_trend_tstat","total_cpi_inflation_coef","total_cpi_inflation_tstat","total_inflation_zlb_coef","total_inflation_zlb_tstat","total_cpi_output_gap_coef","total_cpi_output_gap_tstat","total_output_zlb_coef","total_output_zlb_tstat","total_cpi_lag_rate_coef","total_cpi_lag_rate_tstat","total_cpi_time_trend_coef","total_cpi_time_trend_tstat","AIC_core","AIC_total","num_obs","r_squared","country","inflation_variable","output_gap_variable","start_date","end_date")
#
#   write.csv(second_round_taylor_regressions_zlb,paste0(results_folder,"second_round_taylor_results_1970_2019_zlb.csv"))
# }
#
# # third round adds exchange rates, and EME regressions use US policy rate as well
# third_round_taylor_regressions = data.frame()
# for(a in unique(panel_df$country)){
#
#   if(a%in%c("Australia","Canada","Chile","Colombia","Czech Republic",
#             "Denmark","Hungary","Iceland","India","Israel",
#             "Japan","Malaysia","New Zealand","Norway","Peru",
#             "Philippines","Romania","Russia","Serbia","Sweden",
#             "Switzerland","United Kingdom","United States","Euro area")){
#     form= as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate+neer",ifelse(Mode1(panel_df$advanced[panel_df$country==a])=="Emerging Market Economy","+US_rate",""),"+time_trend"))
#   }else{form=as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate+neer",ifelse(Mode1(panel_df$advanced[panel_df$country==a])=="Emerging Market Economy","+US_rate","")))}
#
#   data_filtered = panel_df %>%
#     filter(country==a&date>=ifelse(a%in%c("Australia","Canada","Chile","Colombia","Czech Republic",
#                                           "Denmark","Hungary","Iceland","India","Israel",
#                                           "Japan","Malaysia","New Zealand","Norway","Peru",
#                                           "Philippines","Romania","Russia","Serbia","Sweden",
#                                           "Switzerland","United Kingdom","United States","Euro area"),"1970-01-01","2011-01-01")&date<="2019-12-01") %>%
#     drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]),neer)
#
#   reg = lm_robust(form,data=data_filtered)
#
#   b = tidy(reg)
#
#   third_round_taylor_regressions = rbind(third_round_taylor_regressions,c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],b[5,2],b[5,4],ifelse(Mode1(panel_df$advanced[panel_df$country==a])=="Emerging Market Economy",b[6,2],NA),ifelse(Mode1(panel_df$advanced[panel_df$country==a])=="Emerging Market Economy",b[6,4],NA),reg$nobs,reg$r.squared,a,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]),as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(third_round_taylor_regressions) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","neer_coef","neer_tstat","US_rate_coef","US_rate_tstat","num_obs","r_squared","country","inflation_variable","output_gap_variable","start_date","end_date")
#
#   write.csv(third_round_taylor_regressions,paste0(results_folder,"third_round_taylor_results_neer.csv"))
# }
#


# make taylor rule chart
# example code
for(a in unique(panel_df$country)){

  if(a%in%c("Australia","Canada","Chile","Colombia","Czech Republic",
            "Denmark","Hungary","Iceland","India","Israel",
            "Japan","Malaysia","New Zealand","Norway","Peru",
            "Philippines","Romania","Russia","Serbia","Sweden",
            "Switzerland","United Kingdom","United States","Euro area")){
    form= as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate+time_trend"))
  }else{form=as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate"))}

  data_filtered = panel_df %>%
    filter(country==a&date>=ifelse(a%in%c("Australia","Canada","Chile","Colombia","Czech Republic",
                                          "Denmark","Hungary","Iceland","India","Israel",
                                          "Japan","Malaysia","New Zealand","Norway","Peru",
                                          "Philippines","Romania","Russia","Serbia","Sweden",
                                          "Switzerland","United Kingdom","United States","Euro area"),"1970-01-01","2011-01-01")&date<="2019-12-01") %>%
    drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]))

  reg = lm_robust(form,data=data_filtered)

  b = tidy(reg)

  inflation_coef = b[2,2]
  output_coef = b[3,2]
  lag_rate_coef = b[4,2]
  time_trend_coef=ifelse(a%in%c("Australia","Canada","Chile","Colombia","Czech Republic",
                                "Denmark","Hungary","Iceland","India","Israel",
                                "Japan","Malaysia","New Zealand","Norway","Peru",
                                "Philippines","Romania","Russia","Serbia","Sweden",
                                "Switzerland","United Kingdom","United States","Euro area"),b[5,2],0)
  intercept=b[1,2]

  data_pred = panel_df %>%
    ungroup() %>%
    filter(country==a&date>"2019-12-01") %>%
    select(c(country,date,time_trend,Mode1(panel_df$inflation_variable[panel_df$country==a]),gdp_output_gap,lag_rate,rate))

  data_pred$lag_rate_rule[1] = as.numeric(data_pred$lag_rate[1])
  for(i in 1:nrow(data_pred)){
    data_pred$rate_rule[i] = intercept+(data_pred$time_trend[i]*time_trend_coef)+as.numeric(data_pred$lag_rate_rule[i]*as.numeric(lag_rate_coef))+as.numeric(data_pred[i,4]*as.numeric(inflation_coef))+as.numeric(data_pred[i,5]*as.numeric(output_coef))
    data_pred$taylor_rate_rule[i] = 2+as.numeric(data_pred[i,4])+.5*as.numeric(data_pred[i,4]-2)+as.numeric(data_pred[i,5]*.5)
    if(i < nrow(data_pred)){
    data_pred$lag_rate_rule[i+1]=as.numeric(data_pred$rate_rule[i])
    data_pred$rate_rule[(i+1):nrow(data_pred)] = NA
    data_pred$taylor_rate_rule[(i+1):nrow(data_pred)] = NA
    }
  }

  print(
  ggplot(data_pred  %>% mutate(date=date %m-% months(2))) +
    geom_line(aes(x=date,y=rate,colour="Actual Rate"),size=2) +
    geom_line(aes(x=date,y=rate_rule,colour="Predicted Rate"),size=2) +
    geom_line(aes(x=date,y=taylor_rate_rule,colour="Taylor Rule Rate"),size=2) +
    scale_x_date(labels = function(x) zoo::format.yearqtr(x, "%Y.q%q")) +
    labs(caption=a,y="Nominal Policy Rate",x="Quarter") +
    geom_hline(yintercept=0) +
    theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
  )

}







# code to run regression and dynamic prediction per country, with flexibility to change variables and specifications used


# # try and recreate paper regressions
# # regress interest rate on inflation and output gap
# usa_1960_1999 = lm_robust(rate~CPI+gdp_output_gap,data=panel_df %>% filter(country=="United States"&date>="1960-01-01"&date<="1999-12-01"))
# usa_1960_1979 = lm_robust(rate~CPI+gdp_output_gap,data=panel_df %>% filter(country=="United States"&date>="1960-01-01"&date<="1979-06-01"))
# usa_1979_1999 = lm_robust(rate~CPI+gdp_output_gap,data=panel_df %>% filter(country=="United States"&date>="1979-09-01"&date<="1999-12-01"))
# usa_1987_1999 = lm_robust(rate~CPI+gdp_output_gap,data=panel_df %>% filter(country=="United States"&date>="1987-12-01"&date<="1999-12-01"))
#
# usa_fran = read.csv(paste0(data_folder,"usa_francheschi_data.csv")) %>%
#   rowwise() %>%
#   mutate(date=as.Date(paste0(substr(date,1,5),as.numeric(substr(date,6,7))+2,"-01"),format="%Y-%m-%d")) %>%
#   select(-X)
#
# data_filtered = left_join(data_filtered,usa_fran,by=c("date","country"))
#
# summary(lm(rate~CPI+expost_gap+lag_rate,data=data_filtered %>% filter(country=="United States"&date<="2013-12-01"&date>="1967-01-01")))
#
#
#
# # implement Tobit (corner/censored) regression
# test1 = censReg(rate~core_inflation+gdp_output_gap,left=.125,data=panel_df %>% filter(country=="United States"&date>="1970-01-01"&date<="2019-12-01"))
#
# test2 = vglm(rate~core_inflation+gdp_output_gap,tobit(Lower=.125),data=panel_df %>% filter(country=="United States"&date>="1970-01-01"&date<="2019-12-01"))
#
# test3 = AER::tobit(rate~core_inflation+gdp_output_gap,left=.125,data=panel_df %>% filter(country=="United States"&date>="1970-01-01"&date<="2019-12-01"),x=TRUE)
#
# pnorm(sum(apply(test3$x,2,FUN=mean) * test3$coef)/test3$scale) * test3$coef[-1]
#
#
# tobit_taylor_regressions = data.frame()
# for(a in unique(panel_df$country)){
#
#   form= as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate"))
#
#   data_filtered = panel_df %>%
#     filter(country==a&date>="1970-01-01"&date<="2019-12-01") %>%
#     drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),CPI,Mode1(panel_df$output_variable[panel_df$country==a]))
#
#   if(a %in% c("Canada","Chile","Czech Republic","Denmark","Euro area","Japan","Israel","Norway","New Zealand","Sweden","Switzerland","United Kingdom","United States")){
#     reg = censReg(form,left=min(data_filtered$rate,na.rm=TRUE),data=data_filtered)
#     AIC_core = AIC(reg)
#   }else{
#       reg = lm_robust(form,data=data_filtered)
#       AIC_core = AIC(lm(form,data=data_filtered))
#     }
#
#
#   b = tidy(reg)
#
#   form= as.formula(paste0("rate~","CPI","+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate"))
#
#   if(a %in% c("Canada","Chile","Czech Republic","Denmark","Euro area","Japan","Israel","Norway","New Zealand","Sweden","Switzerland","United Kingdom","United States")){
#     reg = censReg(form,left=min(data_filtered$rate,na.rm=TRUE),data=data_filtered)
#     AIC_core = AIC(reg)
#   }else{
#     reg = lm_robust(form,data=data_filtered)
#     AIC_total = AIC(lm(form,data=data_filtered))
#   }
#
#   c = tidy(reg)
#
#   row = as.data.frame(c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],c[2,2],c[2,4],c[3,2],c[3,4],c[4,2],c[4,4],AIC_core,AIC_total,a,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]),as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#   if(nrow(row)>1){row=t(row)}
#   colnames(row) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","total_cpi_inflation_coef","total_cpi_inflation_tstat","total_cpi_output_gap_coef","total_cpi_output_gap_tstat","total_cpi_lag_rate_coef","total_cpi_lag_rate_tstat","AIC_core","AIC_total","country","inflation_variable","output_gap_variable","start_date","end_date")
#
#   tobit_taylor_regressions = rbind(tobit_taylor_regressions,row)
#   rownames(tobit_taylor_regressions) = NULL
#
#
#   write.csv(tobit_taylor_regressions,paste0(results_folder,"tobit_taylor_results_1970_2019.csv"))
# }
#
#
#
# tobit_taylor_time_trend_regressions = data.frame()
# for(a in unique(panel_df$country)){
#
#   form= as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate+time_trend"))
#
#   data_filtered = panel_df %>%
#     filter(country==a&date>="1970-01-01"&date<="2019-12-01") %>%
#     drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),CPI,Mode1(panel_df$output_variable[panel_df$country==a]))
#
#   if(a %in% c("Canada","Chile","Czech Republic","Denmark","Euro area","Japan","Israel","Norway","New Zealand","Sweden","Switzerland","United Kingdom","United States")){
#     reg = censReg(form,left=min(data_filtered$rate,na.rm=TRUE),data=data_filtered)
#     AIC_core = AIC(reg)
#   }else{
#     reg = lm_robust(form,data=data_filtered)
#     AIC_core = AIC(lm(form,data=data_filtered))
#   }
#
#
#   b = tidy(reg)
#
#   form= as.formula(paste0("rate~","CPI","+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate+time_trend"))
#
#   if(a %in% c("Canada","Chile","Czech Republic","Denmark","Euro area","Japan","Israel","Norway","New Zealand","Sweden","Switzerland","United Kingdom","United States")){
#     reg = censReg(form,left=min(data_filtered$rate,na.rm=TRUE),data=data_filtered)
#     AIC_core = AIC(reg)
#   }else{
#     reg = lm_robust(form,data=data_filtered)
#     AIC_total = AIC(lm(form,data=data_filtered))
#   }
#
#   c = tidy(reg)
#
#   row = as.data.frame(c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],b[5,2],b[5,4],c[2,2],c[2,4],c[3,2],c[3,4],c[4,2],c[4,4],c[5,2],c[5,4],AIC_core,AIC_total,a,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]),as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#   if(nrow(row)>1){row=t(row)}
#   colnames(row) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","time_trend_coef","time_trend_tstat","total_cpi_inflation_coef","total_cpi_inflation_tstat","total_cpi_output_gap_coef","total_cpi_output_gap_tstat","total_cpi_lag_rate_coef","total_cpi_lag_rate_tstat","total_cpi_time_trend_coef","total_cpi_time_trend_tstat","AIC_core","AIC_total","country","inflation_variable","output_gap_variable","start_date","end_date")
#
#   tobit_taylor_time_trend_regressions = rbind(tobit_taylor_time_trend_regressions,row)
#   rownames(tobit_taylor_time_trend_regressions) = NULL
#
#
#   write.csv(tobit_taylor_time_trend_regressions,paste0(results_folder,"tobit_taylor_results_1970_2019_time_trend.csv"))
# }
#
#
# # results from 2011 to 2019, with time trend to see if we get good coefs on both inflation and output
# first_round_taylor_regressions = data.frame()
# for(a in unique(panel_df$country)){
#
#   form = as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate+time_trend"))
#
#   data_filtered = panel_df %>%
#     filter(country==a&date>="2011-01-01"&date<="2019-12-01") %>%
#     drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]))
#
#   reg = lm_robust(form,data=data_filtered)
#
#   b = tidy(reg)
#
#   first_round_taylor_regressions = rbind(first_round_taylor_regressions,c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],b[5,2],b[5,4],reg$nobs,reg$r.squared,a,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]),as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(first_round_taylor_regressions) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","time_trend_coef","time_trend_tstat","num_obs","r_squared","country","inflation_variable","output_gap_variable","start_date","end_date")
#
#   write.csv(first_round_taylor_regressions,paste0(results_folder,"taylor_results_2011_2019_time_trend.csv"))
# }
#
# first_round_taylor_regressions = data.frame()
# for(a in unique(panel_df$country)){
#
#   form = as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate+time_trend+neer_12_month_change"))
#
#   data_filtered = panel_df %>%
#     filter(country==a&date>="2011-01-01"&date<="2019-12-01") %>%
#     drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]))
#
#   reg = lm_robust(form,data=data_filtered)
#
#   b = tidy(reg)
#
#   first_round_taylor_regressions = rbind(first_round_taylor_regressions,c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],b[5,2],b[5,4],b[6,2],b[6,4],b[7,2],b[7,4],reg$nobs,reg$r.squared,a,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]),as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(first_round_taylor_regressions) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","time_trend_coef","time_trend_tstat","neer_coef","neer_tstat",'us_rate_coef',"us_rate_tstat","num_obs","r_squared","country","inflation_variable","output_gap_variable","start_date","end_date")
#
#   write.csv(first_round_taylor_regressions,paste0(results_folder,"taylor_results_2011_2019_time_trend_neer.csv"))
# }
#
# # try rolling regression
# rolling_taylor_regressions = list()
# for(a in unique(panel_df$country)){
#
#   first_round_taylor_regressions = data.frame()
#
#   form = as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate+time_trend"))
#
#   df_country = panel_df %>%
#     filter(country==a) %>%
#     drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]))
#
#   for(yr in unique(df_country$year[df_country$year<2020])){
#     if(length(c(yr:2019))<10){
#       next
#     }
#
#     data_filtered = panel_df %>%
#       filter(country==a&year%in%c(yr:min((yr+9),2019))) %>%
#       drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]))
#
#     reg = lm_robust(form,data=data_filtered)
#
#     b = tidy(reg)
#
#     first_round_taylor_regressions = rbind(first_round_taylor_regressions,c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],b[5,2],b[5,4],reg$nobs,reg$r.squared,a,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]),as.character(lubridate::year(head(data_filtered$date,1))),as.character(lubridate::year(tail(data_filtered$date,1)))))
#
#     colnames(first_round_taylor_regressions) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","time_trend_coef","time_trend_tstat","num_obs","r_squared","country","inflation_variable","output_gap_variable","start_date","end_date")
#   }
#
#   rolling_taylor_regressions[[a]] = first_round_taylor_regressions
#
#   #write.csv(first_round_taylor_regressions,paste0(results_folder,"taylor_results_2011_2019_time_trend.csv"))
# }
#
# pdf(paste0(charts_folder,"rolling_10_years_core_inflation.pdf"))
# for(i in c(1:length(rolling_taylor_regressions))[-13]){ # Indonesia doesnt work
#   plot_df = reshape2::melt(rolling_taylor_regressions[[i]] %>% mutate_at(vars(inflation_coef:time_trend_tstat),as.numeric) %>% mutate(start_date=as.numeric(start_date)) %>%  select(start_date,inflation_coef,output_gap_coef),id.vars="start_date",measure.vars=c("inflation_coef","output_gap_coef"),variable.name="coef",value.name="value")
#   print(ggplot(plot_df,aes(x=start_date,y=value,colour=coef)) +
#           geom_line(size=2) +
#           labs(x="Date Range (start year to start year + 9)",y="Regression coefficient value",caption=names(rolling_taylor_regressions)[i])
#         )
# }
# dev.off()
#
#
# # try expanding regression
# expanding_taylor_regressions = list()
# for(a in unique(panel_df$country)){
#
#   first_round_taylor_regressions = data.frame()
#
#   form = as.formula(paste0("rate~",Mode1(panel_df$inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$output_variable[panel_df$country==a]),"+lag_rate+time_trend"))
#
#   df_country = panel_df %>%
#     filter(country==a) %>%
#     drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]))
#
#   for(yr in unique(df_country$year[df_country$year<2020])[-1]){
#
#     data_filtered = panel_df %>%
#       filter(country==a&year%in%c(min(df_country$year):yr)) %>%
#       drop_na(rate,lag_rate,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]))
#
#     reg = lm_robust(form,data=data_filtered)
#
#     b = tidy(reg)
#
#     first_round_taylor_regressions = rbind(first_round_taylor_regressions,c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],b[5,2],b[5,4],reg$nobs,reg$r.squared,a,Mode1(panel_df$inflation_variable[panel_df$country==a]),Mode1(panel_df$output_variable[panel_df$country==a]),as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#     colnames(first_round_taylor_regressions) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","time_trend_coef","time_trend_tstat","num_obs","r_squared","country","inflation_variable","output_gap_variable","start_date","end_date")
#   }
#
#   expanding_taylor_regressions[[a]] = first_round_taylor_regressions
#
#   #write.csv(first_round_taylor_regressions,paste0(results_folder,"taylor_results_2011_2019_time_trend.csv"))
# }
#
# pdf(paste0(charts_folder,"expanding_core_inflation.pdf"))
# for(i in c(1:length(expanding_taylor_regressions))){
#   plot_df = reshape2::melt(expanding_taylor_regressions[[i]] %>% mutate_at(vars(inflation_coef:time_trend_tstat),as.numeric) %>% mutate(end_date=as.Date(end_date,format="%Y-%m-%d")) %>%  select(end_date,inflation_coef,output_gap_coef),id.vars="end_date",measure.vars=c("inflation_coef","output_gap_coef"),variable.name="coef",value.name="value")
#   print(ggplot(plot_df,aes(x=end_date,y=value,colour=coef)) +
#           geom_line(size=2) +
#           labs(x="Date Range (first observed year to year shown on axis)",y="Regression coefficient value",caption=names(expanding_taylor_regressions)[i])
#   )
# }
# dev.off()
#
#
#
# mu = fitted(reg)
# sigma = reg$scale
#
# p0 = pnorm(mu/sigma)
#
# lambda = function(x) dnorm(x)/pnorm(x)
#
# ey0 = mu + sigma*lambda(mean(mu)/sigma)
#
# pnorm(sum(apply(test3$x,2,FUN=mean) * test3$coef)/test3$scale) * test3$coef[-1]
#
# pnorm(sum(tidy(reg)$estimate[-c(1,6)]*reg$xMean[-1])/)
#
#
#
#
# # results from 2011 to 2019, with time trend to see if we get good coefs on both inflation and output
# first_round_taylor_regressions = data.frame()
# for(a in unique(panel_df_monthly$country)){
#
#   form = as.formula(paste0("rate~",Mode1(panel_df_monthly$inflation_variable[panel_df_monthly$country==a]),"+",Mode1(panel_df_monthly$output_variable[panel_df_monthly$country==a]),"+lag_rate+time_trend"))
#
#   data_filtered = panel_df_monthly %>%
#     filter(country==a&date>="1970-01-01"&date<="2019-12-01") %>%
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
#   write.csv(first_round_taylor_regressions,paste0(results_folder,"taylor_results_1970_2019_time_trend_monthly.csv"))
# }
#
#
# # in differences monthly
# first_round_taylor_regressions = data.frame()
# for(a in unique(panel_df_monthly$country)){
#
#   form = as.formula(paste0("diff_rate~",Mode1(panel_df_monthly$diff_inflation_variable[panel_df_monthly$country==a]),"+",Mode1(panel_df_monthly$diff_output_variable[panel_df_monthly$country==a]),"+lag_diff_rate"))
#
#   data_filtered = panel_df_monthly %>%
#     filter(country==a&date>="1970-01-01"&date<="2019-12-01") %>%
#     drop_na(diff_rate,lag_diff_rate,Mode1(panel_df_monthly$diff_inflation_variable[panel_df_monthly$country==a]),Mode1(panel_df_monthly$diff_output_variable[panel_df_monthly$country==a]))
#
#
#   reg = lm_robust(form,data=data_filtered)
#
#   b = tidy(reg)
#
#   first_round_taylor_regressions = rbind(first_round_taylor_regressions,c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],reg$nobs,reg$r.squared,a,Mode1(panel_df_monthly$diff_inflation_variable[panel_df_monthly$country==a]),Mode1(panel_df_monthly$diff_output_variable[panel_df_monthly$country==a]),as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(first_round_taylor_regressions) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","num_obs","r_squared","country","inflation_variable","output_gap_variable","start_date","end_date")
#
#   write.csv(first_round_taylor_regressions,paste0(results_folder,"taylor_results_1970_2019_differences_monthly.csv"))
# }
#
#
# first_round_taylor_regressions = data.frame()
# for(a in unique(panel_df$country)){
#
#   form = as.formula(paste0("diff_rate~",Mode1(panel_df$diff_inflation_variable[panel_df$country==a]),"+",Mode1(panel_df$diff_output_variable[panel_df$country==a]),"+lag_diff_rate"))
#
#   data_filtered = panel_df %>%
#     filter(country==a&date>="1970-01-01"&date<="2019-12-01") %>%
#     drop_na(diff_rate,lag_diff_rate,Mode1(panel_df$diff_inflation_variable[panel_df$country==a]),Mode1(panel_df$diff_output_variable[panel_df$country==a]))
#
#   reg = lm_robust(form,data=data_filtered)
#
#   b = tidy(reg)
#
#   first_round_taylor_regressions = rbind(first_round_taylor_regressions,c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],reg$nobs,reg$r.squared,a,Mode1(panel_df$diff_inflation_variable[panel_df$country==a]),Mode1(panel_df$diff_output_variable[panel_df$country==a]),as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1))))
#
#   colnames(first_round_taylor_regressions) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","num_obs","r_squared","country","inflation_variable","output_gap_variable","start_date","end_date")
#
#   write.csv(first_round_taylor_regressions,paste0(results_folder,"taylor_results_1970_2019_differences_quarterly.csv"))
# }
