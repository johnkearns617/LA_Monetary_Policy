la_data = panel_df_prediction_data %>% 
  filter(country_group=="Latin America")

for(a in unique(la_data$country)){
  
  la_data$gdp_index[la_data$country==a&!(la_data$year<2020&is.na(la_data$gdp_index))] = imputeTS::na.kalman(la_data$gdp_index[la_data$country==a&!(la_data$year<2020&is.na(la_data$gdp_index))],model="auto.arima")

}

la_data$gdp_output_gap = (la_data$gdp_index/la_data$gdp_trend-1)*100

# redo sim with fixed effects, intercept, but no netting out, and no intercept for FE
# step 1: regression for LAC, no pandemic dummies, with time and country FEs, 2007-2019
lac_regression_2007_2019 = lm_robust(rate~CPI+gdp_output_gap+lag_rate+factor(country)+factor(date),data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Latin America"))
summary(lac_regression_2007_2019)

# step 2: dynamically and statically simulate path of rates during 2020 and 2021

intercept = .372392
inflation_coef = as.numeric(tidy(lac_regression_2007_2019)[2,2])
gdp_coef = as.numeric(tidy(lac_regression_2007_2019)[3,2])
lag_rate_coef = as.numeric(tidy(lac_regression_2007_2019)[4,2])

# dynamic
dynamic_lac_sim = data.frame()
for(a in c("Brazil","Chile","Colombia","Peru","Mexico")){
  
  base_df = la_data %>% 
    filter(country==a&date>="2020-01-01"&date<="2022-03-31") %>% 
    select(date,country,CPI,gdp_output_gap,lag_rate) %>% 
    mutate(country_factor=case_when(
      country=="Brazil"~0,
      country=="Chile"~-0.271606,
      country=="Colombia"~-0.208057,
      country=="Mexico"~-0.139785,
      country=="Peru"~-0.224761
    ),
    country_factor=as.numeric(country_factor))
  
  base_df$lag_rate_sim = ifelse(base_df$date=="2020-03-01",base_df$lag_rate[1],NA)
  base_df$predicted_rate = NA
  for(i in 1:nrow(base_df)){
    base_df$predicted_rate[i] = (base_df$CPI[i]*inflation_coef)+(base_df$gdp_output_gap[i]*gdp_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)
    if(i < nrow(base_df)){
      base_df$lag_rate_sim[i+1]= base_df$predicted_rate[i]
    }
  }
  
  dynamic_lac_sim = bind_rows(dynamic_lac_sim,la_data %>% 
                                filter(country==a&date>="2019-01-01"&date<="2019-12-31") %>% 
                                select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>% 
                                rename(predicted_rate=rate),
                              base_df)
}
dynamic_lac_sim_full = bind_rows(dynamic_lac_sim,
                                 dynamic_lac_sim %>% 
                                   group_by(date) %>% 
                                   summarise(CPI=mean(CPI,na.rm=TRUE),
                                             gdp_output_gap = mean(gdp_output_gap,na.rm=TRUE),
                                             lag_rate=mean(lag_rate,na.rm=TRUE),
                                             predicted_rate=mean(predicted_rate,na.rm=TRUE),
                                             country="Latin America"))
dynamic_lac_sim_full$country = factor(dynamic_lac_sim_full$country,levels=c("Brazil","Chile","Colombia","Mexico","Peru","Latin America"))

panel_df_prediction_data = panel_df_prediction_data %>% filter(!is.na(country))

# because Chile and Brazil are missing
# dynamic_lac_sim_full$predicted_rate[dynamic_lac_sim_full$date=="2021-12-01"&dynamic_lac_sim_full$country=="Latin America"] = 2.19225374

fig11 = ggplot(dynamic_lac_sim_full %>% filter(country!="Latin America") %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup(),aes(x=date)) +
  geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) +
  geom_line(data=panel_df_prediction_data %>% filter(!is.na(country)) %>%  rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(date<="2021-09-30"&date>="2019-01-01"&country%in%c("Brazil","Chile","Colombia","Peru","Mexico")),
            aes(y=rate,colour="Actual Rate"),size=2) +
  #geom_line(data=panel_df_prediction_data %>% filter(!is.na(country)) %>%  rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(date<="2021-09-30"&date>="2019-01-01"&country%in%c("Brazil","Chile","Colombia","Peru","Mexico")) %>% group_by(date) %>% summarise(rate=mean(rate,na.rm=TRUE),country="Latin America"),
  #          aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(data=daily_rates %>% filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2021-09-29"),aes(y=rate,colour="Actual Rate"),size=2) +
  #geom_line(data=daily_rates %>% filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2021-09-29") %>% group_by(date) %>% summarise(rate=mean(rate,na.rm=TRUE)) %>% ungroup() %>% mutate(country="Latin America"),aes(y=rate,colour="Actual Rate"),size=2) +
  facet_wrap(~~fct_relevel(country,"Brazil","Chile","Colombia","Mexico","Peru"))+
  scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("Actual Rate"="red","Predicted Rate"="blue")) +
  scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2022-03-31"),
                              format="%Y-%m-%d"),minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31","2022-03-31"),
                                                                      format="%Y-%m-%d"),labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
  labs(x="Quarter",y="Policy Rate (%)") +
  theme(text = element_text(size=30),axis.text.x = element_text(size=25,angle=45,hjust=1),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
        legend.position=c(.5,.9),legend.background=element_blank(),legend.title=element_blank())
ggsave(plot=fig11,filename=paste0(charts_folder,"Scatterplots/2022-03-16_Presentation_Update/fig11_LA_sims_20220330.png"),
       width=12.72,height=6.49,units="in")

# output gap bar chart
barchart_data = la_data %>% 
  select(country,date,gdp_output_gap)

barchart_fig = ggplot(la_data %>% filter(date=="2022-03-01") %>% mutate(maintitle="Estimated Output Gaps - 2022 Q1"),aes(x=country,y=gdp_output_gap)) +
  geom_bar(stat="identity",fill="blue") +
  facet_wrap(~maintitle) +
  labs(x="",y="GDP Output Gap (%)") +
  theme(text = element_text(size=30),axis.text.x = element_text(size=25,angle=45,hjust=1),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
        strip.text.x = element_text(size = 20))
ggsave(plot=barchart_fig,filename=paste0(charts_folder,"Scatterplots/2022-03-16_Presentation_Update/barchart_2022q1_20220328.png"),
       width=4.29*2,height=3.56*2,units="in")


# inflation expectations chart
brazil_xinflation = readxl::read_excel(paste0(data_folder,"Raw/Brazil Inflation Expectations.xls"))
colnames(brazil_xinflation) = c("date",brazil_xinflation[1,2:(ncol(brazil_xinflation))])
brazil_xinflation = brazil_xinflation[-1,]
brazil_xinflation = brazil_xinflation %>% 
  pivot_longer(
    cols=`43070`:`45352`,
    names_to="forecast_date",
    values_to="exp_inflation"
  ) %>% 
  mutate(date=as.Date(as.numeric(date),origin="1899-12-30",format="%Y-%m-%d"),
         forecast_date=as.Date(as.numeric(forecast_date),origin="1899-12-30",format="%Y-%m-%d"),
         year=lubridate::year(date),
         month=lubridate::month(date),
         forecast_month=lubridate::month(forecast_date),
         forecast_year=lubridate::year(forecast_date)) %>%
  drop_na(exp_inflation) %>% 
  filter(forecast_date>=date-27) %>% 
  group_by(date) %>%
  mutate(exp_inflation_12mo = zoo::rollapply(1+(exp_inflation/100),12,prod,na.pad=T,align="right")) %>% 
  filter(forecast_year!=year&forecast_month==month) %>% 
  group_by(forecast_date) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  select(forecast_date,exp_inflation_12mo) %>% 
  mutate(country="Brazil")

brazil_xinflation$exp_inflation_12mo = NA
for(i in 12:nrow(brazil_xinflation)){
  
  brazil_xinflation$exp_inflation_12mo[i] = (cumprod(1+(brazil_xinflation$exp_inflation[(i-11):i]/100))[12]-1)*100
}

brazil_xinflation = brazil_xinflation %>% 
  mutate(country="Brazil") %>% 
  select(country,forecast_date,exp_inflation_12mo)

# Mexico
mexico_exp_inflation = readxl::read_excel(paste0(data_folder,"Raw/Mexico Inflation Expectations.xlsx"))
mexico_exp_inflation = mexico_exp_inflation[2:nrow(mexico_exp_inflation),c(1,4)]
colnames(mexico_exp_inflation) = c("date","exp_inflation")
mexico_exp_inflation = mexico_exp_inflation %>% 
  mutate(forecast_date=as.Date(substr(date,1,10),format="%Y-%m-%d") %m+% years(1),
         exp_inflation_12mo = as.numeric(exp_inflation,12),
         country="Mexico") %>% 
  select(country,forecast_date,exp_inflation_12mo)

# Peru
peru_exp_inflation = readxl::read_excel(paste0(data_folder,"Raw/Peru Inflation Expectations.xlsx"))[2:243,]
colnames(peru_exp_inflation) = c("forecast_date","exp_inflation_12mo")
peru_exp_inflation$forecast_date = seq(as.Date("2002-01-01",format="%Y-%m-%d"),as.Date("2022-02-01",format="%Y-%m-%d"),by="1 month") %m+% years(1)
peru_exp_inflation = peru_exp_inflation %>% 
  mutate(country="Peru",
         exp_inflation_12mo=as.numeric(exp_inflation_12mo))

# Colombia
colombia_exp_inflation = readxl::read_excel(paste0(data_folder,"Raw/Colombia Inflation Expectations.xls"),sheet="INFLACION_TOTAL")
colombia_exp_inflation = colombia_exp_inflation[2:224,c(1,12)]
colnames(colombia_exp_inflation) = c("forecast_date","exp_inflation_12mo")
colombia_exp_inflation = colombia_exp_inflation %>% 
  mutate(forecast_date=as.Date(as.numeric(forecast_date),origin="1899-12-30",format="%Y-%m-%d"),
         exp_inflation_12mo=as.numeric(exp_inflation_12mo)*100,
         country="Colombia",
         year=lubridate::year(forecast_date),
         month=lubridate::month(forecast_date),
         forecast_date=as.Date(paste0(year+1,"-",month,"-01"),format="%Y-%m-%d")) %>% 
  select(country,forecast_date,exp_inflation_12mo)

# Chile
chile_exp_inflation = readxl::read_excel(paste0(data_folder,"Raw/Chile Inflation Expectations.xlsx"))
chile_exp_inflation = chile_exp_inflation[c(8,15),2:39]
chile_exp_inflation = as.data.frame(t(chile_exp_inflation))
chile_exp_inflation$V1 = seq(as.Date("2019-01-01",format="%Y-%m-%d"),as.Date("2022-02-01",format="%Y-%m-%d"),by="1 month") %m+% years(1)
colnames(chile_exp_inflation) = c("forecast_date","exp_inflation_12mo")
chile_exp_inflation = chile_exp_inflation %>% 
  mutate(exp_inflation_12mo=as.numeric(exp_inflation_12mo),
         country="Chile")
rownames(chile_exp_inflation) = NULL

# make full set
la_exp_inflation=bind_rows(brazil_xinflation,
                           mexico_exp_inflation,
                           peru_exp_inflation,
                           colombia_exp_inflation,
                           chile_exp_inflation)

la_exp_inflation = left_join(la_exp_inflation,panel_df_monthly %>% select(country,date,CPI),by=c("country"="country","forecast_date"="date"))


ggplot(la_exp_inflation %>% filter(forecast_date>="2019-01-01"),aes(x=forecast_date)) +
  geom_line(aes(y=exp_inflation_12mo),size=2,linetype="dashed",colour="blue") +
  geom_line(aes(y=CPI),size=2,colour="red") +
  theme(text = element_text(size=30),axis.text.x = element_text(size=25,angle=45,hjust=1),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey")) +
  facet_wrap(~country) +
  labs(x="Date",y="Headline inflation (%)",caption="Sources: Central banks of Brazil, Chile, Colombia, Mexico, and Peru.\nNote: Inflation expectations are given as the survey-based, median estimate of year-ahead inflation.") +
  scale_colour_manual(values=c("blue","red"),labels=c("Expected inflation","Actual inflation"))


