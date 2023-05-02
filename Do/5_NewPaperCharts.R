
# appendix table 1: individual latin america regressions, 2007 to 2019
## Appendix Table 1.1 ##
col1 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Brazil"))
col2 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Chile"))
col3 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Colombia"))
col4 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Mexico"))
col5 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Peru"))
htmlreg(list(col1,col2,col3,col4,col5),file=paste0(results_folder,"Regression Results/2021-11-22_Taylor_Rule_expanded_LA/LA_individual_countries.html"),caption="Individual Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019, No Time or Country FE, Quarterly Percent Change in REER",caption.above=TRUE,
        custom.model.names = c("Brazil","Chile","Colombia","Mexico","Peru"),stars=c(.01,.05,.1))
wordreg(list(col1,col2,col3,col4,col5),file="LA_individual_countries.html.doc",caption="Individual Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019 extrapolation, Time and Country FE, Quarterly Percent Change in REER",caption.above=TRUE,
        custom.model.names = c("Brazil","Chile","Colombia","Mexico","Peru"),stars=c(.01,.05,.1))

# simulation of policy rates (dynamic and static) for LAC


#step 2: dynamically and statically simulate path of rates during 2020 and 2021

#step 1: regression for individual LACs, starting with brazil
## brazil
brazil = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Brazil"))
summary(brazil)

intercept_Brazil = -1.00825 # updates 4/17
inflation_coef_Brazil = as.numeric(tidy(brazil)[2,2])
gdp_coef_Brazil = as.numeric(tidy(brazil)[3,2])
lag_rate_coef_Brazil = as.numeric(tidy(brazil)[4,2])

# dynamic
dynamic_lac_sim = data.frame()
for(a in c("Brazil")){
  
  base_df = panel_df_prediction_data %>%
    filter(country==a&date>="2020-01-01"&date<="2022-12-01") %>% #filter for the "pandemic period"
    select(date,country,CPI,gdp_output_gap,lag_rate)
  
  base_df$lag_rate_sim = ifelse(base_df$date=="2020-03-01",base_df$lag_rate[1],NA)
  base_df$predicted_rate = NA
  for(i in 1:nrow(base_df)){ #for each row, estimate the predicted rate as a fxn of the below
    base_df$predicted_rate[i] = (base_df$CPI[i]*inflation_coef_Brazil)+(base_df$gdp_output_gap[i]*gdp_coef_Brazil)+(base_df$lag_rate_sim[i]*lag_rate_coef_Brazil)
    if(i < nrow(base_df)){
      base_df$lag_rate_sim[i+1]= base_df$predicted_rate[i]
    }
  }
  
  dynamic_lac_sim = bind_rows(dynamic_lac_sim,panel_df_prediction_data %>%
                                filter(country==a&date>="2019-01-01"&date<"2019-12-31") %>%
                                select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>%
                                rename(predicted_rate=rate),
                              base_df)
}

brazil_plot <- ggplot(dynamic_lac_sim %>% filter(date>="2019-09-30") %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup(),aes(x=date)) +
  geom_line(data=panel_df_prediction_data %>% #this line plots the actual data, which should start at Q1 19 and end at the latest quarter (in this case, Q2 22)
              rowwise() %>% 
              mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% 
              ungroup() %>% 
              filter(date<="2022-12-31"&date>="2019-01-01"&country%in%c("Brazil")),
              aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) + #this line plots the predicted data, which should start Q4 of 2019 (filter date in the first line of this plot)
  # geom_line(data=daily_rates %>% filter(country%in%c("Brazil")&date>="2022-12-31"), #this line plot daily data for beyond when we hav qtrly data
  #           aes(y=rate,colour="Actual Rate"),size=2) +
  facet_wrap(~country)+
  scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
  scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2022-03-31","2022-09-30","2023-03-31"),format="%Y-%m-%d"),
               minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31","2022-03-31","2022-06-30","2022-09-30","2022-12-31","2023-03-31"),format="%Y-%m-%d"),
               labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
  scale_y_continuous(limits=c(-10,15), breaks=c(-10,-5,0,5,10,15)) +
  labs(y="Policy Rate (%)") +
  theme(text = element_text(size=20),legend.position="none",axis.text.x = element_text(size=17,angle=45,hjust=1),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))


## chile ---------------------------------------------------------------------------------------------------
chile = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Chile"))
summary(chile)

intercept_Chile = 0.88127  # update 4/17
inflation_coef_Chile = as.numeric(tidy(chile)[2,2])
gdp_coef_Chile = as.numeric(tidy(chile)[3,2])
lag_rate_coef_Chile = as.numeric(tidy(chile)[4,2])

# dynamic
dynamic_lac_sim = data.frame()
for(a in c("Chile")){
  
  base_df = panel_df_prediction_data %>%
    filter(country==a&date>="2020-01-01"&date<="2022-12-31") %>%
    select(date,country,CPI,gdp_output_gap,lag_rate)
  
  base_df$lag_rate_sim = ifelse(base_df$date=="2020-03-01",base_df$lag_rate[1],NA)
  base_df$predicted_rate = NA
  for(i in 1:nrow(base_df)){
    base_df$predicted_rate[i] = (base_df$CPI[i]*inflation_coef_Chile)+(base_df$gdp_output_gap[i]*gdp_coef_Chile)+(base_df$lag_rate_sim[i]*lag_rate_coef_Chile)
    if(i < nrow(base_df)){
      base_df$lag_rate_sim[i+1]= base_df$predicted_rate[i]
    }
  }
  
  dynamic_lac_sim = bind_rows(dynamic_lac_sim,panel_df_prediction_data %>%
                                filter(country==a&date>="2019-01-01"&date<"2019-12-31") %>%
                                select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>%
                                rename(predicted_rate=rate),
                              base_df)
}

chile_plot <- ggplot(dynamic_lac_sim %>% filter(date>="2019-09-30") %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup(),aes(x=date)) +
  geom_line(data=panel_df_prediction_data %>% #this line plots the actual data, which should start at Q1 19 and end at the latest quarter (in this case, Q2 22)
              rowwise() %>% 
              mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% 
              ungroup() %>% 
              filter(date<="2022-12-31"&date>="2019-01-01"&country%in%c("Chile")),
            aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) + #this line plots the predicted data, which should start Q4 of 2019 (filter date in the first line of this plot)
  # geom_line(data=daily_rates %>% filter(country%in%c("Chile")&date>="2022-12-31"), #this line plot daily data for beyond when we hav qtrly data
  #           aes(y=rate,colour="Actual Rate"),size=2) +
  facet_wrap(~country)+
  scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
  scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2022-03-31","2022-09-30","2023-03-31"),format="%Y-%m-%d"),
               minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31","2022-03-31","2022-06-30","2022-09-30","2022-12-31","2023-03-31"),format="%Y-%m-%d"),
               labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
  scale_y_continuous(limits=c(-10,15), breaks=c(-10,-5,0,5,10,15)) +
  labs(y="Policy Rate (%)") +
  theme(text = element_text(size=20),axis.text.x = element_text(size=17,angle=45,hjust=1),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

chile_plot2 <- chile_plot + theme(legend.position="none")
## colombia ---------------------------------------------------------------------------------------------------
colombia = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Colombia"))
summary(colombia)

intercept_Colombia = 0.55294 # update 4/17
inflation_coef_Colombia = as.numeric(tidy(colombia)[2,2])
gdp_coef_Colombia = as.numeric(tidy(colombia)[3,2])
lag_rate_coef_Colombia = as.numeric(tidy(colombia)[4,2])

# dynamic
dynamic_lac_sim = data.frame()
for(a in c("Colombia")){
  
  base_df = panel_df_prediction_data %>%
    filter(country==a&date>="2020-01-01"&date<="2022-12-31") %>%
    select(date,country,CPI,gdp_output_gap,lag_rate)
  
  base_df$lag_rate_sim = ifelse(base_df$date=="2020-03-01",base_df$lag_rate[1],NA)
  base_df$predicted_rate = NA
  for(i in 1:nrow(base_df)){
    base_df$predicted_rate[i] = (base_df$CPI[i]*inflation_coef_Colombia)+(base_df$gdp_output_gap[i]*gdp_coef_Colombia)+(base_df$lag_rate_sim[i]*lag_rate_coef_Colombia)
    if(i < nrow(base_df)){
      base_df$lag_rate_sim[i+1]= base_df$predicted_rate[i]
    }
  }
  
  dynamic_lac_sim = bind_rows(dynamic_lac_sim,panel_df_prediction_data %>%
                                filter(country==a&date>="2019-01-01"&date<"2019-12-31") %>%
                                select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>%
                                rename(predicted_rate=rate),
                              base_df)
}

colombia_plot <- ggplot(dynamic_lac_sim %>% filter(date>="2019-09-30") %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup(),aes(x=date)) +
  geom_line(data=panel_df_prediction_data %>% #this line plots the actual data, which should start at Q1 19 and end at the latest quarter (in this case, Q2 22)
              rowwise() %>% 
              mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% 
              ungroup() %>% 
              filter(date<="2022-12-31"&date>="2019-01-01"&country%in%c("Colombia")),
            aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) + #this line plots the predicted data, which should start Q4 of 2019 (filter date in the first line of this plot)
  # geom_line(data=daily_rates %>% filter(country%in%c("Colombia")&date>="2022-12-31"), #this line plot daily data for beyond when we hav qtrly data
  #           aes(y=rate,colour="Actual Rate"),size=2) +
  facet_wrap(~country)+
  scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
  scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2022-03-31","2022-09-30","2023-03-31"),format="%Y-%m-%d"),
               minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31","2022-03-31","2022-06-30","2022-09-30","2022-12-31","2023-03-31"),format="%Y-%m-%d"),
               labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
  scale_y_continuous(limits=c(-10,15), breaks=c(-10,-5,0,5,10,15)) +
  labs(y="Policy Rate (%)") +
  theme(text = element_text(size=20),legend.position="none",axis.text.x = element_text(size=17,angle=45,hjust=1),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

## mexico ---------------------------------------------------------------------------------------------------
mexico = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Mexico"))
summary(mexico)

intercept_Mexico = 0.25786 # updated 4/17
inflation_coef_Mexico = as.numeric(tidy(mexico)[2,2])
gdp_coef_Mexico = as.numeric(tidy(mexico)[3,2])
lag_rate_coef_Mexico = as.numeric(tidy(mexico)[4,2])

# dynamic
dynamic_lac_sim = data.frame()
for(a in c("Mexico")){
  
  base_df = panel_df_prediction_data %>%
    filter(country==a&date>="2020-01-01"&date<="2022-12-01") %>%
    select(date,country,CPI,gdp_output_gap,lag_rate)
  
  base_df$lag_rate_sim = ifelse(base_df$date=="2020-03-01",base_df$lag_rate[1],NA)
  base_df$predicted_rate = NA
  for(i in 1:nrow(base_df)){
    base_df$predicted_rate[i] = (base_df$CPI[i]*inflation_coef_Mexico)+(base_df$gdp_output_gap[i]*gdp_coef_Mexico)+(base_df$lag_rate_sim[i]*lag_rate_coef_Mexico)
    if(i < nrow(base_df)){
      base_df$lag_rate_sim[i+1]= base_df$predicted_rate[i]
    }
  }
  
  dynamic_lac_sim = bind_rows(dynamic_lac_sim,panel_df_prediction_data %>%
                                filter(country==a&date>="2019-01-01"&date<"2019-12-31") %>%
                                select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>%
                                rename(predicted_rate=rate),
                              base_df)
}

mexico_plot <- ggplot(dynamic_lac_sim %>% filter(date>="2019-09-30") %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup(),aes(x=date)) +
  geom_line(data=panel_df_prediction_data %>% #this line plots the actual data, which should start at Q1 19 and end at the latest quarter (in this case, Q2 22)
              rowwise() %>% 
              mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% 
              ungroup() %>% 
              filter(date<="2022-12-31"&date>="2019-01-01"&country%in%c("Mexico")),
            aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) + #this line plots the predicted data, which should start Q4 of 2019 (filter date in the first line of this plot)
  # geom_line(data=daily_rates %>% filter(country%in%c("Mexico")&date>="2022-12-31"), #this line plot daily data for beyond when we hav qtrly data
  #           aes(y=rate,colour="Actual Rate"),size=2) +
  facet_wrap(~country)+
  scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
  scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2022-03-31","2022-09-30","2023-03-31"),format="%Y-%m-%d"),
               minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31","2022-03-31","2022-06-30","2022-09-30","2022-12-31","2023-03-31"),format="%Y-%m-%d"),
               labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
  scale_y_continuous(limits=c(-10,15), breaks=c(-10,-5,0,5,10,15)) +
  labs(y="Policy Rate (%)") +
  theme(text = element_text(size=20),legend.position="none",axis.text.x = element_text(size=17,angle=45,hjust=1),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

## peru ---------------------------------------------------------------------------------------------------
peru = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Peru"))
summary(peru)

intercept_Peru = 0.59253 
inflation_coef_Peru = as.numeric(tidy(peru)[2,2])
gdp_coef_Peru = as.numeric(tidy(peru)[3,2])
lag_rate_coef_Peru = as.numeric(tidy(peru)[4,2])

# dynamic
dynamic_lac_sim = data.frame()
for(a in c("Peru")){
  
  base_df = panel_df_prediction_data %>%
    filter(country==a&date>="2020-01-01"&date<="2022-12-01") %>%
    select(date,country,CPI,gdp_output_gap,lag_rate)
  
  base_df$lag_rate_sim = ifelse(base_df$date=="2020-03-01",base_df$lag_rate[1],NA)
  base_df$predicted_rate = NA
  for(i in 1:nrow(base_df)){
    base_df$predicted_rate[i] = (base_df$CPI[i]*inflation_coef_Peru)+(base_df$gdp_output_gap[i]*gdp_coef_Peru)+(base_df$lag_rate_sim[i]*lag_rate_coef_Peru)
    if(i < nrow(base_df)){
      base_df$lag_rate_sim[i+1]= base_df$predicted_rate[i]
    }
  }
  
  dynamic_lac_sim = bind_rows(dynamic_lac_sim,panel_df_prediction_data %>%
                                filter(country==a&date>="2019-01-01"&date<"2019-12-31") %>%
                                select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>%
                                rename(predicted_rate=rate),
                              base_df)
}

peru_plot <- ggplot(dynamic_lac_sim %>% filter(date>="2019-09-30") %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup(),aes(x=date)) +
  geom_line(data=panel_df_prediction_data %>% #this line plots the actual data, which should start at Q1 19 and end at the latest quarter (in this case, Q2 22)
              rowwise() %>% 
              mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% 
              ungroup() %>% 
              filter(date<="2022-12-31"&date>="2019-01-01"&country%in%c("Peru")),
            aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) + #this line plots the predicted data, which should start Q4 of 2019 (filter date in the first line of this plot)
  # geom_line(data=daily_rates %>% filter(country%in%c("Peru")&date>="2022-12-31"), #this line plot daily data for beyond when we hav qtrly data
  #           aes(y=rate,colour="Actual Rate"),size=2) +
  facet_wrap(~country)+
  scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
  scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2022-03-31","2022-09-30","2023-03-31"),format="%Y-%m-%d"),
               minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31","2022-03-31","2022-06-30","2022-09-30","2022-12-31","2023-03-31"),format="%Y-%m-%d"),
               labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
  scale_y_continuous(limits=c(-10,15), breaks=c(-10,-5,0,5,10,15)) +
  labs(y="Policy Rate (%)") +
  theme(text = element_text(size=20),legend.position="none",axis.text.x = element_text(size=17, angle=45,hjust=1),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))


#now, combine all these together in a single plot!
legend <- get_legend(chile_plot)

appx_fig_1.1 <- plot_grid(brazil_plot, chile_plot2, colombia_plot, mexico_plot, peru_plot, legend, align="h", axis='1')

ggsave(plot=appx_fig_1.1,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/appx_fig_1.1_la_individual_reg_predicted_interest_rates.png"),
       width=20,height=12,units="in")
ggsave(plot=appx_fig_1.1,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/appx_fig_1.1_la_individual_reg_predicted_interest_rates_20230417.png"),
       width=20,height=12,units="in",dpi=72)

#################################################################################################################################################
######## I AM ONCE AGAIN ATTEMPTING TO WRITE A LOOP ##############################################################################################

# redo sim with fixed effects, intercept, but no netting out, and no intercept for FE
# step 1: regression for LAC, no pandemic dummies, with time and country FEs, 2007-2019

varlist <- c("Brazil","Chile","Colombia","Mexico","Peru")

models <- lapply(varlist, function(x) {
  lac_regression_2007_2019_a = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,
                                    data = panel_df_prediction_data %>%
                                    filter(date>="2007-01-01"&date<="2019-12-31"& country==list(a = as.name(x))))

  intercept_a = as.numeric(tidy(lac_regression_2007_2019_a)[1,2])
  inflation_coef_a = as.numeric(tidy(lac_regression_2007_2019_a)[2,2])
  gdp_coef_a = as.numeric(tidy(lac_regression_2007_2019_a)[3,2])
  lag_rate_coef_a = as.numeric(tidy(lac_regression_2007_2019_a)[4,2])
})

models[[1]]

View(models[[2]]) 
#   
#   

# dynamic
dynamic_lac_sim = data.frame()
for(a in c("Brazil","Chile","Colombia","Peru","Mexico")){ #for each one of these countries
  
  lac_regression_2007_2019_a = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,
                                    data = panel_df_prediction_data %>% 
                                      filter(date>="2007-01-01"&date<="2019-12-31"&country==list(a = as.name(a))))
  summary(lac_regression_2007_2019_a)
  
  intercept_a = as.numeric(tidy(lac_regression_2007_2019_a)[1.2])
  inflation_coef_a = as.numeric(tidy(lac_regression_2007_2019_a)[2,2])
  gdp_coef_a = as.numeric(tidy(lac_regression_2007_2019_a)[3,2])
  lag_rate_coef_a = as.numeric(tidy(lac_regression_2007_2019_a)[4,2])
}

  
  base_df_a = panel_df_prediction_data %>% #create a base df for that country
    filter(country==a&date>="2020-01-01"&date<="2022-03-31") %>% #and filter that dataset for the "pandemic period"
    select(date,country,CPI,gdp_output_gap,lag_rate) %>%
    mutate(country_factor=case_when(
      country=="Brazil"~0, #"country effect" (coef for a country). brazil is reference, so it's coef=0
      country=="Chile"~-0.271193, #all these were updated on 7/25/22 (no sig changes from prev version)
      country=="Colombia"~-0.207733,
      country=="Mexico"~-0.139415,
      country=="Peru"~-0.223970 
    ),
    country_factor=as.numeric(country_factor))
  
  base_df$lag_rate_sim = ifelse(base_df$date=="2020-03-01",base_df$lag_rate[1],NA) #for the first "pandemic" qtr, the "sim" rate is going to be the same as the lagged rate
  base_df$predicted_rate = NA #create a new column for the predicted rate
  for(i in 1:nrow(base_df)){ # now, for every yr-qtr,
    base_df$predicted_rate[i] = (base_df$CPI[i]*inflation_coef)+(base_df$gdp_output_gap[i]*gdp_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef) #re-estimate the taylor rule but include the simulation of the lagged rate
    if(i < nrow(base_df)){
      base_df$lag_rate_sim[i+1]= base_df$predicted_rate[i]
    }
  }
  
  dynamic_lac_sim = bind_rows(dynamic_lac_sim,panel_df_prediction_data %>%
                                filter(country==a&date>="2019-01-01"&date<="2019-12-31") %>%
                                select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>%
                                rename(predicted_rate=rate),
                              base_df)

dynamic_lac_sim_full = bind_rows(dynamic_lac_sim,
                                 dynamic_lac_sim %>%
                                   group_by(date) %>%
                                   summarise(CPI=mean(CPI,na.rm=TRUE),
                                             gdp_output_gap = mean(gdp_output_gap,na.rm=TRUE),
                                             lag_rate=mean(lag_rate,na.rm=TRUE),
                                             predicted_rate=mean(predicted_rate,na.rm=TRUE),
                                             country="Latin America"))
dynamic_lac_sim_full$country = factor(dynamic_lac_sim_full$country,levels=c("Brazil","Chile","Colombia","Mexico","Peru","Latin America")) #this reorders the variables

panel_df_prediction_data = panel_df_prediction_data %>% filter(!is.na(country))

# because Chile and Brazil are missing
# dynamic_lac_sim_full$predicted_rate[dynamic_lac_sim_full$date=="2021-12-01"&dynamic_lac_sim_full$country=="Latin America"] = 2.19225374

## 08/01/22: This *was* figure 3, now it is figure 8 ##

fig8 = ggplot(dynamic_lac_sim_full %>% 
                rowwise() %>% 
                mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% 
                ungroup(),aes(x=date)) +
  geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) +
  geom_line(data=panel_df_prediction_data %>% filter(!is.na(country)) %>%  
              rowwise() %>% 
              mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% 
              ungroup() %>% 
              filter(date<="2022-03-31"&date>="2019-01-01"&country%in%c("Brazil","Chile","Colombia","Peru","Mexico")),
            aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(data=panel_df_prediction_data %>% filter(!is.na(country)) %>%  rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(date<="2022-03-31"&date>="2019-01-01"&country%in%c("Brazil","Chile","Colombia","Peru","Mexico")) %>% group_by(date) %>% summarise(rate=mean(rate,na.rm=TRUE),country="Latin America"),
            aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(data=daily_rates %>% filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2022-03-30"),aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(data=daily_rates %>% filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2022-03-30") %>% group_by(date) %>% summarise(rate=mean(rate,na.rm=TRUE)) %>% ungroup() %>% mutate(country="Latin America"),aes(y=rate,colour="Actual Rate"),size=2) +
  facet_wrap(~~fct_relevel(country,"Brazil","Chile","Colombia","Mexico","Peru","Latin America"))+
  scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("Actual Rate"="red","Predicted Rate"="blue")) +
  scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2022-03-31","2022-06-30"),
                              format="%Y-%m-%d"),minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31","2022-03-31","2022-06-30"),
                                                                      format="%Y-%m-%d"),labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
  labs(x="Quarter",y="Policy Rate (%)",caption="Sources: Bank for International Settlements; CEIC\nNote:Predicted rates based on dynamic simulation of equation shown in Table 1, column 1, with intercept and country fixed effects\nset to zero.  In Latin America panel, actual and predicted rates are the mean average of the country-specific rates shown in the other\nfive panels.") +
  theme(text = element_text(size=30),axis.text.x = element_text(size=25,angle=45,hjust=1),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
        legend.position=c(.5,.9),legend.background=element_blank(),legend.title=element_blank())
ggsave(plot=fig8,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/fig8_LA_sims_20220801.png"),
       width=17,height=15,units="in")
