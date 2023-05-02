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

#load(paste0(data_folder,"Processing/3_Taylor_Rule_Spec.RData"))

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
  filter(country %in% c("Brazil","Colombia","Chile","Mexico","Peru")) %>%
  mutate(country=as.character(country))

# scaling the charts is very annoying in this code, takes lots of trial and error with the secondary y-axis
myPlot = function(data){

  data = data %>%
    #updated 4.24.23
    #mutate(exch_rate=exch_rate/exch_rate[date==min(date,na.rm=TRUE)]*100) %>%
    group_by(country) %>%
    mutate(exch_rate=(exch_rate/exch_rate[date=="2020-01-06"])*100) %>% 
    ungroup()

  scaleFactor = min(data$exch_rate,na.rm=TRUE)/max(data$rate,na.rm=TRUE)
  Country = data$country
  minyear = year(data$date[1])


  p = ggplot(data,aes(x=date)) +
    geom_line(aes(y=rate,colour="black"),size=2) +
    #geom_line(aes(y=(exch_rate-90)/10,colour="red"),size=2) +
    geom_line(aes(y=(exch_rate-92)/4,colour="red"),size=2) +
    
    scale_x_date(date_minor_break="1 month",date_labels="%Y-%m") +
    scale_y_continuous(name=ifelse(minyear==2020,"Policy rate (%)",""),
                       sec.axis=sec_axis(~(.*4)+92,
                                         
                       #sec.axis=sec_axis(~(.*10)+10,
                                         #name=ifelse(minyear==2020,"","FX value of US dollar")),
                       name="Index"),
                       limits = c(0, 15)) +
    theme(text = element_text(size=30),axis.title.x=element_blank(),axis.text.x = element_text(size=23),axis.text.y = element_text(size=23),axis.title.y=element_text(size=23),plot.caption = element_text(face="italic",hjust = 0,size=23),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))+
    ggtitle(Country) +
    scale_color_manual(breaks=c("black","red"),labels=c("Policy rate","FX value"),values=c("black","red")) +
    theme(legend.position = "none")

  p

}

# Code below makes Figure 1
# 2020 to present
plot_data = lac_data_daily %>% filter(date>="2020-01-01" & date <= "2022-12-31") %>% group_split(country) # creates a list of data frames by splitting on the given variable
plot_list_2020 = lapply(plot_data,myPlot)

p1 = add_sub(plot_grid(plotlist=plot_list_2020,
                       ncol=1),"Source: Bank for International Settlements\nNote: Exchange rates against dollar (in red) indexed Jan. 6, 2020 = 100; an increase indicates depreciation.  \nPolicy interest rates in black. ",x=0,hjust=0)

# ggsave(plot=p1,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/daily_policy_exchange_rate_2020.png"),
#        width=20,height=10,units="in",dpi=72)
ggsave(plot=p1,filename=paste0(charts_folder,"Scatterplots/2023-04-11_UpdatedCharts/daily_policy_exchange_rate_2020_20230425.png"),
       width=10,height=20,units="in",dpi=72)

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
ggsave(plot=ggdraw(p2),filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/fig1daily_policy_exchange_rate_combined_2008_2020_20230405.png"),
       width=20,height=30,units="in")

# LA average of Fig 1
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

# 2020 to present
plot_data = lac_data_daily %>% filter(date>="2020-01-01") %>% group_by(date) %>% summarise(rate=mean(rate,na.rm=TRUE),exch_rate=mean(exch_rate,na.rm=TRUE))
plot_list_2020 = myPlot(plot_data)

# July 2008 to July 2009
plot_data = lac_data_daily %>% filter(date>="2008-07-01"&date<="2009-07-01") %>% group_by(date) %>% summarise(rate=mean(rate,na.rm=TRUE),exch_rate=mean(exch_rate,na.rm=TRUE))
plot_list_2008 = myPlot(plot_data)

p2 = add_sub(plot_grid(plot_list_2020,plot_list_2008,
                       ncol=2),"Source: Bank for International Settlements\nNote: Red plots FX value indexed where first shown date equals 100.\nAn increase indicates depreciation of the Latin American currency.\nAverages of data for Brazil, Chile, Colombia, Mexico, and Peru.",x=0,hjust=0,size=25)

ggsave(plot=ggdraw(p2),filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/daily_policy_exchange_rate_combined_2008_2020_LA_average.png"),
       width=15,height=10,units="in")


# monthly scatterplot
monthly_scatter = policy_exchange_df  %>%
  filter(date>="2019-12-01"&date<="2020-06-30"&country%in%panel_df$country) %>%
  group_by(year(date),month(date),country) %>%
  summarise(rate=rate[n()],
            exch_rate=exch_rate[n()]) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(change_policy_rate=rate[`year(date)`==2020&`month(date)`==4]-rate[`year(date)`==2020&`month(date)`==1],
            change_exchange_rate=exch_rate[`year(date)`==2020&`month(date)`==4]/exch_rate[`year(date)`==2020&`month(date)`==1]-1) %>%
  ungroup() %>%
  mutate(advanced="Advanced Economies",
            advanced=ifelse(country%in%emes|country%in%c("South Korea","Czech Republic"),"Non-LA EMEs",advanced),
            advanced=ifelse(country%in%c("Brazil","Chile","Mexico","Colombia","Peru"),"Latin America",advanced))

monthly_scatter$change_exchange_rate[monthly_scatter$country=="United States"]=NA
monthly_scatter$advanced = factor(monthly_scatter$advanced,levels=c("Advanced Economies","Non-LA EMEs","Latin America"))

# below code makes Figure 4
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

ggplot(monthly_scatter,aes(x=change_exchange_rate,y=change_policy_rate,colour=advanced)) +
  stat_smooth(method="lm",inherit.aes = FALSE,data=subset(monthly_scatter,advanced!="Advanced Economies"),aes(x=change_exchange_rate,y=change_policy_rate)) +
  geom_point(size=3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept= 0) +
  geom_label_repel(aes(label = country)) +
  scale_x_continuous(labels=scales::percent) +
  stat_poly_eq(formula=y~x,data=subset(monthly_scatter,advanced!="Advanced Economies"),aes(x=change_exchange_rate,y=change_policy_rate,label=paste(..eq.label..,"p-value:",..p.value..,sep="~~~")),colour="black",parse=TRUE,label.y="top",label.x="right") +
  labs(y="Percentage point change in policy rate",x="Percent change FX value of US dollar",colour="Classification",caption="Source: Bank for International Settlements\nNote: Line-of-best-fit and regression equation are estimated for non-LA EMEs and Latin America alone.") +
  theme(text = element_text(size=17),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))


#monthly_scatter = policy_exchange_df  %>%
#  filter(date>="2019-12-01"&date<="2020-06-30"&country%in%panel_df$country) %>%
#  group_by(year(date),month(date),country) %>%
#  summarize(rate=mean(rate,na.rm=TRUE),
#            exch_rate=mean(exch_rate,na.rm=TRUE)) %>%
#  ungroup() %>%
#  group_by(country) %>%
#  summarise(change_policy_rate=(rate[`year(date)`==2020&`month(date)`==4]-rate[`year(date)`==2020&`month(date)`==1])/abs(rate[`year(date)`==2020&`month(date)`==1]),
#            change_exchange_rate=exch_rate[`year(date)`==2020&`month(date)`==4]/exch_rate[`year(date)`==2020&`month(date)`==1]-1) %>%
#  ungroup() %>%
#  mutate(advanced="Advanced Economies",
#         advanced=ifelse(country%in%emes|country%in%c("South Korea","Czech Republic"),"Non-LA EMEs",advanced),
#         advanced=ifelse(country%in%c("Brazil","Chile","Mexico","Colombia","Peru"),"Latin America",advanced),
#         change_policy_rate=ifelse(is.nan(change_policy_rate),0,change_policy_rate))

#monthly_scatter$change_exchange_rate[monthly_scatter$country=="United States"]=NA
#monthly_scatter$advanced = factor(monthly_scatter$advanced,levels=c("Advanced Economies","Non-LA EMEs","Latin America"))

#ggplot(monthly_scatter,aes(x=change_exchange_rate,y=change_policy_rate,colour=advanced)) +
#  stat_smooth(method="lm",inherit.aes = FALSE,data=subset(monthly_scatter,advanced!="Advanced Economies"),aes(x=change_exchange_rate,y=change_policy_rate)) +
#  geom_point(size=3) +
#  geom_hline(yintercept = 0) +
#  geom_vline(xintercept= 0) +
#  geom_label_repel(aes(label = country)) +
#  scale_x_continuous(labels=scales::percent) +
#  scale_y_continuous(labels=scales::percent) +
#  stat_poly_eq(formula=y~x,data=subset(monthly_scatter,advanced!="Advanced Economies"),aes(x=change_exchange_rate,y=change_policy_rate,label=paste(..eq.label..,"p-value:",..p.value..,sep="~~~")),colour="black",parse=TRUE,label.y="top",label.x="right") +
#  labs(y="Proportional change in policy rate",x="Percent change FX value of US dollar",colour="Classification",caption="Source: Bank for International Settlements\nNote: Line-of-best-fit and regression equation are estimated for non-LA EMEs and Latin America alone.") +
#  theme(text = element_text(size=17),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

# 2008-2009
monthly_scatter = policy_exchange_df  %>%
  filter(date>="2008-07-01"&date<="2009-02-28"&country%in%panel_df$country) %>%
  group_by(year(date),month(date),country) %>%
  summarise(rate=rate[n()],
            exch_rate=exch_rate[n()]) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(change_policy_rate=rate[`year(date)`==2008&`month(date)`==10]-rate[`year(date)`==2008&`month(date)`==7],
            change_exchange_rate=exch_rate[`year(date)`==2008&`month(date)`==10]/exch_rate[`year(date)`==2008&`month(date)`==7]-1) %>%
  ungroup() %>%
  mutate(advanced="Advanced Economies",
         advanced=ifelse(country%in%emes|country%in%c("South Korea","Czech Republic"),"Non-LA EMEs",advanced),
         advanced=ifelse(country%in%c("Brazil","Chile","Mexico","Colombia","Peru"),"Latin America",advanced))

monthly_scatter$change_exchange_rate[monthly_scatter$country=="United States"]=NA
monthly_scatter$advanced = factor(monthly_scatter$advanced,levels=c("Advanced Economies","Non-LA EMEs","Latin America"))

# Below makes Figure 5
ggplot(monthly_scatter %>% filter(advanced!="Advanced Economies"),aes(x=change_exchange_rate,y=change_policy_rate,colour=advanced)) +
  stat_smooth(method="lm",inherit.aes = FALSE,aes(x=change_exchange_rate,y=change_policy_rate),se=TRUE) +
  geom_point(size=3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept= 0) +
  geom_label_repel(aes(label = country),size=9) +
  scale_x_continuous(labels=scales::percent) +
  stat_poly_eq(formula=y~x,aes(x=change_exchange_rate,y=change_policy_rate,label=paste(..eq.label..,"p-value:",round(..p.value..,2),sep="~~~")),size=10,color="black",parse=TRUE,label.y="top",label.x="left") +
  labs(y="Percentage point change in policy rate",x="Percent change in exchange rate (local currency per dollar)",colour="Classification",caption="Source: Bank for International Settlements") +
  theme(text = element_text(size=30),axis.text.x = element_text(size=25),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

#ggplot(monthly_scatter,aes(x=change_exchange_rate,y=change_policy_rate,colour=advanced)) +
#  stat_smooth(method="lm",inherit.aes = FALSE,data=subset(monthly_scatter,advanced!="Advanced Economies"),aes(x=change_exchange_rate,y=change_policy_rate),se=TRUE) +
#  geom_point(size=3) +
#  geom_hline(yintercept = 0) +
#  geom_vline(xintercept= 0) +
#  geom_label_repel(aes(label = country)) +
#  scale_x_continuous(labels=scales::percent) +
#  stat_poly_eq(formula=y~x,data=subset(monthly_scatter,advanced!="Advanced Economies"),aes(x=change_exchange_rate,y=change_policy_rate,label=paste(..eq.label..,"p-value:",..p.value..,sep="~~~")),color="black",parse=TRUE,label.y="top",label.x="left") +
#  labs(y="Percentage point change in policy rate",x="Percent change FX value of US Dollar",colour="Classification",caption="Source: Bank for International Settlements\nNote: Line-of-best-fit and regression equation are estimated for non-LA EMEs and Latin America alone.") +
#  theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

#monthly_scatter = policy_exchange_df  %>%
#  filter(date>="2008-07-01"&date<="2009-02-28"&country%in%panel_df$country) %>%
#  group_by(year(date),month(date),country) %>%
#  summarize(rate=mean(rate,na.rm=TRUE),
#            exch_rate=mean(exch_rate,na.rm=TRUE)) %>%
#  ungroup() %>%
#  group_by(country) %>%
#  summarize(change_policy_rate=(rate[`year(date)`==2008&`month(date)`==10]-rate[`year(date)`==2008&`month(date)`==7])/abs(rate[`year(date)`==2008&`month(date)`==7]),
#            change_exchange_rate=exch_rate[`year(date)`==2008&`month(date)`==10]/exch_rate[`year(date)`==2008&`month(date)`==7]-1) %>%
#  ungroup() %>%
#  mutate(advanced="Advanced Economies",
#         advanced=ifelse(country%in%emes|country%in%c("South Korea","Czech Republic"),"Non-LA EMEs",advanced),
#         advanced=ifelse(country%in%c("Brazil","Chile","Mexico","Colombia","Peru"),"Latin America",advanced))

#monthly_scatter$change_exchange_rate[monthly_scatter$country=="United States"]=NA
#monthly_scatter$advanced = factor(monthly_scatter$advanced,levels=c("Advanced Economies","Non-LA EMEs","Latin America"))

#ggplot(monthly_scatter,aes(x=change_exchange_rate,y=change_policy_rate,colour=advanced)) +
#  stat_smooth(method="lm",inherit.aes = FALSE,data=subset(monthly_scatter,advanced!="Advanced Economies"),aes(x=change_exchange_rate,y=change_policy_rate),se=TRUE) +
#  geom_point(size=3) +
#  geom_hline(yintercept = 0) +
#  geom_vline(xintercept= 0) +
#  geom_label_repel(aes(label = country)) +
#  scale_x_continuous(labels=scales::percent) +
#  scale_y_continuous(labels=scales::percent) +
#  stat_poly_eq(formula=y~x,data=subset(monthly_scatter,advanced!="Advanced Economies"),aes(x=change_exchange_rate,y=change_policy_rate,label=paste(..eq.label..,"p-value:",..p.value..,sep="~~~")),color="black",parse=TRUE,label.y="top",label.x="left") +
#  labs(y="Proportional change in policy rate",x="Percent change FX value of US Dollar",colour="Classification",caption="Source: Bank for International Settlements\nNote: Line-of-best-fit and regression equation are estimated for non-LA EMEs and Latin America alone.") +
#  theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))


# chart showing output gap and policy rate during the pandemic
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

# Below makes Figure 6
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

#ggplot(df_scatter_quarterly %>% filter(country!="Philippines"),aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020,colour=advanced)) +
#  stat_smooth(method="lm",inherit.aes=FALSE,data=subset(df_scatter_quarterly,advanced!="Advanced Economies"),aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020)) +
#  geom_point(size=3) +
#  geom_hline(yintercept = 0) +
#  geom_vline(xintercept= 0) +
#  geom_label_repel(aes(label = country)) +
#  #scale_y_continuous(labels=scales::percent) +
#  stat_poly_eq(formula=y~x,data=subset(df_scatter_quarterly,advanced!="Advanced Economies"),aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020,color=NA,label=paste(..eq.label..,"p-value:",..p.value..,sep="~~~")),color="black",parse=TRUE) +
#  labs(y="Percentage point change in policy rate",x="Percentage point change in GDP output gap",colour="Classification",caption="Source: Bank for International Settlements; CEIC\nNote: Line-of-best-fit and regression equation are estimated for non-LA EMEs and Latin America alone.") +
#  theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

#df_scatter_quarterly = panel_df %>%
#  filter(date>="2019-12-01") %>%
#  drop_na(rate,core_inflation) %>%
#  group_by(country) %>%
#  summarize(total_cpi_dec_2020_q3 = (CPI[n()]-CPI[date=="2020-12-01"]),
#            core_cpi_dec_2020_q3 = (core_inflation[n()]-core_inflation[date=="2020-12-01"]),
#            gdp_output_gap_dec_2020_q3 = (gdp_output_gap[date=="2020-06-01"]-gdp_output_gap[date=="2019-12-01"]),
#            policy_rate_dec_2020_q3 = (rate[n()]-rate[date=="2020-12-01"]),
#            policy_rate_dec_2019_q2_2020 = (rate[date=="2020-06-01"]-rate[date=="2019-12-01"])/abs(rate[date=="2019-12-01"]),
#            advanced=advanced[1]) %>%
#  mutate(advanced=ifelse(advanced!="Non-LA EMEs","Advanced Economies",advanced),
#         advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced),
#         policy_rate_dec_2020_q3 = ifelse(is.infinite(policy_rate_dec_2020_q3),NA,policy_rate_dec_2020_q3),
#         policy_rate_dec_2020_q3 = ifelse(is.nan(policy_rate_dec_2020_q3),0,policy_rate_dec_2020_q3),
#         policy_rate_dec_2019_q2_2020 = ifelse(is.infinite(policy_rate_dec_2019_q2_2020),NA,policy_rate_dec_2019_q2_2020),
#         policy_rate_dec_2019_q2_2020 = ifelse(is.nan(policy_rate_dec_2019_q2_2020),0,policy_rate_dec_2019_q2_2020)) %>%
#  ungroup()

#df_scatter_quarterly$advanced = factor(df_scatter_quarterly$advanced,levels=c("Advanced Economies","Non-LA EMEs","Latin America"))

#ggplot(df_scatter_quarterly %>% filter(country!="Philippines"),aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020,colour=advanced)) +
#  stat_smooth(method="lm",inherit.aes=FALSE,data=subset(df_scatter_quarterly,advanced!="Advanced Economies"),aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020)) +
#  geom_point(size=3) +
#  geom_hline(yintercept = 0) +
#  geom_vline(xintercept= 0) +
#  geom_label_repel(aes(label = country)) +
#  scale_y_continuous(labels=scales::percent) +
#  stat_poly_eq(formula=y~x,data=subset(df_scatter_quarterly,advanced!="Advanced Economies"),aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020,color=NA,label=paste(..eq.label..,"p-value:",..p.value..,sep="~~~")),color="black",parse=TRUE) +
#  labs(y="Proportional change in policy rate",x="Percentage point change in GDP output gap",colour="Classification",caption="Source: Bank for International Settlements; CEIC\nNote: Line-of-best-fit and regression equation are estimated for non-LA EMEs and Latin America alone.") +
#  theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))


# chart showing output gap and policy rate during the GFC
weird_df = bind_rows(panel_df %>% mutate(type="q"),
                     panel_df_monthly %>% mutate(type="m")) %>%
  group_by(country) %>%
  summarise(change_outputgap_2008_2009=(gdp_output_gap[date=="2009-03-01"&type=="q"]-gdp_output_gap[date=="2008-09-01"&type=="q"]),
            change_rate_2008_2009=(rate[date=="2009-03-01"&type=="m"]-rate[date=="2008-08-01"&type=="m"]),
            advanced=advanced[1]) %>%
  mutate(advanced=ifelse(advanced!="Non-LA EMEs","Advanced Economies",advanced),
         advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced),
         change_rate_2008_2009=ifelse(is.infinite(change_rate_2008_2009),NA,change_rate_2008_2009),
         change_rate_2008_2009=ifelse(is.nan(change_rate_2008_2009),0,change_rate_2008_2009))

weird_df$advanced = factor(weird_df$advanced,levels=c("Advanced Economies","Non-LA EMEs","Latin America"))

# Below makes Figure 7
ggplot(weird_df %>% filter(country!="Philippines"&advanced!="Advanced Economies"),aes(x=change_outputgap_2008_2009,y=change_rate_2008_2009,colour=advanced)) +
  stat_smooth(method="lm",inherit.aes=FALSE,aes(x=change_outputgap_2008_2009,y=change_rate_2008_2009)) +
  geom_point(size=3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept= 0) +
  geom_label_repel(aes(label = country),size=9) +
  #scale_y_continuous(labels=scales::percent) +
  stat_poly_eq(formula=y~x,aes(x=change_outputgap_2008_2009,y=change_rate_2008_2009,color=NA,label=paste(..eq.label..,"p-value:",round(..p.value..,2),sep="~~~")),size=9,color="black",parse=TRUE,label.y="top",label.x="right") +
  labs(y="Percentage point change in policy rate",x="Percentage point change in GDP output gap",colour="Classification",caption="Source: Bank for International Settlements; CEIC") +
  theme(text = element_text(size=20),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20),axis.title.y=element_text(size=20),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

#ggplot(weird_df %>% filter(country!="Philippines"),aes(x=change_outputgap_2008_2009,y=change_rate_2008_2009,colour=advanced)) +
#  stat_smooth(method="lm",inherit.aes=FALSE,data=subset(weird_df,advanced!="Advanced Economies"),aes(x=change_outputgap_2008_2009,y=change_rate_2008_2009)) +
#  geom_point(size=3) +
#  geom_hline(yintercept = 0) +
#  geom_vline(xintercept= 0) +
#  geom_label_repel(aes(label = country)) +
#  #scale_y_continuous(labels=scales::percent) +
#  stat_poly_eq(formula=y~x,data=subset(weird_df,advanced!="Advanced Economies"),aes(x=change_outputgap_2008_2009,y=change_rate_2008_2009,color=NA,label=paste(..eq.label..,"p-value:",..p.value..,sep="~~~")),color="black",parse=TRUE,label.y="top",label.x="right") +
#  labs(y="Percentage point change in policy rate",x="Percentage point change in GDP output gap",colour="Classification",caption="Source: Bank for International Settlements; CEIC\nNote: Line-of-best-fit and regression equation are estimated for non-LA EMEs and Latin America alone.") +
#  theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

#weird_df = bind_rows(panel_df %>% mutate(type="q"),
#                     panel_df_monthly %>% mutate(type="m")) %>%
#  group_by(country) %>%
#  summarize(change_outputgap_2008_2009=(gdp_output_gap[date=="2009-03-01"&type=="q"]-gdp_output_gap[date=="2008-09-01"&type=="q"]),
#            change_rate_2008_2009=(rate[date=="2009-03-01"&type=="m"]-rate[date=="2008-08-01"&type=="m"])/abs(rate[date=="2008-08-01"&type=="m"]),
#            advanced=advanced[1]) %>%
#  mutate(advanced=ifelse(advanced!="Non-LA EMEs","Advanced Economies",advanced),
#         advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced),
#         change_rate_2008_2009=ifelse(is.infinite(change_rate_2008_2009),NA,change_rate_2008_2009),
#         change_rate_2008_2009=ifelse(is.nan(change_rate_2008_2009),0,change_rate_2008_2009))

#weird_df$advanced = factor(weird_df$advanced,levels=c("Advanced Economies","Non-LA EMEs","Latin America"))

#ggplot(weird_df %>% filter(country!="Philippines"),aes(x=change_outputgap_2008_2009,y=change_rate_2008_2009,colour=advanced)) +
#  stat_smooth(method="lm",inherit.aes=FALSE,data=subset(weird_df,advanced!="Advanced Economies"),aes(x=change_outputgap_2008_2009,y=change_rate_2008_2009)) +
#  geom_point(size=3) +
#  geom_hline(yintercept = 0) +
#  geom_vline(xintercept= 0) +
#  geom_label_repel(aes(label = country)) +
#  scale_y_continuous(labels=scales::percent) +
#  stat_poly_eq(formula=y~x,data=subset(weird_df,advanced!="Advanced Economies"),aes(x=change_outputgap_2008_2009,y=change_rate_2008_2009,color=NA,label=paste(..eq.label..,"p-value:",..p.value..,sep="~~~")),color="black",parse=TRUE,label.y="top",label.x="right") +
#  labs(y="Proportional change in policy rate",x="Percentage point change in GDP output gap",colour="Classification",caption="Source: Bank for International Settlements; CEIC\nNote: Line-of-best-fit and regression equation are estimated for non-LA EMEs and Latin America alone.") +
#  theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

# time series of GDP, inflation, and policy rate
# This code makes Figure 11 (which is now figure 3)
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
    geom_line(data=data,
              aes(x=date,
                  y=(((gdp_output_gap*40)/15)-30),
                  colour="blue"),size=2) +
    scale_x_date(breaks=as.Date(c("2019-03-01","2020-03-01","2021-03-01","2022-03-01","2023-03-01"),
                                format="%Y-%m-%d"),
                                limits = as.Date(c("2019-03-01","2022-12-31")),
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
    theme(legend.position = "none") +
    geom_hline(yintercept = 0)

  if(Country=="Brazil"|Country=="Mexico"|Country=="Peru"){
    p = p +
      geom_line(data=rate_data,
                aes(x=date,
                    #y=(rate-5)/.166667,
                    y=rate,
                    colour="black"),
                size=2) +
      scale_y_continuous(name="Output gap (%)",
                         limits=c(0,15),
                         #limits=c(-60,60),
                         #sec.axis=sec_axis(~(.*.166667)+5,
                         sec.axis = sec_axis(~(.)),
                         name=ifelse(minyear==2019,"Policy rate (%)\n","Policy rate (%)\n"))
  }
  if(Country!="Brazil"&Country!="Mexico"&Country!="Peru"){
    p = p +
      geom_line(data=rate_data,
                aes(x=date,y=(rate-4)/.2, colour="black"),
                size=2) +
      scale_y_continuous(name="Output gap (%)",
                         limits=c(-50,50),
                         sec.axis=sec_axis(~(.*.2)+4,
                         labels=scales::number_format(accuracy=.01),name=ifelse(minyear==2019,"Policy rate (%)\n","Policy rate (%)\n")))
  }

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
    geom_line(data=rate_data,aes(x=date,y=rate,colour="black"),size=2) +
    geom_line(data=data,aes(x=date,y=CPI,colour="green"),size=2) +
    geom_line(data=data,aes(x=date,y=core_inflation,colour="purple"),size=2) +
    scale_x_date(breaks=as.Date(c("2019-03-01","2020-03-01","2021-03-01","2022-03-01","2023-03-01"),
                                format="%Y-%m-%d"),
                                limits = as.Date(c("2019-03-01","2022-12-31")),
                                labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
    scale_y_continuous(name="",
                       labels=scales::number_format(accuracy=ifelse(Country=="Brazil"|Country=="Mexico"|Country=="Peru",.1,.01)[1]),
                       limits=c(0,15),
                       # limits=c(0,ifelse((Country=="Brazil"|Country=="Chile"|Country=="Colombia"),15,
                       #                   ifelse(Country=="Mexico"|Country=="Peru",10,8))),
                       sec.axis=sec_axis(~.,name=ifelse(minyear==2019,"12-month inflation rate (%)","12-month inflation rate (%)"))) +
    theme(text = element_text(size=30),axis.title.x=element_blank(),axis.text.x = element_text(size=25),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))+
    ggtitle(Country) +
    geom_hline(yintercept=0) +
    scale_color_manual(breaks=c("black","green","purple"),labels=c("Policy rate","Total Inflation","Core Inflation"),values=c("black","red","purple")) +
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

# regression in levels, 2007-2021, total inflation
#model_quarterly_gdp_total_advanced = felm(rate~CPI+gdp_output_gap+lag_rate+pandemic+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Advanced Economy"))
#model_quarterly_gdp_total_eme = felm(rate~CPI+gdp_output_gap+lag_rate+pandemic+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_total_lac = felm(rate~CPI+gdp_output_gap+lag_rate+pandemic+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0(charts_folder,"Scatterplots/2021-11-08_Paper_Charts/total_inflation_gdp_panel_levels_2007_2021.html"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2021, Time and Country FE, Pandemic Interaction Terms",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0("total_inflation_gdp_panel_levels_2007_2021.doc"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2021, Time and Country FE, Pandemic Interaction Terms",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))

#summary(lm_robust(rate~CPI+gdp_output_gap+lag_rate+pandemic+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate+factor(date)+factor(country),data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Latin America")))
#summary(lm_robust(rate~CPI+gdp_output_gap+lag_rate+pandemic+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate+factor(date)+factor(country),data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Emerging Market Economy")))
#summary(lm_robust(rate~CPI+gdp_output_gap+lag_rate+pandemic+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate+factor(date)+factor(country),data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Advanced Economy")))

# appendix table
#model_quarterly_gdp_core_advanced = felm(rate~core_inflation+gdp_output_gap+lag_rate+pandemic+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Advanced Economy"))
#model_quarterly_gdp_core_eme = felm(rate~core_inflation+gdp_output_gap+lag_rate+pandemic+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_core_lac = felm(rate~core_inflation+gdp_output_gap+lag_rate+pandemic+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0(charts_folder,"Scatterplots/2021-11-08_Paper_Charts/core_inflation_gdp_panel_levels_2007_2021.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2021, Time and Country FE, Pandemic Interaction Terms",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0("core_inflation_gdp_panel_levels_2007_2021.doc"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2021, Time and Country FE, Pandemic Interaction Terms",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))

#summary(lm_robust(rate~core_inflation+gdp_output_gap+lag_rate+pandemic+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate+factor(date)+factor(country),data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Latin America")))
#summary(lm_robust(rate~core_inflation+gdp_output_gap+lag_rate+pandemic+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate+factor(date)+factor(country),data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Advanced Economy")))
#summary(lm_robust(rate~core_inflation+gdp_output_gap+lag_rate+pandemic+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate+factor(date)+factor(country),data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Emerging Market Economy")))


# regression in levels, 2007-2021, total inflation, no pandemic dummy by itself
#model_quarterly_gdp_total_advanced = felm(rate~CPI+gdp_output_gap+lag_rate+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Advanced Economy"))
#model_quarterly_gdp_total_eme = felm(rate~CPI+gdp_output_gap+lag_rate+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_total_lac = felm(rate~CPI+gdp_output_gap+lag_rate+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0(charts_folder,"Scatterplots/2021-11-08_Paper_Charts/total_inflation_gdp_panel_levels_nodummy_2007_2021.html"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2021, Time and Country FE, Pandemic Interaction Terms without pandemic",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0("total_inflation_gdp_panel_levels_nodummy_2007_2021.doc"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2021, Time and Country FE, Pandemic Interaction Terms without pandemic dummy",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#write.csv(rbind(getfe(model_quarterly_gdp_total_advanced) %>% mutate(model="Advanced Economy"),getfe(model_quarterly_gdp_total_eme) %>% mutate(model="Non-LA EMEs"),getfe(model_quarterly_gdp_total_lac) %>% mutate(model="Latin America")),file=paste0(charts_folder,"Scatterplots/2021-11-08_Paper_Charts/fixed_effects_total_inflation_gdp_panel_levels_nodummy_2007_2021.csv"))

#summary(lm_robust(rate~CPI+gdp_output_gap+lag_rate+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate+factor(date)+factor(country),data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Latin America")))
#summary(lm_robust(rate~CPI+gdp_output_gap+lag_rate+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate+factor(date)+factor(country),data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Emerging Market Economy")))
#summary(lm_robust(rate~CPI+gdp_output_gap+lag_rate+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate+factor(date)+factor(country),data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Advanced Economy")))

# appendix table
#model_quarterly_gdp_core_advanced = felm(rate~core_inflation+gdp_output_gap+lag_rate+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Advanced Economy"))
#model_quarterly_gdp_core_eme = felm(rate~core_inflation+gdp_output_gap+lag_rate+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_core_lac = felm(rate~core_inflation+gdp_output_gap+lag_rate+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2021-06-01"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0(charts_folder,"Scatterplots/2021-11-08_Paper_Charts/core_inflation_gdp_panel_levels_nodummy_2007_2021.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2021, Time and Country FE, Pandemic Interaction Terms without pandemic dummy",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0("core_inflation_gdp_panel_levels_nodummy_2007_2021.doc"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2021, Time and Country FE, Pandemic Interaction Terms without pandemic dummy",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#write.csv(rbind(getfe(model_quarterly_gdp_core_advanced) %>% mutate(model="Advanced Economy"),getfe(model_quarterly_gdp_core_eme) %>% mutate(model="Non-LA EMEs"),getfe(model_quarterly_gdp_core_lac) %>% mutate(model="Latin America")),file=paste0(charts_folder,"Scatterplots/2021-11-08_Paper_Charts/fixed_effects_core_inflation_gdp_panel_levels_nodummy_2007_2021.csv"))



#model_quarterly_gdp_total_advanced = felm(rate~CPI+gdp_output_gap+lag_rate+neer_quarter_perc_change|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Advanced Economy"))
#model_quarterly_gdp_total_eme = felm(rate~CPI+gdp_output_gap+lag_rate+neer_quarter_perc_change|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_total_lac = felm(rate~CPI+gdp_output_gap+lag_rate+neer_quarter_perc_change|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0(results_folder,"Regression Results/2021-11-17_Taylor_Rule_Panel_2007_2019_ExchRates/total_inflation_gdp_panel_levels_exchrates_2007_2019.html"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE, No Pandemic Interaction Terms, Quarterly Percent Change in NEER",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0("total_inflation_gdp_panel_levels_exchrates_2007_2019.doc"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE, No Pandemic Interaction Terms, Quarterly Percent Change in NEER",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#write.csv(rbind(getfe(model_quarterly_gdp_total_advanced) %>% mutate(model="Advanced Economy"),getfe(model_quarterly_gdp_total_eme) %>% mutate(model="Non-LA EMEs"),getfe(model_quarterly_gdp_total_lac) %>% mutate(model="Latin America")),file=paste0(results_folder,"Regression Results/2021-11-17_Taylor_Rule_Panel_2007_2019_ExchRates/fixed_effects_total_inflation_gdp_panel_levels_exchrates_2007_2019.csv"))

# appendix table
#model_quarterly_gdp_core_advanced = felm(rate~core_inflation+gdp_output_gap+lag_rate+neer_quarter_perc_change|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Advanced Economy"))
#model_quarterly_gdp_core_eme = felm(rate~core_inflation+gdp_output_gap+lag_rate+neer_quarter_perc_change|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_core_lac = felm(rate~core_inflation+gdp_output_gap+lag_rate+neer_quarter_perc_change|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0(results_folder,"Regression Results/2021-11-17_Taylor_Rule_Panel_2007_2019_ExchRates/core_inflation_gdp_panel_levels_exchrates_2007_2019.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019 extrapolation, Time and Country FE, No Pandemic Interaction Terms, Quarterly Percent Change in NEER",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0("core_inflation_gdp_panel_levels_exchrates_2007_2019.doc"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE, No Pandemic Interaction Terms, Quarterly Percent Change in NEER",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#write.csv(rbind(getfe(model_quarterly_gdp_core_advanced) %>% mutate(model="Advanced Economy"),getfe(model_quarterly_gdp_core_eme) %>% mutate(model="Non-LA EMEs"),getfe(model_quarterly_gdp_core_lac) %>% mutate(model="Latin America")),file=paste0(results_folder,"Regression Results/2021-11-17_Taylor_Rule_Panel_2007_2019_ExchRates/fixed_effects_core_inflation_gdp_panel_levels_exchrates_2007_2019.csv"))

# add lagged exchange rate change
#model_quarterly_gdp_total_advanced = felm(rate~CPI+gdp_output_gap+lag_rate+neer_quarter_perc_change+lag_neer_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Advanced Economy"))
#model_quarterly_gdp_total_eme = felm(rate~CPI+gdp_output_gap+lag_rate+neer_quarter_perc_change+lag_neer_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_total_lac = felm(rate~CPI+gdp_output_gap+lag_rate+neer_quarter_perc_change+lag_neer_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0(results_folder,"Regression Results/2021-11-17_Taylor_Rule_Panel_2007_2019_ExchRates/total_inflation_gdp_panel_levels_exchrates_lag_2007_2019.html"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE, No Pandemic Interaction Terms, Quarterly Percent Change in NEER + lag of it",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0("total_inflation_gdp_panel_levels_exchrates_lag_2007_2019.doc"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE, No Pandemic Interaction Terms, Quarterly Percent Change in NEER + lag of it",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#write.csv(rbind(getfe(model_quarterly_gdp_total_advanced) %>% mutate(model="Advanced Economy"),getfe(model_quarterly_gdp_total_eme) %>% mutate(model="Non-LA EMEs"),getfe(model_quarterly_gdp_total_lac) %>% mutate(model="Latin America")),file=paste0(results_folder,"Regression Results/2021-11-17_Taylor_Rule_Panel_2007_2019_ExchRates/fixed_effects_total_inflation_gdp_panel_levels_exchrates_lag_2007_2019.csv"))

# appendix table
#model_quarterly_gdp_core_advanced = felm(rate~core_inflation+gdp_output_gap+lag_rate+neer_quarter_perc_change+lag_neer_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Advanced Economy"))
#model_quarterly_gdp_core_eme = felm(rate~core_inflation+gdp_output_gap+lag_rate+neer_quarter_perc_change+lag_neer_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_core_lac = felm(rate~core_inflation+gdp_output_gap+lag_rate+neer_quarter_perc_change+lag_neer_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0(results_folder,"Regression Results/2021-11-17_Taylor_Rule_Panel_2007_2019_ExchRates/core_inflation_gdp_panel_levels_exchrates_lag_2007_2019.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019 extrapolation, Time and Country FE, No Pandemic Interaction Terms, Quarterly Percent Change in NEER + lag of it",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0("core_inflation_gdp_panel_levels_exchrates_lag_2007_2019.doc"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE, No Pandemic Interaction Terms, Quarterly Percent Change in NEER + lag of it",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#write.csv(rbind(getfe(model_quarterly_gdp_core_advanced) %>% mutate(model="Advanced Economy"),getfe(model_quarterly_gdp_core_eme) %>% mutate(model="Non-LA EMEs"),getfe(model_quarterly_gdp_core_lac) %>% mutate(model="Latin America")),file=paste0(results_folder,"Regression Results/2021-11-17_Taylor_Rule_Panel_2007_2019_ExchRates/fixed_effects_core_inflation_gdp_panel_levels_exchrates_lag_2007_2019.csv"))

# replace with real effective exchange rate
# add lagged exchange rate change
#model_quarterly_gdp_total_advanced = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+lag_reer_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Advanced Economy"))
#model_quarterly_gdp_total_eme = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+lag_reer_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_total_lac = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+lag_reer_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0(results_folder,"Regression Results/2021-11-17_Taylor_Rule_Panel_2007_2019_ExchRates/total_inflation_gdp_panel_levels_REER_lag_2007_2019.html"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE, No Pandemic Interaction Terms, Quarterly Percent Change in REER + lag of it",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0("total_inflation_gdp_panel_levels_REER_lag_2007_2019.doc"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE, No Pandemic Interaction Terms, Quarterly Percent Change in REER + lag of it",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#write.csv(rbind(getfe(model_quarterly_gdp_total_advanced) %>% mutate(model="Advanced Economy"),getfe(model_quarterly_gdp_total_eme) %>% mutate(model="Non-LA EMEs"),getfe(model_quarterly_gdp_total_lac) %>% mutate(model="Latin America")),file=paste0(results_folder,"Regression Results/2021-11-17_Taylor_Rule_Panel_2007_2019_ExchRates/fixed_effects_total_inflation_gdp_panel_levels_REER_lag_2007_2019.csv"))

# appendix table
#model_quarterly_gdp_core_advanced = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change+lag_reer_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Advanced Economy"))
#model_quarterly_gdp_core_eme = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change+lag_reer_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_core_lac = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change+lag_reer_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0(results_folder,"Regression Results/2021-11-17_Taylor_Rule_Panel_2007_2019_ExchRates/core_inflation_gdp_panel_levels_REER_lag_2007_2019.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019 extrapolation, Time and Country FE, No Pandemic Interaction Terms, Quarterly Percent Change in REER + lag of it",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0("core_inflation_gdp_panel_levels_REER_lag_2007_2019.doc"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE, No Pandemic Interaction Terms, Quarterly Percent Change in REER + lag of it",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#write.csv(rbind(getfe(model_quarterly_gdp_core_advanced) %>% mutate(model="Advanced Economy"),getfe(model_quarterly_gdp_core_eme) %>% mutate(model="Non-LA EMEs"),getfe(model_quarterly_gdp_core_lac) %>% mutate(model="Latin America")),file=paste0(results_folder,"Regression Results/2021-11-17_Taylor_Rule_Panel_2007_2019_ExchRates/fixed_effects_core_inflation_gdp_panel_levels_REER_lag_2007_2019.csv"))



# predict third quarter GDP for those who don't have it in CEIC yet
# update to Q4 2021
countries_missing_gdp = c(panel_df %>% filter(is.na(gdp_index)&date=="2021-12-01") %>% distinct(country))$country

panel_df_prediction_data = panel_df %>%
  group_by(country) %>%
  arrange(date) %>%
  mutate(qoq_change_gdp = gdp_index/dplyr::lag(gdp_index)-1,
         qoq_change_ip = ip_index/dplyr::lag(ip_index)-1) %>%
  ungroup()

#rsq = c()
#for(countrya in countries_missing_gdp){
#  print(countrya)
#  reg = lm_robust(qoq_change_gdp~qoq_change_ip,data=panel_df_prediction_data %>% filter(country==countrya))
#
#  a = tidy(reg)
#  coef = a[2,2]
#  rsq = c(rsq,reg$r.squared)
#
#  panel_df_prediction_data$gdp_index[panel_df_prediction_data$country==countrya&panel_df_prediction_data$date=="2021-09-01"] =
#    panel_df_prediction_data$gdp_index[panel_df_prediction_data$country==countrya&panel_df_prediction_data$date=="2021-06-01"][1] * (1+(coef*panel_df_prediction_data$qoq_change_ip[panel_df_prediction_data$country==countrya&panel_df_prediction_data$date=="2021-09-01"]))[1]
#
#}

# extend trend
#panel_df_prediction_data = panel_df_prediction_data %>%
#  group_by(country) %>%
#  mutate(gdp_trend=zoo::na.approx(gdp_trend,rule=2,na.rm=FALSE),
#         gdp_output_gap=ifelse(date=="2021-09-01",(gdp_index/gdp_trend-1)*100,gdp_output_gap),
#         pandemic_total_inf = pandemic*CPI,
#         pandemic_core_inf = pandemic*core_inflation,
#         pandemic_lag_rate = pandemic*lag_rate,
#         pandemic_output_gap = pandemic*gdp_output_gap)

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

# new table 1 with six columns
## TABLE 1 AND TABLE 2 ## (updated on 7/21/2022 - bal)
panel_df_prediction_data = panel_df_prediction_data %>%
  mutate(pandemic_reer_change = pandemic*reer_quarter_perc_change)

panel_df_prediction_data$country_group = ifelse(panel_df_prediction_data$country_group=="Emerging Market Economy","Non-LA EMEs",panel_df_prediction_data$country_group)
panel_df_prediction_data$country_group = ifelse(panel_df_prediction_data$country%in%c("South Korea","Czech Republic"),"Non-LA EMEs",panel_df_prediction_data$country_group)

#let's save this dataset while we're at it, why don't we?
save(panel_df_prediction_data,file=paste0(data_folder,"Final/panel_df_prediction_data_20230417.rda"))

#now build table 1
col1 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Latin America"))
col2 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate+pandemic_reer_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2022-12-31"&country_group=="Latin America"))
col3 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change|country + date|0|0,data = panel_df_prediction_data %>% filter(date>"1998-09-30"&date<="2019-12-31"&country_group=="Latin America"))
col4 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Non-LA EMEs"))
col5 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate+pandemic_reer_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2022-12-31"&country_group=="Non-LA EMEs"))
col6 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change|country + date|0|0,data = panel_df_prediction_data %>% filter(date>"1998-09-30"&date<="2019-12-31"&country_group=="Non-LA EMEs"))

htmlreg(list(col1,col2,col3,col4,col5,col6),
        file=paste0(results_folder,"Regression Results/2022-07-21_Taylor_Rule_expanded_LA/LA_and_EME_Table_2022_08_01.html"),
        caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019 extrapolation, Time and Country FE, Quarterly Percent Change in REER",caption.above=TRUE,
        custom.model.names = c("LA, 2007-2019","LA, 2007-2022 Q4","LA, 1998 Q4 - 2019 Q4","EME, 2007-2019","EME, 2007-2022 Q4","EME, 1998 Q4 - 2019 Q4"),stars=c(.01,.05,.1))

wordreg(list(col1,col2,col3,col4,col5,col6),
        file="LA_and_EME_Table_2022_08_01.doc",
        caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019 extrapolation, Time and Country FE, Quarterly Percent Change in REER",caption.above=TRUE,
        custom.model.names = c("LA, 2007-2019","LA, 2007-2022 Q4","LA, 1998 Q4 - 2019 Q4","EME, 2007-2019","EME, 2007-2022 Q4","EME, 1998 Q4 - 2019 Q4"),stars=c(.01,.05,.1))

# df that shows when each country starts
 

# LA average of chart above
plot_data = panel_df_prediction_data %>% filter(date>="2019-01-01"&date<="2021-12-31"&country%in%c("Brazil","Chile","Peru","Mexico","Colombia")) %>% group_by(date) %>% summarise(gdp_output_gap=mean(gdp_output_gap,na.rm=TRUE)) %>% ungroup() %>% mutate(country="Latin America")
plot_list_gdp = myPlot_gdp(plot_data)

plot_data = panel_df_monthly %>% filter(date>="2019-01-01"&country%in%c("Brazil","Chile","Peru","Mexico","Colombia")) %>% group_by(date) %>% summarise(CPI=mean(CPI),core_inflation=mean(core_inflation)) %>% ungroup() %>% mutate(country="Latin America")
plot_list_inflation = myPlot_inflation(plot_data)

p2 = add_sub(plot_grid(plot_list_gdp,plot_list_inflation,
                       ncol=2),"Source: Bank for International Settlements, CEIC; Averages of data for Brazil, Chile, Colombia, Mexico, and Peru.",x=0,hjust=0,size=25)

ggsave(plot=ggdraw(p2),filename=paste0(charts_folder,"Scatterplots/2023-03-27_UpdatedCharts/fig6_daily_policy_inflation_gdp_LA_average_20230424.png"),
       width=20,height=8,units="in")



# current inflation charts (Figure 3)
df_scatter_monthly = panel_df_monthly %>%
  filter(date>="2019-12-01") %>%
  group_by(country) %>%
  summarise(total_cpi_change = CPI[date=="2022-12-01"]-CPI[date=="2021-01-01"],
            core_cpi_change = core_inflation[date=="2022-12-01"]-core_inflation[date=="2021-01-01"],
            policy_rate_pp_change = rate[date=="2022-12-01"]-rate[date=="2021-01-01"],
            policy_rate_prop_change = (rate[date=="2022-12-01"]-rate[date=="2021-01-01"])/abs(rate[date=="2021-01-01"]),
            advanced=advanced[1]) %>%  
    
    #         total_cpi_change = (dplyr::last(na.omit(CPI))-CPI[date=="2021-06-01"]),
    #         core_cpi_change = (dplyr::last(na.omit(core_inflation))-core_inflation[date=="2021-06-01"]),
    #         policy_rate_pp_change = (dplyr::last(na.omit(rate))-rate[date=="2021-06-01"]),
    #         policy_rate_prop_change = (dplyr::last(na.omit(rate))-rate[date=="2021-06-01"])/abs(rate[date=="2021-06-01"]),
    #          advanced=advanced[1]) %>%   
  
  mutate(advanced=ifelse(advanced!="Non-LA EMEs","Advanced Economies",advanced),
         advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced),
         policy_rate_prop_change = ifelse(is.infinite(policy_rate_prop_change),NA,policy_rate_prop_change),
         policy_rate_prop_change = ifelse(is.nan(policy_rate_prop_change),0,policy_rate_prop_change)) %>%
  ungroup()

df_scatter_monthly$advanced = factor(df_scatter_monthly$advanced,levels=c("Non-LA EMEs","Advanced Economies","Latin America"))

# Below makes Figure 12 (but you may want to remove Turkey)
fig12 <- ggplot(df_scatter_monthly %>% filter(advanced!="Advanced Economies"&country!="Turkey"),
                aes(x=total_cpi_change,
                    y=policy_rate_pp_change,
                    colour=advanced)) +
  stat_smooth(method="lm",inherit.aes=FALSE,aes(x=total_cpi_change,y=policy_rate_pp_change)) +
  geom_point(size=3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept= 0) +
  geom_label_repel(aes(label = country),size=9) +
  stat_poly_eq(formula=y~x,aes(x=total_cpi_change,y=policy_rate_pp_change,color=NA,label=paste(..eq.label..,"p-value:",round(..p.value..,2),sep="~~~")),size=9,color="black",parse=TRUE) +
  labs(y="Percentage point change in policy rate (December 2022 - January 2021)",
       x="Percentage point change in 12-month inflation (December 2022 - January 2021)",
       colour="Classification",caption="Source: Bank for International Settlements; CEIC") +
  theme(text = element_text(size=20),axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title.y=element_text(size=15),
        plot.caption = element_text(face="italic",hjust = 0,size=20),
        panel.background = element_rect(fill = "white",colour="black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

ggsave(fig12,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/fig12_12m_change_inflation_and_pr_20230425.png"),
       width=15,height=8,units="in")

# ggplot(df_scatter_monthly,aes(x=total_cpi_change,y=policy_rate_pp_change,colour=advanced)) +
#  stat_smooth(method="lm",data=subset(df_scatter_monthly,advanced!="Advanced Economies"),inherit.aes=FALSE,aes(x=total_cpi_change,y=policy_rate_pp_change)) +
#  geom_point(size=3) +
#  geom_hline(yintercept = 0) +
#  geom_vline(xintercept= 0) +
#  geom_label_repel(aes(label = country)) +
#  stat_poly_eq(formula=y~x,data=subset(df_scatter_monthly,advanced!="Advanced Economies"),aes(x=total_cpi_change,y=policy_rate_pp_change,color=NA,label=paste(..eq.label..,"p-value:",..p.value..,sep="~~~")),color="black",parse=TRUE) +
#  labs(y="Percentage point change in policy rate",x="Percentage point change in 12-month inflation",colour="Classification",caption="Source: Bank for International Settlements; CEIC\nNote: Line-of-best-fit and regression equation are estimated for non-LA EMEs and Latin America alone.") +
#  theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))


# This makes Figure 13
fig13 <- ggplot(df_scatter_monthly %>% filter(advanced!="Advanced Economies"&country!="Turkey"),aes(x=core_cpi_change,y=policy_rate_pp_change,colour=advanced)) +
  stat_smooth(method="lm",inherit.aes=FALSE,aes(x=core_cpi_change,y=policy_rate_pp_change)) +
  geom_point(size=3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept= 0) +
  geom_label_repel(aes(label = country),size=9) +
  stat_poly_eq(formula=y~x,aes(x=core_cpi_change,y=policy_rate_pp_change,color=NA,label=paste(..eq.label..,"p-value:",round(..p.value..,2),sep="~~~")),size=9,color="black",parse=TRUE) +
  labs(y="Percentage point change in policy rate (June 2022 - January 2021)",x="Percentage point change in 12-month core inflation (June 2022 - June 2021)",colour="Classification",caption="Source: Bank for International Settlements; CEIC") +
  theme(text = element_text(size=20),axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),axis.title.y=element_text(size=15),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

ggsave(fig13,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/fig13_12m_change_CORE_inflation_and_pr.png"),
       width=15,height=8,units="in")

#ggplot(df_scatter_monthly,aes(x=core_cpi_change,y=policy_rate_pp_change,colour=advanced)) +
#  stat_smooth(method="lm",data=subset(df_scatter_monthly,advanced!="Advanced Economies"),inherit.aes=FALSE,aes(x=core_cpi_change,y=policy_rate_pp_change)) +
#  geom_point(size=3) +
#  geom_hline(yintercept = 0) +
#  geom_vline(xintercept= 0) +
#  geom_label_repel(aes(label = country)) +
#  stat_poly_eq(formula=y~x,data=subset(df_scatter_monthly,advanced!="Advanced Economies"),aes(x=core_cpi_change,y=policy_rate_pp_change,color=NA,label=paste(..eq.label..,"p-value:",..p.value..,sep="~~~")),color="black",parse=TRUE) +
#  labs(y="Percentage point change in policy rate",x="Percentage point change in 12-month core inflation",colour="Classification",caption="Source: Bank for International Settlements; CEIC\nNote: Line-of-best-fit and regression equation are estimated for non-LA EMEs and Latin America alone.") +
#  theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))



# inflation forecasts central banks
cb_forecast_data = read_excel(paste0(data_folder,"Raw/lac_cb_inflation_forecasts.xlsx")) %>%
  rename(erroramt=`Month source`) %>%
  mutate(erroramt=as.numeric(erroramt))
cb_forecast_data = reshape2::melt(cb_forecast_data,id.vars=c("country","erroramt","type"),measure.vars=c("2018","2019","2020","2021","2022")) %>%
  mutate(variable=as.numeric(as.character(variable)))

# This makes Figure 10
ggplot(cb_forecast_data %>% filter(type%in%c("total","target")),aes(x=variable,y=value,colour=type)) +
  geom_rect(mapping=aes(xmin=2021.25,xmax=2022,ymin=-Inf,ymax=Inf),alpha=.1,fill="lightgrey",colour="lightgrey") +
  geom_line(size=2) +
  geom_ribbon(aes(ymin=value-erroramt,ymax=value+erroramt),linetype=2,alpha=0,size=2) +
  facet_wrap(~country) +
  scale_x_continuous(minor_breaks = c(2018:2022)) +
  scale_colour_manual(breaks=c("target","total"),labels=c("Target inflation","Actual/forecast inflation"),values=c("blue","red")) +
  labs(x="Year",y="Annual headline inflation (%)",caption="Sources: CEIC; Federal Reserve; Central Bank of Brazil; Central Bank of Chile; Central Bank of Colombia; Bank of Mexico;\n Central Reserve Bank of Peru\nNote: Inflation for 2022 is forecast by national central banks as of Dec 2021 - Feb 2022.") +
  theme(text = element_text(size=30),axis.text.x = element_text(size=25,angle=45,hjust=1),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
        legend.title=element_blank(),strip.text.x=element_text(size=20))


# weo scatterplots
df_scatter_monthly = panel_df_monthly %>%
  filter(date>="2019-12-01") %>%
  group_by(country) %>%
  summarise(change_rate_oct_2020_present=rate[date=="2022-04-01"]-rate[date=="2020-10-01"],
            weo_change_inflation=weo_change_inflation[1],
            advanced=advanced[1]) %>%
  mutate(advanced=ifelse(advanced!="Non-LA EMEs","Advanced Economies",advanced),
         advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced)) %>%
  ungroup()

df_scatter_monthly$advanced = factor(df_scatter_monthly$advanced,levels=c("Non-LA EMEs","Advanced Economies","Latin America"))

# This makes Figure 14
ggplot(df_scatter_monthly %>% filter(advanced!="Advanced Economies"),aes(x=weo_change_inflation,y=change_rate_oct_2020_present,colour=advanced)) +
  stat_smooth(method="lm",inherit.aes=FALSE,aes(x=weo_change_inflation,y=change_rate_oct_2020_present)) +
  geom_point(size=3) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept= 0) +
  geom_label_repel(aes(label = country),size=9) +
  stat_poly_eq(formula=y~x,aes(x=weo_change_inflation,y=change_rate_oct_2020_present,color=NA,label=paste(..eq.label..,"p-value:",round(..p.value..,2),sep="~~~")),size=9,color="black",parse=TRUE) +
  labs(y="Percentage point change in policy rate",x="Percentage point change in year-ahead inflation forecast",colour="Classification",caption="Sources: International Monetary Fund; Bank for International Settlements") +
  theme(text = element_text(size=30),axis.text.x = element_text(size=25),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

#ggplot(df_scatter_monthly,aes(x=weo_change_inflation,y=change_rate_oct_2020_present,colour=advanced)) +
#  stat_smooth(method="lm",data=subset(df_scatter_monthly,advanced!="Advanced Economies"),inherit.aes=FALSE,aes(x=weo_change_inflation,y=change_rate_oct_2020_present)) +
#  geom_point(size=3) +
#  geom_hline(yintercept = 0) +
#  geom_vline(xintercept= 0) +
#  geom_label_repel(aes(label = country)) +
#  stat_poly_eq(formula=y~x,data=subset(df_scatter_monthly,advanced!="Advanced Economies"),aes(x=weo_change_inflation,y=change_rate_oct_2020_present,color=NA,label=paste(..eq.label..,"p-value:",..p.value..,sep="~~~")),color="black",parse=TRUE) +
#  labs(y="Percentage point change in policy rate",x="Percentage point change in year-ahead inflation forecast",colour="Classification",caption="Sources: International Monetary Fund; Bank for International Settlements\nNote: Line-of-best-fit and regression equation are estimated for non-LA EMEs and Latin America alone.") +
#  theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))


#  regression in levels, 2007-2019, total inflation
#panel_df = panel_df %>%
#  mutate(country_group=ifelse(country%in%c("South Korea","Czech Republic"),"Non-LA EMEs",country_group),
#         country_group=ifelse(country_group!="Non-LA EMEs"&country_group!="Latin America"&country_group!="Advanced Economy","Non-LA EMEs",country_group))

#model_quarterly_gdp_total_advanced = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
#model_quarterly_gdp_total_eme = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_total_lac = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|0,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0(charts_folder,"Scatterplots/2021-11-08_Paper_Charts/total_inflation_gdp_panel_levels_2007_2019.html"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0("total_inflation_gdp_panel_levels_2007_2019.doc"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))

# appendix table
#model_quarterly_gdp_core_advanced = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Advanced Economy"))
#model_quarterly_gdp_core_eme = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_core_lac = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|country,data = panel_df %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0(charts_folder,"Scatterplots/2021-11-08_Paper_Charts/core_inflation_gdp_panel_levels_2007_2019.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0("core_inflation_gdp_panel_levels_2007_2019.doc"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019, Time and Country FE",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))

# regression in levels, 2007-2021 Q3, total inflation, no pandemic dummy by itself
#model_quarterly_gdp_total_advanced = felm(rate~CPI+gdp_output_gap+lag_rate+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2021-09-01"&country_group=="Advanced Economy"))
#model_quarterly_gdp_total_eme = felm(rate~CPI+gdp_output_gap+lag_rate+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2021-09-01"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_total_lac = felm(rate~CPI+gdp_output_gap+lag_rate+pandemic_total_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2021-09-01"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0(results_folder,"Regression Results/2021-11-12_Taylor_Rule_Panel_2007_2021/total_inflation_gdp_panel_levels_nodummy_2007_2021Q3.html"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2021 Q3 extrapolation, Time and Country FE, Pandemic Interaction Terms without pandemic",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0("total_inflation_gdp_panel_levels_nodummy_2007_2021Q3.doc"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2021Q3 extrapolation, Time and Country FE, Pandemic Interaction Terms without pandemic dummy",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#write.csv(rbind(getfe(model_quarterly_gdp_total_advanced) %>% mutate(model="Advanced Economy"),getfe(model_quarterly_gdp_total_eme) %>% mutate(model="Non-LA EMEs"),getfe(model_quarterly_gdp_total_lac) %>% mutate(model="Latin America")),file=paste0(results_folder,"Regression Results/2021-11-12_Taylor_Rule_Panel_2007_2021/fixed_effects_total_inflation_gdp_panel_levels_nodummy_2007_2021Q3.csv"))

# appendix table
#model_quarterly_gdp_core_advanced = felm(rate~core_inflation+gdp_output_gap+lag_rate+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2021-09-01"&country_group=="Advanced Economy"))
#model_quarterly_gdp_core_eme = felm(rate~core_inflation+gdp_output_gap+lag_rate+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2021-09-01"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_core_lac = felm(rate~core_inflation+gdp_output_gap+lag_rate+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2021-09-01"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0(results_folder,"Regression Results/2021-11-12_Taylor_Rule_Panel_2007_2021/core_inflation_gdp_panel_levels_nodummy_2007_2021Q3.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2021Q3 extrapolation, Time and Country FE, Pandemic Interaction Terms without pandemic dummy",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0("core_inflation_gdp_panel_levels_nodummy_2007_2021Q3.doc"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2021, Time and Country FE, Pandemic Interaction Terms without pandemic dummy",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#write.csv(rbind(getfe(model_quarterly_gdp_core_advanced) %>% mutate(model="Advanced Economy"),getfe(model_quarterly_gdp_core_eme) %>% mutate(model="Non-LA EMEs"),getfe(model_quarterly_gdp_core_lac) %>% mutate(model="Latin America")),file=paste0(results_folder,"Regression Results/2021-11-12_Taylor_Rule_Panel_2007_2021/fixed_effects_core_inflation_gdp_panel_levels_nodummy_2007_2021Q3.csv"))


# regression in levels, 2007-2021 Q3, total inflation, no pandemic interaction terms
#model_quarterly_gdp_total_advanced = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2021-09-01"&country_group=="Advanced Economy"))
#model_quarterly_gdp_total_eme = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2021-09-01"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_total_lac = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2021-09-01"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0(results_folder,"Regression Results/2021-11-15_Taylor_Rule_Panel_2007_2021_NoIntTerms/total_inflation_gdp_panel_levels_NoIntTerms_2007_2021Q3.html"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2021 Q3 extrapolation, Time and Country FE, No Pandemic Interaction Terms",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_total_advanced,model_quarterly_gdp_total_eme,model_quarterly_gdp_total_lac),file=paste0("total_inflation_gdp_panel_levels_NoIntTerms_2007_2021Q3.doc"),caption="Panel Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2021Q3 extrapolation, Time and Country FE, No Pandemic Interaction Terms",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#write.csv(rbind(getfe(model_quarterly_gdp_total_advanced) %>% mutate(model="Advanced Economy"),getfe(model_quarterly_gdp_total_eme) %>% mutate(model="Non-LA EMEs"),getfe(model_quarterly_gdp_total_lac) %>% mutate(model="Latin America")),file=paste0(results_folder,"Regression Results/2021-11-15_Taylor_Rule_Panel_2007_2021_NoIntTerms/fixed_effects_total_inflation_gdp_panel_levels_NoIntTerms_2007_2021Q3.csv"))

# appendix table
#model_quarterly_gdp_core_advanced = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2021-09-01"&country_group=="Advanced Economy"))
#model_quarterly_gdp_core_eme = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2021-09-01"&country_group=="Emerging Market Economy"))
#model_quarterly_gdp_core_lac = felm(rate~core_inflation+gdp_output_gap+lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2021-09-01"&country_group=="Latin America"))
#htmlreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0(results_folder,"Regression Results/2021-11-15_Taylor_Rule_Panel_2007_2021_NoIntTerms/core_inflation_gdp_panel_levels_NoIntTerms_2007_2021Q3.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2021Q3 extrapolation, Time and Country FE,No Pandemic Interaction Terms",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#wordreg(list(model_quarterly_gdp_core_advanced,model_quarterly_gdp_core_eme,model_quarterly_gdp_core_lac),file=paste0("core_inflation_gdp_panel_levels_NoIntTerms_2007_2021Q3.doc"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2021, Time and Country FE, No Pandemic Interaction Terms",caption.above=TRUE,
#        custom.model.names = c("Advanced Economies","Non-LA EMEs","Latin America"),stars=c(.01,.05,.1))
#write.csv(rbind(getfe(model_quarterly_gdp_core_advanced) %>% mutate(model="Advanced Economy"),getfe(model_quarterly_gdp_core_eme) %>% mutate(model="Non-LA EMEs"),getfe(model_quarterly_gdp_core_lac) %>% mutate(model="Latin America")),file=paste0(results_folder,"Regression Results/2021-11-15_Taylor_Rule_Panel_2007_2021_NoIntTerms/fixed_effects_core_inflation_gdp_panel_levels_NoIntTerms_2007_2021Q3.csv"))


## Appendix Table 1.2 and Appendix Table 1.3 ##
col1 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Latin America"))
col2 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate+pandemic_reer_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2022-12-31"&country_group=="Latin America"))
col3 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change|country + date|0|0,data = panel_df_prediction_data %>% filter(date>"1998-09-30"&date<="2019-12-31"&country_group=="Latin America"))
col4 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country_group=="Non-LA EMEs"))
col5 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change+pandemic_core_inf+pandemic_output_gap+pandemic_lag_rate+pandemic_reer_change |country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2022-12-31"&country_group=="Non-LA EMEs"))
col6 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change|country + date|0|0,data = panel_df_prediction_data %>% filter(date>"1998-09-30"&date<="2019-12-31"&country_group=="Non-LA EMEs"))
htmlreg(list(col1,col2,col3,col4,col5,col6),file=paste0(results_folder,"Regression Results/2021-11-22_Taylor_Rule_expanded_LA/LA_and_EME_Table_Appendix_Core.html"),caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019 extrapolation, Time and Country FE, Quarterly Percent Change in REER",caption.above=TRUE,
        custom.model.names = c("LA, 2007-2019","LA, 2007-2022 Q4","LA, 1998 Q4 - 2019 Q4","EME, 2007-2019","EME, 2007-2021 Q3","EME, 1998 Q4 - 2019 Q4"),stars=c(.01,.05,.1))
wordreg(list(col1,col2,col3,col4,col5,col6),file="LA_and_EME_Table_Appendix_Core.doc",caption="Panel Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019 extrapolation, Time and Country FE, Quarterly Percent Change in REER",caption.above=TRUE,
        custom.model.names = c("LA, 2007-2019","LA, 2007-2022 Q4","LA, 1998 Q4 - 2019 Q4","EME, 2007-2019","EME, 2007-2021 Q3","EME, 1998 Q4 - 2019 Q4"),stars=c(.01,.05,.1))


# appendix table 1: individual latin america regressions, 2007 to 2019
## Appendix Table 1.1 ##
col1 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Brazil"))
col2 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Chile"))
col3 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Colombia"))
col4 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Mexico"))
col5 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Peru"))
htmlreg(list(col1,col2,col3,col4,col5),file=paste0(results_folder,"Regression Results/2021-11-22_Taylor_Rule_expanded_LA/LA_individual_countries_20230405.html"),caption="Individual Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019, No Time or Country FE, Quarterly Percent Change in REER",caption.above=TRUE,
        custom.model.names = c("Brazil","Chile","Colombia","Mexico","Peru"),stars=c(.01,.05,.1))
wordreg(list(col1,col2,col3,col4,col5),file="LA_individual_countries.html.doc",caption="Individual Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019 extrapolation, Time and Country FE, Quarterly Percent Change in REER",caption.above=TRUE,
        custom.model.names = c("Brazil","Chile","Colombia","Mexico","Peru"),stars=c(.01,.05,.1))

# total inflation with time trend, 2007 to 2019
# col1 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Brazil"))
# col2 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Chile"))
# col3 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Colombia"))
# col4 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Mexico"))
# col5 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Peru"))
# htmlreg(list(col1,col2,col3,col4,col5),file=paste0(results_folder,"Regression Results/2021-11-22_Taylor_Rule_expanded_LA/LA_individual_countries_time_trend_2007_2019.html"),caption="Individual Regression Results: Total Inflation and GDP Output Gap in Levels, 2007-2019, Time Trend, Quarterly Percent Change in REER",caption.above=TRUE,
#         custom.model.names = c("Brazil","Chile","Colombia","Mexico","Peru"),stars=c(.01,.05,.1))

# total inflation, no time trend, 2011 to 2019
# col1 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Brazil"))
# col2 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Chile"))
# col3 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Colombia"))
# col4 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Mexico"))
# col5 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Peru"))
# htmlreg(list(col1,col2,col3,col4,col5),file=paste0(results_folder,"Regression Results/2021-11-22_Taylor_Rule_expanded_LA/LA_individual_countries_2011_2019.html"),caption="Individual Regression Results: Total Inflation and GDP Output Gap in Levels, 2011-2019, No Time Trend, Quarterly Percent Change in REER",caption.above=TRUE,
#         custom.model.names = c("Brazil","Chile","Colombia","Mexico","Peru"),stars=c(.01,.05,.1))

# total inflation, with time trend, 2011 to 2019
# col1 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Brazil"))
# col2 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Chile"))
# col3 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Colombia"))
# col4 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Mexico"))
# col5 = felm(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Peru"))
# htmlreg(list(col1,col2,col3,col4,col5),file=paste0(results_folder,"Regression Results/2021-11-22_Taylor_Rule_expanded_LA/LA_individual_countries_time_trend_2011_2019.html"),caption="Individual Regression Results: Total Inflation and GDP Output Gap in Levels, 2011-2019, Time Trend, Quarterly Percent Change in REER",caption.above=TRUE,
#         custom.model.names = c("Brazil","Chile","Colombia","Mexico","Peru"),stars=c(.01,.05,.1))

# core inflation, with time trend, 2011 to 2019
# col1 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Brazil"))
# col2 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Chile"))
# col3 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Colombia"))
# col4 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Mexico"))
# col5 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change+time_trend |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Peru"))
# htmlreg(list(col1,col2,col3,col4,col5),file=paste0(results_folder,"Regression Results/2021-11-22_Taylor_Rule_expanded_LA/LA_individual_countries_core_inflation_time_trend_2011_2019.html"),caption="Individual Regression Results: Core Inflation and GDP Output Gap in Levels, 2011-2019, Time Trend, Quarterly Percent Change in REER",caption.above=TRUE,
#         custom.model.names = c("Brazil","Chile","Colombia","Mexico","Peru"),stars=c(.01,.05,.1))
#
# # core inflation, no time trend, 2011 to 2019
# col1 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Brazil"))
# col2 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Chile"))
# col3 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Colombia"))
# col4 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Mexico"))
# col5 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2011-01-01"&date<="2019-12-31"&country=="Peru"))
# htmlreg(list(col1,col2,col3,col4,col5),file=paste0(results_folder,"Regression Results/2021-11-22_Taylor_Rule_expanded_LA/LA_individual_countries_core_inflation_2011_2019.html"),caption="Individual Regression Results: Core Inflation and GDP Output Gap in Levels, 2011-2019, No Time Trend, Quarterly Percent Change in REER",caption.above=TRUE,
#         custom.model.names = c("Brazil","Chile","Colombia","Mexico","Peru"),stars=c(.01,.05,.1))
#
# # core inflation, no time trend, 2007 to 2019
# col1 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Brazil"))
# col2 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Chile"))
# col3 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Colombia"))
# col4 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Mexico"))
# col5 = felm(rate~core_inflation+gdp_output_gap+lag_rate+reer_quarter_perc_change |0|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-31"&country=="Peru"))
# htmlreg(list(col1,col2,col3,col4,col5),file=paste0(results_folder,"Regression Results/2021-11-22_Taylor_Rule_expanded_LA/LA_individual_countries_core_inflation_2007_2019.html"),caption="Individual Regression Results: Core Inflation and GDP Output Gap in Levels, 2007-2019, No Time Trend, Quarterly Percent Change in REER",caption.above=TRUE,
#         custom.model.names = c("Brazil","Chile","Colombia","Mexico","Peru"),stars=c(.01,.05,.1))

# simulation of policy rates (dynamic and static) for LAC

# step 1: regression for LAC, no pandemic dummies, with time and country FEs, 2007-2019
# lac_regression_2007_2019 = felm(rate~CPI+gdp_output_gap+lag_rate|country + date|0|0,data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Latin America"))
#
# # step 2: dynamically and statically simulate path of rates during 2020 and 2021
#
# intercept = .495634
# inflation_coef = as.numeric(tidy(lac_regression_2007_2019)[1,2])
# gdp_coef = as.numeric(tidy(lac_regression_2007_2019)[2,2])
# lag_rate_coef = as.numeric(tidy(lac_regression_2007_2019)[3,2])
#
# # static
# static_lac_sim = panel_df_prediction_data %>%
#   ungroup() %>%
#   filter(country_group=="Latin America"&date<="2021-09-01"&date>="2019-01-01") %>%
#   select(country,date,CPI,gdp_output_gap,lag_rate,rate) %>%
#   arrange(country) %>%
#   mutate(predicted_rate = (inflation_coef*CPI)+(gdp_coef*gdp_output_gap)+(lag_rate_coef*lag_rate),
#          predicted_rate=ifelse(date<"2020-01-01",rate,predicted_rate))
#
# ggplot(static_lac_sim %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup(),aes(x=date)) +
#   geom_line(aes(y=rate,colour="Actual Rate"),size=2) +
#   geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) +
#   geom_line(data=daily_rates %>% filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2021-09-29"),aes(y=rate,colour="Actual Rate"),size=2) +
#   facet_wrap(~country)+
#   scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
#   scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2021-12-31"),
#                               format="%Y-%m-%d"),minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31"),
#                               format="%Y-%m-%d"),labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
#   labs(x="Quarter",y="Policy Rate (%)",caption="Sources: Bank for International Settlements; CEIC") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#
# # dynamic
# dynamic_lac_sim = data.frame()
# for(a in c("Brazil","Chile","Colombia","Peru","Mexico")){
#
#   base_df = panel_df_prediction_data %>%
#     filter(country==a&date>="2020-01-01"&date<="2021-09-30") %>%
#     select(date,country,CPI,gdp_output_gap,lag_rate)
#
#   base_df$lag_rate_sim = ifelse(base_df$date=="2020-03-01",base_df$lag_rate[1],NA)
#   base_df$predicted_rate = NA
#   for(i in 1:nrow(base_df)){
#     base_df$predicted_rate[i] = (base_df$CPI[i]*inflation_coef)+(base_df$gdp_output_gap[i]*gdp_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)
#     if(i < nrow(base_df)){
#       base_df$lag_rate_sim[i+1]= base_df$predicted_rate[i]
#     }
#   }
#
#   dynamic_lac_sim = bind_rows(dynamic_lac_sim,panel_df_prediction_data %>%
#                               filter(country==a&date>="2019-01-01"&date<"2020-01-01") %>%
#                               select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>%
#                                 rename(predicted_rate=rate),
#                             base_df)
# }
#
# ggplot(dynamic_lac_sim %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup(),aes(x=date)) +
#   geom_line(data=panel_df_prediction_data %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(date<="2021-09-30"&date>="2019-01-01"&country%in%c("Brazil","Chile","Colombia","Peru","Mexico")),
#             aes(y=rate,colour="Actual Rate"),size=2) +
#   geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) +
#   geom_line(data=daily_rates %>% filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2021-09-29"),aes(y=rate,colour="Actual Rate"),size=2) +
#   facet_wrap(~country)+
#   scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
#   scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2021-12-31"),
#                               format="%Y-%m-%d"),minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31"),
#                                                                       format="%Y-%m-%d"),labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
#   labs(x="Quarter",y="Policy Rate (%)",caption="Sources: Bank for International Settlements; CEIC") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#
# # make excel chart
# sim_full_table = left_join(static_lac_sim,
#                            dynamic_lac_sim %>% select(date,country,predicted_rate) %>% rename(predicted_rate_dyn=predicted_rate),
#                            by=c("country","date")) %>%
#   mutate(error_static = predicted_rate-rate,
#          error_dynamic=predicted_rate_dyn-rate)
# write.csv(sim_full_table,file=paste0(charts_folder,"Scatterplots/2021-11-08_Paper_Charts/sim_full_table_errors_2020_2021.csv"))



# also simulate for 2008 Q1 to 2009 Q3
# static
# static_lac_sim = panel_df_prediction_data %>%
#   ungroup() %>%
#   filter(country_group=="Latin America"&date<="2009-09-01"&date>="2007-01-01") %>%
#   select(country,date,CPI,gdp_output_gap,lag_rate,rate) %>%
#   arrange(country) %>%
#   mutate(predicted_rate = (inflation_coef*CPI)+(gdp_coef*gdp_output_gap)+(lag_rate_coef*lag_rate),
#          predicted_rate=ifelse(date<"2008-01-01",rate,predicted_rate))
#
# ggplot(static_lac_sim %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup(),aes(x=date)) +
#   geom_line(aes(y=rate,colour="Actual Rate"),size=2) +
#   geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) +
#   facet_wrap(~country)+
#   scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
#   scale_x_date(breaks=as.Date(c("2007-03-31","2007-09-30","2008-03-31","2008-09-30","2009-03-31","2009-09-30"),
#                               format="%Y-%m-%d"),minor_breaks=as.Date(c("2007-03-31","2007-06-30","2007-09-30","2007-12-31","2008-03-31","2008-06-30","2008-09-30","2008-12-31","2009-03-31","2009-06-30","2009-09-30","2009-12-31"),
#                                                                       format="%Y-%m-%d"),labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
#   labs(x="Quarter",y="Policy Rate (%)",caption="Sources: Bank for International Settlements; CEIC") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#
# # dynamic
# dynamic_lac_sim = data.frame()
# for(a in c("Brazil","Chile","Colombia","Peru","Mexico")){
#
#   base_df = panel_df_prediction_data %>%
#     filter(country==a&date>="2008-01-01"&date<="2009-09-30") %>%
#     select(date,country,CPI,gdp_output_gap,lag_rate)
#
#   base_df$lag_rate_sim = ifelse(base_df$date=="2008-03-01",base_df$lag_rate[1],NA)
#   base_df$predicted_rate = NA
#   for(i in 1:nrow(base_df)){
#     base_df$predicted_rate[i] = (base_df$CPI[i]*inflation_coef)+(base_df$gdp_output_gap[i]*gdp_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)
#     if(i < nrow(base_df)){
#       base_df$lag_rate_sim[i+1]= base_df$predicted_rate[i]
#     }
#   }
#
#   dynamic_lac_sim = bind_rows(dynamic_lac_sim,panel_df_prediction_data %>%
#                                 filter(country==a&date>="2007-01-01"&date<"2008-01-01") %>%
#                                 select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>%
#                                 rename(predicted_rate=rate),
#                               base_df)
# }
#
# ggplot(dynamic_lac_sim %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup(),aes(x=date)) +
#   geom_line(data=panel_df_prediction_data %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(date<="2009-09-30"&date>="2007-01-01"&country%in%c("Brazil","Chile","Colombia","Peru","Mexico")),
#             aes(y=rate,colour="Actual Rate"),size=2) +
#   geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) +
#   facet_wrap(~country)+
#   scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
#   scale_x_date(breaks=as.Date(c("2007-03-31","2007-09-30","2008-03-31","2008-09-30","2009-03-31","2009-09-30"),
#                               format="%Y-%m-%d"),minor_breaks=as.Date(c("2007-03-31","2007-06-30","2007-09-30","2007-12-31","2008-03-31","2008-06-30","2008-09-30","2008-12-31","2009-03-31","2009-06-30","2009-09-30"),
#                                                                       format="%Y-%m-%d"),labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
#   labs(x="Quarter",y="Policy Rate (%)",caption="Sources: Bank for International Settlements; CEIC") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#
# # make excel chart
# sim_full_table = left_join(static_lac_sim,
#                            dynamic_lac_sim %>% select(date,country,predicted_rate) %>% rename(predicted_rate_dyn=predicted_rate),
#                            by=c("country","date")) %>%
#   mutate(error_static = predicted_rate-rate,
#          error_dynamic=predicted_rate_dyn-rate)
# write.csv(sim_full_table,file=paste0(charts_folder,"Scatterplots/2021-11-08_Paper_Charts/sim_full_table_errors_2008_2009.csv"))



# redo sim with fixed effects, intercept, and netting out
# step 1: regression for LAC, no pandemic dummies, with time and country FEs, 2007-2019
# lac_regression_2007_2019 = lm_robust(rate~CPI+gdp_output_gap+lag_rate+factor(country)+factor(date),data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# summary(lac_regression_2007_2019)
#
# # step 2: dynamically and statically simulate path of rates during 2020 and 2021
#
# intercept = .495634
# inflation_coef = as.numeric(tidy(lac_regression_2007_2019)[2,2])
# gdp_coef = as.numeric(tidy(lac_regression_2007_2019)[3,2])
# lag_rate_coef = as.numeric(tidy(lac_regression_2007_2019)[4,2])
#
# # dynamic
# dynamic_lac_sim = data.frame()
# for(a in c("Brazil","Chile","Colombia","Peru","Mexico")){
#
#   base_df = panel_df_prediction_data %>%
#     filter(country==a&date>="2019-09-30"&date<="2021-09-30") %>%
#     select(date,country,CPI,gdp_output_gap,lag_rate) %>%
#     mutate(country_factor=case_when(
#       country=="Brazil"~0,
#       country=="Chile"~-.279614,
#       country=="Colombia"~-.230887,
#       country=="Mexico"~-.161228,
#       country=="Peru"~-.245274
#     ),
#     country_factor=as.numeric(country_factor))
#
#   net_factor = base_df$lag_rate[2]-((base_df$CPI[1]*inflation_coef)+(base_df$gdp_output_gap[1]*gdp_coef)+(base_df$lag_rate[1]*lag_rate_coef)+intercept+base_df$country_factor[1])
#
#   base_df$net_factor = net_factor
#   base_df$lag_rate_sim = ifelse(base_df$date=="2019-12-01",base_df$lag_rate[1],NA)
#   base_df$predicted_rate = NA
#   for(i in 1:nrow(base_df)){
#     base_df$predicted_rate[i] = (base_df$CPI[i]*inflation_coef)+(base_df$gdp_output_gap[i]*gdp_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)+intercept+base_df$country_factor[i]+base_df$net_factor[i]
#     if(i < nrow(base_df)){
#       base_df$lag_rate_sim[i+1]= base_df$predicted_rate[i]
#     }
#   }
#
#   dynamic_lac_sim = bind_rows(dynamic_lac_sim,panel_df_prediction_data %>%
#                                 filter(country==a&date>="2019-01-01"&date<"2019-10-01") %>%
#                                 select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>%
#                                 rename(predicted_rate=rate),
#                               base_df)
# }
# panel_df_prediction_data = panel_df_prediction_data %>% filter(!is.na(country))
#
# ggplot(dynamic_lac_sim %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup(),aes(x=date)) +
#   geom_line(data=panel_df_prediction_data %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(date<="2021-09-30"&date>="2019-01-01"&country%in%c("Brazil","Chile","Colombia","Peru","Mexico")),
#             aes(y=rate,colour="Actual Rate"),size=2) +
#   geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) +
#   geom_line(data=daily_rates %>% filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2021-09-29"),aes(y=rate,colour="Actual Rate"),size=2) +
#   facet_wrap(~country)+
#   scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
#   scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2021-12-31"),
#                               format="%Y-%m-%d"),minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31"),
#                                                                       format="%Y-%m-%d"),labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
#   labs(x="Quarter",y="Policy Rate (%)",caption="Sources: Bank for International Settlements; CEIC") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))


# make excel chart
# sim_full_table = left_join(static_lac_sim,
#                            dynamic_lac_sim %>% select(date,country,predicted_rate) %>% rename(predicted_rate_dyn=predicted_rate),
#                            by=c("country","date")) %>%
#   mutate(error_static = predicted_rate-rate,
#          error_dynamic=predicted_rate_dyn-rate)
# write.csv(sim_full_table,file=paste0(charts_folder,"Scatterplots/2021-11-08_Paper_Charts/sim_full_table_errors_2020_2021.csv"))



# redo sim with fixed effects, intercept, but no netting out
# step 1: regression for LAC, no pandemic dummies, with time and country FEs, 2007-2019
# lac_regression_2007_2019 = lm_robust(rate~CPI+gdp_output_gap+lag_rate+factor(country)+factor(date),data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Latin America"))
# summary(lac_regression_2007_2019)
#
# # step 2: dynamically and statically simulate path of rates during 2020 and 2021
#
# intercept = .495634
# inflation_coef = as.numeric(tidy(lac_regression_2007_2019)[2,2])
# gdp_coef = as.numeric(tidy(lac_regression_2007_2019)[3,2])
# lag_rate_coef = as.numeric(tidy(lac_regression_2007_2019)[4,2])
#
# # dynamic
# dynamic_lac_sim = data.frame()
# for(a in c("Brazil","Chile","Colombia","Peru","Mexico")){
#
#   base_df = panel_df_prediction_data %>%
#     filter(country==a&date>="2020-01-01"&date<="2021-09-30") %>%
#     select(date,country,CPI,gdp_output_gap,lag_rate) %>%
#     mutate(country_factor=case_when(
#       country=="Brazil"~0,
#       country=="Chile"~-.279614,
#       country=="Colombia"~-.230887,
#       country=="Mexico"~-.161228,
#       country=="Peru"~-.245274
#     ),
#     country_factor=as.numeric(country_factor))
#
#   base_df$lag_rate_sim = ifelse(base_df$date=="2020-03-01",base_df$lag_rate[1],NA)
#   base_df$predicted_rate = NA
#   for(i in 1:nrow(base_df)){
#     base_df$predicted_rate[i] = (base_df$CPI[i]*inflation_coef)+(base_df$gdp_output_gap[i]*gdp_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)+intercept+base_df$country_factor[i]
#     if(i < nrow(base_df)){
#       base_df$lag_rate_sim[i+1]= base_df$predicted_rate[i]
#     }
#   }
#
#   dynamic_lac_sim = bind_rows(dynamic_lac_sim,panel_df_prediction_data %>%
#                                 filter(country==a&date>="2019-01-01"&date<="2019-12-31") %>%
#                                 select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>%
#                                 rename(predicted_rate=rate),
#                               base_df)
# }
# panel_df_prediction_data = panel_df_prediction_data %>% filter(!is.na(country))
#
# ggplot(dynamic_lac_sim %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup(),aes(x=date)) +
#   geom_line(data=panel_df_prediction_data %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(date<="2021-09-30"&date>="2019-01-01"&country%in%c("Brazil","Chile","Colombia","Peru","Mexico")),
#             aes(y=rate,colour="Actual Rate"),size=2) +
#   geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) +
#   geom_line(data=daily_rates %>% filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2021-09-29"),aes(y=rate,colour="Actual Rate"),size=2) +
#   facet_wrap(~country)+
#   scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
#   scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2021-12-31"),
#                               format="%Y-%m-%d"),minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31"),
#                                                                       format="%Y-%m-%d"),labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
#   labs(x="Quarter",y="Policy Rate (%)",caption="Sources: Bank for International Settlements; CEIC") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))


# make excel chart (skip down to line 1331 to gen dynamic_lac_sim before you run this)
sim_full_table = left_join(panel_df_prediction_data %>%
                             filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2019-01-01"&date<="2022-12-31") %>%
                             select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>% arrange(country),
                           dynamic_lac_sim %>% select(date,country,predicted_rate) %>% rename(predicted_rate_dyn=predicted_rate),
                           by=c("country","date")) %>%
  mutate(error_dynamic=predicted_rate_dyn-rate)
write.csv(sim_full_table,file=paste0(charts_folder,"Scatterplots/2022-07-22_NewSims_NoAddFactor/sim_full_table_errors_noaddfactor_2020_2022.csv"))



# redo sim with fixed effects, intercept, but no netting out, and no intercept for FE
# step 1: regression for LAC, no pandemic dummies, with time and country FEs, 2007-2019
lac_regression_2007_2019 = lm_robust(rate~CPI+gdp_output_gap+lag_rate+factor(country)+factor(date),data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Latin America"))
summary(lac_regression_2007_2019)

# step 2: dynamically and statically simulate path of rates during 2020 and 2021
# these numbers are pulled from the above regression

intercept = 0.376955 #updated 4/11/2023
#intercept = 0.371694 #updated on 7/25/22
inflation_coef = as.numeric(tidy(lac_regression_2007_2019)[2,2])
gdp_coef = as.numeric(tidy(lac_regression_2007_2019)[3,2])
lag_rate_coef = as.numeric(tidy(lac_regression_2007_2019)[4,2])

# dynamic
dynamic_lac_sim = data.frame()
for(a in c("Brazil","Chile","Colombia","Peru","Mexico")){ #for each one of these countries

  base_df = panel_df_prediction_data %>% #create a base df for that country
    filter(country==a&date>="2020-01-01"&date<="2023-03-31") %>% #and filter that dataset for the "pandemic period"
    select(date,country,CPI,gdp_output_gap,lag_rate) %>%
    mutate(country_factor=case_when(
      country=="Brazil"~0, #"country effect" (coef for a country). brazil is reference, so it's coef=0
      
      country=="Chile"~-0.271760, #all these were updated on 4/11/23 (no sig changes from prev version)
      country=="Colombia"~ -0.208003,
      country=="Mexico"~-0.136095,
      country=="Peru"~-0.220825 

      # country=="Chile"~--0.273871, #all these were updated on 7/25/22 (no sig changes from prev version)
      # country=="Colombia"~-0.207733,
      # country=="Mexico"~-0.139415,
      # country=="Peru"~-0.223970 
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
}
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
              filter(date<="2022-12-31"&date>="2019-01-01"&country%in%c("Brazil","Chile","Colombia","Peru","Mexico")),
            aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(data=panel_df_prediction_data %>% filter(!is.na(country)) %>%  rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(date<="2022-12-31"&date>="2019-01-01"&country%in%c("Brazil","Chile","Colombia","Peru","Mexico")) %>% group_by(date) %>% summarise(rate=mean(rate,na.rm=TRUE),country="Latin America"),
            aes(y=rate,colour="Actual Rate"),size=2) +
  # geom_line(data=daily_rates %>% filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2022-12-30"&date<="2023-12-30"),aes(y=rate,colour="Actual Rate"),size=2) +
  # geom_line(data=daily_rates %>% filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2022-12-30"&date<="2023-12-30") %>% group_by(date) %>% summarise(rate=mean(rate,na.rm=TRUE)) %>% ungroup() %>% mutate(country="Latin America"),aes(y=rate,colour="Actual Rate"),size=2) +
  facet_wrap(~~fct_relevel(country,"Brazil","Chile","Colombia","Mexico","Peru","Latin America"))+
  scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("Actual Rate"="red","Predicted Rate"="blue")) +
  scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2022-03-31","2022-09-30","2023-03-31"),
                              format="%Y-%m-%d"),minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31","2022-03-31","2022-06-31","2022-09-30","2022-12-31","2023-03-31"),
                                                                      format="%Y-%m-%d"),labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
  labs(x="Quarter",y="Policy Rate (%)",caption="Sources: Bank for International Settlements; CEIC\nNote: Predicted rates based on dynamic simulation of equation shown in Table 1, column 1, with intercept and country fixed effects set to zero.  \nIn Latin America panel, actual and predicted rates are the mean average of the country-specific rates shown in the other five panels.") +
  theme(text = element_text(size=30),axis.text.x = element_text(size=25,angle=45,hjust=1),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
        legend.position=c(.5,.9),legend.background=element_blank(),legend.title=element_blank())
# ggsave(plot=fig8,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/fig8_LA_sims_20220801.png"),
#        width=17,height=15,units="in")
ggsave(plot=fig8,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/fig8_LA_sims_20230425.png"),
       width=20,height=15,units="in",dpi=72)


# make excel chart
sim_full_table = left_join(panel_df_prediction_data %>%
                             filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2019-01-01"&date<="2022-03-30") %>%
                             select(date,country,CPI,gdp_output_gap,lag_rate,rate) %>% arrange(country),
                           dynamic_lac_sim %>% select(date,country,predicted_rate) %>% rename(predicted_rate_dyn=predicted_rate),
                           by=c("country","date")) %>%
  mutate(error_dynamic=predicted_rate_dyn-rate)
write.csv(sim_full_table,file=paste0(charts_folder,"Scatterplots/2022-07-25_NewSims_NoInt_NoFE/sim_full_table_errors_noaddfactor_2020_2022.csv"))


# do for non-LA EMEs
# step 1: regression, no pandemic dummies, with time and country FEs, 2007-2019
eme_regression_2007_2019 = lm_robust(rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+factor(country)+factor(date),data = panel_df_prediction_data %>% filter(date>="2007-01-01"&date<="2019-12-01"&country_group=="Non-LA EMEs"))
summary(eme_regression_2007_2019)

# step 2: dynamically and statically simulate path of rates during 2020 and 2021

intercept = 0.564253 #updated 8/3/22
inflation_coef = as.numeric(tidy(eme_regression_2007_2019)[2,2])
gdp_coef = as.numeric(tidy(eme_regression_2007_2019)[3,2])
lag_rate_coef = as.numeric(tidy(eme_regression_2007_2019)[4,2])
reer_coef = as.numeric(tidy(eme_regression_2007_2019)[5,2])

# dynamic
dynamic_eme_sim = data.frame()
for(a in unique(panel_df_prediction_data$country[panel_df_prediction_data$country_group=="Non-LA EMEs"])){

  base_df = panel_df_prediction_data %>%
    filter(country==a&date>="2020-01-01"&date<="2022-12-01") %>%
    select(date,country,CPI,gdp_output_gap,lag_rate,reer_quarter_perc_change)

  base_df$lag_rate_sim = ifelse(base_df$date=="2020-03-01",base_df$lag_rate[1],NA)
  base_df$predicted_rate = NA
  for(i in 1:nrow(base_df)){
    base_df$predicted_rate[i] = (base_df$CPI[i]*inflation_coef)+(base_df$gdp_output_gap[i]*gdp_coef)+(base_df$lag_rate_sim[i]*lag_rate_coef)+(base_df$reer_quarter_perc_change[i]*reer_coef)
    if(i < nrow(base_df)){
      base_df$lag_rate_sim[i+1]= base_df$predicted_rate[i]
    }
  }

  dynamic_eme_sim = bind_rows(dynamic_eme_sim,panel_df_prediction_data %>%
                                filter(country==a&date>="2019-01-01"&date<="2019-12-31") %>%
                                select(date,country,CPI,gdp_output_gap,lag_rate,reer_quarter_perc_change,rate) %>%
                                rename(predicted_rate=rate),
                              base_df)
}
dynamic_eme_sim_full = bind_rows(dynamic_eme_sim,
                                 dynamic_eme_sim %>%
                                   group_by(date) %>%
                                   summarise(CPI=mean(CPI,na.rm=TRUE),
                                             gdp_output_gap = mean(gdp_output_gap,na.rm=TRUE),
                                             lag_rate=mean(lag_rate,na.rm=TRUE),
                                             reer_quarter_perc_change=mean(reer_quarter_perc_change,na.rm=TRUE),
                                             predicted_rate=mean(predicted_rate,na.rm=TRUE),
                                             country="Non-LA EMEs"))

## This makes figure 9 but now its fig 11 ##
fig11 = ggplot(dynamic_eme_sim_full %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(country=="Non-LA EMEs"),aes(x=date)) +
  geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) + # this is the Non-LA EME simulated rate
  geom_line(data=dynamic_lac_sim_full %>%  #this is the LA simulated rate
              rowwise() %>% 
              mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% 
              ungroup() %>% 
              filter(country=="Latin America"),
              aes(y=predicted_rate,colour="Predicted Rate"), 
              size=2) +
  geom_line(data=panel_df_prediction_data %>%  #this is the actual rate for the Non-LA EMES
              filter(!is.na(country)) %>% 
              rowwise() %>% 
              mutate(date=seq(as.Date(date,format="%Y-%m-%d"),length=2,by="1 month")[2]-1) %>% 
              ungroup() %>% 
              filter(date<="2022-12-31"&date>="2019-01-01"&country%in%unique(panel_df_prediction_data$country[panel_df_prediction_data$country_group=="Non-LA EMEs"])) %>% 
              group_by(date) %>% 
              summarise(rate=mean(rate,na.rm=TRUE),country="Non-LA EMEs"),
              aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(data=panel_df_prediction_data %>% filter(!is.na(country)) %>% #this is the actual rate for LACs
              rowwise() %>% 
              mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% 
              ungroup() %>% 
              filter(date<="2022-12-31"&date>="2019-01-01"&country%in%c("Brazil","Chile","Colombia","Peru","Mexico")) %>% 
              group_by(date) %>% 
              summarise(rate=mean(rate,na.rm=TRUE),country="Latin America"),
              aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(data=daily_rates %>%
              filter(country%in%c("Brazil","Chile","Colombia","Peru","Mexico")&date>="2022-12-31") %>%
              group_by(date) %>%
              summarise(rate=mean(rate,na.rm=TRUE)) %>%
              ungroup() %>%
              mutate(country="Latin America"),aes(y=rate,colour="Actual Rate"),size=2) +
  geom_line(data=daily_rates %>% 
              filter(country%in%unique(panel_df_prediction_data$country[panel_df_prediction_data$country_group=="Non-LA EMEs"])&date>="2022-12-31") %>% 
              group_by(date) %>% 
              summarise(rate=mean(rate,na.rm=TRUE)) %>% 
              ungroup() %>% 
              mutate(country="Non-LA EMEs"),aes(y=rate,colour="Actual Rate"),size=2) +
  facet_wrap(~country)+
  scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
  scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2022-03-31","2022-09-30","2023-03-31"),
                              format="%Y-%m-%d"),minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31", "2022-03-31", "2022-06-30","2022-09-30","2022-12-31","2023-03-31"),
                                                                      format="%Y-%m-%d"),labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
  labs(x="Quarter",y="Policy Rate (%)",caption="Sources: Bank for International Settlements; CEIC\nNote: Predicted rates based on dynamic simulation of equation shown in Table 1, column 1 and Table 2, column 1, with intercept and\ncountry fixed effects set to zero.  Actual and predicted rates are the mean average of the country-specific rates calculated for each region.") +
  theme(text = element_text(size=30),axis.text.x = element_text(size=25,angle=45,hjust=1),axis.text.y = element_text(size=25),axis.title.y=element_text(size=25),plot.caption = element_text(face="italic",hjust = 0,size=20),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
        legend.position=c(.3,.9),legend.background=element_blank(),legend.title=element_blank())


ggsave(plot=fig11,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/figure11_la_and_non_la_eme_predicted_v_actual_rates_20230417.png"),
       width=20,height=12,units="in")


# mean EME sim for countries that hit the ZLB
# dynamic_eme_sim_full = bind_rows(dynamic_eme_sim %>% filter(!(country%in%c("Czech Republic","Hungary","Israel","Poland","South Korea","Thailand"))),
#                                  dynamic_eme_sim %>%
#                                    filter(!(country%in%c("Czech Republic","Hungary","Israel","Poland","South Korea","Thailand"))) %>%
#                                    group_by(date) %>%
#                                    summarise(CPI=mean(CPI,na.rm=TRUE),
#                                              gdp_output_gap = mean(gdp_output_gap,na.rm=TRUE),
#                                              lag_rate=mean(lag_rate,na.rm=TRUE),
#                                              reer_quarter_perc_change=mean(reer_quarter_perc_change,na.rm=TRUE),
#                                              predicted_rate=mean(predicted_rate,na.rm=TRUE),
#                                              country="Non-LA EMEs"))
#
# ggplot(dynamic_eme_sim_full %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(country=="Non-LA EMEs"),aes(x=date)) +
#   geom_line(data=panel_df_prediction_data %>% filter(!is.na(country)&!(country%in%c("Czech Republic","Hungary","Israel","Poland","South Korea","Thailand"))) %>% rowwise() %>% mutate(date=seq(as.Date(date,format="%Y-%m-%d"),length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(date<="2021-09-30"&date>="2019-01-01"&country%in%unique(panel_df_prediction_data$country[panel_df_prediction_data$country_group=="Non-LA EMEs"])) %>% group_by(date) %>% summarise(rate=mean(rate,na.rm=TRUE),country="Non-LA EMEs"),
#             aes(y=rate,colour="Actual Rate"),size=2) +
#   geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) +
#   geom_line(data=daily_rates %>% filter(country%in%unique(panel_df_prediction_data$country[panel_df_prediction_data$country_group=="Non-LA EMEs"])&!(country%in%c("Czech Republic","Hungary","Israel","Poland","South Korea","Thailand"))&date>="2021-09-29") %>% group_by(date) %>% summarise(rate=mean(rate,na.rm=TRUE)) %>% ungroup(),aes(y=rate,colour="Actual Rate"),size=2) +
#   facet_wrap(~country)+
#   scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
#   scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2021-12-31"),
#                               format="%Y-%m-%d"),minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31"),
#                                                                       format="%Y-%m-%d"),labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
#   labs(x="Quarter",y="Policy Rate (%)",caption="Sources: Bank for International Settlements; CEIC") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#
# dynamic_eme_sim_full = bind_rows(dynamic_eme_sim %>% filter((country%in%c("Czech Republic","Hungary","Israel","Poland","South Korea","Thailand"))),
#                                  dynamic_eme_sim %>%
#                                    filter((country%in%c("Czech Republic","Hungary","Israel","Poland","South Korea","Thailand"))) %>%
#                                    group_by(date) %>%
#                                    summarise(CPI=mean(CPI,na.rm=TRUE),
#                                              gdp_output_gap = mean(gdp_output_gap,na.rm=TRUE),
#                                              lag_rate=mean(lag_rate,na.rm=TRUE),
#                                              reer_quarter_perc_change=mean(reer_quarter_perc_change,na.rm=TRUE),
#                                              predicted_rate=mean(predicted_rate,na.rm=TRUE),
#                                              country="Non-LA EMEs"))
#
# ggplot(dynamic_eme_sim_full %>% rowwise() %>% mutate(date=seq(date,length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(country=="Non-LA EMEs"),aes(x=date)) +
#   geom_line(data=panel_df_prediction_data %>% filter(!is.na(country)&(country%in%c("Czech Republic","Hungary","Israel","Poland","South Korea","Thailand"))) %>% rowwise() %>% mutate(date=seq(as.Date(date,format="%Y-%m-%d"),length=2,by="1 month")[2]-1) %>% ungroup() %>% filter(date<="2021-09-30"&date>="2019-01-01"&country%in%unique(panel_df_prediction_data$country[panel_df_prediction_data$country_group=="Non-LA EMEs"])) %>% group_by(date) %>% summarise(rate=mean(rate,na.rm=TRUE),country="Non-LA EMEs"),
#             aes(y=rate,colour="Actual Rate"),size=2) +
#   geom_line(aes(y=predicted_rate,colour="Predicted Rate"),size=2) +
#   geom_line(data=daily_rates %>% filter(country%in%unique(panel_df_prediction_data$country[panel_df_prediction_data$country_group=="Non-LA EMEs"])&(country%in%c("Czech Republic","Hungary","Israel","Poland","South Korea","Thailand"))&date>="2021-09-29") %>% group_by(date) %>% summarise(rate=mean(rate,na.rm=TRUE)) %>% ungroup(),aes(y=rate,colour="Actual Rate"),size=2) +
#   facet_wrap(~country)+
#   scale_color_manual(breaks=c("Actual Rate","Predicted Rate"),values=c("red","blue")) +
#   scale_x_date(breaks=as.Date(c("2019-03-31","2019-09-30","2020-03-31","2020-09-30","2021-03-31","2021-09-30","2021-12-31"),
#                               format="%Y-%m-%d"),minor_breaks=as.Date(c("2019-03-31","2019-06-30","2019-09-30","2019-12-31","2020-03-31","2020-06-30","2020-09-30","2020-12-31","2021-03-31","2021-06-30","2021-09-30","2021-12-31"),
#                                                                       format="%Y-%m-%d"),labels=function(x) zoo::format.yearqtr(x,"Q%q '%y")) +
#   labs(x="Quarter",y="Policy Rate (%)",caption="Sources: Bank for International Settlements; CEIC") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))


# recursive rolling 7 year window for LA and EMEs separately
# This kind of chart would show how much the coefficient changes as the window of time examined changes
# rolling_panel_regression_reer = list()
# for(a in unique(panel_df_prediction_data$country_group)[-1]){
#
#   first_round_taylor_regressions = data.frame()
#
#   form = as.formula("rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+lag_reer_perc_change+factor(country)+factor(date)")
#
#   df_country = panel_df_prediction_data %>%
#     filter(country_group==a)
#
#   for(yr in c(2007,2008,2009,2010,2011,2012,2013)){
#     data_filtered = panel_df_prediction_data %>%
#       filter(country_group==a&year%in%c(yr:(yr+6)))
#
#     reg = lm_robust(form,data=data_filtered)
#
#     b = tidy(reg)
#
#     first_round_taylor_regressions = rbind(first_round_taylor_regressions,c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],b[5,2],b[5,4],b[6,2],b[6,4],a,as.character(lubridate::year(head(data_filtered$date,1))),as.character(lubridate::year(tail(data_filtered$date,1)))))
#
#     colnames(first_round_taylor_regressions) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","reer_coef","reer_tstat","reer_lag_coef","reer_lag_tstat","country","start_date","end_date")
#   }
#
#   rolling_panel_regression_reer[[a]] = first_round_taylor_regressions
#
#   #write.csv(first_round_taylor_regressions,paste0(results_folder,"taylor_results_2011_2019_time_trend.csv"))
# }
#
# pdf(paste0(charts_folder,"Scatterplots/2021-11-19_NewSims_NoInt_NoFE/rolling_7_years_total_inflation.pdf"),width=10,height=10)
# for(i in c(1:length(rolling_panel_regression_reer))){
#   plot_df = reshape2::melt(rolling_panel_regression_reer[[i]] %>% mutate_at(vars(inflation_coef:reer_lag_tstat),as.numeric) %>% mutate(start_date=as.numeric(start_date)) %>%  select(start_date,inflation_coef,output_gap_coef,lag_rate_coef,reer_coef,reer_lag_coef),id.vars="start_date",measure.vars=c("inflation_coef","output_gap_coef","lag_rate_coef","reer_coef","reer_lag_coef"),variable.name="coef",value.name="value")
#   print(ggplot(plot_df,aes(x=start_date,y=value,colour=coef)) +
#           geom_line(size=2) +
#           facet_wrap(~coef,scales="free") +
#           labs(x="Date Range (start year to start year + 6)",y="Regression coefficient value",caption=names(rolling_panel_regression_reer)[i])
#   )
# }
# dev.off()
#
#
# rolling_panel_regression_reer = list()
# for(a in unique(panel_df_prediction_data$country_group)[-1]){
#
#   first_round_taylor_regressions = data.frame()
#
#   form = as.formula("rate~CPI+gdp_output_gap+lag_rate+neer_12_month_change+factor(country)+factor(date)")
#
#   df_country = panel_df_prediction_data %>%
#     filter(country_group==a)
#
#   for(yr in c(2007,2008,2009,2010,2011,2012,2013)){
#     data_filtered = panel_df_prediction_data %>%
#       filter(country_group==a&year%in%c(yr:(yr+6)))
#
#     reg = lm_robust(form,data=data_filtered)
#
#     b = tidy(reg)
#
#     first_round_taylor_regressions = rbind(first_round_taylor_regressions,c(b[2,2],b[2,4],b[3,2],b[3,4],b[4,2],b[4,4],b[5,2],b[5,4],a,as.character(lubridate::year(head(data_filtered$date,1))),as.character(lubridate::year(tail(data_filtered$date,1)))))
#
#     colnames(first_round_taylor_regressions) = c("inflation_coef","inflation_tstat","output_gap_coef","output_gap_tstat","lag_rate_coef","lag_rate_tstat","neer_12_month_coef","neer_12_month_tstat","country","start_date","end_date")
#   }
#
#   rolling_panel_regression_reer[[a]] = first_round_taylor_regressions
#
#   #write.csv(first_round_taylor_regressions,paste0(results_folder,"taylor_results_2011_2019_time_trend.csv"))
# }
#
# pdf(paste0(charts_folder,"Scatterplots/2021-11-19_NewSims_NoInt_NoFE/rolling_7_years_total_inflation_12monthchange.pdf"),width=10,height=10)
# for(i in c(1:length(rolling_panel_regression_reer))){
#   plot_df = reshape2::melt(rolling_panel_regression_reer[[i]] %>% mutate_at(vars(inflation_coef:neer_12_month_tstat),as.numeric) %>% mutate(start_date=as.numeric(start_date)) %>%  select(start_date,inflation_coef,output_gap_coef,lag_rate_coef,neer_12_month_coef),id.vars="start_date",measure.vars=c("inflation_coef","output_gap_coef","lag_rate_coef","neer_12_month_coef"),variable.name="coef",value.name="value")
#   print(ggplot(plot_df,aes(x=start_date,y=value,colour=coef)) +
#           geom_line(size=2) +
#           facet_wrap(~coef,scales="free") +
#           labs(x="Date Range (start year to start year + 6)",y="Regression coefficient value",caption=names(rolling_panel_regression_reer)[i])
#   )
# }
# dev.off()
#
#
#
# # expanding regression, one quarter at a time
## by doing this, we can see if the coefficient is changing over time. 
## if it is, it is a sign that monetary policy may be becoming more or less sensitive to certain indicators over time

expanding_taylor_regressions = list()
confidence_intervals = list()
for(a in c("Latin America","Non-LA EMEs")){ #for all the LACs and the Non-LA EMEs...

  first_round_taylor_regressions = data.frame()
  conf_ints = data.frame()

  form = as.formula("rate~CPI+gdp_output_gap+lag_rate+reer_quarter_perc_change+factor(country)+factor(date)")

  df_country = panel_df %>%
    filter(country_group==a) %>%
    mutate(year=year(date))

  for(yr in unique(df_country$date[df_country$date<="2022-12-01"&df_country$date>="2005-01-01"])){ #for every yr in btw 2005 and 2023...

    data_filtered = panel_df_prediction_data %>%
      filter(country_group==a&date>="1998-10-01"&date<=yr) #create a dataset that is from 1998 Q3 to your designated end date

    if(yr==as.Date("2005-12-01",format="%Y-%m-%d")&a=="Non-LA EMEs"){reg = lm(form,data=data_filtered %>% filter(country!="Indonesia"))}else{reg = lm(form,data=data_filtered)} 

    b = tidy(reg) # run the regression that we stated above. the lines below pull all the values you need for the table

    row = c(b[2,2],b[2,4],b[2,3],b[2,3],b[3,2],b[3,4],b[3,3],b[3,3],b[4,2],b[4,4],b[4,3],b[4,3],b[5,2],b[5,4],b[5,3],b[5,3],reg$df.residual,a,as.character(head(data_filtered$date,1)),as.character(tail(data_filtered$date,1)))
    rownames(row) = NULL
    colnames(row) = NULL
    row = unlist(row)
    first_round_taylor_regressions = rbind(first_round_taylor_regressions,row)
    row = data.frame(coef=c("inflation_coef","output_gap_coef","lag_rate_coef","reer_coef"),conf_low=unlist(c(b[2,3],b[3,3],b[4,3],b[5,3])),conf_high=unlist(c(b[2,3],b[3,3],b[4,3],b[5,3])),end_date=as.character(tail(data_filtered$date,1)))
    conf_ints=rbind(conf_ints,row)

    colnames(first_round_taylor_regressions) = c("inflation_coef","inflation_tstat","inflation_conf_low","inflation_conf_high","output_gap_coef","output_gap_tstat","output_gap_conf_low","output_gap_conf_high","lag_rate_coef","lag_rate_tstat","lag_rate_conf_low","lag_rate_conf_high","reer_coef","reer_tstat","reer_conf_low","reer_conf_high","num_obs","country","start_date","end_date")
  }

  expanding_taylor_regressions[[a]] = first_round_taylor_regressions
  confidence_intervals[[a]] = conf_ints

  #write.csv(first_round_taylor_regressions,paste0(results_folder,"taylor_results_2011_2022_time_trend.csv"))
}

# 08/01/2022 this was the code for figs 2 (now 7) and 8 (now 8)
pdf(paste0(results_folder,"Regression Results/2022-07-22_Taylor_Rule_expanded_LA/expanding_total_inflation1.pdf"),width=10,height=10)
for(i in c(1:length(expanding_taylor_regressions))){ # for both the LACs and Non-LA EMEs...
  plot_df = reshape2::melt(expanding_taylor_regressions[[i]] %>% mutate_at(vars(inflation_coef:reer_conf_high),as.numeric) %>%  select(end_date,inflation_coef,output_gap_coef,lag_rate_coef,reer_coef),id.vars="end_date",measure.vars=c("inflation_coef","output_gap_coef","lag_rate_coef","reer_coef"),variable.name="coef",value.name="value") %>% ungroup() %>%
    mutate(end_date=as.Date(end_date,format="%Y-%m-%d"))
  confidence_intervals[[i]]$end_date = as.Date(confidence_intervals[[i]]$end_date,format="%Y-%m-%d")
  plot_df = left_join(plot_df,confidence_intervals[[i]],by=c("coef","end_date")) %>%
    mutate(conf_low=value-(2*as.numeric(conf_low)), #calculate the confidence interval (from standard errors)
           conf_high=value+(2*as.numeric(conf_high)),
           coeff=case_when(
             coef=="inflation_coef"~"Headline Inflation",
             coef=="lag_rate_coef"~"Lagged Policy Rate",
             coef=="output_gap_coef"~"Output Gap",
             coef=="reer_coef"~"Real Exchange Rate"
           ))
  plot = ggplot(plot_df ,aes(x=end_date,y=value,colour=coeff)) +
    geom_line(size=2) +
    facet_wrap(~coeff,scales="free") +
    geom_ribbon(aes(ymin=conf_low, ymax=conf_high), linetype=2, alpha=0.1)+
    labs(colour="Coefficient",x="",y="Regression coefficient value",caption="Panels display coefficients estimated over period starting in 1998 Q4 and ending in quarter\nindicated on the x-axis.\nGrey areas represent 95% confidence intervals.") +
    theme(text = element_text(size=20),axis.title.x=element_blank(),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=10),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
          legend.position="none")
  ggsave(plot=plot,filename=paste0(charts_folder,"Scatterplots/2022-07-22_UpdatedCharts/expanding_reg_,",i,".png"),
         width=7,height=4,units="in")
  print(plot)
}
dev.off()

# create four panel charts for Latin American countries for Appendix 2
for(a in c("Brazil","Colombia","Chile","Mexico","Peru")){
  pdf(file=paste0(charts_folder,"Scatterplots/2023-03-27_UpdatedCharts/appendix_2_four_panel_charts_LA_20230327",a,".pdf"),width=17,height=22,compress=FALSE)
  
  #plot policy rate and headline inflation rate
  plot1 = ggplot() +
    geom_line(data=panel_df_prediction_data %>% filter(country==a&date>="2000-01-01"),aes(x=date,y=rate,colour="Policy rate"),size=1) +
    geom_line(data=panel_df_prediction_data %>% filter(country==a&date>="2000-01-01"),aes(x=date,y=CPI,colour="Headline inflation"),size=1) +
    labs(x="",y="",title="Rate (%)") +
    geom_hline(yintercept = 0)+
    theme(text = element_text(size=30),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
          legend.title=element_blank(),legend.position = c(.7,.85),legend.background = element_blank())

  #plot output gap
  plot2 = ggplot() +
    geom_line(data=panel_df_prediction_data %>% filter(country==a&date>="2000-01-01"),aes(x=date,y=gdp_output_gap),size=1) +
    labs(x="",y="",title="Output gap (%)") +
    coord_cartesian(ylim=c(-10,max(panel_df_prediction_data$gdp_output_gap[panel_df_prediction_data$country==a&panel_df_prediction_data$date>="2000-01-01"]+.5))) +
    geom_hline(yintercept=0) +
    theme(text = element_text(size=30),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

  #plot real gdp index
  plot3 = ggplot() +
    geom_line(data=panel_df_prediction_data %>% filter(country==a&date>="2000-01-01"),aes(x=date,y=gdp_index,colour="Actual GDP"),size=1) +
    geom_line(data=panel_df_prediction_data %>% filter(country==a&date>="2000-01-01"),aes(x=date,y=gdp_trend,colour="Trend GDP"),size=1) +
    labs(x="",title="Real GDP Index (2020 Q1 = 100)",y="") +
    scale_color_manual(labels=c("Actual GDP","Trend GDP"),values=c("blue","red")) +
    theme(text = element_text(size=30),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
          legend.title=element_blank(),legend.position = c(.2,.8),legend.background = element_blank())

  #plot real effective exchange rate
  plot4 = ggplot() +
    geom_line(data=panel_df_prediction_data %>% filter(country==a&date>="2000-01-01"),aes(x=date,y=reer_index),size=1) +
    labs(x="",title="Real effective exchange rate",y="") +
    theme(text = element_text(size=30),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
          legend.position="none")

  print(grid.arrange(plot1,plot2,plot3,plot4,top=grid::textGrob(a,gp=grid::gpar(fontsize=30,font=3)),ncol=1))
  dev.off()
}



