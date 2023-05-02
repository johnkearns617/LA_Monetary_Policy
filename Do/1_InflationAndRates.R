# 1_InflationAndRates_072022_Updated.R
# Beatrice Lee
# Goal: Do-file to gather inflation rate and policy rate data for 22 EMEs for as far back as possible. Aim is to estimate a Taylor Rule
# Date Created: 2021-09-22
# Last Updated: 2023-04-10 \\ bal

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
library(BISdata)
source(paste0(do_folder,"Mode1.R"))

# Get list  of countries needed
countries = c("Brazil","China","Chile","Colombia","Czech Republic","Hungary","Indonesia",
              "India","Israel","Korea","Malaysia","Mexico","Philippines","Poland","Romania","South Korea",
              "Russia","South Africa","Singapore","Taiwan","Thailand","Turkey","Hong Kong")
full_countries = c("Brazil","China","Chile","Colombia","Czech Republic","Hungary","Indonesia",
              "India","Israel","Korea","Malaysia","Mexico","Philippines","Poland","Romania","South Korea",
              "Russia","South Africa","Singapore","Taiwan","Thailand","Turkey","Hong Kong","Argentina","Peru","Croatia","Saudi Arabia","Serbia","North Macedonia") # What Steve thinks an EME is
emes = c("Brazil","China","Chile","Colombia","Hungary","Indonesia",
                     "India","Malaysia","Mexico","Philippines","Poland","Romania",
                     "Russia","South Africa","Thailand","Turkey","Argentina","Peru","Croatia","Saudi Arabia","Serbia","North Macedonia") # this list removes countries like South Korea that steve did not want to keep as EMEs

all_countries = c("Hong Kong SAR (China)", "India", "Indonesia", "Philippines", "Singapore", 
                  "Taiwan", "Thailand", "Czech Republic", "Hungary", "Poland", 
                  "Romania", "Russian Federation", "Israel", "South Africa", "Brazil", 
                  "Chile", "Colombia","South Korea", "Mexico", "Turkey", 
                  "China", "Malaysia", "Argentina", "Australia", "Canada", 
                  "Switzerland", "Denmark", "United Kingdom","Croatia","Iceland" ,       
                  "Japan", "Norway", "Peru", "Saudi Arabia", "Sweden", 
                  "European Union", "Serbia", "North Macedonia", "New Zealand", "United States")     

# load in inflation
# 4.11.2023 - This is monthly headline inflation from CEIC.
# we used to use a combined interest rate/ headline CPI dataset, but since it had been a while since we did a fresh pull from CEIC
# and we never used the CEIC interest rate data, I did some minor restructuring of the code to take a dataset that contained
# only headline CPI.

headline_cpi_data = read_excel(paste0(data_folder,"Raw/Headline CPI Monthly YoY change_20230411.xlsx"),
                      col_names = FALSE)
colnames(headline_cpi_data) = c("date",paste0(c(rep("CPI_",117)),headline_cpi_data[2,2:ncol(headline_cpi_data)])) # this dataset contains CPI and policy rate data (we won't really ever use policy rate data from CEIC. The BIS is better)
headline_cpi_data = headline_cpi_data[-c(1:27),]
headline_cpi_data = headline_cpi_data %>%
  mutate(date=as.Date(as.numeric(date),origin="1899-12-30"))

headline_df = reshape2::melt(headline_cpi_data,id.vars="date",measure.vars=colnames(headline_cpi_data)[2:ncol(headline_cpi_data)]) %>%
  separate(variable,c("type","country"),sep="_") %>%
  filter(country %in% all_countries)

headline_df$country = str_replace_all(headline_df$country,setNames(c("South Korea","Russia","Hong Kong"),c("South Korea","Russian Federation","Hong Kong SAR \\(China\\)")))

headline_df = headline_df %>%
  mutate(value=as.numeric(value)/100) %>%
  group_by(date,type) %>% #for a year and metric
  mutate(avg=mean(value,na.rm=TRUE), #calculate the global average for that year and metric
         advanced=ifelse(country%in%emes,"Emerging Market Economy","Advanced Economy"), # IMF definition
         advanced1 = ifelse(country%in%full_countries,"Emerging Market Economy","Advanced Economy")) # Steve definition

# fix CPI for Australia and New Zealand
df = headline_df %>%
  group_by(country,type,quarter(date),year(date)) %>%
  mutate(value=ifelse(country%in%c("New Zealand","Australia"),Mode1(value,na.rm=TRUE),value)) %>%
  ungroup() %>%
  select(-c(`quarter(date)`,`year(date)`))
df$country = ifelse(df$country=="European Union","Euro area",df$country)

# raw_data = read_excel(paste0(data_folder,"Raw/EME_Inflation_Policy_Rates_08152022.xlsx"),
#                       col_names = FALSE)
# colnames(raw_data) = c("date",paste0(c(rep("CPI_",21),rep("rate_",22)),raw_data[2,2:ncol(raw_data)])) # this dataset contains CPI and policy rate data (we won't really ever use policy rate data from CEIC. The BIS is better)
# raw_data = raw_data[-c(1:26),]
# raw_data = raw_data %>%
#   mutate(date=as.Date(as.numeric(date),origin="1899-12-30"))
# 
# df = reshape2::melt(raw_data,id.vars="date",measure.vars=colnames(raw_data)[2:ncol(raw_data)]) %>%
#   separate(variable,c("type","country"),sep="_")
# df$country = str_replace_all(df$country,setNames(c("South Korea","Russia","Hong Kong"),c("South Korea","Russian Federation","Hong Kong SAR \\(China\\)")))
# 
# df = df %>%
#   mutate(value=as.numeric(value)/100) %>%
#   group_by(date,type) %>% #for a year and metric
#   mutate(avg=mean(value,na.rm=TRUE), #calculate the global average for that year and metric
#          advanced=ifelse(country%in%emes,"Emerging Market Economy","Advanced Economy"), # IMF definition
#          advanced1 = ifelse(country%in%full_countries,"Emerging Market Economy","Advanced Economy")) # Steve definition
# 
# # fix CPI for Australia and New Zealand
# df = df %>%
#   group_by(country,type,quarter(date),year(date)) %>%
#   mutate(value=ifelse(country%in%c("New Zealand","Australia"),Mode1(value,na.rm=TRUE),value)) %>%
#   ungroup() %>%
#   select(-c(`quarter(date)`,`year(date)`))
# df$country = ifelse(df$country=="European Union","Euro area",df$country)

# bring in BIS data (on central bank policy rates) and replace. the link should contain updated data (you don't have to pull a new file from bls)
bis_daily <- BISdata::fetch_dataset(dest.dir = paste0(data_folder,"Raw/BIS_data_folder/"),
                          "https://www.bis.org/statistics/full_cbpol_d_csv_row.zip")

bis_daily[1,2:ncol(bis_daily)] = unlist(str_split(bis_daily[1,2:ncol(bis_daily)],":"))[seq(2,78,2)]
colnames(bis_daily) = c("date",bis_daily[1,2:ncol(bis_daily)])
bis_daily = bis_daily[-c(1:8),]
bis_daily = reshape2::melt(bis_daily,
                           id.vars="date",
                           measure.vars=colnames(bis_daily)[2:ncol(bis_daily)],
                           variable.name="country",value.name="rate")
bis_daily = bis_daily %>%
  mutate(rate=as.numeric(rate)/100,
         country=as.character(country)) %>%
  filter(!is.nan(rate)) %>% # filter out any countries that are missing the policy rate
  group_by(country) %>%
  tidyr::fill(rate) %>%
  ungroup() %>%
  filter(!is.na(rate))
bis_daily$country = ifelse(bis_daily$country=="Hong Kong SAR","Hong Kong",bis_daily$country)
bis_daily$country = ifelse(bis_daily$country=="Korea","South Korea",bis_daily$country)
bis_daily = bis_daily %>%
  filter(country%in%df$country) %>% #filter 
  mutate(date=as.Date(date,format="%Y-%m-%d")) %>%
  group_by(year(date),month(date),country) %>%
  summarise(rate=rate[n()],
            date=date[n()]) %>%
  mutate(date=as.Date(paste0(substr(date,1,8),"01"),format="%Y-%m-%d")) %>%
  ungroup() %>%
  select(-c(`year(date)`,`month(date)`)) %>%
  rename(value=rate) %>%
  mutate(type="rate")


# augment with Peru policy rate data
peru_rate = readxl::read_excel(paste0(data_folder,"Raw/peru_policy_rate_07202022.xlsx")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(country="Peru",
         type="rate") %>%
  group_by(month(date),year(date)) %>%
  summarise(rate=median(tail(rate,15),na.rm=TRUE),
            date=date[n()]) %>%
  mutate(date=as.Date(paste0(substr(date,1,8),"01"),format="%Y-%m-%d")) %>%
  ungroup() %>%
  select(-c(`year(date)`,`month(date)`)) %>%
  mutate(type="rate",
         rate=rate/100,
         country="Peru") %>%
  rename(value=rate)

bis_daily = bind_rows(bis_daily,peru_rate[peru_rate$date<"2003-09-01",]) %>% # get Peru data starting in 2003
  arrange(country, date)

# join together the BIS rate data except for Taiwan with CEIC inflation data
df1 = bind_rows(df %>% filter(!(type=="rate"&country!="Taiwan")&country!="Singapore"&country!="Croatia"),bis_daily) %>%
  drop_na(country)

# plot
# using CEIC data only
# ggplot(df %>% filter(country%in%countries),aes(x=date,y=value,color=type)) +
#   geom_line(size=2) +
#   facet_wrap(~country,scales="free",ncol=3)
#
# pdf(file=paste0(charts_folder,"inflation_policy_rate_by_country.pdf"),width=15,height=10)
# for(v in unique(df$country[df$country%in%countries])){
#   print(ggplot(df %>% filter(country==v) %>% drop_na(value) %>% mutate(type=ifelse(type=="CPI","12-Month CPI\nInflation Rate","Policy Rate")) %>% filter(!(country%in%c("Brazil","Chile")&date<"1997-01-01")&!(country=="Russia"&date<"2000-01-01")),aes(x=date,y=value,color=type)) +
#           geom_line(size=2) +
#           scale_y_continuous(labels=scales::percent) +
#           labs(caption=v,y="12-Month CPI Inflation Rate (%) or Policy Rate (%)") +
#           theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
#                 axis.title.x = element_blank())
#   )
# }
# dev.off()
#
# # using CEIC CPI data and BIS policy rate data (except for Taiwan)
# pdf(file=paste0(charts_folder,"inflation_policy_rate_by_country_BIS.pdf"),width=15,height=10)
# for(v in unique(df1$country[df1$country%in%countries])){
#   print(ggplot(df1 %>% filter(country==v) %>% drop_na(value) %>% mutate(type=ifelse(type=="CPI","12-Month CPI\nInflation Rate","Policy Rate")) %>% filter(date>="2018-01-01"),aes(x=date,y=value,color=type)) +
#           geom_line(size=2) +
#           scale_y_continuous(labels=scales::percent) +
#           labs(caption=v,y="12-Month CPI Inflation Rate (%) or Policy Rate (%)") +
#           theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"),
#                 axis.title.x = element_blank())
#   )
# }
# dev.off()
#
# # scatter plots for steve showing relationship between policy rates and inflation
# df_scatter = df1 %>%
#   drop_na(value) %>%
#   group_by(country,type) %>%
#   summarise(value_dec_2020 = value[n()]-value[date=="2020-12-01"],
#             advanced=advanced[1],
#             advanced1=advanced1[1]) %>%
#   group_by(country) %>%
#   mutate(advanced=Mode1(advanced,na.rm=TRUE),
#          advanced1=Mode1(advanced1,na.rm=TRUE)) %>%
#   ungroup()
# df_scatter = reshape2::dcast(df_scatter,country+advanced+advanced1~type,value.var="value_dec_2020")
#
# ggplot(df_scatter %>% filter(country %in% countries),aes(x=CPI,y=rate)) +
#   stat_smooth(method="lm") +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   scale_y_continuous(labels=scales::percent) +
#   scale_x_continuous(labels=scales::percent) +
#   labs(y="Percentage point change in policy rate since Dec 2020",x="Percentage point change in 12-month inflation since Dec 2020") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#
# df_scatter = df1 %>%
#   drop_na(value) %>%
#   group_by(country,type) %>%
#   summarize(value_dec_2017 = value[n()]-value[date=="2017-12-01"]) %>%
#   ungroup()
# df_scatter = reshape2::dcast(df_scatter,country~type)
#
# ggplot(df_scatter %>% filter(country %in% countries),aes(x=CPI,y=rate)) +
#   stat_smooth(method="lm") +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   scale_y_continuous(labels=scales::percent) +
#   scale_x_continuous(labels=scales::percent) +
#   labs(y="Percentage point change in policy rate since Dec 2017",x="Percentage point change in 12-month inflation since Dec 2017") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
# df_scatter = df1 %>%
#   drop_na(value) %>%
#   group_by(country,type) %>%
#   summarize(value_dec_2017 = value[date=="2019-12-01"]-value[date=="2017-12-01"]) %>%
#   ungroup()
# df_scatter = reshape2::dcast(df_scatter,country~type)
#
# ggplot(df_scatter %>% filter(country %in% countries),aes(x=CPI,y=rate)) +
#   stat_smooth(method="lm") +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   scale_y_continuous(labels=scales::percent) +
#   scale_x_continuous(labels=scales::percent) +
#   labs(y="Percentage point change in policy rate since Dec 2017 to Dec 2019",x="Percentage point change in 12-month inflation since Dec 2017 to Dec 2019") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#
# df_scatter = df1 %>%
#   drop_na(value) %>%
#   group_by(country,type) %>%
#   summarize(value_dec_2020 = value[n()]-value[date=="2020-12-01"],
#             advanced=advanced[1],
#             advanced1=advanced1[1]) %>%
#   group_by(country) %>%
#   mutate(advanced=Mode1(advanced,na.rm=TRUE),
#          advanced1=Mode1(advanced1,na.rm=TRUE)) %>%
#   ungroup()
# df_scatter = reshape2::dcast(df_scatter,country+advanced+advanced1~type,value.var="value_dec_2020")
#
# ggplot(df_scatter,aes(x=CPI,y=rate,colour=advanced1)) +
#   stat_smooth(method="lm",aes(group=1)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country),min.segment.length = 0) +
#   scale_y_continuous(labels=scales::percent) +
#   scale_x_continuous(labels=scales::percent) +
#   labs(y="Percentage point change in policy rate since Dec 2020",x="Percentage point change in 12-month inflation since Dec 2020") +
#   guides(colour=guide_legend(title="Economy Type")) +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
# ggplot(df_scatter,aes(x=CPI,y=rate,colour=advanced)) +
#   stat_smooth(method="lm",aes(group=1)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country),min.segment.length=0) +
#   scale_y_continuous(labels=scales::percent) +
#   scale_x_continuous(labels=scales::percent) +
#   labs(y="Percentage point change in policy rate since Dec 2020",x="Percentage point change in 12-month inflation since Dec 2020") +
#   guides(colour=guide_legend(title="Economy Type (IMF)")) +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
# ggplot(df_scatter %>% filter(country!="Argentina"),aes(x=CPI,y=rate,colour=advanced)) +
#   stat_smooth(method="lm",aes(group=1)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country),min.segment.length=0) +
#   scale_y_continuous(labels=scales::percent) +
#   scale_x_continuous(labels=scales::percent) +
#   labs(y="Percentage point change in policy rate since Dec 2020",x="Percentage point change in 12-month inflation since Dec 2020") +
#   guides(colour=guide_legend(title="Economy Type (IMF)")) +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))

#save.image(file=paste0(data_folder,"Processing/1_Inflation_and_Rates.RData"))

# df_scatter = df1 %>%
#   drop_na(value) %>%
#   group_by(country,type) %>%
#   summarize(value_dec_2020 = value[n()]-value[date=="2020-12-01"],
#             advanced=advanced[1],
#             advanced1=advanced1[1]) %>%
#   group_by(country) %>%
#   mutate(advanced=Mode1(advanced,na.rm=TRUE),
#          advanced1=Mode1(advanced1,na.rm=TRUE)) %>%
#   ungroup()
# df_scatter = reshape2::dcast(df_scatter,country+advanced+advanced1~type,value.var="value_dec_2020")
#
# df_scatter_monthly = panel_df_monthly %>%
#   filter(date>="2019-12-01") %>%
#   drop_na(rate,CPI) %>%
#   group_by(country) %>%
#   summarize(total_cpi_dec_2019_july = CPI[n()]-CPI[date=="2020-12-01"],
#             core_cpi_dec_2019_july = core_inflation[n()]-core_inflation[date=="2020-12-01"],
#             ip_output_gap_dec_2019_july = output_gap[date=="2020-06-01"]-output_gap[date=="2019-12-01"],
#             policy_rate_dec_2019_july = rate[n()]-rate[date=="2020-12-01"],
#             advanced=advanced[1]) %>%
#   mutate(advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced)) %>%
#   ungroup()
#
#
# ggplot(df_scatter_monthly,aes(x=total_cpi_dec_2019_july,y=policy_rate_dec_2019_july,colour=advanced)) +
#   stat_smooth(method="lm",inherit.aes=FALSE,aes(x=total_cpi_dec_2019_july,y=policy_rate_dec_2019_july)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   labs(y="Percentage point change in policy rate\nfrom Dec 2020 to Present",x="Percentage point change in 12-month inflation from Dec 2020 to Present") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
# df_scatter_monthly = panel_df_monthly %>%
#   filter(date>="2019-12-01") %>%
#   drop_na(rate,core_inflation) %>%
#   group_by(country) %>%
#   summarize(total_cpi_dec_2019_july = CPI[n()]-CPI[date=="2020-12-01"],
#             core_cpi_dec_2019_july = core_inflation[n()]-core_inflation[date=="2020-12-01"],
#             ip_output_gap_dec_2019_july = output_gap[date=="2020-06-01"]-output_gap[date=="2019-12-01"],
#             policy_rate_dec_2019_july = rate[n()]-rate[date=="2020-12-01"],
#             policy_rate_dec_2019_june_2020 = rate[date=="2020-06-01"]-rate[date=="2019-12-01"],
#             advanced=advanced[1]) %>%
#   mutate(advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced)) %>%
#   ungroup()
#
# ggplot(df_scatter_monthly,aes(x=core_cpi_dec_2019_july,y=policy_rate_dec_2019_july,colour=advanced)) +
#   stat_smooth(method="lm",inherit.aes=FALSE,aes(x=core_cpi_dec_2019_july,y=policy_rate_dec_2019_july)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   labs(y="Percentage point change in policy rate\nfrom Dec 2020 to Present",x="Percentage point change in 12-month core inflation from Dec 2020 to Present") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
# ggplot(df_scatter_monthly %>% filter(country!="Philippines"),aes(x=ip_output_gap_dec_2019_july,y=policy_rate_dec_2019_june_2020,colour=advanced)) +
#   stat_smooth(method="lm",inherit.aes=FALSE,aes(x=ip_output_gap_dec_2019_july,y=policy_rate_dec_2019_june_2020)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   labs(y="Percentage point change in policy rate\nfrom Dec 2019 to June 2020",x="Percentage point change in IP output gap from Dec 2019 to June 2020") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#
# df_scatter_quarterly = panel_df %>%
#   filter(date>="2019-12-01") %>%
#   drop_na(rate,CPI) %>%
#   group_by(country) %>%
#   summarize(total_cpi_dec_2020_q3 = CPI[n()]-CPI[date=="2020-12-01"],
#             core_cpi_dec_2020_q3 = core_inflation[n()]-core_inflation[date=="2020-12-01"],
#             gdp_output_gap_dec_2020_q3 = gdp_output_gap[date=="2020-06-01"]-gdp_output_gap[date=="2019-12-01"],
#             policy_rate_dec_2020_q3 = rate[n()]-rate[date=="2020-12-01"],
#             policy_rate_dec_2019_q2_2020 = rate[date=="2020-06-01"]-rate[date=="2019-12-01"],
#             advanced=advanced[1]) %>%
#   mutate(advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced)) %>%
#   ungroup()
#
# ggplot(df_scatter_quarterly,aes(x=total_cpi_dec_2020_q3,y=policy_rate_dec_2020_q3,colour=advanced)) +
#   stat_smooth(method="lm",inherit.aes=FALSE,aes(x=total_cpi_dec_2020_q3,y=policy_rate_dec_2020_q3)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   labs(y="Percentage point change in policy rate\nfrom Q4 2020 to Present",x="Percentage point change in 12-month inflation from Q4 2020 to Present") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
# df_scatter_quarterly = panel_df %>%
#   filter(date>="2019-12-01") %>%
#   drop_na(rate,core_inflation) %>%
#   group_by(country) %>%
#   summarize(total_cpi_dec_2020_q3 = CPI[n()]-CPI[date=="2020-12-01"],
#             core_cpi_dec_2020_q3 = core_inflation[n()]-core_inflation[date=="2020-12-01"],
#             gdp_output_gap_dec_2020_q3 = gdp_output_gap[date=="2020-06-01"]-gdp_output_gap[date=="2019-12-01"],
#             policy_rate_dec_2020_q3 = rate[n()]-rate[date=="2020-12-01"],
#             policy_rate_dec_2019_q2_2020 = rate[date=="2020-06-01"]-rate[date=="2019-12-01"],
#             advanced=advanced[1]) %>%
#   mutate(advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced)) %>%
#   ungroup()
#
# ggplot(df_scatter_quarterly,aes(x=core_cpi_dec_2020_q3,y=policy_rate_dec_2020_q3,colour=advanced)) +
#   stat_smooth(method="lm",inherit.aes=FALSE,aes(x=core_cpi_dec_2020_q3,y=policy_rate_dec_2020_q3)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   labs(y="Percentage point change in policy rate\nfrom Q4 2020 to Present",x="Percentage point change in 12-month core inflation from Q4 2020 to Present") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
# ggplot(df_scatter_quarterly %>% filter(country!="Philippines"),aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020,colour=advanced)) +
#   stat_smooth(method="lm",inherit.aes=FALSE,aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   labs(y="Percentage point change in policy rate\nfrom Q4 2019 to Q2 2020",x="Percentage point change in GDP output gap from Q4 2019 to Q2 2020") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#
# # do it in percentages
# df_scatter_monthly = panel_df_monthly %>%
#   filter(date>="2019-12-01") %>%
#   drop_na(rate,CPI) %>%
#   group_by(country) %>%
#   summarize(total_cpi_dec_2019_july = (CPI[n()]-CPI[date=="2020-12-01"]),
#             core_cpi_dec_2019_july = (core_inflation[n()]-core_inflation[date=="2020-12-01"]),
#             ip_output_gap_dec_2019_july = (output_gap[date=="2020-06-01"]-output_gap[date=="2019-12-01"]),
#             policy_rate_dec_2019_july = (rate[n()]-rate[date=="2020-12-01"]),
#             advanced=advanced[1]) %>%
#   mutate(advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced)) %>%
#   ungroup()
#
# library(ggpmisc)
#
# ggplot(df_scatter_monthly,aes(x=total_cpi_dec_2019_july,y=policy_rate_dec_2019_july,colour=advanced)) +
#   stat_smooth(method="lm",inherit.aes=FALSE,aes(x=total_cpi_dec_2019_july,y=policy_rate_dec_2019_july)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   stat_poly_eq(formula=y~x,aes(x=total_cpi_dec_2019_july,y=policy_rate_dec_2019_july,color=NA,label=paste(..eq.label..,..p.value..,sep="~~~")),color="black",parse=TRUE) +
#   labs(y="Percentage point change in policy rate\nfrom Dec 2020 to Present",x="Percentage point change in 12-month inflation from Dec 2020 to Present") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
# df_scatter_monthly = panel_df_monthly %>%
#   filter(date>="2019-12-01") %>%
#   drop_na(rate,core_inflation) %>%
#   group_by(country) %>%
#   summarize(total_cpi_dec_2019_july = (CPI[n()]-CPI[date=="2020-12-01"]),
#             core_cpi_dec_2019_july = (core_inflation[n()]-core_inflation[date=="2020-12-01"]),
#             ip_output_gap_dec_2019_july = (output_gap[date=="2020-06-01"]-output_gap[date=="2019-12-01"]),
#             policy_rate_dec_2019_july = (rate[n()]-rate[date=="2020-12-01"]),
#             policy_rate_dec_2019_june_2020 = (rate[date=="2020-06-01"]-rate[date=="2019-12-01"])/abs(rate[date=="2019-12-01"]),
#             advanced=advanced[1]) %>%
#   mutate(advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced),
#          policy_rate_dec_2019_july = ifelse(is.infinite(policy_rate_dec_2019_july),NA,policy_rate_dec_2019_july),
#          policy_rate_dec_2019_july = ifelse(is.nan(policy_rate_dec_2019_july),0,policy_rate_dec_2019_july),
#          policy_rate_dec_2019_june_2020 = ifelse(is.infinite(policy_rate_dec_2019_june_2020),NA,policy_rate_dec_2019_june_2020),
#          policy_rate_dec_2019_june_2020 = ifelse(is.nan(policy_rate_dec_2019_june_2020),0,policy_rate_dec_2019_june_2020)) %>%
#   ungroup()
#
# ggplot(df_scatter_monthly,aes(x=core_cpi_dec_2019_july,y=policy_rate_dec_2019_july,colour=advanced)) +
#   stat_smooth(data=df_scatter_monthly,method="lm",inherit.aes=FALSE,aes(x=core_cpi_dec_2019_july,y=policy_rate_dec_2019_july)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   stat_poly_eq(formula=y~x,aes(x=core_cpi_dec_2019_july,y=policy_rate_dec_2019_july,color=NA,label=paste(..eq.label..,..p.value..,sep="~~~")),color="black",parse=TRUE) +
#   labs(y="Percentage point change in policy rate\nfrom Dec 2020 to Present",x="Percentage point change in 12-month core inflation from Dec 2020 to Present") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
# ggplot(df_scatter_monthly %>% filter(country!="Philippines"),aes(x=ip_output_gap_dec_2019_july,y=policy_rate_dec_2019_june_2020,colour=advanced)) +
#   stat_smooth(method="lm",inherit.aes=FALSE,aes(x=ip_output_gap_dec_2019_july,y=policy_rate_dec_2019_june_2020)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   scale_y_continuous(labels=scales::percent) +
#   stat_poly_eq(formula=y~x,aes(x=ip_output_gap_dec_2019_july,y=policy_rate_dec_2019_june_2020,color=NA,label=paste(..eq.label..,..p.value..,sep="~~~")),color="black",parse=TRUE) +
#   labs(y="Proportional change in policy rate\nfrom Dec 2019 to June 2020",x="Percentage point change in IP output gap from Dec 2019 to June 2020") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
#
# df_scatter_quarterly = panel_df %>%
#   filter(date>="2019-12-01") %>%
#   drop_na(rate,CPI) %>%
#   group_by(country) %>%
#   summarize(total_cpi_dec_2020_q3 = (CPI[n()]-CPI[date=="2020-12-01"]),
#             core_cpi_dec_2020_q3 = (core_inflation[n()]-core_inflation[date=="2020-12-01"]),
#             gdp_output_gap_dec_2020_q3 = (gdp_output_gap[date=="2020-06-01"]-gdp_output_gap[date=="2019-12-01"]),
#             policy_rate_dec_2020_q3 = (rate[n()]-rate[date=="2020-12-01"]),
#             policy_rate_dec_2019_q2_2020 = (rate[date=="2020-06-01"]-rate[date=="2019-12-01"])/abs(rate[date=="2019-12-01"]),
#             advanced=advanced[1]) %>%
#   mutate(advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced),
#          policy_rate_dec_2020_q3 = ifelse(is.infinite(policy_rate_dec_2020_q3),NA,policy_rate_dec_2020_q3),
#          policy_rate_dec_2020_q3 = ifelse(is.nan(policy_rate_dec_2020_q3),0,policy_rate_dec_2020_q3)) %>%
#   ungroup()
#
# ggplot(df_scatter_quarterly,aes(x=total_cpi_dec_2020_q3,y=policy_rate_dec_2020_q3,colour=advanced)) +
#   stat_smooth(method="lm",inherit.aes=FALSE,aes(x=total_cpi_dec_2020_q3,y=policy_rate_dec_2020_q3)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   stat_poly_eq(formula=y~x,aes(x=total_cpi_dec_2020_q3,y=policy_rate_dec_2020_q3,color=NA,label=paste(..eq.label..,..p.value..,sep="~~~")),color="black",parse=TRUE) +
#   labs(y="Percentage point change in policy rate\nfrom Q4 2020 to Present",x="Percentage point change in 12-month inflation from Q4 2020 to Present") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
# df_scatter_quarterly = panel_df %>%
#   filter(date>="2019-12-01") %>%
#   drop_na(rate,core_inflation) %>%
#   group_by(country) %>%
#   summarize(total_cpi_dec_2020_q3 = (CPI[n()]-CPI[date=="2020-12-01"]),
#             core_cpi_dec_2020_q3 = (core_inflation[n()]-core_inflation[date=="2020-12-01"]),
#             gdp_output_gap_dec_2020_q3 = (gdp_output_gap[date=="2020-06-01"]-gdp_output_gap[date=="2019-12-01"]),
#             policy_rate_dec_2020_q3 = (rate[n()]-rate[date=="2020-12-01"]),
#             policy_rate_dec_2019_q2_2020 = (rate[date=="2020-06-01"]-rate[date=="2019-12-01"])/abs(rate[date=="2019-12-01"]),
#             advanced=advanced[1]) %>%
#   mutate(advanced=ifelse(country%in%c("Brazil","Chile","Peru","Colombia","Mexico"),"Latin America",advanced),
#          policy_rate_dec_2020_q3 = ifelse(is.infinite(policy_rate_dec_2020_q3),NA,policy_rate_dec_2020_q3),
#          policy_rate_dec_2020_q3 = ifelse(is.nan(policy_rate_dec_2020_q3),0,policy_rate_dec_2020_q3),
#          policy_rate_dec_2019_q2_2020 = ifelse(is.infinite(policy_rate_dec_2019_q2_2020),NA,policy_rate_dec_2019_q2_2020),
#          policy_rate_dec_2019_q2_2020 = ifelse(is.nan(policy_rate_dec_2019_q2_2020),0,policy_rate_dec_2019_q2_2020)) %>%
#   ungroup()
#
# ggplot(df_scatter_quarterly,aes(x=core_cpi_dec_2020_q3,y=policy_rate_dec_2020_q3,colour=advanced)) +
#   stat_smooth(method="lm",inherit.aes=FALSE,aes(x=core_cpi_dec_2020_q3,y=policy_rate_dec_2020_q3)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   stat_poly_eq(formula=y~x,aes(x=core_cpi_dec_2020_q3,y=policy_rate_dec_2020_q3,color=NA,label=paste(..eq.label..,..p.value..,sep="~~~")),color="black",parse=TRUE) +
#   labs(y="Percentage point change in policy rate\nfrom Q4 2020 to Present",x="Percentage point change in 12-month core inflation from Q4 2020 to Present") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
# ggplot(df_scatter_quarterly %>% filter(country!="Philippines"),aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020,colour=advanced)) +
#   stat_smooth(method="lm",inherit.aes=FALSE,aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020)) +
#   geom_point(size=3) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept= 0) +
#   geom_label_repel(aes(label = country)) +
#   scale_y_continuous(labels=scales::percent) +
#   stat_poly_eq(formula=y~x,aes(x=gdp_output_gap_dec_2020_q3,y=policy_rate_dec_2019_q2_2020,color=NA,label=paste(..eq.label..,..p.value..,sep="~~~")),color="black",parse=TRUE) +
#   labs(y="Proportional change in policy rate\nfrom Q4 2019 to Q2 2020",x="Percentage point change in GDP output gap from Q4 2019 to Q2 2020") +
#   theme(text = element_text(size=20),axis.text.x = element_text(size=17),axis.text.y = element_text(size=17),axis.title.y=element_text(size=17),plot.caption = element_text(face="italic",hjust = 0,size=15),panel.background = element_rect(fill = "white",colour="black"),panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), panel.grid.minor = element_line(size = 0.25, linetype="solid",colour = "grey"))
#
