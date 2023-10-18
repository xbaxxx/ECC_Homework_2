
####Annual Surface Temperature Change

setwd("C:/Users/~")

#Import data

d.f.ASTC<-as.data.frame(read.csv("Annual_Surface_Temperature_Change.csv"))


####1st Arrange & subset data####

df_filter<-d.f.ASTC[,c("Country","F1981","F1982","F1983","F1984","F1985","F1986","F1987","F1988","F1989","F1990","F1991","F1992","F1993","F1994","F1995","F1996","F1997","F1998","F1999","F2000","F2001","F2002","F2003","F2004","F2005","F2006","F2007","F2008","F2009","F2010","F2011","F2012","F2013","F2014","F2015","F2016","F2017","F2018","F2019","F2020","F2021","F2022")]

NL<-df_filter[c(df_filter$Country=="Netherlands, The"),]
US<-df_filter[c(df_filter$Country=="United States"),]
DNM<-df_filter[c(df_filter$Country=="Denmark"),]
SWD<-df_filter[c(df_filter$Country=="Sweden"),]
IND<-df_filter[c(df_filter$Country=="India"),]
CND<-df_filter[c(df_filter$Country=="Canada"),]
CHN<-df_filter[c(df_filter$Country=="China, P.R.: Mainland"),]
RUS<-df_filter[c(df_filter$Country=="Russian Federation"),]
UK<-df_filter[c(df_filter$Country=="United Kingdom"),]
BRA<-df_filter[c(df_filter$Country=="Brazil"),]
CHL<-df_filter[c(df_filter$Country=="Chile"),]
FNL<-df_filter[c(df_filter$Country=="Finland"),]
FRA<-df_filter[c(df_filter$Country=="France"),]
GER<-df_filter[c(df_filter$Country=="Germany"),]
JPN<-df_filter[c(df_filter$Country=="Japan"),]
NZL<-df_filter[c(df_filter$Country=="New Zealand"),]
NOR<-df_filter[c(df_filter$Country=="Norway"),]
WRLD<-df_filter[c(df_filter$Country=="World"),]

d<-rbind(NL,US,NOR,DNM,SWD,IND,CND,CHN,RUS,UK,BRA,CHL,FNL,FRA,GER,JPN,NZL,WRLD)

####2nd Convert to panel####

library(tidyr)


d.panel<-d %>% pivot_longer(cols=c('F1981','F1982','F1983','F1984','F1985','F1986','F1987',
                          'F1988','F1989','F1990','F1991','F1992','F1993','F1994',
                          'F1995','F1996','F1997','F1998','F1999','F2000','F2001',
                          'F2002','F2003','F2004','F2005','F2006','F2007','F2008',
                          'F2009','F2010','F2011','F2012','F2013','F2014','F2015',
                          'F2016','F2017','F2018','F2019','F2020','F2021','F2022'),
                   names_to = "Year",
                   values_to='ASTC')


d.panel.filter<-d.panel[c(d.panel$Country=="Norway"|
                          d.panel$Country=="World"|d.panel$Country=="France"|
                            d.panel$Country=="India"|d.panel$Country=="United States"),]


d.panel.filter$Year<-sub("F","",d.panel.filter$Year)

d.panel.filter$Year<-as.numeric(d.panel.filter$Year)





####3rd Create the graphs####

##Good Graph

library(ggplot2)
library(grid)

g_g<-ggplot(d.panel.filter,aes(Year,ASTC,group=Country,colour=Country,size=Country))+geom_smooth()+theme_minimal()+labs(x="Year",y="Average Surface Temperature Increase (°C)")+theme(plot.margin=unit(c(1,3,1,1),"lines"))+scale_size_manual(values=c(0.5,0.5,0.5,0.5,2))+scale_color_manual(values=c("darkgreen","red","darkgreen","red","black"))

    g_g

###order of the lines is France, Netherlands,Germany, United Kingdom, China ,World,United States, India

##Bad Graph

g_g1<-ggplot(d.panel.filter,aes(Year,ASTC,group=Country,colour=Country,size=Country))+geom_line()+labs(x="Year",y="Average Surface Temperature Increase (°C)")+ylim(-1,15)+theme(plot.margin=unit(c(1,3,1,1),"lines"))+scale_size_manual(values=c(0.5,0.5,0.5,0.5,0.5))


    g_g1





