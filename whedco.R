---
  title: "Untitled"
author: "Bailey Nix"
date: "2024-01-23"
output: pdf_document
---
  
  ```{r, message=FALSE}
library(tidyverse)
library(ggplot2)
setwd("/Users/baileynix/Documents/WHEDco Project")
#NYC Open data
dat<-read_csv("Evictions_Bronx_Only_20240123.csv")

#these datasets were not necessary. keeping in code for records
#eviction lab
#dat2<-read_csv("newyork_map.csv")
#nycplanning.gov
#dat3<-read_csv("cd_demo_race_economics.csv")
#dat4<-read_csv("ACS2019RENT.csv")
#dat5<-read_csv("ACS2022RENT.csv")
#dat7<-read_csv("newyork_map.csv")
```


```{r}
table(dat$`Community Board`)
1746+2085+3342

dat$`Community Board`<-as.factor(dat$`Community Board`)
dat$`Eviction Postcode`<-factor(dat$`Eviction Postcode`)

dat<-subset(dat,!is.na(`Community Board`))

library(RColorBrewer)

colourCount = length(unique(dat$`Community Board`))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))


table(dat$`Eviction Postcode`)

ggplot(dat,aes(x=`Eviction Postcode`))+geom_bar()

#based on pumas
#data.frame(dat3$cd_name,dat3$pct_hh_rent_burd)

#fix dates
dat<-mutate(dat, `Executed Date`=as.Date(`Executed Date`,format="%m/%d/%Y"))

dat_2023<-subset(dat,`Executed Date` > "2023-01-01" & `Executed Date` <"2023-12-31")
dat_2023<-subset(dat_2023,!is.na(`Community Board`))
table(dat_2023$`Community Board`)


dat_2019<-subset(dat,`Executed Date` > "2019-01-01" & `Executed Date` <"2019-12-31")
dat_2020<-subset(dat,`Executed Date` > "2020-01-01" & `Executed Date` <"2020-12-31")
dat_2021<-subset(dat,`Executed Date` > "2021-01-01" & `Executed Date` <"2021-12-31")
dat_2022<-subset(dat,`Executed Date` > "2022-01-01" & `Executed Date` <"2022-12-31")

table(dat_2019$`Community Board`)

392+323+479+788+709+510+588+209+646+219+355+749 #5967

392/5967
479/5967
788/5967

table(dat_2023$`Community Board`)

ggplot(dat_2019,aes(x=`Community Board`,fill=`Community Board`))+geom_bar(fill=getPalette(colourCount))+ggtitle("Number of Evictions by Community District in 2019")+ylab("Number of Evictions")+xlab("Community District")+theme(plot.title = element_text(hjust = 0.5))

295+171+262+523+533+404+541+173+387+141+264+438 #4132

295/4132
262/4132
523/4132


#dat4_clean<-dat4[-c(1,11),]
#dat5_clean<-dat5[-c(1,11),]
#pie(dat4_clean$`NYC-Bronx Community District 1 & 2--Hunts Point, Longwood & Melrose PUMA; New York!!Estimate`)
#pie(dat5_clean$`NYC-Bronx Community Districts 1 & 2--Melrose, Mott Haven, Longwood, & Hunts Point PUMA; New York!!Estimate`)


table(dat_2022$`Community Board`)

table(dat_2020$`Community Board`)

pie(table(dat_2020$`Community Board`))

brewer.pal(n = 12, name = "Spectral")
```

```{r}
ggplot(dat_2019,aes(x=`Community Board`,fill=`Community Board`))+geom_bar(fill=getPalette(colourCount))+ggtitle("Number of Evictions by Community District in 2019")+ylab("Number of Evictions")+xlab("Community District")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))

ggplot(dat_2020,aes(x=`Community Board`,fill=`Community Board`))+geom_bar(fill=getPalette(colourCount))+ggtitle("Number of Evictions by Community District in 2020")+ylab("Number of Evictions")+xlab("Community District")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))

ggplot(dat_2021, aes(x = `Community Board`, fill = `Community Board`)) + 
  geom_bar() + 
  ggtitle("Number of Evictions by Community District in 2021") + 
  ylab("Number of Evictions") + 
  xlab("Community District") + 
  theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"),legend.position="none") + 
  scale_x_discrete(drop = FALSE) + 
  scale_fill_manual(values = c("#FDAE61","#FEE08B","#FFFFBF","#E6F598","#ABDDA4","#3288BD","#5E4FA2"))

ggplot(dat_2022,aes(x=`Community Board`,fill=`Community Board`))+geom_bar(fill=getPalette(colourCount))+ggtitle("Number of Evictions by Community District in 2022")+ylab("Number of Evictions")+xlab("Community District")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(0,175)

ggplot(dat_2023,aes(x=`Community Board`,fill=`Community Board`))+geom_bar(fill=getPalette(colourCount))+ggtitle("Number of Evictions by Community District in 2023")+ylab("Number of Evictions")+xlab("Community District")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(0,600)
```

```{r}
myPalette<-brewer.pal(12,"Set3")
pie(table(dat_2021$`Community Board`),border="white",col=myPalette)
```

```{r}
#ACS 1 year Rent burden estimates
year<-c(rep("2018",3),rep("2019",3),rep("2020",3),rep("2021",3),rep("2022",3))
community_district<-(rep(c("1 & 2","3 & 6","4"),5))
value_35<-c(51.17,50.56,49.36,49.20,54.53,49.70,49.35,48,49.90,51.16,46.88,49.70,46.4,41.2,45.5)
burden_35<-data.frame(year,community_district,value_35)

value_50<-c(33.01,35.65,34.96,31.29,38.65,36.28,33.89,34.22,34.81,35.26,34.70,30.32,33.99,32.2,35.1)
burden_50<-data.frame(year,community_district,value_50)

ggplot(burden_35,aes(fill=community_district,y=value_35,x=year))+geom_bar(position="dodge",stat="identity")+ylim(0,60)+scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))+xlab("Year")+ylab("Percent of Households")+ggtitle("Households with Rent Burden\nby Community District between 2018 and 2022")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+guides(fill=guide_legend("Community District"))

ggplot(burden_50,aes(fill=community_district,y=value_50,x=year))+geom_bar(position="dodge",stat="identity")+ylim(0,50)+scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))+xlab("Year")+ylab("Percent of Households")+ggtitle("Households with Extreme Rent Burden\nby Community District between 2018 and 2022")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+guides(fill=guide_legend("Community District"))

burden35_12<-filter(burden_35,community_district=="1 & 2")
burden35_36<-filter(burden_35,community_district=="3 & 6")
burden35_4<-filter(burden_35,community_district=="4")


burden50_12<-filter(burden_50,community_district=="1 & 2")
burden50_36<-filter(burden_50,community_district=="3 & 6")
burden50_4<-filter(burden_50,community_district=="4")
ggplot(burden50_4,aes(x=year,y=value_50,group=1))+geom_point()+geom_line()
```

```{r}
#extreme burden line graphs by CD
ggplot(burden50_12,aes(x=year,y=value_50,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(25,45)+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with Extreme Rent Burden\n in Community Districts 1 & 2 between 2018 and 2022")

ggplot(burden50_36,aes(x=year,y=value_50,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(25,45)+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with Extreme Rent Burden\n in Community Districts 3 & 6 between 2018 and 2022")

ggplot(burden50_4,aes(x=year,y=value_50,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(25,45)+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with Extreme Rent Burden\n in Community District 4 between 2018 and 2022")

#burden line graphs
ggplot(burden35_12,aes(x=year,y=value_35,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(40,60)+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with Rent Burden\n in Community Districts 1 & 2 between 2018 and 2022")

ggplot(burden35_36,aes(x=year,y=value_35,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(40,60)+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with Rent Burden\n in Community Districts 3 & 6 between 2018 and 2022")

ggplot(burden35_4,aes(x=year,y=value_35,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(40,60)+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with Rent Burden\n in Community District 4 between 2018 and 2022")
```

```{r}
#tenure data
time<-c("1 year or less","2-4","5-12","13-22","23-32","33 or more")
cd1_2<-c(6.77,16.19,36.81,18.73,14.12,7.37)
cd3_6<-c(12.78,19.72,33.35,18.78,10.97,4.4)
cd4<-c(8.65,21.32,29.62,23.96,9.33,7.12)
tenure<-data.frame(time,cd1_2,cd3_6,cd4)


col<-brewer.pal(6,"Spectral")
pie(tenure$cd4,labels=c("1 yr or less","2-4 yrs","5-12 yrs","13-22 yrs","23-32 yrs","33 yrs or more"),border="white",col=col,main=paste(strwrap("Population of Occupied Housing Units by Time in Community District 4",width=50),collapse="\n"))
pie(tenure$cd3_6,labels=c("1 yr or less","2-4 yrs","5-12 yrs","13-22 yrs","23-32 yrs","33 yrs or more"),border="white",col=col,main=paste(strwrap("Population of Occupied Housing Units by Time in Community Districts 3 & 6",width=50),collapse="\n"))
pie(tenure$cd1_2,labels=c("1 yr or less","2-4 yrs","5-12 yrs","13-22 yrs","23-32 yrs","33 yrs or more"),border="white",col=col,main=paste(strwrap("Population of Occupied Housing Units by Time in Community Districts 1 & 2",width=50),collapse="\n"))
```

```{r}
#occupancy
value_occ<-c(14.02,12.28,18.67,11.01,10.95,20.64,14.40,12.27,18.42,15.29,13.11,20.36,13.47,14.31,19.66)
occupancy<-data.frame(year,community_district,value_occ)

ggplot(occupancy,aes(fill=community_district,y=value_occ,x=year))+geom_bar(position="dodge",stat="identity")+scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))+xlab("Year")+ylab("Percent of Households")+ggtitle("Households with More than 1 Occupant per Room\nby Community District between 2018 and 2022")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+guides(fill=guide_legend("Community District"))

occupancy4<-filter(occupancy,community_district=="4")

ggplot(occupancy4,aes(x=year,y=value_occ,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with More than 1 Occupant per Room\nin Community District 4 between 2018 and 2022")+ylim(15,25)
```

```{r,eval=FALSE}
dat2$id<-factor(dat2$id)
select(dat2,c("10451","10452", "10454", "10455", "10456", "10459", "10460"))

dat2_sub<-subset(dat2, id %in% c("10451","10452", "10454", "10455", "10456", "10459", "10460"))
dat2_sub
#number of evictions since 1/1/2023, last updated 1/1/24
sum(dat2_sub$month_filings)

#eviction lab
dat6<-read_csv("newyork_monthly_2020_2021.csv")


```

```{r}
#data 2 go

yrs<-c(rep("2006-2010",3),rep("2014-2018",3))
cd<-rep(c("1","3","4"),2)
d2g_value50<-c(27.84,31.47,35.76,31.89,32.33,39.17)

d2g50<-data.frame(yrs,cd,d2g_value50)

d2g_value35<-c(52.89,57.77,59.15,60.62,58.1,63.01)

d2g35<-data.frame(yrs,cd,d2g_value35)

ggplot(d2g35,aes(fill=cd,y=d2g_value35,x=yrs))+geom_bar(position="dodge",stat="identity")+ylim(0,70)+scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))+xlab("Year")+ylab("Percent of Households")+ggtitle("Households with Rent Burden\nby Community District: 2006-2010 & 2014-2018")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+guides(fill=guide_legend("Community District"))

ggplot(d2g35,aes(fill=cd,y=d2g_value50,x=yrs))+geom_bar(position="dodge",stat="identity")+ylim(0,50)+scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))+xlab("Year")+ylab("Percent of Households")+ggtitle("Households with Extreme Rent Burden\nby Community District: 2006-2010 & 2014-2018")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+guides(fill=guide_legend("Community District"))
```

#exporting all plots
```{r}
pdf("evictions19.pdf")
ggplot(dat_2019,aes(x=`Community Board`,fill=`Community Board`))+geom_bar(fill=getPalette(colourCount))+ggtitle("Number of Evictions by Community District in 2019")+ylab("Number of Evictions")+xlab("Community District")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))
dev.off()
```

```{r}
pdf("evictions20.pdf")
ggplot(dat_2020,aes(x=`Community Board`,fill=`Community Board`))+geom_bar(fill=getPalette(colourCount))+ggtitle("Number of Evictions by Community District in 2020")+ylab("Number of Evictions")+xlab("Community District")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))
dev.off()
```

```{r}
pdf("evictions21.pdf")
ggplot(dat_2021, aes(x = `Community Board`, fill = `Community Board`)) + 
  geom_bar() + 
  ggtitle("Number of Evictions by Community District in 2021") + 
  ylab("Number of Evictions") + 
  xlab("Community District") + 
  theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"),legend.position="none") + 
  scale_x_discrete(drop = FALSE) + 
  scale_fill_manual(values = c("#FDAE61","#FEE08B","#FFFFBF","#E6F598","#ABDDA4","#3288BD","#5E4FA2"))
dev.off()
```

```{r}
pdf("evictions22.pdf")
ggplot(dat_2022,aes(x=`Community Board`,fill=`Community Board`))+geom_bar(fill=getPalette(colourCount))+ggtitle("Number of Evictions by Community District in 2022")+ylab("Number of Evictions")+xlab("Community District")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(0,175)
dev.off()
```

```{r}
pdf("evictions23.pdf")
ggplot(dat_2023,aes(x=`Community Board`,fill=`Community Board`))+geom_bar(fill=getPalette(colourCount))+ggtitle("Number of Evictions by Community District in 2023")+ylab("Number of Evictions")+xlab("Community District")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(0,600)
dev.off()
```

```{r}
pdf("burden35.pdf")
ggplot(burden_35,aes(fill=community_district,y=value_35,x=year))+geom_bar(position="dodge",stat="identity")+ylim(0,60)+scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))+xlab("Year")+ylab("Percent of Households")+ggtitle("Households with Rent Burden\nby Community District between 2018 and 2022")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+guides(fill=guide_legend("Community District"))
dev.off()
```

```{r}
pdf("burden50.pdf")
ggplot(burden_50,aes(fill=community_district,y=value_50,x=year))+geom_bar(position="dodge",stat="identity")+ylim(0,50)+scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))+xlab("Year")+ylab("Percent of Households")+ggtitle("Households with Extreme Rent Burden\nby Community District between 2018 and 2022")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+guides(fill=guide_legend("Community District"))
dev.off()
```

```{r}
pdf("burden50_12.pdf")
ggplot(burden50_12,aes(x=year,y=value_50,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(25,45)+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with Extreme Rent Burden\n in Community Districts 1 & 2 between 2018 and 2022")
dev.off()
```

```{r}
pdf("burden50_36.pdf")
ggplot(burden50_36,aes(x=year,y=value_50,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(25,45)+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with Extreme Rent Burden\n in Community Districts 3 & 6 between 2018 and 2022")
dev.off()
```

```{r}
pdf("burden50_4.pdf")
ggplot(burden50_4,aes(x=year,y=value_50,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(25,45)+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with Extreme Rent Burden\n in Community District 4 between 2018 and 2022")
dev.off()
```

```{r}
pdf("burden35_12.pdf")
ggplot(burden35_12,aes(x=year,y=value_35,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(40,60)+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with Rent Burden\n in Community Districts 1 & 2 between 2018 and 2022")
dev.off()
```

```{r}
pdf("burden35_36.pdf")
ggplot(burden35_36,aes(x=year,y=value_35,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(40,60)+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with Rent Burden\n in Community Districts 3 & 6 between 2018 and 2022")
dev.off()
```

```{r}
pdf("burden35_4.pdf")
ggplot(burden35_4,aes(x=year,y=value_35,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylim(40,60)+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with Rent Burden\n in Community District 4 between 2018 and 2022")
dev.off()
```

```{r}
pdf("tenure4.pdf")
pie(tenure$cd4,labels=c("1 yr or less","2-4 yrs","5-12 yrs","13-22 yrs","23-32 yrs","33 yrs or more"),border="white",col=col,main=paste(strwrap("Population of Occupied Housing Units by Time in Community District 4",width=50),collapse="\n"))
dev.off()
```

```{r}
pdf("tenure36.pdf")
pie(tenure$cd3_6,labels=c("1 yr or less","2-4 yrs","5-12 yrs","13-22 yrs","23-32 yrs","33 yrs or more"),border="white",col=col,main=paste(strwrap("Population of Occupied Housing Units by Time in Community Districts 3 & 6",width=50),collapse="\n"))
dev.off()
```

```{r}
pdf("tenure12.pdf")
pie(tenure$cd1_2,labels=c("1 yr or less","2-4 yrs","5-12 yrs","13-22 yrs","23-32 yrs","33 yrs or more"),border="white",col=col,main=paste(strwrap("Population of Occupied Housing Units by Time in Community Districts 1 & 2",width=50),collapse="\n"))
dev.off()
```

```{r}
pdf("occupancy.pdf")
ggplot(occupancy,aes(fill=community_district,y=value_occ,x=year))+geom_bar(position="dodge",stat="identity")+scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))+xlab("Year")+ylab("Percent of Households")+ggtitle("Households with More than 1 Occupant per Room\nby Community District between 2018 and 2022")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+guides(fill=guide_legend("Community District"))
dev.off()
```

```{r}
pdf("occupancy4.pdf")
ggplot(occupancy4,aes(x=year,y=value_occ,group=1))+geom_point()+geom_line()+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+ylab("Percent of Households")+xlab("Year")+ggtitle("Households with More than 1 Occupant per Room\nin Community District 4 between 2018 and 2022")+ylim(15,25)
dev.off()
```

```{r}
pdf("d2g_burden35.pdf")
ggplot(d2g35,aes(fill=cd,y=d2g_value35,x=yrs))+geom_bar(position="dodge",stat="identity")+ylim(0,70)+scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))+xlab("Year")+ylab("Percent of Households")+ggtitle("Households with Rent Burden\nby Community District: 2006-2010 & 2014-2018")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+guides(fill=guide_legend("Community District"))
dev.off()
```

```{r}
pdf("d2g_burden50.pdf")
ggplot(d2g35,aes(fill=cd,y=d2g_value50,x=yrs))+geom_bar(position="dodge",stat="identity")+ylim(0,50)+scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))+xlab("Year")+ylab("Percent of Households")+ggtitle("Households with Extreme Rent Burden\nby Community District: 2006-2010 & 2014-2018")+theme(plot.title = element_text(hjust = 0.5),panel.background = element_blank(),axis.line = element_line(color = "black"))+guides(fill=guide_legend("Community District"))
dev.off()
```

```{r}
table(dat_2021$`Community Board`)
(7/22)*100

```