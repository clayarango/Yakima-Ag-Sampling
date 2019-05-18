#Author:  Sarah Roley
#Date Created: 17-May-2019
#Script to analyze all sites together re NDS deployment with C. Arango and A. Alexiades

#packages
library(nlme)
library(plyr)
library(dplyr)
library(ggplot2)

#Load data
aht.s<-read.csv("aht_summer.csv")
aht.f<-read.csv("aht_fall.csv")
satus.s<-read.csv("satus_summer.csv")
satus.f<-read.csv("satus_fall.csv")
toppenish.s<-read.csv("toppenish_summer.csv")
reecer.s<-read.csv("reec_summer.csv")
reecer.f<-read.csv("reec_fall.csv")
wenas.s<-read.csv("wen_summer.csv")
wenas.f<-read.csv("wen_fall.csv")

#get all files with same variables
str(aht.s)
aht.s<-select(aht.s, -10)
aht.s$site.date<-"Ahtanum_summer_2018"
aht.s$time<-"summer"
str(aht.f)
colnames(aht.f)[colnames(aht.f)=="notes"] <- "Notes"
aht.f$time<-"fall"
str(satus.s)
satus.s$time<-"summer"
str(satus.f)
colnames(satus.f)[colnames(satus.f)=="notes"] <- "Notes"
satus.f$time<-"fall"
toppenish.s$time<-"summer"
str(toppenish.s)
colnames(toppenish.s)[colnames(toppenish.s)=="notes"] <- "Notes"
reecer.s$time<-"summer"
str(reecer.s)
reecer.s<-select(reecer.s, -1, -11, -12)
reecer.s$chla_ug_cm2 <-NA
reecer.s$Notes<-NA
reecer.s$site.date<-"Reecer_summer_2018"
colnames(reecer.s)[colnames(reecer.s)=="chla"] <- "chla_mgL"
reecer.f$time<-"fall"
str(reecer.f)
reecer.f<-select(reecer.f, -10,-11)
reecer.f$Notes<-NA
reecer.f$chla_ug_cm2<-NA
reecer.f$site.date<-"Reecer_fall_2018"
colnames(reecer.f)[colnames(reecer.f)=="chla"] <- "chla_mgL"
wenas.s$time<-"summer"
wenas.s$site.date<-"Wenas_summer_2018"
str(wenas.s)
wenas.s<-select(wenas.s, -1, -11, -12)
wenas.s$Notes<-NA
wenas.s$chla_ug_cm2<-NA
colnames(wenas.s)[colnames(wenas.s)=="chla"] <- "chla_mgL"
wenas.f$time<-"fall"
wenas.f$site.date<-"Wenas_fall_2018"
colnames(wenas.f)[colnames(wenas.f)=="chla"] <- "chla_mgL"
str(wenas.f)
wenas.f<-select(wenas.f, -1, -11, -12)
wenas.f$Notes<-NA
wenas.f$chla_ug_cm2<-NA


#now put them together
tribs<-rbind(aht.s, aht.f)
tribs<-rbind(tribs, reecer.f)
tribs<-rbind(tribs, reecer.s)
tribs<-rbind(tribs, satus.s)
tribs<-rbind(tribs, satus.f)
tribs<-rbind(tribs, toppenish.s)
tribs<-rbind(tribs, wenas.f)
tribs<-rbind(tribs, wenas.s)
names(aht.f)

write.table(tribs, "tribs.csv", sep=",", quote=F, row.names=F)

tribs<-read.csv("tribs.csv")

tribs$site<-factor(tribs$site, levels=c("Reecer", "Wenas", "Ahtanum", "Toppenish", "Satus"))
tribs$season<-factor(tribs$season, levels=c("summer", "fall"))

ggplot(subset(tribs, nutrient=="C"), aes(x=site, y = cr.area))+geom_boxplot() +theme_classic()+
  facet_wrap(~season)+scale_y_continuous(limits=c(-20, 0))

ggplot(subset(tribs, nutrient=="C"), aes(x=site, y = gpp.area))+geom_boxplot() +theme_classic()+
  facet_wrap(~season)+scale_y_continuous(limits=c(0,10))

ggplot(subset(tribs, nutrient=="C"), aes(x=site, y = chla_mgL))+geom_boxplot() +theme_classic()+
  facet_wrap(~season)
