#Author:  Sarah Roley
#Date Created: 17-May-2019, updated 23-March-2021
#Script to analyze all sites together re NDS deployment with C. Arango and A. Alexiades

#packages
library(nlme)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)

#load NRR data and combine

mab_fall<-read.csv("NRR files/mab_fall_nrr.csv")
mab_fall<-mab_fall%>%rename(chla=chla_ug_cm2)
mab_fall$chla_mgL<-NULL
mab_fall$notes<-NULL
cen_fall<-read.csv("NRR files/cen_lan_fall_nrr.csv")
unique(cen_fall$nutrient)
cen_fall$notes<-NULL
cen_fall$chla_mgL<-NULL
cen_fall<-cen_fall%>%rename(chla=chla_ug_cm2)
cen_summ<-read.csv("NRR files/cen_lan_summer_nrr.csv")
cen_summ$chla_mgL<-NULL
cen_summ<-cen_summ%>%rename(chla=chla_ug_cm2)
unique(cen_summ$nutrient)
cen_summ$Notes<-NULL
cle_summ<-read.csv("NRR files/cle_summer_nrr.csv")
unique(cle_summ$nutrient)
cle_summ$nutrient<-recode(cle_summ$nutrient, "N+P"= "NP", "P+Si"="PSi", "N+P+Si"="NPSi", "N+Si"="NSi", "C"="control")
cle_summ$id<-NULL
cle_fall<-read.csv("NRR files/cle_fall_nrr.csv")
cle_fall$nutrient<-recode(cle_fall$nutrient, "N+P"= "NP", "P+Si"="PSi", "N+P+Si"="NPSi", "N+Si"="NSi", "C"="control")
cle_fall$id<-NULL
kiona_fall<-read.csv("NRR files/kiona_fall_nrr.csv")
unique(kiona_fall$nutrient)
kiona_fall$id<-NULL
kiona_summ<-read.csv("NRR files/kiona_summer_nrr.csv")
unique(kiona_summ$nutrient)
kiona_summ$Notes<-NULL
mab_summ<-read.csv("NRR files/mab_summer_nrr.csv")
mab_summ$notes<-NULL
mab_summ$chla_mgL<-NULL
mab_summ<-mab_summ%>%rename(chla=chla_ug_cm2)
unique(mab_summ$nutrient)
ring_fall<-read.csv("NRR files/ring_fall_nrr.csv")       
ring_fall$nutrient<-recode(ring_fall$nutrient, "N+P"= "NP", "P+Si"="PSi", "N+P+Si"="NPSi", "N+Si"="NSi", "C"="control")
ring_fall$id<-NULL
ring_summ<-read.csv("NRR files/ring_summer_nrr.csv")
ring_summ$nutrient<-recode(ring_summ$nutrient, "N+P"= "NP", "P+Si"="PSi", "N+P+Si"="NPSi", "N+Si"="NSi", "C"="control")
ring_summ$id<-NULL
roza_fall<-read.csv("NRR files/roza_fall_nrr.csv")
unique(roza_fall$nutrient)
roza_fall$nutrient<-recode(roza_fall$nutrient, "N+P"= "NP", "P+Si"="PSi", "N+P+Si"="NPSi", "N+Si"="NSi", "C"="control")
roza_fall$id<-NULL
roza_summ<-read.csv("NRR files/roza_summer_nrr.csv")
roza_summ$nutrient<-recode(roza_summ$nutrient, "N+P"= "NP", "P+Si"="PSi", "N+P+Si"="NPSi", "N+Si"="NSi", "C"="control")
roza_summ$id<-NULL
topp<-read.csv("NRR files/toppenish_summer_nrr.csv")
topp$notes<-NULL
topp<-topp%>%rename(chla=chla_ug_cm2)
topp$chla_mgL<-NULL
wenas_summ<-read.csv("NRR files/wenas_summer_nrr.csv")
wenas_summ$id<-NULL
#need to remove G8 for chla and chla.nrr; remove C8 for gpp and gpp.nrr
wenas_summ$gpp.area<-ifelse(wenas_summ$nds.id=="C8", NA, wenas_summ$gpp.area)
wenas_summ$gpp.nrr<-ifelse(wenas_summ$nds.id=="C8", NA, wenas_summ$gpp.nrr)
wenas_summ$gpp.es<-ifelse(wenas_summ$nds.id=="C8", NA, wenas_summ$gpp.es)
wenas_summ$chla<-ifelse(wenas_summ$nds.id=="G8", NA, wenas_summ$chla)
wenas_summ$chla.nrr<-ifelse(wenas_summ$nds.id=="G8", NA, wenas_summ$chla.nrr)
wenas_summ$chla.es<-ifelse(wenas_summ$nds.id=="G8", NA, wenas_summ$chla.es)

wenas_fall<-read.csv("NRR files/wenas_fall_nrr.csv")
wenas_fall<-wenas_fall%>%rename(site.date=site_date)
wenas_fall$id<-NULL
satus_fall<-read.csv("NRR files/satus_fall_nrr.csv")
satus_fall<-satus_fall%>%rename(chla=chla_ug_cm2)
satus_fall$chla_mgL<-NULL
satus_fall$notes<-NULL
satus_summ<-read.csv("NRR files/satus_summer_nrr.csv")
satus_summ<-satus_summ%>%rename(chla=chla_ug_cm2)
satus_summ$chla_mgL<-NULL
satus_summ$Notes<-NULL
aht_fall<-read.csv("NRR files/aht_fall_nrr.csv")
aht_fall$notes<-NULL
#need to remove B5 for chla, chla.nrr, and chla.es
aht_fall$chla<-ifelse(aht_fall$nds.id=="B5", NA, aht_fall$chla)
aht_fall$chla.nrr<-ifelse(aht_fall$nds.id=="B5", NA, aht_fall$chla.nrr)
aht_fall$chla.es<-ifelse(aht_fall$nds.id=="B5", NA, aht_fall$chla.es)

aht_summ<-read.csv("NRR files/aht_summer_nrr.csv")
aht_summ$chla_mgL<-NULL
aht_summ$chla_mg<-NULL
aht_summ<-aht_summ%>%rename(chla=chla_ug_cm2)
aht_summ$Notes<-NULL
#need to address outliers
aht_summ$chla<-ifelse(aht_summ$nds.id=="F6", NA, aht_summ$chla)
aht_summ$chla.nrr<-ifelse(aht_summ$nds.id=="F6", NA, aht_summ$chla.nrr)
aht_summ$chla.es<-ifelse(aht_summ$nds.id=="F6", NA, aht_summ$chla.es)

reec_summ<-read.csv("NRR files/reec_summer_nrr.csv")
reec_summ$id<-NULL
reec_summ<-reec_summ%>%rename(site.date=site_date)
reec_fall<-read.csv("NRR files/reec_fall_nrr.csv")
reec_fall$id<-NULL
reec_fall<-reec_fall%>%rename(site.date=site_date)

nds_all<-rbind(aht_fall, aht_summ, cen_fall, cen_summ, cle_summ, cle_fall, kiona_fall, kiona_summ,
               mab_fall, mab_summ, reec_fall, reec_summ, ring_fall, ring_summ, roza_fall, roza_summ,
               satus_fall, satus_summ, topp, wenas_summ, wenas_fall)

nds_all$site.date<-recode(nds_all$site.date, "cen_lan_summer" = "cen_summer", "cen_lan_fall" = "cen_fall")
nds_all$nutrient<-recode(nds_all$nutrient, "C" = "control")

nds_all<-nds_all%>%separate(site.date, c("stream", "season"))
unique(nds_all$stream)
unique(nds_all$season)

#load water chem data
chem<-read.csv("nds_water_chem.csv")
unique(chem$stream)
unique(chem$season)
nds_all$stream<-recode(nds_all$stream, "mab"="mabton", "aht"="ahtanum", "cen"="century", "cle"="cleelum",
                       "reec"="reecer", "ring"="ringer")
str(nds_all)
str(chem)

#remove extraneous columns prior to merge
chem$Site<-NULL
chem$no_DOC_TDN<-NULL
#take seasonal average (sample taken at deployment and at retrieval)
chem_sum<-ddply(chem, c("stream", "season", "type", "position", "river_mile"),summarise, Si.mgL=mean(Si_mgL),
                oP.mgPL=mean(oP_mgL), NH4.mgNL=mean(NH4_mgNL), NO3.mgNL=mean(NO3_mgNL), DOC.mgL=mean(doc.mg.l, na.rm=T),
                TDN.mgL=mean(tdn.mg.l, na.rm=T)) 

write.table(chem_sum, "chem_summary.csv", sep=",", quote=F, row.names=F)

nds_chem<-merge(nds_all, chem_sum, by=c("stream", "season"), all=T)

#additional useful chem metrics
nds_chem$DIN.mgNL<-nds_chem$NO3.mgNL+nds_chem$NH4.mgNL
nds_chem$N.P.ratio<-(nds_chem$DIN.mgNL/14)/(nds_chem$oP.mgPL/31)
nds_chem$N.Si.ratio<-(nds_chem$DIN.mgNL/14)/(nds_chem$Si.mgL/28)
nds_chem$P.Si.ratio<-(nds_chem$oP.mgPL/31)/(nds_chem$Si.mgL/28)

write.table(nds_chem, "NDS_chem_all.csv", sep = ",", quote=F, row.names=F)

unique(nds_chem$nutrient)

  
  
###OLD CODE##########
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

ggplot(subset(tribs, nutrient=="C"), aes(x=site, y = chla_ug_cm2))+geom_boxplot() +theme_classic()+
  facet_wrap(~season)
