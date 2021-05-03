library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)

#data (created in "all sites" by combining NDS files and water chem file)
nds_chem<-read.csv("NDS_chem_all.csv")

nds_chem$river_mile<-ifelse(nds_chem$stream=="ahtanum", 106, nds_chem$river_mile)

N<-subset(nds_chem, N==1)
sponge<-subset(nds_chem, top=="sponge")
glass<-subset(nds_chem, top =="glass")

#summaries
sum_s<-ddply(sponge, c("stream", "nutrient", "season", "river_mile", "top", "type"), summarise, cr=mean(cr.area, na.rm=T), 
               cr_nrr=mean(cr.nrr, na.rm=T), se_cr=(sd(cr.area, na.rm=T)/sqrt(sum(!is.na(cr.area)))),
               se_cr_nrr=(sd(cr.nrr, na.rm=T)/sqrt(sum(!is.na(cr.nrr)))))

sum_g<-ddply(glass, c("stream", "nutrient", "season", "river_mile", "top","type"), summarise, gpp=mean(gpp.area, na.rm=T),
               chl_a=mean(chla, na.rm=T), chl_a.nrr=mean(chla.nrr, na.rm=T), gpp.nrr=mean(gpp.nrr, na.rm=T), 
               se_gpp=(sd(gpp.area, na.rm=T)/sqrt(sum(!is.na(gpp.area)))), 
               se_chla=(sd(chla, na.rm=T)/sqrt(sum(!is.na(chla)))), 
               se_chla_nrr=(sd(chla.nrr, na.rm=T)/sqrt(sum(!is.na(chla.nrr)))))

#Figures with river mile
ggplot(subset(sum_s, !(nutrient=="control")),aes(x=river_mile, y=cr_nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=cr_nrr-se_cr_nrr, ymax=cr_nrr+se_cr_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)

ggplot(subset(sum_g, !(nutrient=="control")),aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)

  