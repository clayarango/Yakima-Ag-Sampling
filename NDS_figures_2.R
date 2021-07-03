library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

#data (created in "all sites" by combining NDS files and water chem file)
nds_chem<-read.csv("NDS_chem_all.csv")
nds_chem$river_mile<-ifelse(nds_chem$stream=="ahtanum", 106, nds_chem$river_mile)
nds_chem$stream<-as.factor(nds_chem$stream)
nds_chem$season<-as.factor(nds_chem$season)
nds_chem$type<-as.factor(nds_chem$type)
nds_chem$Stream<-recode(nds_chem$stream, 'ahtanum'='Ahtanum', 'century'='Century Landing', 'kiona'='Kiona', 'mabton'='Mabton',
                                           'reecer'= 'Reecer', 'ringer'='Ringer', 'roza'='Roza', 'satus'='Satus', 
                                           'toppenish'='Toppenish', 'wenas'='Wenas', 'cleelum'='Cle Elum')

nds_s<-subset(nds_chem, top=="sponge")
nds_s <- subset(nds_s, !(is.na(nds_s$cr.nrr)))

nds_g<-subset(nds_chem, top=="glass")
nds_g <- subset(nds_g, !(is.na(nds_g$chla.nrr)))

nds_s$Stream <- factor(nds_s$Stream, levels = c("Cle Elum", "Reecer", "Ringer", "Roza", "Wenas", "Century Landing",
                                                "Ahtanum", "Toppenish", "Satus", "Mabton", "Kiona"))

nds_g$Stream <- factor(nds_g$Stream, levels = c("Cle Elum", "Reecer", "Ringer", "Roza", "Wenas", "Century Landing",
                                                "Ahtanum", "Toppenish", "Satus", "Mabton", "Kiona"))

nds_g$season<-factor(nds_g$season, levels=c("summer", "fall"))

nds_s$season<-factor(nds_s$season, levels=c("summer", "fall"))

CR_C<-ggplot(subset(nds_s,nutrient=="control"), aes(x=Stream, y=abs(cr.area)))+geom_boxplot(aes(fill=factor(season)))+
  theme_bw(base_size = 12)+ theme(axis.title.x = element_blank(), axis.text.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank())+
  scale_fill_manual(values=c("plum3","goldenrod2"))+labs(y="Respiration (ug O2/cm2/h)")+
  theme(legend.position = "none")+theme( axis.text.x = element_text(angle = 35, vjust=0.65))

Chla_C<-ggplot(subset(nds_g,nutrient=="control"), aes(x=Stream, y=chla))+geom_boxplot(aes(fill=factor(season)))+
  theme_bw(base_size=12)+theme(legend.position = "none")+theme( axis.text.x = element_text(angle = 30, vjust=0.75), axis.title.x = element_blank())+
  scale_fill_manual(values=c("plum3","goldenrod2"))+labs(y="Chlorophyll-a (ug/cm2)")

grid.arrange(CR_C, Chla_C, ncol=1, heights=unit(c(2.5,3.2), c("in","in")))

ddply(subset(nds_s,nutrient=="control"& season=="fall"), "Stream", summarise, cr = abs(mean(cr.area)), 
      se = (sd(abs(cr.area))/sqrt(sum(!is.na(cr.area)))))

ddply(subset(nds_s,nutrient=="control"&season=="summer"), "Stream", summarise, cr = abs(mean(cr.area)), 
      se = (sd(abs(cr.area))/sqrt(sum(!is.na(cr.area)))))

  nds_g$Stream <- factor(nds_g$Stream, levels = c("Cle Elum", "Reecer", "Ringer", "Roza", "Wenas", "Century Landing",
                                                "Ahtanum", "Toppenish", "Satus", "Mabton", "Kiona"))

ddply(subset(nds_g,nutrient=="control"), c("Stream","season"), summarise, chlor = mean(chla), 
      se = (sd(chla))/sqrt(sum(!is.na(chla))))

#NRR figures
#summaries
sum_s<-ddply(nds_s, c("stream", "nutrient", "season", "river_mile", "top", "type", "Si.mgL", "DOC.mgL", "DIN.mgNL", "oP.mgPL"), summarise, cr=mean(cr.area, na.rm=T), 
             cr_nrr=mean(cr.nrr, na.rm=T), se_cr=(sd(cr.area, na.rm=T)/sqrt(sum(!is.na(cr.area)))),
             se_cr_nrr=(sd(cr.nrr, na.rm=T)/sqrt(sum(!is.na(cr.nrr)))))

sum_g<-ddply(nds_g, c("stream", "nutrient", "season", "river_mile", "top","type", "Si.mgL", "DOC.mgL", "DIN.mgNL", "oP.mgPL"), summarise, gpp=mean(gpp.area, na.rm=T),
             chl_a=mean(chla, na.rm=T), chl_a.nrr=mean(chla.nrr, na.rm=T), gpp.nrr=mean(gpp.nrr, na.rm=T), 
             se_gpp=(sd(gpp.area, na.rm=T)/sqrt(sum(!is.na(gpp.area)))), 
             se_chla=(sd(chla, na.rm=T)/sqrt(sum(!is.na(chla)))), 
             se_chla_nrr=(sd(chla.nrr, na.rm=T)/sqrt(sum(!is.na(chla.nrr)))))

#Figures with all nutrient treatments
sum_s$nutrient<-factor(sum_s$nutrient, levels=c("control", "N", "P", "NP","NSi","PSi", "Si","NPSi"))

ggplot(subset(sum_s, !(nutrient=="control")),aes(x=Si.mgL, y=cr_nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_bw()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=cr_nrr-se_cr_nrr, ymax=cr_nrr+se_cr_nrr), width=4)+
  scale_color_manual(values=c("orchid3","goldenrod2"))+geom_hline(yintercept = 1)+theme(legend.position = "none")+
  labs(x="Silica (mg/L)", y="NRR, Respiration")+theme(strip.background =element_rect(fill="white"), strip.text=element_text(hjust=0))

sum_g$nutrient<-factor(sum_g$nutrient, levels=c("control", "N", "P", "NP","NSi","PSi", "Si","NPSi"))

ggplot(subset(sum_g, !(nutrient=="control")),aes(x=river_mile, y=log(chl_a.nrr+1)))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_bw()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=log(chl_a.nrr+1)-log(se_chla_nrr+1), ymax=log(chl_a.nrr+1)+log(se_chla_nrr+1)), width=6)+
  scale_color_manual(values=c("orchid3","goldenrod2"))+geom_hline(yintercept = log(2))+theme(legend.position = "none")+
  labs(x="River Mile", y="Ln (NRR, Chlorophyll-a)")+theme(strip.background =element_rect(fill="white"), strip.text=element_text(hjust=0))

#attempts to make figures showing ME models
nds_ssummer<-subset(nds_s, season=="summer")
nds_sfall<-subset(nds_s, season=="fall")

newdat1<-expand.grid(type=unique(nds_ssummer$type),
                     season=unique(nds_ssummer$season),
                     river_mile=c(min(nds_ssummer$river_mile),
                                  max(nds_ssummer$river_mile)))
newdat2<-expand.grid(type=unique(nds_sfall$type),
                     season=unique(nds_sfall$season),
                     river_mile=c(min(nds_sfall$river_mile),
                                  max(nds_sfall$river_mile)))

ggplot(nds_s, aes(x=river_mile, y=log(cr.nrr+1)))+
  geom_point(aes(shape=factor(type)))+
  geom_line(aes(y=predict(Mf), group=nutrient))+
  geom_line(data=newdat, aes(y=predict(Mf, level=0,newdata=newdat), color=factor(season)))+
  annotate()
theme_bw()

#example:
ggplot(Orthodont, aes(x=age, y=distance, colour=Sex)) +
  geom_point(size=3) +
  geom_line(aes(y=predict(fm2), group=Subject, size="Subjects")) +
  geom_line(data=newdat, aes(y=predict(fm2, level=0, newdata=newdat), size="Population")) +
  scale_size_manual(name="Predictions", values=c("Subjects"=0.5, "Population"=3)) +
  theme_bw(base_size=22) 
      