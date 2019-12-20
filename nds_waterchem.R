#packages
library(nlme)
library(plyr)
library(dplyr)
library(ggplot2)

chem<-read.csv("nds_water_chem.csv")

chem$DIN.mg.l<-chem$NO3_mgNL+chem$NH4_mgNL

chem_sum<-ddply(subset(chem,!(no_DOC_TDN=="*")), c("stream", "season", "type", "position", "river_mile"), 
                summarise, si.mg.l=mean(Si_mgL), op.mg.l=mean(oP_mgL), nh4.mg.l=mean(NH4_mgNL),
                no3.mg.l=mean(NO3_mgNL), doc.mg.l=mean(doc.mg.l), tdn.mg.l=mean(tdn.mg.l),
                DIN.mg.l=mean(DIN.mg.l))

chem_sum$np_ratio<-((chem_sum$nh4.mg.l+chem_sum$no3.mg.l)/14)/(chem_sum$op.mg.l/31)

chem_sum_wide<-reshape(chem_sum, v.names=c("si.mg.l", "op.mg.l", "nh4.mg.l", "no3.mg.l",
                                           "doc.mg.l", "tdn.mg.l", "np_ratio"),
                       timevar="season", idvar=c("stream", "type", "position"), direction="wide")

t.test(chem_sum_wide$np_ratio.fall, chem_sum_wide$np_ratio.summer, paired=T, na.rm=T)
#p = 0.1893, t = -1.4199

t.test(chem_sum_wide$no3.mg.l.fall, chem_sum_wide$no3.mg.l.summer, paired=T, na.rm=T)
#p = 0.32, t= 1.05

t.test(chem_sum_wide$op.mg.l.fall, chem_sum_wide$op.mg.l.summer, paired=T, na.rm=T)
#p = 0.126, t=1.6862

t.test(chem_sum_wide$si.mg.l.fall, chem_sum_wide$si.mg.l.summer, paired=T, na.rm=T)
#t = 4.0428, df = 9, p = 0.002916

t.test(chem_sum_wide$doc.mg.l.fall, chem_sum_wide$doc.mg.l.summer, paired=T, na.rm=T)
#t = 2.2534, df=9, p = 0.0507

t.test(chem_sum_wide$nh4.mg.l.fall, chem_sum_wide$nh4.mg.l.summer, paired=T, na.rm=T)
#t = 1.153, df=9, p = 0.2787

chem_sum_wide_trib<-subset(chem_sum_wide, type=="trib")
t.test(chem_sum_wide_trib$np_ratio.fall, chem_sum_wide_trib$np_ratio.summer, paired =T, na.rm=T)
#t=-0.81551, df=3, p=0.4745

chem_sum_wide_main<-subset(chem_sum_wide, type=="mainstem")
t.test(chem_sum_wide_main$np_ratio.fall, chem_sum_wide_main$np_ratio.summer, paired =T, na.rm=T)
#t=-1.107, df=5, p=0.3187

ggplot(subset(chem_sum, type=="mainstem"), aes(x=position, y=np_ratio))+
  geom_point(aes(color=factor(season)))+  theme_classic()

ggplot(subset(chem_sum, type=="mainstem"), aes(x=position, y=no3.mg.l))+
  geom_point(aes(color=factor(season)))+  theme_classic()

ggplot(subset(chem_sum, type=="mainstem"), aes(x=position, y=op.mg.l))+
  geom_point(aes(color=factor(season)))+  theme_classic()

ggplot(subset(chem_sum, type=="mainstem"), aes(x=position, y=doc.mg.l))+
  geom_point(aes(color=factor(season)))+  theme_classic()

ggplot(subset(chem_sum, type=="mainstem"), aes(x=position, y=tdn.mg.l))+
  geom_point(aes(color=factor(season)))+  theme_classic()

ggplot(subset(chem_sum, type=="mainstem"), aes(x=-river_mile, y=DIN.mg.l))+
  geom_point(aes(color=factor(season)))+  theme_classic()

gnls(DIN.mg.l~a*exp(-river_mile*b), start =list(a=6, b=0.03),data=chem_sum)
#y=2.71*e^-rivermile*0.0127  
  