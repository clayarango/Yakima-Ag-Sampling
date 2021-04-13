####Script created 29 March 2021 to analyze relationships between NRR and water chem
## S Roley

#packages
library(nlme)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)

#data (created in "all sites" by combining NDS files and water chem file)
nds_chem<-read.csv("NDS_chem_all.csv")

nds_chem$river_mile<-ifelse(nds_chem$stream=="ahtanum", 106, nds_chem$river_mile)

N<-subset(nds_chem, N==1)
N_sponge<-subset(N, top=="sponge")
N_glass<-subset(N, top =="glass")

N_sum_s<-ddply(N_sponge, c("stream", "nutrient", "season", "river_mile", "top", "type"), summarise, cr=mean(cr.area, na.rm=T), 
               cr_nrr=mean(cr.nrr, na.rm=T), se_cr=(sd(cr.area, na.rm=T)/sqrt(sum(!is.na(cr.area)))),
              se_cr_nrr=(sd(cr.nrr, na.rm=T)/sqrt(sum(!is.na(cr.nrr)))))

N_sum_g<-ddply(N_glass, c("stream", "nutrient", "season", "river_mile", "top","type"), summarise, gpp=mean(gpp.area, na.rm=T),
               chl_a=mean(chla, na.rm=T), chl_a.nrr=mean(chla.nrr, na.rm=T), gpp.nrr=mean(gpp.nrr, na.rm=T), 
              se_gpp=(sd(gpp.area, na.rm=T)/sqrt(sum(!is.na(gpp.area)))), 
               se_chla=(sd(chla, na.rm=T)/sqrt(sum(!is.na(chla)))), 
               se_chla_nrr=(sd(chla.nrr, na.rm=T)/sqrt(sum(!is.na(chla.nrr)))))

P<-subset(nds_chem, P==1)
P_sponge<-subset(P, top=="sponge")
P_glass<-subset(P, top =="glass")

P_sum_s<-ddply(P_sponge, c("stream", "nutrient", "season", "river_mile", "top","type"), summarise, cr=mean(cr.area, na.rm=T), 
               cr_nrr=mean(cr.nrr, na.rm=T), se_cr=(sd(cr.area, na.rm=T)/sqrt(sum(!is.na(cr.area)))),
               se_cr_nrr=(sd(cr.nrr, na.rm=T)/sqrt(sum(!is.na(cr.nrr)))))

P_sum_g<-ddply(P_glass, c("stream", "nutrient", "season", "river_mile", "top","type"), summarise, gpp=mean(gpp.area, na.rm=T),
               chl_a=mean(chla, na.rm=T), chl_a.nrr=mean(chla.nrr, na.rm=T), gpp.nrr=mean(gpp.nrr, na.rm=T), 
               se_gpp=(sd(gpp.area, na.rm=T)/sqrt(sum(!is.na(gpp.area)))), 
               se_chla=(sd(chla, na.rm=T)/sqrt(sum(!is.na(chla)))), 
               se_chla_nrr=(sd(chla.nrr, na.rm=T)/sqrt(sum(!is.na(chla.nrr)))))

Si<-subset(nds_chem, Si==1)
Si_sponge<-subset(Si, top=="sponge")
Si_glass<-subset(Si, top =="glass")

Si_sum_s<-ddply(Si_sponge, c("stream", "nutrient", "season", "river_mile", "top","type"), summarise, cr=mean(cr.area, na.rm=T), 
               cr_nrr=mean(cr.nrr, na.rm=T), se_cr=(sd(cr.area, na.rm=T)/sqrt(sum(!is.na(cr.area)))),
               se_cr_nrr=(sd(cr.nrr, na.rm=T)/sqrt(sum(!is.na(cr.nrr)))))

Si_sum_g<-ddply(Si_glass, c("stream", "nutrient", "season", "river_mile", "top","type"), summarise, gpp=mean(gpp.area, na.rm=T),
               chl_a=mean(chla, na.rm=T), chl_a.nrr=mean(chla.nrr, na.rm=T), gpp.nrr=mean(gpp.nrr, na.rm=T), 
               se_gpp=(sd(gpp.area, na.rm=T)/sqrt(sum(!is.na(gpp.area)))), 
               se_chla=(sd(chla, na.rm=T)/sqrt(sum(!is.na(chla)))), 
               se_chla_nrr=(sd(chla.nrr, na.rm=T)/sqrt(sum(!is.na(chla.nrr)))))

########
#N limitation
#######
#how does N limitation differ among seasons and type? Expectation: N limitation more common in upper river and in summer.
#also N limitation more common in fall in tribs and summer in mainstem, esp for chla
#conclusion: Mainstem CR limited by N in fall, Tributaries N limited in summer

ggplot(N, aes(x=season, y=cr.nrr))+geom_boxplot(aes(color=factor(type)))+theme_classic()

t.test(cr.nrr~type, data=subset(N, season=="fall"))
#t = 3.2253, df = 184.74, p-value = 0.001488
#95 percent confidence interval: 0.07564847 0.31397925
#  mean in group mainstem     mean in group trib 
#0.9884479              0.7936341  

t.test(cr.nrr~type, data=subset(N, season=="summer"))
#t = 1.8336, df = 167.37, p-value = 0.06849
#95 percent confidence interval: -0.01124728  0.30445623
#  mean in group mainstem     mean in group trib 
#1.319428               1.172824 


ggplot(N, aes(x=season, y=chla.nrr))+geom_boxplot(aes(color=factor(type)))+theme_classic()+
  scale_y_continuous(limits=c(0,20))

t.test(chla.nrr~type, data=subset(N, season=="fall"))
#t = 1.3332, df = 127.58, p-value = 0.1848
#95 percent confidence interval: -0.05369536  0.27549457
#mean in group mainstem     mean in group trib 
# 0.8256694              0.7147698 

t.test(chla.nrr~type, data=subset(N, season=="summer"))
#t = -4.1246, df = 102.98, p-value = 7.542e-05
#95 percent confidence interval: -5.595440 -1.961683
#  mean in group mainstem     mean in group trib 
#1.836982               5.615544

#How does river position affect N limitation? Hypothesis: lower in WS (lower river mile) = less N limitation
ggplot(N_sum_s, aes(x=river_mile, y=cr_nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=cr_nrr-se_cr_nrr, ymax=cr_nrr+se_cr_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)
#summer, higher river mile = bigger response in mainstem (more upstream). Minimal response in tribs, except for NPSi
#fall: bigger response with river mile for NPSi and maybe N
#NSi no change with river mile, inhibition

ggplot(N_sum_g, aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)
#wenas dwarfs all others - scale so that patterns at other sites visible. Reecer also high for NSi

ggplot(N_sum_g, aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)+scale_y_continuous(limits=c(0,5))
#fall: no response and no pattern with river mile - all waver around 1
#summer: really strong response in some tribs (wenas) and also some mainstem (Cle Elum)
#aside from those, no real patterns with river mile. Instead, scattered responses to N addition.

ggplot(N, aes(x=position, y=chla.nrr))+geom_point(aes(color=factor(type)))+theme_classic()+
  facet_wrap(~season)
#results at toppenish dwarfing all else and hard to see patterns
ggplot(N, aes(x=position, y=chla.nrr))+geom_point(aes(color=factor(type)))+theme_classic()+
  facet_wrap(~season)+scale_y_continuous(limits=c(0,20))
#no real patterns by river mile

trib<-subset(N, type=="trib")
unique(trib$stream)
unique(trib$position)
unique(trib$river_mile)
main<-subset(N, type=="mainstem")
unique(main$river_mile)

#####
#P limitation
######
ggplot(P, aes(x=season, y=cr.nrr))+geom_boxplot(aes(color=factor(type)))+theme_classic()
#In fall, tribs inhibited by P; mainstem neutral. In summer, no difference.

t.test(cr.nrr~type, data=subset(P, season=="fall"))
#t = 4.2868, df = 178.33, p-value = 2.962e-05
#95 percent confidence interval:0.1070222 0.2895987
#  mean in group mainstem     mean in group trib 
#1.0442496              0.8459391 

t.test(cr.nrr~type, data=subset(P, season=="summer"))
#t = 1.0391, df = 175.55, p-value = 0.3002
#95 percent confidence interval:  -0.07066581  0.22781283
#  mean in group mainstem     mean in group trib 
#1.206963               1.128389 

t.test(cr.nrr~season, data=subset(P, type=="mainstem"))
#t = -2.1914, df = 177.94, p-value = 0.02972
#95 percent confidence interval:  -0.30923885 -0.01618744
#  mean in group fall mean in group summer 
#1.044250             1.206963 

t.test(cr.nrr~season, data=subset(P, type=="trib"))
#t = -5.8326, df = 133.73, p-value = 3.899e-08
#95 percent confidence interval:  -0.3782296 -0.1866705
#  mean in group fall mean in group summer 
#0.8459391            1.1283892

P_sum_s$nutrient<-factor(P_sum_s$nutrient,levels= c("P", "NP", "NPSi", "PSi"))

ggplot(P_sum_s, aes(x=river_mile, y=cr_nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=cr_nrr-se_cr_nrr, ymax=cr_nrr+se_cr_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)
#only a response when N present
#only a response in summer, except Cle Elum
#in summer, general increase in response with river mile for NP and NPSi, but some variation

P_sum_g$nutrient<-factor(P_sum_g$nutrient,levels= c("P", "NP", "NPSi", "PSi"))
ggplot(P_sum_g, aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)
#high value from Wenas obscures others

ggplot(P_sum_g, aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)+scale_y_continuous(limits=c(0,10))
#with the exception of Wenas, mostly limitation occurs when N present.
#limitation almost always occurs in summer.

######
#Si
######

Si_sum_s$nutrient<-factor(Si_sum_s$nutrient,levels= c("Si", "NSi", "NPSi", "PSi"))

ggplot(Si_sum_s, aes(x=river_mile, y=cr_nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=cr_nrr-se_cr_nrr, ymax=cr_nrr+se_cr_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)
#neutral or inhibition, no patterns in mainstem vs. tribs or with location, except NPSi

Si_sum_g$nutrient<-factor(Si_sum_g$nutrient,levels= c("Si", "NSi", "NPSi", "PSi"))

ggplot(Si_sum_g, aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)
#high values from Wenas, Reecer, Cle Elum
#limitation only in summer
#no apparent pattern with geography

ggplot(Si_sum_g, aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)+scale_y_continuous(limits=c(0,5))

####################
#water chem figures
####################
library(gridExtra)

NO3<-ggplot(nds_chem, aes(x=river_mile, y = NO3.mgNL))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("NO3-")

SRP<-ggplot(nds_chem, aes(x=river_mile, y = oP.mgPL))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("SRP")

Si<-ggplot(nds_chem, aes(x=river_mile, y = Si.mgL))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("Si")

NH4<-ggplot(nds_chem, aes(x=river_mile, y = NH4.mgNL))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("NH4")

DOC<-ggplot(nds_chem, aes(x=river_mile, y = DOC.mgL))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("DOC")

TDN<-ggplot(nds_chem, aes(x=river_mile, y = TDN.mgL))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("TDN")

grid.arrange(NO3, SRP, Si, NH4, DOC, TDN, ncol=3)

NP<-ggplot(nds_chem, aes(x=river_mile, y = N.P.ratio))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("N:P")+geom_hline(yintercept = 16)

NSi<-ggplot(nds_chem, aes(x=river_mile, y = N.Si.ratio))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("N:Si")

PSi<-ggplot(nds_chem, aes(x=river_mile, y = P.Si.ratio))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("P:Si")

grid.arrange(NP, NSi, PSi, ncol=3)

###########
#water chem stats
#########
chem$river_mile<-ifelse(chem$stream=="ahtanum", 106, chem$river_mile)

#How does water chem compare summer to fall?
chem<-read.csv("chem_summary.csv")
chem$DIN.mgNL<-chem$NH4.mgNL+chem$NO3.mgNL
chem$N.P.ratio<-(chem$DIN.mgNL/14)/(chem$oP.mgPL/31)
chem$N.Si.ratio<-(chem$DIN.mgNL/14)/(chem$Si.mgL/28)
chem$P.Si.ratio<-(chem$oP.mgPL/31)/(chem$Si.mgL/28)

#paired t-tests. first need to shift to wide
chem_wide<-reshape(chem, timevar="season", v.names=c("Si.mgL", "oP.mgPL", "NH4.mgNL", "NO3.mgNL", "DOC.mgL", "TDN.mgL",
                                                     "DIN.mgNL", "N.P.ratio", "N.Si.ratio", "P.Si.ratio"), 
                   idvar=c("stream", "type", "river_mile"), direction="wide", drop="position")
#drop toppenish b/c no pair
chem_wide<-subset(chem_wide, !(stream=="toppenish"))

#all data combined
t.test(chem_wide$Si.mgL.fall, chem_wide$Si.mgL.summer, paired=T)#t = 4.0888, df = 9, p-value = 0.002722
t.test(chem_wide$oP.mgPL.fall, chem_wide$oP.mgPL.summer, paired = T)#t = 1.6701, df = 9, p-value = 0.1292
t.test(chem_wide$NH4.mgNL.fall, chem_wide$NH4.mgNL.summer, paired = T)#t = -0.93564, df = 9, p-value = 0.3739
t.test(chem_wide$NO3.mgNL.fall, chem_wide$NO3.mgNL.summer, paired = T)#t = 0.9758, df = 9, p-value = 0.3547
t.test(chem_wide$DIN.mgNL.fall, chem_wide$DIN.mgNL.summer, paired = T)#t = 0.38253, df = 9, p-value = 0.7109
t.test(chem_wide$TDN.mgL.fall, chem_wide$TDN.mgL.summer, paired = T)#t = -1.4657, df = 9, p-value = 0.1768
t.test(chem_wide$DOC.mgL.fall, chem_wide$DOC.mgL.summer, paired = T)#t = 2.2534, df = 9, p-value = 0.05072
t.test(chem_wide$N.P.ratio.fall, chem_wide$N.P.ratio.summer, paired = T)#t = -1.6554, df = 9, p-value = 0.1322
t.test(chem_wide$N.Si.ratio.fall, chem_wide$N.Si.ratio.summer, paired = T)#t = -0.24664, df = 9, p-value = 0.8107
t.test(chem_wide$P.Si.ratio.fall, chem_wide$P.Si.ratio.summer, paired = T)#t = 1.9864, df = 9, p-value = 0.078252

#So: When all combined, only Si has a consistent seasonal pattern of more Si in fall.

#mainstem only
chem_m<-subset(chem_wide, type=="mainstem")

t.test(chem_m$Si.mgL.fall, chem_m$Si.mgL.summer, paired = T) #t = 6.3335, df = 5, p-value = 0.001447
t.test(chem_m$oP.mgPL.fall, chem_m$oP.mgPL.summer, paired = T)#t = 5.602, df = 5, p-value = 0.002504
t.test(chem_m$NH4.mgNL.fall, chem_m$NH4.mgNL.summer, paired = T)#t = 0.29006, df = 5, p-value = 0.7834
t.test(chem_m$NO3.mgNL.fall, chem_m$NO3.mgNL.summer, paired = T)#t = 3.2675, df = 5, p-value = 0.02226
t.test(chem_m$DIN.mgNL.fall, chem_m$DIN.mgNL.summer, paired=T)#t = 2.3745, df = 5, p-value = 0.0636
t.test(chem_m$TDN.mgL.fall, chem_m$TDN.mgL.summer, paired = T)#t = -0.68538, df = 5, p-value = 0.5236
t.test(chem_m$DOC.mgL.fall, chem_m$DOC.mgL.summer, paired = T)#t = 2.1604, df = 5, p-value = 0.08314
t.test(chem_m$N.P.ratio.fall, chem_m$N.P.ratio.summer, paired = T)#t = -1.36, df = 5, p-value = 0.2319
t.test(chem_m$N.Si.ratio.fall, chem_m$N.Si.ratio.summer, paired = T)#t = 1.3671, df = 5, p-value = 0.2299
t.test(chem_m$P.Si.ratio.fall, chem_m$P.Si.ratio.summer, paired = T)#t = 3.4637, df = 5, p-value = 0.01797

#So: in the mainstem, Si, NO3, and SRP decrease summer-fall. P decreases more than Si - P:Si decreases summer-fall

#tribs only
chem_t<-subset(chem_wide, type=="trib")
t.test(chem_t$Si.mgL.fall, chem_t$Si.mgL.summer, paired = T) #t = 1.466, df = 3, p-value = 0.2389
t.test(chem_t$oP.mgPL.fall, chem_t$oP.mgPL.summer, paired = T)#t = 0.41106, df = 3, p-value = 0.7086
t.test(chem_t$NH4.mgNL.fall, chem_t$NH4.mgNL.summer, paired = T)#t = -1.8817, df = 3, p-value = 0.1564
t.test(chem_t$NO3.mgNL.fall, chem_t$NO3.mgNL.summer, paired = T)# = -0.59059, df = 3, p-value = 0.5963
t.test(chem_t$DIN.mgNL.fall, chem_t$DIN.mgNL.summer, paired=T)#t = -1.042, df = 3, p-value = 0.374
t.test(chem_t$TDN.mgL.fall, chem_t$TDN.mgL.summer, paired = T)#t = -1.6188, df = 3, p-value = 0.2039
t.test(chem_t$DOC.mgL.fall, chem_t$DOC.mgL.summer, paired = T)#t = 1.139, df = 3, p-value = 0.3374
t.test(chem_t$N.P.ratio.fall, chem_t$N.P.ratio.summer, paired = T)#t = -0.81551, df = 3, p-value = 0.4745
t.test(chem_t$N.Si.ratio.fall, chem_t$N.Si.ratio.summer, paired = T)#t = -1.1372, df = 3, p-value = 0.3381
t.test(chem_t$P.Si.ratio.fall, chem_t$P.Si.ratio.summer, paired = T)#t = 0.10447, df = 3, p-value = 0.9234

#no consistent relationships across tributaries, which indicates different mechanisms at each tributary site

#effect of river mile
library(mgcv)

#different values in summer vs fall so makes sense to have separate equations. but what if all in one?
nit1<-lm(NO3.mgNL~river_mile+type + season, data=chem) 
summary(nit1)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1.654855   0.269214   6.147 1.07e-05 ***
#  river_mile   -0.010260   0.002060  -4.982 0.000114 ***
#  typetrib      0.017424   0.179390   0.097 0.923762    
#seasonsummer  0.009767   0.177784   0.055 0.956830  

#Residual standard error: 0.4061 on 17 degrees of freedom
#Multiple R-squared:  0.5938,	Adjusted R-squared:  0.5221 
#F-statistic: 8.284 on 3 and 17 DF,  p-value: 0.001293

nit2<-lm(NO3.mgNL~river_mile, data=chem)
summary(nit2)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.667390   0.228204   7.307 6.27e-07 ***
#  river_mile  -0.010260   0.001948  -5.267 4.40e-05 ***

#Residual standard error: 0.3843 on 19 degrees of freedom
#Multiple R-squared:  0.5935,	Adjusted R-squared:  0.5721 
#F-statistic: 27.74 on 1 and 19 DF,  p-value: 4.395e-05

nit3<-lm(NO3.mgNL~river_mile, data=subset(chem, type=="mainstem"))
summary(nit3)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.67014    0.17117   9.757 1.99e-06 ***
#  river_mile  -0.01036    0.00143  -7.242 2.78e-05 ***
#Residual standard error: 0.2522 on 10 degrees of freedom
#Multiple R-squared:  0.8399,	Adjusted R-squared:  0.8239 
#F-statistic: 52.45 on 1 and 10 DF,  p-value: 2.783e-05

nit4<-lm(NO3.mgNL~river_mile+season, data=subset(chem, type=="mainstem")) #BEST MODEL for mainstem NO3-#
summary(nit4)
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1.800755   0.161481  11.151 1.43e-06 ***
#  river_mile   -0.010356   0.001241  -8.343 1.58e-05 ***
#  seasonsummer -0.261236   0.126427  -2.066   0.0688 .  

#Residual standard error: 0.219 on 9 degrees of freedom
#Multiple R-squared:  0.8914,	Adjusted R-squared:  0.8673 
#F-statistic: 36.94 on 2 and 9 DF,  p-value: 4.584e-05

nit5<-lm(NO3.mgNL~river_mile, data=subset(chem, type=="mainstem"| season=="summer"))
summary(nit5)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.818081   0.235173   7.731 1.31e-06 ***
#  river_mile  -0.011222   0.002005  -5.596 5.10e-05 ***
#Residual standard error: 0.3766 on 15 degrees of freedom
#Multiple R-squared:  0.6761,	Adjusted R-squared:  0.6545 
#F-statistic: 31.31 on 1 and 15 DF,  p-value: 5.104e-05

nit6<-lm(NO3.mgNL~river_mile, data=subset(chem, type=="mainstem"| season=="fall"))
summary(nit6)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.510794   0.184218   8.201 1.02e-06 ***
#  river_mile  -0.009335   0.001549  -6.027 3.10e-05 ***

#Residual standard error: 0.2887 on 14 degrees of freedom
#Multiple R-squared:  0.7218,	Adjusted R-squared:  0.702 
#F-statistic: 36.33 on 1 and 14 DF,  p-value: 3.105e-05
AIC(nit6) #9.5
AIC(nit5) #18.92
AIC(nit4)#2.15
AIC(nit3)#4.8
AIC(nit2)#23.3
AIC(nit1)#27.3

#So: According to AIC and r2, the best model for mainstem NO3- is river mile and season

nit7<-lm(NO3.mgNL~river_mile, data=subset(chem, type=="trib"))
summary(nit7)
#Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  1.637268   0.716290   2.286   0.0562 .
#river_mile  -0.009892   0.006302  -1.570   0.1605 
#Residual standard error: 0.5564 on 7 degrees of freedom
#Multiple R-squared:  0.2603,	Adjusted R-squared:  0.1546 
#F-statistic: 2.463 on 1 and 7 DF,  p-value: 0.1605

nit8<-lm(NO3.mgNL~river_mile+season, data=subset(chem, type=="trib"))
summary(nit8)
#Estimate Std. Error t value Pr(>|t|)
#(Intercept)   1.362016   0.763788   1.783    0.125
#river_mile   -0.009311   0.006311  -1.475    0.191
#seasonsummer  0.380698   0.373767   1.019    0.348

#Residual standard error: 0.5549 on 6 degrees of freedom
#Multiple R-squared:  0.3694,	Adjusted R-squared:  0.1591 
#F-statistic: 1.757 on 2 and 6 DF,  p-value: 0.2508

#continue with geographic patterns in water chem#

###########################
#univariate relationships
###########################
N<-subset(nds_chem, N==1)
P<-subset(nds_chem, P==1)
Si<-subset(nds_chem, Si==1)

#N limitation/inhibition
ggplot(subset(N, nutrient=="N"), aes(x=DIN.mgNL, y=chla.nrr))+
  geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#no pattern

ggplot(subset(N, nutrient=="N"), aes(x=NO3.mgNL, y=chla.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#general increase with NO3 but lots of variability

ggplot(subset(N, nutrient=="N"), aes(x=NH4.mgNL, y=chla.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#general increase with NH4 

ggplot(subset(nds_chem, nutrient=="control"), aes(x=NH4.mgNL, y=chla))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#general increase in chla with NH4 but LOTS of variability; probably not much explanatory power

ggplot(subset(nds_chem, nutrient=="control"), aes(x=NO3.mgNL, y=chla))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#no pattern

ggplot(subset(N, nutrient=="N"), aes(x=NO3.mgNL, y=cr.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#negative exponential, both summer and fall. N< 0.5 mg/L to get limitation

ggplot(subset(N, nutrient=="N"), aes(x=NH4.mgNL, y=cr.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#limitation at very low end only, but even there some inhibition

ggplot(subset(nds_chem, nutrient=="control"), aes(x=NO3.mgNL, y=cr.area))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#no pattern

ggplot(subset(nds_chem, nutrient=="control"|nutrient=="C"), aes(x=NH4.mgNL, y=cr.area))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#general increase in CR with NH4


#P limitation/inhibition
ggplot(subset(P, nutrient=="P"), aes(x=oP.mgPL, y=chla.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#in summer, some intermediate values had limitation. P is not driving this system!

ggplot(subset(nds_chem, nutrient=="control"|nutrient=="C"), aes(x=oP.mgPL, y=chla))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#no pattern

ggplot(subset(P, nutrient=="P"), aes(x=oP.mgPL, y=cr.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#maybe a faint upward relationship? but soo variable

ggplot(subset(nds_chem, nutrient=="control"|nutrient=="C"), aes(x=oP.mgPL, y=cr.area))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#increase in summer CR with P (but variable)

#Si limitation/inhibition
ggplot(subset(Si, nutrient=="Si"), aes(x=Si.mgL, y=chla.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))+geom_hline(yintercept=1)
#in summer, occasional spikes but no pattern to the spikes 

ggplot(subset(nds_chem, nutrient=="control"|nutrient=="C"), aes(x=Si.mgL, y=chla))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#up and down with no real pattern

ggplot(subset(Si, nutrient=="Si"), aes(x=Si.mgL, y=cr.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#general upward relationship  with Si, especially in summer, but variable

ggplot(subset(nds_chem, nutrient=="control"|nutrient=="C"), aes(x=Si.mgL, y=cr.area))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#maybe hump-shaped? with max CR at medium Si - but lots of variation

#N:P ratios
ggplot(subset(nds_chem, nutrient=="control"|nutrient=="C"), aes(x=N.P.ratio, y=chla))+theme_classic()+
  geom_point(aes(color=factor(season)))+scale_color_manual(values=c("goldenrod2", "plum3"))+
  geom_vline(xintercept = 16)
#increase!

ggplot(N, aes(x=N.P.ratio, y=chla.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()+geom_vline(xintercept = 16)
#In summer: negative exponential (lower N:P = greater response to N addition). greatest increase with N alone
#In fall: no change

ggplot(P, aes(x=N.P.ratio, y=chla.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()+geom_vline(xintercept = 16)

ggplot(subset(nds_chem, nutrient=="control"), aes(x=N.P.ratio, y=cr.area))+theme_classic()+
  geom_point(aes(color=factor(season)))+scale_color_manual(values=c("goldenrod2", "plum3"))+
  geom_vline(xintercept = 16)
#increase to threshold

ggplot(N, aes(x=N.P.ratio, y=cr.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()+geom_vline(xintercept = 16)
#In summer and fall, slight decline with N:P ratio. No response with NSi

#N:Si ratios
ggplot(subset(nds_chem, nutrient=="control"), aes(x=N.Si.ratio, y=chla))+theme_classic()+
  geom_point(aes(color=factor(season)))+scale_color_manual(values=c("goldenrod2", "plum3"))+
  geom_vline(xintercept = 0.8)
#increase in chla with N:Si ratio, in both fall and summer

ggplot(N, aes(x=N.Si.ratio, y=chla.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()+geom_vline(xintercept = 0.8)

ggplot(Si, aes(x=N.Si.ratio, y=chla.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()+geom_vline(xintercept = 0.8)
#lowerN:Si ratio = higher chla, espeically in NPSi and Si (??? why would adding more Si help?)

ggplot(subset(nds_chem, nutrient=="control"), aes(x=N.Si.ratio, y=cr.area*-1))+theme_classic()+
  geom_point(aes(color=factor(season)))+scale_color_manual(values=c("goldenrod2", "plum3"))+
  geom_vline(xintercept = 0.8)
#|CR| increases with N:Si

ggplot(Si, aes(x=N.Si.ratio, y=cr.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()+geom_vline(xintercept = 0.8)
#decreases with N:Si ratio but only because of NPSi in summer. so basically no pattern

#P:Si ratios
ggplot(subset(nds_chem, nutrient=="control"), aes(x=P.Si.ratio, y=chla))+theme_classic()+
  geom_point(aes(color=factor(season)))+scale_color_manual(values=c("goldenrod2", "plum3"))+
  geom_vline(xintercept = 0.05)
#all sites far below Redfield P:Si ratio of 0.05. General increase with P:Si ratio but high variability

ggplot(P, aes(x=P.Si.ratio, y=chla.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()
#higher chla at low P:Si ratios, esp for NP

ggplot(Si, aes(x=P.Si.ratio, y=chla.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()
#no clear patterns

