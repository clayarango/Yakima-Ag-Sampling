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

N<-subset(nds_chem, N==1)
P<-subset(nds_chem, P==1)
Si<-subset(nds_chem, Si==1)

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
ggplot(N, aes(x=river_mile, y=cr.nrr))+geom_point(aes(color=factor(type)))+theme_classic()+
  facet_wrap(~season)
#summer, higher river mile = bigger response in mainstem

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
#Ahtanum and Century Landing have the same river mile. Need to fix - Ahtanum flows in downstream of CenLand.

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

