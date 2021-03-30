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

