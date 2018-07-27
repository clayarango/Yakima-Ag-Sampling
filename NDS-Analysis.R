#2-Jul-18
#Code to analyze NDS experiment in Yakima Basin
#Sarah Roley, Clay Arango, Alex Alexiades

#Columbia River: Greenhouse vs. lab boxplots
library(ggplot2)

GH_lab<-read.csv("ColumbiaRiver_GH_lab.csv")

GH_lab_noC<-subset(GH_lab, !nutrient=="control")
ggplot(GH_lab_noC, aes(x=nutrient, y= cr.nrr))+geom_boxplot(aes(color=factor(incubation)))+facet_wrap(~top)+theme_bw()+
  geom_hline(yintercept=1)
ggplot(GH_lab_noC, aes(x=nutrient, y= gpp.nrr))+geom_boxplot(aes(color=factor(incubation)))+facet_wrap(~top)+theme_bw()+
  geom_hline(yintercept=1)

#limitation analysis
str(GH_lab)
GH<-subset(GH_lab, incubation=="GH")
GH_glass<-subset(GH, top=="glass")
GH_sponge<-subset(GH, top=="cellulose")

lab<-subset(GH_lab, incubation=="lab")
lab_glass<-subset(lab, top=="glass")
lab_sponge<-subset(lab, top=="cellulose")

str(lab)

#analyze Greenhouse limitation
gpp_GH = aov(gpp.area~N*P, data=GH_glass)
summary(gpp_GH)
     #no limitation of gpp
     #still needs residual analysis

cr_GH = aov(cr.area~N*P, data=GH_sponge)
summary(cr_GH)
     #no limitation of cr (N limitation at a=0.10)
     #still needs residual analysis

#analyze lab limitation
gpp_lab = aov(gpp.area~N*P, data=lab_glass)
summary(gpp_lab)
     #P limitation of gpp
     #still needs residual analysis

cr_lab = aov(cr.area~N*P, data=lab_sponge)
summary(cr_lab)
     #N limitation of CR
     #still needs residual analysis

#not sure of the plot to use, but we'll want control, N, P, N+P on x axis, and I think GH v. lab inside each panel, with cr for just sponge and gpp for just glass
