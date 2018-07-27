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