#script to analyze the greenhouse vs. lab comparison of nutrient limitation. 
#first, determine nutrient limitation within substrate type and incubation location. 
#then, compare among locations

#packages
library(nlme)
library(dplyr)
library(ggplot2)
library(gridExtra)

#Load data
gh_lab <- read.csv("GH_lab_Columbia.csv")

#convert N and P values (0 or 1 for absence or presence) to factors
gh_lab$N<-as.factor(gh_lab$N)
gh_lab$P<-as.factor(gh_lab$P)

#subset data into gpp and cr response
gh<-subset(gh_lab, incubation=="GH", data=gh_lab)
lab<-subset(gh_lab, incubation =="lab", data=gh_lab)

                       
############################################################
#plots!
GH_CR<-ggplot(data=subset(gh, top=="cellulose"), aes(x=nutrient, y = -cr.area))+geom_boxplot()+theme_classic()+
  ggtitle("GH cellulose")+ylim(10,30)
lab_CR<-ggplot(data=subset(lab, top=="cellulose"), aes(x=nutrient, y = -cr.area))+geom_boxplot()+theme_classic()+
  ggtitle("lab cellulose")+ylim(10,30)
grid.arrange(GH_CR, lab_CR, ncol=2)

GH_CR.NRR<-ggplot(data=subset(gh, top=="cellulose"), aes(x=nutrient, y = cr.nrr))+geom_boxplot()+theme_classic()+
  ggtitle("GH cellulose")
lab_CR.NRR<-ggplot(data=subset(lab, top=="cellulose"), aes(x=nutrient, y =cr.nrr))+geom_boxplot()+theme_classic()+
  ggtitle("lab cellulose")
grid.arrange(GH_CR.NRR, lab_CR.NRR, ncol=2)

GH_GPP<-ggplot(data=subset(gh, top=="glass"), aes(x=nutrient, y = gpp.area))+geom_boxplot()+theme_classic()+
  ggtitle("GH glass")+ylim(0,25)
lab_GPP<-ggplot(data=subset(lab, top=="glass"), aes(x=nutrient, y = gpp.area))+geom_boxplot()+theme_classic()+
  ggtitle("lab glass")+ylim(0,25)
grid.arrange(GH_GPP, lab_GPP, ncol=2)

GH_GPP.NRR<-ggplot(data=subset(gh, top=="glass"), aes(x=nutrient, y = gpp.nrr))+geom_boxplot()+theme_classic()+
  ggtitle("GH glass")
lab_GPP.NRR<-ggplot(data=subset(lab, top=="glass"), aes(x=nutrient, y =gpp.nrr))+geom_boxplot()+theme_classic()+
  ggtitle("lab glass")
grid.arrange(GH_GPP.NRR, lab_GPP.NRR, ncol=2)

GH_chla<-ggplot(data=gh, aes(x=nutrient, y=chla_mgL))+geom_boxplot()+theme_classic()+
  ggtitle("GH")
lab_chla<-ggplot(data=lab, aes(x=nutrient, y=chla_mgL))+geom_boxplot()+theme_classic()+
  ggtitle("lab")
grid.arrange(GH_chla, lab_chla, ncol=2)

GH_chla.NRR<-ggplot(data=gh, aes(x=nutrient, y = chla.nrr))+geom_boxplot()+theme_classic()+
  ggtitle("GH")
lab_chla.NRR<-ggplot(data=lab, aes(x=nutrient, y =chla.nrr))+geom_boxplot()+theme_classic()+
  ggtitle("lab")
grid.arrange(GH_chla.NRR, lab_chla.NRR, ncol=2)

#analyze CR data
M1<-aov(cr.area~N*P, data=subset(gh, top=="cellulose"))
summary(M1)
M2<-aov(cr.area~N*P, data=subset(lab, top=="cellulose"))
summary(M2)
M3<-aov(cr.nrr~nutrient, data=subset(gh, top =="cellulose"))
summary(M3) #p = 0.11
M4<-aov(cr.nrr~nutrient, data=subset(lab, top =="cellulose"))
summary(M4)#p = 0.674

#analyze GPP data
M1<-aov(gpp.area~N*P, data=subset(gh, top =="glass"))
summary(M1)
M2<-aov(gpp.area~N*P, data=subset(lab, top =="glass"))
summary(M2)
M3<-aov(gpp.nrr~nutrient, data=subset(gh, top =="glass"))
summary(M3) 
M4<-aov(gpp.nrr~nutrient, data=subset(lab, top =="glass"))
summary(M4)

#analyze chla data
M1<-aov(chla_mgL~N*P, data=subset(gh, top =="glass"))
summary(M1)
M2<-aov(chla_mgL~N*P, data=subset(lab, top =="glass"))
summary(M2)
M3<-aov(chla.nrr~nutrient, data=gh)
summary(M3) 
TukeyHSD(M3)
M4<-aov(chla.nrr~nutrient, data=lab)
summary(M4)
TukeyHSD(M4)






