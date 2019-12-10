#script to analyze the greenhouse vs. lab comparison of nutrient limitation. 
#first, determine nutrient limitation within substrate type and incubation location. 
#then, compare among locations

#packages
library(nlme)
library(dplyr)
library(ggplot2)

#Load data
gh_lab <- read.csv("GH_lab_Columbia.csv")

#convert N and P values (0 or 1 for absence or presence) to factors
gh_lab$N<-as.factor(gh_lab$N)
gh_lab$P<-as.factor(gh_lab$P)

#subset data into gpp and cr response
gh<-subset(gh_lab, incubation=="GH", data=gh_lab)
lab<-subset(gh_lab, incubation =="lab", data=gh_lab)

                       
############################################################
#plot CR data
ggplot(data=subset(gh, top=="cellulose"), aes(x=nutrient, y = cr.area))+geom_boxplot()+theme_classic()+
  ggtitle("GH cellulose")
ggplot(data=subset(lab, top=="cellulose"), aes(x=nutrient, y = cr.area))+geom_boxplot()+theme_classic()+
  ggtitle("lab cellulose")

#analyze CR data
M1<-aov(cr.area~N*P, data=subset(gh, top=="cellulose"))
summary(M1)




