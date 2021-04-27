#Author:  Clay Arango
#Creation date: 10-Feb-19
#Script to analyze NDS data from Summer 2018 WSB work with S. Roley and A. Alexiades

#packages
library(nlme)
library(nortest)
library(plyr)
library(dplyr)
library(multcomp)
library(MASS)
library(ggplot2)

#Load data
reec_summer <- read.table(file="reec_summer.csv", header=T, sep=",")

#set variable
d = reec_summer

#evaluate data to make sure factors are correct
names(d)
unique(d$nutrient) #control, N, NP, NPSi, NSi, P, PSi, Si
unique(d$N) #should be 0 and 1
unique(d$P) #should be 0 and 1
unique(d$Si) #should be 0 and 1
unique(d$top) #should be "sponge" and "glass"
#if need to change, use recode function below. 
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#convert N and P vand Si alues (0 or 1 for absence or presence) to factors
d$N<-as.factor(d$N)
d$P<-as.factor(d$P)
d$Si<-as.factor(d$Si)
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge", data=d)
d.gpp = subset(d, top=="glass", data=d)

#check distribution of controls and remove as needed before calculating NRR.
ggplot(d.cr, aes(x=nutrient, y=cr.area))+geom_boxplot() +theme_classic()
#ok

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-15.02240 #divide by control ave_cr

#check distribution of controls and remove as needed before calculating NRR.
ggplot(d.gpp, aes(x=nutrient, y=gpp.area))+geom_boxplot() +theme_classic()
#check CC1, but probably ok
ggplot(d.gpp, aes(x=nutrient, y=chla))+geom_boxplot() +theme_classic()
#ok - updated b/c original file had values too small (units were mg/cm2)

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/5.770496 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.05252345 #divide by control ave_chla

#combine files and export
d.cr$chla.nrr<-NA
d.cr$gpp.nrr<-NA
d.gpp$cr.nrr<-NA
d.nrr<-rbind(d.cr, d.gpp)
d.nrr$site_date<-"reec_summer"

d.nrr$gpp.es<-log(d.nrr$gpp.nrr)
d.nrr$cr.es<-log(d.nrr$cr.nrr)
d.nrr$chla.es<-log(d.nrr$chla.nrr)

write.table(d.nrr, "reec_summer_nrr.csv", sep=",", quote=F, row.names =F)

###############
#plots of NRR and effect size
##############
ggplot(data=d.cr, aes(x=nutrient, y=cr.nrr))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#inhibition of most; limitation with NP

ggplot(data=subset(d.nrr, (top=="sponge")), aes(x=nutrient, y=cr.es))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=subset(d.gpp, !(nutrient=="control")), aes(x=nutrient, y=gpp.nrr))+geom_boxplot()+theme_bw()+
  ylab("GPP NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#inhibition or neutral

ggplot(data=subset(d.nrr, (top=="glass")), aes(x=nutrient, y=gpp.es))+geom_boxplot()+theme_bw()+
  ylab("GPP Effect Size")+geom_abline(slope = 0, intercept = 1)+geom_hline(yintercept = -0.7, lty="dashed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=d.gpp, aes(x=nutrient, y=log(chla.nrr)))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#limitation for nearly all

ggplot(data=subset(d.nrr, (top=="glass")), aes(x=nutrient, y=chla.es))+geom_boxplot()+theme_bw()+
  ylab("Chla Effect Size")+geom_hline(yintercept = 1.385, lty="solid")+ 
 geom_hline(yintercept = 0.7, lty="dashed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

##########
#NRR Summary Files
#########

GPP_sum<-ddply(d.gpp, "nutrient", summarise, ave_nrr.gpp=mean(gpp.nrr, na.rm=T), sd_nrr.gpp=sd(gpp.nrr, na.rm=T), 
               se_nrr.gpp =(sd(gpp.nrr)/sqrt(sum(!is.na(gpp.nrr)))), 
               ci95_nrr.gpp = (1.96*(sd(gpp.nrr)/sqrt(sum(!is.na(gpp.nrr))))),
               ave_nrr.chla=mean(chla.nrr, na.rm=T), sd_nrr.chla=sd(chla.nrr, na.rm=T), 
               se_nrr.chla =(sd(chla.nrr)/sqrt(sum(!is.na(chla.nrr)))), 
               ci95_nrr.chla = (1.96*(sd(chla.nrr)/sqrt(sum(!is.na(chla.nrr))))))
CR_sum<-ddply(d.cr, "nutrient", summarise, ave_nrr.cr=mean(cr.nrr, na.rm=T), sd_nrr.cr=sd(cr.nrr, na.rm=T), 
              se_nrr.cr =(sd(cr.nrr)/sqrt(sum(!is.na(cr.nrr)))), 
              ci95_nrr.cr = (1.96*(sd(cr.nrr)/sqrt(sum(!is.na(cr.nrr))))))

#now combine into one and export
d.sum<-merge(GPP_sum, CR_sum, by="nutrient")
d.sum$site_date<-"reec_summer"
write.table(d.sum, "reec_summer_summ.csv",  sep=",", quote=F, row.names =F)

############################################################
#analyze RESPIRATION data
############################################################
d.cr1<-subset(d.cr, !(nds.id=="U1"))
M1<-gls(cr.area~N*P*Si, data=d.cr1, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)#mostly ok, except for a couple points
ad.test(E1)
  #removed problematic point and now p = 0.3584   
#residuals are normally distributed, p=0.20289
  #logging makes it worse (p= 0.007278)
hist(E1) #normal except one point 
plot(M1)
   
plot(filter(d.cr1, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr1)
   #variance test not OK, even with logging and removing problematic point
  #but after removing problematic point, variance looks ok

anova(M1) #N limitation
#Results with problematic point removed
# numDF  F-value p-value
#(Intercept)     1 919.7099  <.0001
#N               1   6.5966  0.0153
#P               1   0.2429  0.6256
#Si              1   2.1390  0.1537
#N:P             1   7.5672  0.0098
#N:Si            1   0.6311  0.4330
#P:Si            1   0.0119  0.9138
#N:P:Si          1   0.1241  0.7271

#Results with all data
#N               1   7.8923  0.0084
#P               1   0.8282  0.3696
#Si              1   3.2453  0.0811
#N:P             1   3.8224  0.0594
#N:Si            1   1.7009  0.2015
#P:Si            1   0.4663  0.4996
#N:P:Si          1   0.0752  0.7857

##Analyze respiration with all points
M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are not normal, p=0.2028
hist(E1)  
plot(M1)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
#variance test is no ok, p=0.002

#try normalizing
d.cr$l.cr.area = log10(d.cr$cr.area*-1)

M2<-gls(l.cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are not normal, p=0.007278
hist(E2)  
plot(M2)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(l.cr.area~nutrient, data=d.cr)
#variance test is not equal, p=4.037e-5

#try cube root transformation, but need to make the values positive

d.cr$cube.cr.area = (d.cr$cr.area*-1)^(1/3)

M3<-gls(cube.cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
#residuals are not normally distributed p = 0.04518
hist(E3)  
plot(M3)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(cube.cr.area~nutrient, data=d.cr)
#variance test 0.0003634

#try square root transformation

d.cr$sqr.cr.area = (d.cr$cr.area*-1)^(1/2)

M4<-gls(sqr.cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E4<-residuals(M3)
qqnorm(E4)
qqline(E4)
ad.test(E4)
#residuals are not normally distributed p = 0.04518
hist(E4)  
plot(M4)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(sqr.cr.area~nutrient, data=d.cr)
#variance test 0.000765

#try non-parametric Aligned Rank Test
install.packages("ARTool")
library(ARTool)

d.cr.clean = na.omit(subset(d.cr, select = c(N,P,Si,cr.area)))

M5 = art(cr.area ~ N*P*Si, data=d.cr.clean)

summary(M5) #supposed to be at or about 0

shapiro.test(residuals(M5))
qqnorm(residuals(M5)); qqline(residuals(M5))
anova(M5)
anova(M5, type = 3)#type 3 is better for interaction terms 

##########################################################
#do multiple 2 way ANOVAs to improve our ability to interpret
##########################################################
#N and P
xx = na.omit(subset(d.cr, select = c(N,P,cr.area)))
interaction.plot(xx$N, xx$P, xx$cr.area*-1)

#N and Si
xx = na.omit(subset(d.cr, select = c(N,Si,cr.area)))
interaction.plot(xx$N, xx$Si, xx$cr.area*-1)

#P and Si
xx = na.omit(subset(d.cr, select = c(P,Si,cr.area)))
interaction.plot(xx$P, xx$Si, xx$cr.area*-1)

############################################################
#analyze the PRODUCTION data
############################################################

M1<-gls(gpp.area~N*P*Si, data=d.gpp, na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals are OK

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")

bartlett.test(gpp.area~nutrient, data=d.gpp)
   #ok

anova(M1)
  


############################################################
#analyze the CHL-A biomass on disks

M1<-gls(log(chla)~N*P*Si, data=d.gpp, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals NOT ok
#with ln, p = 0.065
hist(E1)
plot(M1) # variance increases with mean!

plot(filter(d.gpp, !is.na(chla)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")

#can't do bartlett test b/c no variance in control, so instead created data frame without control
b<-subset(d.gpp, !(nutrient=="C"))

bartlett.test(log(chla)~nutrient, data=b) #p = 0.069

anova(M1)
#N               1   1.37757  0.2501
#P               1   1.42451  0.2423
#Si              1   1.74922  0.1963
#N:P             1   1.06962  0.3096
#N:Si            1   3.61945  0.0671
#P:Si            1   9.58250  0.0043
#N:P:Si          1   5.03540  0.0326


M1<-gls(chla~N*P*Si, data=d.gpp, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals look bad (p=5.487e-7)
hist(E1)
plot(M1)

bartlett.test(chla~nutrient, data=d.gpp)
#data look bad p<2.2e-16

#log transformation
d.gpp$l.chla = log10(d.gpp$chla+1)

M2<-gls(l.chla~N*P*Si, data=d.gpp, na.action=na.omit) 
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are not normal, p=0.000129

hist(E2, xlab="residuals", main="")
plot(M2)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(l.chla~nutrient, data=d.gpp)
#data look bad p<2.2e-16

#try cube root transformation

d.gpp$cube.chla.area = (d.gpp$chla)^(1/3)

M3<-gls(cube.chla.area~N*P*Si, data=d.gpp, na.action=na.omit)
E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
#residuals are not normally distributed p = 0.02452
hist(E3)  
plot(M3)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(cube.chla.area~nutrient, data=d.gpp)
#data look bad p<2.2e-16

#try sqr root
d.gpp$sqr.chla.area = (d.gpp$chla)^(1/2)

M4<-gls(sqr.chla.area~N*P*Si, data=d.gpp, na.action=na.omit)
E4<-residuals(M4)
qqnorm(E4)
qqline(E4)
ad.test(E4)
#residuals are not normally distributed p = 0.0046
hist(E4)  
plot(M4)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(sqr.chla.area~nutrient, data=d.gpp)
#data look bad p<2.2e-16

#try non-parametric Aligned Rank Test
install.packages("ARTool")
library(ARTool)

d.chla.clean = na.omit(subset(d.gpp, select = c(N,P,Si,chla)))

M5 = art(chla ~  N*P*Si, data=d.chla.clean)

summary(M5) #supposed to be at or about 0

shapiro.test(residuals(M5))
qqnorm(residuals(M5)); qqline(residuals(M5))
anova(M5)
anova(M5, type = 3)#type 3 is better for interaction terms 

##########################################################
#do multiple 2 way ANOVAs to improve our ability to interpret
##########################################################
#N and P  
xx = na.omit(subset(d.gpp, select = c(N,P,chla)))
interaction.plot(xx$N, xx$P, xx$chla)

#N and Si Si inhibited in presence of N (but NS)
xx = na.omit(subset(d.gpp, select = c(N,Si,chla)))
interaction.plot(xx$N, xx$Si, xx$chla)

#P and Si P reduces resposne to Si
xx = na.omit(subset(d.gpp, select = c(P,Si,chla)))
interaction.plot(xx$P, xx$Si, xx$chla)


##########################################################
#Analyze chl-a by removing all Si treatments
##########################################################
d.gppNoSi = subset(d.gpp, Si==0)

M1<-gls(chla~N*P, data=d.gppNoSi, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals look good (p=0.086)
hist(E1)
plot(M1)

bartlett.test(chla~nutrient, data=d.gpp)
#data look bad p=2.2e-16

#log transformation
d.gppNoSi$l.chla = log10(d.gppNoSi$chla+1)

M2<-gls(l.chla~N*P, data=d.gppNoSi, na.action=na.omit) 
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are normal, p=0.12

hist(E2, xlab="residuals", main="")
plot(M2)
bartlett.test(l.chla~nutrient, data=d.gppNoSi)
#not good, p = 2.2e-16

#try cube root transformation, but need to make the values positive

d.gppNoSi$cube.chla.area = (d.gppNoSi$chla)^(1/3)

M3<-gls(cube.chla.area~N*P, data=d.gppNoSi, na.action=na.omit)
E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
#residuals are not normally distributed p = 0.43
hist(E3)  
plot(M3)

bartlett.test(cube.chla.area~nutrient, data=d.gppNoSi)
#variance test p = 2.2e-16

#try sqr root
d.gppNoSi$sqr.chla.area = (d.gppNoSi$chla)^(1/2)

M4<-gls(sqr.chla.area~N*P, data=d.gppNoSi, na.action=na.omit)
E4<-residuals(M4)
qqnorm(E4)
qqline(E4)
ad.test(E4)
#residuals are normally distributed p = 0.32
hist(E4)  
plot(M4)

bartlett.test(sqr.chla.area~nutrient, data=d.gppNoSi)
#variance test p = 2.2e-16

#try non-parametric Aligned Rank Test
install.packages("ARTool")
library(ARTool)

d.gppNoSi = na.omit(subset(d.gppNoSi, select = c(N,P,chla)))

M5 = art(chla ~  N*P, data=d.gppNoSi)

summary(M5) #supposed to be at or about 0

shapiro.test(residuals(M5))
qqnorm(residuals(M5)); qqline(residuals(M5))
anova(M5)
anova(M5, type = 3)#type 3 is better for interaction terms 

#Interpret Interaction
#N and P
xx = na.omit(subset(d.gppNoSi, select = c(N,P,chla)))
interaction.plot(xx$N, xx$P, xx$chla)
