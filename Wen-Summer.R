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
wenas_summer <- read.table(file="wen_summer.csv", header=T, sep=",")

#set variable
d = wenas_summer

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
d$chla.nrr<-NA

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge", data=d)
d.gpp = subset(d, top=="glass", data=d)

#check distribution of controls and remove as needed before calculating NRR.
ggplot(d.cr, aes(x=nutrient, y=cr.area))+geom_boxplot() +theme_classic()
#ok

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-5.765783 #divide by control ave_cr

#check distribution of controls and remove as needed before calculating NRR.
ggplot(d.gpp, aes(x=nutrient, y=gpp.area))+geom_boxplot() +theme_classic()
#one outlier. C8 has GPP = 2.94, which is > 4x as high as next-highest control. Remove?
#also, C2 has GPP of 12.68, which is 3x as high as others in N treatment, but just slightly
#higher than those in NPSi. Numbers were checked and correct, but concern over effect of control on NRR.
ggplot(d.gpp, aes(x=nutrient, y=chla))+geom_boxplot() +theme_classic()
#G8 has chla of 0.94, which is >7x higher than other controls. Remove?
#also, F5 has chla = 4.3, which is almost twice as high as next highest. Numbers were checked and are correct
#but are they too high to be reasonable?

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/0.6824351 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.22347695 #divide by control ave_chla
x1g<-ddply(subset(d.gpp, !(nds.id=="C8")), "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T))
x1g
d.gpp$gpp.nrr = d.gpp$gpp.area/0.2308552 #NRR with weird control removed.
x1c<-ddply(subset(d.gpp, !(nds.id=="G8")), "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T))
x1c
d.gpp$chla.nrr=d.gpp$chla/0.03803029
#note: chose to keep G8 out of the chla calcs and C8 out of the GPP calcs.

#combine files and export
d.cr$gpp.nrr_1<-NA
d.cr$chla.nrr_1<-NA
d.nrr<-rbind(d.cr, d.gpp)
d.nrr$site.date<-"wenas_summer"
d.nrr$cr.es<-log(d.nrr$cr.nrr)
d.nrr$gpp.es<-log(d.nrr$gpp.nrr)
d.nrr$chla.es<-log(d.nrr$chla.nrr)
d.nrr$gpp.es_1<-log(d.nrr$gpp.nrr_1)
d.nrr$chla.es_1<-log(d.nrr$chla.nrr_1)
write.table(d.nrr, "wenas_summer_nrr.csv", sep=",", quote=F, row.names =F)

write.table(d.nrr, "roza_fall_nrr.csv",  sep=",", quote=F, row.names =F)
###############
#plots of NRR
##############
ggplot(data=d.cr, aes(x=nutrient, y=cr.nrr))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#inhibition of most N; limitation with Si; others neutral

ggplot(data=subset(d.nrr, (top=="sponge")), aes(x=nutrient, y=cr.es))+geom_boxplot()+theme_bw()+
  ylab("CR Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_hline(yintercept = -1.385)+geom_hline(yintercept = 1.385)

ggplot(data=d.gpp, aes(x=nutrient, y=gpp.nrr))+geom_boxplot()+theme_bw()+
  ylab("GPP NRR")+geom_abline(slope = 0, intercept = 1)+ggtitle("All Data")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#limitation of most with P; others neutral. One point on N very high (NRR > 15 on C2)

ggplot(data=subset(d.gpp,!(nds.id=="C8")), aes(x=nutrient, y=gpp.nrr_1))+geom_boxplot()+theme_bw()+
  ylab("GPP NRR")+geom_abline(slope = 0, intercept = 1)+ggtitle("Outlier Removed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#limitation of most with P; others neutral. One point on N very high (NRR > 15 on C2)

d.nrrG<-subset(d.nrr, (top=="glass"))
ggplot(data=d.nrrG, aes(x=nutrient, y=gpp.es))+geom_boxplot()+theme_bw()+
  ylab("GPP Effect Size")+geom_hline(yintercept = -0.7, lty="dashed")+geom_hline(yintercept = 0.7, lty="dashed")+
  geom_hline(yintercept = -1.385)+geom_hline(yintercept = 1.385)+ggtitle("All Data")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=subset(d.nrrG,!(nds.id=="C8")), aes(x=nutrient, y=gpp.es_1))+geom_boxplot()+theme_bw()+
  ylab("GPP Effect Size")+geom_hline(yintercept = -0.7, lty="dashed")+geom_hline(yintercept = 0.7, lty="dashed")+
  geom_hline(yintercept = -1.385)+geom_hline(yintercept = 1.385)+ ggtitle ("Outlier Removed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=d.gpp, aes(x=nutrient, y=chla.nrr))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+ggtitle("All Data")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#limitation for most N; inhibition of P. One N value very high (> 20). This is F5 - looks like chla was so high it was diluted.
#Also check one high Control value (G8).

ggplot(data=subset (d.gpp, !(nds.id =="G8")), aes(x=nutrient, y=chla.nrr_1))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+ggtitle("Outlier Removed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=subset(d.nrrG, !(nds.id=="G8")), aes(x=nutrient, y=chla.es_1))+geom_boxplot()+theme_bw()+
  ylab("Chla Effect Size")+ geom_hline(yintercept = 0.7, lty="dashed")+  geom_hline(yintercept = -1.385)+  
  geom_hline(yintercept = -0.7, lty="dashed")+ geom_hline(yintercept = 1.385)+ggtitle ("Outlier Removed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=d.nrrG, aes(x=nutrient, y=chla.es))+geom_boxplot()+theme_bw()+
  ylab("Chla Effect Size")+ geom_hline(yintercept = 0.7, lty="dashed")+  geom_hline(yintercept = -1.385)+  
  geom_hline(yintercept = -0.7, lty="dashed")+ geom_hline(yintercept = 1.385)+ ggtitle("All Data")+
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

M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #deviates from QQ line (p= 0.0459). worse if logged
hist(E1)#skewed b/c of 2 low-ish points. not too bad though. inclined to leave it.  
plot(M1)
   
plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
   #variance test OK

anova(M1) #P limitation, N limitation, various interactions
#N               1  42.75147  <.0001
#P               1  18.33570  0.0002
#Si              1   7.92525  0.0085
#N:P             1   1.57398  0.2193
#N:Si            1   8.41162  0.0069
#P:Si            1  29.37030  <.0001
#N:P:Si          1   5.88350  0.0215


M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.04592
hist(E1)  
plot(M1)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
#variance test is not equal, p=0.1986

#try normalizing
d.cr$l.cr.area = log10(d.cr$cr.area*-1)

M2<-gls(l.cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are not normal, p=0.002243
hist(E2)  
plot(M2)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(l.cr.area~nutrient, data=d.cr)
#variance test is not equal, p=3.608e-5

#try cube root transformation, but need to make the values positive

d.cr$cube.cr.area = (d.cr$cr.area*-1)^(1/3)

M3<-gls(cube.cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
#residuals are not normally distributed p = 0.6208
hist(E3)  
plot(M3)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(cube.cr.area~nutrient, data=d.cr)
#variance test 0.04486

#try square root transformation

d.cr$sqr.cr.area = (d.cr$cr.area*-1)^(1/2)

M4<-gls(sqr.cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E4<-residuals(M3)
qqnorm(E4)
qqline(E4)
ad.test(E4)
#residuals are normally distributed p = 0.6208
hist(E4)  
plot(M4)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(sqr.cr.area~nutrient, data=d.cr)
#variance test 0.2371

anova(M4)

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

M1<-gls(log(gpp.area+1)~N*P*Si, data=subset(d.gpp, !(nds.id=="C8")), na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1) #way off, much better with log-transform
ad.test(E1)
   #p <0.000001, p = 0.1195 with log-transform

hist(E1, xlab="residuals", main="") #ok
plot(M1) #increase in variation with magnitude, ok with log-transform

plot(filter(subset(d.gpp, !(nds.id=="C8")), !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(log(gpp.area+1)~nutrient, data=d.gpp)
   #p <0.000001, p = 0.02966 with log-transform

anova(M1)
  
x <- group_by(d.gpp, nutrient) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gpp.mean = abs(mean(gpp.area, na.rm = TRUE)), # na.rm = TRUE to remove missing values
            gpp.sd=abs(sd(gpp.area, na.rm = TRUE)),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gpp.area)), # of observations, excluding NAs. 
            gpp.se=gpp.sd/sqrt(n))




############################################################
#analyze the CHL-A biomass on disks
############################################################
d.gpp1<-subset(d.gpp, !(nds.id=="G8"))
M1<-gls(chla~N*P*Si, data=d.gpp1, na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1) #ugh - WAY off
ad.test(E1)
#p<0.00001, p = 0.000334 with square root transformation
#log-transform doesn't do much

hist(E1, xlab="residuals", main="") #good!
plot(M1) #huge increase in variation with magnitude

plot(filter(subset(d.gpp, !(nds.id=="G8")), !is.na(chla)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(log(chla)~nutrient, data=d.gpp1)
#N way more variation than other treatments; P and PSi almost no variation. not much we can do here.
#neither log transform nor square root helps

anova(M1)
#N               1 37.60024  <.0001
#P               1  2.51261  0.1238
#Si              1  0.46426  0.5010
#N:P             1  0.22276  0.6405
#N:Si            1  1.83080  0.1865
#P:Si            1  5.45885  0.0266
#N:P:Si          1  8.39505  0.0071


M1<-gls(chla~N*P*Si, data=d.gpp, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are not normal, p=2.078e-7

hist(E1)  
plot(M1)

plot(filter(d.gpp, !is.na(chla)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(chla~nutrient, data=d.gpp)
#variance test p<2.2e-16

#try normalizing

d.gpp$l.chla = log10(d.gpp$chla+1)

M2<-gls(l.chla~N*P*Si, data=d.gpp, na.action=na.omit)
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are not normal, p=2.134e-5

hist(E2)  
plot(M2)

plot(filter(d.gpp, !is.na(chla)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(l.chla~nutrient, data=d.gpp)
#variance test p<2.2e-16

#try cube root transformation

d.gpp$cube.chla.area = (d.gpp$chla)^(1/3)

M3<-gls(cube.chla.area~N*P*Si, data=d.gpp, na.action=na.omit)
E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
#residuals are not normally distributed p = 8.372e-5
hist(E3)  
plot(M3)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(cube.chla.area~nutrient, data=d.gpp)
#variance test p<2.2e-16

#try sqr root
d.gpp$sqr.chla.area = (d.gpp$chla)^(1/2)

M4<-gls(sqr.chla.area~N*P*Si, data=d.gpp, na.action=na.omit)
E4<-residuals(M4)
qqnorm(E4)
qqline(E4)
ad.test(E4)
#residuals are not normally distributed p = 2.615e-5
hist(E4)  
plot(M4)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(sqr.chla.area~nutrient, data=d.gpp)
#variance test p<2.2e-16

#try non-parametric Aligned Rank Test
install.packages("ARTool")
library(ARTool)

M5 = art(chla ~  N*P*Si, data=d.gpp)

summary(M5) #supposed to be at or about 0

shapiro.test(residuals(M5))
qqnorm(residuals(M5)); qqline(residuals(M5))
anova(M5)
anova(M5, type = 3)#type 3 is better for interaction terms 

################################
##2-way interaction plots#####
#################################
#N+P
xx = na.omit(subset(d.gpp, select = c(N,P,chla)))
interaction.plot(xx$N, xx$P, xx$chla)

#N+Si
xx = na.omit(subset(d.gpp, select = c(N,Si,chla)))
interaction.plot(xx$N, xx$Si, xx$chla)

#P+Si
xx = na.omit(subset(d.gpp, select = c(Si,P,chla)))
interaction.plot(xx$Si, xx$P, xx$chla)

##########################################################
#Analyze chl-a by removing all Si treatments
##########################################################
d.gppNoSi = subset(d.gpp, Si==0)

M1<-gls(chla~N*P, data=d.gppNoSi, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals look bad (p=0.0011)
hist(E1)
plot(M1)

bartlett.test(chla~nutrient, data=d.gpp)
#data look bad p<2.2e-16

#log transformation
d.gppNoSi$l.chla = log10(d.gppNoSi$chla+1)

M2<-gls(l.chla~N*P, data=d.gppNoSi, na.action=na.omit) 
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are not normal, p=0.012

hist(E2, xlab="residuals", main="")
plot(M2)
bartlett.test(l.chla~nutrient, data=d.gppNoSi)
#not good, p<2.2e16

#try cube root transformation, but need to make the values positive

d.gppNoSi$cube.chla.area = (d.gppNoSi$chla)^(1/3)

M3<-gls(cube.chla.area~N*P, data=d.gppNoSi, na.action=na.omit)
E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
#residuals are not normally distributed p = 0.04075
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
#residuals are not normally distributed p = 0.02408
hist(E4)  
plot(M4)

bartlett.test(sqr.chla.area~nutrient, data=d.gppNoSi)
#variance test p < 2.2e-16

#try non-parametric Aligned Rank Test
install.packages("ARTool")
library(ARTool)

M5 = art(chla ~  N*P, data=d.gppNoSi)

summary(M5) #supposed to be at or about 0

shapiro.test(residuals(M5))
qqnorm(residuals(M5)); qqline(residuals(M5))
anova(M5)

#Interpret Interaction
#N and P
xx = na.omit(subset(d.gppNoSi, select = c(N,P,chla)))
interaction.plot(xx$N, xx$P, xx$chla)
