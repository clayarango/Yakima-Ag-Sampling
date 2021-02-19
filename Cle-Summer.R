#Author:  Clay Arango
#Creation date: 10-Feb-19
#Script to analyze NDS data from Summer 2018 WSB work with S. Roley and A. Alexiades

#packages
install.packages("nlme")
install.packages("nortest")
install.packages("plyr")
install.packages("dplyr")
install.packages("multcomp")
install.packages("MASS")
install.packages("ggplot2")
library(nlme)
library(nortest)
library(plyr)
library(dplyr)
library(multcomp)
library(MASS)
library(ggplot2)

#Load data
cle_summer <- read.table(file="cle_summer.csv", header=T, sep=",")

#set variable
d = cle_summer

#evaluate data to make sure factors are correct
names(d)

unique(d$nutrient) #control, N, NP, NPSi, NSi, P, PSi, Si
unique(d$N) #should be 0 and 1
unique(d$P) #should be 0 and 1
unique(d$Si) #should be 0 and 1
unique(d$top) #should be "sponge" and "glass"
#if need to change, use recode function below. 
#d$top<-recode(d$top, "cellulose" = "sponge")
str(d)

#convert N and P and Si values (0 or 1 for absence or presence) to factors
d$N<-as.factor(d$N)
d$P<-as.factor(d$P)
d$Si<-as.factor(d$Si)
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge", data=d)
d.gpp = subset(d, top=="glass", data=d)

#check for outliers and check data entry before calculating CR-NRR
ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#BB5, W1 data entry check out, controls look OK
#keep BB5 and W1

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-4.808690 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#Z2 data entry checks out
#can remove Z2 from analysis of NRR, but not affected in NRR calculation

ggplot(d.gpp, aes(x=nutrient, y=chla)) + geom_boxplot() + theme_classic()

#substitute b.d. chlorophyll with 1/2 detection limit using
#3*SD(y/x)/slope, where SD(y/x) is standard error of the chlorophyll regression
#make vectors for the x (chla) and y (RFU) values found in spreadsheet
#y = c(3.39, 397.33, 693, 1390.66, 3003.32, 5897.49)
#x = c(0, 0.025, 0.05, 0.1, 0.25, 0.5)
#summary(lm(y~x))
#chlabd = (188.47/11627.13)/2

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/2.633438 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.4318408 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#combine into one file and export
d.cr$chla.nrr<-NA
#d.cr$chla.nrr_1<-NA#for excluded outliers
d.cr$gpp.nrr<-NA
d.gpp$cr.nrr<-NA
d.nrr<-rbind(d.cr, d.gpp)
d.nrr$site.date<-"cle_summer"
d.nrr$gpp.es<-log(d.nrr$gpp.nrr)
d.nrr$cr.es<-log(d.nrr$cr.nrr)
d.nrr$chla.es<-log(d.nrr$chla.nrr)

write.table(d.nrr, "cle_summer_nrr.csv",  sep=",", quote=F, row.names =F)

###############
#plots of NRR
##############
ggplot(data=subset(d.cr, !(nutrient=="C")), aes(x=nutrient, y=cr.nrr))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#N limitation, possible Si inhibition

ggplot(data=subset(d.nrr, (top=="sponge")), aes(x=nutrient, y=cr.es))+geom_boxplot()+theme_bw()+
  ylab("CR Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+
  geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#nothing over 1.385, so no significance?

ggplot(data=subset(d.gpp, !(nutrient=="C")), aes(x=nutrient, y=gpp.nrr))+geom_boxplot()+theme_bw()+
  ylab("GPP NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#possible N limitation (but not NP), likely no limitation (i.e., light?)

ggplot(data=subset(d.nrr, top =="glass"),aes(x=nutrient, y=gpp.es))+geom_boxplot()+theme_bw()+
  ylab("GPP Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+
  geom_hline(yintercept = -0.7, lty="dashed")+ geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#nothing over 1.385 so no significance?

ggplot(data=subset(d.gpp, !(nutrient=="C")), aes(x=nutrient, y=chla.nrr))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+ 
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#N limitation, possible N+P colimitation

ggplot(data=subset(d.nrr, (top=="glass")), aes(x=nutrient, y=chla.es))+geom_boxplot()+theme_bw()+
  ylab("Chla Effect Size")+ geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+    
  geom_hline(yintercept = 1.385)+
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
d.sum$site_date<-"cle_summer"
write.table(d.sum, "cle_summer_summ.csv",  sep=",", quote=F, row.names=F)

############################################################
#analyze RESPIRATION data
############################################################

M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals are not normally distributed p = 0.02542
hist(E1)  
plot(M1)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
   #variance test 0.02909, we'll probably need to mess with this one

anova(M1)
 
d.cr$l.cr.area = log10(d.cr$cr.area*-1)
 
M2<-gls(l.cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are not normally distributed p = 0.01256
hist(E2)  
plot(M2)

plot(filter(d.cr, !is.na(l.cr.area)) %>% dplyr::select(nutrient), 
     E2, xlab="nutrient", ylab="Residuals")
bartlett.test(l.cr.area~nutrient, data=d.cr)
#variance test 0.01279, we'll probably need to mess with this one

anova(M2)

#How to normalize leptokurtic distribution?
#try cube root transformation, but need to make the values positive

d.cr$cube.cr.area = (d.cr$cr.area*-1)^(1/3)

M3<-gls(cube.cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
#residuals are not normally distributed p = 0.03379
hist(E3)  
plot(M3)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(cube.cr.area~nutrient, data=d.cr)
#variance test 0.05274

anova(M3)

x <- group_by(d.cr, nutrient) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(cr.mean = abs(mean(cube.cr.area, na.rm = TRUE)), # na.rm = TRUE to remove missing values
            cr.sd=abs(sd(cube.cr.area, na.rm = TRUE)),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(cube.cr.area)), # of observations, excluding NAs. 
            cr.se=cr.sd/sqrt(n))

ggplot(data=x, aes(x=nutrient, y=cr.mean)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=cr.mean, ymax=cr.mean+cr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Nutrient") +
  ylab(expression(Respiration~(ug~O[2]~m^{-2}~h^{-1}))) +
  #ylim(0,20) +
  labs(fill="Light") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=6),
        legend.key=element_blank(), 
        legend.position=c(0.5,0.95), 
        legend.text=element_text(size=8), 
        legend.background=element_blank(), 
        legend.direction="horizontal", 
        legend.key.size=unit(0.3, "cm"), 
        axis.title.y=element_text(size=8), 
        axis.title.x=element_text(size=8), 
        axis.text.x=element_text(size=8))

#ggsave('output/figures/Ring_summer.tiff',
#       units="in",
#       width=3.25,
#       height=3.25,
#       dpi=1200,
#       compression="lzw")

#summarize nrr
x <- group_by(d.cr, nutrient) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(cr.mean = abs(mean(cr.nrr, na.rm = TRUE)), # na.rm = TRUE to remove missing values
            cr.sd=abs(sd(cr.nrr, na.rm = TRUE)),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(cr.nrr)), # of observations, excluding NAs. 
            cr.se=cr.sd/sqrt(n))

############################################################
#analyze the PRODUCTION data
############################################################

M1<-gls(gpp.area~N*P*Si, data=d.gpp, na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals are not normal, remove outlier Z2?

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
   #not good, check outlier

anova(M1)

#remove Z2
d.gpp.x1 = subset(d.gpp, !(nds.id=="Z2"))

M1<-gls(gpp.area~N*P*Si, data=d.gpp.x1, na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normal, p=0.0791

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
#not good, 0.0002363

anova(M1)

#log transformation
d.gpp.x1$l.gpp.area = log10(d.gpp.x1$gpp.area+1)
  
M2<-gls(l.gpp.area~N*P*Si, data=d.gpp.x1, na.action=na.omit) 
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are normal, p=0.2161

hist(E2, xlab="residuals", main="")
plot(M2)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(l.gpp.area~nutrient, data=d.gpp.x1)
#not good, 0.01833

anova(M2)

x <- group_by(d.gpp, nutrient) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gpp.mean = abs(mean(gpp.area, na.rm = TRUE)), # na.rm = TRUE to remove missing values
            gpp.sd=abs(sd(gpp.area, na.rm = TRUE)),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gpp.area)), # of observations, excluding NAs. 
            gpp.se=gpp.sd/sqrt(n))

ggplot(data=x, aes(x=nutrient, y=gpp.mean)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=gpp.mean, ymax=gpp.mean+gpp.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Nutrient") +
  ylab(expression(Production~(ug~O[2]~m^{-2}~h^{-1}))) +
  labs(fill="Light") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(), 
        legend.title=element_text(size=6), 
        legend.key=element_blank(),  
        legend.position=c(0.5,0.95),  
        legend.text=element_text(size=8),  
        legend.background=element_blank(),  
        legend.direction="horizontal",  
        legend.key.size=unit(0.3, "cm"),  
        axis.title.y=element_text(size=8),  
        axis.title.x=element_text(size=8),  
        axis.text.x=element_text(size=8))

#ggsave('output/figures/gppByNutrient.tiff',
#       units="in",
#       width=3.25,
#       height=3.25,
#       dpi=1200,
#       compression="lzw")


#summarize nrr
x <- group_by(d.gpp, nutrient) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gpp.mean = abs(mean(gpp.nrr, na.rm = TRUE)), # na.rm = TRUE to remove missing values
            gpp.sd=abs(sd(gpp.nrr, na.rm = TRUE)),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gpp.nrr)), # of observations, excluding NAs. 
            gpp.se=gpp.sd/sqrt(n))

############################################################
#analyze the CHL-A biomass on disks

M1<-gls(chla~N*P*Si, data=d.gpp, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
  #residuals look bad (p=2.65e-8)
hist(E1)
plot(M1)

bartlett.test(chla~nutrient, data=d.gpp)
#data look bad p=6.335e-12

#log transformation
d.gpp$l.chla = log10(d.gpp$chla+1)

M2<-gls(l.chla~N*P*Si, data=d.gpp, na.action=na.omit) 
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are not normal, p=4.758e-5

hist(E2, xlab="residuals", main="")
plot(M2)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(l.chla~nutrient, data=d.gpp)
#not good, 3.008e-6

anova(M2)
