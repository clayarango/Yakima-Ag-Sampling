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
ring_fall <- read.table(file="ring_fall.csv", header=T, sep=",")

#set variable
d = ring_fall

#evaluate data to make sure factors are correct
names(d)

unique(d$nutrient) #control, N, NP, NPSi, NSi, P, PSi, Si
unique(d$N) #should be 0 and 1
unique(d$P) #should be 0 and 1
unique(d$Si) #should be 0 and 1
unique(d$top) #should be "sponge" and "glass"
#if need to change, use recode function below. 
#d$top<-recode(d$top, "cellulose" ="sponge")
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
#P3, O2, T7, K5, L7 are entered correctly, controls look OK
#K3, M1, N8, R6, missing in field

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-11.397616 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#K2 is entered correctly
#K1, M7 missing in field
ggplot(d.gpp, aes(x=nutrient, y=chla)) + geom_boxplot() + theme_classic()
#P and Si groups are somewhat spread but somewhat centered

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/3.232171 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.4619133 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla_ug_cm2, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#combine into one file and export
d.cr$chla.nrr<-NA
#d.cr$chla.nrr_1<-NA#for excluded outliers
d.cr$gpp.nrr<-NA
d.gpp$cr.nrr<-NA
d.nrr<-rbind(d.cr, d.gpp)
d.nrr$site.date<-"ring_fall"
d.nrr$gpp.es<-log(d.nrr$gpp.nrr)
d.nrr$cr.es<-log(d.nrr$cr.nrr)
d.nrr$chla.es<-log(d.nrr$chla.nrr)

write.table(d.nrr, "ring_fall_nrr.csv",  sep=",", quote=F, row.names =F)

###############
#plots of NRR
##############
ggplot(data=subset(d.cr, !(nutrient=="control")), aes(x=nutrient, y=cr.nrr))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#possible NPSi colimitation, but likely not limited

ggplot(data=subset(d.nrr, (top=="sponge")), aes(x=nutrient, y=cr.es))+geom_boxplot()+theme_bw()+
  ylab("CR Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+
  geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#nothing over 1.385, so no significance?

ggplot(data=subset(d.gpp, !(nutrient=="control")), aes(x=nutrient, y=gpp.nrr))+geom_boxplot()+theme_bw()+
  ylab("GPP NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#no limitation

ggplot(data=subset(d.nrr, top =="glass"),aes(x=nutrient, y=gpp.es))+geom_boxplot()+theme_bw()+
  ylab("GPP Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+
  geom_hline(yintercept = -0.7, lty="dashed")+ geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#nothing over 1.385 so no significance?

ggplot(data=subset(d.gpp, !(nutrient=="control")), aes(x=nutrient, y=chla.nrr))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+ 
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#possible N limitation, but likely not limited

ggplot(data=subset(d.nrr, (top=="glass")), aes(x=nutrient, y=chla.es))+geom_boxplot()+theme_bw()+
  ylab("Chla Effect Size")+ geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+    
  geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#nothing over 1.385 so no significance?

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
d.sum$site_date<-"ring_fall"
write.table(d.sum, "ring_fall_summ.csv",  sep=",", quote=F, row.names=F)

############################################################
#analyze RESPIRATION data
############################################################

M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals are not normal, p=0.01806
hist(E1)  
plot(M1)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
   #variance test is ok, p=0.212

#try normalizing
d.cr$l.cr.area = log10(d.cr$cr.area*-1)

M2<-gls(l.cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are not normal, p=0.0002844
hist(E2)  
plot(M2)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(l.cr.area~nutrient, data=d.cr)
#variance test is not equal, p=0.002413

#try cube root transformation, but need to make the values positive

d.cr$cube.cr.area = (d.cr$cr.area*-1)^(1/3)

M3<-gls(cube.cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
#residuals are not normally distributed p = 0.002936
hist(E3)  
plot(M3)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(cube.cr.area~nutrient, data=d.cr)
#variance test 0.02264

anova(M3)

#try square root transformation

d.cr$sqr.cr.area = (d.cr$cr.area*-1)^(1/2)

M4<-gls(sqr.cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E4<-residuals(M3)
qqnorm(E4)
qqline(E4)
ad.test(E4)
#residuals are not normally distributed p = 0.02939
hist(E4)  
plot(M4)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(sqr.cr.area~nutrient, data=d.cr)
#variance test 0.04937

anova(M4)

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
##########################################################
#Analyze CR by removing all Si treatments
##########################################################
d.crNoSi = subset(d.cr, Si==0)

M1<-gls(cr.area~N*P, data=d.crNoSi, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are not normally distributed p = 0.011
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.crNoSi)
#variance test 0.13

d.crNoSi$l.cr.area = log10(d.crNoSi$cr.area*-1)

M2<-gls(l.cr.area~N*P, data=d.crNoSi, na.action=na.omit)
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are not normally distributed p = 0.015
hist(E2)  
plot(M2)

bartlett.test(l.cr.area~nutrient, data=d.crNoSi)
#variance test 0.016

#try cube root transformation, but need to make the values positive

d.crNoSi$cube.cr.area = (d.crNoSi$cr.area*-1)^(1/3)

M3<-gls(cube.cr.area~N*P, data=d.crNoSi, na.action=na.omit)
E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
#residuals are not normally distributed p = 0.017
hist(E3)  
plot(M3)

bartlett.test(cube.cr.area~nutrient, data=d.crNoSi)
#variance test 0.039

d.crNoSi$sqr.cr.area = (d.crNoSi$cr.area*-1)^(1/2)

M4<-gls(sqr.cr.area~N*P, data=d.crNoSi, na.action=na.omit)
E4<-residuals(M3)
qqnorm(E4)
qqline(E4)
ad.test(E4)
#residuals are not normally distributed p = 0.017
hist(E4)  
plot(M4)

bartlett.test(sqr.cr.area~nutrient, data=d.crNoSi)
#variance test 0.056

#try non-parametric Aligned Rank Test
install.packages("ARTool")
library(ARTool)

d.crNoSi = na.omit(subset(d.crNoSi, select = c(N,P,cr.area)))

M5 = art(cr.area ~  N*P, data=d.crNoSi)

summary(M5) #supposed to be at or about 0

shapiro.test(residuals(M5))
qqnorm(residuals(M5)); qqline(residuals(M5))
anova(M5)
anova(M5, type = 3)#type 3 is better for interaction terms 

##########################################################
#do 2 way ANOVAs to interpret
##########################################################
#N and P
xx = na.omit(subset(d.cr, select = c(N,P,cr.area)))
interaction.plot(xx$N, xx$P, xx$cr.area*-1)

#no limitation
##########################################################
x <- group_by(d.cr, nutrient) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(cr.mean = abs(mean(cr.area, na.rm = TRUE)), # na.rm = TRUE to remove missing values
            cr.sd=abs(sd(cr.area, na.rm = TRUE)),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(cr.area)), # of observations, excluding NAs. 
            cr.se=cr.sd/sqrt(n))

ggplot(data=x, aes(x=nutrient, y=cr.mean)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=cr.mean, ymax=cr.mean+cr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  xlab("Nutrient") +
  ylab(expression(Respiration~(ug~O[2]~m^{-2}~h^{-1}))) +
  ylim(0,20) +
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

#ggsave('output/figures/Ring_fall.tiff',
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
   #residuals are normal, p=0.4272

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
   #variances are equal, p=0.09051

anova(M1)
  
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
#residuals are normal, p=0.7195

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(chla~nutrient, data=d.gpp)
#variances are equal, p=0.6106

anova(M1)

##########################################################
#do multiple 2 way ANOVAs to improve our ability to interpret
##########################################################
#N and P
xx = na.omit(subset(d.gpp, select = c(N,P,chla)))
interaction.plot(xx$N, xx$P, xx$chla)

#N and Si
xx = na.omit(subset(d.gpp, select = c(N,Si,chla)))
interaction.plot(xx$N, xx$Si, xx$chla)

#P and Si
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
#residuals look good (p=0.6053)
hist(E1)
plot(M1)

bartlett.test(chla~nutrient, data=d.gpp)
#data look good p=0.6106
anova(M1)

#Interpret Interaction
#N and P
xx = na.omit(subset(d.gppNoSi, select = c(N,P,chla)))
interaction.plot(xx$N, xx$P, xx$chla)

