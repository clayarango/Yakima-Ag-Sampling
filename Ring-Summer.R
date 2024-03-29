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
ring_summer <- read.table(file="ring_summer.csv", header=T, sep=",")

#set variable
d = ring_summer

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
#looks OK
#R5 lost in the field

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-7.554539 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#K8 data entry checks out, can stay in analysis
#K1 lost in the field
ggplot(d.gpp, aes(x=nutrient, y=chla)) + geom_boxplot() + theme_classic()
#S1, L7, N2 data entry checks out
#N2 could be excluded from analysis, but it won't affect NRR calculation
#K1 lost in the field

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/6.422422 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/1.824141 #divide by control ave_chla
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
d.nrr$site.date<-"ring_summer"
d.nrr$gpp.es<-log(d.nrr$gpp.nrr)
d.nrr$cr.es<-log(d.nrr$cr.nrr)
d.nrr$chla.es<-log(d.nrr$chla.nrr)

write.table(d.nrr, "ring_summer_nrr.csv",  sep=",", quote=F, row.names =F)

###############
#plots of NRR
##############
ggplot(data=subset(d.cr, !(nutrient=="control")), aes(x=nutrient, y=cr.nrr))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#N limitation, possible P secondary limitation

ggplot(data=subset(d.nrr, (top=="sponge")), aes(x=nutrient, y=cr.es))+geom_boxplot()+theme_bw()+
  ylab("CR Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+
  geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#nothing over 1.385, so no significance?

ggplot(data=subset(d.gpp, !(nutrient=="control")), aes(x=nutrient, y=gpp.nrr))+geom_boxplot()+theme_bw()+
  ylab("GPP NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#possible N limitation (but not NP), likely no limitation

ggplot(data=subset(d.nrr, top =="glass"),aes(x=nutrient, y=gpp.es))+geom_boxplot()+theme_bw()+
  ylab("GPP Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+
  geom_hline(yintercept = -0.7, lty="dashed")+ geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#nothing over 1.385 so no significance?

ggplot(data=subset(d.gpp, !(nutrient=="control")), aes(x=nutrient, y=chla.nrr))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+ 
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#N limitation (but why not N+P?)

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
d.sum$site_date<-"ring_summer"
write.table(d.sum, "ring_summer_summ.csv",  sep=",", quote=F, row.names=F)

############################################################
#analyze RESPIRATION data
############################################################

M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals are normally distributed, p=0.241
hist(E1)  
plot(M1)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
   #variance test p=0.1614

anova(M1)


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
#residuals are normally distributed p = 0.12
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.crNoSi)
#variance test 0.05

anova(M1)

##########################################################
#do 2 way ANOVAs to interpret
##########################################################
#N and P
xx = na.omit(subset(d.cr, select = c(N,P,cr.area)))
interaction.plot(xx$N, xx$P, xx$cr.area*-1)

#N limitation
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
   #residuals are not normal, p=0.0403

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
   #variances are equal, p=0.3016

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


##########################################################
#analyze the CHL-A biomass on disks

M1<-gls(chla~N*P*Si, data=d.gpp, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are not normal, p=0.005993

hist(E1)  
plot(M1)

plot(filter(d.gpp, !is.na(chla)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(chla~nutrient, data=d.gpp)
#variance test p=0.0006267

#try normalizing

d.gpp$l.chla = log10(d.gpp$chla+1)

M2<-gls(l.chla~N*P*Si, data=d.gpp, na.action=na.omit)
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are normal, p=0.07118

hist(E2)  
plot(M2)

plot(filter(d.gpp, !is.na(chla)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(l.chla~nutrient, data=d.gpp)
#variance test is not good p=0.007904

#try cube root transformation

d.gpp$cube.chla.area = (d.gpp$chla)^(1/3)

M3<-gls(cube.chla.area~N*P*Si, data=d.gpp, na.action=na.omit)
E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
#residuals are normally distributed p = 0.07603
hist(E3)  
plot(M3)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(cube.chla.area~nutrient, data=d.gpp)
#variance test p = 0.0077

#try sqr root
d.gpp$sqr.chla.area = (d.gpp$chla)^(1/2)

M4<-gls(sqr.chla.area~N*P*Si, data=d.gpp, na.action=na.omit)
E4<-residuals(M4)
qqnorm(E4)
qqline(E4)
ad.test(E4)
#residuals are normally distributed p = 0.05187
hist(E4)  
plot(M4)

plot(filter(d.cr, !is.na(cube.cr.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(sqr.chla.area~nutrient, data=d.gpp)
#variance test p = 0.005511

#try non-parametric Aligned Rank Test
install.packages("ARTool")
library(ARTool)

M5 = art(chla ~  N*P*Si, data=d.gpp)

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

#N and Si
xx = na.omit(subset(d.gpp, select = c(N,Si,chla)))
interaction.plot(xx$N, xx$Si, xx$chla)

interaction.plot(d.cr$N, d.cr$Si, d.cr$cr.area)

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
#residuals look good (p=0.1494)
hist(E1)
plot(M1)

bartlett.test(chla~nutrient, data=d.gpp)
#data look bad p=0.0006

#log transformation
d.gppNoSi$l.chla = log10(d.gppNoSi$chla+1)

M2<-gls(l.chla~N*P, data=d.gppNoSi, na.action=na.omit) 
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are not normal, p=0.50

hist(E2, xlab="residuals", main="")
plot(M2)
bartlett.test(l.chla~nutrient, data=d.gppNoSi)
#not good, p = 0.034

#try cube root transformation

d.gppNoSi$cube.chla.area = (d.gppNoSi$chla)^(1/3)

M3<-gls(cube.chla.area~N*P, data=d.gppNoSi, na.action=na.omit)
E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
#residuals are normally distributed p = 0.521
hist(E3)  
plot(M3)

bartlett.test(cube.chla.area~nutrient, data=d.gppNoSi)
#variance test p 0.03354

#try sqr root
d.gppNoSi$sqr.chla.area = (d.gppNoSi$chla)^(1/2)

M4<-gls(sqr.chla.area~N*P, data=d.gppNoSi, na.action=na.omit)
E4<-residuals(M4)
qqnorm(E4)
qqline(E4)
ad.test(E4)
#residuals are normally distributed p = 0.365
hist(E4)  
plot(M4)

bartlett.test(sqr.chla.area~nutrient, data=d.gppNoSi)
#variance test p = 0.02622

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
#N limitation, P suppression
