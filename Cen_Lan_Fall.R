#Author:  Clay Arango
#Creation date: 10-Feb-19
#Script to analyze NDS data from Summer 2018 NDS work with S. Roley and A. Alexiades

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
cen_lan_fall <- read.table(file="cen_lan_fall.csv", header=T, sep=",")

#set variable
d = cen_lan_fall

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

#check for outliers and check data entry before calculating CR-NRR
ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#N+Si group is spread out but centered

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-9.172439 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#way too many zeros in this data set
ggplot(d.gpp, aes(x=nutrient, y=chla_ug_cm2)) + geom_boxplot() + theme_classic()
#no outliers

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla_ug_cm2, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/0.06366686 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla_ug_cm2/3.143975 #divide by control ave_chla
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
d.nrr$site.date<-"cen_lan_fall"
d.nrr$gpp.es<-log(d.nrr$gpp.nrr)
d.nrr$cr.es<-log(d.nrr$cr.nrr)
d.nrr$chla.es<-log(d.nrr$chla.nrr)

write.table(d.nrr, "cen_lan_fall_nrr.csv",  sep=",", quote=F, row.names =F)

###############
#plots of NRR
##############
ggplot(data=subset(d.cr, !(nutrient=="control")), aes(x=nutrient, y=cr.nrr))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#maybe N+P+SI colimitation? probably no limitation

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
#too few replicates to be meaningful

ggplot(data=subset(d.gpp, !(nutrient=="control")), aes(x=nutrient, y=chla.nrr))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+ 
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#no limitation

ggplot(data=subset(d.nrr, (top=="glass")), aes(x=nutrient, y=chla.es))+geom_boxplot()+theme_bw()+
  ylab("Chla Effect Size")+ geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+    
  geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#nothing over 1.385 so no significance, possible P and or Si limitation?

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
d.sum$site_date<-"cen_lan_fall"
write.table(d.sum, "cen_lan_fall_summ.csv",  sep=",", quote=F, row.names=F)

############################################################
#analyze RESPIRATION data
############################################################

M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals p = 0.6784
hist(E1)  
plot(M1)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
   #variance test p = 0.4546

anova(M1)
#numDF   F-value p-value
#(Intercept)     1 1238.9517  <.0001
#N               1    0.1641  0.6881
#P               1    1.2067  0.2802
#Si              1    1.1268  0.2964
#N:P             1    7.3144  0.0109
#N:Si            1    0.0672  0.7972
#P:Si            1    0.3027  0.5860
#N:P:Si          1    1.3141  0.2602

#Results suggest NxP interaction (co-limitation), but this appears to only occur when Si present.
#Does the NPSi treatment just increase N and P replication? Or is there something about Si?
  

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
#residuals are normally distributed p = 0.05338
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.crNoSi)
#variance test 0.16

anova(M1)

##########################################################
#do 2 way ANOVAs to interpret
##########################################################
#N and P
xx = na.omit(subset(d.cr, select = c(N,P,cr.area)))
interaction.plot(xx$N, xx$P, xx$cr.area*-1)

#No limitation
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
   #residuals are not normal

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
   #variance test is bad

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

M1<-gls(chla_ug_cm2~N*P*Si, data=d.gpp, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
  #residuals look OK (p = 0.4738)
hist(E1)
plot(M1)

bartlett.test(chla_ug_cm2~nutrient, data=d.gpp)
  #variance is OK p = 0.5385

anova(M1)

##########################################################
#do multiple 2 way ANOVAs to improve our ability to interpret
##########################################################
#N and P
xx = na.omit(subset(d.gpp, select = c(N,P,chla_ug_cm2)))
interaction.plot(xx$N, xx$P, xx$chla_ug_cm2)

#N and Si
xx = na.omit(subset(d.gpp, select = c(N,Si,chla_ug_cm2)))
interaction.plot(xx$N, xx$Si, xx$chla_ug_cm2)

#P and Si
xx = na.omit(subset(d.gpp, select = c(P,Si,chla_ug_cm2)))
interaction.plot(xx$P, xx$Si, xx$chla_ug_cm2)


##########################################################
#Analyze chl-a by removing all Si treatments
##########################################################
d.gppNoSi = subset(d.gpp, Si==0)

M1<-gls(chla_ug_cm2~N*P, data=d.gppNoSi, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals look good (p=0.9029)
hist(E1)
plot(M1)

bartlett.test(chla_ug_cm2~nutrient, data=d.gpp)
#data look good p=0.5386

anova(M1)

#Interpret Interaction
#N and P
xx = na.omit(subset(d.gppNoSi, select = c(N,P,chla_ug_cm2)))
interaction.plot(xx$N, xx$P, xx$chla_ug_cm2)

#No limitation