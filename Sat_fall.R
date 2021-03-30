#Author:  Clay Arango
#Creation date: 14-Feb-19
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
sat_fall <- read.table(file="satus_fall.csv", header=T, sep=",")

#set variable
d = sat_fall

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

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) #changed to ddply b/c allows
#to specify by column name - I had a csv file with the relevant column in a different position.
x
d.cr$cr.nrr = d.cr$cr.area/-17.11723 #divide by control ave_cr

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla_ug_cm2, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/4.345200 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla_ug_cm2/0.1823788 #divide by control ave_chla

#now combine into one and export
d.cr$chla.nrr<-NA
d.cr$gpp.nrr<-NA
d.gpp$cr.nrr<-NA
d.nrr<-rbind(d.cr, d.gpp)
d.nrr$site.date<-"satus_fall"
d.nrr$cr.es<-log(d.nrr$cr.nrr)
d.nrr$gpp.es<-log(d.nrr$gpp.nrr)
d.nrr$chla.es<-log(d.nrr$chla.nrr)
write.table(d.nrr, "satus_fall_nrr.csv",  sep=",", quote=F, row.names =F)

###############
#plots of NRR
##############
ggplot(data=subset(d.cr, !(nutrient=="control")), aes(x=nutrient, y=cr.nrr))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#no response

ggplot(data=subset(d.nrr, (top=="sponge")), aes(x=nutrient, y=cr.es))+geom_boxplot()+theme_bw()+
  ylab("CR Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=subset(d.gpp, !(nutrient=="control")), aes(x=nutrient, y=gpp.nrr))+geom_boxplot()+theme_bw()+
  ylab("GPP NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#mostly neutral. 

ggplot(data=subset(d.nrr,top=="glass"), aes(x=nutrient, y=gpp.es))+geom_boxplot()+theme_bw()+
  ylab("GPP Effect Size")+geom_hline(yintercept = -0.7, lty="dashed")+geom_hline(yintercept = 0.7, lty="dashed")+
  geom_hline(yintercept = -1.385)+geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=subset(d.gpp, !(nutrient=="control")), aes(x=nutrient, y=chla.nrr))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+ 
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#limitation or neutral. A few high values which check out as correct.

ggplot(data=subset(d.nrr, (top=="glass")), aes(x=nutrient, y=chla.es))+geom_boxplot()+theme_bw()+
  ylab("Chla Effect Size")+ geom_hline(yintercept = 0.7, lty="dashed")+  geom_hline(yintercept = -1.385)+  
  geom_hline(yintercept = -0.7, lty="dashed")+ geom_hline(yintercept = 1.385)+
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
d.sum$site_date<-"aht_summer"
write.table(d.sum, "reec_summer_summ.csv",  sep=",", quote=F, row.names =F)

############################################################
#analyze RESPIRATION data
############################################################

M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals are more-or-less normally distributed, p=0.1239. A lot of wiggle around QQ line
hist(E1) #ok
plot(M1)# a little bit heteroscedastic but not dramatically so. less variation as values increase

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
   #variance test OK p = 0.1883

anova(M1)
  #P:Si interaction significant. in figure, Si appears higher, NP lower, all others equal to Control
#P:Si            1    8.8278  0.0056
##########################################################
#do multiple 2 way ANOVAs to improve our ability to interpret
##########################################################
#N and P
xx = na.omit(subset(d.cr, select = c(N,P,cr.area)))
interaction.plot(xx$N, xx$P, xx$cr.area)

#N and Si
xx = na.omit(subset(d.cr, select = c(N,Si,cr.area)))
interaction.plot(xx$N, xx$Si, xx$cr.area)

#P and Si
xx = na.omit(subset(d.cr, select = c(P,Si,cr.area)))
interaction.plot(xx$P, xx$Si, xx$cr.area*-1)
#plot is an X (CR declines in presence of P alone and Si alone, but neutral with Si and P)

############################################################
#analyze the PRODUCTION data
############################################################

M1<-gls(gpp.area~N*P*Si, data=d.gpp, na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
  #ok

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
   #ok

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
############################################################
#analyze the CHL-A biomass on disks
############################################################
M1<-gls(chla_ug_cm2~N*P*Si, data=d.gpp, na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)#a bit wiggly
ad.test(E1)
#ok p = 0.1737

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(chla_ug_cm2)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
#ok p = 0.72

anova(M1) #no limitation

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
