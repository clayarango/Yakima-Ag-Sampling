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
aht_summer <- read.table(file="aht_summer.csv", header=T, sep=",")

#set variable
d = aht_summer

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

#check for outliers and check data entry before calculating NRR.
ggplot(d.cr, aes(x=nutrient, y=cr.area))+geom_boxplot() +theme_classic()
#one low and one high control but seem well-distributed

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) #changed to ddply b/c allows
#to specify by column name - I had a csv file with the relevant column in a different position.
x
d.cr$cr.nrr = d.cr$cr.area/-20.436057 #divide by control ave_cr

#check for outliers and check data entry before calculating NRR.
ggplot(d.gpp, aes(x=nutrient, y=gpp.area))+geom_boxplot() +theme_classic()
#one high-ish Si value checks out
ggplot(d.gpp, aes(x=nutrient, y=chla_ug_cm2))+geom_boxplot() +theme_classic()
#one high control checks out, but consider dropping. F6

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla_ug_cm2, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/3.099015 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla_ug_cm2/2.985545 #divide by control ave_chla
x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla_ug_cm2, na.rm=T)) 
x1
d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340
  
#now combine into one and export
d.cr$chla.nrr<-NA
d.cr$chla.nrr_1<-NA
d.cr$gpp.nrr<-NA
d.gpp$cr.nrr<-NA
d.nrr<-rbind(d.cr, d.gpp)
d.nrr$site.date<-"aht_summer"
d.nrr$gpp.es<-log(d.nrr$gpp.nrr)
d.nrr$cr.es<-log(d.nrr$cr.nrr)
d.nrr$chla.es<-log(d.nrr$chla.nrr)
d.nrr$chla.es_1<-log(d.nrr$chla.nrr_1)

write.table(d.nrr, "aht_summer_nrr.csv",  sep=",", quote=F, row.names =F)

###############
#plots of NRR
##############
ggplot(data=subset(d.cr, !(nutrient=="control")), aes(x=nutrient, y=cr.nrr))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#Si inhibition; others neutral or limitation

ggplot(data=subset(d.nrr, (top=="sponge")), aes(x=nutrient, y=cr.es))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+
  geom_hline(yintercept = -1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=d.gpp, aes(x=nutrient, y=gpp.nrr))+geom_boxplot()+theme_bw()+
  ylab("GPP NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#mostly inhibition

ggplot(data=subset(d.nrr, top =="glass"),aes(x=nutrient, y=gpp.es))+geom_boxplot()+theme_bw()+
  ylab("GPP Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+
  geom_hline(yintercept = -0.7, lty="dashed")+ geom_hline(yintercept = -1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=d.gpp, aes(x=nutrient, y=chla.nrr))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+ 
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=d.gpp, aes(x=nutrient, y=chla.nrr_1))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+ 
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=subset(d.nrr, (top=="glass")), aes(x=nutrient, y=chla.es))+geom_boxplot()+theme_bw()+
  ylab("Chla Effect Size")+ geom_hline(yintercept = 0.7, lty="dashed")+    
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=subset(d.nrr, (top=="glass")), aes(x=nutrient, y=chla.es_1))+geom_boxplot()+theme_bw()+
  ylab("Chla Effect Size")+ geom_hline(yintercept = 0.7, lty="dashed")+    
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
   #residuals are more-or-less normally distributed, p=0.4456. A lot of wiggle around QQ line
hist(E1) #ok
plot(M1)# a little bit heteroscedastic but not dramatically so. less variation as values increase

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
   #variance test OK

anova(M1)
  #N limitation. P and Si co-limitation (secondary co-limitation??)
#            numDF  F-value p-value
#(Intercept)     1 492.4297  <.0001
#N               1  13.0729  0.0010
#P               1   5.6370  0.0240
#Si              1   0.2001  0.6577
#N:P             1   0.0790  0.7805
#N:Si            1   1.5230  0.2264
#P:Si            1   7.3806  0.0107
#N:P:Si          1   0.0524  0.8205


##########################################################
#do multiple 2 way ANOVAs to improve our ability to interpret
##########################################################
#N and P
M1<-gls(cr.area~N*P, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.3835
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #interpretation, N and P limitation
#N               1  11.3964  0.0018
#P               1   4.9140  0.0332
#N:P             1   0.0751  0.7857

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(N,P,cr.area)))
interaction.plot(xx$N, xx$P, xx$cr.area*-1)

#N and Si
M1<-gls(cr.area~N*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.2922
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #interpretation, N limitation
#N               1  10.3180  0.0028
#Si              1   0.1158  0.7357
#N:Si            1   1.0894  0.3038

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(N,Si,cr.area)))
interaction.plot(xx$N, xx$Si, xx$cr.area*-1)

#P and Si
M1<-gls(cr.area~P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.2969
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #P is significant only in the presence of silica
#P               1   4.6804  0.0374
#Si              1   0.2266  0.6370
#P:Si            1   5.3667  0.0265

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(P,Si,cr.area)))
interaction.plot(xx$P, xx$Si, xx$cr.area*-1) #CR increases with P and Si, but not individually
##########################################################
##########################################################

ggplot(data=d.cr, aes(x=nutrient, y = cr.area))+geom_boxplot()

x <- group_by(d.cr, nutrient) %>%  # Grouping function causes subsequent functions to aggregate by treatment
  summarize(cr.mean = abs(mean(cr.area, na.rm = TRUE)), # na.rm = TRUE to remove missing values
            cr.sd=abs(sd(cr.area, na.rm = TRUE)),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(cr.area)), # of observations, excluding NAs. 
            cr.se=cr.sd/sqrt(n))


#ggsave('output/figures/Aht_summer_CRNRR.tiff',
#       units="in",
#       width=3.25,
#       height=3.25,
#       dpi=1200,
#       compression="lzw")

##########################
#analyze the CHLA data
##########################
M1<-gls(chla_ug_cm2~N*P*Si, data=subset(d.gpp,!nds.id=="F6"), na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#ok
hist(E1) #ok
plot(M1)# ok

plot(filter(d.gpp, !is.na(chla_ug_cm2)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1)
#N               1   20.5176  0.0001
#P               1    0.6151  0.4388
#Si              1    5.3094  0.0281
#N:P             1    1.7464  0.1960
#N:Si            1    1.7223  0.1990
#P:Si            1    5.1347  0.0306
#N:P:Si          1   20.1955  0.0001

##########################################################
#do multiple 2 way ANOVAs to improve our ability to interpret
##########################################################
#N and P
d.gpp1<-subset(d.gpp,!nds.id=="F6")
M1<-gls(chla_ug_cm2~N*P, data=d.gpp1, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)#ok
ad.test(E1)
#residuals are normally distributed, p=0.496
hist(E1)  #ok
plot(M1)

bartlett.test(chla_ug_cm2~nutrient, subset(d.gpp,!nds.id=="F6"))
#variance test OK

anova(M1) #interpretation: N limitation
#N               1   11.3644  0.0018
#P               1    0.3407  0.5632
#N:P             1    1.0625  0.3097


#remove NA for plotting
xx = na.omit(subset(d.gpp1, select = c(N,P,chla_ug_cm2)))
interaction.plot(xx$N, xx$P, xx$chla_ug_cm2)
#N increases chla, w or w/o P

#N and Si
M1<-gls(chla_ug_cm2~N*Si, data=d.gpp1, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)#wiggly
ad.test(E1)
#residuals are normally distributed, p=0.1983
hist(E1) #ok
plot(M1)

bartlett.test(chla_ug_cm2~nutrient, data=d.gpp1)
#variance test OK

anova(M1) #interpretation: N limitation
#N               1   12.2870  0.0013
#Si              1    3.2360  0.0807
#N:Si            1    1.1225  0.2966

#remove NA for plotting
xx = na.omit(subset(d.gpp1, select = c(N,Si,chla_ug_cm2)))
interaction.plot(xx$N, xx$Si, xx$chla_ug_cm2)

#P and Si
M1<-gls(chla_ug_cm2~P*Si, data=d.gpp1, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)#wiggly
ad.test(E1)
#residuals normally distributed, p=0.2832
hist(E1)  #ok
plot(M1)#ok

bartlett.test(chla_ug_cm2~nutrient, data=d.gpp1)
#variance OK

anova(M1) #interpretation: no response
#P               1    0.3752  0.5442
#Si              1    2.1887  0.1480
#P:Si            1    2.0973  0.1565

xx = na.omit(subset(d.gpp1, select = c(P,Si,chla_ug_cm2)))
interaction.plot(xx$P, xx$Si, xx$chla_ug_cm2)
#decline when P and Si combined, increase with P in absence of Si

############################################################
#analyze the PRODUCTION data
############################################################

M1<-gls(log10(gpp.area+1)~N*P*Si, data=d.gpp, na.action=na.omit) #had to log-transform
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
  #with log transformation: looks better, p = 0.8947)
  #w/o transformation: residuals not normal (p = 0.012, clear deviation from QQ line)

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
   #variance increases with value, even with transformation. will use kruskal-wallis

ggplot(data=d.gpp, aes(x=nutrient, y = gpp.area))+geom_boxplot()
anova(M1)#doing anova anyway
#nothing significant

kruskal.test(gpp.area~nutrient, data=d.gpp, na.action=na.omit)#kruskal-wallis can only have 1 factor.




