#Author:  Clay Arango
#Creation date: 10-Feb-19
#Script to analyze NDS data from Summer 2018 NDS work with S. Roley and A. Alexiades

#packages
library(nlme)
library(nortest)
library(plyr)
library(dplyr)
library(multcomp)
library(MASS)
library(ggplot2)
library(tidyr)

#Load data
aht_fall <- read.table(file="aht_fall.csv", header=T, sep=",")

#set variable
d = aht_fall

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

#check for outliers and check data entry before calculating NRR.
ggplot(d.cr, aes(x=nutrient, y=cr.area))+geom_boxplot() +theme_classic()
#one high NSi checks out, one low Si checks out

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-16.93973 #divide by control ave_cr
d.cr$chla.nrr<-NA
d.cr$gpp.nrr<-NA

#check for outliers and check data entry before calculating NRR.
ggplot(d.gpp, aes(x=nutrient, y=gpp.area))+geom_boxplot() +theme_classic()
#ok, but huge range in controls (but appear to have a normal distn - no outliers to remove)
ggplot(d.gpp, aes(x=nutrient, y=chla))+geom_boxplot() +theme_classic()
#one low control value checks out. is an outlier, but not sure if should be removed. will calculate both.

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/6.501087 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/1.4299814 #divide by control ave_chla
d.gpp$cr.nrr<-NA
x1<- ddply(subset(d.gpp,!(nds.id=="B5")), "nutrient", summarise,  ave_chla = mean(chla, na.rm=T)) 
x1
d.gpp$chla.nrr_1 = d.gpp$chla/1.7693330
d.cr$chla.nrr_1<-NA

#combine files and export
d.nrr<-rbind(d.cr, d.gpp)
d.nrr$cr.es<-log(d.nrr$cr.nrr)
d.nrr$gpp.es<-log(d.nrr$gpp.nrr)
d.nrr$chla.es<-log(d.nrr$chla.nrr)
d.nrr$chla.es_1<-log(d.nrr$chla.nrr_1)
d.nrr$site.date<-"aht_fall"
write.table(d.nrr, "aht_fall_nrr.csv", sep=",", quote=F, row.names =F)

###############
#plots of NRR
##############
ggplot(data=subset(d.cr, !(nutrient=="control")), aes(x=nutrient, y=cr.nrr))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#inhibition or neutral

ggplot(data=subset(d.nrr, (top=="sponge")), aes(x=nutrient, y=cr.es))+geom_boxplot()+theme_bw()+
  ylab("CR Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+
  geom_hline(yintercept = -1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=d.gpp, aes(x=nutrient, y=gpp.nrr))+geom_boxplot()+theme_bw()+
  ylab("GPP NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#neutral or inhibited

ggplot(data=subset(d.nrr,top=="glass"), aes(x=nutrient, y=gpp.es))+geom_boxplot()+theme_bw()+
  ylab("GPP Effect Size")+geom_abline(slope = 0, intercept = 1)+geom_hline(yintercept = -0.7, lty="dashed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=d.gpp, aes(x=nutrient, y=chla.nrr))+geom_boxplot()+theme_bw()+ggtitle ("All Data")+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+scale_y_continuous(limits=c(0, 2))+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#inhibition

ggplot(data=subset(d.gpp, !(nds.id == "B5")),aes(x=nutrient, y=chla.nrr_1))+geom_boxplot()+theme_bw()+ggtitle ("Outlier Removed")+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+scale_y_continuous(limits=c(0, 2))+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=subset(d.nrr, (top=="glass")), aes(x=nutrient, y=chla.es))+geom_boxplot()+theme_bw()+
  ylab("Chla Effect Size")+ geom_hline(yintercept = 0.7, lty="dashed")+  geom_hline(yintercept = -1.385)+  
  ggtitle ("All Data")+geom_hline(yintercept = -0.7, lty="dashed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=subset(d.nrr, (top=="glass") & !(nds.id=="B5")), aes(x=nutrient, y=chla.es_1))+geom_boxplot()+theme_bw()+
  ylab("Chla Effect Size")+ geom_hline(yintercept = 0.7, lty="dashed")+  geom_hline(yintercept = -1.385)+  
  ggtitle ("Outlier Removed")+geom_hline(yintercept = -0.7, lty="dashed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

##########
#NRR Summary Files
#########
#no need to make these now; wait until all NRR files collated and then do a summary! But leaving code here because can use
#as template for data all together.
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
d.sum$site_date<-"aht_fall"
write.table(d.sum, "aht_fall_summ.csv",  sep=",", quote=F, row.names =F)


############################################################
#analyze RESPIRATION data
############################################################

M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals are normally distributed
hist(E1)  
plot(M1)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
   #variance test is ok

anova(M1)  #N inhibition
#(Intercept)     1 672.5216  <.0001
#N               1   4.8667  0.0347
#P               1   1.6032  0.2146
#Si              1   2.9798  0.0940
#N:P             1   3.4407  0.0728
#N:Si            1   1.2128  0.2790
#P:Si            1   2.5467  0.1204
#N:P:Si          1   2.8114  0.1033

##########################################################
#do multiple 2 way ANOVAs to improve our ability to interpret
##########################################################
#N and P
M1<-gls(cr.area~N*P, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.1159
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #interpretation: N suppression
#N               1   4.2165  0.0474
#P               1   1.3891  0.2463
#N:P             1   2.9810  0.0928

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(N,P,cr.area)))
interaction.plot(xx$N, xx$P, xx$cr.area*-1)

#N and Si
M1<-gls(cr.area~N*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.06525
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #interpretation, no limitation (N suppression)
#N               1   4.1319  0.0495
#Si              1   2.5299  0.1204
#N:Si            1   1.0297  0.3170

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(N,Si,cr.area)))
interaction.plot(xx$N, xx$Si, xx$cr.area*-1)
#decline in presence of N, but more dramatic decline with N alone (Si tempers a bit)

#P and Si
M1<-gls(cr.area~P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.2388
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #interpretation, no limitation
#P               1   1.3019  0.2614
#Si              1   2.4198  0.1286
#P:Si            1   2.0681  0.1590

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(P,Si,cr.area)))
interaction.plot(xx$P, xx$Si, xx$cr.area*-1)
#looks like P limitation, but NS
##########################################################
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

############################################################
#analyze the PRODUCTION data
############################################################

M1<-gls(gpp.area~N*P*Si, data=d.gpp, na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals ok

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
   #OK

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


############################################################
#analyze the CHL-A biomass on disks
###########################################################
d.gpp1<-subset(d.gpp,!(nds.id=="B5"))
M1<-gls(chla.nrr_1~N*P*Si, data=d.gpp1, na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals ok

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
#OK

anova(M1)# interpretation: Si, N serial inhibition; P, Si serial inhibition
#N               1  17.54615  0.0002
#P               1  10.03378  0.0036
#Si              1  21.31226  0.0001
#N:P             1   0.05818  0.8111
#N:Si            1   5.32462  0.0284
#P:Si            1   7.69659  0.0096
#N:P:Si          1   1.46880  0.2353

##########################################################
#do multiple 2 way ANOVAs to improve our ability to interpret
##########################################################
#N and P
d.gpp1<-subset(d.gpp,!nds.id=="B5")
M1<-gls(chla~N*P, data=d.gpp1, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)#ok
ad.test(E1)
#residuals are normally distributed, p=0.7595
hist(E1)  #ok
plot(M1)

bartlett.test(chla~nutrient, data=d.gpp1)
#variance test OK

anova(M1) #interpretation: N and P serial inhibition
#N               1  8.92895  0.0053
#P               1  5.10603  0.0306
#N:P             1  0.00644  0.9365


#remove NA for plotting
xx = na.omit(subset(d.gpp1, select = c(N,P,chla)))
interaction.plot(xx$N, xx$P, xx$chla)
#N inhibition, P inhibition

#N and Si
M1<-gls(chla~N*Si, data=d.gpp1, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)#wiggly
ad.test(E1)
#residuals are normally distributed, p=0.3379
hist(E1) #ok
plot(M1)

bartlett.test(chla~nutrient, data=d.gpp1)
#variance test OK

anova(M1) #interpretation: N inhibition, Si inhibition
#N               1 12.11584  0.0014
#Si              1 15.28690  0.0004
#N:Si            1  3.42854  0.0730

#remove NA for plotting
xx = na.omit(subset(d.gpp1, select = c(N,Si,chla)))
interaction.plot(xx$N, xx$Si, xx$chla)

#P and Si
M1<-gls(chla~P*Si, data=d.gpp1, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)#wiggly
ad.test(E1)
#residuals no normally distributed, p=0.02796
hist(E1)  #ok
plot(M1)#ok

bartlett.test(chla~nutrient, data=d.gpp1)
#variance OK

anova(M1) #interpretation: no response
#P               1  6.28981  0.0172
#Si              1 10.63918  0.0026
#P:Si            1  4.22278  0.0479

xx = na.omit(subset(d.gpp1, select = c(P,Si,chla)))
interaction.plot(xx$P, xx$Si, xx$chla)
#Si and P inhibition; Si dampens P inhibition
