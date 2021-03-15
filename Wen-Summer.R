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
d.gpp$gpp.nrr_1 = d.gpp$gpp.area/0.2308552 #NRR with weird controls removed.
x1c<-ddply(subset(d.gpp, !(nds.id=="G8")), "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T))
x1c
d.gpp$chla.nrr_1=d.gpp$chla/0.04351025

#combine files and export
d.cr$gpp.nrr_1<-NA
d.cr$chla.nrr_1<-NA
d.nrr<-rbind(d.cr, d.gpp)
d.nrr$site_date<-"wenas_summer"
d.nrr$cr.es<-log(d.nrr$cr.nrr)
d.nrr$gpp.es<-log(d.nrr$gpp.nrr)
d.nrr$chla.es<-log(d.nrr$chla.nrr)
d.nrr$gpp.es_1<-log(d.nrr$gpp.nrr_1)
d.nrr$chla.es_1<-log(d.nrr$chla.nrr_1)
write.table(d.nrr, "wenas_summer_nrr.csv", sep=",", quote=F, row.names =F)

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


##########################################################
#do multiple 2 way ANOVAs to improve our ability to interpret
##########################################################
#N and P
M1<-gls(cr.area~N*P, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.5236
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #N and P independent limitation
#N               1  17.72482  0.0002
#P               1   7.60201  0.0093
#N:P             1   0.48018  0.4930

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(N,P,cr.area)))
interaction.plot(xx$N, xx$P, xx$cr.area*-1)

#N and Si
M1<-gls(cr.area~N*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.5998
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #interpretation: N limitation, Si inhibitory on N

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(N,Si,cr.area)))
interaction.plot(xx$N, xx$Si, xx$cr.area*-1)

#P and Si
M1<-gls(cr.area~P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.0754
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #interpretation: simultaneous P and Si limitation
#P               1   6.06118  0.0190
#Si              1   2.30454  0.1382
#P:Si            1  14.76178  0.0005

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(P,Si,cr.area)))
interaction.plot(xx$P, xx$Si, xx$cr.area*-1)
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

#ggsave('output/figures/Ring_summer.tiff',
#       units="in",
#       width=3.25,
#       height=3.25,
#       dpi=1200,
#       compression="lzw")

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
############################################################
M1<-gls(log(chla+1)~N*P*Si, data=subset(d.gpp, !(nds.id=="G8")), na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1) #ugh - WAY off
ad.test(E1)
#p<0.00001
#log-transform doesn't do much

hist(E1, xlab="residuals", main="") #good!
plot(M1) #huge increase in variation with magnitude

plot(filter(subset(d.gpp, !(nds.id=="G8")), !is.na(chla)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
#N way more variation than other treatments
#log transform doesn't help

anova(M1)





