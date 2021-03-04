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
wenas_fall <- read.table(file="wen_fall.csv", header=T, sep=",")

#set variable
d = wenas_fall

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
#check A5, others ok

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) #changed to ddply b/c allows
#to specify by column name - I had a csv file with the relevant column in a different position.
x
d.cr$cr.nrr = d.cr$cr.area/-7.662895 #divide by control ave_cr
d.cr$gpp.nrr<-NA

#check distribution of controls and remove as needed before calculating NRR.
ggplot(d.gpp, aes(x=nutrient, y=gpp.area))+geom_boxplot() +theme_classic()
#ok
ggplot(d.gpp, aes(x=nutrient, y=chla))+geom_boxplot() +theme_classic()
#ok

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/2.5640416 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.6598855 #divide by control ave_chla
d.gpp$cr.nrr<-NA

#combine files and export
d.cr$chla.nrr<-NA
d.nrr<-rbind(d.cr, d.gpp)
d.nrr$site_date<-"wenas_fall"
d.nrr$cr.es<-log(d.nrr$cr.nrr)
d.nrr$gpp.es<-log(d.nrr$gpp.nrr)
d.nrr$chla.es<-log(d.nrr$chla.nrr)
write.table(d.nrr, "wenas_fall_nrr.csv", sep=",", quote=F, row.names =F)

###############
#plots of NRR
##############
ggplot(data=d.cr, aes(x=nutrient, y=cr.nrr))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#inhibition of most; NPSi neutral

ggplot(data=subset(d.nrr, (top=="sponge")), aes(x=nutrient, y=cr.es))+geom_boxplot()+theme_bw()+
  ylab("CR Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  geom_hline(yintercept = -1.385)

ggplot(data=d.gpp, aes(x=nutrient, y=gpp.nrr))+geom_boxplot()+theme_bw()+
  ylab("GPP NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#inhibition of most; N neutral

ggplot(data=subset(d.nrr,top=="glass"), aes(x=nutrient, y=gpp.es))+geom_boxplot()+theme_bw()+
  ylab("GPP Effect Size")+geom_hline(yintercept = -0.7, lty="dashed")+geom_hline(yintercept = 0.7, lty="dashed")+
  geom_hline(yintercept = -1.385)+geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=d.gpp, aes(x=nutrient, y=chla.nrr))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#limitation for N and NPSi; others neutral

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
d.sum$site_date<-"reec_summer"
write.table(d.sum, "reec_summer_summ.csv",  sep=",", quote=F, row.names =F)

############################################################
#analyze RESPIRATION data
############################################################

M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)#ok except for 1 point
ad.test(E1)
   #residuals are normally distributed, p=0.1089
hist(E1)  
plot(M1)
#ok

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
   #variance test not OK, we'll probably need to mess with this one. looks like one point in N

anova(M1)
  

##########################################################
#do multiple 2 way ANOVAs to improve our ability to interpret
##########################################################
#N and P
M1<-gls(cr.area~N*P, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.1807
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test not OK

anova(M1) #interpretation, no limitation due to antagonistic interaction

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(N,P,cr.area)))
interaction.plot(xx$N, xx$P, xx$cr.area)

#N and Si
M1<-gls(cr.area~N*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.2967
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test noy OK

anova(M1) #interpretation, no limitation

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(N,Si,cr.area)))
interaction.plot(xx$N, xx$Si, xx$cr.area)

#P and Si
M1<-gls(cr.area~P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are not normally distributed, p=0.03369
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #interpretation, no limitation due to antagonistic interacdtion

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(P,Si,cr.area)))
interaction.plot(xx$P, xx$Si, xx$cr.area)
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
M1<-gls(chla~N*P*Si, data=d.gpp, na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#ok

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(chla)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
#ok

anova(M1)
