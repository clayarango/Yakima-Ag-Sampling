#Author:  Clay Arango
#Creation date: 14-Feb-19
#Script to analyze NDS data from Summer 2018 WSB work with S. Roley and A. Alexiades

#packages
install.packages("nlme")
install.packages("nortest")
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
sat_summer<- read.csv("satus_summer.csv")

#set variable
d = sat_summer

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

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T))
x
d.cr$cr.nrr = d.cr$cr.area/-21.02584 #divide by control ave_cr

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla_ug_cm2, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/2.7173342 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla_ug_cm2/0.8314271 #divide by control ave_chla

###############
#plots of NRR
##############
ggplot(data=subset(d.cr, !(nutrient=="control")), aes(x=nutrient, y=cr.nrr))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=subset(d.gpp, !(nutrient=="control")), aes(x=nutrient, y=gpp.nrr))+geom_boxplot()+theme_bw()+
  ylab("GPP NRR")+geom_abline(slope = 0, intercept = 1)+scale_y_continuous(limits=c(0,4))+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(data=subset(d.gpp, !(nutrient=="control")), aes(x=nutrient, y=chla.nrr))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())

############################################################
#analyze RESPIRATION data
############################################################

M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals are more-or-less normally distributed, p=0.06625. A lot of wiggle around QQ line
    #p = 0.1898
hist(E1) #ok
plot(M1)# a little bit heteroscedastic but not dramatically so. less variation as values increase

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
   #variance test p = 0.03735

anova(M1)
  #P and NPSi interaction signficant. in figure, Si appears higher, NP lower, all others equal to Control

#try normalizing
d.cr$l.cr.area = log10(d.cr$cr.area*-1)

M2<-gls(l.cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are normal, p=0.1572
hist(E2)  
plot(M2)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(l.cr.area~nutrient, data=d.cr)
#variance test is equal, p=0.05247
anova(M2)




ggplot(data=d.cr, aes(x=nutrient, y = cr.area))+geom_boxplot()

x <- group_by(d.cr, nutrient) %>%  # Grouping function causes subsequent functions to aggregate by treatment
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


#ggsave('output/figures/Aht_summer_CRNRR.tiff',
#       units="in",
#       width=3.25,
#       height=3.25,
#       dpi=1200,
#       compression="lzw")

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

ggplot(data=d.gpp, aes(x=nutrient, y = gpp.area))+geom_boxplot()# one funky control value!! check on it!!
anova(M1)#doing anova anyway
#nothing significant

kruskal.test(gpp.area~N+P+Si, data=d.gpp, na.action=na.omit)#kruskal-wallis can only have 1 factor


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
M1<-gls(chla_ug_cm2~N*P*Si, data=d.gpp, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are not normal, p=0.8999

hist(E1)  
plot(M1)

plot(filter(d.gpp, !is.na(chla)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(chla_ug_cm2~nutrient, data=d.gpp)
#variance test p=0.1341

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

############################################################
############################################################
#calculate nrr mean and standard error
x1 <- group_by(roza_sum, nutrient, light) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(chla.nrr.mean = mean(chla.nrr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            chla.nrr.sd=sd(chla.nrr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            chla.n = sum(!is.na(chla.nrr)), # of observations, excluding NAs. 
            chla.nrr.se=chla.nrr.sd/sqrt(chla.n))


x2 <- group_by(roza_sum, nutrient, light) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gpp.nrr.mean = mean(gpp.nrr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            gpp.nrr.sd=sd(gpp.nrr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            gpp.n = sum(!is.na(gpp.nrr)), # of observations, excluding NAs. 
            gpp.nrr.se=gpp.nrr.sd/sqrt(gpp.n))

x3 <- group_by(roza_sum, nutrient, light) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(cr.nrr.mean = mean(cr.nrr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            cr.nrr.sd=sd(cr.nrr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            cr.n = sum(!is.na(cr.nrr)), # of observations, excluding NAs. 
            cr.nrr.se=cr.nrr.sd/sqrt(cr.n))

roza_sum.nrr<-cbind(x1,x2,x3)
roza_sum.nrr <- roza_sum.nrr[, !duplicated(colnames(roza_sum.nrr))]
write.csv(roza_sum.nrr, "roza_sum.nrr.csv")
