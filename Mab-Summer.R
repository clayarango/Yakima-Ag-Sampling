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
mab_summer <- read.table(file="mab_summer.csv", header=T, sep=",")

#set variable
d = mab_summer

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
#P5 is an outlier

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-13.104819 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#K1, N+P+Si group is bimodal, P+Si group is widely but evenly spread
ggplot(d.gpp, aes(x=nutrient, y=chla_ug_cm2)) + geom_boxplot() + theme_classic()
#Q4, K1, S4, N group is evenly spread out

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/1.61646980 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.3782043 #divide by control ave_chla
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
d.nrr$site.date<-"mab_summer"
d.nrr$gpp.es<-log(d.nrr$gpp.nrr)
d.nrr$cr.es<-log(d.nrr$cr.nrr)
d.nrr$chla.es<-log(d.nrr$chla.nrr)

write.table(d.nrr, "mab_summer_nrr.csv",  sep=",", quote=F, row.names =F)

###############
#plots of NRR
##############
ggplot(data=subset(d.cr, !(nutrient=="control")), aes(x=nutrient, y=cr.nrr))+geom_boxplot()+theme_bw()+
  ylab("CR NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#P limitation, Si inhibition?

ggplot(data=subset(d.nrr, (top=="sponge")), aes(x=nutrient, y=cr.es))+geom_boxplot()+theme_bw()+
  ylab("CR Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+
  geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#nothing over 1.385, so no significance?

ggplot(data=subset(d.gpp, !(nutrient=="control")), aes(x=nutrient, y=gpp.nrr))+geom_boxplot()+theme_bw()+
  ylab("GPP NRR")+geom_abline(slope = 0, intercept = 1)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#possible P limitation

ggplot(data=subset(d.nrr, top =="glass"),aes(x=nutrient, y=gpp.es))+geom_boxplot()+theme_bw()+
  ylab("GPP Effect Size")+geom_hline(yintercept = 0.7, lty="dashed")+
  geom_hline(yintercept = -0.7, lty="dashed")+ geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#no limitation

ggplot(data=subset(d.gpp, !(nutrient=="control")), aes(x=nutrient, y=chla.nrr))+geom_boxplot()+theme_bw()+
  ylab("Chlorophyll-a NRR")+geom_abline(slope = 0, intercept = 1)+ 
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#no limitation

ggplot(data=subset(d.nrr, (top=="glass")), aes(x=nutrient, y=chla.es))+geom_boxplot()+theme_bw()+
  ylab("Chla Effect Size")+ geom_hline(yintercept = 0.7, lty="dashed")+ geom_hline(yintercept = -0.7, lty="dashed")+    
  geom_hline(yintercept = 1.385)+
  theme(axis.title.x=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
#P+Si inhibition?  nothing over 1.385 so no significance?

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
d.sum$site_date<-"mab_summer"
write.table(d.sum, "mab_summer_summ.csv",  sep=",", quote=F, row.names=F)

############################################################
#analyze RESPIRATION data
############################################################

M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals are normally distributed p = 0.7291
hist(E1)  
plot(M1)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
   #variance test is OK p = 0.0853

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
#residuals are normally distributed, p=0.4852
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #interpretation, P limitation

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(N,P,cr.area)))
interaction.plot(xx$N, xx$P, xx$cr.area)

#N and Si
M1<-gls(cr.area~N*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.958
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #Si inhibition

#remove NA for plotting
xx = na.omit(subset(d.cr, select = c(N,Si,cr.area)))
interaction.plot(xx$N, xx$Si, xx$cr.area)

#P and Si
M1<-gls(cr.area~P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are normally distributed, p=0.6196
hist(E1)  
plot(M1)

bartlett.test(cr.area~nutrient, data=d.cr)
#variance test OK

anova(M1) #interpretation, P limitation, Si inhibition

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
   #residuals are not normal p = 0.001 

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
   #variance not good 0.0007

#try log normalizing
d.gpp$l.gpp = log10(d.gpp$gpp.area+1)

M2<-gls(l.gpp~N*P*Si, data=d.gpp, na.action=na.omit) 
E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are normal p = 0.74 

hist(E2, xlab="residuals", main="")
plot(M2)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(l.gpp~nutrient, data=d.gpp)
#variance good 0.07

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
  #residuals look bad (p=0.0001)
hist(E1)
plot(M1)

bartlett.test(chla~nutrient, data=d.gpp)
  #Variance looks OK

#try log transformation
d.gpp$l.chla = log10(d.gpp$chla+1)

M1<-gls(l.chla~N*P*Si, data=d.gpp, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
  #residuals look bad (p=0.0032)
hist(E1)
plot(M1)

bartlett.test(l.chla~nutrient, data=d.gpp)
  #variance is OK p = 0.29

anova(M1)

#light is significant and N and P separately interact with light

x<-roza_sum.chla[complete.cases(roza_sum.chla$chla),]

with(x, 
     interaction.plot(nutrient,light,chla, 
                      ylim=c(0,1.5),lty=c(1,12),lwd=2,ylab="Chlorophyll a", 
                      xlab="Nutrient", trace.label="Light"))

x <- group_by(roza_sum.chla, nutrient, light) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(chla.mean = mean(chla, na.rm = TRUE), # na.rm = TRUE to remove missing values
            chla.sd=sd(chla, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(chla)), # of observations, excluding NAs. 
            chla.se=chla.sd/sqrt(n))

ggplot(data=x, 
       aes(x=nutrient, y=chla.mean, fill=light)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=chla.mean, ymax=chla.mean+chla.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("white","black")) +
  xlab("Nutrient") +
  ylab(expression(Chlorophyll~a~(ug~cm^{-2}))) +
  ylim(0,1.7) +
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

ggsave('output/figures/chlaByNutrientLight.tiff',
       units="in",
       width=3.25,
       height=3.25,
       dpi=1200,
       compression="lzw")



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
