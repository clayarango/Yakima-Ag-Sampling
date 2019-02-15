#Author:  Clay Arango
#Creation date: 10-Feb-19
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
library(dplyr)
library(multcomp)
library(MASS)
library(ggplot2)

#Load data
roza_fall <- read.table(file="roza_fall.csv", header=T, sep=",")

#set variable
d = roza_fall

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
x = aggregate(d.cr[,8], list(d.cr$nutrient), mean, na.rm=T)
x
d.cr$cr.nrr = d.cr$cr.area/-10.244565 #is there a prettier way to do this?

#calculate nrr for gpp
x = aggregate(d.gpp[,9], list(d.gpp$nutrient), mean, na.rm=T)
x
d.gpp$gpp.nrr = d.gpp$gpp.area/3.991310 #is there a prettier way to do this?

############################################################
#analyze RESPIRATION data
############################################################

M1<-gls(cr.area~N*P*Si, data=d.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals are normally distributed, p=0.1036
hist(E1)  
plot(M1)

plot(filter(d.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=d.cr)
   #variance test is below 0.05, but just barely

anova(M1)
  #co-limited by P and Si

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

#ggsave('output/figures/Roza_summer.tiff',
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
   #residuals are normal

hist(E1, xlab="residuals", main="")
plot(M1)

plot(filter(d.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E1, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=d.gpp)
   #OK

anova(M1)
  #P+Si co-limitation

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


#BELOW HERE IS NOT EDITED





############################################################
#analyze the CHL-A biomass on disks

M1<-gls(chla~N*P*light, data=roza_sum.chla, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
  #residuals look bad (p=0.001)
hist(E1)
plot(M1)

vf1 = varIdent(form = ~ 1|N*P)
vf2 = varIdent(form = ~ 1|light)
vf3 = varIdent(form = ~ 1|N*P*light)
vf4 = varPower(form = ~ fitted(.)) 
vf5 = varExp(form = ~ fitted(.))
vf6 = varConstPower(form = ~ fitted(.))
vf7 = varExp(form = ~ fitted(.)|nutrient)

M2<-gls(chla~N*P*light, data=roza_sum.chla, na.action=na.omit, weights=vf1)
M3<-gls(chla~N*P*light, data=roza_sum.chla, na.action=na.omit, weights=vf2)
M4<-gls(chla~N*P*light, data=roza_sum.chla, na.action=na.omit, weights=vf3)
M5<-gls(chla~N*P*light, data=roza_sum.chla, na.action=na.omit, weights=vf4)
M6<-gls(chla~N*P*light, data=roza_sum.chla, na.action=na.omit, weights=vf5)
M7<-gls(chla~N*P*light, data=roza_sum.chla, na.action=na.omit, weights=vf6)
M8<-gls(chla~N*P*light, data=roza_sum.chla, na.action=na.omit, weights=vf7)

anova(M1,M2,M3,M4,M5,M6,M7,M8)

#M3 is best on AIC

E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
#residuals are not normal (p=0.001)
hist(E3, xlab="residuals", main="")
plot(M3)
  #hist and plot look good

plot(filter(roza_sum.chla, !is.na(chla)) %>% dplyr::select(light), 
     E3, xlab="light", ylab="Residuals")
bartlett.test(chla~light, data=roza_sum.chla)

plot(filter(roza_sum.chla, !is.na(chla)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(chla~nutrient, data=roza_sum.chla)

anova(M3)

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
