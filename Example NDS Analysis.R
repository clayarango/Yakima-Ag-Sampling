#Author:  Clay Arango
#Creation date: 5-Oct-15
#R file:  NDS2015analysis.R
#Script to load and analyze NDS data from Summer 2015 WSB work in the Teanaway

#This script works with an individual nds data file from a stream, 
#which needs to be specified

#18-Aug-16 revisions to finalize and simplify analysis

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
standup <- read.table(file="standup.nds.2015.csv", header=T, sep=",")

#evaluate data to make sure factors are correct
unique(standup$nutrient)
unique(standup$N)
unique(standup$P)
unique(standup$N.c)
unique(standup$P.c)
unique(standup$light)
unique(standup$concentration)
str(standup)

#convert N and P values (0 or 1 for absence or presence) to factors
standup$N<-as.factor(standup$N)
standup$P<-as.factor(standup$P)

#subset data into gpp and cr response
standup.cr = subset(standup, top=="sponge", data=standup)
standup.chla = subset(standup, top=="glass", data=standup)

#remove the low light treatment bc they were all zeros
standup.gpp = subset(standup.chla, light=="high", data=standup) 
                       
############################################################

#analyze RESPIRATION data
  #including concentration as a main factor with all the other factors 
    #trips up the model because it's not fully factorial
  #run concentration separately to see if it is significant
M1<-gls(cr.area~N.c+P.c, data=standup.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
  #residuals OK (p=0.05729)
hist(E1)
plot(M1)

vf1 = varIdent(form = ~ 1|N.c*P.c)  
vf1a = varIdent(form = ~ 1|N.c)
vf1b = varIdent(form ~1|P.c)
vf2 = varPower(form = ~ fitted(.)) 
vf3 = varExp(form = ~ fitted(.))
vf4 = varConstPower(form = ~ fitted(.))

M2<-gls(cr.area~N.c+P.c, data=standup.cr, na.action=na.omit, weights=vf1)
M2a<-gls(cr.area~N.c+P.c, data=standup.cr, na.action=na.omit, weights=varComb(vf1a,vf1b))
M3<-gls(cr.area~N.c+P.c, data=standup.cr, na.action=na.omit, weights=vf2)
M4<-gls(cr.area~N.c+P.c, data=standup.cr, na.action=na.omit, weights=vf3)
M5<-gls(cr.area~N.c+P.c, data=standup.cr, na.action=na.omit, weights=vf4)#no convergence
M6<-gls(cr.area~N.c+P.c, data=standup.cr, na.action=na.omit, weights=varComb(vf1,vf2))

anova(M1,M2,M2a,M3,M4,M6)
  #M4 is best
anova(M1,M2)

E2<-residuals(M2)
qqnorm(E2)
qqline(E2)
ad.test(E2)
#residuals are not normal (p=0.006)
hist(E2, xlab="residuals", main="")
plot(M2)
  #M2 looks best

anova(M2)

model.matrix.gls <- function(M2, ...){
  model.matrix(terms(M2), data = getData(M2), ...)  
}
model.frame.gls <- function(M2, ...){
  model.frame(formula(M2), data = getData(M2), ...)  
}
terms.gls <- function(M2, ...){
  terms(model.frame(M2),...)  
}

multCompTukey <- glht(M2, linfct = mcp(P.c = "Tukey")) 
summary(multCompTukey)
  #P concentration has no effect on cr

multCompTukey <- glht(M2, linfct = mcp(N.c = "Tukey")) 
summary(multCompTukey)
  #N concentration has no effect on cr

#analyze the respiration data on sponges

M1<-gls(cr.area~N*P*light, data=standup.cr, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
   #residuals are normally distributed, p=0.3398
hist(E1)  
plot(M1)

vf1 = varIdent(form = ~ 1|N*P)
vf2 = varIdent(form = ~ 1|light)
vf3 = varIdent(form = ~ 1|N*P*light)
vf4 = varPower(form = ~ fitted(.)) 
vf5 = varExp(form = ~ fitted(.))
vf6 = varConstPower(form = ~ fitted(.))
vf7 = varExp(form = ~ fitted(.)|nutrient)

M2<-gls(cr.area~N*P*light, data=standup.cr, na.action=na.omit, weights=vf1)
M3<-gls(cr.area~N*P*light, data=standup.cr, na.action=na.omit, weights=vf2)
M4<-gls(cr.area~N*P*light, data=standup.cr, na.action=na.omit, weights=vf3)
M5<-gls(cr.area~N*P*light, data=standup.cr, na.action=na.omit, weights=vf4)
M6<-gls(cr.area~N*P*light, data=standup.cr, na.action=na.omit, weights=vf5)
M7<-gls(cr.area~N*P*light, data=standup.cr, na.action=na.omit, weights=vf6)
M8<-gls(cr.area~N*P*light, data=standup.cr, na.action=na.omit, weights=vf7)

anova(M1,M2,M3,M4,M5,M6,M7,M8)
  #M6 is best on AIC

E6<-residuals(M6)
qqnorm(E6)
qqline(E6)
ad.test(E6)
  #residuals are normal
hist(E6, xlab="residuals", main="")
plot(M6)

anova(M1,M6)

plot(filter(standup.cr, !is.na(cr.area)) %>% dplyr::select(light), 
     E6, xlab="light", ylab="Residuals")
bartlett.test(cr.area~light, data=standup.cr)
#OK

plot(filter(standup.cr, !is.na(cr.area)) %>% dplyr::select(nutrient), 
     E6, xlab="nutrient", ylab="Residuals")
bartlett.test(cr.area~nutrient, data=standup.cr)
#OK

anova(M6)
  #co-limited by N and P, P interaction with light

x <- group_by(standup.cr, nutrient, light) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(cr.mean = abs(mean(cr.area, na.rm = TRUE)), # na.rm = TRUE to remove missing values
            cr.sd=abs(sd(cr.area, na.rm = TRUE)),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(cr.area)), # of observations, excluding NAs. 
            cr.se=cr.sd/sqrt(n))

ggplot(data=x, 
       aes(x=nutrient, y=cr.mean, fill=light)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=cr.mean, ymax=cr.mean+cr.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("white","black")) +
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

ggsave('output/figures/crByNutrientLight.tiff',
       units="in",
       width=3.25,
       height=3.25,
       dpi=1200,
       compression="lzw")

############################################################

#analyze the PRODUCTION data on disks
  #run concentration separately to see if it is significant
M1<-gls(gpp.area~N.c+P.c, data=standup.gpp, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
  #residuals are not normal (p=0.002111)
hist(E1)
plot(M1)

vf1 = varIdent(form = ~ 1|N.c*P.c)  
vf1a = varIdent(form = ~ 1|N.c)
vf1b = varIdent(form ~1|P.c)
vf2 = varPower(form = ~ fitted(.)) 
vf3 = varExp(form = ~ fitted(.))
vf4 = varConstPower(form = ~ fitted(.))

M2<-gls(gpp.area~N.c+P.c, data=standup.gpp, na.action=na.omit, weights=vf1)
M2a<-gls(gpp.area~N.c+P.c, data=standup.gpp, na.action=na.omit, weights=varComb(vf1a,vf1b))
M3<-gls(gpp.area~N.c+P.c, data=standup.gpp, na.action=na.omit, weights=vf2)
M4<-gls(gpp.area~N.c+P.c, data=standup.gpp, na.action=na.omit, weights=vf3)
M5<-gls(gpp.area~N.c+P.c, data=standup.gpp, na.action=na.omit, weights=vf4)#no convergence
M6<-gls(gpp.area~N.c+P.c, data=standup.gpp, na.action=na.omit, weights=varComb(vf1,vf2))

anova(M1,M2,M2a,M3,M4,M5,M6)
 #M3 is best

E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
  #residuals are not normal (p=0.001)
hist(E3, xlab="residuals", main="")
plot(M3)

anova(M3)
  #no concentration effect

#no light factor bc all low light values were zero
M1<-gls(gpp.area~N*P, data=standup.gpp, na.action=na.omit) 
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
#residuals are not normal p=1e-5

vf1 = varIdent(form = ~ 1|N*P)
vf2 = varPower(form = ~ fitted(.)) 
vf3 = varExp(form = ~ fitted(.))
vf4 = varConstPower(form = ~ fitted(.))
vf5 = varExp(form = ~ fitted(.)|nutrient)

M2<-gls(gpp.area~N*P, data=standup.gpp, na.action=na.omit, weights=vf1)
M3<-gls(gpp.area~N*P, data=standup.gpp, na.action=na.omit, weights=vf2)
M4<-gls(gpp.area~N*P, data=standup.gpp, na.action=na.omit, weights=vf3)
M5<-gls(gpp.area~N*P, data=standup.gpp, na.action=na.omit, weights=vf4)
M6<-gls(gpp.area~N*P, data=standup.gpp, na.action=na.omit, weights=vf5)

anova(M1,M2,M3,M4,M5)

anova(M1,M3)

E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
  #not normal
hist(E3, xlab="residuals", main="")
plot(M3)

plot(filter(standup.gpp, !is.na(gpp.area)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(gpp.area~nutrient, data=standup.gpp)
#OK

anova(M3)
  #no limitation

x <- group_by(standup.gpp, nutrient) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gpp.mean = abs(mean(gpp.area, na.rm = TRUE)), # na.rm = TRUE to remove missing values
            gpp.sd=abs(sd(gpp.area, na.rm = TRUE)),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(gpp.area)), # of observations, excluding NAs. 
            gpp.se=gpp.sd/sqrt(n))

ggplot(data=x, 
       aes(x=nutrient, y=gpp.mean)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=gpp.mean, ymax=gpp.mean+gpp.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("white")) +
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

ggsave('output/figures/gppByNutrient.tiff',
       units="in",
       width=3.25,
       height=3.25,
       dpi=1200,
       compression="lzw")

############################################################

#analyze the CHL-A biomass on disks
  #including concentration as a main factor with all the other factors 
    #trips up the model because it's not fully factorial
  #run concentration separately to see if it is significant
M1<-gls(chla~N.c+P.c, data=standup.chla, na.action=na.omit)
E1<-residuals(M1)
qqnorm(E1)
qqline(E1)
ad.test(E1)
  #residuals look bad (p=2.4e-5)
hist(E1)
plot(M1)

vf1 = varIdent(form = ~ 1|N.c*P.c)  
vf1a = varIdent(form = ~ 1|N.c)
vf1b = varIdent(form ~1|P.c)
vf2 = varPower(form = ~ fitted(.)) 
vf3 = varExp(form = ~ fitted(.))
vf4 = varConstPower(form = ~ fitted(.))
vf5 = varIdent(form = ~ 1|light)
vf6 = varExp(form=~fitted(.)|light)

M2<-gls(chla~N.c+P.c, data=standup.chla, na.action=na.omit, weights=vf1)
M2a<-gls(chla~N.c+P.c, data=standup.chla, na.action=na.omit, weights=varComb(vf1a,vf1b))
M3<-gls(chla~N.c+P.c, data=standup.chla, na.action=na.omit, weights=vf2)
M4<-gls(chla~N.c+P.c, data=standup.chla, na.action=na.omit, weights=vf3)
M5<-gls(chla~N.c+P.c, data=standup.chla, na.action=na.omit, weights=vf4)
M6<-gls(chla~N.c+P.c, data=standup.chla, na.action=na.omit, weights=varComb(vf1,vf2))
M7<-gls(chla~N.c+P.c, data=standup.chla, na.action=na.omit, weights=vf6)#no convergence

anova(M1,M2,M2a,M3,M4,M5,M6)
  #M3

E3<-residuals(M3)
qqnorm(E3)
qqline(E3)
ad.test(E3)
  #residuals are not normal (p=8.0e-5)
hist(E3, xlab="residuals", main="")
plot(M3)

anova(M3)

model.matrix.gls <- function(M3, ...){
  model.matrix(terms(M3), data = getData(M3), ...)  
}
model.frame.gls <- function(M3, ...){
  model.frame(formula(M3), data = getData(M3), ...)  
}
terms.gls <- function(M3, ...){
  terms(model.frame(M3),...)  
}

multCompTukey <- glht(M3, linfct = mcp(P.c = "Tukey")) 
summary(multCompTukey)
#P concentration has no effect on chla

multCompTukey <- glht(M4, linfct = mcp(N.c = "Tukey")) 
summary(multCompTukey)
#N concentration has no effect on chla


#analyze chl a biomass
M1<-gls(chla~N*P*light, data=standup.chla, na.action=na.omit)
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

M2<-gls(chla~N*P*light, data=standup.chla, na.action=na.omit, weights=vf1)
M3<-gls(chla~N*P*light, data=standup.chla, na.action=na.omit, weights=vf2)
M4<-gls(chla~N*P*light, data=standup.chla, na.action=na.omit, weights=vf3)
M5<-gls(chla~N*P*light, data=standup.chla, na.action=na.omit, weights=vf4)
M6<-gls(chla~N*P*light, data=standup.chla, na.action=na.omit, weights=vf5)
M7<-gls(chla~N*P*light, data=standup.chla, na.action=na.omit, weights=vf6)
M8<-gls(chla~N*P*light, data=standup.chla, na.action=na.omit, weights=vf7)

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

plot(filter(standup.chla, !is.na(chla)) %>% dplyr::select(light), 
     E3, xlab="light", ylab="Residuals")
bartlett.test(chla~light, data=standup.chla)

plot(filter(standup.chla, !is.na(chla)) %>% dplyr::select(nutrient), 
     E3, xlab="nutrient", ylab="Residuals")
bartlett.test(chla~nutrient, data=standup.chla)

anova(M3)

#light is significant and N and P separately interact with light

x<-standup.chla[complete.cases(standup.chla$chla),]

with(x, 
     interaction.plot(nutrient,light,chla, 
                      ylim=c(0,1.5),lty=c(1,12),lwd=2,ylab="Chlorophyll a", 
                      xlab="Nutrient", trace.label="Light"))

x <- group_by(standup.chla, nutrient, light) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
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
x1 <- group_by(standup, nutrient, light) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(chla.nrr.mean = mean(chla.nrr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            chla.nrr.sd=sd(chla.nrr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            chla.n = sum(!is.na(chla.nrr)), # of observations, excluding NAs. 
            chla.nrr.se=chla.nrr.sd/sqrt(chla.n))


x2 <- group_by(standup, nutrient, light) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(gpp.nrr.mean = mean(gpp.nrr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            gpp.nrr.sd=sd(gpp.nrr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            gpp.n = sum(!is.na(gpp.nrr)), # of observations, excluding NAs. 
            gpp.nrr.se=gpp.nrr.sd/sqrt(gpp.n))

x3 <- group_by(standup, nutrient, light) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(cr.nrr.mean = mean(cr.nrr, na.rm = TRUE), # na.rm = TRUE to remove missing values
            cr.nrr.sd=sd(cr.nrr, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            cr.n = sum(!is.na(cr.nrr)), # of observations, excluding NAs. 
            cr.nrr.se=cr.nrr.sd/sqrt(cr.n))

standup.nrr<-cbind(x1,x2,x3)
standup.nrr <- standup.nrr[, !duplicated(colnames(standup.nrr))]
write.csv(standup.nrr, "standup.nrr.csv")
