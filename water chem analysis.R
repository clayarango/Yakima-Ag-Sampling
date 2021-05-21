####Script created 29 March 2021 to analyze relationships between NRR and water chem
## S Roley

#packages
library(nlme)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lmtest)
library(lattice)
library(MuMIn)

#data (created in "all sites" by combining NDS files and water chem file)
nds_chem<-read.csv("NDS_chem_all.csv")

nds_chem$river_mile<-ifelse(nds_chem$stream=="ahtanum", 106, nds_chem$river_mile)

N<-subset(nds_chem, N==1)
N_sponge<-subset(N, top=="sponge")
N_glass<-subset(N, top =="glass")

chem<-read.csv("chem_summary.csv")
chem$river_mile<-ifelse(chem$stream=="ahtanum", 106, chem$river_mile)

#ratios
chem$DIN.mgNL<-chem$NH4.mgNL+chem$NO3.mgNL
chem$N.P.ratio<-(chem$DIN.mgNL/14)/(chem$oP.mgPL/31)
chem$N.Si.ratio<-(chem$DIN.mgNL/14)/(chem$Si.mgL/28)
chem$P.Si.ratio<-(chem$oP.mgPL/31)/(chem$Si.mgL/28)

chem$stream<-as.factor(chem$stream)
chem$season<-as.factor(chem$season)
chem$type<-as.factor(chem$type)

#NRR model, based on plots
#start with fixed effects and then test if adding random effects improves the model
#M1 is all the variables and interactions (except nutrient, which we want to model as a random effect)
M1<-lm(NO3.mgNL~river_mile*season + type*river_mile, chem)

plot(M1)
E<-rstandard(M1)#mostly normal and homoscedastistic
boxplot(E~stream, data=chem)
abline(0,0) #huge variation by stream - need to include as random effect

#now, use gls so can compare with lme
M1<-gls(NO3.mgNL~river_mile*season + type*river_mile, chem)
M1a<-lme(NO3.mgNL~river_mile*season + type*river_mile, random = ~1+river_mile|stream, 
         method="REML", chem) #doesn't converge
M1b<-lme(NO3.mgNL~river_mile*season + type*river_mile, random = ~1|stream, 
         method="REML", chem)
anova(M1, M1b)
#     Model df    AIC      BIC    logLik   Test  L.Ratio p-value
#M1      1  7 66.20444 71.16079 -26.10222                        
#M1b     2  8 66.01708 71.68149 -25.00854 1 vs 2 2.187355  0.1391

#random effects really don't help (or hurt), prob b/c so little replication. will leave random effects in b/c graphs of 
#residuals suggested they were needed.

#now, fixed effects
summary(M1b)

M2b<-lme(NO3.mgNL~river_mile*season, random = ~1|stream, 
         method="REML", chem)

lrtest(M1b, M2b)
#   Df  LogLik Df Chisq Pr(>Chisq)  
#1   8 -25.009                      
#2   6 -20.707 -2 8.603    0.01355 *

#model 2 better, keep season out
summary(M2b)

M3b<-lme(NO3.mgNL~river_mile+season, random = ~1|stream, 
         method="REML", chem)

lrtest(M2b, M3b)
#   Df  LogLik Df  Chisq Pr(>Chisq)   
#1   6 -20.707                        
#2   5 -15.752 -1 9.9104   0.001643 **

#model 2 better

summary(M3b)
#AIC      BIC    logLik
#41.50363 45.95548 -15.75181

#Value          Std.Error DF            t-value p-value
#(Intercept)   1.7868658 0.3371792  9  5.299454  0.0005
#river_mile   -0.0107186 0.0028375  9 -3.777483  0.0044
#seasonsummer -0.0648577 0.1169943  9 -0.554367  0.5928

M4b<-lme(NO3.mgNL~river_mile, random = ~1|stream, 
         method="REML", chem)

lrtest(M3b, M4b)
#   Df  LogLik Df  Chisq Pr(>Chisq)
#1   5 -15.752                     
#2   4 -14.672 -1 2.1591     0.1417

#no difference but AIC was lower for M4b
summary(M4b)
#AIC      BIC    logLik
#37.34457 41.12233 -14.67229

#Random effects:
#  Formula: ~1 | stream
#(Intercept)  Residual
#StdDev:   0.3436648 0.2592373

#Fixed effects: NO3.mgNL ~ river_mile 
#Value Std.Error DF   t-value p-value
#(Intercept)  1.7484984 0.3235024 10  5.404900  0.0003
#river_mile  -0.0106873 0.0027804  9 -3.843825  0.0039
#Correlation: 
#  (Intr)
#river_mile -0.931

#Standardized Within-Group Residuals:
#  Min           Q1          Med           Q3          Max 
#-1.698179941 -0.484607900  0.001849822  0.374311253  1.591452626 

#Number of Observations: 21
#Number of Groups: 11 

plot(M4b)

r.squaredGLMM(M4b)
#     R2m       R2c
#[1,] 0.54536 0.8351211

plot(predict(M4b),chem$NO3.mgNL, xlab="Predicted NO3",ylab="Actual NO3",abline (0,1))
#looks great, even scatter around line

cor.test(predict(M4b),chem$NO3.mgNL)
#0.94233
0.94233^2
#0.888

#####SRP#####



#####old stuff below - can probably delete########
N_sum_s<-ddply(N_sponge, c("stream", "nutrient", "season", "river_mile", "top", "type"), summarise, cr=mean(cr.area, na.rm=T), 
               cr_nrr=mean(cr.nrr, na.rm=T), se_cr=(sd(cr.area, na.rm=T)/sqrt(sum(!is.na(cr.area)))),
              se_cr_nrr=(sd(cr.nrr, na.rm=T)/sqrt(sum(!is.na(cr.nrr)))))

N_sum_s<-merge(chem, N_sum_s, by=c("stream", "season", "type", "river_mile"), all=T)

N_sum_g<-ddply(N_glass, c("stream", "nutrient", "season", "river_mile", "top","type"), summarise, gpp=mean(gpp.area, na.rm=T),
               chl_a=mean(chla, na.rm=T), chl_a.nrr=mean(chla.nrr, na.rm=T), gpp.nrr=mean(gpp.nrr, na.rm=T), 
              se_gpp=(sd(gpp.area, na.rm=T)/sqrt(sum(!is.na(gpp.area)))), 
               se_chla=(sd(chla, na.rm=T)/sqrt(sum(!is.na(chla)))), 
               se_chla_nrr=(sd(chla.nrr, na.rm=T)/sqrt(sum(!is.na(chla.nrr)))))

N_sum_g<-merge(chem, N_sum_g, by=c("stream", "season", "type", "river_mile"), all=T)

P<-subset(nds_chem, P==1)
P_sponge<-subset(P, top=="sponge")
P_glass<-subset(P, top =="glass")

P_sum_s<-ddply(P_sponge, c("stream", "nutrient", "season", "river_mile", "top","type"), summarise, cr=mean(cr.area, na.rm=T), 
               cr_nrr=mean(cr.nrr, na.rm=T), se_cr=(sd(cr.area, na.rm=T)/sqrt(sum(!is.na(cr.area)))),
               se_cr_nrr=(sd(cr.nrr, na.rm=T)/sqrt(sum(!is.na(cr.nrr)))))

P_sum_g<-ddply(P_glass, c("stream", "nutrient", "season", "river_mile", "top","type"), summarise, gpp=mean(gpp.area, na.rm=T),
               chl_a=mean(chla, na.rm=T), chl_a.nrr=mean(chla.nrr, na.rm=T), gpp.nrr=mean(gpp.nrr, na.rm=T), 
               se_gpp=(sd(gpp.area, na.rm=T)/sqrt(sum(!is.na(gpp.area)))), 
               se_chla=(sd(chla, na.rm=T)/sqrt(sum(!is.na(chla)))), 
               se_chla_nrr=(sd(chla.nrr, na.rm=T)/sqrt(sum(!is.na(chla.nrr)))))

Si<-subset(nds_chem, Si==1)
Si_sponge<-subset(Si, top=="sponge")
Si_glass<-subset(Si, top =="glass")

Si_sum_s<-ddply(Si_sponge, c("stream", "nutrient", "season", "river_mile", "top","type"), summarise, cr=mean(cr.area, na.rm=T), 
               cr_nrr=mean(cr.nrr, na.rm=T), se_cr=(sd(cr.area, na.rm=T)/sqrt(sum(!is.na(cr.area)))),
               se_cr_nrr=(sd(cr.nrr, na.rm=T)/sqrt(sum(!is.na(cr.nrr)))))

Si_sum_g<-ddply(Si_glass, c("stream", "nutrient", "season", "river_mile", "top","type"), summarise, gpp=mean(gpp.area, na.rm=T),
               chl_a=mean(chla, na.rm=T), chl_a.nrr=mean(chla.nrr, na.rm=T), gpp.nrr=mean(gpp.nrr, na.rm=T), 
               se_gpp=(sd(gpp.area, na.rm=T)/sqrt(sum(!is.na(gpp.area)))), 
               se_chla=(sd(chla, na.rm=T)/sqrt(sum(!is.na(chla)))), 
               se_chla_nrr=(sd(chla.nrr, na.rm=T)/sqrt(sum(!is.na(chla.nrr)))))

########
#N limitation
#######
#how does N limitation differ among seasons and type? Expectation: N limitation more common in upper river and in summer.
#also N limitation more common in fall in tribs and summer in mainstem, esp for chla
#conclusion: Mainstem CR limited by N in fall, Tributaries N limited in summer

ggplot(N, aes(x=season, y=cr.nrr))+geom_boxplot(aes(color=factor(type)))+theme_classic()

t.test(cr.nrr~type, data=subset(N, season=="fall"))
#t = 3.2253, df = 184.74, p-value = 0.001488
#95 percent confidence interval: 0.07564847 0.31397925
#  mean in group mainstem     mean in group trib 
#0.9884479              0.7936341  

t.test(cr.nrr~type, data=subset(N, season=="summer"))
#t = 1.8336, df = 167.37, p-value = 0.06849
#95 percent confidence interval: -0.01124728  0.30445623
#  mean in group mainstem     mean in group trib 
#1.319428               1.172824 


ggplot(N, aes(x=season, y=chla.nrr))+geom_boxplot(aes(color=factor(type)))+theme_classic()+
  scale_y_continuous(limits=c(0,20))

t.test(chla.nrr~type, data=subset(N, season=="fall"))
#t = 1.3332, df = 127.58, p-value = 0.1848
#95 percent confidence interval: -0.05369536  0.27549457
#mean in group mainstem     mean in group trib 
# 0.8256694              0.7147698 

t.test(chla.nrr~type, data=subset(N, season=="summer"))
#t = -4.1246, df = 102.98, p-value = 7.542e-05
#95 percent confidence interval: -5.595440 -1.961683
#  mean in group mainstem     mean in group trib 
#1.836982               5.615544

#How does river position affect N limitation? Hypothesis: lower in WS (lower river mile) = less N limitation
ns<-function(x){0.6266+0.00605*x}
nps<-function(x){0.5816+0.0063*x}
npsis<-function(x){0.5823+0.0064*x}
nsis<-function(x){0.593+0.0054*x}

nf<-function(x){0.452+0.005*x}
npf<-function(x){0.5066+0.0045*x}
npsif<-function(x){0.456+0.0055*x}
nsif<-function(x){0.6127+0.0026*x}

ggplot(N_sum_s, aes(x=river_mile, y=cr_nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=cr_nrr-se_cr_nrr, ymax=cr_nrr+se_cr_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)+
  stat_function(data=subset(N_sum_s, nutrient=="N"), fun=nf, lty="dashed")+
  stat_function(data=subset(N_sum_s, nutrient=="N"), fun=ns)+
  stat_function(data=subset(N_sum_s, nutrient=="NP"), fun=npf, lty="dashed")+
  stat_function(data=subset(N_sum_s, nutrient=="NP"), fun=nps)+
  stat_function(data=subset(N_sum_s, nutrient=="NPSi"), fun=npsif, lty="dashed")+
  stat_function(data=subset(N_sum_s, nutrient=="NPSi"), fun=npsis)+
  stat_function(data=subset(N_sum_s, nutrient=="NSi"), fun=nsif, lty="dashed")+
  stat_function(data=subset(N_sum_s, nutrient=="NSi"), fun=nsis)

#summer, higher river mile = bigger response in mainstem (more upstream). Minimal response in tribs, except for NPSi
#fall: bigger response with river mile for NPSi and maybe N
#NSi no change with river mile, inhibition

#is limitation predicted by water chem? yes, in summer
ggplot(N_sum_s, aes(x=NO3.mgNL, y=cr_nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=cr_nrr-se_cr_nrr, ymax=cr_nrr+se_cr_nrr))+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)
#yes, exponential decline in NRR with DIN and NO3.

nl<-function(x){1.568-0.008*x}
ne<-function(x){1.63*exp(x*-0.0077)}

ggplot(N_sum_s, aes(x=N.P.ratio, y=cr_nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_bw()+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+ facet_wrap(~nutrient)+
  geom_errorbar(aes(ymin=cr_nrr-se_cr_nrr, ymax=cr_nrr+se_cr_nrr))+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)+geom_vline(xintercept = 16)+
  stat_function(data=subset(N_sum_s, nutrient=="N"), fun=nl)+
  stat_function(data=subset(N_sum_s, nutrient=="N"), fun=ne)
#yes, linear decline in NRR with N:P ratio, although N limitation continues past Redfield ratio

ggplot(N_sum_s, aes(x=N.Si.ratio, y=cr_nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=cr_nrr-se_cr_nrr, ymax=cr_nrr+se_cr_nrr))+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)+geom_vline(xintercept = 0.8)

#stats - linear models 
rm1<-lm(cr.nrr~river_mile, data=subset(N_sponge, season=="summer"| nutrient=="N"))
summary(rm1) #r2=0.133, p = 0.000000001, AIC = 505

rmf<-lm(cr.nrr~river_mile, data=subset(N_sponge, season=="fall"| nutrient=="N"))
summary(rmf) #r2 = 0.13, p = 0.000000003

rm2<-lm(cr.nrr~river_mile, data=subset(N_sponge, season=="summer"| nutrient=="NP"))
summary(rm2)#r2 = 0.141, p = 0.0000000004, AIC = 508

rmf2<-lm(cr.nrr~river_mile, data=subset(N_sponge, season=="fall"| nutrient=="NP"))
summary(rmf2) #r2 = 0.127, p = 0.000000006

rm3<-lm(cr.nrr~river_mile, data=subset(N_sponge, season=="summer"| nutrient=="NPSi"))
summary(rm3)#r2 = 0.145, p = 0.0000000002, AIC = 499.9

rmf3<-lm(cr.nrr~river_mile, data=subset(N_sponge, season=="fall"| nutrient=="NPSi"))
summary(rmf3) #r2 = 0.123, p = 0.00000001

rm4<-lm(cr.nrr~river_mile, data=subset(N_sponge, season=="summer"| nutrient=="NSi"))
summary(rm4) #r2 = 0.09, p = 0.0000008, AIC = 564

rmf4<-lm(cr.nrr~river_mile, data=subset(N_sponge, season=="fall"| nutrient=="NSi"))
summary(rmf4) #r2 = 0.06, p = 0.00007



np1<-lm(cr.nrr~N.P.ratio, data=subset(N_sponge, season=="summer"| nutrient=="N"))
summary(np1) #r2=0.094, p = 0.000003, aic=561

np2<-lm(cr.nrr~N.P.ratio, data=subset(N_sponge, season=="summer"| nutrient=="NP"))
summary(np2)#r2 = 0.094, p = 0.0000004, aic=521

np3<-lm(cr.nrr~N.P.ratio, data=subset(N_sponge, season=="summer"| nutrient=="NPSi"))
summary(np3) #r2 = 0.103, p = 0.0000001, aic=512

np<-lm(cr.nrr~N.P.ratio, data=subset(N_sponge, season=="summer"| !nutrient=="NSi"))
summary(np) #r2 = 0.08, p = 0.000000089, aic=656

#stats - exponential models
enp1<-gnls(cr.nrr~a*exp(N.P.ratio*b), data=subset(N_sponge, season=="summer"| nutrient=="N"), 
           start=list(a=2.8, b=-0.02),na.action=na.omit)
summary(enp1) #AIC = 513

enp2<-gnls(cr.nrr~a*exp(N.P.ratio*b), data=subset(N_sponge, season=="summer"| nutrient=="NP"), 
           start=list(a=2.8, b=-0.02),na.action=na.omit)
summary(enp2) #AIC = 518

enp3<-gnls(cr.nrr~a*exp(N.P.ratio*b), data=subset(N_sponge, season=="summer"| nutrient=="NPSi"), 
           start=list(a=2.8, b=-0.02),na.action=na.omit)
summary(enp3) #AIC = 508

enp<-gnls(cr.nrr~a*exp(N.P.ratio*b), data=subset(N_sponge, season=="summer"| !nutrient=="NSi"),
          start=list(a=2.8, b=-0.02),na.action=na.omit)
summary(enp)#AIC = 651

#conclusion: exponential models for individual nutrients best fit.


#N and chla                                                                                              
ggplot(N_sum_g, aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)
#wenas dwarfs all others - scale so that patterns at other sites visible. Reecer also high for NSi

ggplot(N_sum_g, aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)+scale_y_continuous(limits=c(0,5))
#fall: no response and no pattern with river mile - all waver around 1
#summer: really strong response in some tribs (wenas) and also some mainstem (Cle Elum)
#aside from those, no real patterns with river mile. Instead, scattered responses to N addition.

ggplot(N, aes(x=position, y=chla.nrr))+geom_point(aes(color=factor(type)))+theme_classic()+
  facet_wrap(~season)
#results at toppenish dwarfing all else and hard to see patterns
ggplot(N, aes(x=position, y=chla.nrr))+geom_point(aes(color=factor(type)))+theme_classic()+
  facet_wrap(~season)+scale_y_continuous(limits=c(0,20))
#no real patterns by river mile

trib<-subset(N, type=="trib")
unique(trib$stream)
unique(trib$position)
unique(trib$river_mile)
main<-subset(N, type=="mainstem")
unique(main$river_mile)

#####
#P limitation
######
ggplot(P, aes(x=season, y=cr.nrr))+geom_boxplot(aes(color=factor(type)))+theme_classic()
#In fall, tribs inhibited by P; mainstem neutral. In summer, no difference.

t.test(cr.nrr~type, data=subset(P, season=="fall"))
#t = 4.2868, df = 178.33, p-value = 2.962e-05
#95 percent confidence interval:0.1070222 0.2895987
#  mean in group mainstem     mean in group trib 
#1.0442496              0.8459391 

t.test(cr.nrr~type, data=subset(P, season=="summer"))
#t = 1.0391, df = 175.55, p-value = 0.3002
#95 percent confidence interval:  -0.07066581  0.22781283
#  mean in group mainstem     mean in group trib 
#1.206963               1.128389 

t.test(cr.nrr~season, data=subset(P, type=="mainstem"))
#t = -2.1914, df = 177.94, p-value = 0.02972
#95 percent confidence interval:  -0.30923885 -0.01618744
#  mean in group fall mean in group summer 
#1.044250             1.206963 

t.test(cr.nrr~season, data=subset(P, type=="trib"))
#t = -5.8326, df = 133.73, p-value = 3.899e-08
#95 percent confidence interval:  -0.3782296 -0.1866705
#  mean in group fall mean in group summer 
#0.8459391            1.1283892

P_sum_s$nutrient<-factor(P_sum_s$nutrient,levels= c("P", "NP", "NPSi", "PSi"))

ggplot(P_sum_s, aes(x=river_mile, y=cr_nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=cr_nrr-se_cr_nrr, ymax=cr_nrr+se_cr_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)
#only a response when N present
#only a response in summer, except Cle Elum
#in summer, general increase in response with river mile for NP and NPSi, but some variation

P_sum_g$nutrient<-factor(P_sum_g$nutrient,levels= c("P", "NP", "NPSi", "PSi"))
ggplot(P_sum_g, aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)
#high value from Wenas obscures others

ggplot(P_sum_g, aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)+scale_y_continuous(limits=c(0,10))
#with the exception of Wenas, mostly limitation occurs when N present.
#limitation almost always occurs in summer.

######
#Si
######

Si_sum_s$nutrient<-factor(Si_sum_s$nutrient,levels= c("Si", "NSi", "NPSi", "PSi"))

ggplot(Si_sum_s, aes(x=river_mile, y=cr_nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=cr_nrr-se_cr_nrr, ymax=cr_nrr+se_cr_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)
#neutral or inhibition, no patterns in mainstem vs. tribs or with location, except NPSi

Si_sum_g$nutrient<-factor(Si_sum_g$nutrient,levels= c("Si", "NSi", "NPSi", "PSi"))

ggplot(Si_sum_g, aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)
#high values from Wenas, Reecer, Cle Elum
#limitation only in summer
#no apparent pattern with geography

ggplot(Si_sum_g, aes(x=river_mile, y=chl_a.nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_classic()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=6)+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+geom_hline(yintercept = 1)+scale_y_continuous(limits=c(0,5))

####################
#water chem figures
####################
library(gridExtra)

NO3<-ggplot(nds_chem, aes(x=river_mile, y = NO3.mgNL))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("NO3-")

SRP<-ggplot(nds_chem, aes(x=river_mile, y = oP.mgPL))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("SRP")

Si<-ggplot(nds_chem, aes(x=river_mile, y = Si.mgL))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("Si")

NH4<-ggplot(nds_chem, aes(x=river_mile, y = NH4.mgNL))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("NH4")

DOC<-ggplot(nds_chem, aes(x=river_mile, y = DOC.mgL))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("DOC")

TDN<-ggplot(nds_chem, aes(x=river_mile, y = TDN.mgL))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("TDN")

grid.arrange(NO3, SRP, Si, NH4, DOC, TDN, ncol=3)

NP<-ggplot(nds_chem, aes(x=river_mile, y = N.P.ratio))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("N:P")+geom_hline(yintercept = 16)

NSi<-ggplot(nds_chem, aes(x=river_mile, y = N.Si.ratio))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("N:Si")

PSi<-ggplot(nds_chem, aes(x=river_mile, y = P.Si.ratio))+geom_point(aes(shape=factor(type), color=factor(season)), size=3)+
  theme_bw()+theme(legend.position="none", axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_color_manual(values=c("goldenrod2", "orchid3"))+ggtitle("P:Si")

grid.arrange(NP, NSi, PSi, ncol=3)

###########
#water chem stats
#########
chem<-read.csv("chem_summary.csv")
chem$river_mile<-ifelse(chem$stream=="ahtanum", 106, chem$river_mile)

#ratios
chem$DIN.mgNL<-chem$NH4.mgNL+chem$NO3.mgNL
chem$N.P.ratio<-(chem$DIN.mgNL/14)/(chem$oP.mgPL/31)
chem$N.Si.ratio<-(chem$DIN.mgNL/14)/(chem$Si.mgL/28)
chem$P.Si.ratio<-(chem$oP.mgPL/31)/(chem$Si.mgL/28)

#How does water chem compare summer to fall?
#paired t-tests. first need to shift to wide
chem_wide<-reshape(chem, timevar="season", v.names=c("Si.mgL", "oP.mgPL", "NH4.mgNL", "NO3.mgNL", "DOC.mgL", "TDN.mgL",
                                                     "DIN.mgNL", "N.P.ratio", "N.Si.ratio", "P.Si.ratio"), 
                   idvar=c("stream", "type", "river_mile"), direction="wide", drop="position")
#drop toppenish b/c no pair
chem_wide<-subset(chem_wide, !(stream=="toppenish"))

#all data combined
t.test(chem_wide$Si.mgL.fall, chem_wide$Si.mgL.summer, paired=T)#t = 4.0888, df = 9, p-value = 0.002722
t.test(chem_wide$oP.mgPL.fall, chem_wide$oP.mgPL.summer, paired = T)#t = 1.6701, df = 9, p-value = 0.1292
t.test(chem_wide$NH4.mgNL.fall, chem_wide$NH4.mgNL.summer, paired = T)#t = -0.93564, df = 9, p-value = 0.3739
t.test(chem_wide$NO3.mgNL.fall, chem_wide$NO3.mgNL.summer, paired = T)#t = 0.9758, df = 9, p-value = 0.3547
t.test(chem_wide$DIN.mgNL.fall, chem_wide$DIN.mgNL.summer, paired = T)#t = 0.38253, df = 9, p-value = 0.7109
t.test(chem_wide$TDN.mgL.fall, chem_wide$TDN.mgL.summer, paired = T)#t = -1.4657, df = 9, p-value = 0.1768
t.test(chem_wide$DOC.mgL.fall, chem_wide$DOC.mgL.summer, paired = T)#t = 2.2534, df = 9, p-value = 0.05072
t.test(chem_wide$N.P.ratio.fall, chem_wide$N.P.ratio.summer, paired = T)#t = -1.6554, df = 9, p-value = 0.1322
t.test(chem_wide$N.Si.ratio.fall, chem_wide$N.Si.ratio.summer, paired = T)#t = -0.24664, df = 9, p-value = 0.8107
t.test(chem_wide$P.Si.ratio.fall, chem_wide$P.Si.ratio.summer, paired = T)#t = 1.9864, df = 9, p-value = 0.078252

#So: When all combined, only Si has a consistent seasonal pattern of more Si in fall.

#mainstem only
chem_m<-subset(chem_wide, type=="mainstem")

t.test(chem_m$Si.mgL.fall, chem_m$Si.mgL.summer, paired = T) #t = 6.3335, df = 5, p-value = 0.001447
t.test(chem_m$oP.mgPL.fall, chem_m$oP.mgPL.summer, paired = T)#t = 5.602, df = 5, p-value = 0.002504
t.test(chem_m$NH4.mgNL.fall, chem_m$NH4.mgNL.summer, paired = T)#t = 0.29006, df = 5, p-value = 0.7834
t.test(chem_m$NO3.mgNL.fall, chem_m$NO3.mgNL.summer, paired = T)#t = 3.2675, df = 5, p-value = 0.02226
t.test(chem_m$DIN.mgNL.fall, chem_m$DIN.mgNL.summer, paired=T)#t = 2.3745, df = 5, p-value = 0.0636
t.test(chem_m$TDN.mgL.fall, chem_m$TDN.mgL.summer, paired = T)#t = -0.68538, df = 5, p-value = 0.5236
t.test(chem_m$DOC.mgL.fall, chem_m$DOC.mgL.summer, paired = T)#t = 2.1604, df = 5, p-value = 0.08314
t.test(chem_m$N.P.ratio.fall, chem_m$N.P.ratio.summer, paired = T)#t = -1.36, df = 5, p-value = 0.2319
t.test(chem_m$N.Si.ratio.fall, chem_m$N.Si.ratio.summer, paired = T)#t = 1.3671, df = 5, p-value = 0.2299
t.test(chem_m$P.Si.ratio.fall, chem_m$P.Si.ratio.summer, paired = T)#t = 3.4637, df = 5, p-value = 0.01797

#So: in the mainstem, Si, NO3, and SRP decrease summer-fall. P decreases more than Si - P:Si decreases summer-fall

#tribs only
chem_t<-subset(chem_wide, type=="trib")
t.test(chem_t$Si.mgL.fall, chem_t$Si.mgL.summer, paired = T) #t = 1.466, df = 3, p-value = 0.2389
t.test(chem_t$oP.mgPL.fall, chem_t$oP.mgPL.summer, paired = T)#t = 0.41106, df = 3, p-value = 0.7086
t.test(chem_t$NH4.mgNL.fall, chem_t$NH4.mgNL.summer, paired = T)#t = -1.8817, df = 3, p-value = 0.1564
t.test(chem_t$NO3.mgNL.fall, chem_t$NO3.mgNL.summer, paired = T)# = -0.59059, df = 3, p-value = 0.5963
t.test(chem_t$DIN.mgNL.fall, chem_t$DIN.mgNL.summer, paired=T)#t = -1.042, df = 3, p-value = 0.374
t.test(chem_t$TDN.mgL.fall, chem_t$TDN.mgL.summer, paired = T)#t = -1.6188, df = 3, p-value = 0.2039
t.test(chem_t$DOC.mgL.fall, chem_t$DOC.mgL.summer, paired = T)#t = 1.139, df = 3, p-value = 0.3374
t.test(chem_t$N.P.ratio.fall, chem_t$N.P.ratio.summer, paired = T)#t = -0.81551, df = 3, p-value = 0.4745
t.test(chem_t$N.Si.ratio.fall, chem_t$N.Si.ratio.summer, paired = T)#t = -1.1372, df = 3, p-value = 0.3381
t.test(chem_t$P.Si.ratio.fall, chem_t$P.Si.ratio.summer, paired = T)#t = 0.10447, df = 3, p-value = 0.9234

#no consistent relationships across tributaries, which indicates different mechanisms at each tributary site

#effect of river mile
library(mgcv)

#different values in summer vs fall so makes sense to have separate equations. but what if all in one?
nit1<-lm(NO3.mgNL~river_mile+type + season, data=chem) 
summary(nit1)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1.654855   0.269214   6.147 1.07e-05 ***
#  river_mile   -0.010260   0.002060  -4.982 0.000114 ***
#  typetrib      0.017424   0.179390   0.097 0.923762    
#seasonsummer  0.009767   0.177784   0.055 0.956830  

#Residual standard error: 0.4061 on 17 degrees of freedom
#Multiple R-squared:  0.5938,	Adjusted R-squared:  0.5221 
#F-statistic: 8.284 on 3 and 17 DF,  p-value: 0.001293

nit2<-lm(NO3.mgNL~river_mile, data=chem)
summary(nit2)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.667390   0.228204   7.307 6.27e-07 ***
#  river_mile  -0.010260   0.001948  -5.267 4.40e-05 ***

#Residual standard error: 0.3843 on 19 degrees of freedom
#Multiple R-squared:  0.5935,	Adjusted R-squared:  0.5721 
#F-statistic: 27.74 on 1 and 19 DF,  p-value: 4.395e-05

nit3<-lm(NO3.mgNL~river_mile, data=subset(chem, type=="mainstem"))
summary(nit3)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.67014    0.17117   9.757 1.99e-06 ***
#  river_mile  -0.01036    0.00143  -7.242 2.78e-05 ***
#Residual standard error: 0.2522 on 10 degrees of freedom
#Multiple R-squared:  0.8399,	Adjusted R-squared:  0.8239 
#F-statistic: 52.45 on 1 and 10 DF,  p-value: 2.783e-05

nit4<-lm(NO3.mgNL~river_mile+season, data=subset(chem, type=="mainstem")) #BEST MODEL for mainstem NO3-#
summary(nit4)
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1.800755   0.161481  11.151 1.43e-06 ***
#  river_mile   -0.010356   0.001241  -8.343 1.58e-05 ***
#  seasonsummer -0.261236   0.126427  -2.066   0.0688 .  

#Residual standard error: 0.219 on 9 degrees of freedom
#Multiple R-squared:  0.8914,	Adjusted R-squared:  0.8673 
#F-statistic: 36.94 on 2 and 9 DF,  p-value: 4.584e-05

nit5<-lm(NO3.mgNL~river_mile, data=subset(chem, type=="mainstem"| season=="summer"))
summary(nit5)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.818081   0.235173   7.731 1.31e-06 ***
#  river_mile  -0.011222   0.002005  -5.596 5.10e-05 ***
#Residual standard error: 0.3766 on 15 degrees of freedom
#Multiple R-squared:  0.6761,	Adjusted R-squared:  0.6545 
#F-statistic: 31.31 on 1 and 15 DF,  p-value: 5.104e-05

nit6<-lm(NO3.mgNL~river_mile, data=subset(chem, type=="mainstem"| season=="fall"))
summary(nit6)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.510794   0.184218   8.201 1.02e-06 ***
#  river_mile  -0.009335   0.001549  -6.027 3.10e-05 ***

#Residual standard error: 0.2887 on 14 degrees of freedom
#Multiple R-squared:  0.7218,	Adjusted R-squared:  0.702 
#F-statistic: 36.33 on 1 and 14 DF,  p-value: 3.105e-05
AIC(nit6) #9.5
AIC(nit5) #18.92
AIC(nit4)#2.15
AIC(nit3)#4.8
AIC(nit2)#23.3
AIC(nit1)#27.3

#So: According to AIC and r2, the best model for mainstem NO3- is river mile and season

nit7<-lm(NO3.mgNL~river_mile, data=subset(chem, type=="trib"))
summary(nit7)
#Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  1.637268   0.716290   2.286   0.0562 .
#river_mile  -0.009892   0.006302  -1.570   0.1605 
#Residual standard error: 0.5564 on 7 degrees of freedom
#Multiple R-squared:  0.2603,	Adjusted R-squared:  0.1546 
#F-statistic: 2.463 on 1 and 7 DF,  p-value: 0.1605

nit8<-lm(NO3.mgNL~river_mile+season, data=subset(chem, type=="trib"))
summary(nit8)
#Estimate Std. Error t value Pr(>|t|)
#(Intercept)   1.362016   0.763788   1.783    0.125
#river_mile   -0.009311   0.006311  -1.475    0.191
#seasonsummer  0.380698   0.373767   1.019    0.348

#Residual standard error: 0.5549 on 6 degrees of freedom
#Multiple R-squared:  0.3694,	Adjusted R-squared:  0.1591 
#F-statistic: 1.757 on 2 and 6 DF,  p-value: 0.2508

#continue with geographic patterns in water chem#
si1<-lm(Si.mgL~river_mile+season, data=subset(chem, type=="mainstem"))
summary(si1)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  26.212963   1.299183  20.177  8.4e-09 ***
#  river_mile   -0.085330   0.009987  -8.544  1.3e-05 ***
#  seasonsummer -4.465881   1.017155  -4.391  0.00174 **

#Residual standard error: 1.762 on 9 degrees of freedom
#Multiple R-squared:  0.9111,	Adjusted R-squared:  0.8914 
#F-statistic: 46.14 on 2 and 9 DF,  p-value: 1.859e-05
AIC(si1) #52.2

si2<-lm(Si.mgL~river_mile, data=subset(chem, type=="mainstem"))
summary(si2)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 23.98002    2.01033  11.928 3.09e-07 ***
#  river_mile  -0.08533    0.01679  -5.081 0.000477 ***

#Residual standard error: 2.963 on 10 degrees of freedom
#Multiple R-squared:  0.7208,	Adjusted R-squared:  0.6929 
#F-statistic: 25.82 on 1 and 10 DF,  p-value: 0.0004771
AIC(si2)#63.9

#polynomial relationship?
si3<-gnls(Si.mgL~a*river_mile^2 + b*river_mile, subset(chem, type=="mainstem"), start=list(a=-0.0005, b= 0.02))
summary(si3)
# AIC      BIC    logLik
#81.11475 82.56947 -37.55737

#Residual standard error: 6.06178 
#Degrees of freedom: 12 total; 10 residual

#SO: best model, according to AIC, is the linear model that includes season.

amm1<-lm(NH4.mgNL~river_mile + season + type, chem)
summary(amm1)
#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   0.4339907  0.1204583   3.603   0.0022 **
# river_mile   -0.0028317  0.0009215  -3.073   0.0069 **
#seasonsummer  0.0612678  0.0795485   0.770   0.4518   
#typetrib      0.0835045  0.0802668   1.040   0.3128

#Residual standard error: 0.1817 on 17 degrees of freedom
#Multiple R-squared:  0.3979,	Adjusted R-squared:  0.2917 
#F-statistic: 3.745 on 3 and 17 DF,  p-value: 0.03118
AIC(amm1) #-6.463

amm2<-lm(NH4.mgNL~river_mile+type, chem)
summary(amm2)
AIC(amm2) #-7.74

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.4666526  0.1114654   4.187 0.000555 ***
# river_mile  -0.0028504  0.0009108  -3.130 0.005790 ** 
#typetrib     0.0869353  0.0792323   1.097 0.287007   

#Residual standard error: 0.1797 on 18 degrees of freedom
#Multiple R-squared:  0.3769,	Adjusted R-squared:  0.3077 
#F-statistic: 5.444 on 2 and 18 DF,  p-value: 0.01416

amm3<-lm(NH4.mgNL~river_mile, chem)
summary(amm3)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.5021028  0.1072511   4.682 0.000163 ***
# river_mile  -0.0028338  0.0009155  -3.095 0.005957 ** 

#Residual standard error: 0.1806 on 19 degrees of freedom
#Multiple R-squared:  0.3352,	Adjusted R-squared:  0.3002 
#F-statistic: 9.581 on 1 and 19 DF,  p-value: 0.005957
AIC(amm3) #-8.4

amm4<-lm(NH4.mgNL~river_mile, subset(chem, season=="fall"))
summary(amm4)
AIC(amm4)#-14.76
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.4636700  0.0819584   5.657 0.000477 ***
#  river_mile  -0.0027979  0.0006917  -4.045 0.003710 ** 

#Residual standard error: 0.0958 on 8 degrees of freedom
#Multiple R-squared:  0.6716,	Adjusted R-squared:  0.6306 
#F-statistic: 16.36 on 1 and 8 DF,  p-value: 0.00371

amm4b<-lm(NH4.mgNL~river_mile, subset(chem, season=="summer"))
summary(amm4b)
#r2=0.15, p = 0.133, AIC = 3.72

amm5<-lm(NH4.mgNL~river_mile, subset(chem, type=="trib"))
summary(amm5)
#Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  0.889778   0.215904   4.121  0.00445 **
#  river_mile  -0.005913   0.001900  -3.113  0.01702 *

#Residual standard error: 0.1677 on 7 degrees of freedom
#Multiple R-squared:  0.5805,	Adjusted R-squared:  0.5206 
#F-statistic: 9.688 on 1 and 7 DF,  p-value: 0.01702

AIC(amm5) #-2.86

amm5a<-lm(NH4.mgNL~river_mile, subset(chem, type=="mainstem"))
summary(amm5a) #r2 = 0.25, p = 0.06

#so: NH4 decreases with river mile in the fall but not the summer. relationship true for all sites together and for tribs,
#but not for mainstem alone

srp1<-lm(oP.mgPL~river_mile+season+type, chem)
summary(srp1)
#Estimate Std. Error t value Pr(>|t|)  
#(Intercept)   0.0615071  0.0221274   2.780   0.0128 *
#  river_mile   -0.0001519  0.0001693  -0.898   0.3819  
#seasonsummer -0.0205507  0.0146125  -1.406   0.1776  
#typetrib      0.0426181  0.0147445   2.890   0.0102 *

#Residual standard error: 0.03338 on 17 degrees of freedom
#Multiple R-squared:  0.3835,	Adjusted R-squared:  0.2747 
#F-statistic: 3.525 on 3 and 17 DF,  p-value: 0.03759
AIC(srp1) #-77.63

srp2<-gnls(oP.mgPL~a*river_mile^2 + b*river_mile, subset(chem, type=="mainstem"), start=list(a=-0.000002, b= 0.056))
summary(srp2)
#AIC      BIC   logLik
#-51.99662 -50.5419 28.99831

srp32<-gnls(oP.mgPL~a*river_mile^2 + b*river_mile, chem, start=list(a=-0.000002, b= 0.056))
summary(srp32)
#AIC      BIC   logLik
#-70.89606 -67.7625 38.44803

#  Value    Std.Error   t-value p-value
#a -0.000006278 0.0000024069 -2.608375  0.0173
#b  0.001255088 0.0003373271  3.720686  0.0014

srp3<-lm(oP.mgPL~river_mile, chem)
AIC(srp3)#-71.99

srp4<-lm(oP.mgPL~river_mile+type, chem)
AIC(srp4)#-77.3
summary(srp4)
#Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.0505516  0.0212660   2.377   0.0287 *
#  river_mile  -0.0001457  0.0001738  -0.838   0.4128  
#typetrib     0.0414674  0.0151164   2.743   0.0134 *

#Residual standard error: 0.03428 on 18 degrees of freedom
#Multiple R-squared:  0.3118,	Adjusted R-squared:  0.2353 
#F-statistic: 4.077 on 2 and 18 DF,  p-value: 0.03464

srp5<-lm(oP.mgPL~type, chem)
AIC(srp5)#-78.52
summary(srp5)

NP1<-lm(N.P.ratio~river_mile, chem)
summary(NP1)
AIC(NP1)#175.3
NP2<-lm(N.P.ratio~river_mile+season+type, chem)
AIC(NP2)#172.8
summary(NP2)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   79.65399    8.60269   9.259 4.73e-08 ***
#river_mile    -0.45423    0.06581  -6.902 2.56e-06 ***
#seasonsummer  10.03382    5.68106   1.766   0.0953 .  
#typetrib     -10.48549    5.73236  -1.829   0.0850 . 

#Residual standard error: 12.98 on 17 degrees of freedom
#Multiple R-squared:  0.7634,	Adjusted R-squared:  0.7217 
#F-statistic: 18.28 on 3 and 17 DF,  p-value: 1.455e-05

NP3<-lm(N.P.ratio~river_mile+type, chem)
AIC(NP3)#174
NP4<-lm(N.P.ratio~river_mile+season, chem)
AIC(NP4)#174


NS1<-lm(N.Si.ratio~river_mile+season+type, chem)
AIC(NS1) #-61
NS2<-lm(N.Si.ratio~river_mile, chem)
AIC(NS2)#-63
summary(NS2)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.2025709  0.0287633   7.043 1.05e-06 ***
# river_mile  -0.0012201  0.0002455  -4.970 8.51e-05 ***

#Residual standard error: 0.04844 on 19 degrees of freedom
#Multiple R-squared:  0.5652,	Adjusted R-squared:  0.5423 
#F-statistic:  24.7 on 1 and 19 DF,  p-value: 8.51e-05

###########################
#univariate relationships
###########################
N<-subset(nds_chem, N==1)
P<-subset(nds_chem, P==1)
Si<-subset(nds_chem, Si==1)

#N limitation/inhibition
ggplot(subset(N, nutrient=="N"), aes(x=DIN.mgNL, y=chla.nrr))+
  geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#no pattern

ggplot(subset(N, nutrient=="N"), aes(x=NO3.mgNL, y=chla.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#general increase with NO3 but lots of variability

ggplot(subset(N, nutrient=="N"), aes(x=NH4.mgNL, y=chla.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#general increase with NH4 

ggplot(subset(nds_chem, nutrient=="control"), aes(x=NH4.mgNL, y=chla))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#general increase in chla with NH4 but LOTS of variability; probably not much explanatory power

ggplot(subset(nds_chem, nutrient=="control"), aes(x=NO3.mgNL, y=chla))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#no pattern

ggplot(subset(N, nutrient=="N"), aes(x=NO3.mgNL, y=cr.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#negative exponential, both summer and fall. N< 0.5 mg/L to get limitation

ggplot(subset(N, nutrient=="N"), aes(x=NH4.mgNL, y=cr.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#limitation at very low end only, but even there some inhibition

ggplot(subset(nds_chem, nutrient=="control"), aes(x=NO3.mgNL, y=cr.area))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#no pattern

ggplot(subset(nds_chem, nutrient=="control"|nutrient=="C"), aes(x=NH4.mgNL, y=cr.area))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#general increase in CR with NH4


#P limitation/inhibition
ggplot(subset(P, nutrient=="P"), aes(x=oP.mgPL, y=chla.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#in summer, some intermediate values had limitation. P is not driving this system!

ggplot(subset(nds_chem, nutrient=="control"|nutrient=="C"), aes(x=oP.mgPL, y=chla))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#no pattern

ggplot(subset(P, nutrient=="P"), aes(x=oP.mgPL, y=cr.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#maybe a faint upward relationship? but soo variable

ggplot(subset(nds_chem, nutrient=="control"|nutrient=="C"), aes(x=oP.mgPL, y=cr.area))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#increase in summer CR with P (but variable)

#Si limitation/inhibition
ggplot(subset(Si, nutrient=="Si"), aes(x=Si.mgL, y=chla.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))+geom_hline(yintercept=1)
#in summer, occasional spikes but no pattern to the spikes 

ggplot(subset(nds_chem, nutrient=="control"|nutrient=="C"), aes(x=Si.mgL, y=chla))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#up and down with no real pattern

ggplot(subset(Si, nutrient=="Si"), aes(x=Si.mgL, y=cr.nrr))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#general upward relationship  with Si, especially in summer, but variable

ggplot(subset(nds_chem, nutrient=="control"|nutrient=="C"), aes(x=Si.mgL, y=cr.area))+geom_point(aes(color=factor(season)))+
  theme_classic()+scale_color_manual(values=c("goldenrod2", "plum3"))
#maybe hump-shaped? with max CR at medium Si - but lots of variation

#N:P ratios
ggplot(subset(nds_chem, nutrient=="control"|nutrient=="C"), aes(x=N.P.ratio, y=chla))+theme_classic()+
  geom_point(aes(color=factor(season)))+scale_color_manual(values=c("goldenrod2", "plum3"))+
  geom_vline(xintercept = 16)
#increase!

ggplot(N, aes(x=N.P.ratio, y=chla.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()+geom_vline(xintercept = 16)
#In summer: negative exponential (lower N:P = greater response to N addition). greatest increase with N alone
#In fall: no change

ggplot(P, aes(x=N.P.ratio, y=chla.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()+geom_vline(xintercept = 16)

ggplot(subset(nds_chem, nutrient=="control"), aes(x=N.P.ratio, y=cr.area))+theme_classic()+
  geom_point(aes(color=factor(season)))+scale_color_manual(values=c("goldenrod2", "plum3"))+
  geom_vline(xintercept = 16)
#increase to threshold

ggplot(N, aes(x=N.P.ratio, y=cr.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()+geom_vline(xintercept = 16)
#In summer and fall, slight decline with N:P ratio. No response with NSi

#N:Si ratios
ggplot(subset(nds_chem, nutrient=="control"), aes(x=N.Si.ratio, y=chla))+theme_classic()+
  geom_point(aes(color=factor(season)))+scale_color_manual(values=c("goldenrod2", "plum3"))+
  geom_vline(xintercept = 0.8)
#increase in chla with N:Si ratio, in both fall and summer

ggplot(N, aes(x=N.Si.ratio, y=chla.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()+geom_vline(xintercept = 0.8)

ggplot(Si, aes(x=N.Si.ratio, y=chla.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()+geom_vline(xintercept = 0.8)
#lowerN:Si ratio = higher chla, espeically in NPSi and Si (??? why would adding more Si help?)

ggplot(subset(nds_chem, nutrient=="control"), aes(x=N.Si.ratio, y=cr.area*-1))+theme_classic()+
  geom_point(aes(color=factor(season)))+scale_color_manual(values=c("goldenrod2", "plum3"))+
  geom_vline(xintercept = 0.8)
#|CR| increases with N:Si

ggplot(Si, aes(x=N.Si.ratio, y=cr.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()+geom_vline(xintercept = 0.8)
#decreases with N:Si ratio but only because of NPSi in summer. so basically no pattern

#P:Si ratios
ggplot(subset(nds_chem, nutrient=="control"), aes(x=P.Si.ratio, y=chla))+theme_classic()+
  geom_point(aes(color=factor(season)))+scale_color_manual(values=c("goldenrod2", "plum3"))+
  geom_vline(xintercept = 0.05)
#all sites far below Redfield P:Si ratio of 0.05. General increase with P:Si ratio but high variability

ggplot(P, aes(x=P.Si.ratio, y=chla.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()
#higher chla at low P:Si ratios, esp for NP

ggplot(Si, aes(x=P.Si.ratio, y=chla.nrr))+ geom_point(aes(color=factor(season), shape=factor(nutrient)))+
  scale_color_manual(values=c("goldenrod2", "plum3"))+theme_classic()
#no clear patterns

