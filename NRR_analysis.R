#packages
library(nlme)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lmtest)
library(lattice)

#data (created in "all sites" by combining NDS files and water chem file)
nds_chem<-read.csv("NDS_chem_all.csv")
nds_chem$river_mile<-ifelse(nds_chem$stream=="ahtanum", 106, nds_chem$river_mile)

nds_s<-subset(nds_chem, top=="sponge")
nds_s <- subset(nds_s, !(is.na(nds_s$cr.nrr)))

#NRR model, based on plots
#start with fixed effects and then test if adding random effects improves the model
#M1 is all the variables and interactions (except nutrient, which we want to model as a random effect)
M1<-lm(log(cr.nrr+1)~river_mile*season + type*river_mile, nds_s) #needed to log nrr to account for heteroscedasticity in residuals
plot(M1)
E<-rstandard(M1)
boxplot(E~stream, data=nds_s)
abline(0,0)
#none of the boxplots are completely above or below the 0 line, so don't need to include stream as random variable.

#now, use gls so can compare with lme
M1<-gls(log(cr.nrr+1)~river_mile*season + type*river_mile, nds_s)
M1a<-lme(log(cr.nrr+1)~river_mile*season + type*river_mile,random = ~1+river_mile|nutrient, 
         method="REML", nds_s)

anova(M1, M1a)
#       Model df   AIC        BIC    logLik   Test  L.Ratio p-value
#M1      1  7  -19.91584   12.79725  16.95792                        
#M1a     2 10 -280.61663 -233.88365 150.30832 1 vs 2 266.7008  <.0001

#SO much better with random effects. Now: check the random structure. try a random intercept - meaning the slope
#is consistent among nutrient treatments but the slope is not 
#(so changes with river mile in same way, but overall response is higher or lower)

M1b<-lme(log(cr.nrr+1)~river_mile*season + type*river_mile,random = ~1|nutrient, 
         method="REML", nds_s)
anova(M1a, M1b)
#   Model df       AIC       BIC   logLik   Test  L.Ratio p-value
#M1a     1 10 -280.6166 -233.8836 150.3083                        
#M1b     2  8 -206.2294 -168.8430 111.1147 1 vs 2 78.38723  <.0001

#random slope and intercept is best

#now optimize the fixed part
summary(M1a)
#                           Value  Std.Error  DF   t-value p-value
#(Intercept)              0.4326074 0.03912778 784 11.056273  0.0000
#river_mile               0.0016092 0.00062091 784  2.591679  0.0097
#seasonsummer             0.1054820 0.03818589 784  2.762328  0.0059
#typetrib                 0.2675061 0.04479698 784  5.971522  0.0000
#river_mile:seasonsummer  0.0000384 0.00032168 784  0.119479  0.9049
#river_mile:typetrib     -0.0024235 0.00038684 784 -6.264750  0.0000

#consider dropping river mile*season interaction. to test, compare logLikelihood ratio
M1a<-lme(log(cr.nrr+1)~river_mile*season + type*river_mile,random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
M2a<-lme(log(cr.nrr+1)~river_mile+season + type*river_mile,random = ~1+river_mile|nutrient, 
         method="REML", nds_s)

lrtest(M1a, M2a) #to compare the two models
#Model 1: log(cr.nrr + 1) ~ river_mile * season + type * river_mile
#Model 2: log(cr.nrr + 1) ~ river_mile + season + type * river_mile
#   Df LogLik Df  Chisq Pr(>Chisq)    
#1  10 150.31                         
#2   9 157.43 -1 14.232  0.0001616 ***
#conclude that river_mile*season interaction not significant b/c model is better without it

#next, drop interaction b/n river mile and stream typ.
M3a<-lme(log(cr.nrr+1)~river_mile +season + type,random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
lrtest(M2a, M3a)
#Model 1: log(cr.nrr + 1) ~ river_mile + season + type * river_mile
#Model 2: log(cr.nrr + 1) ~ river_mile + season + type
#Df LogLik Df  Chisq Pr(>Chisq)    
#1   9 157.43                         
#2   8 145.18 -1 24.499  7.433e-07 ***
#conclude that river_mile*type interaction significant. Best model still M2a

#next, drop season
M4a<-lme(log(cr.nrr+1)~river_mile +type*river_mile,random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
lrtest(M2a, M4a)
#Model 1: log(cr.nrr + 1) ~ river_mile + season + type * river_mile
#Model 2: log(cr.nrr + 1) ~ river_mile + type * river_mile
#Df LogLik Df  Chisq Pr(>Chisq)    
#1   9 157.43                         
#2   8 128.87 -1 57.104  4.133e-14 ***

#conclude that M2a still best
#now eliminate type
M5a<-lme(log(cr.nrr+1)~river_mile ,random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
lrtest(M2a, M5a)
#Model 1: log(cr.nrr + 1) ~ river_mile + season + type * river_mile
#Model 2: log(cr.nrr + 1) ~ river_mile
#Df LogLik Df Chisq Pr(>Chisq)    
#1   9 157.43                        
#2   6 118.75 -3 77.35  < 2.2e-16 ***

#CONCLUSION: best model: log(cr.nrr+1)~river_mile+season + type*river_mile,random = ~1+river_mile|nutrient)
#IOW: NRR for CR is predicted by river mile, season, and stream type, with a different relationship
#with river mile, depending on stream type. The slope and intercept vary for each nutrient treatment.

summary(M2a)
#Random effects:
#  Formula: ~1 + river_mile | nutrient
#Structure: General positive-definite, Log-Cholesky parametrization
#             StdDev      Corr  
#(Intercept) 0.072670706 (Intr)
#river_mile  0.001613624 -0.894
#Residual    0.189145517       

#Fixed effects: log(cr.nrr + 1) ~ river_mile + season + type * river_mile 
#                       Value  Std.Error  DF   t-value p-value
#(Intercept)          0.4304319 0.03463478 785 12.427738  0.0000
#river_mile           0.0016288 0.00059886 785  2.719851  0.0067
#seasonsummer         0.1097521 0.01345564 785  8.156583  0.0000
#typetrib             0.2673480 0.04474965 785  5.974304  0.0000
#river_mile:typetrib -0.0024225 0.00038652 785 -6.267546  0.0000

#Correlation: 
#                     (Intr) rvr_ml ssnsmm typtrb
#river_mile          -0.810                     
#seasonsummer        -0.193 -0.001              
#typetrib            -0.307  0.138 -0.065       
#river_mile:typetrib  0.267 -0.143  0.053 -0.953

#Standardized Within-Group Residuals:
#  Min          Q1         Med          Q3         Max 
#-3.85610090 -0.54560536  0.01397457  0.60660720  4.34598363 

#Number of Observations: 797
#Number of Groups: 8 

E<-resid(M2a)
F2<-fitted(M2a)
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(x=F2, y=E, xlab="Fitted values", ylab="Residuals")
boxplot(E~type, data=nds_s, main="Type", ylab="Residuals")
boxplot(E~season, data=nds_s, main="Season", ylab="Residuals")
plot(x=nds_s$river_mile, y=E, xlab="River Mile", ylab="Residuals")
#no obvious pattern in residuals, but next need to also try a LOESS smoother

xyplot(E~river_mile|type*season, data=nds_s, ylab="Residuals", xlab="River Mile", 
       panel=function(x,y){panel.grid(h=-1, v=2)
         panel.points(x,y,col=1)
         panel.loess(x,y,span=0.5, col=1, lwd=2)})
#not linear! need to use additive mixed model!

library(mgcv)
M2b<-gam(log(cr.nrr+1)~s(river_mile)+(season) + (type*river_mile),
          random = list(~1|nutrient,1+river_mile), data=nds_s)
summary(M2b)
#Family: gaussian 
#Link function: identity 

#Formula:
 # log(cr.nrr + 1) ~ s(river_mile) + (season) + (type * river_mile)

#Parametric coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          0.0535863  0.0034812  15.393  < 2e-16 ***
#  seasonsummer         0.1084128  0.0162421   6.675 4.65e-11 ***
#  typetrib             0.2679225  0.0540027   4.961 8.57e-07 ***
#  river_mile           0.0050247  0.0001102  45.582  < 2e-16 ***
#  typetrib:river_mile -0.0024313  0.0004664  -5.213 2.37e-07 ***
  ---
#Approximate significance of smooth terms:
#               edf Ref.df     F p-value    
#s(river_mile) 0.8758 0.8758 270.1  <2e-16 ***
  ---
# Rank: 13/14
#R-sq.(adj) =  0.112   Deviance explained = 11.7%
#GCV = 0.052487  Scale est. = 0.052158  n = 797

#edf = degrees of freedom for smoother. It's ~1, which suggests linear is ok
  plot(M2b)
#plot is linear
#note: Zuur et al. recommend using gamm (not gam), but I couldn't get it to work (error said "object river_mile not found")
#but I seem to be getting the expected type of output, so *shrug*?

######
#chla NRR

nds_g<-subset(nds_chem, top=="glass")
nds_g <- subset(nds_g, !(is.na(nds_g$chla.nrr)))

#start with fixed effects and then test if adding random effects improves the model
#M1 is all the variables and interactions (except nutrient, which we want to model as a random effect)
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
M1<-lm(log10(chla.nrr+1)~river_mile*season + type*river_mile, nds_g) #needed to log nrr to account for heteroscedasticity in residuals
plot(M1) #even with log-transformation, deviation from normal in Q-Q plot and heteroscedasticity
E<-rstandard(M1)
op<-par(mfrow=c(1,1))
boxplot(E~stream, data=nds_g)
abline(0,0)
#none of the boxplots are completely above or below the 0 line, so don't need to include stream as random variable.but note that 
#ahtanum and toppenshi just barely in there.

#now, use gls so can compare with lme
M1<-gls(log10(chla.nrr+1)~river_mile*season + type*river_mile, nds_g)
M1a<-lme(log10(chla.nrr+1)~river_mile*season + type*river_mile,random = ~1+river_mile|nutrient, 
         method="REML", nds_g)

anova(M1, M1a)
#     Model df       AIC       BIC   logLik   Test  L.Ratio p-value
#M1      1  7 -147.3034 -114.5111 80.65169                        
#M1a     2 10 -168.6338 -121.7876 94.31688 1 vs 2 27.33039  <.0001

#much better with random effects included
#now check random structure. try just the random slope

M1b<-lme(log10(chla.nrr+1)~river_mile*season + type*river_mile,random = ~1|nutrient, 
         method="REML", nds_g)

anova(M1a, M1b)
#     Model df       AIC       BIC   logLik   Test  L.Ratio p-value
#M1a     1 10 -168.6338 -121.7876 94.31688                        
#M1b     2  8 -163.7885 -126.3116 89.89425 1 vs 2 8.845259   0.012

#slightly better to include random slope and intercept.

#now optimize the fixed part
summary(M1a)
#                             Value  Std.Error  DF   t-value p-value
#(Intercept)              0.25342414 0.03039909 793  8.336570  0.0000
#river_mile              -0.00037070 0.00031055 793 -1.193682  0.2330
#seasonsummer            -0.00933374 0.03980442 793 -0.234490  0.8147
#typetrib                -0.05226620 0.04815416 793 -1.085393  0.2781
#river_mile:seasonsummer  0.00174109 0.00033813 793  5.149217  0.0000
#river_mile:typetrib      0.00112807 0.00041868 793  2.694329  0.0072

M2a<-lme(log10(chla.nrr+1)~river_mile*season + type*river_mile,random = ~1+river_mile|nutrient, 
         method="REML", nds_g)
