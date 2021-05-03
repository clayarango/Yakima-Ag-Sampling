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

########################################################################################
#chla NRR#
########################################################################################

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
#ahtanum and toppenish just barely in there.And much higher variance for Wenas and Rrecer than other sites.
#suggests that we should try: 1) allowing variation to increase with mean (varFixed) OR 2) allow to vary with stream (varIdent). 
#can skip the log10 tranformation to do this

M1<-gls(chla.nrr~river_mile*season+type*river_mile, data=nds_g)
vf1<-varIdent(form=~1|stream)
M1s<-gls(chla.nrr~river_mile*season+type*river_mile, data=nds_g, weights=vf1)
vf2<-varFixed(~chla.nrr)
M1sb<-gls(chla.nrr~river_mile*season+type*river_mile, data=nds_g, weights=vf2)
anova(M1, M1s, M1sb)
#     Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#M1      1  7 4374.147 4406.940 -2180.074                        
#M1s     2 17 2576.900 2656.538 -1271.450 1 vs 2  1817.247  <.0001
#M1sb    3  7 2372.097 2404.889 -1179.048 2 vs 3  184.8034  <.0001

#MUCH better when residuals allowed to vary, and best if vary with NRR
#but because better when allowed to vary with a continuous variable, need to check other structures, too

vf3<-varPower(form =~chla.nrr| stream)
M1sc<-gls(chla.nrr~river_mile*season+type*river_mile, data=nds_g, weights=vf3)
vf4<-varPower(form=~chla.nrr|season)
M1sd<-gls(chla.nrr~river_mile*season+type*river_mile, data=nds_g, weights=vf4)
vf5<-varPower(form=~chla.nrr|type)
M1se<-gls(chla.nrr~river_mile*season+type*river_mile, data=nds_g, weights=vf4)

anova(M1sb, M1sc, M1sd, M1se)
#       Model df    AIC      BIC     logLik   Test  L.Ratio p-value
#M1sb     1  7 2372.097 2404.889 -1179.0483                        
#M1sc     2 18 1727.281 1811.604  -845.6405 1 vs 2 666.8155  <.0001
#M1sd     3  9 1810.398 1852.559  -896.1987 2 vs 3 101.1164  <.0001
#M1se     4  9 1810.398 1852.559  -896.1987 

#SO: best variance structure allows variance to increase with NRR but only in certain streams

summary(M1sc)
#Generalized least squares fit by REML
#Model: chla.nrr ~ river_mile * season + type * river_mile 
#Data: nds_g 
#AIC      BIC   logLik
#1727.281 1811.604 -845.6405

#Variance function:
#Structure: Power of variance covariate, different strata
#Formula: ~chla.nrr | stream 
#Parameter estimates: (Sarah note: higher number means more variation in residuals allowed.)
#  ahtanum   century   cleelum     kiona    mabton    reecer    ringer      roza     satus toppenish     wenas 
#0.6832503 0.6061846 0.8524888 0.4007073 0.2211139 1.0919692 0.8137915 0.9296681 1.1071669 0.4652958 1.1601197 

#Coefficients:
#                         Value     Std.Error   t-value   p-value
#(Intercept)              0.8008404 0.06949103 11.524371  0.0000
#river_mile              -0.0035686 0.00048162 -7.409466  0.0000
#seasonsummer            -0.0280413 0.09560101 -0.293316  0.7694
#typetrib                -0.3964672 0.07814168 -5.073697  0.0000
#river_mile:seasonsummer  0.0042555 0.00081737  5.206399  0.0000
#river_mile:typetrib      0.0011701 0.00054263  2.156354  0.0314

#Correlation: 
#  (Intr) rvr_ml ssnsmm typtrb rvr_ml:s
#river_mile              -0.953                              
#seasonsummer            -0.530  0.521                       
#typetrib                -0.811  0.782  0.349                
#river_mile:seasonsummer  0.460 -0.486 -0.940 -0.335         
#river_mile:typetrib      0.772 -0.826 -0.347 -0.962  0.360  

#Standardized residuals:
#  Min         Q1        Med         Q3        Max 
#-3.5155154  0.1309112  0.8442069  1.1400711  2.6016745 

#Residual standard error: 0.6655261 
#Degrees of freedom: 806 total; 800 residual

#now, get random structure
M1a<-lme(chla.nrr~river_mile*season + river_mile*type, random=~1+river_mile|nutrient,
         weights = vf3, method="REML", data=nds_g)
#get error: nlminb problem, convergence error code = 1, message = iteration limit reached without convergence (10)
#guessing this is due to over-parameterization? maybe too many variables that used to model variation and/or random effects

#try just random slope, instead of random slope and intercept
M2a<-lme(chla.nrr~river_mile*season + river_mile*type, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)

anova(M1sc, M2a)
##    Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#M1sc     1 18 1727.281 1811.604 -845.6405                        
#M2a      2 19 1710.177 1799.185 -836.0885 1 vs 2 19.10411  <.0001

#improves when slope varies with nutrient category

#now: optimize the fixed portion!
summary(M2a)

M2b<-lme(chla.nrr~river_mile+season + river_mile*type, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)

lrtest(M2a, M2b)
##  Df  LogLik Df  Chisq Pr(>Chisq)
#1  19 -836.09                     
#2  18 -836.62 -1 1.0588     0.3035

summary(M2b)

M2c<-lme(chla.nrr~river_mile+season + type, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)
#doesn't converge

M2d<-lme(chla.nrr~river_mile + river_mile*type, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)
#doesn't converge

M2e<-lme(chla.nrr~river_mile+type, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)

lrtest(M2b, M2e)
##  Df  LogLik Df  Chisq Pr(>Chisq)    
#1  18 -836.62                         
#2  16 -898.13 -2 123.03  < 2.2e-16 ***
#M2b still better

M2f<-lme(chla.nrr~type+season, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)

lrtest(M2b, M2f)
##  Df  LogLik Df  Chisq Pr(>Chisq)   
#1  18 -836.62                        
#2  16 -842.38 -2 11.522   0.003148 **

M2g<-lme(chla.nrr~type, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)

lrtest(M2b, M2g)
#   Df  LogLik Df  Chisq Pr(>Chisq)    
#1  18 -836.62                         
#2  15 -908.79 -3 144.34  < 2.2e-16 ***

M2h<-lme(chla.nrr~season, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)

lrtest(M2b, M2h)
#   Df  LogLik Df  Chisq Pr(>Chisq)    
#1  18 -836.62                         
#2  15 -883.51 -3 93.775  < 2.2e-16 ***

#Best model is M2b
summary(M2b)
#Linear mixed-effects model fit by REML
#Data: nds_g 
#AIC      BIC    logLik
#1709.236 1793.581 -836.6179

#Random effects:
#  Formula: ~1 | nutrient
#(Intercept)  Residual
#StdDev:  0.05421041 0.6528178

#Variance function:
#  Structure: Power of variance covariate, different strata
#Formula: ~chla.nrr | stream 
#Parameter estimates:
#  ahtanum    century    cleelum      kiona     mabton     reecer     ringer       roza      satus  toppenish      wenas 
#0.79350627 0.63204766 0.90662707 0.22282314 0.06263507 1.10684423 0.86923056 0.95291056 1.11271175 0.33659866 1.16607644 
#Fixed effects: chla.nrr ~ river_mile + season + river_mile * type 
#                     Value  Std.Error    DF   t-value  p-value
#(Intercept)          0.7526378 0.06627973 794 11.355474  0.0000
#river_mile          -0.0031057 0.00042297 794 -7.342575  0.0000
#seasonsummer         0.4611727 0.03273352 794 14.088696  0.0000
#typetrib            -0.4392558 0.07392803 794 -5.941668  0.0000
#river_mile:typetrib  0.0015501 0.00050217 794  3.086829  0.0021
#Correlation: 
#                     (Intr) rvr_ml ssnsmm typtrb
#river_mile          -0.909                     
#seasonsummer        -0.288  0.204              
#typetrib            -0.788  0.791  0.133       
#river_mile:typetrib  0.735 -0.814 -0.061 -0.964

#Standardized Within-Group Residuals:
#  Min         Q1        Med         Q3        Max 
#-3.6264063  0.1084158  0.7827289  1.1393529  3.0264409 

#Number of Observations: 806
#Number of Groups: 8 

#SO: Chl-a NRR decreases (slightly!) with river mile. It's much higher in summer and much lower in tribs. Also, the slope
#varies with of NRR with river mile increases when it's a trib (I think....)  