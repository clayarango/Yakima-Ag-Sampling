#packages
library(nlme)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lmtest)
library(lattice)
library(MuMIn)
library(multcomp)

#data (created in "all sites" by combining NDS files and water chem file)
nds_chem<-read.csv("NDS_chem_all.csv")
nds_chem$river_mile<-ifelse(nds_chem$stream=="ahtanum", 106, nds_chem$river_mile)

nds_s<-subset(nds_chem, top=="sponge")
nds_s <- subset(nds_s, !(is.na(nds_s$cr.nrr)))

Z<-cbind(nds_s$river_mile, nds_s$Si.mgL, nds_s$oP.mgPL, nds_s$NO3.mgNL, nds_s$DOC.mgL, nds_s$DIN.mgNL, nds_s$N.P.ratio,
         nds_s$N.Si.ratio, nds_s$P.Si.ratio, nds_s$cr.nrr)
colnames(Z)<-c("mile", "silica", "SRP", "NO3", "DOC", "DIN", "N:P", "N:Si", "P:Si", "NRR")
pairs(Z)
cor(Z, use="pairwise.complete.obs", method="spearman")
#pairs >|0.6| include: NO3, DIN, N:P, N:Si and mile; SRP and P:Si; NO3 and DIN, N:P, N:Si, P:Si; DOC and Si; SRP and NO3; P:Si and N:Si
#continuous variables to try with NRR: mile, silica, SRP, NO3, DIN, DOC.Choose DIN instead of NO3 (correlation higher with NRR) 
#combinations that are ok: mile + silica, mile + SRP, DOC + DIN, silica + DIN. First, do river mile alone. River mile will 
#directly address our question on watershed location and NRR.

#River mile, season, and type to test question of watershed position. NOTE: this model was selected based on the usual
#procedures, but the code must have been deleted. Instead of re-doing model selection, will start with final model.
Mf<-lme(log(cr.nrr+1)~season+ river_mile*type, random = ~1+river_mile| nutrient, method="REML", data=nds_s)
summary(Mf)
#Random effects:
#Formula: ~1 + river_mile | nutrient
#Structure: General positive-definite, Log-Cholesky parametrization
#StdDev      Corr  
#(Intercept) 0.072670380 (Intr)
#river_mile  0.001613627 -0.894
#Residual    0.189145516       

#Fixed effects: log(cr.nrr + 1) ~ season + river_mile * type 
#                       Value  Std.Error  DF   t-value  p-value
#(Intercept)          0.5401840 0.03465829 785 15.585997  0.0000
#seasonfall          -0.1097521 0.01345564 785  8.156582  0.0000
#river_mile           0.0016288 0.00059886 785  2.719846  0.0067
#typetrib             0.2673480 0.04474965 785  5.974304  0.0000
#river_mile:typetrib -0.0024225 0.00038652 785 -6.267546  0.0000

ranef(Mf)
#       (Intercept)    river_mile
#control  0.05064554 -0.0004123336
#N       -0.06665818  0.0016244156
#NP      -0.05226612  0.0012694891
#NPSi    -0.10001441  0.0023269448
#NSi      0.02581716 -0.0015291970
#P        0.02678770 -0.0006280988
#PSi      0.04495817 -0.0005058603
#Si       0.07073013 -0.0021453599

r.squaredGLMM(Mf)
        #R2m       R2c
#[1,] 0.113609 0.4167893

#try a model with nutrient as fixed effect so can do post-hoc tests on it.
M1<-lm(log(cr.nrr+1)~nutrient +river_mile*type +season, nds_s)
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(M1) #ok
E<-rstandard(M1)
op<-par(mfrow=c(1,1))
boxplot(E~stream, data=nds_s)#ok - looks like no need to include stream as a random
abline(0,0)

M1<-gls(log(cr.nrr+1)~nutrient +river_mile*type +season, nds_s)
M1b<-lme(log(cr.nrr+1)~nutrient +river_mile*type +season, 
         random = ~1+river_mile|stream , nds_s)

anova(M1, M1b)
#no difference - no value in adding the random effects

M2<-gls(log(cr.nrr+1)~nutrient +river_mile+type +season, nds_s)
lrtest(M1, M2)
#M1 much better (interaction significant)
step(M1)
summary(M1)

#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          0.4493627  0.0311116  14.444  < 2e-16 ***
#nutrientN            0.0956563  0.0282333   3.388 0.000739 ***
#nutrientNP           0.0705546  0.0283756   2.486 0.013109 *  
#nutrientNPSi         0.1443496  0.0284413   5.075 4.83e-07 ***
#nutrientNSi         -0.1716455  0.0279541  -6.140 1.31e-09 ***
#nutrientP           -0.0582314  0.0282968  -2.058 0.039932 *  
#nutrientPSi         -0.0178228  0.0282294  -0.631 0.527991    
#nutrientSi          -0.1856265  0.0283040  -6.558 9.87e-11 ***
#river_mile           0.0016061  0.0001921   8.362 2.80e-16 ***
#typetrib             0.2653397  0.0471977   5.622 2.63e-08 ***
#seasonsummer         0.1084612  0.0141945   7.641 6.28e-14 ***
#river_mile:typetrib -0.0024083  0.0004076  -5.908 5.16e-09 ***

#Residual standard error: 0.1996 on 785 degrees of freedom
#Multiple R-squared:  0.3317,	Adjusted R-squared:  0.3223 
#F-statistic: 35.42 on 11 and 785 DF,  p-value: < 2.2e-16

#post-hoc tests
model.matrix.gls <- function(object, ...) {
        model.matrix(terms(object), data = getData(object), ...)
}
model.frame.gls <- function(object, ...) {
        model.frame(formula(object), data = getData(object), ...)
}
terms.gls <- function(object, ...) {
        terms(model.frame(object), ...)
}
multcomp<-glht(M1, linfct=mcp(nutrient="Tukey"))

#NRR model, based on plots. starting variables include season, type, and the 4 combinations above
#start with fixed effects and then test if adding random effects improves the model
M1<-lme(log(cr.nrr+1)~river_mile+season + type*river_mile, random = ~1+river_mile|nutrient, nds_s)
summary(M1)
#Random effects:
#Formula: ~1 + river_mile | nutrient
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

r.squaredGLMM(M1)
#       R2m       R2c
#[1,] 0.1136093 0.4167879

#M1 is all the variables and interactions (except nutrient, which we want to model as a random effect)

M1<-lm(log(cr.nrr+1)~river_mile*season + type*river_mile + Si.mgL*type + Si.mgL*season, nds_s) #needed to log nrr to account for heteroscedasticity in residuals
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(M1)
E<-rstandard(M1)
op<-par(mfrow=c(1,1))
boxplot(E~stream, data=nds_s)
abline(0,0) #none of the boxplots are completely above or below the 0 line, so don't need to include stream as random variable.

#now, use gls so can compare with lme
M1<-gls(log(cr.nrr+1)~river_mile*season + type*river_mile + Si.mgL*type + Si.mgL*season, nds_s)
M1a<-lme(log(cr.nrr+1)~river_mile*season + type*river_mile + Si.mgL*type + Si.mgL*season, random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
M1b<-lme(log(cr.nrr+1)~river_mile*season + type*river_mile + Si.mgL*type + Si.mgL*season, random = ~1|nutrient, 
         method="REML", nds_s)
M1c<-lme(log(cr.nrr+1)~river_mile*season + type*river_mile + Si.mgL*type + Si.mgL*season, random = ~1+Si.mgL|nutrient, 
         method="REML", nds_s)

anova(M1, M1a, M1b, M1c)
#M1a is best (lowest AIC, highest logLik)

#now optimize the fixed part
summary(M1a)
M2a<-lme(log(cr.nrr+1)~river_mile*season + type + Si.mgL*type + Si.mgL*season, random = ~1+river_mile|nutrient, 
         method="REML", nds_s)

lrtest(M1a, M2a) 
#M2a is better (so type*river_mile interaction NS b/c model better w/o it)
summary(M2a) #mile has p of 0.27. will drop the intxn first
M3a<-lme(log(cr.nrr+1)~river_mile + Si.mgL*type + Si.mgL*season, random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
lrtest(M2a, M3a)
#M3a better so conclude intxn wasn't sig
summary(M3a) #river mile p = 0.92
M4a<-lme(log(cr.nrr+1)~Si.mgL*type + Si.mgL*season, random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
lrtest(M3a, M4a)
#M4a better
summary(M4a) #all p < 0.003 but will remove lowest just in case
M5a<-lme(log(cr.nrr+1)~Si.mgL*type + Si.mgL+season, random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
lrtest(M4a, M5a)
#M4a better
M6a<-lme(log(cr.nrr+1)~type + Si.mgL+season, random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
lrtest(M4a, M6a)
#M4a better
summary(M4a)
#Random effects:
#Formula: ~1 + river_mile | nutrient
#Structure: General positive-definite, Log-Cholesky parametrization
#StdDev      Corr  
#(Intercept) 0.070437940 (Intr)
#river_mile  0.001521477 -0.866
#Residual    0.188189541       

#Fixed effects: log(cr.nrr + 1) ~ Si.mgL * type + Si.mgL * season 
#                       Value  Std.Error  DF   t-value p-value
#(Intercept)          0.9724807 0.07129803 784 13.639657  0.0000
#Si.mgL              -0.0203280 0.00328211 784 -6.193570  0.0000
#typetrib            -0.2102705 0.06256215 784 -3.360985  0.0008
#seasonsummer        -0.1048075 0.03435448 784 -3.050765  0.0024
#Si.mgL:typetrib      0.0156919 0.00329061 784  4.768699  0.0000
#Si.mgL:seasonsummer  0.0074717 0.00121631 784  6.142902  0.0000

#CONCLUSION: best model: log(cr.nrr+1)~Si.mgL*type + Si.mgL*season, random = ~1+river_mile|nutrient
#IOW: NRR for CR is predicted by [Si], season, and stream type, with a different relationship
#with Si, depending on stream type and season. The slope and intercept vary for each nutrient treatment.

ranef(M4a)
#       (Intercept)    river_mile
#control  0.06162274 -0.0005294019
#N       -0.06422561  0.0015697653
#NP      -0.05115053  0.0012432033
#NPSi    -0.09738324  0.0022626647
#NSi      0.02230987 -0.0015445037
#P        0.02782972 -0.0006681402
#PSi      0.04794643 -0.0005558573
#Si       0.06822504 -0.0021561141

r.squaredGLMM(M4a)
#       R2m       R2c
#[1,] 0.1262404 0.4082482
#R2m is marginal (fixed effects only) and R2c is conditional (entire model)

plot(predict(M4a),log(nds_s$cr.nrr+1), xlab="Predicted NRR",ylab="Actual NRR",abline (0,1))
#looks great - linear with nice scatter around the line, consistent variance
cor.test(predict(M4a),log(nds_s$cr.nrr+1))
#0.6399556  
0.639956^2
#0.4095437 

E<-resid(M4a)
F2<-fitted(M4a)
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(x=F2, y=E, xlab="Fitted values", ylab="Residuals")
boxplot(E~type, data=nds_s, main="Type", ylab="Residuals")
boxplot(E~season, data=nds_s, main="Season", ylab="Residuals")
plot(x=nds_s$river_mile, y=E, xlab="River Mile", ylab="Residuals")
#no obvious pattern in residuals, but next need to also try a LOESS smoother

xyplot(E~Si.mgL|type*season, data=nds_s, ylab="Residuals", xlab="Si (mg/L)", 
       panel=function(x,y){panel.grid(h=-1, v=2)
         panel.points(x,y,col=1)
         panel.loess(x,y,span=0.5, col=1, lwd=2)})
#looks linear.

library(mgcv)
M4a<-gam(log(cr.nrr+1)~s(Si.mgL)*(season) + s(Si.mgL)*(type),
          random = list(~1|nutrient,1+river_mile), data=nds_s)

#now, CR NRR with river mile + SRP
M1<-lm(log(cr.nrr+1)~river_mile*season + type*river_mile + oP.mgPL*type + oP.mgPL*season, nds_s)#need to log to meet assumptions
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(M1)
E<-rstandard(M1)
op<-par(mfrow=c(1,1))
boxplot(E~stream, data=nds_s)
abline(0,0)

M1a<-lme(log(cr.nrr+1)~river_mile*season + type*river_mile + oP.mgPL*type + oP.mgPL*season, random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
M1b<-lme(log(cr.nrr+1)~river_mile*season + type*river_mile + oP.mgPL*type + oP.mgPL*season, random = ~1|nutrient, 
         method="REML", nds_s)
M1c<-lme(log(cr.nrr+1)~river_mile*season + type*river_mile + oP.mgPL*type + oP.mgPL*season, random = ~1+Si.mgL|nutrient, 
         method="REML", nds_s)

anova(M1, M1a, M1b, M1c)
#M1a is best (lowest AIC, highest logLik, anova p <0.0001 for comparisons)

#now optimize the fixed part
summary(M1a)
M2a<-lme(log(cr.nrr+1)~river_mile*type +river_mile*season + oP.mgPL*type, random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
lrtest(M1a, M2a)
#no difference
M3a<-lme(log(cr.nrr+1)~river_mile*type + oP.mgPL+season, random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
lrtest(M1a, M3a)
#M3a better
summary(M3a)
M4a<-lme(log(cr.nrr+1)~river_mile*type +season, random = ~1+river_mile|nutrient, 
              method="REML", nds_s)
lrtest(M3a, M4a)
#no difference
M5a<-lme(log(cr.nrr+1)~river_mile+type +season, random = ~1+river_mile|nutrient, 
         method="REML", nds_s)
lrtest(M3a, M5a)
#M3a better

E<-resid(M3a)
F2<-fitted(M3a)
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(x=F2, y=E, xlab="Fitted values", ylab="Residuals")
boxplot(E~type, data=nds_s, main="Type", ylab="Residuals")
boxplot(E~season, data=nds_s, main="Season", ylab="Residuals")
plot(x=nds_s$oP.mgPL, y=E, xlab="River Mile", ylab="Residuals")
#looks good!

summary(M3a)
#Random effects:
#Formula: ~1 + river_mile | nutrient
#Structure: General positive-definite, Log-Cholesky parametrization
#           StdDev      Corr  
#(Intercept) 0.072616743 (Intr)
#river_mile  0.001613409 -0.894
#Residual    0.189266416       

#Fixed effects: log(cr.nrr + 1) ~ river_mile * type + oP.mgPL + season 
#                       Value  Std.Error  DF   t-value p-value
#(Intercept)          0.4282076 0.04036040 784 10.609597  0.0000
#river_mile           0.0016382 0.00060511 784  2.707197  0.0069
#typetrib             0.2689036 0.04706304 784  5.713691  0.0000
#oP.mgPL              0.0272022 0.25376852 784  0.107193  0.9147
#seasonsummer         0.1102535 0.01425527 784  7.734223  0.0000
#river_mile:typetrib -0.0024472 0.00044976 784 -5.441086  0.0000

ranef(M3a)
#       (Intercept)    river_mile
#control  0.05047241 -0.0004109468
#N       -0.06661202  0.0016240199
#NP      -0.05223394  0.0012690817
#NPSi    -0.09994236  0.0023263837
#NSi      0.02593365 -0.0015301829
#P        0.02678356 -0.0006280061
#PSi      0.04486456 -0.0005051781
#Si       0.07073414 -0.0021451714

r.squaredGLMM(M3a)
# R2m       R2c
#[1,] 0.113537 0.4164623

xyplot(E~oP.mgPL|type*season, data=nds_s, ylab="Residuals", xlab="SRP (mg/L)", 
       panel=function(x,y){panel.grid(h=-1, v=2)
         panel.points(x,y,col=1)
         panel.loess(x,y,span=0.5, col=1, lwd=2)})
#looks linear

#respiration with DOC and DIN
M1<-lm(log(cr.nrr+1)~DOC.mgL*season + type*DOC.mgL + DIN.mgNL*type + DIN.mgNL*season, nds_s)#need to log to meet assumptions
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(M1)
E<-rstandard(M1)
op<-par(mfrow=c(1,1))
boxplot(E~stream, data=nds_s)#ok
abline(0,0)
M1<-gls(log(cr.nrr+1)~DOC.mgL*season + type*DOC.mgL + DIN.mgNL*type + DIN.mgNL*season, nds_s)
M1a<-lme(log(cr.nrr+1)~DOC.mgL*season + type*DOC.mgL + DIN.mgNL*type + DIN.mgNL*season, random = ~1+DOC.mgL|nutrient, 
         method="REML", nds_s)
M1b<-lme(log(cr.nrr+1)~DOC.mgL*season + type*DOC.mgL + DIN.mgNL*type + DIN.mgNL*season, random = ~1|nutrient, 
         method="REML", nds_s)
M1c<-lme(log(cr.nrr+1)~DOC.mgL*season + type*DOC.mgL + DIN.mgNL*type + DIN.mgNL*season, random = ~1+DIN.mgNL|nutrient, 
         method="REML", nds_s)

anova(M1, M1a, M1b, M1c)
#M1c best
summary(M1c)
M2c<-lme(log(cr.nrr+1)~type*DOC.mgL + DIN.mgNL*type + DIN.mgNL*season, random = ~1+DIN.mgNL|nutrient, 
         method="REML", nds_s)
lrtest(M1c, M2c)
#M2c better
summary(M2c)
M3c<-lme(log(cr.nrr+1)~type*DOC.mgL + DIN.mgNL*type + DIN.mgNL+season, random = ~1+DIN.mgNL|nutrient, 
         method="REML", nds_s)
lrtest(M2c, M3c)
#M3c better
summary(M3c)
M4c<-lme(log(cr.nrr+1)~DIN.mgNL*type + DIN.mgNL+season, random = ~1+DIN.mgNL|nutrient, 
         method="REML", nds_s)
lrtest(M3c, M4c)
#no difference but M3c higher loglik
summary(M4c)
M5c<-lme(log(cr.nrr+1)~type + DIN.mgNL+season, random = ~1+DIN.mgNL|nutrient, 
         method="REML", nds_s)
lrtest(M3c, M5c)
#M3c better
summary(M5c)
M6c<-lme(log(cr.nrr+1)~DIN.mgNL+season, random = ~1+DIN.mgNL|nutrient, 
         method="REML", nds_s)
lrtest(M3c, M6c)
#M3c better

E<-resid(M3c)
F2<-fitted(M3c)
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(x=F2, y=E, xlab="Fitted values", ylab="Residuals")
boxplot(E~type, data=nds_s, main="Type", ylab="Residuals")
boxplot(E~season, data=nds_s, main="Season", ylab="Residuals")
plot(x=nds_s$DOC.mgL, y=E, xlab="DOC", ylab="Residuals")
op<-par(mfrow=c(1,1))
plot(x=nds_s$DIN.mgNL, y=E, xlab="DIN", ylab="Residuals")
#ok!

summary(M3c)
#Random effects:
#Formula: ~1 + DIN.mgNL | nutrient
#Structure: General positive-definite, Log-Cholesky parametrization
#           StdDev     Corr  
#(Intercept) 0.17359943 (Intr)
#DIN.mgNL    0.07666769 -0.99 
#Residual    0.19345983       

#Fixed effects: log(cr.nrr + 1) ~ type * DOC.mgL + DIN.mgNL * type + DIN.mgNL +      season 
#                     Value  Std.Error  DF   t-value p-value
#(Intercept)        0.6758406 0.06606386 783 10.230110  0.0000
#typetrib           0.0560419 0.03637908 783  1.540498  0.1238
#DOC.mgL            0.0049968 0.00425834 783  1.173420  0.2410
#DIN.mgNL          -0.1117517 0.03178298 783 -3.516086  0.0005
#seasonsummer       0.0816523 0.01604431 783  5.089176  0.0000
#typetrib:DOC.mgL  -0.0162799 0.00473202 783 -3.440379  0.0006
#typetrib:DIN.mgNL  0.1191199 0.02199948 783  5.414671  0.0000

r.squaredGLMM(M3c)
#       R2m       R2c
#[1,] 0.1100741 0.3889311

ranef(M3c)
#       (Intercept)    DIN.mgNL
#control  0.004390869  0.00419214
#N        0.171194939 -0.07804768
#NP       0.147316590 -0.07039631
#NPSi     0.221002240 -0.09523225
#NSi     -0.210913379  0.08760922
#P       -0.074042790  0.03603886
#PSi     -0.022515720  0.01573467
#Si      -0.236432749  0.10010135

xyplot(E~DOC.mgL|type*season, data=nds_s, ylab="Residuals", xlab="DOC (mg/L)", 
       panel=function(x,y){panel.grid(h=-1, v=2)
         panel.points(x,y,col=1)
         panel.loess(x,y,span=0.5, col=1, lwd=2)})
#DIN looks linear but DOC in summer tribs not

#respiration with silica and DIN
M1<-lm(log(cr.nrr+1)~Si.mgL*season + type*Si.mgL + DIN.mgNL*type + DIN.mgNL*season, nds_s)#need to log to meet assumptions
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(M1)
E<-rstandard(M1)
op<-par(mfrow=c(1,1))
boxplot(E~stream, data=nds_s)#ok
abline(0,0)

M1<-gls(log(cr.nrr+1)~Si.mgL*season + type*Si.mgL + DIN.mgNL*type + DIN.mgNL*season, nds_s)
M1a<-lme(log(cr.nrr+1)~Si.mgL*season + type*Si.mgL + DIN.mgNL*type + DIN.mgNL*season, random = ~1+Si.mgL|nutrient, 
         method="REML", nds_s)
M1b<-lme(log(cr.nrr+1)~Si.mgL*season + type*Si.mgL + DIN.mgNL*type + DIN.mgNL*season, random = ~1|nutrient, 
         method="REML", nds_s)
M1c<-lme(log(cr.nrr+1)~Si.mgL*season + type*Si.mgL + DIN.mgNL*type + DIN.mgNL*season, random = ~1+DIN.mgNL|nutrient, 
         method="REML", nds_s)
anova(M1, M1a, M1b, M1c)
#M1c is best
summary(M1c)
M2c<-lme(log(cr.nrr+1)~Si.mgL*season + type*Si.mgL +  DIN.mgNL*season, random = ~1+DIN.mgNL|nutrient, 
         method="REML", nds_s)
lrtest(M1c, M2c)
#M2c better
summary(M2c)
M3c<-lme(log(cr.nrr+1)~Si.mgL*season + type*Si.mgL +  DIN.mgNL, random = ~1+DIN.mgNL|nutrient, 
         method="REML", nds_s)
lrtest(M2c, M3c)
#no difference (p = 0.053) but M3c higher loglik
summary(M3c)
M4c<-lme(log(cr.nrr+1)~Si.mgL*season + type*Si.mgL, random = ~1+DIN.mgNL|nutrient, 
         method="REML", nds_s)
lrtest(M3c, M4c)
#M4c better
summary(M4c)#all p <0.0001 but will remove anyway
M5c<-lme(log(cr.nrr+1)~season + type*Si.mgL, random = ~1+DIN.mgNL|nutrient, 
         method="REML", nds_s)
lrtest(M4c, M5c)
#M4c better
M6c<-lme(log(cr.nrr+1)~season + type+Si.mgL, random = ~1+DIN.mgNL|nutrient, 
         method="REML", nds_s)
lrtest(M4c, M6c)
#M4c better
summary(M6c)

E<-resid(M4c)
F2<-fitted(M4c)
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(x=F2, y=E, xlab="Fitted values", ylab="Residuals")
boxplot(E~type, data=nds_s, main="Type", ylab="Residuals")
boxplot(E~season, data=nds_s, main="Season", ylab="Residuals")
plot(x=nds_s$DOC.mgL, y=E, xlab="Si.mgL", ylab="Residuals")
#ok!

summary(M4c)
#Random effects:
#Formula: ~1 + DIN.mgNL | nutrient
#Structure: General positive-definite, Log-Cholesky parametrization
#StdDev     Corr  
#(Intercept) 0.17045437 (Intr)
#DIN.mgNL    0.07525272 -0.989
#Residual    0.19158180       

#Fixed effects: log(cr.nrr + 1) ~ Si.mgL * season + type * Si.mgL 
#Value  Std.Error  DF   t-value p-value
#(Intercept)          1.0307520 0.06664612 784 15.466048   0e+00
#Si.mgL              -0.0224019 0.00291462 784 -7.686040   0e+00
#seasonsummer        -0.1124826 0.03314921 784 -3.393220   7e-04
#typetrib            -0.2733986 0.07319974 784 -3.734966   2e-04
#Si.mgL:seasonsummer  0.0075782 0.00120741 784  6.276395   0e+00
#Si.mgL:typetrib      0.0186581 0.00343600 784  5.430197   0e+00

r.squaredGLMM(M4c)
#     R2m       R2c
#[1,] 0.134377 0.4027907

ranef(M4c)
#       (Intercept)    DIN.mgNL
#control -0.03568001  0.02275485
#N        0.13021801 -0.05993333
#NP       0.10811537 -0.05350095
#NPSi     0.17929903 -0.07645590
#NSi     -0.25141414  0.10504168
#P       -0.11426887  0.05463416
#PSi     -0.06287778  0.03423132
#Si      -0.27736848  0.11785053

xyplot(E~Si.mgL|type*season, data=nds_s, ylab="Residuals", xlab="Si (mg/L)", 
       panel=function(x,y){panel.grid(h=-1, v=2)
         panel.points(x,y,col=1)
         panel.loess(x,y,span=0.5, col=1, lwd=2)})
#linear-ish

#respiration with N:P ratio
M1<-lm(log(cr.nrr+1)~N.P.ratio*season + type*N.P.ratio, nds_s) #needed to log nrr to account for non-normality in residuals
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(M1)
E<-rstandard(M1)
boxplot(E~stream, data=nds_s)
abline(0,0)
#none of the boxplots are completely above or below the 0 line, so don't need to include stream as random variable.

#now, use gls so can compare with lme
M1<-gls(log(cr.nrr+1)~N.P.ratio*season + type*N.P.ratio, nds_s)
M1a<-lme(log(cr.nrr+1)~N.P.ratio*season + type*N.P.ratio,random = ~1+N.P.ratio|nutrient, 
         method="REML", nds_s)
M1b<-lme(log(cr.nrr+1)~N.P.ratio*season + type*N.P.ratio,random = ~1|nutrient, 
         method="REML", nds_s)
anova(M1, M1a, M1b) #M1a has highest logLik and lowest AIC and BIC

#now, fixed effects
summary(M1a)
M2a<-lme(log(cr.nrr+1)~season + type*N.P.ratio,random = ~1+N.P.ratio|nutrient, 
         method="REML", nds_s)
lrtest(M1a, M2a) #M2a better
summary(M2a)

M3a<-lme(log(cr.nrr+1)~season + type+N.P.ratio,random = ~1+N.P.ratio|nutrient, 
         method="REML", nds_s)
lrtest(M2a, M3a)#M2a better
M4a<-lme(log(cr.nrr+1)~season + N.P.ratio,random = ~1+N.P.ratio|nutrient, 
         method="REML", nds_s)
lrtest(M2a, M4a)#M2a better
M5a<-lme(log(cr.nrr+1)~season * type+N.P.ratio,random = ~1+N.P.ratio|nutrient, 
         method="REML", nds_s)
lrtest(M2a, M5a)#M2a better

op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
E2<-residuals(M2a, type="normalized")
F2<-fitted(M2a)
myYlab<-"Residuals"
plot(x=F2, y=E2, xlab="Fitted Values", ylab=myYlab)
boxplot(E2~season, data=nds_s, main="Season", ylab=myYlab)
abline(0,0)
boxplot(E2~type, data=nds_s, main="Type", ylab=myYlab)
abline(0,0)
plot(x=nds_s$N.P.ratio, y=E2, main="N:P", ylab=myYlab)
#looks great
summary(M2a)
#Random effects:
#Formula: ~1 + N.P.ratio | nutrient
#Structure: General positive-definite, Log-Cholesky parametrization

#Fixed effects: log(cr.nrr + 1) ~ season + type * N.P.ratio 
#                       Value  Std.Error  DF   t-value p-value
#(Intercept)         0.7200105 0.07086392 785 10.160466   0e+00
#seasonsummer        0.1182111 0.01418333 785  8.334505   0e+00
#typetrib           -0.1264464 0.02215142 785 -5.708278   0e+00
#N.P.ratio          -0.0032760 0.00098180 785 -3.336694   9e-04
#typetrib:N.P.ratio  0.0037734 0.00059408 785  6.351701   0e+00

r.squaredGLMM(M2a)
#R2m       R2c
#[1,] 0.1028653 0.3920104

plot(predict(M2a),log(nds_s$cr.nrr+1), xlab="Predicted NRR",ylab="Actual NRR",abline (0,1))
#even scatter around line

cor.test(predict(M2a),log(nds_s$cr.nrr+1))
#0.6133408 

ranef (M2a)
#       (Intercept)     N.P.ratio
#control  0.007225176 -3.545659e-05
#N        0.197942682 -2.624889e-03
#NP       0.142855944 -1.861779e-03
#NPSi     0.250703659 -3.261163e-03
#NSi     -0.242201628  3.122981e-03
#P       -0.069831052  9.132532e-04
#PSi     -0.017572963  2.685322e-04
#Si      -0.269121818  3.478522e-03

#slightly less predictive power than with river mile

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
#can skip the log10 transformation to do this

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

#now, get random structure
M1a<-lme(chla.nrr~river_mile*season + river_mile*type, random=~1+river_mile|nutrient,
         weights = vf3, method="REML", data=nds_g)
#get error: nlminb problem, convergence error code = 1, message = iteration limit reached without convergence (10)
#guessing this is due to over-parameterization? maybe too many variables that used to model variation and/or random effects

#try just random intercept, instead of random slope and intercept
M2a<-lme(chla.nrr~river_mile*season + river_mile*type, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)

#also try random model only (fixed effects explain very little)
M3a<-lme(chla.nrr~1, random =~1|nutrient, 
         weights = vf3, method="REML", data=nds_g)#doesn't converge

anova(M1sc, M2a)
##    Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#M1sc     1 18 1727.281 1811.604 -845.6405                        
#M2a      2 19 1710.177 1799.185 -836.0885 1 vs 2 19.10411  <.0001

#improves when slope varies with nutrient category. need to have both fixed and random effects

#now: optimize the fixed portion!
summary(M2a)

M2b<-lme(chla.nrr~season + river_mile*type, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)

lrtest(M2a, M2b)
#p = 0.3, so mile * season interaction not significant

summary(M2b)

M2c<-lme(chla.nrr~river_mile+ type+season, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)
#doesn't converge

M2d<-lme(chla.nrr~river_mile+type, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)

lrtest(M2b, M2d)
##  M2b still better

M2e<-lme(chla.nrr~type+season, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)

lrtest(M2b, M2e)
##  M2b better

M2f<-lme(chla.nrr~river_mile + season, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)

lrtest(M2b, M2f)
#   M2b still better

M2g<-lme(chla.nrr~season, random=~1|nutrient,
         weights = vf3, method="REML", data=nds_g)

lrtest(M2b, M2g)
#   M2b still better

M2h<-lme(chla.nrr~river_mile, random=~1|nutrient,
         weights=vf3, method="REML", data=nds_g)
lrtest(M2b, M2h)

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
r.squaredGLMM(M2b)
#       R2m       R2c
#[1,] 0.1614768 0.1672194
#R2m is marginal (fixed effects only) and R2c is conditional (entire model)

ranef(M2b)
#       (Intercept)
#control  0.06433244
#N        0.07263615
#NP      -0.05449180
#NPSi     0.01110275
#NSi     -0.04395794
#P        0.02348266
#PSi     -0.04629925
#Si      -0.02680503

plot(predict(M2b),nds_g$chla.nrr, xlab="Predicted NRR",ylab="Actual NRR",abline (0,1))# ick - looks like a straight line d/t
#all the outliers. it doesn't predict the high values very well
cor.test(predict(M2b),nds_g$chla.nrr)
#0.0776288
0.0776288^2
#much lower than that estimated with pseudo-r2. possibly b/c of weird variance structure?

#SO: Chl-a NRR decreases (slightly!) with river mile. It's much higher in summer and much lower in tribs. Also, the slope
#varies with of NRR with river mile increases when it's a trib (I think....)  

#correlations
Z<-cbind(nds_g$river_mile, nds_g$Si.mgL, nds_g$oP.mgPL, nds_g$NO3.mgNL, nds_g$DOC.mgL, nds_g$DIN.mgNL, nds_g$N.P.ratio,
         nds_g$N.Si.ratio, nds_g$P.Si.ratio, nds_g$chla.nrr)
colnames(Z)<-c("mile", "silica", "SRP", "NO3", "DOC", "DIN", "N:P", "N:Si", "P:Si", "NRR")
pairs(Z)
cor(Z, use="pairwise.complete.obs", method="spearman")
#other combinations to use, besides river mile: 
#SRP, DOC, river mile, N:P, N:Si (probably not worth it though)
#silica, NO3
#mile, silica
#P:Si, N:P

op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
M1<-lm(chla.nrr~NO3.mgNL*season + type*NO3.mgNL + Si.mgL*season + Si.mgL*type, nds_g) 
plot(M1) #even with log-transformation, deviation from normal in Q-Q plot and heteroscedasticity
E<-rstandard(M1)
op<-par(mfrow=c(1,1))
boxplot(E~stream, data=nds_g)
abline(0,0) #maybe ok - ahtanum and wenas a bit off but all intersect at some point

M1<-gls(chla.nrr~NO3.mgNL*season + type*NO3.mgNL + Si.mgL*season + Si.mgL*type, nds_g) 
vf1<-varIdent(form=~1|stream)
M1s<-gls(chla.nrr~NO3.mgNL*season + type*NO3.mgNL + Si.mgL*season + Si.mgL*type, data=nds_g, weights=vf1)
vf2<-varFixed(~chla.nrr)
M1sb<-gls(chla.nrr~NO3.mgNL*season + type*NO3.mgNL + Si.mgL*season + Si.mgL*type, data=nds_g, weights=vf2)

anova(M1, M1s, M1sb)
#     Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#M1       1 10 4284.756 4331.564 -2132.378                         
#M1s      2 20 2447.097 2540.714 -1203.548 1 vs 2 1857.6591  <.0001
#M1sb     3 10 2351.243 2398.052 -1165.622 2 vs 3   75.8533  <.0001

#MUCH better when residuals allowed to vary, and best if vary with NRR
#but because better when allowed to vary with a continuous variable, need to check other structures, too

vf3<-varPower(form =~chla.nrr| stream)
M1sc<-gls(chla.nrr~NO3.mgNL*season + type*NO3.mgNL + Si.mgL*season + Si.mgL*type, data=nds_g, weights=vf3)
vf4<-varPower(form=~chla.nrr|season)
M1sd<-gls(chla.nrr~NO3.mgNL*season + type*NO3.mgNL + Si.mgL*season + Si.mgL*type, data=nds_g, weights=vf4)
vf5<-varPower(form=~chla.nrr|type)
M1se<-gls(chla.nrr~NO3.mgNL*season + type*NO3.mgNL + Si.mgL*season + Si.mgL*type, data=nds_g, weights=vf4)

anova(M1sb, M1sc, M1sd, M1se)
#M1sc is best:  allows variance to increase with NRR but only in certain streams

#now get random structure
M1sc_1<-lme(chla.nrr~NO3.mgNL*season + type*NO3.mgNL + Si.mgL*season + Si.mgL*type, random = ~1+NO3.mgNL|nutrient,
            method="REML", data=nds_g, weights=vf3) #doesn't converge
M1sc_2<-lme(chla.nrr~NO3.mgNL*season + type*NO3.mgNL + Si.mgL*season + Si.mgL*type, random = ~1|nutrient,
            method="REML", data=nds_g, weights=vf3)
M1sc_3<-lme(chla.nrr~NO3.mgNL*season + type*NO3.mgNL + Si.mgL*season + Si.mgL*type, random = ~1+Si.mgL|nutrient,
            method="REML", data=nds_g, weights=vf3)#doesn't converge

anova(M1sc, M1sc_2)
#better with nutrient as random intercept
summary(M1sc_2)
#now fixed effects
M2sc_2<-lme(chla.nrr~ type*NO3.mgNL + Si.mgL*season + Si.mgL*type, random = ~1|nutrient,
            method="REML", data=nds_g, weights=vf3)
lrtest(M1sc_2, M2sc_2)
#not sig difft (p = 0.084), so season * No3 intxn not sig

M3sc_2<-lme(chla.nrr~NO3.mgNL + Si.mgL*season + Si.mgL*type, random = ~1|nutrient,
            method="REML", data=nds_g, weights=vf3)
lrtest(M1sc_2, M3sc_2)
#not sig diff ( p = 0.3), so type *No3 intxn not sig

M4sc_2<-lme(chla.nrr~NO3.mgNL + type*Si.mgL, random = ~1|nutrient,
            method="REML", data=nds_g, weights=vf3)
lrtest(M1sc_2,M4sc_2)
#M1sc_2 better, so season * Si intxn is sig

M5sc_2<-lme(chla.nrr~type*NO3.mgNL + season*Si.mgL + type, random = ~1|nutrient,
            method="REML", data=nds_g, weights=vf3)
lrtest(M1sc_2, M5sc_2)
#M1sc_2 better, so type *Si.mgL sign

M6sc_2<-lme(chla.nrr~Si.mgL*season + Si.mgL*type, random = ~1|nutrient,
           method="REML", data=nds_g, weights=vf3)
lrtest(M3sc_2, M6sc_2)
lrtest(M1sc_2, M6sc_2)
#not sig; don't need NO3 either

summary(M6sc_2)
#Linear mixed-effects model fit by REML
#Data: nds_g 
#AIC      BIC    logLik
# 1715.706 1804.714 -838.8532

#Random effects:
#  Formula: ~1 | nutrient
#(Intercept)  Residual
#StdDev:  0.05903136 0.6600309

#Variance function:
#  Structure: Power of variance covariate, different strata
#Formula: ~chla.nrr | stream 
#Parameter estimates:
#  ahtanum    century    cleelum      kiona     mabton     reecer     ringer       roza      satus  toppenish      wenas 
#0.99797858 0.62171878 1.00352931 0.29224163 0.04321229 1.07779479 0.92454211 0.86781881 1.12091691 0.33415659 1.15710613  
#Fixed effects: chla.nrr ~ Si.mgL * season + Si.mgL * type 
#Value  Std.Error  DF   t-value p-value
#(Intercept)        -0.2003948 0.05532218 793 -3.622324  0.0003
#Si.mgL               0.0377739 0.00400975 793  9.420502  0.0000
#seasonsummer         0.6441555 0.06418636 793 10.035707  0.0000
#typetrib             0.2579099 0.08727505 793  2.955139  0.0032
#Si.mgL:seasonsummer -0.0040503 0.00239977 793 -1.687802  0.0918
#Si.mgL:typetrib     -0.0368538 0.00448394 793 -8.219069  0.0000

r.squaredGLMM(M6sc_2)
        #R2m       R2c
#[1,] 0.1549568 0.1614803

op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
E2<-residuals(M6sc_2, type="normalized")
F2<-fitted(M6sc_2)
myYlab<-"Residuals"
plot(x=F2, y=E2, xlab="Fitted Values", ylab=myYlab)
boxplot(E2~season, data=nds_g, main="Season", ylab=myYlab)
abline(0,0)
boxplot(E2~type, data=nds_g, main="Type", ylab=myYlab)
abline(0,0)
plot(x=nds_g$Si.mgL, y=E2, main="Si", ylab=myYlab)
plot(M6sc_2)

####mile and silica. will start with variance structure used in other models
M1a<-lme(chla.nrr~river_mile*season + type*river_mile + Si.mgL*season + Si.mgL*type, random = ~1|nutrient,
            method="REML", data=nds_g, weights=vf3)

M2a<-lme(chla.nrr~type*river_mile + Si.mgL*season + Si.mgL*type, random = ~1|nutrient,
         method="REML", data=nds_g, weights=vf3)

M3a<-lme(chla.nrr~river_mile*season + Si.mgL*season + Si.mgL*type, random = ~1|nutrient,
         method="REML", data=nds_g, weights=vf3) #doesn't converge

M4a<-lme(chla.nrr~river_mile*season + type*river_mile + Si.mgL*type, random = ~1|nutrient,
         method="REML", data=nds_g, weights=vf3)

M5a<-lme(chla.nrr~river_mile*season + type*river_mile + Si.mgL*season, random = ~1|nutrient,
         method="REML", data=nds_g, weights=vf3)

lrtest(M1a, M2a)
#M2a better. interpretation: remove rivermile x season intxn.
lrtest(M1a, M4a)
#no difference. interpretation: remove Si x season
lrtest(M1a, M5a)
#M1a better. interpretation: leave in Si x type

M6a<-lme(chla.nrr~river_mile*type + season + Si.mgL*type, random = ~1|nutrient,
         method="REML", data=nds_g, weights=vf3)
M7a<-lme(chla.nrr~river_mile + Si.mgL*type, random = ~1|nutrient,
         method="REML", data=nds_g, weights=vf3)#doesn't converge
M8a<-lme(chla.nrr~ season + Si.mgL*type, random = ~1|nutrient,
         method="REML", data=nds_g, weights=vf3)
lrtest(M6a, M8a)
#M6a better
M9a<-lme(chla.nrr~river_mile*type + Si.mgL*type, random = ~1|nutrient,
         method="REML", data=nds_g, weights=vf3)
lrtest(M6a, M9a)
#M6a better
M10a<-lme(chla.nrr~Si.mgL*type, random = ~1|nutrient,
          method="REML", data=nds_g, weights=vf3)
lrtest(M6a, M10a)
#M6a better
plot(M6a)
#ok

summary(M6a)
#Random effects:
#Formula: ~1 | nutrient
#(Intercept)  Residual
#StdDev:  0.06795017 0.6613609

#Variance function:
#  Structure: Power of variance covariate, different strata
#Formula: ~chla.nrr | stream 
#Parameter estimates:
#  ahtanum    century    cleelum      kiona     mabton     reecer     ringer       roza 
#0.93734116 0.66018017 0.92367637 1.37999778 0.28449516 1.03397404 0.79861050 0.69380569 
#satus  toppenish      wenas 
#1.00505200 0.08114343 1.10495680 
#Fixed effects: chla.nrr ~ type * river_mile + season + Si.mgL * type 
#                       Value  Std.Error  DF   t-value p-value
#(Intercept)         -2.966308 0.16773578 792 -17.68441       0
#typetrib             3.470837 0.18240994 792  19.02767       0
#river_mile           0.011197 0.00057189 792  19.57929       0
#seasonsummer         0.664582 0.03962804 792  16.77051       0
#Si.mgL               0.131770 0.00706182 792  18.65944       0
#typetrib:river_mile -0.012625 0.00059069 792 -21.37344       0
#typetrib:Si.mgL     -0.137690 0.00716419 792 -19.21925       0

r.squaredGLMM(M6a)
#       R2m       R2c
#[1,] 0.1542205 0.1630554

#nutrient as fixed effect
M1<-lm(log(chla.nrr+1)~nutrient + river_mile*type + season, data=nds_g)
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
plot(M1) #nope. 
E<-rstandard(M1)
op<-par(mfrow=c(1,1))
boxplot(E~stream, data=nds_g)#prob should include as a random variable or include
#in variance structure
abline(0,0)

M1a<-lme(chla.nrr~nutrient + river_mile*type + season, random=~1|stream,
         method="REML", data=nds_g, weights = vf3)#doesn't converge

M1b<-gls(chla.nrr~nutrient + river_mile*type + season, 
         method="REML", data=nds_g, weights = vf3)
M1c<-gls(chla.nrr~nutrient + river_mile*type + season, 
         method="REML", data=nds_g, weights = vf1)
M1d<-gls(chla.nrr~nutrient + river_mile*type + season, 
         method="REML", data=nds_g, weights = vf2)
M1e<-gls(chla.nrr~nutrient + river_mile*type + season, 
         method="REML", data=nds_g, weights = vf4)
M1f<-gls(chla.nrr~nutrient + river_mile*type + season, 
         method="REML", data=nds_g, weights = vf5)
anova(M1b, M1c, M1d, M1e, M1f)
#M1b best

plot(M1b)
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
E2<-residuals(M1b, type="normalized")
F2<-fitted(M1b)
myYlab<-"Residuals"
plot(x=F2, y=E2, xlab="Fitted Values", ylab=myYlab)
boxplot(E2~season, data=nds_g, main="Season", ylab=myYlab)
abline(0,0)
boxplot(E2~type, data=nds_g, main="Type", ylab=myYlab)
abline(0,0)
plot(x=nds_g$river_mile, y=E2, main="River Mile", ylab=myYlab)
plot(x=nds_g$chla.nrr, y=E2, main="NRR", ylab=myYlab)
#ok, I think

summary(M1b)
M2b<-gls(chla.nrr~nutrient + river_mile+type + season, 
         method="REML", data=nds_g, weights = vf3)
lrtest(M1b, M2b)
#M2b better, so remove interaction
summary(M2b)
M3b<-gls(chla.nrr~nutrient + river_mile+ season, 
         method="REML", data=nds_g, weights = vf3)
M4b<-gls(chla.nrr~nutrient + river_mile+type, 
         method="REML", data=nds_g, weights = vf3)
M5b<-gls(chla.nrr~nutrient + season+type, 
         method="REML", data=nds_g, weights = vf3)
lrtest(M2b, M3b, M4b, M5b)
#M2b better than the others
plot(M2b)
op<-par(mfrow=c(2,2), mar=c(4,4,3,2))
E2<-residuals(M2b, type="normalized")
F2<-fitted(M2b)
myYlab<-"Residuals"
plot(x=F2, y=E2, xlab="Fitted Values", ylab=myYlab)
boxplot(E2~season, data=nds_g, main="Season", ylab=myYlab)
abline(0,0)
boxplot(E2~type, data=nds_g, main="Type", ylab=myYlab)
abline(0,0)
plot(x=nds_g$river_mile, y=E2, main="River Mile", ylab=myYlab)
#probably ok, but residuals maybe not quite ok

summary(M2b)
#Generalized least squares fit by REML
#Model: chla.nrr ~ nutrient + river_mile + type + season 
#Data: nds_g 
#AIC      BIC    logLik
#1726.036 1833.638 -840.0182

#Variance function:
#        Structure: Power of variance covariate, different strata
#Formula: ~chla.nrr | stream 
#Parameter estimates:
#        ahtanum   century   cleelum     kiona    mabton    reecer    ringer      roza     satus 
#0.6675350 0.6459092 0.8519935 0.2705605 0.1095413 1.0844502 0.8244674 0.9221497 1.0023819 
#toppenish     wenas 
#0.2890087 1.1647960 

#Coefficients:
#        Value  Std.Error    t-value p-value
#(Intercept)   0.7462181 0.05702853  13.084998  0.0000
#nutrientN     0.0037537 0.04561606   0.082290  0.9344
#nutrientNP   -0.1502326 0.03680633  -4.081706  0.0000
#nutrientNPSi -0.0519220 0.05177382  -1.002861  0.3162
#nutrientNSi  -0.1367699 0.03590135  -3.809605  0.0001
#nutrientP    -0.0712952 0.04255204  -1.675484  0.0942
#nutrientPSi  -0.1391236 0.03622675  -3.840356  0.0001
#nutrientSi   -0.1196874 0.03708787  -3.227129  0.0013
#river_mile   -0.0022904 0.00028917  -7.920437  0.0000
#typetrib     -0.2280174 0.02130840 -10.700823  0.0000
#seasonsummer  0.4500703 0.03362604  13.384577  0.0000

plot(predict(M2b),nds_g$chla.nrr, xlab="Predicted NRR",ylab="Actual NRR",abline (0,1))# ick - looks like a straight line d/t
#all the outliers. it doesn't predict the high values very well
cor.test(predict(M2b),nds_g$chla.nrr)
#0.09122
0.09122635^2
#0.008
#conclusion: nutrient does not predict chla nrr very well at all.