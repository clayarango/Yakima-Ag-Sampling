#Author:  Clay Arango
#Creation date: 18-Apr-23
#Combine NRR from site+season to make consolidated figures

#packages
install.packages("nlme")
install.packages("nortest")
install.packages("plyr")
install.packages("dplyr")
install.packages("multcomp")
install.packages("MASS")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("ggpubr")
install.packages("scales")
library(nlme)
library(nortest)
library(plyr)
library(dplyr)
library(multcomp)
library(MASS)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(scales)

#################################################################
#AHTANUM FALL
#Load data
aht_fall <- read.table(file="aht_fall.csv", header=T, sep=",")

#set variable
d = aht_fall

#recode function to change name of top 
d$top<-recode(d$top, "cellulose" ="sponge")

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge", data=d)
d.gpp = subset(d, top=="glass", data=d)

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-16.93973 #divide by control ave_cr

#check for outliers and check data entry before calculating NRR.
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area))+geom_boxplot() +theme_classic()
#ok, but huge range in controls (but appear to have a normal distn - no outliers to remove)
#ggplot(d.gpp, aes(x=nutrient, y=chla))+geom_boxplot() +theme_classic()
#one low control value checks out. is an outlier, but not sure if should be removed. will calculate both.

#outlier not removed 

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/6.501087 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/1.4299814 #divide by control ave_chla
d.gpp$cr.nrr<-NA
#x1<- ddply(subset(d.gpp,!(nds.id=="B5")), "nutrient", summarise,  ave_chla = mean(chla, na.rm=T)) 
#x1
#d.gpp$chla.nrr = d.gpp$chla/1.7693330 #divide by control with B5 removed (outlier)
#d.cr$chla.nrr_1<-NA

#complete information for files
d.cr$stream = c("Ahtanum")
d.cr$type = c("Tributary")
d.cr$season = c("Fall")
d.cr.aht.f = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.aht.f) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.aht.f = as.data.frame(d.cr.aht.f)
str(d.cr.aht.f)

d.gpp$stream = c("Ahtanum")
d.gpp$type = c("Tributary")
d.gpp$season = c("Fall")
d.gpp.aht.f = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                   d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.aht.f) = c("stream", "type", "season", 
                         "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.aht.f = as.data.frame(d.gpp.aht.f)
str(d.gpp.aht.f)

#################################################################
#AHTANUM SUMMER
#Load data
aht_summer <- read.table(file="aht_summer.csv", header=T, sep=",")

#set variable
d = aht_summer

#recode function to change name of top 
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge", data=d)
d.gpp = subset(d, top=="glass", data=d)

#check for outliers and check data entry before calculating NRR.
#ggplot(d.cr, aes(x=nutrient, y=cr.area))+geom_boxplot() +theme_classic()
#one low and one high control but seem well-distributed

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) #changed to ddply b/c allows
#to specify by column name - I had a csv file with the relevant column in a different position.
x
d.cr$cr.nrr = d.cr$cr.area/-20.43606 #divide by control ave_cr

#check for outliers and check data entry before calculating NRR.
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area))+geom_boxplot() +theme_classic()
#one high-ish Si value checks out
#ggplot(d.gpp, aes(x=nutrient, y=chla_ug_cm2))+geom_boxplot() +theme_classic()
#one high control checks out, but consider dropping. F6

#outlier not removed 

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla_ug_cm2, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/3.099015 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla_ug_cm2/2.985545 #divide by control ave_chla
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla_ug_cm2, na.rm=T)) 
#x1
#d.gpp$chla.nrr<-d.gpp$chla_ug_cm2/2.700340

#complete information for files
d.cr$stream = c("Ahtanum")
d.cr$type = c("Tributary")
d.cr$season = c("Summer")
d.cr.aht.s = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.aht.s) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.aht.s = as.data.frame(d.cr.aht.s)
str(d.cr.aht.s)

d.gpp$stream = c("Ahtanum")
d.gpp$type = c("Tributary")
d.gpp$season = c("Summer")
d.gpp.aht.s = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.aht.s) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.aht.s = as.data.frame(d.gpp.aht.s)
str(d.gpp.aht.s)

#################################################################
#CENTURY LANDING FALL
#Load data
cen_lan_fall <- read.table(file="cen_lan_fall.csv", header=T, sep=",")

#set variable
d = cen_lan_fall

#recode function to change name of top 
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge", data=d)
d.gpp = subset(d, top=="glass", data=d)

#check for outliers and check data entry before calculating CR-NRR
#ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#N+Si group is spread out but centered

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-9.172439 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#way too many zeros in this data set
#ggplot(d.gpp, aes(x=nutrient, y=chla_ug_cm2)) + geom_boxplot() + theme_classic()
#no outliers

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla_ug_cm2, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/0.06366686 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla_ug_cm2/3.143975 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla_ug_cm2, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#complete information for files
d.cr$stream = c("Century Landing")
d.cr$type = c("Mainstem")
d.cr$season = c("Fall")
d.cr.cen.f = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.cen.f) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.cen.f = as.data.frame(d.cr.cen.f)
str(d.cr.cen.f)

d.gpp$stream = c("Century Landing")
d.gpp$type = c("Mainstem")
d.gpp$season = c("Fall")
d.gpp.cen.f = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.cen.f) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.cen.f = as.data.frame(d.gpp.cen.f)
str(d.gpp.cen.f)

#################################################################
#CENTURY LANDING SUMMER
#Load data
cen_lan_summer <- read.table(file="cen_lan_summer.csv", header=T, sep=",")

#set variable
d = cen_lan_summer

#recode function to change name of top 
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge", data=d)
d.gpp = subset(d, top=="glass", data=d)

#check for outliers and check data entry before calculating CR-NRR
#ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#N group is very spread out

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-21.614009 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#no outliers
#ggplot(d.gpp, aes(x=nutrient, y=chla_ug_cm2)) + geom_boxplot() + theme_classic()
#no outliers

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla_ug_cm2, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/3.398031 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla_ug_cm2/1.774638 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla_ug_cm2, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#complete information for files
d.cr$stream = c("Century Landing")
d.cr$type = c("Mainstem")
d.cr$season = c("Summer")
d.cr.cen.s = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.cen.s) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.cen.s = as.data.frame(d.cr.cen.s)
str(d.cr.cen.s)

d.gpp$stream = c("Century Landing")
d.gpp$type = c("Mainstem")
d.gpp$season = c("Summer")
d.gpp.cen.s = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.cen.s) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.cen.s = as.data.frame(d.gpp.cen.s)
str(d.gpp.cen.s)

#################################################################
#CLE ELUM FALL
#Load data
cle_fall <- read.table(file="cle_fall.csv", header=T, sep=",")

#set variable
d = cle_fall

#recode function to change name of top
d$top<-recode(d$top, "cellulose" = "sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge", data=d)
d.gpp = subset(d, top=="glass", data=d)

#check for outliers and check data entry before calculating CR-NRR
#ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#looks OK
#missing CC8 due to NA

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-7.121497 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#looks OK
#missing AA8 due to lost in field
#ggplot(d.gpp, aes(x=nutrient, y=chla)) + geom_boxplot() + theme_classic()
#looks OK
#missing AA8 due to lost in field

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/5.806643 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.7594258 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#complete information for files
d.cr$stream = c("Cle Elum")
d.cr$type = c("Mainstem")
d.cr$season = c("Fall")
d.cr.cle.f = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.cle.f) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.cle.f = as.data.frame(d.cr.cle.f)
str(d.cr.cle.f)

d.gpp$stream = c("Cle Elum")
d.gpp$type = c("Mainstem")
d.gpp$season = c("Fall")
d.gpp.cle.f = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.cle.f) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.cle.f = as.data.frame(d.gpp.cle.f)
str(d.gpp.cle.f)

#################################################################
#CLE ELUM SUMMER
#Load data
cle_summer <- read.table(file="cle_summer.csv", header=T, sep=",")

#set variable
d = cle_summer

#recode function to change name of top
#d$top<-recode(d$top, "cellulose" = "sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check for outliers and check data entry before calculating CR-NRR
#ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#BB5, W1 data entry check out, controls look OK
#keep BB5 and W1

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-4.808690 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#Z2 data entry checks out
#can remove Z2 from analysis of NRR, but not affected in NRR calculation
#ggplot(d.gpp, aes(x=nutrient, y=chla)) + geom_boxplot() + theme_classic()

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/2.633438 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.4318408 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#complete information for files
d.cr$stream = c("Cle Elum")
d.cr$type = c("Mainstem")
d.cr$season = c("Summer")
d.cr.cle.s = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.cle.s) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.cle.s = as.data.frame(d.cr.cle.s)
str(d.cr.cle.s)

d.gpp$stream = c("Cle Elum")
d.gpp$type = c("Mainstem")
d.gpp$season = c("Summer")
d.gpp.cle.s = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.cle.s) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.cle.s = as.data.frame(d.gpp.cle.s)
str(d.gpp.cle.s)

#################################################################
#KIONA FALL
#Load data
kiona_fall <- read.table(file="kiona_fall.csv", header=T, sep=",")

#set variable
d = kiona_fall

#recode function to change name of top
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check for outliers and check data entry before calculating CR-NRR
#ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#no outliers, but lots of zeros

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-15.066287 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#GPP area has 0 controls, so can't calculate NRR for GPP
#way too many zeros in data set
#probably need to drop this from GPP analysis
#ggplot(d.gpp, aes(x=nutrient, y=chla_ug_cm2)) + geom_boxplot() + theme_classic()
#N+P+Si group looks evenly spread

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/0 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/1.8799114 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla_ug_cm2, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#complete information for files
d.cr$stream = c("Kiona")
d.cr$type = c("Mainstem")
d.cr$season = c("Fall")
d.cr.kio.f = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.kio.f) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.kio.f = as.data.frame(d.cr.kio.f)
str(d.cr.kio.f)

d.gpp$stream = c("Kiona")
d.gpp$type = c("Mainstem")
d.gpp$season = c("Fall")
d.gpp.kio.f = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.kio.f) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.kio.f = as.data.frame(d.gpp.kio.f)
str(d.gpp.kio.f)

#################################################################
#KIONA SUMMER
#Load data
kiona_summer <- read.table(file="kiona_summer.csv", header=T, sep=",")

#set variable
d = kiona_summer

#recode function to change name of top
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check for outliers and check data entry before calculating CR-NRR
#ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#W8, V5 are outliers

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-8.067717 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#GPP area has 0 controls, so can't calculate NRR for GPP
#AA7, N+P+Si and Si have only one non-zero value
#probably need to drop this from GPP analysis
#ggplot(d.gpp, aes(x=nutrient, y=chla_ug_cm2)) + geom_boxplot() + theme_classic()
#Y6

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/0 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/1.3413552 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla_ug_cm2, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#complete information for files
d.cr$stream = c("Kiona")
d.cr$type = c("Mainstem")
d.cr$season = c("Summer")
d.cr.kio.s = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.kio.s) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.kio.s = as.data.frame(d.cr.kio.s)
str(d.cr.kio.s)

d.gpp$stream = c("Kiona")
d.gpp$type = c("Mainstem")
d.gpp$season = c("Summer")
d.gpp.kio.s = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.kio.s) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.kio.s = as.data.frame(d.gpp.kio.s)
str(d.gpp.kio.s)

#################################################################
#MABTON FALL
#Load data
mab_fall <- read.table(file="mab_fall.csv", header=T, sep=",")

#set variable
d = mab_fall

#recode function to change name of top
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check for outliers and check data entry before calculating CR-NRR
#ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#A5 is an outlier, Si group has 3 zeros so can't tell what is an outlier

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-21.727991 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#F7, P group has 2 zeros so can't tell what is an outlier, Si group looks OK 
#ggplot(d.gpp, aes(x=nutrient, y=chla_ug_cm2)) + geom_boxplot() + theme_classic()
#P+Si group is evenly spread out, Si and control look OK

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla_ug_cm2, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/3.3947683 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla_ug_cm2/3.136110 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla_ug_cm2, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#complete information for files
d.cr$stream = c("Mabton")
d.cr$type = c("Mainstem")
d.cr$season = c("Fall")
d.cr.mab.f = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.mab.f) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.mab.f = as.data.frame(d.cr.mab.f)
str(d.cr.mab.f)

d.gpp$stream = c("Mabton")
d.gpp$type = c("Mainstem")
d.gpp$season = c("Fall")
d.gpp.mab.f = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.mab.f) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.mab.f = as.data.frame(d.gpp.mab.f)
str(d.gpp.mab.f)

#################################################################
#MABTON SUMMER
#Load data
mab_summer <- read.table(file="mab_summer.csv", header=T, sep=",")

#set variable
d = mab_summer

#recode function to change name of top 
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check for outliers and check data entry before calculating CR-NRR
#ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#P5 is an outlier

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-13.104819 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#K1, N+P+Si group is bimodal, P+Si group is widely but evenly spread
#ggplot(d.gpp, aes(x=nutrient, y=chla_ug_cm2)) + geom_boxplot() + theme_classic()
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

#complete information for files
d.cr$stream = c("Mabton")
d.cr$type = c("Mainstem")
d.cr$season = c("Summer")
d.cr.mab.s = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.mab.s) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.mab.s = as.data.frame(d.cr.mab.s)
str(d.cr.mab.s)

d.gpp$stream = c("Mabton")
d.gpp$type = c("Mainstem")
d.gpp$season = c("Summer")
d.gpp.mab.s = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.mab.s) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.mab.s = as.data.frame(d.gpp.mab.s)
str(d.gpp.mab.s)

#################################################################
#REECER FALL
#Load data
reec_fall <- read.table(file="reec_fall.csv", header=T, sep=",")

#set variable
d = reec_fall

#recode function to change name of top 
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check distribution of controls and remove as needed before calculating NRR.
#ggplot(d.cr, aes(x=nutrient, y=cr.area))+geom_boxplot() +theme_classic()
#ok

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T))
x
d.cr$cr.nrr = d.cr$cr.area/-9.922869 #divide by control ave_cr

#check distribution of controls and remove as needed before calculating NRR.
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area))+geom_boxplot() +theme_classic()
#probably ok, but check AA3, AA6, X3
#ggplot(d.gpp, aes(x=nutrient, y=chla))+geom_boxplot() +theme_classic()+scale_y_continuous(limits=c(0,2))
#one super-high outlier - was a copy/paste error.updated with value from NDS spreadsheet.
#also check CC3

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/2.2686372 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.5705330 #divide by control ave_chla

#complete information for files
d.cr$stream = c("Reecer")
d.cr$type = c("Tributary")
d.cr$season = c("Fall")
d.cr.rec.f = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.rec.f) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.rec.f = as.data.frame(d.cr.rec.f)
str(d.cr.rec.f)

d.gpp$stream = c("Reecer")
d.gpp$type = c("Tributary")
d.gpp$season = c("Fall")
d.gpp.rec.f = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.rec.f) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.rec.f = as.data.frame(d.gpp.rec.f)
str(d.gpp.rec.f)

#################################################################
#REECER SUMMER
#Load data
reec_summer <- read.table(file="reec_summer.csv", header=T, sep=",")

#set variable
d = reec_summer

#recode function to change name of top
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check distribution of controls and remove as needed before calculating NRR.
#ggplot(d.cr, aes(x=nutrient, y=cr.area))+geom_boxplot() +theme_classic()
#ok

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-15.02240 #divide by control ave_cr

#check distribution of controls and remove as needed before calculating NRR.
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area))+geom_boxplot() +theme_classic()
#check CC1, but probably ok
#ggplot(d.gpp, aes(x=nutrient, y=chla))+geom_boxplot() +theme_classic()
#ok - updated b/c original file had values too small (units were mg/cm2)

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/5.770496 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.05252345 #divide by control ave_chla

#complete information for files
d.cr$stream = c("Reecer")
d.cr$type = c("Tributary")
d.cr$season = c("Summer")
d.cr.rec.s = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.rec.s) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.rec.s = as.data.frame(d.cr.rec.s)
str(d.cr.rec.s)

d.gpp$stream = c("Reecer")
d.gpp$type = c("Tributary")
d.gpp$season = c("Summer")
d.gpp.rec.s = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.rec.s) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.rec.s = as.data.frame(d.gpp.rec.s)
str(d.gpp.rec.s)

#################################################################
#RINGER FALL
#Load data
ring_fall <- read.table(file="ring_fall.csv", header=T, sep=",")

#set variable
d = ring_fall

#recode function to change name of top
#d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check for outliers and check data entry before calculating CR-NRR
#ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#P3, O2, T7, K5, L7 are entered correctly, controls look OK
#K3, M1, N8, R6, missing in field

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-11.397616 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#K2 is entered correctly
#K1, M7 missing in field
#ggplot(d.gpp, aes(x=nutrient, y=chla)) + geom_boxplot() + theme_classic()
#P and Si groups are somewhat spread but somewhat centered

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/3.232171 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.4619133 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla_ug_cm2, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#complete information for files
d.cr$stream = c("Ringer")
d.cr$type = c("Mainstem")
d.cr$season = c("Fall")
d.cr.rin.f = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.rin.f) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.rin.f = as.data.frame(d.cr.rin.f)
str(d.cr.rin.f)

d.gpp$stream = c("Ringer")
d.gpp$type = c("Mainstem")
d.gpp$season = c("Fall")
d.gpp.rin.f = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.rin.f) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.rin.f = as.data.frame(d.gpp.rin.f)
str(d.gpp.rin.f)

#################################################################
#RINGER SUMMER
#Load data
ring_summer <- read.table(file="ring_summer.csv", header=T, sep=",")

#set variable
d = ring_summer

#recode function to change name of top
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check for outliers and check data entry before calculating CR-NRR
#ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#looks OK
#R5 lost in the field

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-7.554539 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#K8 data entry checks out, can stay in analysis
#K1 lost in the field
#ggplot(d.gpp, aes(x=nutrient, y=chla)) + geom_boxplot() + theme_classic()
#S1, L7, N2 data entry checks out
#N2 could be excluded from analysis, but it won't affect NRR calculation
#K1 lost in the field

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/6.422422 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/1.824141 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#complete information for files
d.cr$stream = c("Ringer")
d.cr$type = c("Mainstem")
d.cr$season = c("Summer")
d.cr.rin.s = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.rin.s) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.rin.s = as.data.frame(d.cr.rin.s)
str(d.cr.rin.s)

d.gpp$stream = c("Ringer")
d.gpp$type = c("Mainstem")
d.gpp$season = c("Summer")
d.gpp.rin.s = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.rin.s) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.rin.s = as.data.frame(d.gpp.rin.s)
str(d.gpp.rin.s)

#################################################################
#ROZA FALL
#Load data
roza_fall <- read.table(file="roza_fall.csv", header=T, sep=",")

#set variable
d = roza_fall

#recode function to change name of top
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check for outliers and check data entry before calculating CR-NRR
#ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#J8 and G3 are entered correctly, controls look OK
#J8 and G3 might need to be removed
#G4, J1 missing in field

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-10.244565 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#no outliers
#F3, J7 missing in field
#ggplot(d.gpp, aes(x=nutrient, y=chla)) + geom_boxplot() + theme_classic()
#no outliers
#F3, J7 missing in field

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/3.991310 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.21477638 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla_ug_cm2, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#complete information for files
d.cr$stream = c("Roza")
d.cr$type = c("Mainstem")
d.cr$season = c("Fall")
d.cr.roz.f = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.roz.f) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.roz.f = as.data.frame(d.cr.roz.f)
str(d.cr.roz.f)

d.gpp$stream = c("Roza")
d.gpp$type = c("Mainstem")
d.gpp$season = c("Fall")
d.gpp.roz.f = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.roz.f) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.roz.f = as.data.frame(d.gpp.roz.f)
str(d.gpp.roz.f)

#################################################################
#ROZA SUMMER
#Load data
roza_summer <- read.table(file="roza_summer.csv", header=T, sep=",")

#set variable
d = roza_summer

#recode function to change name of top
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check for outliers and check data entry before calculating CR-NRR
#ggplot(d.cr, aes(x=nutrient, y=cr.area)) + geom_boxplot() + theme_classic()
#J1 entered correctly
#OK to keep in analysis

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-9.818865 #divide by control ave_cr

#check for outliers and check data entry before calculating GPP- and chla-NRR
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area)) + geom_boxplot() + theme_classic()
#J2 is entered correctly, P+Si are evenly spread
#A3 is missing in field

#ggplot(d.gpp, aes(x=nutrient, y=chla)) + geom_boxplot() + theme_classic()
#I1, F1 are entered correctly
#A3 is missing in field

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/4.059326 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/1.800835 #divide by control ave_chla
#use to exclude outliers
#x1<- ddply(subset(d.gpp, !(nds.id=="F6")), "nutrient", summarise, ave_chla = mean(chla_ug_cm2, na.rm=T)) 
#x1
#d.gpp$chla.nrr_1<-d.gpp$chla_ug_cm2/2.700340

#complete information for files
d.cr$stream = c("Roza")
d.cr$type = c("Mainstem")
d.cr$season = c("Summer")
d.cr.roz.s = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.roz.s) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.roz.s = as.data.frame(d.cr.roz.s)
str(d.cr.roz.s)

d.gpp$stream = c("Roza")
d.gpp$type = c("Mainstem")
d.gpp$season = c("Summer")
d.gpp.roz.s = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.roz.s) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.roz.s = as.data.frame(d.gpp.roz.s)
str(d.gpp.roz.s)

#################################################################
#SATUS FALL
#Load data
sat_fall <- read.table(file="satus_fall.csv", header=T, sep=",")

#set variable
d = sat_fall

#recode function to change name of top 
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-17.11723 #divide by control ave_cr

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla_ug_cm2, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/4.345200 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla_ug_cm2/0.1823788 #divide by control ave_chla

#complete information for files
d.cr$stream = c("Satus")
d.cr$type = c("Tributary")
d.cr$season = c("Fall")
d.cr.sat.f = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.sat.f) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.sat.f = as.data.frame(d.cr.sat.f)
str(d.cr.sat.f)

d.gpp$stream = c("Satus")
d.gpp$type = c("Tributary")
d.gpp$season = c("Fall")
d.gpp.sat.f = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.sat.f) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.sat.f = as.data.frame(d.gpp.sat.f)
str(d.gpp.sat.f)

#################################################################
#SATUS SUMMER
#Load data
sat_summer<- read.csv("satus_summer.csv")

#set variable
d = sat_summer

#recode function to change name of top
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T))
x
d.cr$cr.nrr = d.cr$cr.area/-25.18067 #divide by control ave_cr

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla_ug_cm2, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/2.7173342 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla_ug_cm2/0.8314271 #divide by control ave_chla

#complete information for files
d.cr$stream = c("Satus")
d.cr$type = c("Tributary")
d.cr$season = c("Summer")
d.cr.sat.s = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.sat.s) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.sat.s = as.data.frame(d.cr.sat.s)
str(d.cr.sat.s)

d.gpp$stream = c("Satus")
d.gpp$type = c("Tributary")
d.gpp$season = c("Summer")
d.gpp.sat.s = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.sat.s) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.sat.s = as.data.frame(d.gpp.sat.s)
str(d.gpp.sat.s)

#################################################################
#TOPPENISH FALL
#no data, site was inaccessible due to high flows

#################################################################
#TOPPENISH SUMMER
#Load data
topp <- read.table(file="toppenish_summer.csv", header=T, sep=",")

#set variable
d = topp

#recode function to change name of top
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check for outliers and check data entry before calculating NRR.
#ggplot(d.cr, aes(x=nutrient, y=cr.area))+geom_boxplot() +theme_classic()
#ok - one low NSi checks out

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-19.11745 #divide by control ave_cr

#check for outliers and check data entry before calculating NRR.
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area))+geom_boxplot() +theme_classic()
#ok
#ggplot(d.gpp, aes(x=nutrient, y=chla_ug_cm2))+geom_boxplot() +theme_classic()
#ok

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla_ug_cm2, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/2.323007 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla_ug_cm2/2.214887 #divide by control ave_chla

#complete information for files
d.cr$stream = c("Toppenish")
d.cr$type = c("Tributary")
d.cr$season = c("Summer")
d.cr.top.s = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.top.s) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.top.s = as.data.frame(d.cr.top.s)
str(d.cr.top.s)

d.gpp$stream = c("Toppenish")
d.gpp$type = c("Tributary")
d.gpp$season = c("Summer")
d.gpp.top.s = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.top.s) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.top.s = as.data.frame(d.gpp.top.s)
str(d.gpp.top.s)

#################################################################
#WENAS FALL
#Load data
wenas_fall <- read.table(file="wen_fall.csv", header=T, sep=",")

#set variable
d = wenas_fall

#recode function to change name of top
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check distribution of controls and remove as needed before calculating NRR.
#ggplot(d.cr, aes(x=nutrient, y=cr.area))+geom_boxplot() +theme_classic()
#check A5, others ok

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T))
x
d.cr$cr.nrr = d.cr$cr.area/-7.662895 #divide by control ave_cr

#check distribution of controls and remove as needed before calculating NRR.
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area))+geom_boxplot() +theme_classic()
#ok
#ggplot(d.gpp, aes(x=nutrient, y=chla))+geom_boxplot() +theme_classic()
#ok

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
d.gpp$gpp.nrr = d.gpp$gpp.area/2.5640416 #divide by control ave_gpp
d.gpp$chla.nrr = d.gpp$chla/0.6598855 #divide by control ave_chla

#complete information for files
d.cr$stream = c("Wenas")
d.cr$type = c("Tributary")
d.cr$season = c("Fall")
d.cr.wen.f = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.wen.f) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.wen.f = as.data.frame(d.cr.wen.f)
str(d.cr.wen.f)

d.gpp$stream = c("Wenas")
d.gpp$type = c("Tributary")
d.gpp$season = c("Fall")
d.gpp.wen.f = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.wen.f) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.wen.f = as.data.frame(d.gpp.wen.f)
str(d.gpp.wen.f)

#################################################################
#WENAS SUMMER
#Load data
wenas_summer <- read.table(file="wen_summer.csv", header=T, sep=",")

#set variable
d = wenas_summer

#recode function to change name of top
d$top<-recode(d$top, "cellulose" ="sponge")
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge")
d.gpp = subset(d, top=="glass")

#check distribution of controls and remove as needed before calculating NRR.
#ggplot(d.cr, aes(x=nutrient, y=cr.area))+geom_boxplot() +theme_classic()
#ok

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-5.765783 #divide by control ave_cr

#check distribution of controls and remove as needed before calculating NRR.
#ggplot(d.gpp, aes(x=nutrient, y=gpp.area))+geom_boxplot() +theme_classic()
#one outlier. C8 has GPP = 2.94, which is > 4x as high as next-highest control. Remove?
#also, C2 has GPP of 12.68, which is 3x as high as others in N treatment, but just slightly
#higher than those in NPSi. Numbers were checked and correct, but concern over effect of control on NRR.
#ggplot(d.gpp, aes(x=nutrient, y=chla))+geom_boxplot() +theme_classic()
#G8 has chla of 0.94, which is >7x higher than other controls. Remove?
#also, F5 has chla = 4.3, which is almost twice as high as next highest. Numbers were checked and are correct
#but are they too high to be reasonable?

#calculate nrr for gpp and chla
x<- ddply(d.gpp, "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T)) 
x
x1c<-ddply(subset(d.gpp, !(nds.id=="G8")), "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T))
x1c
x1g<-ddply(subset(d.gpp, !(nds.id=="C8")), "nutrient", summarise, ave_gpp = mean(gpp.area, na.rm=T), ave_chla = mean(chla, na.rm=T))
x1g
#note: chose to keep G8 out of the chla calcs and C8 out of the GPP calcs.

d.gpp$gpp.nrr = d.gpp$gpp.area/0.2308552 #divide by control ave_gpp
d.gpp$chla.nrr=d.gpp$chla/0.03803029

#complete information for files
d.cr$stream = c("Wenas")
d.cr$type = c("Tributary")
d.cr$season = c("Summer")
d.cr.wen.s = cbind(d.cr$stream, d.cr$type, d.cr$season, 
                   d.cr$nutrient, d.cr$cr.nrr)
colnames(d.cr.wen.s) = c("stream", "type", "season", "nutrient", "cr.nrr")
d.cr.wen.s = as.data.frame(d.cr.wen.s)
str(d.cr.wen.s)

d.gpp$stream = c("Wenas")
d.gpp$type = c("Tributary")
d.gpp$season = c("Summer")
d.gpp.wen.s = cbind(d.gpp$stream, d.gpp$type, d.gpp$season, 
                    d.gpp$nutrient, d.gpp$gpp.nrr, d.gpp$chla.nrr)
colnames(d.gpp.wen.s) = c("stream", "type", "season", 
                          "nutrient", "gpp.nrr", "chla.nrr")
d.gpp.wen.s = as.data.frame(d.gpp.wen.s)
str(d.gpp.wen.s)

#################################################################
#################################################################
#combine files into cr.nrr and gpp.nrr for whole study
cr.nrr.all = rbind(d.cr.aht.f, d.cr.aht.s,
                   d.cr.cen.f, d.cr.cen.s,
                   d.cr.cle.f, d.cr.cle.s,
                   d.cr.mab.f, d.cr.mab.s,
                   d.cr.kio.f, d.cr.kio.s,
                   d.cr.rec.f, d.cr.rec.s,
                   d.cr.rin.f, d.cr.rin.s,
                   d.cr.roz.f, d.cr.roz.s,
                   d.cr.sat.f, d.cr.sat.s,
                   d.cr.top.s,
                   d.cr.wen.f, d.cr.wen.s)

chla.nrr.all = rbind(d.gpp.aht.f, d.gpp.aht.s,
                     d.gpp.cen.f, d.gpp.cen.s,
                     d.gpp.cle.f, d.gpp.cle.s,
                     d.gpp.mab.f, d.gpp.mab.s,
                     d.gpp.kio.f, d.gpp.kio.s,
                     d.gpp.rec.f, d.gpp.rec.s,
                     d.gpp.rin.f, d.gpp.rin.s,
                     d.gpp.roz.f, d.gpp.roz.s,
                     d.gpp.sat.f, d.gpp.sat.s,
                     d.gpp.top.s,
                     d.gpp.wen.f, d.gpp.wen.s)

#recode nutrient names
cr.nrr.all$nutrient <- recode(cr.nrr.all$nutrient, "N+P" = "NP")
cr.nrr.all$nutrient <- recode(cr.nrr.all$nutrient, "N+Si" = "NSi")
cr.nrr.all$nutrient <- recode(cr.nrr.all$nutrient, "P+Si" = "PSi")
cr.nrr.all$nutrient <- recode(cr.nrr.all$nutrient, "N+P+Si" = "NPSi")
cr.nrr.all$nutrient <- recode(cr.nrr.all$nutrient, "C" = "control")

chla.nrr.all$nutrient <- recode(chla.nrr.all$nutrient, "N+P" = "NP")
chla.nrr.all$nutrient <- recode(chla.nrr.all$nutrient, "N+Si" = "NSi")
chla.nrr.all$nutrient <- recode(chla.nrr.all$nutrient, "P+Si" = "PSi")
chla.nrr.all$nutrient <- recode(chla.nrr.all$nutrient, "N+P+Si" = "NPSi")
chla.nrr.all$nutrient <- recode(chla.nrr.all$nutrient, "C" = "control")

#convert nrrs to numeric
cr.nrr.all$cr.nrr <- as.numeric(cr.nrr.all$cr.nrr)
chla.nrr.all$chla.nrr <- as.numeric(chla.nrr.all$chla.nrr)

#################################################################
#make graphics for cr.nrr for all streams

#summarize data
#summarize data for error bar plot and subset by season
x = group_by(cr.nrr.all, stream, nutrient, season) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(mNut = mean(cr.nrr, na.rm = T), # na.rm = TRUE to remove missing values
            sNut = sd(cr.nrr, na.rm = T),  # na.rm = TRUE to remove missing values
            nNut = sum(!is.na(cr.nrr)),
            eNut = sNut/sqrt(nNut))

cr.nrr.all$cr.nrr <- as.numeric(cr.nrr.all$cr.nrr)

#force streams to plot in specific order
x$stream = factor(x$stream, levels = c('Cle Elum', 'Ringer', 'Roza', 'Century Landing', 'Mabton', 'Kiona', 
                                       'Reecer', 'Wenas', 'Ahtanum', 'Toppenish', 'Satus')) 

x.summer <- subset(x, season=="Summer")
x.fall <- subset(x, season=="Fall")

#make plot of CR Summer
cr.summer = 
  ggplot(subset(x.summer, !(nutrient=="control")), aes(x=stream, y=mNut, fill=nutrient)) +
  scale_color_manual(values=c("red", "darkorange2", "slategray4", "darkorchid", "gold", "green3", "steelblue3")) +
  geom_pointrange(aes(color=nutrient, ymin=(mNut-eNut), ymax=(mNut+eNut)), 
                                               position = position_jitter(width=0.15),
                                               linetype = 1) +
  #geom_point(aes(color=factor(nutrient)), size=3, position = "jitter") +
  #geom_errorbar(aes(ymin=(mNut-eNut), ymax=(mNut+eNut), width=2.5)) +
  theme_classic() +
  ylab(expression(NRR[R])) +
  ylim(0,4) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_vline(aes(xintercept = 6.5)) +
  ggtitle("Summer") +
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "top", #change to top for individual plot
        axis.text.x=element_blank(),
        legend.title = element_blank()) +
  guides(colour = guide_legend(nrow = 1)) +
  annotate("text", x=1, y=3.8, label="A", size=4) +
  annotate("text", x=3.5, y=3.8, label="Mainstem", size=4) +
  annotate("text", x=9, y=3.8, label="Tributary", size=4)
  
#make plot of CR Fall
cr.fall = 
  ggplot(subset(x.fall, !(nutrient=="control")), aes(x=stream, y=mNut, fill=nutrient)) +
  scale_color_manual(values=c("red", "darkorange2", "slategray4", "darkorchid", "gold", "green3", "steelblue3")) +
  geom_pointrange(aes(color=nutrient, ymin=(mNut-eNut), ymax=(mNut+eNut)), 
                  position = position_jitter(width=0.15),
                  linetype = 1) +
  #geom_point(aes(color=factor(nutrient)), size=3, position = "jitter") +
  #geom_errorbar(aes(ymin=(mNut-eNut), ymax=(mNut+eNut), width=2.5)) +
  theme_classic() +
  #ylab(expression(CR~NRR)) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_vline(aes(xintercept = 6.5)) +
  ggtitle("Autumn") +
  ylim(0,4) +
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y=element_blank(),
        legend.title = element_blank(),
        legend.position = "top", #change to top for individual plot
        axis.text.x=element_blank()) +
  guides(colour = guide_legend(nrow = 1)) +
  annotate("text", x=1, y=3.8, label="B", size=4) +
  annotate("text", x=3.5, y=3.8, label="Mainstem", size=4) +
  annotate("text", x=9, y=3.8, label="Tributary", size=4)  

#################################################################
#make graphics for chla.nrr for all streams

#summarize data
#summarize data for error bar plot and subset by season
x = group_by(chla.nrr.all, stream, nutrient, season) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(mNut = mean(chla.nrr, na.rm = T), # na.rm = TRUE to remove missing values
            sNut = sd(chla.nrr, na.rm = T),  # na.rm = TRUE to remove missing values
            nNut = sum(!is.na(chla.nrr)),
            eNut = sNut/sqrt(nNut))

#force streams to plot in specific order
x$stream = factor(x$stream, levels = c('Cle Elum', 'Ringer', 'Roza', 'Century Landing', 'Mabton', 'Kiona', 
                                       'Reecer', 'Wenas', 'Ahtanum', 'Toppenish', 'Satus')) 

x.summer <- subset(x, season=="Summer")
x.fall <- subset(x, season=="Fall")

#make plot of CHLA Summer
chla.summer = 
  ggplot(subset(x.summer, !(nutrient=="control")), aes(x=stream, y=mNut, fill=nutrient)) +
  scale_color_manual(values=c("red", "darkorange2", "slategray4", "darkorchid", "gold", "green3", "steelblue3")) +
  geom_pointrange(aes(color=nutrient, ymin=(mNut-eNut), ymax=(mNut+eNut)), 
                  position = position_jitter(width=0.15),
                  linetype = 1) +
  #geom_point(aes(color=factor(nutrient)), size=3, position = "jitter") +
  #geom_errorbar(aes(ymin=(mNut-eNut), ymax=(mNut+eNut), width=2.5)) +
  theme_classic() +
  ylab(expression(NRR[Chla])) +
  xlab(expression(Sites)) +
  xlim(0,40) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_vline(aes(xintercept = 6.5)) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  scale_x_discrete(labels=c("Cl", "Ri", "Ro", "Ce", "Ma", "Ki",
                                  "Re", "We", "Ah", "To", "Sa")) +  #change to top for individual plot
  guides(colour = guide_legend(nrow = 1)) +
  annotate("text", x=1, y=38, label="C", size=4) 
  #annotate("text", x=3.5, y=40, label="Mainstem", size=4) +
  #annotate("text", x=9, y=40, label="Tributary", size=4)

#make plot of CHLA Fall
chla.fall = 
  ggplot(subset(x.fall, !(nutrient=="control")), aes(x=stream, y=mNut, fill=nutrient)) +
  scale_color_manual(values=c("red", "darkorange2", "slategray4", "darkorchid", "gold", "green3", "steelblue3")) +
  geom_pointrange(aes(color=nutrient, ymin=(mNut-eNut), ymax=(mNut+eNut)), 
                  position = position_jitter(width=0.15),
                  linetype = 1) +
  #geom_point(aes(color=factor(nutrient)), size=3, position = "jitter") +
  #geom_errorbar(aes(ymin=(mNut-eNut), ymax=(mNut+eNut), width=2.5)) +
  theme_classic() +
  #ylab(expression(Chl-italic(a)~NRR)) +
  xlab(expression(Sites)) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_vline(aes(xintercept = 6.5)) +
  theme(axis.title.y=element_blank(),
        legend.title = element_blank(),
        legend.position = "top") +  #change to top for individual plot
  scale_x_discrete(labels=c("Cl", "Ri", "Ro", "Ce", "Ma", "Ki", 
                            "Re", "We", "Ah", "To", "Sa")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) +
  guides(colour = guide_legend(nrow = 1)) +
  annotate("text", x=1, y=2.3, label="D", size=4) 
  #annotate("text", x=3.5, y=2.4, label="Mainstem", size=4) +
  #annotate("text", x=9, y=2.4, label="Tributary", size=4)  

#################################################################
#compile figure

#gA <- ggplotGrob(cr.summer)  # set up figure
#gB <- ggplotGrob(cr.fall)  # set up figure
#gC <- ggplotGrob(chla.summer)  # set up figure
#D <- ggplotGrob(chla.fall)  # set up figure

#maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5], gC$widths[2:5], gD$widths[2:5])  # set up figure

#gA$widths[2:5] <- as.list(maxWidth)  # set up figure
#gB$widths[2:5] <- as.list(maxWidth)  # set up figure
#gC$widths[2:5] <- as.list(maxWidth)
#gD$widths[2:5] <- as.list(maxWidth)

ggarrange(cr.summer, cr.fall, chla.summer, chla.fall, ncol=2, nrow=2,
             common.legend = T, #They use the same legend
             legend = "top", align = "h")

fig2 = ggarrange(cr.summer, cr.fall, chla.summer, chla.fall, ncol=2, nrow=2,
                 common.legend = T, #They use the same legend
                 legend = "top", align = "h")

tiff(filename = 'fig2TE.tiff', #open plotting device
     width = 6.5,
     height = 4.0,
     units = "in",
     res = 1200,
     compression = "lzw")

fig2# push plot to device

dev.off()  # close device
