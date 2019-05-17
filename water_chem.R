#Author:  Sarah Roley
#Date Created: 17-May-2019
#Script to analyze water chemistry data from NDS deployment with C. Arango and A. Alexiades

#packages
library(nlme)
library(plyr)
library(dplyr)
library(ggplot2)

#Load data
DIN<-read.csv("DIN.csv")
str(DIN)


#calculate DIN
DIN$DIN_mgNL<-DIN$NH4_mgNL+DIN$NO3_mgNL
DIN$type<-recode(DIN$Site, "'YAK'='main'; 'Selah'='main', 'Cle Elum'='main'; 'Ellensburg'='main'; 'Main Stem 3'='main'; 'Main Stem 1' ='main';
                 'Main Stem 2' = 'main'; 'Ringer'='main'")

#calculate nrr for gpp and chla
N_ave<- ddply(DIN, c("Site_Full_Name", "Time"), summarise, NO3_ave = mean(NO3_mgNL), NH4_ave = mean(NH4_mgNL), DIN_ave = mean(DIN_mgNL)) 
N_ave

write.table(N_ave, "DIN_summary.csv", sep=",", quote=F, row.names=F)

ggplot(N_ave, aes(x=Site_full))