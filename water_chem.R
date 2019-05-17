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

#averages
N_ave<- ddply(DIN, c("Site_Full_Name", "Time", "type"), summarise, NO3_ave = mean(NO3_mgNL), NH4_ave = mean(NH4_mgNL), DIN_ave = mean(DIN_mgNL)) 
N_ave

write.table(N_ave, "DIN_summary.csv", sep=",", quote=F, row.names=F)

#did some Excel data manipulation for stacked bar...
DIN_stackedbar<-read.csv("DIN_stackedbar.csv")

#plot
DIN_stackedbar$Site_Full_Name<-factor(DIN_stackedbar$Site_Full_Name,levels= c("Reecer", "Wenas", "Ahtanum", "Toppenish", "Satus")) 
DIN_stackedbar$Time<-factor(DIN_stackedbar$Time,levels= c("summer", "fall")) 

ggplot(subset(DIN_stackedbar, type=="trib"), aes(fill=DIN_type, x=Site_Full_Name, y=N_mgL))+geom_col()+theme_classic()+
  theme(axis.title.x =element_blank())+ylab("DIN (mg N/L)")+scale_fill_manual(values = c("springgreen4","cyan4"))+
  facet_grid(~Time)+theme(axis.text.x=element_text(size = 12, angle =20), axis.text.y = element_text(size=12),
                                  axis.title.y = element_text(size=14), legend.position="none")

                                                                              