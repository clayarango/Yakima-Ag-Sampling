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
DOC_TDN<-read.csv("DOC_TDN.csv")

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

DOC_ave<- ddply(DOC_TDN, c("stream", "season", "type"), summarise, DOC_ave = mean(doc.mg.l), TDN_ave=mean(tdn.mg.l)) 
DOC_ave

DOC_ave$stream<-recode(DOC_ave$stream, "'ahtanum'='Ahtanum'; 'reecer'='Reecer';
                       'satus'='Satus'; 'toppenish'='Toppenish'; 'wenas'='Wenas'")
str(DOC_ave)

write.table(DOC_ave, "DOC_TDN_summary.csv", sep=",", quote=F, row.names=F)

DOC_ave<-read.csv("DOC_TDN_summary.csv")

DOC_ave$stream<-factor(DOC_ave$stream, levels=c("reecer", "wenas", "ahtanum", "toppenish", "satus"))

str(DOC_ave)
ggplot(subset(DOC_ave, type=="trib"), aes(x=stream, y=DOC_ave))+geom_point(aes(color=factor(season)), size=3)+
  theme_classic()+scale_color_manual(values=c("chocolate1", "green4"))

ggplot(subset(DOC_ave, type=="trib"), aes(x=stream, y=TDN_ave))+geom_point(aes(color=factor(season)), size=3)+
  theme_classic()+scale_color_manual(values=c("chocolate1", "green4"))
