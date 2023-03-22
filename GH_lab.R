#script to analyze the greenhouse vs. lab comparison of nutrient limitation. 
#first, determine nutrient limitation within substrate type and incubation location. 
#then, compare among locations

#install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")

#packages
library(nlme)
library(dplyr)
library(ggplot2)
library(gridExtra)

#Load data
gh_lab <- read.csv("GH_lab_Columbia.csv")
cwu_lab <- read.csv("Ganges_Lab.csv")
cwu_gh <- read.csv("Ganges_GH.csv")

comb <- read.csv("GH_lab_combined.csv") # analyze columbia and ditch together

#convert N and P values (0 or 1 for absence or presence) to factors
gh_lab$N <- as.factor(gh_lab$N)
gh_lab$P <- as.factor(gh_lab$P)

comb$N <- as.factor(comb$N)
comb$P <- as.factor(comb$P)


#subset data into gpp and cr response
gh <- subset(gh_lab, incubation=="GH", data=gh_lab)
lab <- subset(gh_lab, incubation =="lab", data=gh_lab)

comb_gpp <- subset(comb, top == "glass", data=comb)
comb_cr <- subset(comb, top == "cellulose", data=comb)

############################################################
#plots!
GH_CR<-ggplot(data=subset(gh, top=="cellulose"), aes(x=nutrient, y = -cr.area))+geom_boxplot()+theme_classic()+
  ggtitle("GH cellulose")+ylim(10,30)
lab_CR<-ggplot(data=subset(lab, top=="cellulose"), aes(x=nutrient, y = -cr.area))+geom_boxplot()+theme_classic()+
  ggtitle("lab cellulose")+ylim(10,30)
grid.arrange(GH_CR, lab_CR, ncol=2)

GH_CR.NRR<-ggplot(data=subset(gh, top=="cellulose"), aes(x=nutrient, y = cr.nrr))+geom_boxplot()+theme_classic()+
  ggtitle("GH cellulose")
lab_CR.NRR<-ggplot(data=subset(lab, top=="cellulose"), aes(x=nutrient, y =cr.nrr))+geom_boxplot()+theme_classic()+
  ggtitle("lab cellulose")
grid.arrange(GH_CR.NRR, lab_CR.NRR, ncol=2)

GH_GPP<-ggplot(data=subset(gh, top=="glass"), aes(x=nutrient, y = gpp.area))+geom_boxplot()+theme_classic()+
  ggtitle("GH glass")+ylim(0,25)
lab_GPP<-ggplot(data=subset(lab, top=="glass"), aes(x=nutrient, y = gpp.area))+geom_boxplot()+theme_classic()+
  ggtitle("lab glass")+ylim(0,25)
grid.arrange(GH_GPP, lab_GPP, ncol=2)

GH_GPP.NRR<-ggplot(data=subset(gh, top=="glass"), aes(x=nutrient, y = gpp.nrr))+geom_boxplot()+theme_classic()+
  ggtitle("GH glass")
lab_GPP.NRR<-ggplot(data=subset(lab, top=="glass"), aes(x=nutrient, y =gpp.nrr))+geom_boxplot()+theme_classic()+
  ggtitle("lab glass")
grid.arrange(GH_GPP.NRR, lab_GPP.NRR, ncol=2)

GH_chla<-ggplot(data=gh, aes(x=nutrient, y=chla_mgL))+geom_boxplot()+theme_classic()+
  ggtitle("GH")
lab_chla<-ggplot(data=lab, aes(x=nutrient, y=chla_mgL))+geom_boxplot()+theme_classic()+
  ggtitle("lab")
grid.arrange(GH_chla, lab_chla, ncol=2)

GH_chla.NRR<-ggplot(data=gh, aes(x=nutrient, y = chla.nrr))+geom_boxplot()+theme_classic()+
  ggtitle("GH")
lab_chla.NRR<-ggplot(data=lab, aes(x=nutrient, y =chla.nrr))+geom_boxplot()+theme_classic()+
  ggtitle("lab")
grid.arrange(GH_chla.NRR, lab_chla.NRR, ncol=2)

#combined CR figure for revision
columbia_cr <- ggplot(data = subset(comb_cr, location == "columbia"), aes(x = nutrient, y = -cr.area, fill = incubation)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(name="Incubation", values=c("black", "white")) +
#  ggtitle("Columbia") + #delete title for final figure
  ylim(10,30) +
  ylab(expression(CR~(mu*g~O[2]~cm^{2}~h^{-1}))) +
  theme(legend.position = "none",
        axis.title.x=element_blank()) +
  annotate("text", x=0.75, y=30, label="A", size=4)

ditch_cr <- ggplot(data = subset(comb_cr, location == "ditch"), aes(x = nutrient, y = -cr.area, fill = incubation)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(labels = c("greenhouse", "lab"), values=c("black", "white")) +
  #  ggtitle("Ditch") + #delete title for final figure
  ylim(10,30) +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position = c(0.8, 0.8),
        legend.title = element_blank()) + 
  annotate("text", x=0.75, y=30, label="B", size=4)


gA <- ggplotGrob(columbia_cr)  # set up figure
gB <- ggplotGrob(ditch_cr)  # set up figure

maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])  # set up figure

gA$widths[2:5] <- as.list(maxWidth)  # set up figure
gB$widths[2:5] <- as.list(maxWidth)  # set up figure

grid.arrange(gA, gB, ncol=2)

#combined CR-NRR figure for revision
columbia_crnrr <- ggplot(data = subset(comb_cr, location == "columbia"), aes(x = nutrient, y = cr.nrr, fill = incubation)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(name="Incubation", values=c("black", "white")) +
#  ggtitle("Columbia") + #delete title for final figure
  ylim(0,3) +
  ylab(expression(CR~NRR)) +
  theme(legend.position = "none",
        axis.title.x=element_blank()) +
  scale_x_discrete(limit = c("N", "NP", "P")) +
  geom_hline(yintercept = 1, lty = 2) +
  annotate("text", x=0.65, y=3, label="C", size=4)

ditch_crnrr <- ggplot(data = subset(comb_cr, location == "ditch"), aes(x = nutrient, y = cr.nrr, fill = incubation)) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(name="Incubation", values=c("black", "white")) +
#  ggtitle("Ditch") + #delete title for final figure
  ylim(0,3) +
  theme(legend.position = "none",
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  scale_x_discrete(limit = c("N", "NP", "P")) +
  geom_hline(yintercept = 1, lty = 2) +
  annotate("text", x=0.65, y=3, label="D", size=4)


gC <- ggplotGrob(columbia_crnrr)  # set up figure
gD <- ggplotGrob(ditch_crnrr)  # set up figure

maxWidth = grid::unit.pmax(gC$widths[2:5], gD$widths[2:5])  # set up figure

gC$widths[2:5] <- as.list(maxWidth)  # set up figure
gD$widths[2:5] <- as.list(maxWidth)  # set up figure

grid.arrange(gC, gD, ncol=2)

#4 panel plot of CR and CR.nrr for revision

gA <- ggplotGrob(columbia_cr)  # set up figure
gB <- ggplotGrob(ditch_cr)  # set up figure
gC <- ggplotGrob(columbia_crnrr)  # set up figure
gD <- ggplotGrob(ditch_crnrr)  # set up figure

maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5], gC$widths[2:5], gD$widths[2:5])  # set up figure

gA$widths[2:5] <- as.list(maxWidth)  # set up figure
gB$widths[2:5] <- as.list(maxWidth)  # set up figure
gC$widths[2:5] <- as.list(maxWidth)
gD$widths[2:5] <- as.list(maxWidth)

grid.arrange(gA, gB, gC, gD, ncol=2, nrow=2)


#combined GPP figure for revision--not needed bc we don't report gpp
#columbia_gpp <- ggplot(data = subset(comb_gpp, location == "columbia"), aes(x = nutrient, y = gpp.area, fill = incubation)) +
#  geom_boxplot() +
#  theme_classic() +
#  scale_fill_manual(name="Incubation", values=c("black", "white")) +
#  ggtitle("Columbia") + #delete title for final figure
#  ylim(0,30) +
#  ylab(expression(GPP~(mg~O2~cm^{2}~h^{-1}))) +
#  theme(legend.position = "none",
#        axis.title.x=element_blank()) +
#  annotate("text", x=1, y=30, label="A", size=4)
#
#ditch_gpp <- ggplot(data = subset(comb_cr, location == "ditch"), aes(x = nutrient, y = gpp.area, fill = incubation)) +
#  geom_boxplot() +
#  theme_classic() +
#  scale_fill_manual(name="Incubation", values=c("black", "white")) +
#  ggtitle("Ditch") + #delete title for final figure
#  ylim(0,30) +
#  theme(axis.text.y=element_blank(),
#        axis.title.y=element_blank(),
#        axis.title.x=element_blank(),
#        legend.position = c(0.7, 0.9)) +
#  annotate("text", x=1, y=30, label="B", size=4)
#
#gA <- ggplotGrob(columbia_gpp)  # set up figure
#gB <- ggplotGrob(ditch_gpp)  # set up figure
#
#maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])  # set up figure
#
#gA$widths[2:5] <- as.list(maxWidth)  # set up figure
#gB$widths[2:5] <- as.list(maxWidth)  # set up figure
#
#grid.arrange(gA, gB, ncol=2)





#analyze CR data
M1<-aov(cr.area~N*P, data=subset(gh, top=="cellulose"))
summary(M1)
M2<-aov(cr.area~N*P, data=subset(lab, top=="cellulose"))
summary(M2)
M3<-aov(cr.nrr~nutrient, data=subset(gh, top =="cellulose"))
summary(M3) #p = 0.11
M4<-aov(cr.nrr~nutrient, data=subset(lab, top =="cellulose"))
summary(M4)#p = 0.674

#analyze gpp data
M1<-aov(gpp.area~N*P, data=subset(gh, top =="glass"))
summary(M1)
M2<-aov(gpp.area~N*P, data=subset(lab, top =="glass"))
summary(M2)
M3<-aov(gpp.nrr~nutrient, data=subset(gh, top =="glass"))
summary(M3) 
M4<-aov(gpp.nrr~nutrient, data=subset(lab, top =="glass"))
summary(M4)

#analyze chla data
M1<-aov(chla_mgL~N*P, data=subset(gh, top =="glass"))
summary(M1)
M2<-aov(chla_mgL~N*P, data=subset(lab, top =="glass"))
summary(M2)
M3<-aov(chla.nrr~nutrient, data=gh)
summary(M3) 
TukeyHSD(M3)
M4<-aov(chla.nrr~nutrient, data=lab)
summary(M4)
TukeyHSD(M4)

#analyze COMBINED CR data 
columbia = subset(comb_gpp, location == "columbia")

M1 <- aov(cr.area ~ N*P, data = subset(columbia, incubation == "GH"))
summary(M1)
#no limitation

M2 <- aov(cr.area ~ N*P, data = subset(columbia, incubation == "lab"))
summary(M2)
#N*P interaction term is significant bc it is lower, but interpretation is no limitation

M3 <- aov(cr.nrr ~ nutrient, data = subset(columbia, incubation == "GH"))
summary(M3)
#not different from each other

M4 <- aov(cr.nrr ~ nutrient, data = subset(columbia, incubation == "lab"))
summary(M4)
#not different from each other

ditch = subset(comb_cr, location == "ditch")

M5 <- aov(cr.area ~ N*P, data = subset(ditch, incubation == "GH"))
summary(M5)
#N limitation

M6 <- aov(cr.area ~ N*P, data = subset(ditch, incubation == "lab"))
summary(M6)
#N limitation

M7 <- aov(cr.nrr ~ N*P, data = subset(ditch, incubation == "GH"))
summary(M7)
TukeyHSD(M7)
#N limitation

M8 <- aov(cr.nrr ~ N*P, data = subset(ditch, incubation == "lab"))
summary(M8)
TukeyHSD(M8)
#N limitation

#analyze COMBINED GPP data
columbia = subset(comb_gpp, location == "columbia")

M1 <- aov(gpp.area ~ N*P, data = subset(columbia, incubation == "GH"))
summary(M1)
#no limitation

M2 <- aov(gpp.area ~ N*P, data = subset(columbia, incubation == "lab"))
summary(M2)
#no limitation

ditch = subset(comb_gpp, location == "ditch")
M3 <- aov(gpp.area ~ N*P, data = subset(ditch, incubation == "GH"))
summary(M3)
#no limitation

M4 <- aov(gpp.area ~ N*P, data = subset(ditch, incubation == "lab"))
summary(M4)
#no limitation











