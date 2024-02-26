#Example NDS analysis Figure
#Uses Summer Ringer CR

#packages
install.packages("nlme")
install.packages("nortest")
install.packages("plyr")
install.packages("dplyr")
install.packages("multcomp")
install.packages("MASS")
install.packages("ggplot2")
install.packages(gridExtra)
library(nlme)
library(nortest)
library(plyr)
library(dplyr)
library(multcomp)
library(MASS)
library(ggplot2)
library(gridExtra)

#Load data
ring_summer <- read.table(file="ring_summer.csv", header=T, sep=",")

#set variable
d = ring_summer

#convert N and P and Si values (0 or 1 for absence or presence) to factors
d$N<-as.factor(d$N)
d$P<-as.factor(d$P)
d$Si<-as.factor(d$Si)
str(d)

#subset data into gpp and cr response
d.cr = subset(d, top=="sponge", data=d)

#calculate nrr for cr
x<-ddply(d.cr, "nutrient", summarise, ave_cr = mean(cr.area, na.rm=T)) 
x
d.cr$cr.nrr = d.cr$cr.area/-7.554539 #divide by control ave_cr

#remove control from CR for plotting first panel
d.cr.panel1 = subset(d.cr, nutrient != "C", data=d)

#rename nutrients to remove "+"
d.cr.panel1$nutrient <- recode(d.cr.panel1$nutrient, "P+Si" = "PSi")
d.cr.panel1$nutrient <- recode(d.cr.panel1$nutrient, "N+P" = "NP")
d.cr.panel1$nutrient <- recode(d.cr.panel1$nutrient, "N+P+Si" = "NPSi")
d.cr.panel1$nutrient <- recode(d.cr.panel1$nutrient, "N+Si" = "NSi")


###############
#plot of NRR
##############
ringer.sum.cr = ggplot(data=subset(d.cr.panel1, !(nutrient=="control")), aes(x=nutrient, y=cr.nrr)) +
  geom_boxplot() +
  theme_bw() +
  ylab("CR NRR") +
  xlab("Nutrient") +
  geom_abline(slope = 0, intercept = 1) +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank()) +
  annotate("text", x=1, y=2.5, label="A", size=4)

gA <- ggplotGrob(ringer.sum.cr)  # set up figure

###############
#interaction plots
##############
#N and P
xx = na.omit(subset(d.cr, select = c(N,P,cr.area)))
#interaction.plot(xx$N, xx$P, xx$cr.area*-1)

#summarize mean and SE by nutrient
sum <- group_by(xx, N, P) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(avg = mean(cr.area, na.rm = TRUE)*-1, # na.rm = TRUE to remove missing values
            stdev=sd(cr.area, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(cr.area)), # of observations, excluding NAs. 
            se=stdev/sqrt(n))

#make plot
NP.int = ggplot(sum, aes(x=N, y=avg, group = P)) +
  geom_point() +
  geom_line(aes(group=P, linetype = P)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=0.1) +
  theme_classic() +
  ylab(expression(CR~(mu*g~O[2]~cm^{-2}~h^{-1}))) +
  ylim(0,20) +
  theme(legend.justification=c(0.03,0.6),
        aspect.ratio = 0.75, #reduces white space on left and right of data
        legend.position=c(0.2,0.85),
        legend.text=element_text(size=10)) +
  annotate("text", x=0.5, y=19, label="B", size=4)

gB <- ggplotGrob(NP.int)  # set up figure

#N and Si
xx = na.omit(subset(d.cr, select = c(N,Si,cr.area)))
#interaction.plot(xx$N, xx$Si, xx$cr.area*-1)

#summarize mean and SE by nutrient
sum <- group_by(xx, N, Si) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(avg = mean(cr.area, na.rm = TRUE)*-1, # na.rm = TRUE to remove missing values
            stdev=sd(cr.area, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(cr.area)), # of observations, excluding NAs. 
            se=stdev/sqrt(n))

#make plot
NSi.int = ggplot(sum, aes(x=N, y=avg, group = Si)) +
  geom_point() +
  geom_line(aes(group=Si, linetype = Si)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=0.1) +
  theme_classic() +
  ylab(expression(CR~(mu*g~O[2]~cm^{-2}~h^{-1}))) +
  ylim(0,20) +
  theme(legend.justification=c(0.03,0.6),
        aspect.ratio = 0.75, #reduces white space on left and right of data
        legend.position=c(0.2,0.85),
        legend.text=element_text(size=10)) +
  annotate("text", x=0.5, y=19, label="C", size=4)

gC <- ggplotGrob(NSi.int)  # set up figure



#P and Si
xx = na.omit(subset(d.cr, select = c(P,Si,cr.area)))
#interaction.plot(xx$P, xx$Si, xx$cr.area*-1)

#summarize mean and SE by nutrient
sum <- group_by(xx, P, Si) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(avg = mean(cr.area, na.rm = TRUE)*-1, # na.rm = TRUE to remove missing values
            stdev=sd(cr.area, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(cr.area)), # of observations, excluding NAs. 
            se=stdev/sqrt(n))

#make plot
PSi.int = ggplot(sum, aes(x=P, y=avg, group = Si)) +
  geom_point() +
  geom_line(aes(group=Si, linetype = Si)) +
  geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=0.1) +
  theme_classic() +
  ylab(expression(CR~(mu*g~O[2]~cm^{-2}~h^{-1}))) +
  ylim(0,20) +
  theme(legend.justification=c(0.03,0.6),
        aspect.ratio = 0.75, #reduces white space on left and right of data
        legend.position=c(0.2,0.85),
        legend.text=element_text(size=10)) +
  annotate("text", x=0.5, y=19, label="D", size=4)

gD <- ggplotGrob(PSi.int)  # set up figure

#4 panel plot of CR and interaction plots

gA <- ggplotGrob(ringer.sum.cr)  # set up figure
gB <- ggplotGrob(NP.int)  # set up figure
gC <- ggplotGrob(NSi.int)  # set up figure
gD <- ggplotGrob(PSi.int)  # set up figure

#maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5], gC$widths[2:5], gD$widths[2:5])  # set up figure

#gA$widths[2:5] <- as.list(maxWidth)  # set up figure
#gB$widths[2:5] <- as.list(maxWidth)  # set up figure
#gC$widths[2:5] <- as.list(maxWidth)
#gD$widths[2:5] <- as.list(maxWidth)

#grid.arrange(gA, gB, gC, gD, ncol=2, nrow=2)

tiff(filename = 'xx.tiff', #open plotting device
     width = 6.5,
     height = 6.0,
     units = "in",
     res = 1200,
     compression = "lzw")
grid.arrange(gA, gB, gC, gD, nrow=2, ncol=2)  # push plot to device
dev.off()  # close device

