library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

#data (created in "all sites" by combining NDS files and water chem file)
nds_chem<-read.csv("NDS_chem_all.csv")
nds_chem$river_mile<-ifelse(nds_chem$stream=="ahtanum", 106, nds_chem$river_mile)
nds_chem$stream<-as.factor(nds_chem$stream)
nds_chem$season<-as.factor(nds_chem$season)
nds_chem$type<-as.factor(nds_chem$type)
nds_chem$Stream<-recode(nds_chem$stream, 'ahtanum'='Ahtanum', 'century'='Century Landing', 'kiona'='Kiona', 'mabton'='Mabton',
                                           'reecer'= 'Reecer', 'ringer'='Ringer', 'roza'='Roza', 'satus'='Satus', 
                                           'toppenish'='Toppenish', 'wenas'='Wenas', 'cleelum'='Cle Elum')

nds_s<-subset(nds_chem, top=="sponge")
nds_s <- subset(nds_s, !(is.na(nds_s$cr.nrr)))

nds_g<-subset(nds_chem, top=="glass")
nds_g <- subset(nds_g, !(is.na(nds_g$chla.nrr)))

nds_s$Stream <- factor(nds_s$Stream, levels = c("Cle Elum", "Reecer", "Ringer", "Roza", "Wenas", "Century Landing",
                                                "Ahtanum", "Toppenish", "Satus", "Mabton", "Kiona"))

nds_g$Stream <- factor(nds_g$Stream, levels = c("Cle Elum", "Reecer", "Ringer", "Roza", "Wenas", "Century Landing",
                                                "Ahtanum", "Toppenish", "Satus", "Mabton", "Kiona"))

nds_g$season<-factor(nds_g$season, levels=c("summer", "fall"))

nds_s$season<-factor(nds_s$season, levels=c("summer", "fall"))

CR_C<-ggplot(subset(nds_s,nutrient=="control"), aes(x=Stream, y=abs(cr.area)))+geom_boxplot(aes(fill=factor(season)))+
  theme_bw(base_size = 12)+ theme(axis.title.x = element_blank(), axis.text.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank())+
  scale_fill_manual(values=c("plum3","goldenrod2"))+labs(y="Respiration (ug O2/cm2/h)")+
  theme(legend.position = "none")+theme( axis.text.x = element_text(angle = 35, vjust=0.65))

Chla_C<-ggplot(subset(nds_g,nutrient=="control"), aes(x=Stream, y=chla))+geom_boxplot(aes(fill=factor(season)))+
  theme_bw(base_size=12)+theme(legend.position = "none")+theme( axis.text.x = element_text(angle = 30, vjust=0.75), axis.title.x = element_blank())+
  scale_fill_manual(values=c("plum3","goldenrod2"))+labs(y="Chlorophyll-a (ug/cm2)")

grid.arrange(CR_C, Chla_C, ncol=1, heights=unit(c(2.5,3.2), c("in","in")))

ddply(subset(nds_s,nutrient=="control"& season=="fall"), "Stream", summarise, cr = abs(mean(cr.area)), 
      se = (sd(abs(cr.area))/sqrt(sum(!is.na(cr.area)))))

ddply(subset(nds_s,nutrient=="control"&season=="summer"), "Stream", summarise, cr = abs(mean(cr.area)), 
      se = (sd(abs(cr.area))/sqrt(sum(!is.na(cr.area)))))

  nds_g$Stream <- factor(nds_g$Stream, levels = c("Cle Elum", "Reecer", "Ringer", "Roza", "Wenas", "Century Landing",
                                                "Ahtanum", "Toppenish", "Satus", "Mabton", "Kiona"))

ddply(subset(nds_g,nutrient=="control"), c("Stream","season"), summarise, chlor = mean(chla), 
      se = (sd(chla))/sqrt(sum(!is.na(chla))))

#NRR figures
#summaries
sum_s<-ddply(nds_s, c("stream", "nutrient", "season", "river_mile", "top", "type", "Si.mgL", "DOC.mgL", "DIN.mgNL", "oP.mgPL"), summarise, cr=mean(cr.area, na.rm=T), 
             cr_nrr=mean(cr.nrr, na.rm=T), se_cr=(sd(cr.area, na.rm=T)/sqrt(sum(!is.na(cr.area)))),
             se_cr_nrr=(sd(cr.nrr, na.rm=T)/sqrt(sum(!is.na(cr.nrr)))))
sum_s$nutrient<-as.factor(sum_s$nutrient)

sum_g<-ddply(nds_g, c("stream", "nutrient", "season", "river_mile", "top","type", "Si.mgL", "DOC.mgL", "DIN.mgNL", "oP.mgPL"), summarise, gpp=mean(gpp.area, na.rm=T),
             chl_a=mean(chla, na.rm=T), chl_a.nrr=mean(chla.nrr, na.rm=T), gpp.nrr=mean(gpp.nrr, na.rm=T), 
             se_gpp=(sd(gpp.area, na.rm=T)/sqrt(sum(!is.na(gpp.area)))), 
             se_chla=(sd(chla, na.rm=T)/sqrt(sum(!is.na(chla)))), 
             se_chla_nrr=(sd(chla.nrr, na.rm=T)/sqrt(sum(!is.na(chla.nrr)))))
sum_g$nutrient<-as.factor(sum_g$nutrient)

#Figures with all nutrient treatments
sum_s$nutrient<-factor(sum_s$nutrient, levels=c("control", "N", "P", "NP","NSi","PSi", "Si","NPSi"))

ggplot(subset(sum_s, !(nutrient=="control")),aes(x=Si.mgL, y=cr_nrr))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_bw()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=cr_nrr-se_cr_nrr, ymax=cr_nrr+se_cr_nrr), width=4)+
  scale_color_manual(values=c("orchid3","goldenrod2"))+geom_hline(yintercept = 1)+theme(legend.position = "none")+
  labs(x="Silica (mg/L)", y="NRR, Respiration")+theme(strip.background =element_rect(fill="white"), strip.text=element_text(hjust=0))

sum_g$nutrient<-factor(sum_g$nutrient, levels=c("control", "N", "P", "NP","NSi","PSi", "Si","NPSi"))

ggplot(subset(sum_g, !(nutrient=="control")),aes(x=river_mile, y=log(chl_a.nrr+1)))+geom_point(aes(color=factor(season), shape=factor(type)), size=3)+
  theme_bw()+  facet_wrap(~nutrient)+geom_errorbar(aes(ymin=log(chl_a.nrr+1)-log(se_chla_nrr+1), ymax=log(chl_a.nrr+1)+log(se_chla_nrr+1)), width=6)+
  scale_color_manual(values=c("orchid3","goldenrod2"))+geom_hline(yintercept = log(2))+theme(legend.position = "none")+
  labs(x="River Mile", y="Ln (NRR, Chlorophyll-a)")+theme(strip.background =element_rect(fill="white"), strip.text=element_text(hjust=0))

#attempts to make figures showing ME models
#CR NRR - separate by type to show different relationship between river mile and type
N.mains<-function (x){(0.5401840-0.06665818+ (0.0016244156*1.0024225*x))*1.1097521}
NP.mains<-function (x){(0.5401840-0.05226612+(0.0012694891*1.0024225*x))*1.1097521}
NPSi.mains<-function (x){(0.5401840-0.10001441+(0.0023269448*1.0024225*x))*1.1097521}
NSi.mains<-function (x){(0.5401840+0.02581716 -(0.0015291970*1.0024225*x))*1.1097521}
P.mains<-function (x){(0.5401840+0.02678770 -(0.0006280988*1.0024225*x))*1.1097521}
PSi.mains<-function (x){(0.5401840+0.04495817 -(0.0005058603*1.0024225*x))*1.1097521}
Si.mains<-function (x){(0.5401840+0.07073013 -(0.0021453599*1.0024225*x))*1.1097521}

N.mainf<-function (x){(0.5401840-0.06665818+ (0.0016244156*1.0024225*x))}
NP.mainf<-function (x){(0.5401840-0.05226612+(0.0012694891*1.0024225*x))}
NPSi.mainf<-function (x){(0.5401840-0.10001441+(0.0023269448*1.0024225*x))}
NSi.mainf<-function (x){(0.5401840+0.02581716 -(0.0015291970*1.0024225*x))}
P.mainf<-function (x){(0.5401840+0.02678770 -(0.0006280988*1.0024225*x))}
PSi.mainf<-function (x){(0.5401840+0.04495817 -(0.0005058603*1.0024225*x))}
Si.mainf<-function (x){(0.5401840+0.07073013 -(0.0021453599*1.0024225*x))}

N.tribs<-function (x){(0.5401840+0.06665818+ 0.0016244156*x)*1.1097521*1.2673480}
NP.tribs<-function (x){(0.5401840+0.05226612+0.0012694891*x)*1.1097521*1.2673480}
NPSi.tribs<-function (x){(0.5401840+0.10001441+0.0023269448*x)*1.1097521*1.2673480}
NSi.tribs<-function (x){(0.5401840+0.02581716 -0.0015291970*x)*1.1097521*1.2673480}
P.tribs<-function (x){(0.5401840+0.02678770 -0.0006280988*x)*1.1097521*1.2673480}
PSi.tribs<-function (x){(0.5401840+0.04495817 -0.0005058603*x)*1.1097521*1.2673480}
Si.tribs<-function (x){(0.5401840+0.07073013 -0.0021453599*x)*1.1097521*1.2673480}

N.tribf<-function (x){(0.5401840+0.06665818+ 0.0016244156*x)*1.2673480}
NP.tribf<-function (x){(0.5401840-0.05226612+0.0012694891*x)*1.2673480}
NPSi.tribf<-function (x){(0.5401840-0.10001441+0.0023269448*x)*1.2673480}
NSi.tribf<-function (x){(0.5401840+0.02581716 -0.0015291970*x)*1.2673480}
P.tribf<-function (x){(0.5401840+0.02678770 -0.0006280988*x)*1.2673480}
PSi.tribf<-function (x){(0.5401840+0.04495817 -0.0005058603*x)*1.2673480}
Si.tribf<-function (x){(0.5401840+0.07073013 -0.0021453599*x)*1.2673480}

#                       Value  Std.Error  DF   t-value  p-value
#(Intercept)          0.5401840 0.03465829 785 15.585997  0.0000
#seasonfall          -0.1097521 0.01345564 785  8.156582  0.0000
#river_mile           0.0016288 0.00059886 785  2.719846  0.0067
#typetrib             0.2673480 0.04474965 785  5.974304  0.0000
#river_mile:typetrib -0.0024225 0.00038652 785 -6.267546  0.0000

#  (Intercept)    river_mile
#control  0.05064554 -0.0004123336
#N       -0.06665818  0.0016244156
#NP      -0.05226612  0.0012694891
#NPSi    -0.10001441  0.0023269448
#NSi      0.02581716 -0.0015291970
#P        0.02678770 -0.0006280988
#PSi      0.04495817 -0.0005058603
#Si       0.07073013 -0.0021453599

sum_s_main_s<-subset(sum_s,type=="mainstem"&season=="summer")
sum_s_main_f<-subset(sum_s_main, type=="mainstem" & season =="fall")
sum_s_trib_s<-subset(sum_s,type=="trib"&season=="summer")
sum_s_trib_f<-subset(sum_s,type=="trib"&season=="fall")

sum_s_main_s$nutrient<-factor(sum_s_main_s$nutrient, levels=c("control", "N", "P", "Si", "NP","NSi","PSi", "NPSi"))
sum_s_main_f$nutrient<-factor(sum_s_main_f$nutrient, levels=c("control", "N", "P", "Si", "NP","NSi","PSi","NPSi"))
sum_s_trib_s$nutrient<-factor(sum_s_trib_s$nutrient, levels=c("control", "N", "P", "Si", "NP","NSi","PSi","NPSi"))
sum_s_trib_f$nutrient<-factor(sum_s_trib_f$nutrient, levels=c("control", "N", "P", "Si", "NP","NSi","PSi","NPSi"))

A<-ggplot(subset(sum_s_main_s, !(nutrient=="control")), aes(x=river_mile, y=log(cr_nrr+1)))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=log(cr_nrr+1)-log(se_cr_nrr+1), ymax=log(cr_nrr+1)+log(se_cr_nrr+1), width=2.5))+
  scale_color_manual(values=c("red","goldenrod2", "deepskyblue3", "orange", "orchid", "turquoise4", "slategray4"))+
  theme_bw(base_size = 14)+ylab(expression(paste('Ln (NRR'[CR],')'))) +xlab("River Mile")+
  ggtitle("A. Mainstem, Summer")+theme(plot.title = element_text(hjust = 0.02, vjust=-8))+
  theme(legend.title=element_blank(),legend.position="none")+
  stat_function(fun=N.mains, color="red")+
  stat_function(fun=NP.mains, color="orange")+
  stat_function(fun=NPSi.mains, color="slategray4")+
  stat_function(fun=P.mains, color="goldenrod2")+
  stat_function(fun=PSi.mains, color="turquoise4")+
  stat_function(fun=NSi.mains, color="orchid")+
  stat_function(fun=Si.mains, color="deepskyblue3")

B<-ggplot(subset(sum_s_main_f, !(nutrient=="control")), aes(x=river_mile, y=log(cr_nrr+1)))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=log(cr_nrr+1)-log(se_cr_nrr+1), ymax=log(cr_nrr+1)+log(se_cr_nrr+1), width=2.5))+
  scale_color_manual(values=c("red","goldenrod2", "deepskyblue3", "orange", "orchid", "turquoise4", "slategray4"))+
  theme_bw(base_size = 14)+ylab(expression(paste('Ln (NRR'[CR],')')))+xlab("River Mile") +
   ggtitle("B. Mainstem, Fall")+theme(plot.title = element_text(hjust = 0.02, vjust=-8))+
  theme(legend.title=element_blank(),legend.position="none")+
  stat_function(fun=N.mainf, color="red")+
  stat_function(fun=NP.mainf, color="orange")+
  stat_function(fun=NPSi.mainf, color="slategray4")+
  stat_function(fun=P.mainf, color="goldenrod2")+
  stat_function(fun=PSi.mainf, color="turquoise4")+
  stat_function(fun=NSi.mainf, color="orchid")+
  stat_function(fun=Si.mainf, color="deepskyblue3")

C<-ggplot(subset(sum_s_trib_s, !(nutrient=="control")), aes(x=river_mile, y=log(cr_nrr+1)))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=log(cr_nrr+1)-log(se_cr_nrr+1), ymax=log(cr_nrr+1)+log(se_cr_nrr+1), width=2.5))+
  scale_color_manual(values=c("red","goldenrod2", "deepskyblue3", "orange", "orchid", "turquoise4", "slategray4"))+
  theme_bw(base_size=14)+ylab(expression(paste('Ln (NRR'[CR],')'))) +xlab("River Mile")+
  ggtitle("C. Tributaries, Summer")+theme(plot.title = element_text(hjust = 0.02, vjust=-8))+
  theme(legend.title=element_blank(),legend.position="none")+
  stat_function(fun=N.tribs, color="red")+
  stat_function(fun=NP.tribs, color="orange")+
  stat_function(fun=NPSi.tribs, color="slategray4")+
  stat_function(fun=P.tribs, color="goldenrod2")+
  stat_function(fun=PSi.tribs, color="turquoise4")+
  stat_function(fun=NSi.tribs, color="orchid")+
  stat_function(fun=Si.tribs, color="deepskyblue3")

D<-ggplot(subset(sum_s_trib_s, !(nutrient=="control")), aes(x=river_mile, y=log(cr_nrr+1)))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=log(cr_nrr+1)-log(se_cr_nrr+1), ymax=log(cr_nrr+1)+log(se_cr_nrr+1), width=2.5))+
  scale_color_manual(values=c("red","goldenrod2", "deepskyblue3", "orange", "orchid", "turquoise4", "slategray4"))+
  theme_bw(base_size = 14)+ ylab(expression(paste('Ln (NRR'[CR],')'))) +xlab("River Mile")+
  ggtitle("D. Tributaries, Fall")+theme(plot.title = element_text(hjust = 0.02, vjust=-8))+
  theme(legend.title=element_blank(),legend.position="none")+
  stat_function(fun=N.tribf, color="red")+
  stat_function(fun=NP.tribf, color="orange")+
  stat_function(fun=NPSi.tribf, color="slategray4")+
  stat_function(fun=P.tribf, color="goldenrod2")+
  stat_function(fun=PSi.tribf, color="turquoise4")+
  stat_function(fun=NSi.tribf, color="orchid")+
  stat_function(fun=Si.tribf, color="deepskyblue3")
               
library(gridExtra)
grid.arrange(A, B, C, D, ncol=2)

#chla NRR, model with river mile, season, and type only
sum_g$nutrient<-factor(sum_g$nutrient, levels=c("control", "N", "P", "NP","NSi","PSi", "Si","NPSi"))

#equations
#Fixed effects: chla.nrr ~ river_mile + season + river_mile * type 
#                     Value  Std.Error    DF   t-value  p-value
#(Intercept)          0.7526378 0.06627973 794 11.355474  0.0000
#river_mile          -0.0031057 0.00042297 794 -7.342575  0.0000
#seasonsummer         0.4611727 0.03273352 794 14.088696  0.0000
#typetrib            -0.4392558 0.07392803 794 -5.941668  0.0000
#river_mile:typetrib  0.0015501 0.00050217 794  3.086829  0.0021

#       (Intercept)
#control  0.06433244
#N        0.07263615
#NP      -0.05449180
#NPSi     0.01110275
#NSi     -0.04395794
#P        0.02348266
#PSi     -0.04629925
#Si      -0.02680503

N.tribf<-function (x){(0.7526378+0.07263615-(0.0031057*0.0015501)*x)}
NP.tribf<-function(x){0.7526378-0.05449180-(0.0031057*0.0015501)*x}
NPSi.tribf<-function(x){0.7526378+0.01110275-(0.0031057*0.0015501)*x}
NSi.tribf<-function(x){0.7526378-0.04395794-(0.0031057*0.0015501)*x}
P.tribf<-function(x){0.7526378+0.02348266-(0.0031057*0.0015501)*x}
PSi.tribf<-function(x){0.7526378-0.04629925-(0.0031057*0.0015501)*x}
Si.tribf<-function(x){0.7526378-0.02680503-(0.0031057*0.0015501)*x}

N.mains<-function (x){(0.7526378+0.07263615-0.0031057*x)*1.4611727*1.4392558}
NP.mains<-function(x){(0.7526378-0.05449180-0.0031057*x)*1.4611727*1.4392558}
NPSi.mains<-function(x){(0.7526378+0.01110275-0.0031057*x)*1.4611727*1.4392558}
NSi.mains<-function(x){(0.7526378-0.04395794-0.0031057*x)*1.4611727*1.4392558}
P.mains<-function(x){(0.7526378+0.02348266-0.0031057*x)*1.4611727*1.4392558}
PSi.mains<-function(x){(0.7526378-0.04629925-0.0031057*x)*1.4611727*1.4392558}
Si.mains<-function(x){(0.7526378-0.02680503-0.0031057*x)*1.4611727*1.4392558}

N.tribs<-function (x){(0.7526378+0.07263615-(0.0031057*0.0015501)*x)*1.4611727}
NP.tribs<-function(x){(0.7526378-0.05449180-(0.0031057*0.0015501)*x)*1.4611727}
NPSi.tribs<-function(x){(0.7526378+0.01110275-(0.0031057*0.0015501)*x)*1.4611727}
NSi.tribs<-function(x){(0.7526378-0.04395794-(0.0031057*0.0015501)*x)*1.4611727}
P.tribs<-function(x){(0.7526378+0.02348266-(0.0031057*0.0015501)*x)*1.4611727}
PSi.tribs<-function(x){(0.7526378-0.04629925-(0.0031057*0.0015501)*x)*1.4611727}
Si.tribs<-function(x){(0.7526378-0.02680503-(0.0031057*0.0015501)*x)*1.4611727}

N.mainf<-function (x){(0.7526378+0.07263615-0.0031057*x)*1.4392558}
NP.mainf<-function(x){(0.7526378-0.05449180-0.0031057*x)*1.4392558}
NPSi.mainf<-function(x){(0.7526378+0.01110275-0.0031057*x)*1.4392558}
NSi.mainf<-function(x){(0.7526378-0.04395794-0.0031057*x)*1.4392558}
P.mainf<-function(x){(0.7526378+0.02348266-0.0031057*x)*1.4392558}
PSi.mainf<-function(x){(0.7526378-0.04629925-0.0031057*x)*1.4392558}
Si.mainf<-function(x){(0.7526378-0.02680503-0.0031057*x)*1.4392558}

sum_g_main_s<-subset(sum_g,type=="mainstem"&season=="summer")
sum_g_main_f<-subset(sum_g, type=="mainstem" & season =="fall")
sum_g_trib_s<-subset(sum_g,type=="trib"&season=="summer")
sum_g_trib_f<-subset(sum_g,type=="trib"&season=="fall")

sum_g_main_s$nutrient<-factor(sum_g_main_s$nutrient, levels=c("control", "N", "P", "Si", "NP","NSi","PSi", "NPSi"))
sum_g_main_f$nutrient<-factor(sum_g_main_f$nutrient, levels=c("control", "N", "P", "Si", "NP","NSi","PSi","NPSi"))
sum_g_trib_s$nutrient<-factor(sum_g_trib_s$nutrient, levels=c("control", "N", "P", "Si", "NP","NSi","PSi","NPSi"))
sum_g_trib_f$nutrient<-factor(sum_g_trib_f$nutrient, levels=c("control", "N", "P", "Si", "NP","NSi","PSi","NPSi"))

A<-ggplot(subset(sum_g_main_s, !(nutrient=="control")), aes(x=river_mile, y=chl_a.nrr))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr, width=2.5))+
  scale_color_manual(values=c("red","goldenrod2", "deepskyblue3", "orange", "orchid", "turquoise4", "slategray4"))+
  theme_bw(base_size = 14)+ylab(expression(paste('NRR'[chla],')'))) +xlab("River Mile")+
  ggtitle("A. Mainstem, Summer")+theme(plot.title = element_text(hjust = 0.02, vjust=-8))+
  theme(legend.title=element_blank(),legend.position="none")+
  stat_function(fun=N.mains, color="red")+
  stat_function(fun=NP.mains, color="orange")+
  stat_function(fun=NPSi.mains, color="slategray4")+
  stat_function(fun=P.mains, color="goldenrod2")+
  stat_function(fun=PSi.mains, color="turquoise4")+
  stat_function(fun=NSi.mains, color="orchid")+
  stat_function(fun=Si.mains, color="deepskyblue3")

B<-ggplot(subset(sum_g_main_f, !(nutrient=="control")), aes(x=river_mile, y=chl_a.nrr))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr, width=2.5))+
  scale_color_manual(values=c("red","goldenrod2", "deepskyblue3", "orange", "orchid", "turquoise4", "slategray4"))+
  theme_bw(base_size = 14)+ylab(expression(paste('NRR'[chla],')')))+xlab("River Mile") +
  ggtitle("B. Mainstem, Fall")+theme(plot.title = element_text(hjust = 0.02, vjust=-8))+
  theme(legend.title=element_blank(),legend.position="none")+
  stat_function(fun=N.mainf, color="red")+
  stat_function(fun=NP.mainf, color="orange")+
  stat_function(fun=NPSi.mainf, color="slategray4")+
  stat_function(fun=P.mainf, color="goldenrod2")+
  stat_function(fun=PSi.mainf, color="turquoise4")+
  stat_function(fun=NSi.mainf, color="orchid")+
  stat_function(fun=Si.mainf, color="deepskyblue3")

C<-ggplot(subset(sum_g_trib_s, !(nutrient=="control")), aes(x=river_mile, y=chl_a.nrr))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=2.5)+
  scale_color_manual(values=c("red","goldenrod2", "deepskyblue3", "orange", "orchid", "turquoise4", "slategray4"))+
  theme_bw(base_size=14)+ylab(expression(paste('Ln (NRR'[chla],')'))) +xlab("River Mile")+
  ggtitle("C. Tributaries, Summer")+theme(plot.title = element_text(hjust = 0.02, vjust=-8))+
  theme(legend.title=element_blank(),legend.position="none")+
  stat_function(fun=N.tribs, color="red")+
  stat_function(fun=NP.tribs, color="orange")+
  stat_function(fun=NPSi.tribs, color="slategray4")+
  stat_function(fun=P.tribs, color="goldenrod2")+
  stat_function(fun=PSi.tribs, color="turquoise4")+
  stat_function(fun=NSi.tribs, color="orchid")+
  stat_function(fun=Si.tribs, color="deepskyblue3")

D<-ggplot(subset(sum_g_trib_s, !(nutrient=="control")), aes(x=river_mile, y=chl_a.nrr))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=2.5)+
  scale_color_manual(values=c("red","goldenrod2", "deepskyblue3", "orange", "orchid", "turquoise4", "slategray4"))+
  theme_bw(base_size = 14)+ ylab(expression(paste('NRR'[chla],')'))) +xlab("River Mile")+
  ggtitle("D. Tributaries, Fall")+theme(plot.title = element_text(hjust = 0.02, vjust=-8))+
  theme(legend.title=element_blank(),legend.position="none")+
  stat_function(fun=N.tribf, color="red")+
  stat_function(fun=NP.tribf, color="orange")+
  stat_function(fun=NPSi.tribf, color="slategray4")+
  stat_function(fun=P.tribf, color="goldenrod2")+
  stat_function(fun=PSi.tribf, color="turquoise4")+
  stat_function(fun=NSi.tribf, color="orchid")+
  stat_function(fun=Si.tribf, color="deepskyblue3")

grid.arrange(A, B, C, D, ncol=2)

nds_ssummer<-subset(nds_s, season=="summer")
nds_sfall<-subset(nds_s, season=="fall")

newdat1<-expand.grid(type=unique(nds_ssummer$type),
                     season=unique(nds_ssummer$season),
                     river_mile=c(min(nds_ssummer$river_mile),
                                  max(nds_ssummer$river_mile)))
newdat2<-expand.grid(type=unique(nds_sfall$type),
                     season=unique(nds_sfall$season),
                     river_mile=c(min(nds_sfall$river_mile),
                                  max(nds_sfall$river_mile)))

ggplot(nds_s, aes(x=river_mile, y=log(cr.nrr+1)))+
  geom_point(aes(shape=factor(type)))+
  geom_line(aes(y=predict(Mf), group=nutrient))+
  geom_line(data=newdat, aes(y=predict(Mf, level=0,newdata=newdat), color=factor(season)))+
  annotate()
theme_bw()

#example:
ggplot(Orthodont, aes(x=age, y=distance, colour=Sex)) +
  geom_point(size=3) +
  geom_line(aes(y=predict(fm2), group=Subject, size="Subjects")) +
  geom_line(data=newdat, aes(y=predict(fm2, level=0, newdata=newdat), size="Population")) +
  scale_size_manual(name="Predictions", values=c("Subjects"=0.5, "Population"=3)) +
  theme_bw(base_size=22) 
      