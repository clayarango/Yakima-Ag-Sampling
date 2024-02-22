library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

#data (created in "all sites" by combining NDS files and water chem file)
nds_chem<-read.csv("NDS_chem_all.csv")
nds_chem$river_mile<-ifelse(nds_chem$stream=="ahtanum", 106, nds_chem$river_mile)
nds_chem$river_km<-nds_chem$river_mile*1.609
nds_chem$stream<-as.factor(nds_chem$stream)
nds_chem$season<-as.factor(nds_chem$season)
nds_chem$type<-as.factor(nds_chem$type)
nds_chem$Stream<-recode(nds_chem$stream, 'ahtanum'='Ahtanum', 'century'='Century Landing', 'kiona'='Kiona', 'mabton'='Mabton',
                                           'reecer'= 'Reecer', 'ringer'='Ringer', 'roza'='Roza', 'satus'='Satus', 
                                           'toppenish'='Toppenish', 'wenas'='Wenas', 'cleelum'='Cle Elum')
nds_chem$Si.mgL_Si<-nds_chem$Si.mgL*28.0855/60.08 #needed because data from Seal are expressed as mg SiO2, not as mg Si

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

#NRR figures
#summaries
sum_s<-ddply(nds_s, c("stream", "nutrient", "season", "river_km", "top", "type", "Si.mgL_Si", "DOC.mgL", "DIN.mgNL", "oP.mgPL"), summarise, cr=mean(cr.area, na.rm=T), 
             cr_nrr=mean(cr.nrr, na.rm=T), se_cr=(sd(cr.area, na.rm=T)/sqrt(sum(!is.na(cr.area)))),
             se_cr_nrr=(sd(cr.nrr, na.rm=T)/sqrt(sum(!is.na(cr.nrr)))))
sum_s$nutrient<-as.factor(sum_s$nutrient)

sum_g<-ddply(nds_g, c("stream", "nutrient", "season", "river_km", "top","type", "Si.mgL_Si", "DOC.mgL", "DIN.mgNL", "oP.mgPL"), summarise, gpp=mean(gpp.area, na.rm=T),
             chl_a=mean(chla, na.rm=T), chl_a.nrr=mean(chla.nrr, na.rm=T), gpp.nrr=mean(gpp.nrr, na.rm=T), 
             se_gpp=(sd(gpp.area, na.rm=T)/sqrt(sum(!is.na(gpp.area)))), 
             se_chla=(sd(chla, na.rm=T)/sqrt(sum(!is.na(chla)))), 
             se_chla_nrr=(sd(chla.nrr, na.rm=T)/sqrt(sum(!is.na(chla.nrr)))))
sum_g$nutrient<-as.factor(sum_g$nutrient)

#figures showing ME models - updated 2/20/2024 because did not match output - unsure why
#CR NRR - separate by type to show different relationship between river mile and type
N.mains<-function (x){(0.4304319-0.06665833+ (0.0010123+0.0010095816)*x)}
NP.mains<-function (x){(0.4304319-0.05226625+(0.0010123+0.0007889932)*x)}
NPSi.mains<-function (x){(0.4304319-0.10001467+(0.0010123+0.0014462067)*x)}
NSi.mains<-function (x){(0.4304319+0.02581708 +(0.0010123-0.0009504016)*x)}
P.mains<-function (x){(0.4304319+0.02678777 +(0.0010123-0.0003903662)*x)}
PSi.mains<-function (x){(0.4304319+0.04495836 +(0.0010123-0.0003143952)*x)}
Si.mains<-function (x){(0.4304319+0.07073023 +(0.0010123-0.00133335)*x)}
mains<-function (x){0.4304319+0.0010123*x}

N.mainf<-function (x){(0.4304319-0.06665833+ (0.0010123+0.0010095816)*x)-0.1097521}
NP.mainf<-function (x){(0.4304319-0.05226625+(0.0010123+0.0007889932)*x)-0.1097521}
NPSi.mainf<-function (x){(0.4304319-0.10001467+(0.0010123+0.0014462067)*x)-0.1097521}
NSi.mainf<-function (x){(0.4304319+0.02581708 +(0.0010123-0.0009504016)*x)-0.1097521}
P.mainf<-function (x){(0.4304319+0.02678777 +(0.0010123-0.0003903662)*x)-0.1097521}
PSi.mainf<-function (x){(0.4304319+0.04495836 +(0.0010123-0.0003143952)*x)-0.1097521}
Si.mainf<-function (x){(0.4304319+0.07073023 +(0.0010123-0.00133335)*x)-0.1097521}
mainf<-function(x){(0.4304319+0.0010123*x)-0.1097521}

N.tribs<-function (x){(0.4304319-0.06665833+ (0.0010123+0.0010095816-0.0015056)*x)+0.2673480}
NP.tribs<-function (x){(0.4304319-0.05226625+(0.0010123+0.0007889932-0.0015056)*x)+0.2673480}
NPSi.tribs<-function (x){(0.4304319-0.10001450+(0.0010123+0.0014462060-0.0015056)*x)+0.2673480}
NSi.tribs<-function (x){(0.4304319+0.02581708 +(0.0010123-0.0009504016-0.0015056)*x)+0.2673480}
P.tribs<-function (x){(0.4304319+0.02678777 +(0.0010123-0.0003903662-0.0015056)*x)+0.2673480}
PSi.tribs<-function (x){(0.4304319+0.04495836 +(0.0010123-0.0003143952-0.0015056)*x)+0.2673480}
Si.tribs<-function (x){(0.4304319+0.07073023 +(0.0010123-0.00133335-0.0015056)*x)+0.2673480}
tribs<-function(x){(0.4304319+((0.0010123-0.0015056)*x))+0.2673480}

N.tribf<-function (x){(0.4304319-0.06665833+ (0.0010123+0.0010095816-0.0015056)*x)+0.2673480-0.1097521}
NP.tribf<-function (x){(0.4304319-0.05226625+(0.0010123+0.0007889932-0.0015056)*x)+0.2673480-0.1097521}
NPSi.tribf<-function (x){(0.4304319-0.10001450+(0.0010123+0.0014462060-0.0015056)*x)+0.2673480-0.1097521}
NSi.tribf<-function (x){(0.4304319+0.02581708 +(0.0010123-0.0009504016-0.0015056)*x)+0.2673480-0.1097521}
P.tribf<-function (x){(0.4304319+0.02678777 +(0.0010123-0.0003903662-0.0015056)*x)+0.2673480-0.1097521}
PSi.tribf<-function (x){(0.4304319+0.04495836 +(0.0010123-0.0003143952-0.0015056)*x)+0.2673480-0.1097521}
Si.tribf<-function (x){(0.4304319+0.07073023 +(0.0010123-0.00133335-0.0015056)*x)+0.2673480-0.1097521}
tribf<-function (x){(0.4304319+((0.0010123-0.0015056)*x))+0.2673480-0.1097521}

ave_all<-function(x){(0.4304319+0.0010123*x)}

#Fixed effects: log(cr.nrr + 1) ~ season + river_mile * type
#                       Value  Std.Error  DF   t-value  p-value
#(Intercept)          0.4304319 0.03463473 785 	12.427753  	0.0000
#seasonfall          -0.1097521 0.01345564 785 -8.156582  0.0000
#river_km             0.0010123 0.00037220 785  2.719848  0.0067
#typetrib             0.2673480 0.04474965 785  5.974304  0.0000
#river_km:typetrib   -0.0015056 0.00024023 785 -6.267546  0.0000

#         (Intercept)      river_km
#control  0.05064563 -0.0002562674
#N       -0.06665823  0.0010095811
#NP      -0.05226617  0.0007889928
#NPSi    -0.10001450  0.0014462060
#NSi      0.02581713 -0.0009504019
#P        0.02678773 -0.0003903661
#PSi      0.04495824 -0.0003143946
#Si       0.07073017 -0.0013333500

sum_s_main_s<-subset(sum_s,type=="mainstem"&season=="summer")
sum_s_main_f<-subset(sum_s, type=="mainstem" & season =="fall")
sum_s_trib_s<-subset(sum_s,type=="trib"&season=="summer")
sum_s_trib_f<-subset(sum_s,type=="trib"&season=="fall")

sum_s_main_s$nutrient<-factor(sum_s_main_s$nutrient, levels=c("control", "N", "P", "Si", "NP","NSi","PSi", "NPSi"))
sum_s_main_f$nutrient<-factor(sum_s_main_f$nutrient, levels=c("control", "N", "P", "Si", "NP","NSi","PSi","NPSi"))
sum_s_trib_s$nutrient<-factor(sum_s_trib_s$nutrient, levels=c("control", "N", "P", "Si", "NP","NSi","PSi","NPSi"))
sum_s_trib_f$nutrient<-factor(sum_s_trib_f$nutrient, levels=c("control", "N", "P", "Si", "NP","NSi","PSi","NPSi"))

A<-ggplot(subset(sum_s_main_s, !(nutrient=="control")), aes(x=river_km, y=log(cr_nrr+1)))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=log(cr_nrr+1)-log(se_cr_nrr+1), ymax=log(cr_nrr+1)+log(se_cr_nrr+1), width=2.5))+
  scale_color_manual(values=c("red","gold", "steelblue3", "darkorange2", "darkorchid", "green3", "slategray4"))+
  theme_classic(base_size = 12)+ylab(expression(paste('Ln(NRR ' [ R],' + 1)'))) + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  ggtitle("A Mainstem, Summer")+theme(plot.title = element_text(hjust = 0.0, vjust=-1))+
  scale_x_continuous(limits=c(0,290))+
  theme(legend.title=element_blank(),legend.position="none")+
   stat_function(fun=N.mains, color="red", lty="dotted", lwd=1)+
  stat_function(fun=NP.mains, color="darkorange2", lty="dashed",lwd=1)+
  stat_function(fun=NPSi.mains, color="slategray4", lty="solid",lwd=1)+
  stat_function(fun=P.mains, color="gold", lty="dotted",lwd=1)+
  stat_function(fun=PSi.mains, color="green3", lty="dashed",lwd=1)+
  stat_function(fun=NSi.mains, color="darkorchid", lty="dashed",lwd=1)+
  stat_function(fun=Si.mains, color="steelblue3", lty="dotted",lwd=1)+
  geom_hline(yintercept=log(2), lwd=1, lty="dashed")
stat_function(fun=mains, lwd=1)

B<-ggplot(subset(sum_s_main_f, !(nutrient=="control")), aes(x=river_km, y=log(cr_nrr+1)))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=log(cr_nrr+1)-log(se_cr_nrr+1), ymax=log(cr_nrr+1)+log(se_cr_nrr+1), width=2.5))+
  scale_color_manual(values=c("red","gold", "steelblue3", "darkorange2", "darkorchid", "green3", "slategray4"))+
  theme_classic(base_size = 12)+theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank())+
   ggtitle("B Mainstem, Autumn")+theme(plot.title = element_text(hjust = 0.0, vjust=-1))+
  scale_x_continuous(limits=c(0,290))+
  theme(legend.title=element_blank(),legend.position="none")+
    stat_function(fun=N.mainf, color="red", lty="dotted", lwd=1)+
  stat_function(fun=NP.mainf, color="darkorange2", lty="dashed",lwd=1)+
  stat_function(fun=NPSi.mainf, color="slategray4", lty="solid",lwd=1)+
  stat_function(fun=P.mainf, color="gold", lty="dotted",lwd=1)+
  stat_function(fun=PSi.mainf, color="green3", lty="dashed",lwd=1)+
  stat_function(fun=NSi.mainf, color="darkorchid", lty="dashed",lwd=1)+
  stat_function(fun=Si.mainf, color="steelblue3", lty="dotted",lwd=1)+
  geom_hline(yintercept=log(2), lwd=1, lty="dashed")
stat_function(fun=mainf, lwd=1)

C<-ggplot(subset(sum_s_trib_s, !(nutrient=="control")), aes(x=river_km, y=log(cr_nrr+1)))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=log(cr_nrr+1)-log(se_cr_nrr+1), ymax=log(cr_nrr+1)+log(se_cr_nrr+1), width=2.5))+
  scale_color_manual(values=c("red","gold", "steelblue3", "darkorange2", "darkorchid", "green3", "slategray4"))+
  theme_classic(base_size=12)+ylab(expression(paste('Ln(NRR ' [R],' + 1)'))) +xlab("River km")+
  ggtitle("C Tributaries, Summer")+theme(plot.title = element_text(hjust = 0.0, vjust=-1))+
  theme(legend.title=element_blank(),legend.position="none")+
  scale_x_continuous(limits=c(0,290))+
    stat_function(fun=N.tribs, color="red", lty="dotted", lwd=1)+
  stat_function(fun=NP.tribs, color="darkorange2", lty="dashed", lwd=1)+
  stat_function(fun=NPSi.tribs, color="slategray4", lty="solid", lwd=1)+
  stat_function(fun=P.tribs, color="gold", lty="dotted", lwd=1)+
  stat_function(fun=PSi.tribs, color="green3", lty="dashed", lwd=1)+
  stat_function(fun=NSi.tribs, color="darkorchid", lty="dashed", lwd=1)+
  stat_function(fun=Si.tribs, color="steelblue3", lty="dotted", lwd=1)+
  geom_hline(yintercept=log(2), lwd=1, lty="dashed")
stat_function(fun=tribs, lwd=1)

D<-ggplot(subset(sum_s_trib_f, !(nutrient=="control")), aes(x=river_km, y=log(cr_nrr+1)))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=log(cr_nrr+1)-log(se_cr_nrr+1), ymax=log(cr_nrr+1)+log(se_cr_nrr+1), width=2.5))+
  scale_color_manual(values=c("red","gold", "steelblue3", "darkorange2", "darkorchid", "green3", "slategray4"))+
  theme_classic(base_size = 12)+ theme(axis.title.y = element_blank())+xlab("River km")+
  ggtitle("D Tributaries, Autumn")+theme(plot.title = element_text(hjust = 0.0, vjust=-1))+
  scale_x_continuous(limits=c(0,290))+
  theme(legend.title=element_blank(),legend.position = "none")+
    stat_function(fun=N.tribf, color="red", lty="dotted", lwd=1)+
  stat_function(fun=NP.tribf, color="darkorange2", lty="dashed",lwd=1)+
  stat_function(fun=NPSi.tribf, color="slategray4", lty="solid",lwd=1)+
  stat_function(fun=P.tribf, color="gold", lty="dotted",lwd=1)+
  stat_function(fun=PSi.tribf, color="green3", lty="dashed",lwd=1)+
  stat_function(fun=NSi.tribf, color="darkorchid", lty="dashed",lwd=1)+
  stat_function(fun=Si.tribf, color="steelblue3", lty="dotted",lwd=1)+
  geom_hline(yintercept=log(2), lwd=1, lty= "dashed")

#legend.position=c(0.2, 0.3), 
#legend.background = element_rect(color=NA, fill=NA)
#legend.spacing.y = 0.2, "inch"
stat_function(fun=tribf, lwd=1)
               
library(gridExtra)



#chla NRR, model with river km, season, and type only
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

N.tribsL<-function (x){(0.7526378+0.07263615-(0.0031057*0.0015501)*log10(x))*1.4611727}
NP.tribsL<-function(x){(0.7526378-0.05449180-(0.0031057*0.0015501)*log10(x))*1.4611727}
NPSi.tribsL<-function(x){(0.7526378+0.01110275-(0.0031057*0.0015501)*log10(x))*1.4611727}
NSi.tribsL<-function(x){(0.7526378-0.04395794-(0.0031057*0.0015501)*log10(x))*1.4611727}
P.tribsL<-function(x){(0.7526378+0.02348266-(0.0031057*0.0015501)*log10(x))*1.4611727}
PSi.tribsL<-function(x){(0.7526378-0.04629925-(0.0031057*0.0015501)*log10(x))*1.4611727}
Si.tribsL<-function(x){(0.7526378-0.02680503-(0.0031057*0.0015501)*log10(x))*1.4611727}

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

A<-ggplot(subset(sum_g_main_s, !(nutrient=="control")), aes(x=river_km, y=chl_a.nrr))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr, width=2.5))+
  scale_color_manual(values=c("red","gold", "steelblue3", "darkorange2", "darkorchid", "green3", "slategray4"))+
  theme_classic(base_size = 13)+ylab(expression(paste('NRR '[Chla],''))) +theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
  ggtitle("A Mainstem, Summer")+theme(plot.title = element_text(hjust = 0, vjust=-1))+
  theme(legend.title=element_blank(),legend.position="none")+scale_x_continuous(limits=c(0,290))+
  scale_y_continuous(breaks=seq(0,7.5, by=1.5))

  +stat_function(fun=N.mains, color="red")+
  stat_function(fun=NP.mains, color="orange")+
  stat_function(fun=NPSi.mains, color="slategray4")+
  stat_function(fun=P.mains, color="goldenrod2")+
  stat_function(fun=PSi.mains, color="turquoise4")+
  stat_function(fun=NSi.mains, color="orchid")+
  stat_function(fun=Si.mains, color="deepskyblue3")

B<-ggplot(subset(sum_g_main_f, !(nutrient=="control")), aes(x=river_km, y=chl_a.nrr))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr, width=2.5))+
  scale_color_manual(values=c("red","gold", "steelblue3", "darkorange2", "darkorchid", "green3", "slategray4"))+
  theme_classic(base_size = 13)+ ylab(expression(paste('NRR '[Chla],''))) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  ggtitle("B Mainstem, Autumn")+theme(plot.title = element_text(hjust = 0.0, vjust=-1))+
  theme(legend.title=element_blank(),legend.position="none")+
  scale_y_continuous(limits = c(0,7.5), breaks=seq(0,7.5, by=1.5))+scale_x_continuous(limits=c(0,290))

  +stat_function(fun=(N.mainf), color="red")+
  stat_function(fun=(NP.mainf), color="orange")+
  stat_function(fun=(NPSi.mainf), color="slategray4")+
  stat_function(fun=(P.mainf), color="goldenrod2")+
  stat_function(fun=(PSi.mainf), color="turquoise4")+
  stat_function(fun=(NSi.mainf), color="orchid")+
  stat_function(fun=(Si.mainf), color="deepskyblue3")

C<-ggplot(subset(sum_g_trib_s, !(nutrient=="control")), aes(x=river_km, y=chl_a.nrr))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr, width=2.5))+
  scale_color_manual(values=c("red","gold", "steelblue3", "darkorange2", "darkorchid", "green3", "slategray4"))+
  theme_classic(base_size=13)+ylab(expression(paste('NRR '[Chla],''))) +xlab("River km")+
  ggtitle("C Tributaries, Summer")+theme(plot.title = element_text(hjust = 0.0, vjust=-1))+
  theme(legend.title=element_blank(),legend.position="none")+
  scale_x_continuous(limits=c(0,290))+
  scale_y_log10(breaks = c(0.3, 1.0, 3.0, 10, 30), labels = c(0.3, 1.0, 3.0, 10, 30))

  +stat_function(fun=N.tribsL, color="red")+
  stat_function(fun=NP.tribsL, color="orange")+
  stat_function(fun=NPSi.tribsL, color="slategray4")+
  stat_function(fun=P.tribsL, color="goldenrod2")+
  stat_function(fun=PSi.tribsL, color="turquoise4")+
  stat_function(fun=NSi.tribsL, color="orchid")+
  stat_function(fun=Si.tribsL, color="deepskyblue3")

D<-ggplot(subset(sum_g_trib_f, !(nutrient=="control")), aes(x=river_km, y=chl_a.nrr))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=chl_a.nrr-se_chla_nrr, ymax=chl_a.nrr+se_chla_nrr), width=2.5)+
  scale_color_manual(values=c("red","gold", "steelblue3", "darkorange2", "darkorchid", "green3", "slategray4"))+
  theme_classic(base_size = 13)+ ylab(expression(paste('NRR '[Chla],''))) +xlab("River km")+
  ggtitle("D Tributaries, Autumn")+theme(plot.title = element_text(hjust = 0.0, vjust=-1))+
  theme(legend.title=element_blank(),legend.position="none")+
  scale_x_continuous(limits=c(0,290))+scale_y_continuous(limits=c(0,2.5), breaks=seq(0, 2.5, by=0.5))

  +stat_function(fun=N.tribf, color="red")+
  stat_function(fun=NP.tribf, color="orange")+
  stat_function(fun=NPSi.tribf, color="slategray4")+
  stat_function(fun=P.tribf, color="goldenrod2")+
  stat_function(fun=PSi.tribf, color="turquoise4")+
  stat_function(fun=NSi.tribf, color="orchid")+
  stat_function(fun=Si.tribf, color="deepskyblue3")

grid.arrange(A, B, C, D, ncol=2, nrow = 2, heights = c(1, 1.2))

#chlorophyll-a, without Wenas
#Fixed effects: log(chla.nrr) ~ river_mile * season 
#                         Value  Std.Error  DF   t-value p-value
#(Intercept)            -0.3379904 0.11646515 716 -2.902074  0.0038
#river_mile             0.0053242 0.00113156 716  4.705179  0.0000
#seasonfall             0.0848563 0.16588367 716  0.511541  0.6091
#river_mile:seasonfall -0.0079885 0.00141784 716 -5.634295  0.0000

#       (Intercept)    river_mile
#control  0.0001417365  0.0001481625
#N       -0.0285294132  0.0018817072
#NP       0.0148751965 -0.0010105739
#NPSi    -0.0257849645  0.0015902815
#NSi      0.0281159682 -0.0018150838
#P        0.0105744550 -0.0006058083
#PSi      0.0100358109 -0.0006453788
#Si      -0.0094287894  0.0004566935

N.chlas<-function (x){(-0.3379904 -0.0285294132+(0.0018817072*0.0053242*1.00798853*x))}
NP.chlas<-function (x){(-0.3379904 +0.0148751965+(-0.0010105739*0.0053242*1.00798853*x))}
NPSi.chlas<-function (x){(-0.3379904 -0.0257849645+(0.0015902815*0.0053242*1.00798853*x))}
NSi.chlas<-function (x){(-0.3379904 +0.0281159682+(-0.0018150838*0.0053242*1.00798853*x))}
P.chlas<-function (x){(-0.3379904+0.0105744550(-0.0006058083*0.0053242*1.00798853*x))}
PSi.chlas<-function (x){(-0.3379904 +0.0100358109+(-0.0006453788*0.0053242*1.00798853*x))}
Si.chlas<-function (x){(-0.3379904 -0.00942878945+(0.0004566935*0.0053242*1.00798853*x))}

N.chlaf<-function (x){(-0.3379904 -0.0285294132+(0.0018817072*0.0053242*x))*1.0848563}
NP.chlaf<-function (x){(-0.3379904 +0.0148751965+(-0.0010105739*0.0053242*x))*1.0848563}
NPSi.chlaf<-function (x){(-0.3379904 -0.0257849645+(0.0015902815*0.0053242*x))*1.0848563}
NSi.chlaf<-function (x){(-0.3379904 +0.0281159682+(-0.0018150838*0.0053242*x))*1.0848563}
P.chlaf<-function (x){(-0.3379904+0.0105744550(-0.0006058083*0.0053242*x))*1.0848563}
PSi.chlaf<-function (x){(-0.3379904 +0.0100358109+(-0.0006453788*0.0053242*x))*1.0848563}
Si.chlaf<-function (x){(-0.3379904 -0.00942878945+(0.0004566935*0.0053242*x))*1.0848563}

sum_g_w<-subset(sum_g, !(stream=="wenas"))
sum_g_ws<-subset(sum_g_w, season=="summer")
sum_g_wf<-subset(sum_g_w, season=="fall")

A<-ggplot(subset(sum_g_ws, !(nutrient=="control")), aes(x=river_mile, y=log(chl_a.nrr)))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=log(chl_a.nrr)-log(se_chla_nrr), ymax=log(chl_a.nrr)+log(se_chla_nrr), width=2.5))+
  scale_color_manual(values=c("red","goldenrod2", "deepskyblue3", "orange", "orchid", "turquoise4", "slategray4"))+
  theme_bw(base_size = 14)+ylab(expression(paste('log (NRR'[chla],')'))) +xlab("River Mile")+
  ggtitle("A. Summer")+theme(plot.title = element_text(hjust = 0.02, vjust=-8))+
  theme(legend.title=element_blank(),legend.position="none")+
  stat_function(fun=N.chlas, color="red")+
  stat_function(fun=NP.chlas, color="orange")+
  stat_function(fun=NPSi.chlas, color="slategray4")+
  stat_function(fun=P.chlas, color="goldenrod2")+
  stat_function(fun=PSi.chlas, color="turquoise4")+
  stat_function(fun=NSi.chlas, color="orchid")+
  stat_function(fun=Si.chlas, color="deepskyblue3")

B<-ggplot(subset(sum_g_wf, !(nutrient=="control")), aes(x=river_mile, y=log(chl_a.nrr)))+
  geom_point(aes(color=factor(nutrient)), size=3)+
  geom_errorbar(aes(ymin=log(chl_a.nrr)-log(se_chla_nrr), ymax=log(chl_a.nrr)+log(se_chla_nrr), width=2.5))+
  scale_color_manual(values=c("red","goldenrod2", "deepskyblue3", "orange", "orchid", "turquoise4", "slategray4"))+
  theme_bw(base_size = 14)+ylab(expression(paste('log (NRR'[chla],')')))+xlab("River Mile") +
  ggtitle("B. Fall")+theme(plot.title = element_text(hjust = 0.02, vjust=-8))+
  theme(legend.title=element_blank(),legend.position="none")+
  stat_function(fun=N.chlaf, color="red")+
  stat_function(fun=NP.chlaf, color="orange")+
  stat_function(fun=NPSi.chlaf, color="slategray4")+
  stat_function(fun=P.chlaf, color="goldenrod2")+
  stat_function(fun=PSi.chlaf, color="turquoise4")+
  stat_function(fun=NSi.chlaf, color="orchid")+
  stat_function(fun=Si.chlaf, color="deepskyblue3")

grid.arrange(A, B, ncol=1)

#slopes are so low that they look flat here. For some reason, summer lines are below all points - maybe need to check.
#either way, hand-calculation reveals almost no change. 



#previous attempts - can probably delete*****
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
      
