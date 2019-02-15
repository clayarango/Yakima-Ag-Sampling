#14-Feb-2019
#Code to analyze tribs for 2018 NDS
#Sarah Roley, Clay Arango, Alex Alexiades

#First: combine all tribs into one dataset.

aht_summer<-read.csv("aht_summer.csv")
str(aht_summer)
aht_summer$site.date<-"Ahtanum_summer_2018"
aht_summer$chla<-NA
as_control<-subset(aht_summer, nutrient=="control")
as_c_means<-ddply(as_control, "top", summarise, ave_gpp = mean(gpp.area), 
                  ave_cr = mean(cr.area))

as_c_means
aht_summer$nrr.gpp<-ifelse(aht_summer$top =="glass", aht_summer$gpp.area/6.355,NA)
aht_summer$nrr.cr<-ifelse(aht_summer$top=="cellulose", aht_summer$cr.area/-20.4361, NA)

aht_fall<-read.csv("aht_fall.csv")
str(aht_fall)
aht_fall$site.date<-"Ahtanum_fall_2018"
af_control<-subset(aht_fall, nutrient=="control")
af_c_means<-ddply(af_control, "top", summarise, ave_gpp = mean(gpp.area), 
                  ave_cr = mean(cr.area))

af_c_means
aht_fall$nrr.gpp<-ifelse(aht_fall$top =="glass", aht_fall$gpp.area/6.2642,NA)
aht_fall$nrr.cr<-ifelse(aht_fall$top=="cellulose", aht_fall$cr.area/-16.94, NA)


reec_fall<-read.csv("reec_fall.csv")
str(reec_fall)
reec_fall$site.date<-"Reecer_fall_2018"
reec_fall<-reec_fall[-c(1,11:12)]

rf_control<-subset(reec_fall, nutrient=="C")
rf_c_means<-ddply(rf_control, "top", summarise, ave_gpp = mean(gpp.area), 
                  ave_cr = mean(cr.area))

rf_c_means
reec_fall$nrr.gpp<-ifelse(reec_fall$top =="glass", reec_fall$gpp.area/2.2686,NA)
reec_fall$nrr.cr<-ifelse(reec_fall$top=="sponge", reec_fall$cr.area/-9.923, NA)


reec_summer<-read.csv("reec_summer.csv")
str(reec_summer)
reec_summer$site.date<-"Reecer_summer_2018"
reec_summer<-reec_summer[-c(1,11:12)]
rs_control<-subset(reec_summer, nutrient=="C")
rs_c_means<-ddply(rs_control, "top", summarise, ave_gpp = mean(gpp.area), 
                  ave_cr = mean(cr.area))

rs_c_means
reec_summer$nrr.gpp<-ifelse(reec_summer$top =="glass", reec_summer$gpp.area/5.7705,NA)
reec_summer$nrr.cr<-ifelse(reec_summer$top=="sponge", reec_summer$cr.area/-15.022, NA)


satus_fall<-read.csv("satus_fall.csv")
str(satus_fall)
sf_control<-subset(satus_fall, nutrient=="control")
sf_c_means<-ddply(sf_control, "top", summarise, ave_gpp = mean(gpp.area), 
                  ave_cr = mean(cr.area))

sf_c_means
satus_fall$nrr.gpp<-ifelse(satus_fall$top =="glass", satus_fall$gpp.area/4.3452,NA)
satus_fall$nrr.cr<-ifelse(satus_fall$top=="cellulose", satus_fall$cr.area/-17.1172, NA)


satus_summer<-read.csv("satus_summer.csv")
str(satus_summer)
ss_control<-subset(satus_summer, nutrient=="control")
ss_c_means<-ddply(ss_control, "top", summarise, ave_gpp = mean(gpp.area, na.rm=T), 
                  ave_cr = mean(cr.area, na.rm=T))

ss_c_means
satus_summer$nrr.gpp<-ifelse(satus_summer$top =="glass", satus_summer$gpp.area/2.7173,NA)
satus_summer$nrr.cr<-ifelse(satus_summer$top=="cellulose", satus_summer$cr.area/-21.026, NA)


wen_summer<-read.csv("wen_summer.csv")
str(wen_summer)

wen_fall<-read.csv("wen_fall.csv")

topp_summer<-read.csv("toppenish_summer.csv")
str(topp_summer)

ts_control<-subset(topp_summer, nutrient=="control")
ts_c_means<-ddply(ts_control, "top", summarise, ave_gpp = mean(gpp.area), 
                  ave_cr = mean(cr.area))

ts_c_means
topp_summer$nrr.gpp<-ifelse(topp_summer$top =="glass", topp_summer$gpp.area/2.323,NA)
topp_summer$nrr.cr<-ifelse(topp_summer$top=="cellulose", topp_summer$cr.area/-19.11745, NA)


tribs_summer<-rbind(topp_summer, satus_summer, aht_summer, reec_summer)
tribs_summer$top<-recode(tribs_summer$top, "cellulose" ="sponge")
tribs_summer$nutrient<-recode(tribs_summer$nutrient, "C"="control", "N+P"="NP",
                              "N+P+Si"="NPSi", "N+Si"="NSi", "P+Si"="PSi")

tribs_summer[-(321:402),] facet_wrap(~Site+Time)

ggplot(data=subset(tribs_summer, top ="glass"), aes(x=nutrient, y=nrr.gpp))+
  geom_boxplot()+facet_wrap(~site.date)+theme_classic()+ylim(0,5)

ggplot(data=subset(tribs_summer, top ="glass"), aes(x=nutrient, y=nrr.cr))+
  geom_boxplot()+facet_wrap(~site.date)+theme_classic()
