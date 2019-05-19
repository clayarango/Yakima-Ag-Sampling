#code to analyze water quality data

nut = read.table(file="nutrients_summary.csv", header=T, sep=",")

#subset mainstem
nut.main = subset(nut, type=="mainstem", data=nut)
nut.main

#make fall and summer vectors for paired t
fall = subset(nut.main, season=="fall", data=nut.main)
summer = subset(nut.main, season=="summer", data=nut.main)

#code position as upstream->downstream
fall$position = c(4, 1, 6, 5, 2, 3)
summer$position = c(4, 1, 6, 5, 2, 3)

#paired t for DOC
t.test(fall$DOC_ave, summer$DOC_ave, paired=T)

x <- data.frame( 
  season = rep(c("fall", "summer"), each = 6),
  doc = c(fall$DOC_ave,  summer$DOC_ave))

x$season <- factor(x$season, levels = c("summer", "fall"))
x$season<-recode(x$season, "summer" ="Summer")
x$season<-recode(x$season, "fall" ="Fall")

boxplot(x$doc~x$season, ylab = "Dissolved Organic C (mg C/L)")

#plot longitudinal trend
plot(fall$DOC_ave~fall$position, xlab="Longitudinal Position", ylab="Dissolved Organic Carbon (mg C/L)")
plot(summer$DOC_ave~summer$position, xlab="Longitudinal Position", ylab="Dissolved Organic Carbon (mg C/L)")


#paired t for NO3
t.test(fall$NO3, summer$NO3, paired=T)
x <- data.frame( 
  season = rep(c("fall", "summer"), each = 6),
  NO3 = c(fall$NO3,  summer$NO3))

x$season <- factor(x$season, levels = c("summer", "fall"))
x$season<-recode(x$season, "summer" ="Summer")
x$season<-recode(x$season, "fall" ="Fall")

boxplot(x$NO3~x$season, ylab = "Nitrate (mg N/L)")

#plot longitudinal trend
plot(fall$NO3~fall$position, xlab="Longitudinal Position", ylab="Nitrate (mg N/L)")
plot(summer$NO3~summer$position, xlab="Longitudinal Position", ylab="Nitrate (mg N/L)")

#paired t for NH4
t.test(fall$NH4, summer$NH4, paired=T)
x <- data.frame( 
  season = rep(c("fall", "summer"), each = 6),
  NH4 = c(fall$NH4,  summer$NH4))

x$season <- factor(x$season, levels = c("summer", "fall"))
x$season<-recode(x$season, "summer" ="Summer")
x$season<-recode(x$season, "fall" ="Fall")

boxplot(x$NH4~x$season, ylab = "Ammonium (mg N/L)")

#plot longitudinal trend
plot(fall$NH4~fall$position, xlab="Longitudinal Position", ylab="Ammonium (mg N/L)")
plot(summer$NH4~summer$position)

#NRR linear model
x = lm(nut.main$CRNRR.NO3~nut.main$DIN)
summary(x)
plot(nut.main$CRNRR.NO3~nut.main$DIN, xlab="Dissolved Inorganic N (mg/L)", ylab="NRR of NO3 Respiration")

x = lm(nut.main$GPPNRR.NO3~nut.main$DIN)
summary(x)
plot(nut.main$GPPNRR.NO3~nut.main$DIN, xlab="Dissolved Inorganic N (mg/L)", ylab="NRR of NO3 Production")
