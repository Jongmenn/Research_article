library(readxl) ;library(ggplot2)
library(sqldf)  ;library(dplyr)
library(plyr)   ;library(mgcv)
library(TTR)    ;library(Epi)
library(tsModel);library(lubridate)
library(dlnm)   ;library(metafor)
library(mixmeta);library(plotrix)
setwd("C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_AOM\\analysis\\data")

aom_w1<-read.csv("AOM_W1.csv",header=T) ; aom_w1$DATE=as.Date(aom_w1$DATE)
aom_w2<-read.csv("AOM_W2.csv",header=T) ; aom_w2$DATE=as.Date(aom_w2$DATE)
aom_w3<-read.csv("AOM_W3.csv",header=T) ; aom_w3$DATE=as.Date(aom_w3$DATE)
aom_w4<-read.csv("AOM_W4.csv",header=T) ; aom_w4$DATE=as.Date(aom_w4$DATE)

aom_w1$key=paste0(aom_w1$SIDO,"-",aom_w1$DATE) ; aom_w1$yyyy=substr(aom_w1$DATE,1,4) 
aom_w2$key=paste0(aom_w2$SIDO,"-",aom_w2$DATE) ; aom_w2$yyyy=substr(aom_w2$DATE,1,4)
aom_w3$key=paste0(aom_w3$SIDO,"-",aom_w3$DATE) ; aom_w3$yyyy=substr(aom_w3$DATE,1,4)
aom_w4$key=paste0(aom_w4$SIDO,"-",aom_w4$DATE) ; aom_w4$yyyy=substr(aom_w4$DATE,1,4)

# aom_count_w4<-cbind(with(aom_w4,aggregate(AGE0,list(yyyy,SIDO),sum)),
#                     with(aom_w4,aggregate(AGE0_M,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE0_F,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE1,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE1_M,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE1_F,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE2,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE2_M,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE2_F,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE3,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE3_M,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE3_F,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE13,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE13_M,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE13_F,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(TOT,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(TOT_M,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(TOT_F,list(yyyy,SIDO),sum))[,3])
# 
# names(aom_count_w4)=c("year","sido",names(aom_w4)[5:22])
# 
# setwd("C:\\Users\\AROOM206_kard141\\Desktop\\AOM\\분석\\결과")
# write.csv(aom_count_w4,file="aom_count_w4.csv",row.names=F)
####################################################################################################################################
#CMAQ PM2.5 자료 가져오기 ; 노출자료  
setwd("C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_AOM\\analysis")
pm<-read.csv("CMAQ_PM2.5_short.csv",header=T)

names(pm)[31]="mtemp" # 일 평균 기온
names(pm)[48]="mwind" # 일 평균 풍속
names(pm)[50]="mdew"  # 일 평균 이슬점 온도
names(pm)[53]="mhum"  # 일 평균 상대 습도
names(pm)[54]="map"   # 일 평균 증기압

# pm<-pm %>%  select(date:mtemp,mwind,mdew,mhum,map)
pm<-pm [,c(1:10,18:24,31,48,50,53,54)]
pm[,3:17]<-pm[,3:17]/10 #PM 10으로 나눠줌, 1단위 증가당 10ug/m3  으로 보기 위해 

#Apparent Temperature 변수 추가
#Compute simple at (schwartz version)
pm$AT=-2.653+(0.994*pm$mtemp)+(0.0153*pm$mdew*pm$mdew)
pm <- subset(pm,SIDO %in% c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"))

###########################################################################################################################################
#merge 
pm$key=ifelse(pm$SIDO=="Seoul",paste0(11,"-",pm$date),
              ifelse(pm$SIDO=="Busan",paste0(26,"-",pm$date),
                     ifelse(pm$SIDO=="Daegu",paste0(27,"-",pm$date),
                            ifelse(pm$SIDO=="Incheon",paste0(28,"-",pm$date),
                                   ifelse(pm$SIDO=="Gwangju",paste0(29,"-",pm$date),
                                          ifelse(pm$SIDO=="Daejeon",paste0(30,"-",pm$date),paste0(31,"-",pm$date)))))))

pm2<-pm[,3:length(pm)]

aom_w1<-merge(aom_w1,pm2,by="key")
aom_w2<-merge(aom_w2,pm2,by="key")
aom_w3<-merge(aom_w3,pm2,by="key")
aom_w4<-merge(aom_w4,pm2,by="key")

dow<-read.csv("dow.csv",header=T)
dow$date=as.Date(dow$date)
dow<-subset(dow,date>="2008-01-01")
names(dow)=c("DATE","dow")

aom_w1<-merge(dow,aom_w1,by="DATE") ; aom_w1$dow=as.factor(aom_w1$dow)
aom_w2<-merge(dow,aom_w2,by="DATE") ; aom_w2$dow=as.factor(aom_w2$dow)
aom_w3<-merge(dow,aom_w3,by="DATE") ; aom_w3$dow=as.factor(aom_w3$dow)
aom_w4<-merge(dow,aom_w4,by="DATE") ; aom_w4$dow=as.factor(aom_w4$dow)

s1.w1<-subset(aom_w1,aom_w1$SIDO==11);s1.w2<-subset(aom_w2,aom_w2$SIDO==11);s1.w3<-subset(aom_w3,aom_w3$SIDO==11);s1.w4<-subset(aom_w4,aom_w4$SIDO==11);
s2.w1<-subset(aom_w1,aom_w1$SIDO==26);s2.w2<-subset(aom_w2,aom_w2$SIDO==26);s2.w3<-subset(aom_w3,aom_w3$SIDO==26);s2.w4<-subset(aom_w4,aom_w4$SIDO==26);
s3.w1<-subset(aom_w1,aom_w1$SIDO==27);s3.w2<-subset(aom_w2,aom_w2$SIDO==27);s3.w3<-subset(aom_w3,aom_w3$SIDO==27);s3.w4<-subset(aom_w4,aom_w4$SIDO==27);
s4.w1<-subset(aom_w1,aom_w1$SIDO==28);s4.w2<-subset(aom_w2,aom_w2$SIDO==28);s4.w3<-subset(aom_w3,aom_w3$SIDO==28);s4.w4<-subset(aom_w4,aom_w4$SIDO==28);
s5.w1<-subset(aom_w1,aom_w1$SIDO==29);s5.w2<-subset(aom_w2,aom_w2$SIDO==29);s5.w3<-subset(aom_w3,aom_w3$SIDO==29);s5.w4<-subset(aom_w4,aom_w4$SIDO==29);
s6.w1<-subset(aom_w1,aom_w1$SIDO==30);s6.w2<-subset(aom_w2,aom_w2$SIDO==30);s6.w3<-subset(aom_w3,aom_w3$SIDO==30);s6.w4<-subset(aom_w4,aom_w4$SIDO==30);
s7.w1<-subset(aom_w1,aom_w1$SIDO==31);s7.w2<-subset(aom_w2,aom_w2$SIDO==31);s7.w3<-subset(aom_w3,aom_w3$SIDO==31);s7.w4<-subset(aom_w4,aom_w4$SIDO==31);

df1<- s1.w4;df1$exposure=s1.w4$lag04*10
df2<- s2.w4;df2$exposure=s2.w4$lag04*10
df3<- s3.w4;df3$exposure=s3.w4$lag04*10
df4<- s4.w4;df4$exposure=s4.w4$lag04*10
df5<- s5.w4;df5$exposure=s5.w4$lag04*10
df6<- s6.w4;df6$exposure=s6.w4$lag04*10
df7<- s7.w4;df7$exposure=s7.w4$lag04*10

g1<-gam(TOT~s(exposure)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=df1)
g2<-gam(TOT~s(exposure)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=df2)
g3<-gam(TOT~s(exposure)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=df3)
g4<-gam(TOT~s(exposure)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=df4)
g5<-gam(TOT~s(exposure)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=df5)
g6<-gam(TOT~s(exposure)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=df6)
g7<-gam(TOT~s(exposure)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=df7)
x11();
par(mfrow=c(2,4))
plot(g1,select=1,cex.axis=1.45,cex.lab=1.45,main="Seoul",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g2,select=1,cex.axis=1.45,cex.lab=1.45,main="Busan",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g3,select=1,cex.axis=1.45,cex.lab=1.45,main="Daegu",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g4,select=1,cex.axis=1.45,cex.lab=1.45,main="Incheon",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g5,select=1,cex.axis=1.45,cex.lab=1.45,main="Gwangju",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g6,select=1,cex.axis=1.45,cex.lab=1.45,main="Daejeon",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g7,select=1,cex.axis=1.45,cex.lab=1.45,main="Ulsan",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))

###########################################################################################################################################
#########################################################################################################################################
#서울지역만, 질환 연령 다르게 하여서 자유도 비교하기 
#Best Moedl fit 결정하기 위해 여러가지 조합 , Time df, AT df만 조정 
#summary table 

#################################################################################################################################################################################
#Time trend 자유도에 따라 beta값 변이 
#Apparent Temperature fixed, time df 조정 (3~15)
#Seoul Time trend
#################################################################################################################################################################################
########################
###GAM 모델링 - 서울####
########################
fit01=NULL;fit02=NULL;fit03=NULL;fit04=NULL;fit05=NULL;fit06=NULL;fit07=NULL;fit08=NULL;fit09=NULL

for(i in 24:38){
  fit01[[i]]<-summary(gam(AGE0~s1.w2[,i]   +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s1.w2))$'p.table'[2,]
  fit02[[i]]<-summary(gam(AGE13~s1.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s1.w2))$'p.table'[2,]
  fit03[[i]]<-summary(gam(TOT~s1.w2[,i]    +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s1.w2))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0_M~s1.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s1.w2))$'p.table'[2,]
  fit05[[i]]<-summary(gam(AGE0_F~s1.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s1.w2))$'p.table'[2,]
  fit06[[i]]<-summary(gam(AGE13_M~s1.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s1.w2))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE13_F~s1.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s1.w2))$'p.table'[2,]
  fit08[[i]]<-summary(gam(TOT_M~s1.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s1.w2))$'p.table'[2,]
  fit09[[i]]<-summary(gam(TOT_F~s1.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s1.w2))$'p.table'[2,]
  print(i)}

s1.fit01<-as.data.frame(do.call(rbind,fit01));s1.fit01$category="AGE0"   ;s1.fit01$lag=names(s1.w2)[24:38]
s1.fit02<-as.data.frame(do.call(rbind,fit02));s1.fit02$category="AGE13"  ;s1.fit02$lag=names(s1.w2)[24:38]
s1.fit03<-as.data.frame(do.call(rbind,fit03));s1.fit03$category="TOT"    ;s1.fit03$lag=names(s1.w2)[24:38]
s1.fit04<-as.data.frame(do.call(rbind,fit04));s1.fit04$category="AGE0_M" ;s1.fit04$lag=names(s1.w2)[24:38]
s1.fit05<-as.data.frame(do.call(rbind,fit05));s1.fit05$category="AGE0_F" ;s1.fit05$lag=names(s1.w2)[24:38]
s1.fit06<-as.data.frame(do.call(rbind,fit06));s1.fit06$category="AGE13_M";s1.fit06$lag=names(s1.w2)[24:38]
s1.fit07<-as.data.frame(do.call(rbind,fit07));s1.fit07$category="AGE13_F";s1.fit07$lag=names(s1.w2)[24:38]
s1.fit08<-as.data.frame(do.call(rbind,fit08));s1.fit08$category="TOT_M"  ;s1.fit08$lag=names(s1.w2)[24:38]
s1.fit09<-as.data.frame(do.call(rbind,fit09));s1.fit09$category="TOT_F"  ;s1.fit09$lag=names(s1.w2)[24:38]

s1.fit<-rbind(s1.fit01,s1.fit02,s1.fit03,
              s1.fit04,s1.fit05,s1.fit06,
              s1.fit07,s1.fit08,s1.fit09)
##################################################################################################################################################
########################
###GAM 모델링 - 부산####
########################
fit01=NULL;fit02=NULL;fit03=NULL;fit04=NULL;fit05=NULL;fit06=NULL;fit07=NULL;fit08=NULL;fit09=NULL

for(i in 24:38){
  fit01[[i]]<-summary(gam(AGE0~s2.w2[,i]   +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s2.w2))$'p.table'[2,]
  fit02[[i]]<-summary(gam(AGE13~s2.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s2.w2))$'p.table'[2,]
  fit03[[i]]<-summary(gam(TOT~s2.w2[,i]    +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s2.w2))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0_M~s2.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s2.w2))$'p.table'[2,]
  fit05[[i]]<-summary(gam(AGE0_F~s2.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s2.w2))$'p.table'[2,]
  fit06[[i]]<-summary(gam(AGE13_M~s2.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s2.w2))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE13_F~s2.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s2.w2))$'p.table'[2,]
  fit08[[i]]<-summary(gam(TOT_M~s2.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s2.w2))$'p.table'[2,]
  fit09[[i]]<-summary(gam(TOT_F~s2.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s2.w2))$'p.table'[2,]
  print(i)}

s2.fit01<-as.data.frame(do.call(rbind,fit01));s2.fit01$category="AGE0"   ;s2.fit01$lag=names(s2.w2)[24:38]
s2.fit02<-as.data.frame(do.call(rbind,fit02));s2.fit02$category="AGE13"  ;s2.fit02$lag=names(s2.w2)[24:38]
s2.fit03<-as.data.frame(do.call(rbind,fit03));s2.fit03$category="TOT"    ;s2.fit03$lag=names(s2.w2)[24:38]
s2.fit04<-as.data.frame(do.call(rbind,fit04));s2.fit04$category="AGE0_M" ;s2.fit04$lag=names(s2.w2)[24:38]
s2.fit05<-as.data.frame(do.call(rbind,fit05));s2.fit05$category="AGE0_F" ;s2.fit05$lag=names(s2.w2)[24:38]
s2.fit06<-as.data.frame(do.call(rbind,fit06));s2.fit06$category="AGE13_M";s2.fit06$lag=names(s2.w2)[24:38]
s2.fit07<-as.data.frame(do.call(rbind,fit07));s2.fit07$category="AGE13_F";s2.fit07$lag=names(s2.w2)[24:38]
s2.fit08<-as.data.frame(do.call(rbind,fit08));s2.fit08$category="TOT_M"  ;s2.fit08$lag=names(s2.w2)[24:38]
s2.fit09<-as.data.frame(do.call(rbind,fit09));s2.fit09$category="TOT_F"  ;s2.fit09$lag=names(s2.w2)[24:38]

s2.fit<-rbind(s2.fit01,s2.fit02,s2.fit03,
              s2.fit04,s2.fit05,s2.fit06,
              s2.fit07,s2.fit08,s2.fit09)
##################################################################################################################################################
########################
###GAM 모델링 - 대구####
########################
fit01=NULL;fit02=NULL;fit03=NULL;fit04=NULL;fit05=NULL;fit06=NULL;fit07=NULL;fit08=NULL;fit09=NULL

for(i in 24:38){
  fit01[[i]]<-summary(gam(AGE0~s3.w2[,i]   +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s3.w2))$'p.table'[2,]
  fit02[[i]]<-summary(gam(AGE13~s3.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s3.w2))$'p.table'[2,]
  fit03[[i]]<-summary(gam(TOT~s3.w2[,i]    +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s3.w2))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0_M~s3.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s3.w2))$'p.table'[2,]
  fit05[[i]]<-summary(gam(AGE0_F~s3.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s3.w2))$'p.table'[2,]
  fit06[[i]]<-summary(gam(AGE13_M~s3.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s3.w2))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE13_F~s3.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s3.w2))$'p.table'[2,]
  fit08[[i]]<-summary(gam(TOT_M~s3.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s3.w2))$'p.table'[2,]
  fit09[[i]]<-summary(gam(TOT_F~s3.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s3.w2))$'p.table'[2,]
  print(i)}

s3.fit01<-as.data.frame(do.call(rbind,fit01));s3.fit01$category="AGE0"   ;s3.fit01$lag=names(s3.w2)[24:38]
s3.fit02<-as.data.frame(do.call(rbind,fit02));s3.fit02$category="AGE13"  ;s3.fit02$lag=names(s3.w2)[24:38]
s3.fit03<-as.data.frame(do.call(rbind,fit03));s3.fit03$category="TOT"    ;s3.fit03$lag=names(s3.w2)[24:38]
s3.fit04<-as.data.frame(do.call(rbind,fit04));s3.fit04$category="AGE0_M" ;s3.fit04$lag=names(s3.w2)[24:38]
s3.fit05<-as.data.frame(do.call(rbind,fit05));s3.fit05$category="AGE0_F" ;s3.fit05$lag=names(s3.w2)[24:38]
s3.fit06<-as.data.frame(do.call(rbind,fit06));s3.fit06$category="AGE13_M";s3.fit06$lag=names(s3.w2)[24:38]
s3.fit07<-as.data.frame(do.call(rbind,fit07));s3.fit07$category="AGE13_F";s3.fit07$lag=names(s3.w2)[24:38]
s3.fit08<-as.data.frame(do.call(rbind,fit08));s3.fit08$category="TOT_M"  ;s3.fit08$lag=names(s3.w2)[24:38]
s3.fit09<-as.data.frame(do.call(rbind,fit09));s3.fit09$category="TOT_F"  ;s3.fit09$lag=names(s3.w2)[24:38]

s3.fit<-rbind(s3.fit01,s3.fit02,s3.fit03,
              s3.fit04,s3.fit05,s3.fit06,
              s3.fit07,s3.fit08,s3.fit09)
##################################################################################################################################################
########################
###GAM 모델링 - 인천####
########################
fit01=NULL;fit02=NULL;fit03=NULL;fit04=NULL;fit05=NULL;fit06=NULL;fit07=NULL;fit08=NULL;fit09=NULL

for(i in 24:38){
  fit01[[i]]<-summary(gam(AGE0~s4.w2[,i]   +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s4.w2))$'p.table'[2,]
  fit02[[i]]<-summary(gam(AGE13~s4.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s4.w2))$'p.table'[2,]
  fit03[[i]]<-summary(gam(TOT~s4.w2[,i]    +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s4.w2))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0_M~s4.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s4.w2))$'p.table'[2,]
  fit05[[i]]<-summary(gam(AGE0_F~s4.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s4.w2))$'p.table'[2,]
  fit06[[i]]<-summary(gam(AGE13_M~s4.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s4.w2))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE13_F~s4.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s4.w2))$'p.table'[2,]
  fit08[[i]]<-summary(gam(TOT_M~s4.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s4.w2))$'p.table'[2,]
  fit09[[i]]<-summary(gam(TOT_F~s4.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s4.w2))$'p.table'[2,]
  print(i)}

s4.fit01<-as.data.frame(do.call(rbind,fit01));s4.fit01$category="AGE0"   ;s4.fit01$lag=names(s4.w2)[24:38]
s4.fit02<-as.data.frame(do.call(rbind,fit02));s4.fit02$category="AGE13"  ;s4.fit02$lag=names(s4.w2)[24:38]
s4.fit03<-as.data.frame(do.call(rbind,fit03));s4.fit03$category="TOT"    ;s4.fit03$lag=names(s4.w2)[24:38]
s4.fit04<-as.data.frame(do.call(rbind,fit04));s4.fit04$category="AGE0_M" ;s4.fit04$lag=names(s4.w2)[24:38]
s4.fit05<-as.data.frame(do.call(rbind,fit05));s4.fit05$category="AGE0_F" ;s4.fit05$lag=names(s4.w2)[24:38]
s4.fit06<-as.data.frame(do.call(rbind,fit06));s4.fit06$category="AGE13_M";s4.fit06$lag=names(s4.w2)[24:38]
s4.fit07<-as.data.frame(do.call(rbind,fit07));s4.fit07$category="AGE13_F";s4.fit07$lag=names(s4.w2)[24:38]
s4.fit08<-as.data.frame(do.call(rbind,fit08));s4.fit08$category="TOT_M"  ;s4.fit08$lag=names(s4.w2)[24:38]
s4.fit09<-as.data.frame(do.call(rbind,fit09));s4.fit09$category="TOT_F"  ;s4.fit09$lag=names(s4.w2)[24:38]

s4.fit<-rbind(s4.fit01,s4.fit02,s4.fit03,
              s4.fit04,s4.fit05,s4.fit06,
              s4.fit07,s4.fit08,s4.fit09)
##################################################################################################################################################
########################
###GAM 모델링 - 광주####
########################
fit01=NULL;fit02=NULL;fit03=NULL;fit04=NULL;fit05=NULL;fit06=NULL;fit07=NULL;fit08=NULL;fit09=NULL

for(i in 24:38){
  fit01[[i]]<-summary(gam(AGE0~s5.w2[,i]   +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s5.w2))$'p.table'[2,]
  fit02[[i]]<-summary(gam(AGE13~s5.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s5.w2))$'p.table'[2,]
  fit03[[i]]<-summary(gam(TOT~s5.w2[,i]    +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s5.w2))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0_M~s5.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s5.w2))$'p.table'[2,]
  fit05[[i]]<-summary(gam(AGE0_F~s5.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s5.w2))$'p.table'[2,]
  fit06[[i]]<-summary(gam(AGE13_M~s5.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s5.w2))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE13_F~s5.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s5.w2))$'p.table'[2,]
  fit08[[i]]<-summary(gam(TOT_M~s5.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s5.w2))$'p.table'[2,]
  fit09[[i]]<-summary(gam(TOT_F~s5.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s5.w2))$'p.table'[2,]
  print(i)}

s5.fit01<-as.data.frame(do.call(rbind,fit01));s5.fit01$category="AGE0"   ;s5.fit01$lag=names(s5.w2)[24:38]
s5.fit02<-as.data.frame(do.call(rbind,fit02));s5.fit02$category="AGE13"  ;s5.fit02$lag=names(s5.w2)[24:38]
s5.fit03<-as.data.frame(do.call(rbind,fit03));s5.fit03$category="TOT"    ;s5.fit03$lag=names(s5.w2)[24:38]
s5.fit04<-as.data.frame(do.call(rbind,fit04));s5.fit04$category="AGE0_M" ;s5.fit04$lag=names(s5.w2)[24:38]
s5.fit05<-as.data.frame(do.call(rbind,fit05));s5.fit05$category="AGE0_F" ;s5.fit05$lag=names(s5.w2)[24:38]
s5.fit06<-as.data.frame(do.call(rbind,fit06));s5.fit06$category="AGE13_M";s5.fit06$lag=names(s5.w2)[24:38]
s5.fit07<-as.data.frame(do.call(rbind,fit07));s5.fit07$category="AGE13_F";s5.fit07$lag=names(s5.w2)[24:38]
s5.fit08<-as.data.frame(do.call(rbind,fit08));s5.fit08$category="TOT_M"  ;s5.fit08$lag=names(s5.w2)[24:38]
s5.fit09<-as.data.frame(do.call(rbind,fit09));s5.fit09$category="TOT_F"  ;s5.fit09$lag=names(s5.w2)[24:38]

s5.fit<-rbind(s5.fit01,s5.fit02,s5.fit03,
              s5.fit04,s5.fit05,s5.fit06,
              s5.fit07,s5.fit08,s5.fit09)
##################################################################################################################################################
########################
###GAM 모델링 - 대전####
########################
fit01=NULL;fit02=NULL;fit03=NULL;fit04=NULL;fit05=NULL;fit06=NULL;fit07=NULL;fit08=NULL;fit09=NULL

for(i in 24:38){
  fit01[[i]]<-summary(gam(AGE0~s6.w2[,i]   +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s6.w2))$'p.table'[2,]
  fit02[[i]]<-summary(gam(AGE13~s6.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s6.w2))$'p.table'[2,]
  fit03[[i]]<-summary(gam(TOT~s6.w2[,i]    +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s6.w2))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0_M~s6.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s6.w2))$'p.table'[2,]
  fit05[[i]]<-summary(gam(AGE0_F~s6.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s6.w2))$'p.table'[2,]
  fit06[[i]]<-summary(gam(AGE13_M~s6.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s6.w2))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE13_F~s6.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s6.w2))$'p.table'[2,]
  fit08[[i]]<-summary(gam(TOT_M~s6.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s6.w2))$'p.table'[2,]
  fit09[[i]]<-summary(gam(TOT_F~s6.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s6.w2))$'p.table'[2,]
  print(i)}

s6.fit01<-as.data.frame(do.call(rbind,fit01));s6.fit01$category="AGE0"   ;s6.fit01$lag=names(s6.w2)[24:38]
s6.fit02<-as.data.frame(do.call(rbind,fit02));s6.fit02$category="AGE13"  ;s6.fit02$lag=names(s6.w2)[24:38]
s6.fit03<-as.data.frame(do.call(rbind,fit03));s6.fit03$category="TOT"    ;s6.fit03$lag=names(s6.w2)[24:38]
s6.fit04<-as.data.frame(do.call(rbind,fit04));s6.fit04$category="AGE0_M" ;s6.fit04$lag=names(s6.w2)[24:38]
s6.fit05<-as.data.frame(do.call(rbind,fit05));s6.fit05$category="AGE0_F" ;s6.fit05$lag=names(s6.w2)[24:38]
s6.fit06<-as.data.frame(do.call(rbind,fit06));s6.fit06$category="AGE13_M";s6.fit06$lag=names(s6.w2)[24:38]
s6.fit07<-as.data.frame(do.call(rbind,fit07));s6.fit07$category="AGE13_F";s6.fit07$lag=names(s6.w2)[24:38]
s6.fit08<-as.data.frame(do.call(rbind,fit08));s6.fit08$category="TOT_M"  ;s6.fit08$lag=names(s6.w2)[24:38]
s6.fit09<-as.data.frame(do.call(rbind,fit09));s6.fit09$category="TOT_F"  ;s6.fit09$lag=names(s6.w2)[24:38]

s6.fit<-rbind(s6.fit01,s6.fit02,s6.fit03,
              s6.fit04,s6.fit05,s6.fit06,
              s6.fit07,s6.fit08,s6.fit09)
##################################################################################################################################################
########################
###GAM 모델링 - 울산####
########################
fit01=NULL;fit02=NULL;fit03=NULL;fit04=NULL;fit05=NULL;fit06=NULL;fit07=NULL;fit08=NULL;fit09=NULL

for(i in 24:38){
  fit01[[i]]<-summary(gam(AGE0~s7.w2[,i]   +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s7.w2))$'p.table'[2,]
  fit02[[i]]<-summary(gam(AGE13~s7.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s7.w2))$'p.table'[2,]
  fit03[[i]]<-summary(gam(TOT~s7.w2[,i]    +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s7.w2))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0_M~s7.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s7.w2))$'p.table'[2,]
  fit05[[i]]<-summary(gam(AGE0_F~s7.w2[,i] +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s7.w2))$'p.table'[2,]
  fit06[[i]]<-summary(gam(AGE13_M~s7.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s7.w2))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE13_F~s7.w2[,i]+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s7.w2))$'p.table'[2,]
  fit08[[i]]<-summary(gam(TOT_M~s7.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s7.w2))$'p.table'[2,]
  fit09[[i]]<-summary(gam(TOT_F~s7.w2[,i]  +s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s7.w2))$'p.table'[2,]
  print(i)}

s7.fit01<-as.data.frame(do.call(rbind,fit01));s7.fit01$category="AGE0"   ;s7.fit01$lag=names(s7.w2)[24:38]
s7.fit02<-as.data.frame(do.call(rbind,fit02));s7.fit02$category="AGE13"  ;s7.fit02$lag=names(s7.w2)[24:38]
s7.fit03<-as.data.frame(do.call(rbind,fit03));s7.fit03$category="TOT"    ;s7.fit03$lag=names(s7.w2)[24:38]
s7.fit04<-as.data.frame(do.call(rbind,fit04));s7.fit04$category="AGE0_M" ;s7.fit04$lag=names(s7.w2)[24:38]
s7.fit05<-as.data.frame(do.call(rbind,fit05));s7.fit05$category="AGE0_F" ;s7.fit05$lag=names(s7.w2)[24:38]
s7.fit06<-as.data.frame(do.call(rbind,fit06));s7.fit06$category="AGE13_M";s7.fit06$lag=names(s7.w2)[24:38]
s7.fit07<-as.data.frame(do.call(rbind,fit07));s7.fit07$category="AGE13_F";s7.fit07$lag=names(s7.w2)[24:38]
s7.fit08<-as.data.frame(do.call(rbind,fit08));s7.fit08$category="TOT_M"  ;s7.fit08$lag=names(s7.w2)[24:38]
s7.fit09<-as.data.frame(do.call(rbind,fit09));s7.fit09$category="TOT_F"  ;s7.fit09$lag=names(s7.w2)[24:38]

s7.fit<-rbind(s7.fit01,s7.fit02,s7.fit03,
              s7.fit04,s7.fit05,s7.fit06,
              s7.fit07,s7.fit08,s7.fit09)
##################################################################################################################################################

s1.fit$SIDO="Seoul"  ;s1.fit$group=gsub("Seoul_",""  ,s1.fit$category);s1.fit$`Pr(>|z|)`=ifelse(s1.fit$`Pr(>|z|)`<0.001,"<0.001",round(s1.fit$`Pr(>|z|)`,3))
s2.fit$SIDO="Busan"  ;s2.fit$group=gsub("Busan_",""  ,s2.fit$category);s2.fit$`Pr(>|z|)`=ifelse(s2.fit$`Pr(>|z|)`<0.001,"<0.001",round(s2.fit$`Pr(>|z|)`,3))
s3.fit$SIDO="Daegu"  ;s3.fit$group=gsub("Daegu_",""  ,s3.fit$category);s3.fit$`Pr(>|z|)`=ifelse(s3.fit$`Pr(>|z|)`<0.001,"<0.001",round(s3.fit$`Pr(>|z|)`,3))
s4.fit$SIDO="Incheon";s4.fit$group=gsub("Incheon_","",s4.fit$category);s4.fit$`Pr(>|z|)`=ifelse(s4.fit$`Pr(>|z|)`<0.001,"<0.001",round(s4.fit$`Pr(>|z|)`,3))
s5.fit$SIDO="Gwangju";s5.fit$group=gsub("Gwangju_","",s5.fit$category);s5.fit$`Pr(>|z|)`=ifelse(s5.fit$`Pr(>|z|)`<0.001,"<0.001",round(s5.fit$`Pr(>|z|)`,3))
s6.fit$SIDO="Daejeon";s6.fit$group=gsub("Daejeon_","",s6.fit$category);s6.fit$`Pr(>|z|)`=ifelse(s6.fit$`Pr(>|z|)`<0.001,"<0.001",round(s6.fit$`Pr(>|z|)`,3))
s7.fit$SIDO="Ulsan"  ;s7.fit$group=gsub("Ulsan_",""  ,s7.fit$category);s7.fit$`Pr(>|z|)`=ifelse(s7.fit$`Pr(>|z|)`<0.001,"<0.001",round(s7.fit$`Pr(>|z|)`,3))

s1.fit$gubun=c("single lag",rep("single lag",each=7),rep("moving average",each=7))
s2.fit$gubun=c("single lag",rep("single lag",each=7),rep("moving average",each=7))
s3.fit$gubun=c("single lag",rep("single lag",each=7),rep("moving average",each=7))
s4.fit$gubun=c("single lag",rep("single lag",each=7),rep("moving average",each=7))
s5.fit$gubun=c("single lag",rep("single lag",each=7),rep("moving average",each=7))
s6.fit$gubun=c("single lag",rep("single lag",each=7),rep("moving average",each=7))
s7.fit$gubun=c("single lag",rep("single lag",each=7),rep("moving average",each=7))

s1.fit$RR=exp(s1.fit$Estimate);s1.fit$lci=exp(s1.fit$Estimate-1.96*s1.fit$`Std. Error`);s1.fit$uci=exp(s1.fit$Estimate+1.96*s1.fit$`Std. Error`)
s2.fit$RR=exp(s2.fit$Estimate);s2.fit$lci=exp(s2.fit$Estimate-1.96*s2.fit$`Std. Error`);s2.fit$uci=exp(s2.fit$Estimate+1.96*s2.fit$`Std. Error`)
s3.fit$RR=exp(s3.fit$Estimate);s3.fit$lci=exp(s3.fit$Estimate-1.96*s3.fit$`Std. Error`);s3.fit$uci=exp(s3.fit$Estimate+1.96*s3.fit$`Std. Error`)
s4.fit$RR=exp(s4.fit$Estimate);s4.fit$lci=exp(s4.fit$Estimate-1.96*s4.fit$`Std. Error`);s4.fit$uci=exp(s4.fit$Estimate+1.96*s4.fit$`Std. Error`)
s5.fit$RR=exp(s5.fit$Estimate);s5.fit$lci=exp(s5.fit$Estimate-1.96*s5.fit$`Std. Error`);s5.fit$uci=exp(s5.fit$Estimate+1.96*s5.fit$`Std. Error`)
s6.fit$RR=exp(s6.fit$Estimate);s6.fit$lci=exp(s6.fit$Estimate-1.96*s6.fit$`Std. Error`);s6.fit$uci=exp(s6.fit$Estimate+1.96*s6.fit$`Std. Error`)
s7.fit$RR=exp(s7.fit$Estimate);s7.fit$lci=exp(s7.fit$Estimate-1.96*s7.fit$`Std. Error`);s7.fit$uci=exp(s7.fit$Estimate+1.96*s7.fit$`Std. Error`)

s1.fit_single<-subset(s1.fit,gubun=="single lag"); s1.fit_moving<-subset(s1.fit,gubun=="moving average")
s2.fit_single<-subset(s2.fit,gubun=="single lag"); s2.fit_moving<-subset(s2.fit,gubun=="moving average")
s3.fit_single<-subset(s3.fit,gubun=="single lag"); s3.fit_moving<-subset(s3.fit,gubun=="moving average")
s4.fit_single<-subset(s4.fit,gubun=="single lag"); s4.fit_moving<-subset(s4.fit,gubun=="moving average")
s5.fit_single<-subset(s5.fit,gubun=="single lag"); s5.fit_moving<-subset(s5.fit,gubun=="moving average")
s6.fit_single<-subset(s6.fit,gubun=="single lag"); s6.fit_moving<-subset(s6.fit,gubun=="moving average")
s7.fit_single<-subset(s7.fit,gubun=="single lag"); s7.fit_moving<-subset(s7.fit,gubun=="moving average")

fit_single<-rbind(s1.fit_single,s2.fit_single,
                  s3.fit_single,s4.fit_single,
                  s5.fit_single,s6.fit_single,s7.fit_single)

fit_moving<-rbind(s1.fit_moving,s2.fit_moving,
                  s3.fit_moving,s4.fit_moving,
                  s5.fit_moving,s6.fit_moving,s7.fit_moving)

fit_single$SIDO<-factor(fit_single$SIDO,levels=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"))
fit_moving$SIDO<-factor(fit_moving$SIDO,levels=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"))

fit_single$obs=0:7
fit_moving$obs=1:7
setwd("C:\\Users\\AROOM206_kard141\\Desktop\\AOM\\분석\\결과")

# write.csv(fit_single,file="fit_single_w2.csv",row.names=F)
# write.csv(fit_moving,file="fit_moving_w2.csv",row.names=F)
#####################################################################################################################################################################
#####################################################################################################################################################################
#age distribution
ag1<-as.data.frame(read_excel("age.xlsx",sheet=1))
ag2<-as.data.frame(read_excel("age.xlsx",sheet=2))

x11();ggplot(ag1,aes(AGE,y=count))+geom_bar(stat="identity")+labs(x="Age",y="Frequency")+theme_bw(base_size=20)

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\age_distribution_total.tiff",width=3600,height=2800,res=300)
ggplot(ag1,aes(AGE,y=count))+geom_bar(stat="identity")+labs(x="Age",y="Frequency")+theme_bw(base_size=20)
dev.off()

#####################################################################################################################################################################
#####################################################################################################################################################################
#df for Time-trend
#Base Model fit 결정하기위해 자유도 조합하여 모델링
#summary table 만들기
kk=c(11,12,13,14,15,16,17,18,
     21,22,23,24,25,26,27,28,
     31,32,33,34,35,36,37,38,
     41,42,43,44,45,46,47,48,
     51,52,53,54,55,56,57,58,
     61,62,63,64,65,66,67,68,
     71,72,73,74,75,76,77,78,
     81,82,83,84,85,86,87,88,
     91,92,93,94,95,96,97,98)

gam_df=function(fit){data.frame(Y=as.character(fit$formula)[2],family=fit$family[[1]],link=fit$family[[2]],as.data.frame(t(summary(fit)$p.table[2,])),
                                AIC=AIC(fit),BIC=BIC(fit),df_time=substr(kk,1,1)[i],df_AT=substr(kk,2,2)[i])}

#degree of freedom 결정하기 위해 이용 
s1.fit01=NULL ;s1.fit02=NULL;s1.fit03=NULL ;s1.fit04=NULL
s1.fit05=NULL ;s1.fit06=NULL;s1.fit07=NULL

#연령 그룹별 시도별 자유도 조합 결과 확인
for(i in 1:length(kk)){
  fit01<-gam(TOT ~pm+s(as.numeric(date),k=9*as.numeric(substr(kk,1,1)[i]))+s(AT,k=as.numeric(substr(kk,2,2)[i]))+dowholi,family="poisson",data=s1.w4)
  fit02<-gam(TOT ~pm+s(as.numeric(date),k=9*as.numeric(substr(kk,1,1)[i]))+s(AT,k=as.numeric(substr(kk,2,2)[i]))+dowholi,family="poisson",data=s2.w4)
  fit03<-gam(TOT ~pm+s(as.numeric(date),k=9*as.numeric(substr(kk,1,1)[i]))+s(AT,k=as.numeric(substr(kk,2,2)[i]))+dowholi,family="poisson",data=s3.w4)
  fit04<-gam(TOT ~pm+s(as.numeric(date),k=9*as.numeric(substr(kk,1,1)[i]))+s(AT,k=as.numeric(substr(kk,2,2)[i]))+dowholi,family="poisson",data=s4.w4)
  fit05<-gam(TOT ~pm+s(as.numeric(date),k=9*as.numeric(substr(kk,1,1)[i]))+s(AT,k=as.numeric(substr(kk,2,2)[i]))+dowholi,family="poisson",data=s5.w4)
  fit06<-gam(TOT ~pm+s(as.numeric(date),k=9*as.numeric(substr(kk,1,1)[i]))+s(AT,k=as.numeric(substr(kk,2,2)[i]))+dowholi,family="poisson",data=s6.w4)
  fit07<-gam(TOT ~pm+s(as.numeric(date),k=9*as.numeric(substr(kk,1,1)[i]))+s(AT,k=as.numeric(substr(kk,2,2)[i]))+dowholi,family="poisson",data=s7.w4)
  
  s1.fit01[[i]]<-gam_df(fit01) ; s1.fit02[[i]]<-gam_df(fit02) 
  s1.fit03[[i]]<-gam_df(fit03) ; s1.fit04[[i]]<-gam_df(fit04) 
  s1.fit05[[i]]<-gam_df(fit05) ; s1.fit06[[i]]<-gam_df(fit06) 
  s1.fit07[[i]]<-gam_df(fit07) 
  
  print(i)}

fit01<-do.call(rbind,s1.fit01) ; fit02<-do.call(rbind,s1.fit02)
fit03<-do.call(rbind,s1.fit03) ; fit04<-do.call(rbind,s1.fit04)
fit05<-do.call(rbind,s1.fit05) ; fit06<-do.call(rbind,s1.fit06) ;fit07<-do.call(rbind,s1.fit07)

fit01$SIDO="Seoul"  ;fit02$SIDO="Busan"
fit03$SIDO="Daegu"  ;fit04$SIDO="Incheon"
fit05$SIDO="Gwangju";fit06$SIDO="Daejeon"
fit07$SIDO="Ulsan"

aom.df<-rbind(fit01,fit02,fit03,fit04,fit05,fit06,fit07)
aom.df$df_time=as.numeric(as.character(aom.df$df_time))

aom.df$RR =with(aom.df,round(exp(Estimate),4))
aom.df$lwl=with(aom.df,round(exp(Estimate-1.96*`Std..Error`),4))
aom.df$uwl=with(aom.df,round(exp(Estimate+1.96*`Std..Error`),4))

setwd("C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\analysis\\data")
# write.csv(aom.df,file="aom.df.csv",row.names=F)
aom.df<-read.csv("aom.df.csv",header=T)

df<-subset(aom.df,df_AT==6)
df$SIDO=factor(df$SIDO,levels=unique(df$SIDO))
x11();ggplot(df,aes(df_time,Estimate))+geom_point(size=3,col="red")+geom_errorbar(aes(ymin=df$Estimate-1.96*df$Std..Error),
                                                                                  ymax=df$Estimate+1.96*df$Std..Error,width=0.4,size=0.85)+
  labs(x="Degree of freedom for time per year",y="log RR")+theme_gray(base_size=20)+facet_wrap(~SIDO)+
  scale_y_continuous(limits=c(-0.0075,0.03))+scale_x_continuous(breaks = c(1:9))+geom_hline(yintercept =0,col="blue",linetype=2,size=1.1)

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\time_df.tiff",width=2800,height=2800,res=300)
ggplot(df,aes(df_time,Estimate))+geom_point(size=3,col="red")+geom_errorbar(aes(ymin=df$Estimate-1.96*df$Std..Error),                                                                            ymax=df$Estimate+1.96*df$Std..Error,width=0.4,size=0.85)+
  labs(x="Degree of freedom for time per year",y="log RR")+theme_gray(base_size=20)+facet_wrap(~SIDO)+
  scale_y_continuous(limits=c(-0.0075,0.03))+scale_x_continuous(breaks = c(1:9))+geom_hline(yintercept =0,col="blue",linetype=2,size=1.1)
dev.off()

#####################################################################################################################################################################
#####################################################################################################################################################################
tp<-cbind(aggregate(aom_w4$TOT,list(aom_w4$DATE),sum),PM=aggregate(aom_w4$pm,list(aom_w4$DATE),mean)[,2]*10)
names(tp)[1]="date"

library(gridExtra)
x11();grid.arrange(ggplot(tp,aes(date,x))+geom_line()+theme_bw(base_size=15)+geom_hline(yintercept=median(tp$x,na.rm=T),col="red",size=1)+
                     labs(x="Date",y="Acute otitis media counts"),
                   ggplot(tp,aes(date,PM))+theme_bw(base_size=15)+geom_line()+geom_hline(yintercept=median(tp$PM,na.rm=T),col="blue",size=1)+
                     labs(x="Date",y=expression(paste(PM[2.5]," (",mu,g/m^3,")"))))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\time_plot.tiff",width=3600,height=2800,res=300)
grid.arrange(ggplot(tp,aes(date,x))+geom_line()+theme_bw(base_size=15)+geom_hline(yintercept=median(tp$x,na.rm=T),col="red",size=1)+
               labs(x="Date",y="Acute otitis media counts"),
             ggplot(tp,aes(date,PM))+theme_bw(base_size=15)+geom_line()+geom_hline(yintercept=median(tp$PM,na.rm=T),col="blue",size=1)+
               labs(x="Date",y=expression(paste(PM[2.5]," (",mu,g/m^3,")"))))
dev.off()

#####################################################################################################################################################################
#####################################################################################################################################################################
#exposure-response gamm4


#####################################################################################################################################################################
#####################################################################################################################################################################
#meta analysis : Total 
setwd("C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_AOM\\analysis")
moving<-read.csv("moving.csv",header = T)

setwd("C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_AOM\\analysis\\result")
warm<-read.csv("fit_moving_w4.warm_quasipoisson_revision.csv",header=T)
cold<-read.csv("fit_moving_w4.cold_quasipoisson_revision.csv",header=T)

m01<-subset(moving,category=="TOT" & lag=="lag04" & Episode==4)
m02<-subset(moving,category=="TOT_M" & lag=="lag04" & Episode==4)
m03<-subset(moving,category=="TOT_F" & lag=="lag04" & Episode==4)
m04<-subset(moving,category=="AGE0" & lag=="lag04" & Episode==4)
m05<-subset(moving,category=="AGE13" & lag=="lag04" & Episode==4)
m06<-subset(moving,category=="AGE0_M" & lag=="lag04" & Episode==4)
m07<-subset(moving,category=="AGE0_F" & lag=="lag04" & Episode==4)
m08<-subset(moving,category=="AGE13_M" & lag=="lag04" & Episode==4)
m09<-subset(moving,category=="AGE13_F" & lag=="lag04" & Episode==4)
m10<-subset(warm,  category=="TOT" & lag=="lag04")
m11<-subset(cold,  category=="TOT" & lag=="lag04")
m12<-subset(warm,  category=="TOT_M" & lag=="lag04")
m13<-subset(cold,  category=="TOT_M" & lag=="lag04")
m14<-subset(warm,  category=="TOT_F" & lag=="lag04")
m15<-subset(cold,  category=="TOT_F" & lag=="lag04")

with(m01,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))

uni01 <- with(m01,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni02 <- with(m02,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni03 <- with(m03,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni04 <- with(m04,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni05 <- with(m05,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni06 <- with(m06,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni07 <- with(m07,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni08 <- with(m08,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni09 <- with(m09,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni10 <- with(m10,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni11 <- with(m11,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni12 <- with(m12,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni13 <- with(m13,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni14 <- with(m14,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))
uni15 <- with(m15,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5))

uni02 <- with(m02,rma(yi=Estimate, sei=Std..Error, subset=(m02$lag=="lag04"),digits=5))

par(mar=c(4,4,1,2))
x11();
forest(uni01,xlim=c(0.97,1.05), ylim=c(-1, 55), transf=exp,rows=c(7:1),
       refline=1, bg=4, col=2,cex.lab=1.1,cex.axis=1.1,cex=1.1,fontsize=1,digits=3,main="",psize=1,mlab="RE Model for Total");
text(0.985, -1, pos=4, cex=1.1, bquote(paste("(Q = ",.(formatC(uni01$QE, digits=2, format="f")), ", df = ", .(uni01$k - uni01$p),
                                             ", p = ", .(formatC(uni01$QEp, digits=3, format="f")), ", ", I^2, " = ",.(formatC(uni01$I2, digits=2, format="f")), "%)")))
text(0.97, 8.5, pos=4, cex=1.1, paste("Total"))
text(0.97, 55, pos=4, cex=1.1, paste("Cities"))
text(1.042,55, pos=4, cex=1.1, paste("Relative Risk [95% CI]"))

text(0.97, 27.5, pos=4, cex=1.1, paste("Sex: Male"))
text(0.985, 20, pos=4, cex=1.1, bquote(paste("(Q = ",.(formatC(uni02$QE, digits=2, format="f")), ", df = ", .(uni02$k - uni02$p),
                                             ", p = ", .(formatC(uni02$QEp, digits=3, format="f")), ", ", I^2, " = ",.(formatC(uni02$I2, digits=2, format="f")), "%)")))

x11();forest(uni01, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="Male")
x11();forest(uni02, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="Male")
x11();forest(uni03, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="Female")
x11();forest(uni04, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="0 years old")
x11();forest(uni05, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="1-3 years old")
x11();forest(uni06, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="0 yeras old: Male")
x11();forest(uni07, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="0 years old: Female")
x11();forest(uni08, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="1-3 years old: Male")
x11();forest(uni09, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="1-3 years old: Female")
x11();forest(uni10, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="Warm: Total")
x11();forest(uni11, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="Cold: Total")
x11();forest(uni12, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="Warm: Male")
x11();forest(uni13, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="Cold: Male")
x11();forest(uni14, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="Cold: Female")
x11();forest(uni15, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="Cold: Female")

tiff(filename="C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_AOM\\figure\\MEta_total.tiff",width=3600,height=2800,res=300)
forest(uni01, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3,main="",psize=1);
text(0.967, -1.45, pos=4, cex=1.2, bquote(paste("(Q = ",.(formatC(uni01$QE, digits=2, format="f")), ", df = ", .(uni01$k - uni01$p),
                                                ", p-value = ", .(formatC(uni01$QEp, digits=3, format="f")), ", ", I^2, " = ",.(formatC(uni01$I2, digits=2, format="f")), "%)")))
text(0.967, 8.5, pos=4, cex=1.8, paste("Cities"))
text(1.032, 8.5, pos=4, cex=1.8, paste("Relative Risk [95% CI]"))
dev.off()

m01$SIDO=paste0(m01$SIDO);m01$SIDO=factor(m01$SIDO,levels=unique(m01$SIDO))
m02$SIDO=paste0(m02$SIDO," ");m02$SIDO=factor(m02$SIDO,levels=unique(m02$SIDO))
m03$SIDO=paste0(m03$SIDO,"  ");m03$SIDO=factor(m03$SIDO,levels=unique(m03$SIDO))
m10$SIDO=paste0(m10$SIDO,"   ");m10$SIDO=factor(m10$SIDO,levels=unique(m10$SIDO)) ;m10$Episode=as.integer(4);names(m10)[3]="z.value";names(m10)[4]="Pr...z.."
m11$SIDO=paste0(m11$SIDO,"    ");m11$SIDO=factor(m11$SIDO,levels=unique(m11$SIDO));m11$Episode=as.integer(4);names(m11)[3]="z.value";names(m11)[4]="Pr...z.."

head(m03)
head(m11)
str(m01)
str(m11)

m<-rbind(m01,m02,m03,m10,m11)
RM.res<-rma(yi=m$Estimate,sei=m$Std..Error,data=dat,slab=m$SIDO)
x11();par(mar=c(4,4,1,2));
forest(RM.res, xlim=c(0.8, 1.3), ylim=c(5, 69), steps=9, rows=c(65:59,53:47,41:35,29:23,17:11),refline=1,
       order=c(1:nrow(m)),transf = exp,bg=4,col=2,digits=3,cex=0.85,xlab="", mlab="")
addpoly(uni01,transf=exp,cex=0.9,row=56,col="red",digits=3,mlab="RE Model for Subgroup: Total")
addpoly(uni02,transf=exp,cex=0.9,row=44,col="red",digits=3,mlab="RE Model for Subgroup: Male")
addpoly(uni03,transf=exp,cex=0.9,row=32,col="red",digits=3,mlab="RE Model for Subgroup: Female")
addpoly(uni10,transf=exp,cex=0.9,row=20,col="red",digits=3,mlab="RE Model for Subgroup: Warm season")
addpoly(uni11,transf=exp,cex=0.9,row=08,col="red",digits=3,mlab="RE Model for Subgroup: Cold season")

text(0.8,66.3, pos=4, cex=1.1, paste("Total"),col="blue",font=4)
text(0.8,54.3, pos=4, cex=1.1, paste("Male"),col="blue",font=4)
text(0.8,42.3, pos=4, cex=1.1, paste("Female"),col="blue",font=4)
text(0.8,30.3, pos=4, cex=1.1, paste("Warm season"),col="blue",font=4)
text(0.8,18.3, pos=4, cex=1.1, paste("Cold season"),col="blue",font=4)
text(0.8,68, pos=4, cex=1.1, paste("Cities"),font=4)
text(1.25,68, pos=4, cex=1.1, paste("Relative Risk [95% CI]"),font=4)


#####################################################################################################################################################################


#####################################################################################################################################################################
#####################################################################################################################################################################
#two stage Meta analysis using DLNM
dat<-aom_w4
regions<-as.character(unique(dat$SIDO))
dat$region=NA
dat$exposure=dat$pm*10

datalist <- lapply(regions, function(region) dat[dat$SIDO==region,])
names(datalist) <- regions
coef <- matrix(NA,length(datalist), 4, dimnames=list(regions,paste0("b",seq(4))))
S <- vector("list",length(datalist))
names(S) <- regions
bound <- rowMeans(sapply(datalist, function(x) range(x$exposure)))
varknots <- equalknots(bound, fun="bs", degree=2, df=4)
lagknots <- logknots(7, df=3, int=T)
argvar <- list(fun="bs", degree=2, knots=varknots, Bound=bound)
arglag <- list(fun="ns", knots=lagknots)

for(i in seq(datalist)) {
  cb <- crossbasis(datalist[[i]]$exposure, lag=7, argvar=argvar, arglag=arglag)
  m <- gam(TOT ~ cb + s(as.numeric(DATE),k=6*9)+s(AT,k=6) + dow, family="quasipoisson",data=datalist[[i]])
  cr <- crossreduce(cb,m,value=7,cen=15)
  coef[i,] <- coef(cr)
  S[[i]] <- vcov(cr)
  print(i)}

mix <- mixmeta(coef~1, S, method="ml")
print(summary(mix), digits=3)
xvar <- seq(bound[1], bound[2], by=0.1)
bvar <- do.call("onebasis", c(list(x=xvar), argvar))

predpool <- crosspred(bvar, coef=coef(mix), vcov=vcov(mix), model.link="log",by=0.1,cen=15)
predreg <- lapply(seq(nrow(coef)),function(i) crosspred(bvar, coef=coef[i,],vcov=S[[i]], model.link="log",cen=15))

x11();plot(predpool, type="l", ci=, ylab="Relative Risk for Acute otitis media", ylim=c(.8,1.5), lwd=2,col="blue",
           xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)), main="Pooled and first-stage",cex.lab=1.35,cex.axis=1.35)
col1 <- do.call(rgb,c(as.list(col2rgb(4)), alpha=255/6 ,max=255))

p1<-predpool
p2<-predpool

x11();par(mfrow=c(1,2))
plot(p1, type="l", ci=, ylab="Relative Risk", ylim=c(.8,1.5), lwd=2,col="red",
     xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)), main="Pooled meta-analysis",cex.lab=1.35,cex.axis=1.35)+abline(v=15,lwd=1.2,lty=2)
text(42,1.4,label=expression(paste("Reference=",15,mu,g/m^3)),cex=1.3)
plot(p2, type="l", ci=, ylab="Relative Risk", ylim=c(.8,1.5), lwd=2,col="blue",
     xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)), main="Pooled meta-analysis",cex.lab=1.35,cex.axis=1.35)+abline(v=35,lwd=1.2,lty=2)
text(62,1.4,label=expression(paste("Reference=",35,mu,g/m^3)),cex=1.3)

for(i in seq(datalist)) lines(predreg[[i]], col=col1)

x11();plot(predreg[[1]], type="l", ci=, ylab="Relative Risk for ALRI hospital admission", ylim=c(.8,1.5), lwd=2,col="red",
           xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)), main="Pooled and first-stage",cex.lab=1.35,cex.axis=1.35)

cb1 <- crossbasis(datalist[[1]]$exposure, lag=30, argvar=argvar, arglag=arglag)
cb2 <- crossbasis(datalist[[2]]$exposure, lag=7, argvar=argvar, arglag=arglag)
cb3 <- crossbasis(datalist[[3]]$exposure, lag=7, argvar=argvar, arglag=arglag)
cb4 <- crossbasis(datalist[[4]]$exposure, lag=7, argvar=argvar, arglag=arglag)
cb5 <- crossbasis(datalist[[5]]$exposure, lag=7, argvar=argvar, arglag=arglag)
cb6 <- crossbasis(datalist[[6]]$exposure, lag=7, argvar=argvar, arglag=arglag)
cb7 <- crossbasis(datalist[[7]]$exposure, lag=7, argvar=argvar, arglag=arglag)

m1 <- gam(TOT ~ cb1 + s(as.numeric(DATE),k=6*9)+s(AT,k=6) + dow, family="quasipoisson",data=datalist[[1]])
m2 <- gam(TOT ~ cb2 + s(as.numeric(DATE),k=6*9)+s(AT,k=6) + dow, family="quasipoisson",data=datalist[[2]])
m3 <- gam(TOT ~ cb3 + s(as.numeric(DATE),k=6*9)+s(AT,k=6) + dow, family="quasipoisson",data=datalist[[3]])
m4 <- gam(TOT ~ cb4 + s(as.numeric(DATE),k=6*9)+s(AT,k=6) + dow, family="quasipoisson",data=datalist[[4]])
m5 <- gam(TOT ~ cb5 + s(as.numeric(DATE),k=6*9)+s(AT,k=6) + dow, family="quasipoisson",data=datalist[[5]])
m6 <- gam(TOT ~ cb6 + s(as.numeric(DATE),k=6*9)+s(AT,k=6) + dow, family="quasipoisson",data=datalist[[6]])
m7 <- gam(TOT ~ cb7 + s(as.numeric(DATE),k=6*9)+s(AT,k=6) + dow, family="quasipoisson",data=datalist[[7]])
x11();par(mfrow=c(2,4))
plot(crosspred(cb1,m1),xlab="PM2.5",zlab="RR",shade=0.6,main="Seoul")
plot(crosspred(cb2,m2),xlab="PM2.5",zlab="RR",shade=0.6,main="Busan")
plot(crosspred(cb3,m3),xlab="PM2.5",zlab="RR",shade=0.6,main="Daegu")
plot(crosspred(cb4,m4),xlab="PM2.5",zlab="RR",shade=0.6,main="Incheon")
plot(crosspred(cb5,m5),xlab="PM2.5",zlab="RR",shade=0.6,main="Gwangju")
plot(crosspred(cb6,m6),xlab="PM2.5",zlab="RR",shade=0.6,main="Daejeon")
plot(crosspred(cb7,m7),xlab="PM2.5",zlab="RR",shade=0.6,main="Ulsan")

x11();plot(crosspred(cb1,m1,cen=15),xlab="PM2.5",zlab="RR",shade=0.6,main="Seoul")
####################################################
####BLUP로 추정#####################################
####################################################

aom_w4$pp=aom_w4$pm*10
x11();hist(aom_w4$pp,20)

#####################################################################################################################################################################
#####################################################################################################################################################################
#DLNMs (분산 지연 비선형모델)
#DLM에서의 제한 사항을 유연하게 확장 시킴
# install.packages("dlnm")
s1<-subset(dat,SIDO==11)
arglagcbl.pm  <-crossbasis(s1$exposure,lag=7,argvar=list(fun="lin"),arglag=list(fun="ns"))
cbl.temp<-crossbasis(s1$AT,lag=6,argvar=list(df=6),arglag=list(fun="strata",breaks=1))

model1<-gam(TOT~cbl.pm+cbl.temp+s(as.integer(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=s1)
pred1.pm<-crosspred(cbl.pm,model1,at=0:7,bylag=0.1,cumul=TRUE) #bylag는 지연간격을 설정하여 부드러운 곡선을 그리기위해 

exp(pred1.pm$matfit)

pred1.pm$allRRfit #7일 간의 지연에 대한 PM2.5 10단위 증가와 관련된 전체 누적 연관성
pred1.pm$allRRhigh
pred1.pm$allRRlow

pred1.pm$allRRfit["10"]
cbind(pred1.pm$allRRlow,pred1.pm$allRRhigh)["10",]
pred1.pm$cumRRfit
pred1.pm$cumRRlow
pred1.pm$cumRRhigh

data.frame(allRR=pred1.pm$allRRfit,
           allRRlow=pred1.pm$allRRlow,
           allRRhigh=pred1.pm$allRRhigh)

x11();plot(pred1.pm, "slices", var=7, col="blue", ylab="RR", ci.arg=list(density=10,lwd=2,col="grey60"),cex.axis=1.4,cex.lab=1.4,lwd=2,cex.main=2,
           main="",ylim=c(0.98,1.03))

#dlnm plotting tiff
tiff(filename="C:\\Users\\a\\Desktop\\논문쓸거\\미세먼지_AOM\\figure\\dlnm_effect.tiff",width=6000,height=4000,res=600)
plot(pred1.pm, "slices", var=10, col="blue", ylab="RR", ci.arg=list(density=10,lwd=2,col="grey60"),cex.axis=1.4,cex.lab=1.4,lwd=2,cex.main=2,
     main="",ylim=c(0.98,1.03))
par(new=TRUE)
plotCI(x=0:7, y=single_effect_cov2$RR, li=single_effect_cov2$RR_low, ui=single_effect_cov2$RR_high,
       xlab="",xaxt="n", ylab=NA,lwd=2,pch=16,labels = NA,cex=1.4,cex.axis=1.4,cex.lab=1.4,cex.main=2,ylim=c(0.98,1.03));abline(h=1,col="red",lwd=2)
legend(4.13,0.985,legend=c("Single lag effect","Distributed lag non-linear model"),col=c("black","blue"),lty=c(1,1),lwd=1.5,text.font = 1.4,cex=1.2)
dev.off()


#####################################################################################################################################################################
#####################################################################################################################################################################
fit1<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson" ,data=s1.w4)
fit2<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson" ,data=s2.w4)
fit3<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson" ,data=s3.w4)
fit4<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson" ,data=s4.w4)
fit5<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson" ,data=s5.w4)
fit6<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson" ,data=s6.w4)
fit7<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson" ,data=s7.w4)

df1=data.frame(res=fit1$residuals,SIDO="Seoul")
df2=data.frame(res=fit2$residuals,SIDO="Busan")
df3=data.frame(res=fit3$residuals,SIDO="Daegu")
df4=data.frame(res=fit4$residuals,SIDO="Incheon")
df5=data.frame(res=fit5$residuals,SIDO="Gwangju")
df6=data.frame(res=fit6$residuals,SIDO="Daejeon")
df7=data.frame(res=fit7$residuals,SIDO="Ulsan")

res<-rbind(df1,df2,df3,df4,df5,df6,df7)
res$obs=c(c(1:nrow(df1)),c(1:nrow(df2)),c(1:nrow(df3)),c(1:nrow(df4)),c(1:nrow(df5)),c(1:nrow(df6)),c(1:nrow(df7)))

x11();grid.arrange(ggplot(res,aes(obs,res))+geom_line()+facet_wrap(~SIDO)+theme_gray(base_size=15)+labs(y="Residuals",x="Time"),
                   ggplot(res,aes(res))+geom_histogram()+xlim(-1,1)+theme_gray(base_size=15)+facet_wrap(~SIDO)+labs(x="Residuals"),ncol=2)

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\residuals.tiff",width=4000,height=2800,res=300)
grid.arrange(ggplot(res,aes(obs,res))+geom_line()+facet_wrap(~SIDO)+theme_gray(base_size=15)+labs(y="Residuals",x="Time"),
             ggplot(res,aes(res))+geom_histogram()+xlim(-1,1)+theme_gray(base_size=15)+facet_wrap(~SIDO)+labs(x="Residuals"),ncol=2)
dev.off()

Seoul  =data.frame(Residuals=fit1$residuals,SIDO="Seoul")
Busan  =data.frame(Residuals=fit2$residuals,SIDO="Busan")
Daegu  =data.frame(Residuals=fit3$residuals,SIDO="Daegu")
Incheon=data.frame(Residuals=fit4$residuals,SIDO="Incheon")
Gwangju=data.frame(Residuals=fit5$residuals,SIDO="Gwangju")
Daejeon=data.frame(Residuals=fit6$residuals,SIDO="Daejeon")
Ulsan  =data.frame(Residuals=fit7$residuals,SIDO="Ulsan")

x11();grid.arrange(ggPacf(Seoul$Residuals),
                   ggPacf(Busan$Residuals),
                   ggPacf(Daegu$Residuals),
                   ggPacf(Incheon$Residuals),
                   ggPacf(Gwangju$Residuals),
                   ggPacf(Daejeon$Residuals),
                   ggPacf(Ulsan$Residuals))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\residuals2.tiff",width=3600,height=2800,res=300)
grid.arrange(ggPacf(Seoul$Residuals),
             ggPacf(Busan$Residuals),
             ggPacf(Daegu$Residuals),
             ggPacf(Incheon$Residuals),
             ggPacf(Gwangju$Residuals),
             ggPacf(Daejeon$Residuals),
             ggPacf(Ulsan$Residuals))
dev.off()

#####################################################################################################################################################################
#####################################################################################################################################################################
#Single lag RR plot ##
setwd("C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_AOM\\analysis\\result")
sido.fit<-read.csv("fit_single_w4_quasipoisson_revision.csv",header=T)
sido.fit.w<-read.csv("fit_single_w4.cold_quasipoisson_revision.csv",header=T)
sido.fit.s<-read.csv("fit_single_w4.warm_quasipoisson_revision.csv",header=T)

sido.fit

s0<-subset(sido.fit,category %in% c("TOT","TOT_M","TOT_F") & lag %in% c("lag1","lag2","lag3","lag4"))
s1<-subset(sido.fit.w,category %in% c("TOT") & lag %in% c("lag1","lag2","lag3","lag4"))
s2<-subset(sido.fit.s,category %in% c("TOT") & lag %in% c("lag1","lag2","lag3","lag4"))

s0$Cities=factor(s0$SIDO,levels=unique(s0$SIDO))
s1$Cities=factor(s1$SIDO,levels=unique(s1$SIDO))
s2$Cities=factor(s2$SIDO,levels=unique(s2$SIDO))

s0$category=rep(c("Total","Male","Female"),each=4)
s1$category="Cold season"
s2$category="Warm season"
s<-rbind(s0,s1,s2)

s$category=factor(s$category,levels=c("Total","Male","Female","Cold season","Warm season"))

x11();ggplot(s,aes(lag,RR,group=category))+geom_point(size=3.5,aes(shape=category),position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=lci,ymax=uci),position=position_dodge(width=0.5),width=0.4,size=0.9)+theme_bw(base_size=20)+facet_wrap(~Cities)+
  labs(x="Single Lag",y="Relative Risk")+geom_hline(yintercept = 1,linetype=2,size=1)+theme(legend.position = "top",legend.title=element_blank())

sido.fit<-as.data.frame(read_excel("PM_AOM_table.xlsx",sheet=7))
m<-subset(sido.fit,category=="TOT" & Episode==4)
m$Cities=factor(m$SIDO,levels=unique(m$SIDO))
x11();ggplot(m,aes(lag,RR))+geom_point(size=2.5,position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=lci,ymax=uci),position="dodge",width=0.4,size=0.9)+theme_bw(base_size=18)+facet_wrap(~Cities)+
  labs(x="Moving average",y="Relative Risk")+geom_hline(yintercept = 1,linetype=2,size=1)

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\single_lag.tiff",width=4000,height=2800,res=300)
ggplot(s,aes(lag,RR))+geom_point(size=2.5,position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=lci,ymax=uci),position="dodge",width=0.4,size=0.9)+theme_bw(base_size=15)+facet_wrap(~Cities)+
  labs(x="Single Lag",y="Relative Risk")+geom_hline(yintercept = 1,linetype=2,size=1)
dev.off()

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\moving_lag.tiff",width=4000,height=2800,res=300)
ggplot(m,aes(lag,RR))+geom_point(size=2.5,position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=lci,ymax=uci),position="dodge",width=0.4,size=0.9)+theme_bw(base_size=15)+facet_wrap(~Cities)+
  labs(x="Moving average",y="Relative Risk")+geom_hline(yintercept = 1,linetype=2,size=1)
dev.off()

#####################################################################################################################################################################
#####################################################################################################################################################################
#Sensitivity analyisis
fit01=NULL; fit02=NULL; fit03=NULL;fit04=NULL; fit05=NULL; fit06=NULL;
fit07=NULL; fit08=NULL; fit09=NULL;fit10=NULL; fit11=NULL; fit12=NULL;
fit13=NULL; fit14=NULL; fit15=NULL;fit16=NULL; fit17=NULL; fit18=NULL;fit19=NULL; fit20=NULL; fit21=NULL;

for(i in 1:15){
  fit01[[i]]<-summary(gam(TOT~s1.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="poisson"     ,data=s1.w4))$'p.table'[2,]
  fit02[[i]]<-summary(gam(TOT~s1.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=s1.w4))$'p.table'[2,]
  fit03[[i]]<-summary(gam(TOT~s1.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="nb"          ,data=s1.w4))$'p.table'[2,]
  fit04[[i]]<-summary(gam(TOT~s2.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="poisson"     ,data=s2.w4))$'p.table'[2,]
  fit05[[i]]<-summary(gam(TOT~s2.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=s2.w4))$'p.table'[2,]
  fit06[[i]]<-summary(gam(TOT~s2.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="nb"          ,data=s2.w4))$'p.table'[2,]
  fit07[[i]]<-summary(gam(TOT~s3.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="poisson"     ,data=s3.w4))$'p.table'[2,]
  fit08[[i]]<-summary(gam(TOT~s3.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=s3.w4))$'p.table'[2,]
  fit09[[i]]<-summary(gam(TOT~s3.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="nb"          ,data=s3.w4))$'p.table'[2,]
  fit10[[i]]<-summary(gam(TOT~s4.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="poisson"     ,data=s4.w4))$'p.table'[2,]
  fit11[[i]]<-summary(gam(TOT~s4.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=s4.w4))$'p.table'[2,]
  fit12[[i]]<-summary(gam(TOT~s4.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="nb"          ,data=s4.w4))$'p.table'[2,]
  fit13[[i]]<-summary(gam(TOT~s5.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="poisson"     ,data=s5.w4))$'p.table'[2,]
  fit14[[i]]<-summary(gam(TOT~s5.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=s5.w4))$'p.table'[2,]
  fit15[[i]]<-summary(gam(TOT~s5.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="nb"          ,data=s5.w4))$'p.table'[2,]
  fit16[[i]]<-summary(gam(TOT~s6.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="poisson"     ,data=s6.w4))$'p.table'[2,]
  fit17[[i]]<-summary(gam(TOT~s6.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=s6.w4))$'p.table'[2,]
  fit18[[i]]<-summary(gam(TOT~s6.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="nb"          ,data=s6.w4))$'p.table'[2,]
  fit19[[i]]<-summary(gam(TOT~s7.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="poisson"     ,data=s7.w4))$'p.table'[2,]
  fit20[[i]]<-summary(gam(TOT~s7.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="quasipoisson",data=s7.w4))$'p.table'[2,]
  fit21[[i]]<-summary(gam(TOT~s7.w4[,23+i]+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow,family="nb"          ,data=s7.w4))$'p.table'[2,]; 
  print(i)}

sen.fit01<-as.data.frame(do.call(rbind,fit01)); sen.fit01$lag=names(s1.w4)[24:38];sen.fit01$sido="Seoul"
sen.fit02<-as.data.frame(do.call(rbind,fit02)); sen.fit02$lag=names(s1.w4)[24:38];sen.fit02$sido="Seoul"
sen.fit03<-as.data.frame(do.call(rbind,fit03)); sen.fit03$lag=names(s1.w4)[24:38];sen.fit03$sido="Seoul"
sen.fit04<-as.data.frame(do.call(rbind,fit04)); sen.fit04$lag=names(s1.w4)[24:38];sen.fit04$sido="Busan"
sen.fit05<-as.data.frame(do.call(rbind,fit05)); sen.fit05$lag=names(s1.w4)[24:38];sen.fit05$sido="Busan"
sen.fit06<-as.data.frame(do.call(rbind,fit06)); sen.fit06$lag=names(s1.w4)[24:38];sen.fit06$sido="Busan"
sen.fit07<-as.data.frame(do.call(rbind,fit07)); sen.fit07$lag=names(s1.w4)[24:38];sen.fit07$sido="Daegu"
sen.fit08<-as.data.frame(do.call(rbind,fit08)); sen.fit08$lag=names(s1.w4)[24:38];sen.fit08$sido="Daegu"
sen.fit09<-as.data.frame(do.call(rbind,fit09)); sen.fit09$lag=names(s1.w4)[24:38];sen.fit09$sido="Daegu"
sen.fit10<-as.data.frame(do.call(rbind,fit10)); sen.fit10$lag=names(s1.w4)[24:38];sen.fit10$sido="Incheon"
sen.fit11<-as.data.frame(do.call(rbind,fit11)); sen.fit11$lag=names(s1.w4)[24:38];sen.fit11$sido="Incheon"
sen.fit12<-as.data.frame(do.call(rbind,fit12)); sen.fit12$lag=names(s1.w4)[24:38];sen.fit12$sido="Incheon"
sen.fit13<-as.data.frame(do.call(rbind,fit13)); sen.fit13$lag=names(s1.w4)[24:38];sen.fit13$sido="Gwangju"
sen.fit14<-as.data.frame(do.call(rbind,fit14)); sen.fit14$lag=names(s1.w4)[24:38];sen.fit14$sido="Gwangju"
sen.fit15<-as.data.frame(do.call(rbind,fit15)); sen.fit15$lag=names(s1.w4)[24:38];sen.fit15$sido="Gwangju"
sen.fit16<-as.data.frame(do.call(rbind,fit16)); sen.fit16$lag=names(s1.w4)[24:38];sen.fit16$sido="Daejeon"
sen.fit17<-as.data.frame(do.call(rbind,fit17)); sen.fit17$lag=names(s1.w4)[24:38];sen.fit17$sido="Daejeon"
sen.fit18<-as.data.frame(do.call(rbind,fit18)); sen.fit18$lag=names(s1.w4)[24:38];sen.fit18$sido="Daejeon"
sen.fit19<-as.data.frame(do.call(rbind,fit19)); sen.fit19$lag=names(s1.w4)[24:38];sen.fit19$sido="Ulsan"
sen.fit20<-as.data.frame(do.call(rbind,fit20)); sen.fit20$lag=names(s1.w4)[24:38];sen.fit20$sido="Ulsan"
sen.fit21<-as.data.frame(do.call(rbind,fit21)); sen.fit21$lag=names(s1.w4)[24:38];sen.fit21$sido="Ulsan"

sen_df1<-rbind(sen.fit01,sen.fit04,sen.fit07,sen.fit10,sen.fit13,sen.fit16,sen.fit19)
sen_df2<-rbind(sen.fit02,sen.fit05,sen.fit08,sen.fit11,sen.fit14,sen.fit17,sen.fit20)
sen_df3<-rbind(sen.fit03,sen.fit06,sen.fit09,sen.fit12,sen.fit15,sen.fit18,sen.fit21)
setwd("C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\analysis\\result")
# write.csv(sen_df1,file="sen_df1.csv",row.names=F)
# write.csv(sen_df2,file="sen_df2.csv",row.names=F)
# write.csv(sen_df3,file="sen_df3.csv",row.names=F)

library(readxl) ;library(ggplot2)
library(sqldf)  ;library(dplyr)
library(plyr)   ;library(mgcv)
library(TTR)    ;library(Epi)
library(tsModel);library(lubridate)
library(dlnm)   ;library(metafor)
library(mixmeta);library(plotrix)
setwd("C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\analysis\\data")

aom_w1<-read.csv("AOM_W1.csv",header=T) ; aom_w1$DATE=as.Date(aom_w1$DATE)
aom_w2<-read.csv("AOM_W2.csv",header=T) ; aom_w2$DATE=as.Date(aom_w2$DATE)
aom_w3<-read.csv("AOM_W3.csv",header=T) ; aom_w3$DATE=as.Date(aom_w3$DATE)
aom_w4<-read.csv("AOM_W4.csv",header=T) ; aom_w4$DATE=as.Date(aom_w4$DATE)

aom_w1$key=paste0(aom_w1$SIDO,"-",aom_w1$DATE) ; aom_w1$yyyy=substr(aom_w1$DATE,1,4) 
aom_w2$key=paste0(aom_w2$SIDO,"-",aom_w2$DATE) ; aom_w2$yyyy=substr(aom_w2$DATE,1,4)
aom_w3$key=paste0(aom_w3$SIDO,"-",aom_w3$DATE) ; aom_w3$yyyy=substr(aom_w3$DATE,1,4)
aom_w4$key=paste0(aom_w4$SIDO,"-",aom_w4$DATE) ; aom_w4$yyyy=substr(aom_w4$DATE,1,4)

# names(aom_count_w4)=c("year","sido",names(aom_w4)[5:22])
# 
# setwd("C:\\Users\\AROOM206_kard141\\Desktop\\AOM\\분석\\결과")
# write.csv(aom_count_w4,file="aom_count_w4.csv",row.names=F)
####################################################################################################################################
#CMAQ PM2.5 자료 가져오기 ; 노출자료  
setwd("C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\analysis")
pm<-read.csv("CMAQ_PM2.5_short.csv",header=T)

names(pm)[31]="mtemp" # 일 평균 기온
names(pm)[48]="mwind" # 일 평균 풍속
names(pm)[50]="mdew"  # 일 평균 이슬점 온도
names(pm)[53]="mhum"  # 일 평균 상대 습도
names(pm)[54]="map"   # 일 평균 증기압

# pm<-pm %>%  select(date:mtemp,mwind,mdew,mhum,map)
pm<-pm [,c(1:10,18:24,31,48,50,53,54)]
pm[,3:17]<-pm[,3:17]/10 #PM 10으로 나눠줌, 1단위 증가당 10ug/m3  으로 보기 위해 

#Apparent Temperature 변수 추가
#Compute simple at (schwartz version)
pm$AT=-2.653+(0.994*pm$mtemp)+(0.0153*pm$mdew*pm$mdew)
pm <- subset(pm,SIDO %in% c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"))

###########################################################################################################################################
#merge 
pm$key=ifelse(pm$SIDO=="Seoul",paste0(11,"-",pm$date),
              ifelse(pm$SIDO=="Busan",paste0(26,"-",pm$date),
                     ifelse(pm$SIDO=="Daegu",paste0(27,"-",pm$date),
                            ifelse(pm$SIDO=="Incheon",paste0(28,"-",pm$date),
                                   ifelse(pm$SIDO=="Gwangju",paste0(29,"-",pm$date),
                                          ifelse(pm$SIDO=="Daejeon",paste0(30,"-",pm$date),paste0(31,"-",pm$date)))))))

pm2<-pm[,3:length(pm)]

aom_w1<-merge(aom_w1,pm2,by="key")
aom_w2<-merge(aom_w2,pm2,by="key")
aom_w3<-merge(aom_w3,pm2,by="key")
aom_w4<-merge(aom_w4,pm2,by="key")

dow<-read.csv("dow.csv",header=T)
dow$date=as.Date(dow$date)
dow<-subset(dow,date>="2008-01-01")
names(dow)=c("DATE","dow")

aom_w1<-merge(dow,aom_w1,by="DATE") ; aom_w1$dow=as.factor(aom_w1$dow)
aom_w2<-merge(dow,aom_w2,by="DATE") ; aom_w2$dow=as.factor(aom_w2$dow)
aom_w3<-merge(dow,aom_w3,by="DATE") ; aom_w3$dow=as.factor(aom_w3$dow)
aom_w4<-merge(dow,aom_w4,by="DATE") ; aom_w4$dow=as.factor(aom_w4$dow)

s1.w1<-subset(aom_w1,aom_w1$SIDO==11);s1.w2<-subset(aom_w2,aom_w2$SIDO==11);s1.w3<-subset(aom_w3,aom_w3$SIDO==11);s1.w4<-subset(aom_w4,aom_w4$SIDO==11);
s2.w1<-subset(aom_w1,aom_w1$SIDO==26);s2.w2<-subset(aom_w2,aom_w2$SIDO==26);s2.w3<-subset(aom_w3,aom_w3$SIDO==26);s2.w4<-subset(aom_w4,aom_w4$SIDO==26);
s3.w1<-subset(aom_w1,aom_w1$SIDO==27);s3.w2<-subset(aom_w2,aom_w2$SIDO==27);s3.w3<-subset(aom_w3,aom_w3$SIDO==27);s3.w4<-subset(aom_w4,aom_w4$SIDO==27);
s4.w1<-subset(aom_w1,aom_w1$SIDO==28);s4.w2<-subset(aom_w2,aom_w2$SIDO==28);s4.w3<-subset(aom_w3,aom_w3$SIDO==28);s4.w4<-subset(aom_w4,aom_w4$SIDO==28);
s5.w1<-subset(aom_w1,aom_w1$SIDO==29);s5.w2<-subset(aom_w2,aom_w2$SIDO==29);s5.w3<-subset(aom_w3,aom_w3$SIDO==29);s5.w4<-subset(aom_w4,aom_w4$SIDO==29);
s6.w1<-subset(aom_w1,aom_w1$SIDO==30);s6.w2<-subset(aom_w2,aom_w2$SIDO==30);s6.w3<-subset(aom_w3,aom_w3$SIDO==30);s6.w4<-subset(aom_w4,aom_w4$SIDO==30);
s7.w1<-subset(aom_w1,aom_w1$SIDO==31);s7.w2<-subset(aom_w2,aom_w2$SIDO==31);s7.w3<-subset(aom_w3,aom_w3$SIDO==31);s7.w4<-subset(aom_w4,aom_w4$SIDO==31);

tot<-aom_w4 %>%  select(DATE,SIDO,TOT,TOT_M:TOT_F,lag01:lag07,AT,dow)
tot<-tot[complete.cases(tot),]

tot$exposure01=tot$lag01*10
tot$exposure02=tot$lag02*10
tot$exposure03=tot$lag03*10
tot$exposure04=tot$lag04*10
tot$exposure05=tot$lag05*10
tot$exposure06=tot$lag06*10
tot$exposure07=tot$lag07*10

#modeling 전체시도 고려
t1.m=gamm4(TOT_M~s(exposure01)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG01_M.tiff",width=2800,height=2800,res=300)
plot(t1.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

t2.m=gamm4(TOT_M~s(exposure02)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG02_M.tiff",width=2800,height=2800,res=300)
plot(t2.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

t3.m=gamm4(TOT_M~s(exposure03)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG03_M.tiff",width=2800,height=2800,res=300)
plot(t3.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

t4.m=gamm4(TOT_M~s(exposure04)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG04_M.tiff",width=2800,height=2800,res=300)
plot(t4.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

t5.m=gamm4(TOT_M~s(exposure05)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG05_M.tiff",width=2800,height=2800,res=300)
plot(t5.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

t6.m=gamm4(TOT_M~s(exposure06)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG06_M.tiff",width=2800,height=2800,res=300)
plot(t6.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

t7.m=gamm4(TOT_M~s(exposure07)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG07_M.tiff",width=2800,height=2800,res=300)
plot(t7.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

#modeling 전체시도 고려
t1.f=gamm4(TOT_F~s(exposure01)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG01_F.tiff",width=2800,height=2800,res=300)
plot(t1.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

t2.f=gamm4(TOT_F~s(exposure02)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG02_F.tiff",width=2800,height=2800,res=300)
plot(t2.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

t3.f=gamm4(TOT_F~s(exposure03)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG03_F.tiff",width=2800,height=2800,res=300)
plot(t3.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

t4.f=gamm4(TOT_F~s(exposure04)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG04_F.tiff",width=2800,height=2800,res=300)
plot(t4.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

t5.f=gamm4(TOT_F~s(exposure05)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG05_F.tiff",width=2800,height=2800,res=300)
plot(t5.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

t6.f=gamm4(TOT_F~s(exposure06)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG06_F.tiff",width=2800,height=2800,res=300)
plot(t6.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

t7.f=gamm4(TOT_F~s(exposure07)+s(AT)+s(as.numeric(DATE), k=6*9)+ factor(dow), family=poisson, data=tot, random=~(1|SIDO))

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG07_F.tiff",width=2800,height=2800,res=300)
plot(t7.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()
#################################################################################################################################################################
#################################################################################################################################################################
tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG01_sex.tiff",width=2800,height=2800,res=300)
par(mfrow=c(1,2));
plot(t1.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
plot(t1.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG02_sex.tiff",width=2800,height=2800,res=300)
par(mfrow=c(1,2));
plot(t2.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
plot(t2.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG03_sex.tiff",width=2800,height=2800,res=300)
par(mfrow=c(1,2));
plot(t3.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
plot(t3.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG04_sex.tiff",width=2800,height=2800,res=300)
par(mfrow=c(1,2));
plot(t4.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
plot(t4.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG05_sex.tiff",width=2800,height=2800,res=300)
par(mfrow=c(1,2));
plot(t5.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
plot(t5.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG06_sex.tiff",width=2800,height=2800,res=300)
par(mfrow=c(1,2));
plot(t6.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
plot(t6.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\GAMM_LAG07_sex.tiff",width=2800,height=2800,res=300)
par(mfrow=c(1,2));
plot(t7.m$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
plot(t7.f$gam,select=1,scheme=1,cex.lab=1.5,cex.axis=1.45,ylim=c(-0.4,0.4),ylab="Log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")),main="")
dev.off()

g1<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*1)+s(AT,k=6)+dow,family="quasipoisson",data=s1.w4)
g2<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*2)+s(AT,k=6)+dow,family="quasipoisson",data=s1.w4)
g3<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*3)+s(AT,k=6)+dow,family="quasipoisson",data=s1.w4)
g4<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*4)+s(AT,k=6)+dow,family="quasipoisson",data=s1.w4)
g5<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*5)+s(AT,k=6)+dow,family="quasipoisson",data=s1.w4)
g6<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="quasipoisson",data=s1.w4)
g7<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*7)+s(AT,k=6)+dow,family="quasipoisson",data=s1.w4)
g8<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*8)+s(AT,k=6)+dow,family="quasipoisson",data=s1.w4)

g1
g2
g3
g4
g5
g6
g7
g8

###################################################################################################################################################
###################################################################################################################################################
#overdispersion test
library(AER)
m1<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="poisson",data=s1.w4)
m2<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="poisson",data=s2.w4)
m3<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="poisson",data=s3.w4)
m4<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="poisson",data=s4.w4)
m5<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="poisson",data=s5.w4)
m6<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="poisson",data=s6.w4)
m7<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="poisson",data=s7.w4)

d1<-dispersiontest(m1)
d2<-dispersiontest(m2)
d3<-dispersiontest(m3)
d4<-dispersiontest(m4)
d5<-dispersiontest(m5)
d6<-dispersiontest(m6)
d7<-dispersiontest(m7)

z1<-data.frame(beta=d1$estimate,z=d1$statistic,pval=d1$p.value)
z2<-data.frame(beta=d2$estimate,z=d2$statistic,pval=d2$p.value)
z3<-data.frame(beta=d3$estimate,z=d3$statistic,pval=d3$p.value)
z4<-data.frame(beta=d4$estimate,z=d4$statistic,pval=d4$p.value)
z5<-data.frame(beta=d5$estimate,z=d5$statistic,pval=d5$p.value)
z6<-data.frame(beta=d6$estimate,z=d6$statistic,pval=d6$p.value)
z7<-data.frame(beta=d7$estimate,z=d7$statistic,pval=d7$p.value)

View(rbind(z1,z2,z3,z4,z5,z6,z7))

#Model Comparison ; Poisson, Quasi-Poisson, Negative binomial , 
#QAIC, AIC ,loglikelihood 찾으려고 해본거

m1<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="poisson",data=s1.w4)
m2<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="quasipoisson",data=s1.w4)
m3<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="nb",data=s1.w4)

mc<-list("ML-pois"=m1,"Quasi-Pois"=m2,"NB"=m3)
library(splines)

gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="quasipoisson",data=s3.w4)
summary(glm(TOT ~lag04,family="quasipoisson",data=s3.w4))

(sum(dpois(s1.w4$TOT,lambda=exp(predict(m1)),log=TRUE))) 
(sum(dpois(s1.w4$TOT,lambda=exp(predict(m2)),log=TRUE))) 
(sum(dpois(s1.w4$TOT,lambda=exp(predict(m3)),log=TRUE))) 

summary(m1)
summary(m2)
summary(m3)

result<-rbind(estimate=sapply(mc,function(x) coef(x)[2]),
              RR=sapply(mc,function(x) round(exp(coef(x)[2]),3)),
              se=sapply(mc,function(x) summary(x)$p.table[2,2]),
              logLik=sapply(mc,function(x) round(logLik(x),digits=0)),
              AIC=sapply(mc,function(x) AIC(x)),
              BIC=sapply(mc,function(x) BIC(x)),
              deviance=sapply(mc,function(x) deviance(x)),
              df=sapply(mc,function(x) round(attr(logLik(x),"df"),2)))

round(result,3)

2*attr(logLik(m2),"df")-2*sum(dnbinom(s1.w4$TOT,mu=m2$fitted.values,size=m2$fitted.values/(summary(m2)$dispersion-1),log=T))
2*attr(logLik(m3),"df")-2*sum(dnbinom(s1.w4$TOT,mu=m3$fitted.values,size=m3$fitted.values/(summary(m2)$dispersion-1),log=T))

AIC(m1)

AIC(m3)

slibrary(MuMIn)
model.sel(m1, m2, m3,rank = QIC)

-2*logLik(m3)+2*5

dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}

(sum(residuals(m1,"pearson")^2)/m1$df.residual);dfun(m1)
(sum(residuals(m2,"pearson")^2)/m2$df.residual);dfun(m2)
(sum(residuals(m3,"pearson")^2)/m3$df.residual);dfun(m3)

(qAICc(m1,dispersion=dfun(m1),nobs=length(s1.w4$TOT)))
(qAICc(m2,dispersion=dfun(m1),nobs=length(s1.w4$TOT)))
(qAICc(m3,dispersion=dfun(m1),nobs=length(s1.w4$TOT)))

x.quasipoisson <- function(...) {
  res <- quasipoisson(...)
  res$aic <- poisson(...)$aic
  res
}
library(MuMIn)
m4<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family = "x.quasipoisson",data=s1.w4,na.action=na.fail)
gg <- dredge(m4,rank="QAIC", chat=dfun(m4))
gg
m5<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="poisson",data=s1.w4,na.action=na.fail)
gg <- dredge(m5,rank="QAIC", chat=dfun(m5))
gg


##################################################################################################################################
##################################################################################################################################









