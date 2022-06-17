#########################################################################################################################################
#라이브러리
library(readxl)  ;library(ggplot2)
library(plotrix) ;library(sqldf)
library(dplyr)   ;library(plyr)
library(reshape2);library(mgcv)
library(TTR)     ;library(lubridate)
# install.packages("readxl")
# install.packaes("plotrix")
# install.packaes("dplyr")
# install.packaes("reshape2")
# install.packaes("TTR")
# install.packaes("lubridate")
# install.packaes("mgcv")
# install.packaes("sqldf")
########################################################################################################################################
#데이터 불러오기 
setwd("C:\\Users\\AROOM206_kard141\\Desktop\\AOM\\분석")
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
setwd("C:\\Users\\AROOM206_kard141\\Desktop\\AOM\\반입파일")
pm<-read.csv("pm.csv",header=T)

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



g1<-gam(TOT~s(lag07)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=df1)
g2<-gam(TOT~s(lag07)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=df2)
g3<-gam(TOT~s(lag07)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=df3)
g4<-gam(TOT~s(lag07)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=df4)
g5<-gam(TOT~s(lag07)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=df5)
g6<-gam(TOT~s(lag07)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=df6)
g7<-gam(TOT~s(lag07)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow,family="poisson",data=df7)
x11();
par(mfrow=c(2,4))
plot(g1,select=1,cex.axis=1.45,cex.lab=1.45,main="Seoul",scheme=1,shade.col="skyblue",ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g2,select=1,cex.axis=1.45,cex.lab=1.45,main="Busan",scheme=1,shade.col="skyblue",ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g3,select=1,cex.axis=1.45,cex.lab=1.45,main="Daegu",scheme=1,shade.col="skyblue",ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g4,select=1,cex.axis=1.45,cex.lab=1.45,main="Incheon",scheme=1,shade.col="skyblue",ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g5,select=1,cex.axis=1.45,cex.lab=1.45,main="Gwangju",scheme=1,shade.col="skyblue",ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g6,select=1,cex.axis=1.45,cex.lab=1.45,main="Daejeon",scheme=1,shade.col="skyblue",ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g7,select=1,cex.axis=1.45,cex.lab=1.45,main="Ulsan",scheme=1,shade.col="skyblue",ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))

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

write.csv(fit_single,file="fit_single_w2.csv",row.names=F)
write.csv(fit_moving,file="fit_moving_w2.csv",row.names=F)

#######################################################################################################################################################

#Time trend
fit.df01=NULL;
fit.df02=NULL;
fit.df03=NULL;
fit.df04=NULL;
fit.df05=NULL;
fit.df06=NULL;
fit.df07=NULL;
df=1:10
for(i in df) {
  df_fit01<-summary(gam(TOT~lag02+s(as.numeric(DATE),k=df[i]*9)+s(AT,k=6)+dow,family="poisson",data=s1.w4))$p.table[2,]
  df_fit02<-summary(gam(TOT~lag02+s(as.numeric(DATE),k=df[i]*9)+s(AT,k=6)+dow,family="poisson",data=s1.w4))$p.table[2,]
  df_fit03<-summary(gam(TOT~lag02+s(as.numeric(DATE),k=df[i]*9)+s(AT,k=6)+dow,family="poisson",data=s1.w4))$p.table[2,]
  df_fit04<-summary(gam(TOT~lag02+s(as.numeric(DATE),k=df[i]*9)+s(AT,k=6)+dow,family="poisson",data=s1.w4))$p.table[2,]
  df_fit05<-summary(gam(TOT~lag02+s(as.numeric(DATE),k=df[i]*9)+s(AT,k=6)+dow,family="poisson",data=s1.w4))$p.table[2,]
  df_fit06<-summary(gam(TOT~lag02+s(as.numeric(DATE),k=df[i]*9)+s(AT,k=6)+dow,family="poisson",data=s1.w4))$p.table[2,]
  df_fit07<-summary(gam(TOT~lag02+s(as.numeric(DATE),k=df[i]*9)+s(AT,k=6)+dow,family="poisson",data=s1.w4))$p.table[2,]
  
  fit.df01[[i]]=df_fit01 ; fit.df02[[i]]=df_fit02 ;
  fit.df03[[i]]=df_fit03 ; fit.df04[[i]]=df_fit04 ;
  fit.df05[[i]]=df_fit05 ; fit.df06[[i]]=df_fit06 ; fit.df07[[i]]=df_fit07 
  print(i)}

fit.df01<-as.data.frame(do.call(rbind,fit.df01)) ; fit.df02<-as.data.frame(do.call(rbind,fit.df02))
fit.df03<-as.data.frame(do.call(rbind,fit.df03)) ; fit.df04<-as.data.frame(do.call(rbind,fit.df04))
fit.df05<-as.data.frame(do.call(rbind,fit.df05)) ; fit.df06<-as.data.frame(do.call(rbind,fit.df06))
fit.df07<-as.data.frame(do.call(rbind,fit.df07)) 

fit.df01$df=df ; fit.df02$df=df ; fit.df03$df=df ; 
fit.df04$df=df ; fit.df05$df=df ; fit.df06$df=df ; fit.df07$df=df 

fit.df01$RR =exp(fit.df01$Estimate); fit.df01$lci=exp(fit.df01$Estimate-1.96*fit.df01$`Std. Error`); fit.df01$uci=exp(fit.df01$Estimate+1.96*fit.df01$`Std. Error`)
fit.df02$RR =exp(fit.df02$Estimate); fit.df02$lci=exp(fit.df02$Estimate-1.96*fit.df02$`Std. Error`); fit.df02$uci=exp(fit.df02$Estimate+1.96*fit.df02$`Std. Error`)
fit.df03$RR =exp(fit.df03$Estimate); fit.df03$lci=exp(fit.df03$Estimate-1.96*fit.df03$`Std. Error`); fit.df03$uci=exp(fit.df03$Estimate+1.96*fit.df03$`Std. Error`)
fit.df04$RR =exp(fit.df04$Estimate); fit.df04$lci=exp(fit.df04$Estimate-1.96*fit.df04$`Std. Error`); fit.df04$uci=exp(fit.df04$Estimate+1.96*fit.df04$`Std. Error`)
fit.df05$RR =exp(fit.df05$Estimate); fit.df05$lci=exp(fit.df05$Estimate-1.96*fit.df05$`Std. Error`); fit.df05$uci=exp(fit.df05$Estimate+1.96*fit.df05$`Std. Error`)
fit.df06$RR =exp(fit.df06$Estimate); fit.df06$lci=exp(fit.df06$Estimate-1.96*fit.df06$`Std. Error`); fit.df06$uci=exp(fit.df06$Estimate+1.96*fit.df06$`Std. Error`)
fit.df07$RR =exp(fit.df07$Estimate); fit.df07$lci=exp(fit.df07$Estimate-1.96*fit.df07$`Std. Error`); fit.df07$uci=exp(fit.df07$Estimate+1.96*fit.df07$`Std. Error`)


