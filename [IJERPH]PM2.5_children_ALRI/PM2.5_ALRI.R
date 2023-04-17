library(readxl) ;library(ggplot2)
library(sqldf)  ;library(dplyr)
library(plyr)   ;library(mgcv)
library(TTR)    ;library(Epi)
library(tsModel);library(lubridate)
library(dlnm)   ;library(metafor)
library(mixmeta)

setwd("D:\\EUMC\\논문\\연구논문\\SNU_PM2.5(미세먼지 인체건강유해평가)\\미세먼지_소아_LRI\\analysis")

dat<-read.csv("lri.csv",header=T)
dat$date=as.Date(dat$date)
dat$year=year(dat$date)
dat$month=month(dat$date)

dat$SIDO=factor(dat$SIDO,levels=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"))

dat<-dat %>%  select(date,SIDO,AGE0005:SEX_F0005,pm:lag7,lag01:lag07,mtemp:month)

#day of week 배박사님 자료는 평일, 공휴일 1~4 구분, 
dat$dow=weekdays(dat$date) #월화수목금토일 
# dat$dow=as.factor(dat$dow)

dat$year_month=substr(dat$date,1,7)
z<-aggregate(dat$AGE0005,list(dat$year_month),sum,na.rm=T)
names(z)=c("month","x")
x11();ggplot(z,aes(month,x))+geom_bar(stat="identity")
######################################################################################################################################################
dat.s1<-subset(dat,SIDO=="Seoul")   ;dat.s2<-subset(dat,SIDO=="Busan")
dat.s3<-subset(dat,SIDO=="Daegu")   ;dat.s4<-subset(dat,SIDO=="Incheon")
dat.s5<-subset(dat,SIDO=="Gwangju") ;dat.s6<-subset(dat,SIDO=="Daejeon")
dat.s7<-subset(dat,SIDO=="Ulsan")

#계절 나눠서
dat.s1.w<-subset(dat.s1,month %in% c(4:9));dat.s1.s<-subset(dat.s1,month %in% c(1:3,10:12));
dat.s2.w<-subset(dat.s2,month %in% c(4:9));dat.s2.s<-subset(dat.s2,month %in% c(1:3,10:12));
dat.s3.w<-subset(dat.s3,month %in% c(4:9));dat.s3.s<-subset(dat.s3,month %in% c(1:3,10:12));
dat.s4.w<-subset(dat.s4,month %in% c(4:9));dat.s4.s<-subset(dat.s4,month %in% c(1:3,10:12));
dat.s5.w<-subset(dat.s5,month %in% c(4:9));dat.s5.s<-subset(dat.s5,month %in% c(1:3,10:12));
dat.s6.w<-subset(dat.s6,month %in% c(4:9));dat.s6.s<-subset(dat.s6,month %in% c(1:3,10:12));
dat.s7.w<-subset(dat.s7,month %in% c(4:9));dat.s7.s<-subset(dat.s7,month %in% c(1:3,10:12));

dat.warm <- subset(dat,month %in% c(4:9))
dat.cold <- subset(dat,month %in% c(1:3,10:12))

#####################################################################################################################################################################
#####################################################################################################################################################################
###Seoul###
###########
fit01=NULL; fit02=NULL; fit03=NULL;fit04=NULL; fit05=NULL; fit06=NULL;fit07=NULL; fit08=NULL; fit09=NULL
for(i in 1:15){
  fit01[[i]]<-summary(gam(AGE0005  ~dat.s1[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s1))$'p.table'[2,]
  fit02[[i]]<-summary(gam(SEX_M0005~dat.s1[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s1))$'p.table'[2,]
  fit03[[i]]<-summary(gam(SEX_F0005~dat.s1[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s1))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0005  ~dat.s1.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s1.w))$'p.table'[2,]
  fit05[[i]]<-summary(gam(SEX_M0005~dat.s1.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s1.w))$'p.table'[2,]
  fit06[[i]]<-summary(gam(SEX_F0005~dat.s1.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s1.w))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE0005  ~dat.s1.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s1.s))$'p.table'[2,]
  fit08[[i]]<-summary(gam(SEX_M0005~dat.s1.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s1.s))$'p.table'[2,]
  fit09[[i]]<-summary(gam(SEX_F0005~dat.s1.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s1.s))$'p.table'[2,]; print(i)}

s1.fit01<-as.data.frame(do.call(rbind,fit01));s1.fit01$category="AGE0005"         ; s1.fit01$lag=names(dat.s1)[6:20]
s1.fit02<-as.data.frame(do.call(rbind,fit02));s1.fit02$category="SEX_M0005"       ; s1.fit02$lag=names(dat.s1)[6:20]
s1.fit03<-as.data.frame(do.call(rbind,fit03));s1.fit03$category="SEX_F0005"       ; s1.fit03$lag=names(dat.s1)[6:20]
s1.fit04<-as.data.frame(do.call(rbind,fit04));s1.fit04$category="Cold_AGE0005"    ; s1.fit04$lag=names(dat.s1)[6:20]
s1.fit05<-as.data.frame(do.call(rbind,fit05));s1.fit05$category="Cold_SEX_M0005"  ; s1.fit05$lag=names(dat.s1)[6:20]
s1.fit06<-as.data.frame(do.call(rbind,fit06));s1.fit06$category="Cold_SEX_F0005"  ; s1.fit06$lag=names(dat.s1)[6:20]
s1.fit07<-as.data.frame(do.call(rbind,fit07));s1.fit07$category="Warm_AGE0005"    ; s1.fit07$lag=names(dat.s1)[6:20]
s1.fit08<-as.data.frame(do.call(rbind,fit08));s1.fit08$category="Warm_SEX_M0005"  ; s1.fit08$lag=names(dat.s1)[6:20]
s1.fit09<-as.data.frame(do.call(rbind,fit09));s1.fit09$category="Warm_SEX_F0005"  ; s1.fit09$lag=names(dat.s1)[6:20]

dat.s1.fit<-rbind(s1.fit01,s1.fit02,s1.fit03,s1.fit04,s1.fit05,s1.fit06,s1.fit07,s1.fit08,s1.fit09)
#####################################################################################################################################################################
###Busan###
###########
fit01=NULL; fit02=NULL; fit03=NULL;fit04=NULL; fit05=NULL; fit06=NULL;fit07=NULL; fit08=NULL; fit09=NULL
for(i in 1:15){
  fit01[[i]]<-summary(gam(AGE0005  ~dat.s2[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s2))$'p.table'[2,]
  fit02[[i]]<-summary(gam(SEX_M0005~dat.s2[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s2))$'p.table'[2,]
  fit03[[i]]<-summary(gam(SEX_F0005~dat.s2[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s2))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0005  ~dat.s2.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s2.w))$'p.table'[2,]
  fit05[[i]]<-summary(gam(SEX_M0005~dat.s2.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s2.w))$'p.table'[2,]
  fit06[[i]]<-summary(gam(SEX_F0005~dat.s2.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s2.w))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE0005  ~dat.s2.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s2.s))$'p.table'[2,]
  fit08[[i]]<-summary(gam(SEX_M0005~dat.s2.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s2.s))$'p.table'[2,]
  fit09[[i]]<-summary(gam(SEX_F0005~dat.s2.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s2.s))$'p.table'[2,]; print(i)}

s1.fit01<-as.data.frame(do.call(rbind,fit01));s1.fit01$category="AGE0005"         ; s1.fit01$lag=names(dat.s2)[6:20]
s1.fit02<-as.data.frame(do.call(rbind,fit02));s1.fit02$category="SEX_M0005"       ; s1.fit02$lag=names(dat.s2)[6:20]
s1.fit03<-as.data.frame(do.call(rbind,fit03));s1.fit03$category="SEX_F0005"       ; s1.fit03$lag=names(dat.s2)[6:20]
s1.fit04<-as.data.frame(do.call(rbind,fit04));s1.fit04$category="Cold_AGE0005"    ; s1.fit04$lag=names(dat.s2)[6:20]
s1.fit05<-as.data.frame(do.call(rbind,fit05));s1.fit05$category="Cold_SEX_M0005"  ; s1.fit05$lag=names(dat.s2)[6:20]
s1.fit06<-as.data.frame(do.call(rbind,fit06));s1.fit06$category="Cold_SEX_F0005"  ; s1.fit06$lag=names(dat.s2)[6:20]
s1.fit07<-as.data.frame(do.call(rbind,fit07));s1.fit07$category="Warm_AGE0005"    ; s1.fit07$lag=names(dat.s2)[6:20]
s1.fit08<-as.data.frame(do.call(rbind,fit08));s1.fit08$category="Warm_SEX_M0005"  ; s1.fit08$lag=names(dat.s2)[6:20]
s1.fit09<-as.data.frame(do.call(rbind,fit09));s1.fit09$category="Warm_SEX_F0005"  ; s1.fit09$lag=names(dat.s2)[6:20]

dat.s2.fit<-rbind(s1.fit01,s1.fit02,s1.fit03,s1.fit04,s1.fit05,s1.fit06,s1.fit07,s1.fit08,s1.fit09)
#####################################################################################################################################################################
###Daegu###
###########
fit01=NULL; fit02=NULL; fit03=NULL;fit04=NULL; fit05=NULL; fit06=NULL;fit07=NULL; fit08=NULL; fit09=NULL
for(i in 1:15){
  fit01[[i]]<-summary(gam(AGE0005  ~dat.s3[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s3))$'p.table'[2,]
  fit02[[i]]<-summary(gam(SEX_M0005~dat.s3[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s3))$'p.table'[2,]
  fit03[[i]]<-summary(gam(SEX_F0005~dat.s3[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s3))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0005  ~dat.s3.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s3.w))$'p.table'[2,]
  fit05[[i]]<-summary(gam(SEX_M0005~dat.s3.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s3.w))$'p.table'[2,]
  fit06[[i]]<-summary(gam(SEX_F0005~dat.s3.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s3.w))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE0005  ~dat.s3.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s3.s))$'p.table'[2,]
  fit08[[i]]<-summary(gam(SEX_M0005~dat.s3.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s3.s))$'p.table'[2,]
  fit09[[i]]<-summary(gam(SEX_F0005~dat.s3.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s3.s))$'p.table'[2,]; print(i)}

s1.fit01<-as.data.frame(do.call(rbind,fit01));s1.fit01$category="AGE0005"         ; s1.fit01$lag=names(dat.s3)[6:20]
s1.fit02<-as.data.frame(do.call(rbind,fit02));s1.fit02$category="SEX_M0005"       ; s1.fit02$lag=names(dat.s3)[6:20]
s1.fit03<-as.data.frame(do.call(rbind,fit03));s1.fit03$category="SEX_F0005"       ; s1.fit03$lag=names(dat.s3)[6:20]
s1.fit04<-as.data.frame(do.call(rbind,fit04));s1.fit04$category="Cold_AGE0005"    ; s1.fit04$lag=names(dat.s3)[6:20]
s1.fit05<-as.data.frame(do.call(rbind,fit05));s1.fit05$category="Cold_SEX_M0005"  ; s1.fit05$lag=names(dat.s3)[6:20]
s1.fit06<-as.data.frame(do.call(rbind,fit06));s1.fit06$category="Cold_SEX_F0005"  ; s1.fit06$lag=names(dat.s3)[6:20]
s1.fit07<-as.data.frame(do.call(rbind,fit07));s1.fit07$category="Warm_AGE0005"    ; s1.fit07$lag=names(dat.s3)[6:20]
s1.fit08<-as.data.frame(do.call(rbind,fit08));s1.fit08$category="Warm_SEX_M0005"  ; s1.fit08$lag=names(dat.s3)[6:20]
s1.fit09<-as.data.frame(do.call(rbind,fit09));s1.fit09$category="Warm_SEX_F0005"  ; s1.fit09$lag=names(dat.s3)[6:20]

dat.s3.fit<-rbind(s1.fit01,s1.fit02,s1.fit03,s1.fit04,s1.fit05,s1.fit06,s1.fit07,s1.fit08,s1.fit09)
#####################################################################################################################################################################
###Incheon###
###########
fit01=NULL; fit02=NULL; fit03=NULL;fit04=NULL; fit05=NULL; fit06=NULL;fit07=NULL; fit08=NULL; fit09=NULL
for(i in 1:15){
  fit01[[i]]<-summary(gam(AGE0005  ~dat.s4[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s4))$'p.table'[2,]
  fit02[[i]]<-summary(gam(SEX_M0005~dat.s4[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s4))$'p.table'[2,]
  fit03[[i]]<-summary(gam(SEX_F0005~dat.s4[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s4))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0005  ~dat.s4.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s4.w))$'p.table'[2,]
  fit05[[i]]<-summary(gam(SEX_M0005~dat.s4.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s4.w))$'p.table'[2,]
  fit06[[i]]<-summary(gam(SEX_F0005~dat.s4.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s4.w))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE0005  ~dat.s4.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s4.s))$'p.table'[2,]
  fit08[[i]]<-summary(gam(SEX_M0005~dat.s4.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s4.s))$'p.table'[2,]
  fit09[[i]]<-summary(gam(SEX_F0005~dat.s4.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s4.s))$'p.table'[2,]; print(i)}

s1.fit01<-as.data.frame(do.call(rbind,fit01));s1.fit01$category="AGE0005"         ; s1.fit01$lag=names(dat.s4)[6:20]
s1.fit02<-as.data.frame(do.call(rbind,fit02));s1.fit02$category="SEX_M0005"       ; s1.fit02$lag=names(dat.s4)[6:20]
s1.fit03<-as.data.frame(do.call(rbind,fit03));s1.fit03$category="SEX_F0005"       ; s1.fit03$lag=names(dat.s4)[6:20]
s1.fit04<-as.data.frame(do.call(rbind,fit04));s1.fit04$category="Cold_AGE0005"    ; s1.fit04$lag=names(dat.s4)[6:20]
s1.fit05<-as.data.frame(do.call(rbind,fit05));s1.fit05$category="Cold_SEX_M0005"  ; s1.fit05$lag=names(dat.s4)[6:20]
s1.fit06<-as.data.frame(do.call(rbind,fit06));s1.fit06$category="Cold_SEX_F0005"  ; s1.fit06$lag=names(dat.s4)[6:20]
s1.fit07<-as.data.frame(do.call(rbind,fit07));s1.fit07$category="Warm_AGE0005"    ; s1.fit07$lag=names(dat.s4)[6:20]
s1.fit08<-as.data.frame(do.call(rbind,fit08));s1.fit08$category="Warm_SEX_M0005"  ; s1.fit08$lag=names(dat.s4)[6:20]
s1.fit09<-as.data.frame(do.call(rbind,fit09));s1.fit09$category="Warm_SEX_F0005"  ; s1.fit09$lag=names(dat.s4)[6:20]

dat.s4.fit<-rbind(s1.fit01,s1.fit02,s1.fit03,s1.fit04,s1.fit05,s1.fit06,s1.fit07,s1.fit08,s1.fit09)
#####################################################################################################################################################################
###Gwangju###
#############
fit01=NULL; fit02=NULL; fit03=NULL;fit04=NULL; fit05=NULL; fit06=NULL;fit07=NULL; fit08=NULL; fit09=NULL
for(i in 1:15){
  fit01[[i]]<-summary(gam(AGE0005  ~dat.s5[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s5))$'p.table'[2,]
  fit02[[i]]<-summary(gam(SEX_M0005~dat.s5[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s5))$'p.table'[2,]
  fit03[[i]]<-summary(gam(SEX_F0005~dat.s5[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s5))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0005  ~dat.s5.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s5.w))$'p.table'[2,]
  fit05[[i]]<-summary(gam(SEX_M0005~dat.s5.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s5.w))$'p.table'[2,]
  fit06[[i]]<-summary(gam(SEX_F0005~dat.s5.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s5.w))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE0005  ~dat.s5.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s5.s))$'p.table'[2,]
  fit08[[i]]<-summary(gam(SEX_M0005~dat.s5.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s5.s))$'p.table'[2,]
  fit09[[i]]<-summary(gam(SEX_F0005~dat.s5.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s5.s))$'p.table'[2,]; print(i)}

s1.fit01<-as.data.frame(do.call(rbind,fit01));s1.fit01$category="AGE0005"         ; s1.fit01$lag=names(dat.s5)[6:20]
s1.fit02<-as.data.frame(do.call(rbind,fit02));s1.fit02$category="SEX_M0005"       ; s1.fit02$lag=names(dat.s5)[6:20]
s1.fit03<-as.data.frame(do.call(rbind,fit03));s1.fit03$category="SEX_F0005"       ; s1.fit03$lag=names(dat.s5)[6:20]
s1.fit04<-as.data.frame(do.call(rbind,fit04));s1.fit04$category="Cold_AGE0005"    ; s1.fit04$lag=names(dat.s5)[6:20]
s1.fit05<-as.data.frame(do.call(rbind,fit05));s1.fit05$category="Cold_SEX_M0005"  ; s1.fit05$lag=names(dat.s5)[6:20]
s1.fit06<-as.data.frame(do.call(rbind,fit06));s1.fit06$category="Cold_SEX_F0005"  ; s1.fit06$lag=names(dat.s5)[6:20]
s1.fit07<-as.data.frame(do.call(rbind,fit07));s1.fit07$category="Warm_AGE0005"    ; s1.fit07$lag=names(dat.s5)[6:20]
s1.fit08<-as.data.frame(do.call(rbind,fit08));s1.fit08$category="Warm_SEX_M0005"  ; s1.fit08$lag=names(dat.s5)[6:20]
s1.fit09<-as.data.frame(do.call(rbind,fit09));s1.fit09$category="Warm_SEX_F0005"  ; s1.fit09$lag=names(dat.s5)[6:20]

dat.s5.fit<-rbind(s1.fit01,s1.fit02,s1.fit03,s1.fit04,s1.fit05,s1.fit06,s1.fit07,s1.fit08,s1.fit09)
#####################################################################################################################################################################
###Daejeon###
#############
fit01=NULL; fit02=NULL; fit03=NULL;fit04=NULL; fit05=NULL; fit06=NULL;fit07=NULL; fit08=NULL; fit09=NULL
for(i in 1:15){
  fit01[[i]]<-summary(gam(AGE0005  ~dat.s6[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s6))$'p.table'[2,]
  fit02[[i]]<-summary(gam(SEX_M0005~dat.s6[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s6))$'p.table'[2,]
  fit03[[i]]<-summary(gam(SEX_F0005~dat.s6[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s6))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0005  ~dat.s6.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s6.w))$'p.table'[2,]
  fit05[[i]]<-summary(gam(SEX_M0005~dat.s6.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s6.w))$'p.table'[2,]
  fit06[[i]]<-summary(gam(SEX_F0005~dat.s6.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s6.w))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE0005  ~dat.s6.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s6.s))$'p.table'[2,]
  fit08[[i]]<-summary(gam(SEX_M0005~dat.s6.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s6.s))$'p.table'[2,]
  fit09[[i]]<-summary(gam(SEX_F0005~dat.s6.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s6.s))$'p.table'[2,]; print(i)}

s1.fit01<-as.data.frame(do.call(rbind,fit01));s1.fit01$category="AGE0005"         ; s1.fit01$lag=names(dat.s6)[6:20]
s1.fit02<-as.data.frame(do.call(rbind,fit02));s1.fit02$category="SEX_M0005"       ; s1.fit02$lag=names(dat.s6)[6:20]
s1.fit03<-as.data.frame(do.call(rbind,fit03));s1.fit03$category="SEX_F0005"       ; s1.fit03$lag=names(dat.s6)[6:20]
s1.fit04<-as.data.frame(do.call(rbind,fit04));s1.fit04$category="Cold_AGE0005"    ; s1.fit04$lag=names(dat.s6)[6:20]
s1.fit05<-as.data.frame(do.call(rbind,fit05));s1.fit05$category="Cold_SEX_M0005"  ; s1.fit05$lag=names(dat.s6)[6:20]
s1.fit06<-as.data.frame(do.call(rbind,fit06));s1.fit06$category="Cold_SEX_F0005"  ; s1.fit06$lag=names(dat.s6)[6:20]
s1.fit07<-as.data.frame(do.call(rbind,fit07));s1.fit07$category="Warm_AGE0005"    ; s1.fit07$lag=names(dat.s6)[6:20]
s1.fit08<-as.data.frame(do.call(rbind,fit08));s1.fit08$category="Warm_SEX_M0005"  ; s1.fit08$lag=names(dat.s6)[6:20]
s1.fit09<-as.data.frame(do.call(rbind,fit09));s1.fit09$category="Warm_SEX_F0005"  ; s1.fit09$lag=names(dat.s6)[6:20]

dat.s6.fit<-rbind(s1.fit01,s1.fit02,s1.fit03,s1.fit04,s1.fit05,s1.fit06,s1.fit07,s1.fit08,s1.fit09)
#####################################################################################################################################################################
###Ulsan#####
#############
fit01=NULL; fit02=NULL; fit03=NULL;fit04=NULL; fit05=NULL; fit06=NULL;fit07=NULL; fit08=NULL; fit09=NULL
for(i in 1:15){
  fit01[[i]]<-summary(gam(AGE0005  ~dat.s7[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s7))$'p.table'[2,]
  fit02[[i]]<-summary(gam(SEX_M0005~dat.s7[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s7))$'p.table'[2,]
  fit03[[i]]<-summary(gam(SEX_F0005~dat.s7[,5+i]+s(as.numeric(date)  ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s7))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0005  ~dat.s7.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s7.w))$'p.table'[2,]
  fit05[[i]]<-summary(gam(SEX_M0005~dat.s7.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s7.w))$'p.table'[2,]
  fit06[[i]]<-summary(gam(SEX_F0005~dat.s7.w[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s7.w))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE0005  ~dat.s7.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s7.s))$'p.table'[2,]
  fit08[[i]]<-summary(gam(SEX_M0005~dat.s7.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s7.s))$'p.table'[2,]
  fit09[[i]]<-summary(gam(SEX_F0005~dat.s7.s[,5+i]+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s7.s))$'p.table'[2,]; print(i)}

s1.fit01<-as.data.frame(do.call(rbind,fit01));s1.fit01$category="AGE0005"         ; s1.fit01$lag=names(dat.s7)[6:20]
s1.fit02<-as.data.frame(do.call(rbind,fit02));s1.fit02$category="SEX_M0005"       ; s1.fit02$lag=names(dat.s7)[6:20]
s1.fit03<-as.data.frame(do.call(rbind,fit03));s1.fit03$category="SEX_F0005"       ; s1.fit03$lag=names(dat.s7)[6:20]
s1.fit04<-as.data.frame(do.call(rbind,fit04));s1.fit04$category="Cold_AGE0005"    ; s1.fit04$lag=names(dat.s7)[6:20]
s1.fit05<-as.data.frame(do.call(rbind,fit05));s1.fit05$category="Cold_SEX_M0005"  ; s1.fit05$lag=names(dat.s7)[6:20]
s1.fit06<-as.data.frame(do.call(rbind,fit06));s1.fit06$category="Cold_SEX_F0005"  ; s1.fit06$lag=names(dat.s7)[6:20]
s1.fit07<-as.data.frame(do.call(rbind,fit07));s1.fit07$category="Warm_AGE0005"    ; s1.fit07$lag=names(dat.s7)[6:20]
s1.fit08<-as.data.frame(do.call(rbind,fit08));s1.fit08$category="Warm_SEX_M0005"  ; s1.fit08$lag=names(dat.s7)[6:20]
s1.fit09<-as.data.frame(do.call(rbind,fit09));s1.fit09$category="Warm_SEX_F0005"  ; s1.fit09$lag=names(dat.s7)[6:20]

dat.s7.fit<-rbind(s1.fit01,s1.fit02,s1.fit03,s1.fit04,s1.fit05,s1.fit06,s1.fit07,s1.fit08,s1.fit09)
#####################################################################################################################################################################
#####################################################################################################################################################################
dat.s1.fit$SIDO="Seoul"  ; dat.s2.fit$SIDO="Busan"
dat.s3.fit$SIDO="Daegu"  ; dat.s4.fit$SIDO="Incheon"
dat.s5.fit$SIDO="Gwangju"; dat.s6.fit$SIDO="Daejeon" ;dat.s7.fit$SIDO="Ulsan"

sido.fit<-rbind(dat.s1.fit,dat.s2.fit,dat.s3.fit,dat.s4.fit,dat.s5.fit,dat.s6.fit,dat.s7.fit)

sido.fit$RR =with(sido.fit,round(exp(Estimate),4))
sido.fit$lwl=with(sido.fit,round(exp(Estimate-1.96*`Std. Error`),4))
sido.fit$uwl=with(sido.fit,round(exp(Estimate+1.96*`Std. Error`),4))

setwd("C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_소아_LRI\\analysis\\result")
# write.csv(sido.fit,file="lri.sido.fit.csv",row.names=F)

#####################################################################################################################################################################
#####################################################################################################################################################################
#exposure-response GAM plot 
#보고자 하는 노출 시점 값에 대해서 , 초기에 노출 단위를 10으로 나눴음(10 증가 단위당 보기위해서) 
#다시 10 곱해줌 (원자료로)
dat.s1$exposure=dat.s1$lag06*10
dat.s2$exposure=dat.s2$lag06*10
dat.s3$exposure=dat.s3$lag06*10
dat.s4$exposure=dat.s4$lag06*10
dat.s5$exposure=dat.s5$lag06*10
dat.s6$exposure=dat.s6$lag06*10
dat.s7$exposure=dat.s7$lag06*10

#뒤에 method는 ubre, ML, REML 등 옵션임 지워도 무방, Robust하게 보기위해 적용.
g1<-gam(AGE0005~s(exposure)+s(as.numeric(date) ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s1,method="ML")
g2<-gam(AGE0005~s(exposure)+s(as.numeric(date) ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s2,method="ML")
g3<-gam(AGE0005~s(exposure)+s(as.numeric(date) ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s3,method="ML")
g4<-gam(AGE0005~s(exposure)+s(as.numeric(date) ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s4,method="ML")
g5<-gam(AGE0005~s(exposure)+s(as.numeric(date) ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s5,method="ML")
g6<-gam(AGE0005~s(exposure)+s(as.numeric(date) ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s6,method="ML")
g7<-gam(AGE0005~s(exposure)+s(as.numeric(date) ,k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=dat.s7,method="ML")

x11();par(mfrow=c(2,4));
plot(g1,select=1,scheme=1,cex.axis=1.45,cex.lab=1.4,ylim=c(-0.5,0.5),xlim=c(5,70),main="Seoul",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)))
plot(g2,select=1,scheme=1,cex.axis=1.45,cex.lab=1.4,ylim=c(-0.5,0.5),xlim=c(5,70),main="Busan",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)))
plot(g3,select=1,scheme=1,cex.axis=1.45,cex.lab=1.4,ylim=c(-0.5,0.5),xlim=c(5,70),main="Daegu",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)))
plot(g4,select=1,scheme=1,cex.axis=1.45,cex.lab=1.4,ylim=c(-0.5,0.5),xlim=c(5,70),main="Incheon",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)))
plot(g5,select=1,scheme=1,cex.axis=1.45,cex.lab=1.4,ylim=c(-0.5,0.5),xlim=c(5,70),main="Gwangju",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)))
plot(g6,select=1,scheme=1,cex.axis=1.45,cex.lab=1.4,ylim=c(-0.5,0.5),xlim=c(5,70),main="Daejeon",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)))
plot(g7,select=1,scheme=1,cex.axis=1.45,cex.lab=1.4,ylim=c(-0.5,0.5),xlim=c(5,70),main="Ulsan",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)))

##################################################################################################################################
##################################################################################################################################
#Single lag 늘려서 한 14일
#CMAQ PM2.5 자료 가져오기 ; 노출자료  
setwd("D:\\EUMC\\논문\\연구논문\\PM2.5_AOM\\analysis")
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
pm$key=paste0(pm$date,"-",pm$SIDO)

pm2<-pm %>% dplyr:: select(key,AT,pm:lag7,SIDO)

i=1 ;b=NULL
for(i in 1:length(unique(pm2$SIDO))){
  a<-subset(pm2,SIDO==unique(pm2$SIDO)[i])
  a<- a %>% dplyr:: select(-SIDO)
  a$lag8=lag(a$pm,8)
  a$lag9=lag(a$pm,9)
  a$lag10=lag(a$pm,10)
  a$lag11=lag(a$pm,11)
  a$lag12=lag(a$pm,12)
  a$lag13=lag(a$pm,13)
  a$lag14=lag(a$pm,14)
  
  a$ma01=apply(a %>% dplyr:: select(pm:lag1),1,mean,na.rm=T)
  a$ma02=apply(a %>% dplyr:: select(pm:lag2),1,mean,na.rm=T)
  a$ma03=apply(a %>% dplyr:: select(pm:lag3),1,mean,na.rm=T)
  a$ma04=apply(a %>% dplyr:: select(pm:lag4),1,mean,na.rm=T)
  a$ma05=apply(a %>% dplyr:: select(pm:lag5),1,mean,na.rm=T)
  a$ma06=apply(a %>% dplyr:: select(pm:lag6),1,mean,na.rm=T)
  a$ma07=apply(a %>% dplyr:: select(pm:lag7),1,mean,na.rm=T)
  a$ma08=apply(a %>% dplyr:: select(pm:lag8),1,mean,na.rm=T)
  a$ma09=apply(a %>% dplyr:: select(pm:lag9),1,mean,na.rm=T)
  a$ma10=apply(a %>% dplyr:: select(pm:lag10),1,mean,na.rm=T)
  a$ma11=apply(a %>% dplyr:: select(pm:lag11),1,mean,na.rm=T)
  a$ma12=apply(a %>% dplyr:: select(pm:lag12),1,mean,na.rm=T)
  a$ma13=apply(a %>% dplyr:: select(pm:lag13),1,mean,na.rm=T)
  a$ma14=apply(a %>% dplyr:: select(pm:lag14),1,mean,na.rm=T)
  b[[i]]<-a;print(i) }

pm3<-as.data.frame(do.call(rbind,b))
dat$key=paste0(dat$date,"-",dat$SIDO)
dat_rev<-dat %>% select(date:SEX_F0005,dow,key)
dat_rev2<-merge(dat_rev,pm3,by="key",all.x=T)

s1<-subset(dat_rev2,SIDO=="Seoul")
s2<-subset(dat_rev2,SIDO=="Busan")
s3<-subset(dat_rev2,SIDO=="Daegu")
s4<-subset(dat_rev2,SIDO=="Incheon")
s5<-subset(dat_rev2,SIDO=="Gwangju")
s6<-subset(dat_rev2,SIDO=="Daejeon")
s7<-subset(dat_rev2,SIDO=="Ulsan")

#####################################################################################################################################################################
#####################################################################################################################################################################
###Single###
############

s1$dow=as.factor(weekdays.Date(s1$date,abbreviate = T))
s2$dow=as.factor(weekdays.Date(s2$date,abbreviate = T))
s3$dow=as.factor(weekdays.Date(s3$date,abbreviate = T))
s4$dow=as.factor(weekdays.Date(s4$date,abbreviate = T))
s5$dow=as.factor(weekdays.Date(s5$date,abbreviate = T))
s6$dow=as.factor(weekdays.Date(s6$date,abbreviate = T))
s7$dow=as.factor(weekdays.Date(s7$date,abbreviate = T))

fit01=NULL; fit02=NULL; fit03=NULL;fit04=NULL; fit05=NULL; fit06=NULL;fit07=NULL; 
for(i in 1:15){
  fit01[[i]]<-summary(gam(AGE0005  ~s1[,8+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s1))$'p.table'[2,]
  fit02[[i]]<-summary(gam(AGE0005  ~s2[,8+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s2))$'p.table'[2,]
  fit03[[i]]<-summary(gam(AGE0005  ~s3[,8+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s3))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0005  ~s4[,8+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s4))$'p.table'[2,]
  fit05[[i]]<-summary(gam(AGE0005  ~s5[,8+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s5))$'p.table'[2,]
  fit06[[i]]<-summary(gam(AGE0005  ~s6[,8+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s6))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE0005  ~s7[,8+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s7))$'p.table'[2,]
  print(i)}

s1.fit<-as.data.frame(do.call(rbind,fit01));s1.fit$lag=names(s1)[9:23]
s2.fit<-as.data.frame(do.call(rbind,fit02));s2.fit$lag=names(s1)[9:23]
s3.fit<-as.data.frame(do.call(rbind,fit03));s3.fit$lag=names(s1)[9:23]
s4.fit<-as.data.frame(do.call(rbind,fit04));s4.fit$lag=names(s1)[9:23]
s5.fit<-as.data.frame(do.call(rbind,fit05));s5.fit$lag=names(s1)[9:23]
s6.fit<-as.data.frame(do.call(rbind,fit06));s6.fit$lag=names(s1)[9:23]
s7.fit<-as.data.frame(do.call(rbind,fit07));s7.fit$lag=names(s1)[9:23]

s1.fit$SIDO="Seoul"   ; s2.fit$SIDO="Busan"
s3.fit$SIDO="Daegu"   ; s4.fit$SIDO="Incheon"
s5.fit$SIDO="Gwnagju" ; s6.fit$SIDO="Daejeon"
s7.fit$SIDO="Ulsan"

s.fit<-rbind(s1.fit,s2.fit,s3.fit,s4.fit,s5.fit,s6.fit,s7.fit)

s.fit$RR=exp(s.fit$Estimate)
s.fit$lci=exp(s.fit$Estimate-1.96*s.fit$`Std. Error`)
s.fit$uci=exp(s.fit$Estimate+1.96*s.fit$`Std. Error`)

s.fit$lag=gsub("pm","lag0",s.fit$lag)
s.fit$lag=factor(s.fit$lag,levels=unique(s.fit$lag))
s.fit$SIDO=factor(s.fit$SIDO,levels=unique(s.fit$SIDO))

x11();ggplot(s.fit,aes(lag,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=15)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)

a<-gam(AGE0005  ~pm+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s1)
b<-gam(AGE0005  ~pm+s(as.numeric(date),k=7*9)+s(AT,k=6)+dow,family="poisson",data=s1)

tiff(filename="C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_소아_LRI\\figure\\single14.tiff",width=4800,height=2800,res=300)
ggplot(s.fit,aes(lag,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=15)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)+labs(x="Lag")
dev.off()

#####################################################################################################################################################################
#####################################################################################################################################################################
###moving###
############

fit01=NULL; fit02=NULL; fit03=NULL;fit04=NULL; fit05=NULL; fit06=NULL;fit07=NULL; 
for(i in 1:14){
  fit01[[i]]<-summary(gam(AGE0005  ~s1[,23+i]+s(as.numeric(date),k=7*9)+s(AT,k=6)+dow,family="poisson",data=s1))$'p.table'[2,]
  fit02[[i]]<-summary(gam(AGE0005  ~s2[,23+i]+s(as.numeric(date),k=7*9)+s(AT,k=6)+dow,family="poisson",data=s2))$'p.table'[2,]
  fit03[[i]]<-summary(gam(AGE0005  ~s3[,23+i]+s(as.numeric(date),k=7*9)+s(AT,k=6)+dow,family="poisson",data=s3))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0005  ~s4[,23+i]+s(as.numeric(date),k=7*9)+s(AT,k=6)+dow,family="poisson",data=s4))$'p.table'[2,]
  fit05[[i]]<-summary(gam(AGE0005  ~s5[,23+i]+s(as.numeric(date),k=7*9)+s(AT,k=6)+dow,family="poisson",data=s5))$'p.table'[2,]
  fit06[[i]]<-summary(gam(AGE0005  ~s6[,23+i]+s(as.numeric(date),k=7*9)+s(AT,k=6)+dow,family="poisson",data=s6))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE0005  ~s7[,23+i]+s(as.numeric(date),k=7*9)+s(AT,k=6)+dow,family="poisson",data=s7))$'p.table'[2,]
  print(i)}

m1.fit<-as.data.frame(do.call(rbind,fit01));m1.fit$lag=names(s1)[24:37]
m2.fit<-as.data.frame(do.call(rbind,fit02));m2.fit$lag=names(s1)[24:37]
m3.fit<-as.data.frame(do.call(rbind,fit03));m3.fit$lag=names(s1)[24:37]
m4.fit<-as.data.frame(do.call(rbind,fit04));m4.fit$lag=names(s1)[24:37]
m5.fit<-as.data.frame(do.call(rbind,fit05));m5.fit$lag=names(s1)[24:37]
m6.fit<-as.data.frame(do.call(rbind,fit06));m6.fit$lag=names(s1)[24:37]
m7.fit<-as.data.frame(do.call(rbind,fit07));m7.fit$lag=names(s1)[24:37]

m1.fit$SIDO="Seoul"   ; m2.fit$SIDO="Busan"
m3.fit$SIDO="Daegu"   ; m4.fit$SIDO="Incheon"
m5.fit$SIDO="Gwnagju" ; m6.fit$SIDO="Daejeon"
m7.fit$SIDO="Ulsan"

m.fit<-rbind(m1.fit,m2.fit,m3.fit,m4.fit,m5.fit,m6.fit,m7.fit)

m.fit$RR =exp(m.fit$Estimate)
m.fit$lci=exp(m.fit$Estimate-1.96*m.fit$`Std. Error`)
m.fit$uci=exp(m.fit$Estimate+1.96*m.fit$`Std. Error`)

m.fit$lag =gsub("pm","lag0",m.fit$lag)
m.fit$lag =factor(m.fit$lag,levels=unique(m.fit$lag))
m.fit$SIDO=factor(m.fit$SIDO,levels=unique(m.fit$SIDO))

m.fit$obs=paste0("0",1:14)
m.fit$obs=factor(m.fit$obs,levels=unique(m.fit$obs))
x11();ggplot(m.fit,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=15)+facet_wrap(~SIDO,scales="free")+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)+labs(x="Moving average")

tiff(filename="C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_소아_LRI\\figure\\moving30.tiff",width=4800,height=2800,res=300)
ggplot(m.fit,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=15)+facet_wrap(~SIDO,scales="free")+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)+labs(x="Moving average")
dev.off()

#####################################################################################################################################################################
#####################################################################################################################################################################
#Mult-ipollutant

setwd("D:\\EUMC\\데이터관리\\Mornitoring_data\\monitoring_data_JM")

ap08<-as.data.frame(read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2008\\2008_모니터링농도.xlsx",sheet=3))
ap09<-as.data.frame(read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2009\\2009_모니터링농도.xlsx",sheet=3))
ap10<-as.data.frame(read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2010\\2010_모니터링농도.xlsx",sheet=3))
ap11<-as.data.frame(read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2011\\2011_모니터링농도.xlsx",sheet=3))
ap12<-as.data.frame(read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2012\\2012_모니터링농도.xlsx",sheet=3))
ap13<-as.data.frame(read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2013\\2013_모니터링농도.xlsx",sheet=3))
ap14<-as.data.frame(read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2014\\2014_모니터링농도.xlsx",sheet=3))
ap15<-as.data.frame(read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2015\\2015_모니터링농도.xlsx",sheet=3))
ap16<-as.data.frame(read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2016\\2016_모니터링농도.xlsx",sheet=3))

ap0816<-rbind(ap08,ap09,ap10,ap11,ap12,ap13,ap14,ap15,ap16)

ap0816<-ap0816 %>% dplyr::  select(-PM25)
ap0816<-subset(ap0816, sido %in% c("서울","부산","대구","인천","광주","대전","울산") )

ap0816$SIDO=with(ap0816,ifelse(sido=="서울","Seoul",
                               ifelse(sido=="부산","Busan",
                                      ifelse(sido=="대구","Daegu",
                                             ifelse(sido=="인천","Incheon",
                                                    ifelse(sido=="광주","Gwangju",
                                                           ifelse(sido=="대전","Daejeon","Ulsan")))))))
ap0816_rev<-ap0816
ap0816_rev$PM10<-ap0816$PM10/10
ap0816_rev$SO2<-ap0816$SO2*1000
ap0816_rev$NO2<-ap0816$NO2*1000
ap0816_rev$CO<-ap0816$CO*1000
ap0816_rev$O3<-ap0816$O3*1000


ap0816_rev$key=paste0(ap0816_rev$yymmdd,"-",ap0816$SIDO)
ap0816_rev2<- ap0816_rev %>% dplyr::  select(-c(yymmdd,sido,SIDO))

pm$date=as.Date(pm$date)
PM25<-subset(pm,date>="2008-01-01") %>% dplyr::  select(date,SIDO,pm,AT,key)

exposure<-merge(PM25,ap0816_rev2)

exposure <-exposure %>% dplyr:: select(date,SIDO,AT,pm:O3) %>%  mutate(PM25=pm)
exposure <-exposure %>% dplyr:: select(date,SIDO,AT,PM25,PM10:O3)

ag0<-aggregate(exposure$AT,list(exposure$date),mean);names(ag0)=c("date","AT")
ag1<-aggregate(exposure$PM25,list(exposure$date),mean);names(ag1)=c("date","PM25")
ag2<-aggregate(exposure$PM10,list(exposure$date),mean);names(ag2)=c("date","PM10")
ag3<-aggregate(exposure$SO2,list(exposure$date),mean);names(ag3)=c("date","SO2")
ag4<-aggregate(exposure$NO2,list(exposure$date),mean);names(ag4)=c("date","NO2")
ag5<-aggregate(exposure$CO,list(exposure$date),mean);names(ag5)=c("date","CO")
ag6<-aggregate(exposure$O3,list(exposure$date),mean);names(ag6)=c("date","O3")

avg_exposure<-cbind(ag0,PM25=ag1[,2],PM10=ag2[,2],SO2=ag3[,2],NO2=ag4[,2],CO=ag5[,2],O3=ag6[,2])

#correlation plot 
library(ggcorrplot)
avg_exposure
names(avg_exposure)[3]="PM2.5"

cor(avg_exposure %>% dplyr::  select(AT:O3),use="complete.obs",method="spearman")
p.mat=cor_pmat(avg_exposure %>% dplyr:: select(AT:O3),use="complete.obs",method="spearman")
X11();ggcorrplot(cor(avg_exposure %>% dplyr:: select(AT:O3),use="complete.obs",method="spearman"),p.mat=p.mat,hc.order = F,type="lower",pch.cex=25,lab_size=8,pch=4,
                 ,lab=T)+theme(text=element_text(size=15),axis.text.x=element_text(angle=90,size=15),axis.text.y=element_text(size=15))+
  theme(legend.title=element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

tiff(filename="D:\\EUMC\\논문\\연구논문\\SNU_PM2.5(미세먼지 인체건강유해평가)\\미세먼지_소아_LRI\\figure\\corr.tiff",width=2500,height=2500,res=300)
ggcorrplot(cor(avg_exposure %>% dplyr:: select(AT:O3),use="complete.obs",method="spearman"),p.mat=p.mat,hc.order = F,type="lower",pch.cex=25,lab_size=6,pch=4,
           ,lab=T)+theme(text=element_text(size=15),axis.text.x=element_text(angle=90,size=15),axis.text.y=element_text(size=15))+
  theme(legend.title=element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

e=NULL
for(i in 1:length(sido)){
  
  a<-subset(exposure,SIDO==sido[i])
  
  a$lag0_NO2 =a$NO2
  a$lag1_NO2 =lag(a$NO2,1)
  a$lag2_NO2 =lag(a$NO2,2)
  a$lag3_NO2 =lag(a$NO2,3)
  a$lag4_NO2 =lag(a$NO2,4)
  a$lag5_NO2 =lag(a$NO2,5)
  a$lag6_NO2 =lag(a$NO2,6)
  a$lag7_NO2 =lag(a$NO2,7)
  a$lag8_NO2 =lag(a$NO2,8)
  a$lag9_NO2 =lag(a$NO2,9)
  a$lag10_NO2=lag(a$NO2,10)
  a$lag11_NO2=lag(a$NO2,11)
  a$lag12_NO2=lag(a$NO2,12)
  a$lag13_NO2=lag(a$NO2,13)
  a$lag14_NO2=lag(a$NO2,14)
  a$lag15_NO2=lag(a$NO2,15)
  a$lag16_NO2=lag(a$NO2,16)
  a$lag17_NO2=lag(a$NO2,17)
  a$lag18_NO2=lag(a$NO2,18)
  a$lag19_NO2=lag(a$NO2,19)
  a$lag20_NO2=lag(a$NO2,20)
  a$lag21_NO2=lag(a$NO2,21)
  a$lag22_NO2=lag(a$NO2,22)
  a$lag23_NO2=lag(a$NO2,23)
  a$lag24_NO2=lag(a$NO2,24)
  a$lag25_NO2=lag(a$NO2,25)
  a$lag26_NO2=lag(a$NO2,26)
  a$lag27_NO2=lag(a$NO2,27)
  a$lag28_NO2=lag(a$NO2,28)
  a$lag29_NO2=lag(a$NO2,29)
  a$lag30_NO2=lag(a$NO2,30)
  
  a$ma01_NO2  = apply(a %>%  select(lag0_NO2:lag1_NO2),1,mean,na.rm=T)
  a$ma02_NO2  = apply(a %>%  select(lag0_NO2:lag2_NO2),1,mean,na.rm=T)
  a$ma03_NO2  = apply(a %>%  select(lag0_NO2:lag3_NO2),1,mean,na.rm=T)
  a$ma04_NO2  = apply(a %>%  select(lag0_NO2:lag4_NO2),1,mean,na.rm=T)
  a$ma05_NO2  = apply(a %>%  select(lag0_NO2:lag5_NO2),1,mean,na.rm=T)
  a$ma06_NO2  = apply(a %>%  select(lag0_NO2:lag6_NO2),1,mean,na.rm=T)
  a$ma07_NO2  = apply(a %>%  select(lag0_NO2:lag7_NO2),1,mean,na.rm=T)
  a$ma08_NO2  = apply(a %>%  select(lag0_NO2:lag8_NO2),1,mean,na.rm=T)
  a$ma09_NO2  = apply(a %>%  select(lag0_NO2:lag9_NO2),1,mean,na.rm=T)
  a$ma010_NO2 = apply(a %>%  select(lag0_NO2:lag10_NO2),1,mean,na.rm=T)
  a$ma011_NO2 = apply(a %>%  select(lag0_NO2:lag11_NO2),1,mean,na.rm=T)
  a$ma012_NO2 = apply(a %>%  select(lag0_NO2:lag12_NO2),1,mean,na.rm=T)
  a$ma013_NO2 = apply(a %>%  select(lag0_NO2:lag13_NO2),1,mean,na.rm=T)
  a$ma014_NO2 = apply(a %>%  select(lag0_NO2:lag14_NO2),1,mean,na.rm=T)
  a$ma015_NO2 = apply(a %>%  select(lag0_NO2:lag15_NO2),1,mean,na.rm=T)
  a$ma016_NO2 = apply(a %>%  select(lag0_NO2:lag16_NO2),1,mean,na.rm=T)
  a$ma017_NO2 = apply(a %>%  select(lag0_NO2:lag17_NO2),1,mean,na.rm=T)
  a$ma018_NO2 = apply(a %>%  select(lag0_NO2:lag18_NO2),1,mean,na.rm=T)
  a$ma019_NO2 = apply(a %>%  select(lag0_NO2:lag19_NO2),1,mean,na.rm=T)
  a$ma020_NO2 = apply(a %>%  select(lag0_NO2:lag20_NO2),1,mean,na.rm=T)
  a$ma021_NO2 = apply(a %>%  select(lag0_NO2:lag21_NO2),1,mean,na.rm=T)
  a$ma022_NO2 = apply(a %>%  select(lag0_NO2:lag22_NO2),1,mean,na.rm=T)
  a$ma023_NO2 = apply(a %>%  select(lag0_NO2:lag23_NO2),1,mean,na.rm=T)
  a$ma024_NO2 = apply(a %>%  select(lag0_NO2:lag24_NO2),1,mean,na.rm=T)
  a$ma025_NO2 = apply(a %>%  select(lag0_NO2:lag25_NO2),1,mean,na.rm=T)
  a$ma026_NO2 = apply(a %>%  select(lag0_NO2:lag26_NO2),1,mean,na.rm=T)
  a$ma027_NO2 = apply(a %>%  select(lag0_NO2:lag27_NO2),1,mean,na.rm=T)
  a$ma028_NO2 = apply(a %>%  select(lag0_NO2:lag28_NO2),1,mean,na.rm=T)
  a$ma029_NO2 = apply(a %>%  select(lag0_NO2:lag29_NO2),1,mean,na.rm=T)
  a$ma030_NO2 = apply(a %>%  select(lag0_NO2:lag30_NO2),1,mean,na.rm=T)
  
  e[[i]]<-a
  print(i)}

e4<-as.data.frame(do.call(rbind,e))

e22<-e2 %>%  select(date,lag0_PM10:ma030_PM10)
e33<-e3 %>%  select(date,lag0_SO2:ma030_SO2)
e44<-e4 %>%  select(date,lag0_NO2:ma030_NO2)
e55<-e5 %>%  select(date,lag0_CO:ma030_CO)
e66<-e6 %>%  select(date,lag0_O3:ma030_O3)

exposure_m<-cbind(e1,
                  e2 %>%  select(lag0_PM10:ma030_PM10),
                  e3 %>%  select(lag0_SO2:ma030_SO2),
                  e4 %>%  select(lag0_NO2:ma030_NO2),
                  e5 %>%  select(lag0_CO:ma030_CO),
                  e6 %>%  select(lag0_O3:ma030_O3))


# setwd("C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_소아_LRI\\analysis")
# write.csv(exposure_m,file="ap.csv",row.names=F)

exposure_m$key=paste0(exposure_m$date,"-",exposure_m$SIDO)
exposure_m<-exposure_m %>%  select(-c(date,SIDO))

s1<- s1 %>%  select(key:dow,dow)
s2<- s2 %>%  select(key:dow,dow)
s3<- s3 %>%  select(key:dow,dow)
s4<- s4 %>%  select(key:dow,dow)
s5<- s5 %>%  select(key:dow,dow)
s6<- s6 %>%  select(key:dow,dow)
s7<- s7 %>%  select(key:dow,dow)

s1.m<-merge(s1,exposure_m,by="key",all.x=T)
s2.m<-merge(s2,exposure_m,by="key",all.x=T)
s3.m<-merge(s3,exposure_m,by="key",all.x=T)
s4.m<-merge(s4,exposure_m,by="key",all.x=T)
s5.m<-merge(s5,exposure_m,by="key",all.x=T)
s6.m<-merge(s6,exposure_m,by="key",all.x=T)
s7.m<-merge(s7,exposure_m,by="key",all.x=T)

# write.csv(s1.m,file="s1.csv",row.names=F)
# write.csv(s2.m,file="s2.csv",row.names=F)
# write.csv(s3.m,file="s3.csv",row.names=F)
# write.csv(s4.m,file="s4.csv",row.names=F)
# write.csv(s5.m,file="s5.csv",row.names=F)
# write.csv(s6.m,file="s6.csv",row.names=F)
# write.csv(s7.m,file="s7.csv",row.names=F)
################################################################################################################################
################################################################################################################################
setwd("C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_소아_LRI\\analysis")

s1<-read.csv("s1.csv",header=T)
s2<-read.csv("s2.csv",header=T)
s3<-read.csv("s3.csv",header=T)
s4<-read.csv("s4.csv",header=T)
s5<-read.csv("s5.csv",header=T)
s6<-read.csv("s6.csv",header=T)
s7<-read.csv("s7.csv",header=T)

s1$date=as.Date(s1$date)
s2$date=as.Date(s2$date)
s3$date=as.Date(s3$date)
s4$date=as.Date(s4$date)
s5$date=as.Date(s5$date)
s6$date=as.Date(s6$date)
s7$date=as.Date(s7$date)
###################################################################################################################################
###################################################################################################################################
fit01=NULL; fit02=NULL; fit03=NULL;fit04=NULL; fit05=NULL; fit06=NULL;fit07=NULL; 
for(i in 1:366){
  
  fit01[[i]]<-summary(gam(AGE0005  ~s1[,15+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s1))$'p.table'[2,]
  fit02[[i]]<-summary(gam(AGE0005  ~s2[,15+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s2))$'p.table'[2,]
  fit03[[i]]<-summary(gam(AGE0005  ~s3[,15+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s3))$'p.table'[2,]
  fit04[[i]]<-summary(gam(AGE0005  ~s4[,15+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s4))$'p.table'[2,]
  fit05[[i]]<-summary(gam(AGE0005  ~s5[,15+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s5))$'p.table'[2,]
  fit06[[i]]<-summary(gam(AGE0005  ~s6[,15+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s6))$'p.table'[2,]
  fit07[[i]]<-summary(gam(AGE0005  ~s7[,15+i]+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s7))$'p.table'[2,]
  print(i)}

s1.fit<-as.data.frame(do.call(rbind,fit01));s1.fit$lag=names(s1)[16:381]
s2.fit<-as.data.frame(do.call(rbind,fit02));s2.fit$lag=names(s1)[16:381]
s3.fit<-as.data.frame(do.call(rbind,fit03));s3.fit$lag=names(s1)[16:381]
s4.fit<-as.data.frame(do.call(rbind,fit04));s4.fit$lag=names(s1)[16:381]
s5.fit<-as.data.frame(do.call(rbind,fit05));s5.fit$lag=names(s1)[16:381]
s6.fit<-as.data.frame(do.call(rbind,fit06));s6.fit$lag=names(s1)[16:381]
s7.fit<-as.data.frame(do.call(rbind,fit07));s7.fit$lag=names(s1)[16:381]

s1.fit$SIDO="Seoul"   ; s2.fit$SIDO="Busan"
s3.fit$SIDO="Daegu"   ; s4.fit$SIDO="Incheon"
s5.fit$SIDO="Gwnagju" ; s6.fit$SIDO="Daejeon"
s7.fit$SIDO="Ulsan"

s.fit<-rbind(s1.fit,s2.fit,s3.fit,s4.fit,s5.fit,s6.fit,s7.fit)

s.fit$RR=exp(s.fit$Estimate)
s.fit$lci=exp(s.fit$Estimate-1.96*s.fit$`Std. Error`)
s.fit$uci=exp(s.fit$Estimate+1.96*s.fit$`Std. Error`)

s.fit$lag=gsub("pm","lag0",s.fit$lag)
s.fit$lag=factor(s.fit$lag,levels=unique(s.fit$lag))
s.fit$SIDO=factor(s.fit$SIDO,levels=unique(s.fit$SIDO))

s.fit$exposure=s.fit$lag

s.fit$exposure=c(rep("PM25",61),rep("PM10",61),rep("SO2",61),rep("NO2",61),rep("CO",61),rep("O3",61))
s.fit$gubun=c(rep("single",31),rep("moving",30))
s.fit$obs=factor(c(c(0:30),paste0(0,c(1:30))),levels=c(c(0:30),paste0(0,c(1:30))))
s.fit$SIDO=factor(s.fit$SIDO,levels=unique(s.fit$SIDO))

s.fit<-s.fit
# write.csv(s.fit,file="s.fit.csv",row.names=F)
setwd("D:\\EUMC\\논문\\연구논문\\SNU_PM2.5(미세먼지 인체건강유해평가)\\미세먼지_소아_LRI\\analysis")
list.files()
s.fit<-read.csv("s.fit.csv")
s.fit$obs=factor(c(c(0:30),paste0(0,c(1:30))),levels=c(c(0:30),paste0(0,c(1:30))))
s.fit$SIDO=factor(s.fit$SIDO,levels=unique(s.fit$SIDO))
s.fit$pc=round((s.fit$RR-1)*100,2)
s.fit$pcl=round((s.fit$lci-1)*100,2)
s.fit$pcu=round((s.fit$uci-1)*100,2)

s.fit.e1<-subset(s.fit,gubun=="single" & exposure=="PM25" & obs %in% (0:30) )
s.fit.e2<-subset(s.fit,gubun=="single" & exposure=="PM10" & obs %in% (0:10) )
s.fit.e3<-subset(s.fit,gubun=="single" & exposure=="SO2" & obs %in% (0:10) )
s.fit.e4<-subset(s.fit,gubun=="single" & exposure=="NO2" & obs %in% (0:10) )
s.fit.e5<-subset(s.fit,gubun=="single" & exposure=="CO" & obs %in% (0:10) )
s.fit.e6<-subset(s.fit,gubun=="single" & exposure=="O3" & obs %in% (0:10) )

x11();ggplot(s.fit.e1,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)+ylim(0.97,1.03)
x11();ggplot(s.fit.e2,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)+ylim(0.97,1.03)
x11();ggplot(s.fit.e3,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)+ylim(0.97,1.03)
x11();ggplot(s.fit.e4,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)+ylim(0.97,1.03)
x11();ggplot(s.fit.e5,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)+ylim(0.97,1.03)
x11();ggplot(s.fit.e6,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)+ylim(0.97,1.03)

x11();ggplot(s.fit.e1,aes(obs,pc))+geom_point(size=2)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.4)+theme_gray(base_size=15)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 0,col="red",linetype=2,size=0.9)+labs(y="Percent change (%)",x="Lag days")
x11();ggplot(s.fit.e2,aes(obs,pc))+geom_point(size=2)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 0,col="red",linetype=2,size=0.9)+labs(y="Percent change (%)")
x11();ggplot(s.fit.e3,aes(obs,pc))+geom_point(size=2)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 0,col="red",linetype=2,size=0.9)+labs(y="Percent change (%)")
x11();ggplot(s.fit.e4,aes(obs,pc))+geom_point(size=2)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 0,col="red",linetype=2,size=0.9)+labs(y="Percent change (%)")
x11();ggplot(s.fit.e5,aes(obs,pc))+geom_point(size=2)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 0,col="red",linetype=2,size=0.9)+labs(y="Percent change (%)")
x11();ggplot(s.fit.e6,aes(obs,pc))+geom_point(size=2)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 0,col="red",linetype=2,size=0.9)+labs(y="Percent change (%)")

m.fit.e1<-subset(s.fit,gubun=="moving" & exposure=="PM25"  & obs %in% c(paste0(0,1:30)))
m.fit.e2<-subset(s.fit,gubun=="moving" & exposure=="PM10" & obs  %in% c(paste0(0,1:10)))
m.fit.e3<-subset(s.fit,gubun=="moving" & exposure=="SO2" & obs   %in% c(paste0(0,1:10)))
m.fit.e4<-subset(s.fit,gubun=="moving" & exposure=="NO2" & obs   %in% c(paste0(0,1:10)))
m.fit.e5<-subset(s.fit,gubun=="moving" & exposure=="CO" & obs    %in% c(paste0(0,1:10)))
m.fit.e6<-subset(s.fit,gubun=="moving" & exposure=="O3" & obs    %in% c(paste0(0,1:10)))

# write.csv(s.fit.e1,file="C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_소아_LRI\\analysis\\result\\s.fit.e1.csv",row.names=F)
# write.csv(m.fit.e1,file="C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_소아_LRI\\analysis\\result\\m.fit.e1.csv",row.names=F)

x11();ggplot(m.fit.e1,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)
x11();ggplot(m.fit.e2,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)
x11();ggplot(m.fit.e3,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)
x11();ggplot(m.fit.e4,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)
x11();ggplot(m.fit.e5,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)
x11();ggplot(m.fit.e6,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)

x11();ggplot(m.fit.e1,aes(obs,pc))+geom_point(size=2)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.4)+theme_gray(base_size=15)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 0,col="red",linetype=2,size=0.9)+labs(y="Percent change (%)",x="Lag days ")
x11();ggplot(m.fit.e2,aes(obs,pc))+geom_point(size=2)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 0,col="red",linetype=2,size=0.9)+labs(y="Percent change (%)")
x11();ggplot(m.fit.e3,aes(obs,pc))+geom_point(size=2)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 0,col="red",linetype=2,size=0.9)+labs(y="Percent change (%)")
x11();ggplot(m.fit.e4,aes(obs,pc))+geom_point(size=2)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 0,col="red",linetype=2,size=0.9)+labs(y="Percent change (%)")
x11();ggplot(m.fit.e5,aes(obs,pc))+geom_point(size=2)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 0,col="red",linetype=2,size=0.9)+labs(y="Percent change (%)")
x11();ggplot(m.fit.e6,aes(obs,pc))+geom_point(size=2)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 0,col="red",linetype=2,size=0.9)+labs(y="Percent change (%)")

######################################################################################################################################################
######################################################################################################################################################
e1.s00<-subset(s.fit.e1,obs=="0")
e1.s01<-subset(s.fit.e1,obs=="1")
e1.s02<-subset(s.fit.e1,obs=="2")
e1.s03<-subset(s.fit.e1,obs=="3")
e1.s04<-subset(s.fit.e1,obs=="4")
e1.s05<-subset(s.fit.e1,obs=="5")
e1.s06<-subset(s.fit.e1,obs=="6")
e1.s07<-subset(s.fit.e1,obs=="7")
e1.s08<-subset(s.fit.e1,obs=="8")
e1.s09<-subset(s.fit.e1,obs=="9")
e1.s010<-subset(s.fit.e1,obs=="10")
e1.s011<-subset(s.fit.e1,obs=="11")
e1.s012<-subset(s.fit.e1,obs=="12")
e1.s013<-subset(s.fit.e1,obs=="13")
e1.s014<-subset(s.fit.e1,obs=="14")

e1.m01<-subset(m.fit.e1,obs=="01")
e1.m02<-subset(m.fit.e1,obs=="02")
e1.m03<-subset(m.fit.e1,obs=="03")
e1.m04<-subset(m.fit.e1,obs=="04")
e1.m05<-subset(m.fit.e1,obs=="05")
e1.m06<-subset(m.fit.e1,obs=="06")
e1.m07<-subset(m.fit.e1,obs=="07")
e1.m08<-subset(m.fit.e1,obs=="08")
e1.m09<-subset(m.fit.e1,obs=="09")
e1.m010<-subset(m.fit.e1,obs=="010")
e1.m011<-subset(m.fit.e1,obs=="011")
e1.m012<-subset(m.fit.e1,obs=="012")
e1.m013<-subset(m.fit.e1,obs=="013")
e1.m014<-subset(m.fit.e1,obs=="014")

uni0 <- with(e1.s00,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni1 <- with(e1.s01,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni2 <- with(e1.s02,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni3 <- with(e1.s03,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni4 <- with(e1.s04,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni5 <- with(e1.s05,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni6 <- with(e1.s06,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni7 <- with(e1.s07,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni8 <- with(e1.s08,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni9 <- with(e1.s09,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni10 <- with(e1.s010,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni11 <- with(e1.s011,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni12 <- with(e1.s012,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni13 <- with(e1.s013,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni14 <- with(e1.s014,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))

uni01 <- with(e1.m01,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni02 <- with(e1.m02,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni03 <- with(e1.m03,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni04 <- with(e1.m04,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni05 <- with(e1.m05,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni06 <- with(e1.m06,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni07 <- with(e1.m07,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni08 <- with(e1.m08,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni09 <- with(e1.m09,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni010 <- with(e1.m010,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni011 <- with(e1.m011,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni012 <- with(e1.m012,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni013 <- with(e1.m013,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))
uni014 <- with(e1.m014,rma(yi=Estimate, sei=Std..Error, slab=SIDO, measure="RR",digits=5,method="ML"))

df<-rbind(data.frame(b=uni0$b,se=uni0$se),
          data.frame(b=uni1$b,se=uni1$se),
          data.frame(b=uni2$b,se=uni2$se),
          data.frame(b=uni3$b,se=uni3$se),
          data.frame(b=uni4$b,se=uni4$se),
          data.frame(b=uni5$b,se=uni5$se),
          data.frame(b=uni6$b,se=uni6$se),
          data.frame(b=uni7$b,se=uni7$se),
          data.frame(b=uni01$b,se=uni01$se),
          data.frame(b=uni02$b,se=uni02$se),
          data.frame(b=uni03$b,se=uni03$se),
          data.frame(b=uni04$b,se=uni04$se),
          data.frame(b=uni05$b,se=uni05$se),
          data.frame(b=uni06$b,se=uni06$se),
          data.frame(b=uni07$b,se=uni07$se))

df$pc=(exp(df$b)-1)*100
df$pcl=round((exp(df$b-1.96*df$se)-1)*100,2)
df$pcu=round((exp(df$b+1.96*df$se)-1)*100,2)

df$obs=c(paste0("Lag",c(0:7)),paste0("Lag",0,1:7))
df$obs=factor(df$obs,levels=unique(df$obs))

x11();ggplot(df,aes(obs,pc))+geom_point(size=4.5,col=c(rep("black",13),"red","black"))+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.2)+theme_minimal(base_size=20)+labs(x="Lag days",y="Percent change (%)")+
  geom_hline(yintercept = 0,col="blue",linetype="dashed",lwd=1.4)+
  theme(legend.title=element_blank(),panel.border = element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

tiff(filename="D:\\EUMC\\논문\\연구논문\\SNU_PM2.5(미세먼지 인체건강유해평가)\\미세먼지_소아_LRI\\figure\\lagdays.tiff",width=5200,height=3200,res=300)
ggplot(df,aes(obs,pc))+geom_point(size=4.5,col=c(rep("black",13),"red","black"))+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.2)+theme_minimal(base_size=22)+labs(x="Lag days",y="Percent change (%)")+
  geom_hline(yintercept = 0,col="blue",linetype="dashed",lwd=1.4)+
  theme(legend.title=element_blank(),panel.border = element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()



df2<-df[9:15,]
x11();ggplot(df2,aes(obs,pc))+geom_point(size=3)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.2)+theme_gray(base_size=20)+labs(x="Lag days",y="Percent change (%)")+
  geom_hline(yintercept = 0,col="blue",linetype="dashed",lwd=1.4)

mytransf <- function(x)
  (exp(x) - 1) * 100

x11();forest(uni01,transf=mytransf,rows=c(7:1),refline=0, bg=4, col=2,cex.lab=1.1,cex.axis=1.1,cex=1.1,fontsize=1,digits=2,main="",psize=1,mlab="RE Model for Total")
x11();forest(uni02,transf=mytransf,rows=c(7:1),refline=0, bg=4, col=2,cex.lab=1.1,cex.axis=1.1,cex=1.1,fontsize=1,digits=2,main="",psize=1,mlab="RE Model for Total")
x11();forest(uni03,transf=mytransf,rows=c(7:1),refline=0, bg=4, col=2,cex.lab=1.1,cex.axis=1.1,cex=1.1,fontsize=1,digits=2,main="",psize=1,mlab="RE Model for Total")
x11();forest(uni04,transf=mytransf,rows=c(7:1),refline=0, bg=4, col=2,cex.lab=1.1,cex.axis=1.1,cex=1.1,fontsize=1,digits=2,main="",psize=1,mlab="RE Model for Total")
x11();forest(uni05,transf=mytransf,rows=c(7:1),refline=0, bg=4, col=2,cex.lab=1.1,cex.axis=1.1,cex=1.1,fontsize=1,digits=2,main="",psize=1,mlab="RE Model for Total")
x11();forest(uni06,transf=mytransf,rows=c(7:1),refline=0, bg=4, col=2,cex.lab=1.1,cex.axis=1.1,cex=1.1,fontsize=1,digits=2,main="",psize=1,mlab="RE Model for Total")
x11();forest(uni07,transf=mytransf,rows=c(7:1),refline=0, bg=4, col=2,cex.lab=1.1,cex.axis=1.1,cex=1.1,fontsize=1,digits=2,main="",psize=1,mlab="RE Model for Total")
#####################################################################################################################################################################
#####################################################################################################################################################################

r1<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s1))$'p.table'[2,]
r2<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s2))$'p.table'[2,]
r3<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s3))$'p.table'[2,]
r4<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s4))$'p.table'[2,]
r5<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s5))$'p.table'[2,]
r6<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s6))$'p.table'[2,]
r7<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s7))$'p.table'[2,]
total<-as.data.frame(rbind(r1,r2,r3,r4,r5,r6,r7))

r11<-summary(gam(SEX_M0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s1))$'p.table'[2,]
r22<-summary(gam(SEX_M0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s2))$'p.table'[2,]
r33<-summary(gam(SEX_M0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s3))$'p.table'[2,]
r44<-summary(gam(SEX_M0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s4))$'p.table'[2,]
r55<-summary(gam(SEX_M0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s5))$'p.table'[2,]
r66<-summary(gam(SEX_M0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s6))$'p.table'[2,]
r77<-summary(gam(SEX_M0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s7))$'p.table'[2,]
total_m<-as.data.frame(rbind(r11,r22,r33,r44,r55,r66,r77))

r111<-summary(gam(SEX_F0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s1))$'p.table'[2,]
r222<-summary(gam(SEX_F0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s2))$'p.table'[2,]
r333<-summary(gam(SEX_F0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s3))$'p.table'[2,]
r444<-summary(gam(SEX_F0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s4))$'p.table'[2,]
r555<-summary(gam(SEX_F0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s5))$'p.table'[2,]
r666<-summary(gam(SEX_F0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s6))$'p.table'[2,]
r777<-summary(gam(SEX_F0005  ~ma06_PM25+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s7))$'p.table'[2,]
total_f<-as.data.frame(rbind(r111,r222,r333,r444,r555,r666,r777))

s1$month=month(s1$date);s1.w<- subset(s1,month %in% c(4:9));s1.c<- subset(s1,month %in% c(1:3,10:12))
s2$month=month(s2$date);s2.w<- subset(s2,month %in% c(4:9));s2.c<- subset(s2,month %in% c(1:3,10:12))
s3$month=month(s3$date);s3.w<- subset(s3,month %in% c(4:9));s3.c<- subset(s3,month %in% c(1:3,10:12))
s4$month=month(s4$date);s4.w<- subset(s4,month %in% c(4:9));s4.c<- subset(s4,month %in% c(1:3,10:12))
s5$month=month(s5$date);s5.w<- subset(s5,month %in% c(4:9));s5.c<- subset(s5,month %in% c(1:3,10:12))
s6$month=month(s6$date);s6.w<- subset(s6,month %in% c(4:9));s6.c<- subset(s6,month %in% c(1:3,10:12))
s7$month=month(s7$date);s7.w<- subset(s7,month %in% c(4:9));s7.c<- subset(s7,month %in% c(1:3,10:12))

r1.w<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s1.w))$'p.table'[2,]
r2.w<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s2.w))$'p.table'[2,]
r3.w<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s3.w))$'p.table'[2,]
r4.w<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s4.w))$'p.table'[2,]
r5.w<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s5.w))$'p.table'[2,]
r6.w<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s6.w))$'p.table'[2,]
r7.w<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s7.w))$'p.table'[2,]
total.w<-as.data.frame(rbind(r1.w,r2.w,r3.w,r4.w,r5.w,r6.w,r7.w))

r1.c<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s1.c))$'p.table'[2,]
r2.c<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s2.c))$'p.table'[2,]
r3.c<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s3.c))$'p.table'[2,]
r4.c<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s4.c))$'p.table'[2,]
r5.c<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s5.c))$'p.table'[2,]
r6.c<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s6.c))$'p.table'[2,]
r7.c<-summary(gam(AGE0005  ~ma06_PM25+s(as.numeric(date),k=5*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s7.c))$'p.table'[2,]
total.c<-as.data.frame(rbind(r1.c,r2.c,r3.c,r4.c,r5.c,r6.c,r7.c))

a<-gam(AGE0005  ~lag06+s(as.numeric(date),k=3*9,fx=T)+s(AT,k=6,fx=T)+dow+s(yday(date),k=4),family="poisson",data=dat.s1.w)

# x11();plot(a)

exp(total[,1])
exp(total.w[,1])
exp(total.c[,1])


total$SIDO  =c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan")
total_m$SIDO=c("Seoul ","Busan ","Daegu ","Incheon ","Gwangju ","Daejeon ","Ulsan ")
total_f$SIDO=c("Seoul  ","Busan  ","Daegu  ","Incheon  ","Gwangju  ","Daejeon  ","Ulsan  ")
total.w$SIDO=c("Seoul   ","Busan   ","Daegu   ","Incheon   ","Gwangju   ","Daejeon   ","Ulsan   ")
total.c$SIDO=c("Seoul    ","Busan    ","Daegu    ","Incheon    ","Gwangju    ","Daejeon    ","Ulsan    ")

total$g="Total"
total_m$g="Boys"
total_f$g="Girs"
total.w$g="Warm"
total.c$g="Cold"

u<-rbind(total,total_m,total_f,total.w,total.c)
u$pc=(exp(u$Estimate)-1)*100
u$pcl=(exp(u$Estimate-1.96*u$`Std. Error`)-1)*100
u$pcu=(exp(u$Estimate+1.96*u$`Std. Error`)-1)*100

RM.res<-rma(yi=u$Estimate,sei=u$`Std. Error`,data=u,slab=u$SIDO,digits=5)

uni01 <- with(total  ,rma(yi=Estimate, sei=`Std. Error`, slab=SIDO  , measure="RR",digits=5))
uni02 <- with(total_m,rma(yi=Estimate, sei=`Std. Error`, slab=SIDO, measure="RR",digits=5))
uni03 <- with(total_f,rma(yi=Estimate, sei=`Std. Error`, slab=SIDO, measure="RR",digits=5))
uni04 <- with(total.w,rma(yi=Estimate, sei=`Std. Error`, slab=SIDO, measure="RR",digits=5))
uni05 <- with(total.c,rma(yi=Estimate, sei=`Std. Error`, slab=SIDO, measure="RR",digits=5))

mytransf <- function(x)
  (exp(x) - 1) * 100

x11();par(mar=c(4,4,1,2));

forest(RM.res, xlim=c(-5, 10), ylim=c(6, 72), steps=4, rows=c(67:61,54:48,41:35,28:22,15:9),refline=0,
       order=c(1:nrow(u)),transf = mytransf,bg=4,col=2,digits=2,cex=0.8,xlab="", mlab="")
addpoly(uni01,transf=mytransf,cex=0.85,row=58,col="red",digits=2,mlab=paste0("Random Effect Model for Total: ","\n"," I^2=",round(uni01$I2,2),"%",", H^2=",round(uni01$H2,2),", Q=",round(uni01$QE,2)))
addpoly(uni02,transf=mytransf,cex=0.85,row=45,col="red",digits=2,mlab=paste0("Random Effect Model for Boys: ","\n"," I^2=",round(uni02$I2,2),"%",", H^2=",round(uni02$H2,2),", Q=",round(uni02$QE,2)))
addpoly(uni03,transf=mytransf,cex=0.85,row=32,col="red",digits=2,mlab=paste0("Random Effect Model for Girls: ","\n"," I^2=",round(uni03$I2,2),"%",", H^2=",round(uni03$H2,2),", Q=",round(uni03$QE,2)))
addpoly(uni04,transf=mytransf,cex=0.85,row=19,col="red",digits=2,mlab=paste0("Random Effect Model for Warm season: ","\n"," I^2=",round(uni04$I2,2),"%",", H^2=",round(uni04$H2,2),", Q=",round(uni04$QE,2)))
addpoly(uni05,transf=mytransf,cex=0.85,row=6,col="red",digits=2,mlab=paste0("Random Effect Model for Cold season: ","\n"," I^2=",round(uni05$I2,2),"%",", H^2=",round(uni05$H2,2),", Q=",round(uni05$QE,2)))

text(-5,68.3, pos=4, cex=1.1, paste("Total"),col="blue",font=4)
text(-5,55.3, pos=4, cex=1.1, paste("Boys"),col="blue",font=4)
text(-5,42.3, pos=4, cex=1.1, paste("Girls"),col="blue",font=4)
text(-5,29.3, pos=4, cex=1.1, paste("Warm season"),col="blue",font=4)
text(-5,16.3, pos=4, cex=1.1, paste("Cold season"),col="blue",font=4)

text(-5,71  , pos=4, cex=1.1, paste("Cities"),font=4)
text(6,71 , pos=4, cex=1.1, paste("Percent change [95% CI]"),font=4)

tiff(filename="C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_소아_LRI\\figure\\meta_3.tiff",width=2800,height=4200,res=300)
par(mar=c(4,4,1,2));
forest(RM.res, xlim=c(-5, 10), ylim=c(6, 72), steps=4, rows=c(67:61,54:48,41:35,28:22,15:9),refline=0,
       order=c(1:nrow(u)),transf = mytransf,bg=4,col=2,digits=2,cex=0.8,xlab="", mlab="")
addpoly(uni01,transf=mytransf,cex=0.85,row=58,col="red",digits=2,mlab=paste0("RE Model for Total: ","\n"," I^2=",round(uni01$I2,2),", H^2=",round(uni01$H2,2),", Q=",round(uni01$QE,2)))
addpoly(uni02,transf=mytransf,cex=0.85,row=45,col="red",digits=2,mlab=paste0("RE Model for Boys: ","\n"," I^2=",round(uni02$I2,2),", H^2=",round(uni02$H2,2),", Q=",round(uni02$QE,2)))
addpoly(uni03,transf=mytransf,cex=0.85,row=32,col="red",digits=2,mlab=paste0("RE Model for Girls: ","\n"," I^2=",round(uni03$I2,2),", H^2=",round(uni03$H2,2),", Q=",round(uni03$QE,2)))
addpoly(uni04,transf=mytransf,cex=0.85,row=19,col="red",digits=2,mlab=paste0("RE Model for Warm season: ","\n"," I^2=",round(uni04$I2,2),", H^2=",round(uni04$H2,2),", Q=",round(uni04$QE,2)))
addpoly(uni05,transf=mytransf,cex=0.85,row=6,col="red",digits=2,mlab=paste0("RE Model for Cold season: ","\n"," I^2=",round(uni05$I2,2),", H^2=",round(uni05$H2,2),", Q=",round(uni05$QE,2)))

text(-5,68.3, pos=4, cex=1.1, paste("Total"),col="blue",font=4)
text(-5,55.3, pos=4, cex=1.1, paste("Boys"),col="blue",font=4)
text(-5,42.3, pos=4, cex=1.1, paste("Girls"),col="blue",font=4)
text(-5,29.3, pos=4, cex=1.1, paste("Warm season"),col="blue",font=4)
text(-5,16.3, pos=4, cex=1.1, paste("Cold season"),col="blue",font=4)

text(-5,71  , pos=4, cex=1.1, paste("Cities"),font=4)
text(6,71 , pos=4, cex=1.1, paste("Percent change [95% CI]"),font=4)
dev.off()

######################################################################################################################################################
######################################################################################################################################################
#two-pollutant modelling
e01=paste0(names(s1)[grep("PM25",names(s1))][c(2:9,33:39)],"+",names(s1)[grep("PM10",names(s1))[c(2:9,33:39)]])
e02=paste0(names(s1)[grep("PM25",names(s1))][c(2:9,33:39)],"+",names(s1)[grep("SO2",names(s1))[c(2:9,33:39)]])
e03=paste0(names(s1)[grep("PM25",names(s1))][c(2:9,33:39)],"+",names(s1)[grep("CO",names(s1))[c(2:9,33:39)]])
e04=paste0(names(s1)[grep("PM25",names(s1))][c(2:9,33:39)],"+",names(s1)[grep("NO",names(s1))[c(2:9,33:39)]])
e05=paste0(names(s1)[grep("PM25",names(s1))][c(2:9,33:39)],"+",names(s1)[grep("O3",names(s1))[c(2:9,33:39)]])

ee<-c(e01,e02,e03,e04,e05)
eee<-paste0("AGE0005","~",ee,"+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow")

vif(gam(formula(eee[i]),family="poisson",data=s1))

concurvity(gam(formula(eee[i]),family="poisson",data=s1))


fit01=NULL; fit02=NULL; fit03=NULL;fit04=NULL; fit05=NULL; fit06=NULL;fit07=NULL; 
for(i in 1:75){
  
  fit01[[i]]<-summary(gam(formula(eee[i]),family="poisson",data=s1))$'p.table'[2,]
  fit02[[i]]<-summary(gam(formula(eee[i]),family="poisson",data=s2))$'p.table'[2,]
  fit03[[i]]<-summary(gam(formula(eee[i]),family="poisson",data=s3))$'p.table'[2,]
  fit04[[i]]<-summary(gam(formula(eee[i]),family="poisson",data=s4))$'p.table'[2,]
  fit05[[i]]<-summary(gam(formula(eee[i]),family="poisson",data=s5))$'p.table'[2,]
  fit06[[i]]<-summary(gam(formula(eee[i]),family="poisson",data=s6))$'p.table'[2,]
  fit07[[i]]<-summary(gam(formula(eee[i]),family="poisson",data=s7))$'p.table'[2,]
  print(i)}

t1.fit<-as.data.frame(do.call(rbind,fit01));t1.fit$lag=c(0:7,paste0(0,1:7))
t2.fit<-as.data.frame(do.call(rbind,fit02));t2.fit$lag=c(0:7,paste0(0,1:7))
t3.fit<-as.data.frame(do.call(rbind,fit03));t3.fit$lag=c(0:7,paste0(0,1:7))
t4.fit<-as.data.frame(do.call(rbind,fit04));t4.fit$lag=c(0:7,paste0(0,1:7))
t5.fit<-as.data.frame(do.call(rbind,fit05));t5.fit$lag=c(0:7,paste0(0,1:7))
t6.fit<-as.data.frame(do.call(rbind,fit06));t6.fit$lag=c(0:7,paste0(0,1:7))
t7.fit<-as.data.frame(do.call(rbind,fit07));t7.fit$lag=c(0:7,paste0(0,1:7))

t1.fit$SIDO="Seoul"   ; t2.fit$SIDO="Busan"
t3.fit$SIDO="Daegu"   ; t4.fit$SIDO="Incheon"
t5.fit$SIDO="Gwnagju" ; t6.fit$SIDO="Daejeon"
t7.fit$SIDO="Ulsan"

t.fit<-rbind(t1.fit,t2.fit,t3.fit,t4.fit,t5.fit,t6.fit,t7.fit)

t.fit$RR =exp(t.fit$Estimate)
t.fit$lci=exp(t.fit$Estimate-1.96*t.fit$`Std. Error`)
t.fit$uci=exp(t.fit$Estimate+1.96*t.fit$`Std. Error`)

t.fit$lag=gsub("pm","lag0",t.fit$lag)
t.fit$lag =factor(t.fit$lag,levels =unique(t.fit$lag))
t.fit$SIDO=factor(t.fit$SIDO,levels=unique(t.fit$SIDO))

t.fit$exposure=t.fit$lag

t.fit$exposure=ee
t.fit$gubun=c(rep("single",8),rep("moving",7))
t.fit$obs=factor(c(c(0:7),paste0(0,c(1:7))),levels=c(c(0:7),paste0(0,c(1:7))))
t.fit$SIDO=factor(t.fit$SIDO,levels=unique(t.fit$SIDO))
t.fit$category=c(rep("PM25_PM10",each=15),rep("PM25_SO2",each=15),rep("PM25_CO",each=15),rep("PM25_NO",each=15),rep("PM25_O3",each=15))

t.fit.1<-subset(t.fit,category=="PM25_PM10" & gubun=="moving" & lag=="01")
t.fit.2<-subset(t.fit,category=="PM25_SO2"  & gubun=="moving" & lag=="01")
t.fit.3<-subset(t.fit,category=="PM25_CO"   & gubun=="moving" & lag=="01")
t.fit.4<-subset(t.fit,category=="PM25_NO"   & gubun=="moving" & lag=="06")
t.fit.5<-subset(t.fit,category=="PM25_O3"   & gubun=="moving" & lag=="01")

a1<-with(t.fit.1,rma(yi=Estimate, sei=`Std. Error`, slab=SIDO  , measure="RR",digits=5))
a2<-with(t.fit.2,rma(yi=Estimate, sei=`Std. Error`, slab=SIDO  , measure="RR",digits=5))
a3<-with(t.fit.3,rma(yi=Estimate, sei=`Std. Error`, slab=SIDO  , measure="RR",digits=5))
a4<-with(t.fit.4,rma(yi=Estimate, sei=`Std. Error`, slab=SIDO  , measure="RR",digits=5))
a5<-with(t.fit.5,rma(yi=Estimate, sei=`Std. Error`, slab=SIDO  , measure="RR",digits=5))

func=function (x){data.frame(beta=x$beta,
                             se=x$se,
                             pc=(exp(x$beta)-1)*100,
                             pcl=(exp(x$beta-1.96*x$se)-1)*100,
                             pcu=(exp(x$beta+1.96*x$se)-1)*100)}

two01<-round(rbind(func(a1),func(a2),func(a3),func(a4),func(a5)),2)

two_df=rbind(two01,two02,
             two03,two04,
             two05,two06,two07)

two_df$gubun=c("PM25+PM10","PM25+SO2","PM25+CO","PM25+NO2","PM25+O3")
two_df$lag=paste0(0,rep(1:7,each=5))
View(two_df)

two_df$gubun=factor(two_df$gubun,levels=unique(two_df$gubun))
x11();ggplot(two_df,aes(lag,pc))+geom_point(size=4)+geom_errorbar(aes(ymin=pcl,ymax=pcu),width=0.2)+
  geom_hline(yintercept=0,col="red",size=0.9,linetype=2)+facet_wrap(~gubun,scales="free")+theme_gray(base_size=15)+labs(x="Lag days (moving average)",y="Percent change (%)")

x11();ggplot(two_df,aes(lag,pc,group=gubun))+geom_point(size=4,aes(shape=gubun),position=position_dodge(0.5))+geom_errorbar(aes(ymin=pcl,ymax=pcu),position=position_dodge(0.5),width=0.2)

x11();ggplot(t.fit.1,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)

x11();ggplot(t.fit.2,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)

x11();ggplot(t.fit.3,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)

x11();ggplot(t.fit.4,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)

x11();ggplot(t.fit.5,aes(obs,RR))+geom_point(size=2)+geom_errorbar(aes(ymin=lci,ymax=uci),width=0.4)+theme_gray(base_size=12)+facet_wrap(~SIDO)+
  geom_hline(yintercept = 1,col="red",linetype=2,size=0.9)

######################################################################################################################################################
######################################################################################################################################################
tt<-dat %>%  select(date:SEX_F0005,lag06,AT,dow)
tt$exposure=tt$lag06*10

tt<-tt[complete.cases(tt),]
tt_gamm4<-gamm4(AGE0005~s(exposure,k=1,fx=T)+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=tt,random=~(1|SIDO))

x11();plot(tt_gamm4$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
           ylab="Log RR");abline(h=0)

tt$exposure2=round(tt$exposure)
ttt<-aggregate(tt$AGE0005,list(tt$SIDO,tt$exposure2),sum)
names(ttt)=c("SIDO","exposure","count")

# x11();ggplot(ttt,aes(exposure,count,fill=SIDO))+geom_bar(stat="identity")

tt$exposure2=round(tt$exposure)
hh<-aggregate(tt$AGE0005,list(tt$exposure2,tt$SIDO),sum,na.rm=T)

names(hh)=c("exposure","SIDO","counts")

h1<-subset(hh,SIDO=="Seoul")
h2<-subset(hh,SIDO=="Busan")   %>% select(-SIDO)
h3<-subset(hh,SIDO=="Daegu")   %>% select(-SIDO)
h4<-subset(hh,SIDO=="Incheon") %>% select(-SIDO)
h5<-subset(hh,SIDO=="Gwangju") %>% select(-SIDO)
h6<-subset(hh,SIDO=="Daejeon") %>% select(-SIDO)
h7<-subset(hh,SIDO=="Ulsan")   %>% select(-SIDO)

exposure=data.frame(exposure=seq(5:99))

m1<-merge(exposure,h1,all.x=T);names(m1)[3]="h1";m1$h1[is.na(m1$h1)]<-0
m2<-merge(m1,h2,all.x=T,by="exposure");names(m2)[4]="h2";m2$h2[is.na(m2$h2)]<-0
m3<-merge(m2,h3,all.x=T,by="exposure");names(m3)[5]="h3";m3$h3[is.na(m3$h3)]<-0
m4<-merge(m3,h4,all.x=T,by="exposure");names(m4)[6]="h4";m4$h4[is.na(m4$h4)]<-0
m5<-merge(m4,h5,all.x=T,by="exposure");names(m5)[7]="h5";m5$h5[is.na(m5$h5)]<-0
m6<-merge(m5,h6,all.x=T,by="exposure");names(m6)[8]="h6";m6$h6[is.na(m6$h6)]<-0
m7<-merge(m6,h7,all.x=T,by="exposure");names(m7)[9]="h7";m7$h7[is.na(m7$h7)]<-0

# Plot overlapping histograms
xrange=seq(min(tt$exposure),max(tt$exposure))

x11();layout(matrix(c(1,1,1,1,2,2),3,2,byrow=T));
plot(tt_gamm4$gam,select=1,scheme=1,ylim=c(-0.1,0.15),ylab="Log RR",xlab="",cex.lab=1.39,cex.axis=1.39);abline(h=0)
barplot(rbind(m7$h1,m7$h2,m7$h3,m7$h4,m7$h5,m7$h6,m7$h7),col=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),
        xlim=c(min(xrange)-1,max(xrange)),space=0,las=2,main="",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),cex.lab=1.39,cex.axis=1.39)
legend("topright",legend=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"),
       fill=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),box.lty=0,cex=1.1)

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_소아_LRI\\figure\\gamm_total_revise.tiff",width=4800,height=2800,res=300)
layout(matrix(c(1,1,1,1,2,2),3,2,byrow=T));
plot(tt_gamm4$gam,select=1,scheme=1,ylim=c(-0.1,0.15),ylab="Log RR",xlab="",cex.lab=1.39,cex.axis=1.39);abline(h=0)
barplot(rbind(m7$h1,m7$h2,m7$h3,m7$h4,m7$h5,m7$h6,m7$h7),col=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),
        xlim=c(min(xrange)-1,max(xrange)),space=0,las=2,main="",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),cex.lab=1.39,cex.axis=1.39)
legend("topright",legend=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"),
       fill=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),box.lty=0,cex=1.1)

dev.off()

x11();plot(0,0,type="n",xlim=c(0,100),ylim=c(0,500),xlab="x",ylab="freq",main="")
plot(p1,col="red",add=T)
plot(p2,col="blue",add=T)

tt$month=month(tt$date)
tt.sea1<-subset(tt,month %in% c(4:9))
tt.sea2<-subset(tt,month %in% c(1:3,10:12))

t_m<-gamm4(SEX_M0005~s(exposure,k=1,fx=T)+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=tt,random=~(1|SIDO))
t_f<-gamm4(SEX_F0005~s(exposure,k=1,fx=T)+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=tt,random=~(1|SIDO))

t_w<-gamm4(AGE0005~s(exposure,k=1,fx=T)+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=tt.sea1,random=~(1|SIDO))
t_c<-gamm4(AGE0005~s(exposure,k=1,fx=T)+s(as.numeric(date),k=4*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=tt.sea2,random=~(1|SIDO))

x11();par(mfrow=c(2,2))
plot(t_m$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
     ylab="Log RR",main="Male");abline(h=0)
plot(t_f$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
     ylab="Log RR",main="Female");abline(h=0)
plot(t_w$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
     ylab="Log RR",main="Warm");abline(h=0)
plot(t_c$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
     ylab="Log RR",main="Cold");abline(h=0)

tt.sea1$exposure2=round(tt.sea1$exposure)
tt.sea2$exposure2=round(tt.sea2$exposure)

hh<-aggregate(tt$SEX_M0005,list(tt$exposure2,tt$SIDO),sum,na.rm=T)
# hh<-aggregate(tt.sea2$AGE0005,list(tt.sea2$exposure2,tt.sea2$SIDO),sum,na.rm=T)
names(hh)=c("exposure","SIDO","counts")
h1<-subset(hh,SIDO=="Seoul")
h2<-subset(hh,SIDO=="Busan")   %>% select(-SIDO)
h3<-subset(hh,SIDO=="Daegu")   %>% select(-SIDO)
h4<-subset(hh,SIDO=="Incheon") %>% select(-SIDO)
h5<-subset(hh,SIDO=="Gwangju") %>% select(-SIDO)
h6<-subset(hh,SIDO=="Daejeon") %>% select(-SIDO)
h7<-subset(hh,SIDO=="Ulsan")   %>% select(-SIDO)

exposure=data.frame(exposure=seq(5:99))

m1<-merge(exposure,h1,all.x=T);names(m1)[3]="h1";m1$h1[is.na(m1$h1)]<-0
m2<-merge(m1,h2,all.x=T,by="exposure");names(m2)[4]="h2";m2$h2[is.na(m2$h2)]<-0
m3<-merge(m2,h3,all.x=T,by="exposure");names(m3)[5]="h3";m3$h3[is.na(m3$h3)]<-0
m4<-merge(m3,h4,all.x=T,by="exposure");names(m4)[6]="h4";m4$h4[is.na(m4$h4)]<-0
m5<-merge(m4,h5,all.x=T,by="exposure");names(m5)[7]="h5";m5$h5[is.na(m5$h5)]<-0
m6<-merge(m5,h6,all.x=T,by="exposure");names(m6)[8]="h6";m6$h6[is.na(m6$h6)]<-0
m7<-merge(m6,h7,all.x=T,by="exposure");names(m7)[9]="h7";m7$h7[is.na(m7$h7)]<-0

q1<-m7
# Plot overlapping histograms

x11();
layout(cbind(matrix(c(1,1,1,1,2,2),3,2,byrow=T),matrix(c(3,3,3,3,4,4),3,2,byrow=T),matrix(c(5,5,5,5,6,6),3,2,byrow=T),matrix(c(7,7,7,7,8,8),3,2,byrow=T)))

plot(t_m$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
     ylab="Log RR",main="Male");abline(h=0)
barplot(rbind(q1$h1,q1$h2,q1$h3,q1$h4,q1$h5,q1$h6,q1$h7),col=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),
        xlim=c(min(xrange)-1,max(xrange)),space=0,las=2,main="",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),cex.lab=1.39,cex.axis=1.39)

plot(t_f$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
     ylab="Log RR",main="Female");abline(h=0)
barplot(rbind(q2$h1,q2$h2,q2$h3,q2$h4,q2$h5,q2$h6,q2$h7),col=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),
        xlim=c(min(xrange)-1,max(xrange)),space=0,las=2,main="",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),cex.lab=1.39,cex.axis=1.39)


plot(t_w$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
     ylab="Log RR",main="Warm");abline(h=0)
barplot(rbind(q3$h1,q3$h2,q3$h3,q3$h4,q3$h5,q3$h6,q3$h7),col=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),
        xlim=c(min(xrange)-1,max(xrange)),space=0,las=2,main="",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),cex.lab=1.39,cex.axis=1.39)

plot(t_c$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
     ylab="Log RR",main="Cold");abline(h=0)
barplot(rbind(q4$h1,q4$h2,q4$h3,q4$h4,q4$h5,q4$h6,q4$h7),col=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),
        xlim=c(min(xrange)-1,max(xrange)),space=0,las=2,main="",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),cex.lab=1.39,cex.axis=1.39)
legend(50,20000,legend=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"),
       fill=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),box.lty=0,cex=1.1)


tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_소아_LRI\\figure\\gamm_sex_season_revise2.tiff",width=4800,height=2800,res=300)
layout(cbind(matrix(c(1,1,1,1,2,2),3,2,byrow=T),matrix(c(3,3,3,3,4,4),3,2,byrow=T),matrix(c(5,5,5,5,6,6),3,2,byrow=T),matrix(c(7,7,7,7,8,8),3,2,byrow=T)))

plot(t_m$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
     ylab="Log RR",main="Male");abline(h=0)
barplot(rbind(q1$h1,q1$h2,q1$h3,q1$h4,q1$h5,q1$h6,q1$h7),col=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),
        xlim=c(min(xrange)-1,max(xrange)),space=0,las=2,main="",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),cex.lab=1.39,cex.axis=1.39)

plot(t_f$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
     ylab="Log RR",main="Female");abline(h=0)
barplot(rbind(q2$h1,q2$h2,q2$h3,q2$h4,q2$h5,q2$h6,q2$h7),col=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),
        xlim=c(min(xrange)-1,max(xrange)),space=0,las=2,main="",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),cex.lab=1.39,cex.axis=1.39)

plot(t_w$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
     ylab="Log RR",main="Warm");abline(h=0)
barplot(rbind(q3$h1,q3$h2,q3$h3,q3$h4,q3$h5,q3$h6,q3$h7),col=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),
        xlim=c(min(xrange)-1,max(xrange)),space=0,las=2,main="",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),cex.lab=1.39,cex.axis=1.39)

plot(t_c$gam,select=1,scheme=1,ylim=c(-0.1,0.15),xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),
     ylab="Log RR",main="Cold");abline(h=0)
barplot(rbind(q4$h1,q4$h2,q4$h3,q4$h4,q4$h5,q4$h6,q4$h7),col=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),
        xlim=c(min(xrange)-1,max(xrange)),space=0,las=2,main="",xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)),cex.lab=1.39,cex.axis=1.39)
legend(50,20000,legend=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"),
       fill=c("#e76bf3","#f37b59","#e7861b","#00b81f","#00c0b8","#00a5ff","#f8766d"),box.lty=0,cex=1.1)
dev.off()

setwd("C:\\Users\\a\\Desktop\\논문\\미세먼지_소아_LRI\\analysis")
ap<-read.csv("ap.csv")

setwd("D:\\이대목동병원\\데이터관리\\Mornitoring_data\\정리데이터\\PM2.5")
pp<-read.csv("Daily_PM25_15-16_KMH.csv",header=T)
ppcode<-as.data.frame(read_excel("시군구코드.xlsx",sheet=1))

names(ppcode)=c("City","SIGUNGU_NM","SIGUNGU")

pp2<-merge(pp,ppcode,all.x=T,by="SIGUNGU")
pp2$date=with(pp2,as.Date(paste0(yy,"-",mm,"-",dd)))

pp3<-aggregate(pp2$PM25,list(pp2$City,pp2$date),mean,na.rm=T)

names(pp3)=c("SI","date","PM25_M")

pp3$SIDO=substr(pp3$SI,1,2)

pp3$SIDO=with(pp3,ifelse(SIDO=="서울","Seoul",ifelse(SIDO=="부산","Busan",
                                                   ifelse(SIDO=="대구","Daegu",
                                                          ifelse(SIDO=="인천","Incheon",
                                                                 ifelse(SIDO=="광주","Gwangju",
                                                                        ifelse(SIDO=="대전","Daejeon","Ulsan")))))))

ap2<-ap[1:4]
ap2$date=as.Date(ap2$date)

ap2$key=paste0(ap2$SIDO,"-",ap2$date)
pp3$key=paste0(pp3$SIDO,"-",pp3$date)

ap3<-subset(ap2,date>="2015-01-01")

ap3<-ap3 %>% select(-c(SIDO,date))

mmm<-merge(ap3,pp3,by="key",all=T)
mmm$PM25=mmm$PM25*10
c1<-data.frame(date=subset(mmm,SIDO=="Seoul")$date,SIDO="Seoul",PM25=subset(mmm,SIDO=="Seoul")$PM25  ,g="CMAQ PM2.5")
m1<-data.frame(date=subset(mmm,SIDO=="Seoul")$date,SIDO="Seoul",PM25=subset(mmm,SIDO=="Seoul")$PM25_M,g="Monitoring")           
c2<-data.frame(date=subset(mmm,SIDO=="Busan")$date,SIDO="Busan",PM25=subset(mmm,SIDO=="Busan")$PM25  ,g="CMAQ PM2.5")
m2<-data.frame(date=subset(mmm,SIDO=="Busan")$date,SIDO="Busan",PM25=subset(mmm,SIDO=="Busan")$PM25_M,g="Monitoring")  
c3<-data.frame(date=subset(mmm,SIDO=="Daegu")$date,SIDO="Daegu",PM25=subset(mmm,SIDO=="Daegu")$PM25  ,g="CMAQ PM2.5")
m3<-data.frame(date=subset(mmm,SIDO=="Daegu")$date,SIDO="Daegu",PM25=subset(mmm,SIDO=="Daegu")$PM25_M,g="Monitoring")  
c4<-data.frame(date=subset(mmm,SIDO=="Incheon")$date,SIDO="Incheon",PM25=subset(mmm,SIDO=="Incheon")$PM25  ,g="CMAQ PM2.5")
m4<-data.frame(date=subset(mmm,SIDO=="Incheon")$date,SIDO="Incheon",PM25=subset(mmm,SIDO=="Incheon")$PM25_M,g="Monitoring")  
c5<-data.frame(date=subset(mmm,SIDO=="Gwangju")$date,SIDO="Gwangju",PM25=subset(mmm,SIDO=="Gwangju")$PM25  ,g="CMAQ PM2.5")
m5<-data.frame(date=subset(mmm,SIDO=="Gwangju")$date,SIDO="Gwangju",PM25=subset(mmm,SIDO=="Gwangju")$PM25_M,g="Monitoring")  
c6<-data.frame(date=subset(mmm,SIDO=="Daejeon")$date,SIDO="Daejeon",PM25=subset(mmm,SIDO=="Daejeon")$PM25  ,g="CMAQ PM2.5")
m6<-data.frame(date=subset(mmm,SIDO=="Daejeon")$date,SIDO="Daejeon",PM25=subset(mmm,SIDO=="Daejeon")$PM25_M,g="Monitoring")  
c7<-data.frame(date=subset(mmm,SIDO=="Ulsan")$date,SIDO="Ulsan",PM25=subset(mmm,SIDO=="Ulsan")$PM25  ,g="CMAQ PM2.5")
m7<-data.frame(date=subset(mmm,SIDO=="Ulsan")$date,SIDO="Ulsan",PM25=subset(mmm,SIDO=="Ulsan")$PM25_M,g="Monitoring")  

cm<-rbind(c1,m1,m2,c2,m3,c3,m3,c4,m4,c5,m5,c6,m6,c7,m7)
mmm$SIDO=factor(mmm$SIDO,levels=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"))

x11();grid.arrange(ggplot(cm,aes(date,PM25,col=g))+geom_line()+facet_wrap(~SIDO)+theme_gray(base_size=15),
                   ggplot(mmm,aes(PM25,PM25_M))+geom_point(size=3)+stat_smooth(method="lm",se=F)+facet_wrap(~SIDO)+
                     labs(x=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration (CMAQ))),
                          y=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration (Monitoring))))+theme_gray(base_size=15),ncol=2)


###################################################################################################################################################################
o1<-summary(gam(AGE0005  ~ma06_O3+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s1))$'p.table'[2,]
o2<-summary(gam(AGE0005  ~ma06_O3+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s2))$'p.table'[2,]
o3<-summary(gam(AGE0005  ~ma06_O3+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s3))$'p.table'[2,]
o4<-summary(gam(AGE0005  ~ma06_O3+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s4))$'p.table'[2,]
o5<-summary(gam(AGE0005  ~ma06_O3+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s5))$'p.table'[2,]
o6<-summary(gam(AGE0005  ~ma06_O3+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s6))$'p.table'[2,]
o7<-summary(gam(AGE0005  ~ma06_O3+s(as.numeric(date),k=7*9,fx=T)+s(AT,k=6,fx=T)+dow,family="poisson",data=s7))$'p.table'[2,]


ss<-rbind(s1,s2,s3,s4,s5,s6,s7)

ss.w<-subset(ss,month %in% (4:9) )
ss.c<-subset(ss,month %in% c(1:3,10:12) )

rbind(round(cbind(cbind(mean(ss.w$PM25*10,na.rm=T) ,sd(ss.w$PM25*10,na.rm=T)) ,cbind(mean(ss.c$PM25*10,na.rm=T) ,sd(ss.c$PM25*10,na.rm=T)),1),
            round(cbind(cbind(mean(ss.w$PM10*10,na.rm=T) ,sd(ss.w$PM10*10,na.rm=T)) ,cbind(mean(ss.c$PM10*10,na.rm=T) ,sd(ss.c$PM10*10,na.rm=T)),1),
                  round(cbind(cbind(mean(ss.w$SO2*1000,na.rm=T),sd(ss.w$SO2*1000,na.rm=T)),cbind(mean(ss.c$SO2*1000,na.rm=T),sd(ss.c$SO2*1000,na.rm=T)),1),
                        round(cbind(cbind(mean(ss.w$NO2*1000,na.rm=T),sd(ss.w$NO2*1000,na.rm=T)),cbind(mean(ss.c$NO2*1000,na.rm=T),sd(ss.c$NO2*1000,na.rm=T)),1),
                              round(cbind(cbind(mean(ss.w$CO*1000,na.rm=T) ,sd(ss.w$CO*1000,na.rm=T)) ,cbind(mean(ss.c$CO*1000,na.rm=T) ,sd(ss.c$$CO*1000,na.rm=T)),1),
                                    round(cbind(cbind(mean(ss.w$O3*1000,na.rm=T) ,sd(ss.w$O3*1000,na.rm=T)) ,cbind(mean(ss.c$O3*1000,na.rm=T) ,sd(ss.c$O3*1000,na.rm=T)),1))
                                    
                                    
                                    
