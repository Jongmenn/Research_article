###################################################################################################################################
########################
#PM2.5 & AOM Revision ##
########################
####################################################################################################################################
####################################################################################################################################
library(readxl) ;library(ggplot2)   ;library(sqldf)    ;library(dplyr)
library(plyr)   ;library(mgcv)      ;library(TTR)      ;library(Epi)
library(tsModel);library(lubridate) ;library(dlnm)     ;library(metafor)
library(mixmeta);library(plotrix)   ;library(gridExtra)
library(lubridate)
setwd("C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\analysis\\data")

aom_w1<-read.csv("AOM_W1.csv",header=T) ; aom_w1$DATE=as.Date(aom_w1$DATE)
aom_w2<-read.csv("AOM_W2.csv",header=T) ; aom_w2$DATE=as.Date(aom_w2$DATE)
aom_w3<-read.csv("AOM_W3.csv",header=T) ; aom_w3$DATE=as.Date(aom_w3$DATE)
aom_w4<-read.csv("AOM_W4.csv",header=T) ; aom_w4$DATE=as.Date(aom_w4$DATE)

aom_w1$key=paste0(aom_w1$SIDO,"-",aom_w1$DATE) ; aom_w1$yyyy=substr(aom_w1$DATE,1,4) 
aom_w2$key=paste0(aom_w2$SIDO,"-",aom_w2$DATE) ; aom_w2$yyyy=substr(aom_w2$DATE,1,4)
aom_w3$key=paste0(aom_w3$SIDO,"-",aom_w3$DATE) ; aom_w3$yyyy=substr(aom_w3$DATE,1,4)
aom_w4$key=paste0(aom_w4$SIDO,"-",aom_w4$DATE) ; aom_w4$yyyy=substr(aom_w4$DATE,1,4)

# aom_count_w4<-cbind(with(aom_w4,aggregate(AGE0,list(yyyy,SIDO),sum)),with(aom_w4,aggregate(AGE0_M,list(yyyy,SIDO),sum))[,3],with(aom_w4,aggregate(AGE0_F,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE1,list(yyyy,SIDO),sum))[,3],with(aom_w4,aggregate(AGE1_M,list(yyyy,SIDO),sum))[,3],with(aom_w4,aggregate(AGE1_F,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE2,list(yyyy,SIDO),sum))[,3],with(aom_w4,aggregate(AGE2_M,list(yyyy,SIDO),sum))[,3],with(aom_w4,aggregate(AGE2_F,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE3,list(yyyy,SIDO),sum))[,3],with(aom_w4,aggregate(AGE3_M,list(yyyy,SIDO),sum))[,3],with(aom_w4,aggregate(AGE3_F,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(AGE13,list(yyyy,SIDO),sum))[,3],with(aom_w4,aggregate(AGE13_M,list(yyyy,SIDO),sum))[,3],with(aom_w4,aggregate(AGE13_F,list(yyyy,SIDO),sum))[,3],
#                     with(aom_w4,aggregate(TOT,list(yyyy,SIDO),sum))[,3],with(aom_w4,aggregate(TOT_M,list(yyyy,SIDO),sum))[,3],with(aom_w4,aggregate(TOT_F,list(yyyy,SIDO),sum))[,3])

# names(aom_count_w4)=c("year","sido",names(aom_w4)[5:22])
# setwd("C:\\Users\\AROOM206_kard141\\Desktop\\AOM\\분석\\결과")
# write.csv(aom_count_w4,file="aom_count_w4.csv",row.names=F)
###########################################################################################################################################
###########################################################################################################################################
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

aom_w1$year=year(aom_w1$DATE);aom_w1$month=as.factor(month(aom_w1$DATE));aom_w1$day=day(aom_w1$DATE);
aom_w2$year=year(aom_w2$DATE);aom_w2$month=as.factor(month(aom_w2$DATE));aom_w2$day=day(aom_w2$DATE);
aom_w3$year=year(aom_w3$DATE);aom_w3$month=as.factor(month(aom_w3$DATE));aom_w3$day=day(aom_w3$DATE);
aom_w4$year=year(aom_w4$DATE);aom_w4$month=as.factor(month(aom_w4$DATE));aom_w4$day=day(aom_w4$DATE);

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

g1<-gam(TOT~s(exposure,k=7,fx=T)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson",data=df1)
g2<-gam(TOT~s(exposure,k=7,fx=T)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson",data=df2)
g3<-gam(TOT~s(exposure,k=7,fx=T)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson",data=df3)
g4<-gam(TOT~s(exposure,k=7,fx=T)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson",data=df4)
g5<-gam(TOT~s(exposure,k=7,fx=T)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson",data=df5)
g6<-gam(TOT~s(exposure,k=7,fx=T)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson",data=df6)
g7<-gam(TOT~s(exposure,k=7,fx=T)+s(as.numeric(DATE),k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson",data=df7)

x11();par(mfrow=c(2,4))
plot(g1,select=1,cex.axis=1.48,cex.lab=1.48,jit=T,xlim=c(0,80),ylim=c(-0.3,0.4),cex.main=2,main="Seoul",scheme=1,ylab="log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
text(x=45,y=-0.25,label="The total of acute otitis media cases: 1,331,640",cex=1.5)
plot(g2,select=1,cex.axis=1.48,cex.lab=1.48,jit=T,xlim=c(0,80),ylim=c(-0.3,0.4),cex.main=2,main="Busan",scheme=1,ylab="log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
text(x=45,y=-0.25,label="The total of acute otitis media cases: 304,236",cex=1.5)
plot(g3,select=1,cex.axis=1.48,cex.lab=1.48,jit=T,xlim=c(0,80),ylim=c(-0.3,0.4),cex.main=2,main="Daegu",scheme=1,ylab="log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
text(x=45,y=-0.25,label="The total of acute otitis media cases: 279,889",cex=1.5)
plot(g4,select=1,cex.axis=1.48,cex.lab=1.48,jit=T,xlim=c(0,80),ylim=c(-0.3,0.4),cex.main=2,main="Incheon",scheme=1,ylab="log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
text(x=45,y=-0.25,label="The total of acute otitis media cases: 433,824",cex=1.5)
plot(g5,select=1,cex.axis=1.48,cex.lab=1.48,jit=T,xlim=c(0,80),ylim=c(-0.3,0.4),cex.main=2,main="Gwangju",scheme=1,ylab="log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
text(x=45,y=-0.25,label="The total of acute otitis media cases: 265,549",cex=1.5)
plot(g6,select=1,cex.axis=1.48,cex.lab=1.48,jit=T,xlim=c(0,80),ylim=c(-0.3,0.4),cex.main=2,main="Daejeon",scheme=1,ylab="log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
text(x=45,y=-0.25,label="The total of acute otitis media cases: 240,853",cex=1.5)
plot(g7,select=1,cex.axis=1.48,cex.lab=1.48,jit=T,xlim=c(0,80),ylim=c(-0.3,0.4),cex.main=2,main="Ulsan",scheme=1,ylab="log RR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
text(x=45,y=-0.25,label="The total of acute otitis media cases: 209,176",cex=1.5)


head(tt2)
ex<-function(data){data$exposure2=round(data$exposure*10)

hh<-with(data,aggregate(Total_C,list(exposure2,SIDO),sum,na.rm=T))

names(hh)=c("exposure","SIDO","counts")

h1<-subset(df1,SIDO=="Seoul")   
h2<-subset(df2,SIDO=="Busan")   %>% select(-SIDO)
h3<-subset(df3,SIDO=="Daegu")   %>% select(-SIDO)
h4<-subset(df4,SIDO=="Incheon") %>% select(-SIDO)
h5<-subset(df5,SIDO=="Gwangju") %>% select(-SIDO)
h6<-subset(df6,SIDO=="Daejeon") %>% select(-SIDO)
h7<-subset(df,SIDO=="Ulsan")   %>% select(-SIDO)

# m1<-merge(exposure,h1,all.x=T);names(m1)[3]="h1";m1$h1[is.na(m1$h1)]<-0
m2<-merge(h1,h2,all.x=T,by="exposure");names(m2)[4]="h2";m2$h2[is.na(m2$h2)]<-0
m3<-merge(m2,h3,all.x=T,by="exposure");names(m3)[5]="h3";m3$h3[is.na(m3$h3)]<-0
m4<-merge(m3,h4,all.x=T,by="exposure");names(m4)[6]="h4";m4$h4[is.na(m4$h4)]<-0
m5<-merge(m4,h5,all.x=T,by="exposure");names(m5)[7]="h5";m5$h5[is.na(m5$h5)]<-0
m6<-merge(m5,h6,all.x=T,by="exposure");names(m6)[8]="h6";m6$h6[is.na(m6$h6)]<-0
m7<-merge(m6,h7,all.x=T,by="exposure");names(m7)[9]="h7";m7$h7[is.na(m7$h7)]<-0
names(m7)[3]="h1"
m7}


ex1<-ex(tt1)
ex2<-ex(tt2)
ex3<-ex(tt3)
ex4<-ex(tt4)
ex5<-ex(tt5)
ex6<-ex(tt6)

head(df1)
# Plot overlapping histograms
xrange=with(df1,seq(min(exposure),max(exposure)))
oma=c(2.5,1,1,1.5)

x11();
plot(gamm.list$CO$gam,select=1,scheme=1,xlim=c(min(xrange),max(xrange)),lwd=2,ylim=c(-0.1,0.15),ylab="Log Estimate",xlab="",cex.lab=1.39,cex.axis=1.39);abline(h=0,lty=2,col="red",lwd=1.3)
par(new=T,oma=c(2.5,1,1,1.5));
layout(matrix(c(1,1,1,1,1,1,2,2,2,2),5,2,byrow=T));
barplot(with(ex5,rbind(h1,h2,h3,h4,h5,h6,h7)),axes = FALSE,col=alpha(c("#17266B","#144357","#137860","#00b81f","#B07422","#E28021","#E24621"),0.9),
        space=0,las=1,main="",xlab=expression(paste(CO[2]," (",ppb,")")),cex.lab=1.39,cex.axis=1.39)
axis(side = 4, col = "red",cex.axis=1.5)
mtext("Pneumonia counts \n",side=4,col="red",cex=1.3)
legend("bottom",legend=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"),
       fill=c("#17266B","#144357","#137860","#00b81f","#B07422","#E28021","#E24621"),box.lty=0,cex=1.1)





tiff(filename="C:\\Users\\a\\Desktop\\집에서\\20200418\\revision_figure\\Fig.S6.tiff",width=4000,height=3200,res=300)
par(mfrow=c(2,4))
plot(g1,select=1,cex.axis=1.48,cex.lab=1.48,ylim=c(-0.3,0.4),main="Seoul",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g2,select=1,cex.axis=1.48,cex.lab=1.48,ylim=c(-0.3,0.4),,main="Busan",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g3,select=1,cex.axis=1.48,cex.lab=1.48,ylim=c(-0.3,0.4),,main="Daegu",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g4,select=1,cex.axis=1.48,cex.lab=1.48,ylim=c(-0.3,0.4),,main="Incheon",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g5,select=1,cex.axis=1.48,cex.lab=1.48,ylim=c(-0.3,0.4),,main="Gwangju",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g6,select=1,cex.axis=1.48,cex.lab=1.48,ylim=c(-0.3,0.4),,main="Daejeon",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
plot(g7,select=1,cex.axis=1.48,cex.lab=1.48,ylim=c(-0.3,0.4),,main="Ulsan",scheme=1,ylab="logRR",xlab=expression(paste(PM[2.5]," (",mu,g/m^3,")")))
dev.off()
##################################################################################################################################
##################################################################################################################################
#age distribution
ag1<-as.data.frame(read_excel("age.xlsx",sheet=1))
ag2<-as.data.frame(read_excel("age.xlsx",sheet=2))

x11();ggplot(ag1,aes(AGE,y=count))+geom_bar(stat="identity")+labs(x="Age",y="Frequency")+theme_bw(base_size=20)

tiff(filename="C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\figure\\age_distribution_total.tiff",width=3600,height=2800,res=300)
ggplot(ag1,aes(AGE,y=count))+geom_bar(stat="identity")+labs(x="Age",y="Frequency")+theme_bw(base_size=20)
dev.off()
###########################################################################################################################################
#########################################################################################################################################
#서울지역만, 질환 연령 다르게 하여서 자유도 비교하기 
#Best Moedl fit 결정하기 위해 여러가지 조합 , Time df, AT df만 조정 
#summary table 
#df for Time-trend
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
###########################################################################################################################################
###########################################################################################################################################
#Time trend 자유도에 따라 beta값 변이 
#Apparent Temperature fixed, time df 조정 (1~9 per year)
time.gam<-function(i,data){
  d<-data
  g<-gam(TOT ~pm+s(as.numeric(DATE),k=i*6,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
  summary(g)$p.table[2,]}
t1=NULL;t2=NULL;t3=NULL;t4=NULL;t5=NULL;t6=NULL;t7=NULL;

for(i in 1:9){
  t1[[i]]<-time.gam(i,s1.w4)
  t2[[i]]<-time.gam(i,s2.w4)
  t3[[i]]<-time.gam(i,s3.w4)
  t4[[i]]<-time.gam(i,s4.w4)
  t5[[i]]<-time.gam(i,s5.w4)
  t6[[i]]<-time.gam(i,s6.w4)
  t7[[i]]<-time.gam(i,s7.w4)
  print(i)}

time.s1<-as.data.frame(do.call(rbind,t1));time.s1$region="Seoul"
time.s2<-as.data.frame(do.call(rbind,t2));time.s2$region="Busan"
time.s3<-as.data.frame(do.call(rbind,t3));time.s3$region="Daegu"
time.s4<-as.data.frame(do.call(rbind,t4));time.s4$region="Incheon"
time.s5<-as.data.frame(do.call(rbind,t5));time.s5$region="Gwangju"
time.s6<-as.data.frame(do.call(rbind,t6));time.s6$region="Daejeon"
time.s7<-as.data.frame(do.call(rbind,t7));time.s7$region="Ulsan"

time.res<-rbind(time.s1,time.s2,time.s3,time.s4,time.s5,time.s6,time.s7)

time.res$region=factor(time.res$region,levels=unique(time.res$region))

time.res$df=1:9
time.res$RR=exp(time.res$Estimate)
time.res$RR_lci=exp(time.res$Estimate-1.96*time.res$`Std. Error`)
time.res$RR_uci=exp(time.res$Estimate+1.96*time.res$`Std. Error`)

x11();ggplot(time.res,aes(df,RR))+geom_point(size=3,col="red")+geom_errorbar(aes(ymin=RR_lci,ymax=RR_uci),size=1,width=0.5)+
  scale_x_continuous(breaks = c(1:9))+geom_hline(yintercept =1,col="blue",linetype=2,size=1.1)+
  theme_gray(base_size=20)+facet_wrap(~region)+labs(x="Degree of freedom for time per year",y="Relative Risk (95% Confidence Interval)")

tiff(filename="C:\\Users\\a\\Desktop\\집에서\\20200418\\revision_figure\\Fig.S2.tiff",width=4000,height=3200,res=300)
ggplot(time.res,aes(df,RR))+geom_point(size=3,col="red")+geom_errorbar(aes(ymin=RR_lci,ymax=RR_uci),size=1,width=0.5)+
  scale_x_continuous(breaks = c(1:9))+geom_hline(yintercept =1,col="blue",linetype=2,size=1.1)+
  theme_gray(base_size=20)+facet_wrap(~region)+labs(x="Degree of freedom for time per year",y="Relative Risk (95% Confidence Interval)")
dev.off()
###################################################################################################################################################
###################################################################################################################################################
#overdispersion test####
########################
library(AER)
m1<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow+month,family="poisson",data=s1.w4)
m2<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow+month,family="poisson",data=s2.w4)
m3<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow+month,family="poisson",data=s3.w4)
m4<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow+month,family="poisson",data=s4.w4)
m5<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow+month,family="poisson",data=s5.w4)
m6<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow+month,family="poisson",data=s6.w4)
m7<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow+month,family="poisson",data=s7.w4)

d1<-dispersiontest(m1);z1<-data.frame(beta=d1$estimate,z=d1$statistic,pval=d1$p.value)
d2<-dispersiontest(m2);z2<-data.frame(beta=d2$estimate,z=d2$statistic,pval=d2$p.value)
d3<-dispersiontest(m3);z3<-data.frame(beta=d3$estimate,z=d3$statistic,pval=d3$p.value)
d4<-dispersiontest(m4);z4<-data.frame(beta=d4$estimate,z=d4$statistic,pval=d4$p.value)
d5<-dispersiontest(m5);z5<-data.frame(beta=d5$estimate,z=d5$statistic,pval=d5$p.value)
d6<-dispersiontest(m6);z6<-data.frame(beta=d6$estimate,z=d6$statistic,pval=d6$p.value)
d7<-dispersiontest(m7);z7<-data.frame(beta=d7$estimate,z=d7$statistic,pval=d7$p.value)

View(rbind(z1,z2,z3,z4,z5,z6,z7))

#Model Comparison ; Poisson, Quasi-Poisson, Negative binomial , 
#QAIC, AIC ,loglikelihood 찾으려고 해본거
###################################################################################################################################################
###################################################################################################################################################
###GAM 모델링 - Single Lag ########
###################################
s.s1=NULL;s.s2=NULL;s.s3=NULL
s.s4=NULL;s.s5=NULL;s.s6=NULL;s.s7=NULL

for(i in 1:11){
  f1<-gam(TOT ~ lag(pm,i-1)+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=s1.w4)
  f2<-gam(TOT ~ lag(pm,i-1)+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=s2.w4)
  f3<-gam(TOT ~ lag(pm,i-1)+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=s3.w4)
  f4<-gam(TOT ~ lag(pm,i-1)+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=s4.w4)
  f5<-gam(TOT ~ lag(pm,i-1)+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=s5.w4)
  f6<-gam(TOT ~ lag(pm,i-1)+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=s6.w4)
  f7<-gam(TOT ~ lag(pm,i-1)+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=s7.w4)
  
  s.s1[[i]]<-summary(f1)$p.table[2,];s.s2[[i]]<-summary(f2)$p.table[2,];
  s.s3[[i]]<-summary(f3)$p.table[2,];s.s4[[i]]<-summary(f4)$p.table[2,];
  s.s5[[i]]<-summary(f5)$p.table[2,];s.s6[[i]]<-summary(f6)$p.table[2,];
  s.s7[[i]]<-summary(f7)$p.table[2,];
  print(i)}

s.fit1<-as.data.frame(do.call(rbind,s.s1));s.fit1$category="Total";s.fit1$lag=1:11-1;s.fit1$Region="Seoul"
s.fit2<-as.data.frame(do.call(rbind,s.s2));s.fit2$category="Total";s.fit2$lag=1:11-1;s.fit2$Region="Busan"
s.fit3<-as.data.frame(do.call(rbind,s.s3));s.fit3$category="Total";s.fit3$lag=1:11-1;s.fit3$Region="Daegu"
s.fit4<-as.data.frame(do.call(rbind,s.s4));s.fit4$category="Total";s.fit4$lag=1:11-1;s.fit4$Region="Incheon"
s.fit5<-as.data.frame(do.call(rbind,s.s5));s.fit5$category="Total";s.fit5$lag=1:11-1;s.fit5$Region="Gwangju"
s.fit6<-as.data.frame(do.call(rbind,s.s6));s.fit6$category="Total";s.fit6$lag=1:11-1;s.fit6$Region="Daejeon"
s.fit7<-as.data.frame(do.call(rbind,s.s7));s.fit7$category="Total";s.fit7$lag=1:11-1;s.fit7$Region="Ulsan"

s.fit<-rbind(s.fit1,s.fit2,s.fit3,s.fit4,s.fit5,s.fit6,s.fit7)

s.fit$RR    =exp(s.fit$Estimate)
s.fit$RR_lci=with(s.fit,exp(Estimate-1.96*`Std. Error`))
s.fit$RR_uci=with(s.fit,exp(Estimate+1.96*`Std. Error`))

s.fit$Region=factor(s.fit$Region,levels=unique(s.fit$Region))

x11();ggplot(s.fit,aes(lag,RR))+geom_point(size=3,col="red")+geom_errorbar(aes(ymin=RR_lci,ymax=RR_uci),size=1,width=0.5)+
  scale_x_continuous(breaks = c(0:10))+geom_hline(yintercept =1,col="blue",linetype=2,size=1.1)+
  theme_gray(base_size=20)+facet_wrap(~Region)+labs(x="Lag days",y="Relative Risk (95% Confidence Interval)")

tiff(filename="C:\\Users\\a\\Desktop\\집에서\\20200418\\revision_figure\\Fig.S3.tiff",width=4000,height=3200,res=300)
ggplot(s.fit,aes(lag,RR))+geom_point(size=3,col="red")+geom_errorbar(aes(ymin=RR_lci,ymax=RR_uci),size=1,width=0.5)+
  scale_x_continuous(breaks = c(0:10))+geom_hline(yintercept =1,col="blue",linetype=2,size=1.1)+
  theme_gray(base_size=20)+facet_wrap(~Region)+labs(x="Lag days",y="Relative Risk (95% Confidence Interval)")
dev.off()
##################################################################################################################################
##################################################################################################################################
###GAM 모델링 - Moving avergae ####
###################################

fit01=NULL;fit02=NULL;fit03=NULL;fit04=NULL;fit05=NULL
gam.res<-function(fit) {cbind(as.data.frame(summary(fit)$p.table)[2,],
                              Rsquare=summary(fit)$r.sq,
                              glmer.ML=summary(fit)$sp.criterion)}
gam.sido<-function(data){
  d<-data
  
  for(i in 32:38){
    f1<-gam(TOT   ~ d[,i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    f2<-gam(TOT_M ~ d[,i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    f3<-gam(TOT_F ~ d[,i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    f4<-gam(AGE0  ~ d[,i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    f5<-gam(AGE13 ~ d[,i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    
    fit01[[i]]<-gam.res(f1)
    fit02[[i]]<-gam.res(f2)
    fit03[[i]]<-gam.res(f3)
    fit04[[i]]<-gam.res(f4)
    fit05[[i]]<-gam.res(f5)
    
    print(i)}
  
  m.fit01<-as.data.frame(do.call(rbind,fit01));m.fit01$category="Total" ;m.fit01$lag=names(d)[32:38]
  m.fit02<-as.data.frame(do.call(rbind,fit02));m.fit02$category="Male"  ;m.fit02$lag=names(d)[32:38]
  m.fit03<-as.data.frame(do.call(rbind,fit03));m.fit03$category="Female";m.fit03$lag=names(d)[32:38]
  m.fit04<-as.data.frame(do.call(rbind,fit04));m.fit04$category="AGE0"  ;m.fit04$lag=names(d)[32:38]
  m.fit05<-as.data.frame(do.call(rbind,fit05));m.fit05$category="AGE013";m.fit05$lag=names(d)[32:38]
  
  m.fit<-rbind(m.fit01,m.fit02,m.fit03,m.fit04,m.fit05)
  
  m.fit$RR=exp(m.fit$Estimate)
  m.fit$RR_lci=with(m.fit,exp(Estimate-1.96*`Std. Error`))
  m.fit$RR_uci=with(m.fit,exp(Estimate+1.96*`Std. Error`))
  m.fit}

s1.fit<-gam.sido(s1.w4);s1.fit$Region="Seoul"
s2.fit<-gam.sido(s2.w4);s2.fit$Region="Busan"
s3.fit<-gam.sido(s3.w4);s3.fit$Region="Daegu"
s4.fit<-gam.sido(s4.w4);s4.fit$Region="Incheon"
s5.fit<-gam.sido(s5.w4);s5.fit$Region="Gwangju"
s6.fit<-gam.sido(s6.w4);s6.fit$Region="Daejeon"
s7.fit<-gam.sido(s7.w4);s7.fit$Region="Ulsan"

gam.fit<-rbind(s1.fit,s2.fit,s3.fit,s4.fit,s5.fit,s6.fit,s7.fit)

setwd("C:\\Users\\a\\Desktop\\집에서\\20200418")
write.csv(gam.fit,file="gam.fit.csv",row.names=F,na="")
gam.fit<-read.csv("C:\\Users\\a\\Desktop\\집에서\\20200418\\gam.fit.csv",header=T)
##################################################################################################################################
##################################################################################################################################
###GAM 모델링 - Episode ########
################################
s1.fit<-gam.sido(s1.w1);s1.fit$Region="Seoul"
s2.fit<-gam.sido(s2.w1);s2.fit$Region="Busan"
s3.fit<-gam.sido(s3.w1);s3.fit$Region="Daegu"
s4.fit<-gam.sido(s4.w1);s4.fit$Region="Incheon"
s5.fit<-gam.sido(s5.w1);s5.fit$Region="Gwangju"
s6.fit<-gam.sido(s6.w1);s6.fit$Region="Daejeon"
s7.fit<-gam.sido(s7.w1);s7.fit$Region="Ulsan"
gam.fit1<-rbind(s1.fit,s2.fit,s3.fit,s4.fit,s5.fit,s6.fit,s7.fit)

s1.fit<-gam.sido(s1.w2);s1.fit$Region="Seoul"
s2.fit<-gam.sido(s2.w2);s2.fit$Region="Busan"
s3.fit<-gam.sido(s3.w2);s3.fit$Region="Daegu"
s4.fit<-gam.sido(s4.w2);s4.fit$Region="Incheon"
s5.fit<-gam.sido(s5.w2);s5.fit$Region="Gwangju"
s6.fit<-gam.sido(s6.w2);s6.fit$Region="Daejeon"
s7.fit<-gam.sido(s7.w2);s7.fit$Region="Ulsan"
gam.fit2<-rbind(s1.fit,s2.fit,s3.fit,s4.fit,s5.fit,s6.fit,s7.fit)

s1.fit<-gam.sido(s1.w3);s1.fit$Region="Seoul"
s2.fit<-gam.sido(s2.w3);s2.fit$Region="Busan"
s3.fit<-gam.sido(s3.w3);s3.fit$Region="Daegu"
s4.fit<-gam.sido(s4.w3);s4.fit$Region="Incheon"
s5.fit<-gam.sido(s5.w3);s5.fit$Region="Gwangju"
s6.fit<-gam.sido(s6.w3);s6.fit$Region="Daejeon"
s7.fit<-gam.sido(s7.w3);s7.fit$Region="Ulsan"
gam.fit3<-rbind(s1.fit,s2.fit,s3.fit,s4.fit,s5.fit,s6.fit,s7.fit)

setwd("C:\\Users\\a\\Desktop\\집에서\\20200418")
write.csv(gam.fit1,file="gam.fit1.csv",row.names=F,na="")
write.csv(gam.fit2,file="gam.fit2.csv",row.names=F,na="")
write.csv(gam.fit3,file="gam.fit3.csv",row.names=F,na="")
##################################################################################################################################
##################################################################################################################################
###GAM 모델링 - season ########
###############################
warm=NULL
cold=NULL
gam.season<-function(data){
  d1<-subset(data,month %in% c(4:9))
  d2<-subset(data,month %in% c(1:3,10:12))
  
  for(i in 32:38){
    f1<-gam(TOT ~ d1[,i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d1)
    f2<-gam(TOT ~ d2[,i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d2)
    
    warm[[i]]<-gam.res(f1)
    cold[[i]]<-gam.res(f2)
    
    print(i)}
  
  w.fit<-as.data.frame(do.call(rbind,warm));w.fit$category="warm" ;w.fit$lag=names(d)[32:38]
  c.fit<-as.data.frame(do.call(rbind,cold));c.fit$category="cold" ;c.fit$lag=names(d)[32:38]
  
  m.fit<-rbind(w.fit,c.fit)
  m.fit$RR=exp(m.fit$Estimate)
  m.fit$RR_lci=with(m.fit,exp(Estimate-1.96*`Std. Error`))
  m.fit$RR_uci=with(m.fit,exp(Estimate+1.96*`Std. Error`))
  m.fit}

s1.fit.season<-gam.season(s1.w4);s1.fit.season$Region="Seoul"
s2.fit.season<-gam.season(s2.w4);s2.fit.season$Region="Busan"
s3.fit.season<-gam.season(s3.w4);s3.fit.season$Region="Daegu"
s4.fit.season<-gam.season(s4.w4);s4.fit.season$Region="Incheon"
s5.fit.season<-gam.season(s5.w4);s5.fit.season$Region="Gwangju"
s6.fit.season<-gam.season(s6.w4);s6.fit.season$Region="Daejeon"
s7.fit.season<-gam.season(s7.w4);s7.fit.season$Region="Ulsan"

fit.season<-rbind(s1.fit.season,s2.fit.season,s3.fit.season,
                  s4.fit.season,s5.fit.season,s6.fit.season,s7.fit.season)
write.csv(fit.season,file="fit.season.csv",row.names=F,na="")
##################################################################################################################################
##################################################################################################################################
###GAM 모델링 - URI    ########
###############################
###############################

setwd("C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\analysis\\data")
aom_uri<-read.csv("AOM_URI_COUNT_191210.csv",header=T) ; aom_uri$DATE=as.Date(aom_uri$DATE)
aom_uri$key=paste0(aom_uri$SIDO,"-",aom_uri$DATE) ; aom_uri$yyyy=substr(aom_uri$DATE,1,4) 

aom_uri2<-merge(aom_uri,pm2,by="key")
aom_uri3<-merge(dow,aom_uri2,by="DATE") ; aom_uri3$dow=as.factor(aom_uri3$dow)

aom_uri3$uri1_ratio=with(aom_uri3,TOT1_Y/TOT)*100
aom_uri3$uri2_ratio=with(aom_uri3,(TOT1_Y+TOT2_Y)/TOT)*100
aom_uri3$uri3_ratio=with(aom_uri3,(TOT1_Y+TOT2_Y+TOT3_Y)/TOT)*100
aom_uri3$uri4_ratio=with(aom_uri3,(TOT1_Y+TOT2_Y+TOT3_Y+TOT4_Y)/TOT)*100

uri_s1<-subset(aom_uri3,aom_uri3$SIDO==11);uri_s2<-subset(aom_uri3,aom_uri3$SIDO==26);
uri_s3<-subset(aom_uri3,aom_uri3$SIDO==27);uri_s4<-subset(aom_uri3,aom_uri3$SIDO==28);
uri_s5<-subset(aom_uri3,aom_uri3$SIDO==29);uri_s6<-subset(aom_uri3,aom_uri3$SIDO==30);uri_s7<-subset(aom_uri3,aom_uri3$SIDO==31)

URI_Y=NULL;URI_N=NULL
gam.uri<-function(data){
  
  d<-data;d$month=factor(month(d$DATE))
  for(i in 23:29){
    f1<-gam(TOT4_Y ~ d[,i]+s(as.numeric(as.factor(DATE)),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    f2<-gam(TOT4_N ~ d[,i]+s(as.numeric(as.factor(DATE)),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    
    URI_Y[[i]]<-gam.res(f1)
    URI_N[[i]]<-gam.res(f2)
    print(i)}
  
  y.fit<-as.data.frame(do.call(rbind,URI_Y));y.fit$category="URI Yes" ;y.fit$lag=names(d)[23:29]
  n.fit<-as.data.frame(do.call(rbind,URI_N));n.fit$category="URI No " ;n.fit$lag=names(d)[23:29]
  
  m.fit<-rbind(y.fit,n.fit)
  m.fit$RR=exp(m.fit$Estimate)
  m.fit$RR_lci=with(m.fit,exp(Estimate-1.96*`Std. Error`))
  m.fit$RR_uci=with(m.fit,exp(Estimate+1.96*`Std. Error`))
  m.fit}

u1.fit<-gam.uri(uri_s1);u1.fit$Region="Seoul"
u2.fit<-gam.uri(uri_s2);u2.fit$Region="Busan"
u3.fit<-gam.uri(uri_s3);u3.fit$Region="Daegu"
u4.fit<-gam.uri(uri_s4);u4.fit$Region="Incheon"
u5.fit<-gam.uri(uri_s5);u5.fit$Region="Gwangju"
u6.fit<-gam.uri(uri_s6);u6.fit$Region="Daejeon"
u7.fit<-gam.uri(uri_s7);u7.fit$Region="Ulsan"

uri.fit<-rbind(u1.fit,u2.fit,u3.fit,u4.fit,u5.fit,u6.fit,u7.fit)
setwd("C:\\Users\\a\\Desktop\\집에서\\20200418")
write.csv(uri.fit,file="uri.fit.csv",row.names=F,na="")

############################################################################################################################
############################################################################################################################
d<-uri_s1


URI_a1=NULL;URI_a2=NULL
URI_a3=NULL;URI_a4=NULL
#모델식에 daily URI 보정 
gam.uri2<-function(data){
  d<-data;d$month=month(d$DATE)
  for(i in 23:29){
    f1<-gam(TOT ~ d[,i]+s(as.numeric(as.factor(DATE)),k=6*9,fx=T)+s(AT,k=6,fx=T)+uri1_ratio+dow+month,family="quasipoisson",data=d)
    f2<-gam(TOT ~ d[,i]+s(as.numeric(as.factor(DATE)),k=6*9,fx=T)+s(AT,k=6,fx=T)+uri2_ratio+dow+month,family="quasipoisson",data=d)
    f3<-gam(TOT ~ d[,i]+s(as.numeric(as.factor(DATE)),k=6*9,fx=T)+s(AT,k=6,fx=T)+uri3_ratio+dow+month,family="quasipoisson",data=d)
    f4<-gam(TOT ~ d[,i]+s(as.numeric(as.factor(DATE)),k=6*9,fx=T)+s(AT,k=6,fx=T)+uri4_ratio+dow+month,family="quasipoisson",data=d)
    URI_a1[[i]]<-gam.res(f1)
    URI_a2[[i]]<-gam.res(f2)
    URI_a3[[i]]<-gam.res(f3)
    URI_a4[[i]]<-gam.res(f4)
    print(i)}
  
  a1.fit<-as.data.frame(do.call(rbind,URI_a1));a1.fit$category="URI1 prp adjusted" ;a1.fit$lag=names(d)[23:29]
  a2.fit<-as.data.frame(do.call(rbind,URI_a2));a2.fit$category="URI2 prp adjusted" ;a2.fit$lag=names(d)[23:29]
  a3.fit<-as.data.frame(do.call(rbind,URI_a3));a3.fit$category="URI3 prp adjusted" ;a3.fit$lag=names(d)[23:29]
  a4.fit<-as.data.frame(do.call(rbind,URI_a4));a4.fit$category="URI4 prp adjusted" ;a4.fit$lag=names(d)[23:29]
  
  m.fit<-rbind(a1.fit,a2.fit,a3.fit,a4.fit)
  m.fit$RR=exp(m.fit$Estimate)
  m.fit$RR_lci=with(m.fit,exp(Estimate-1.96*`Std. Error`))
  m.fit$RR_uci=with(m.fit,exp(Estimate+1.96*`Std. Error`))
  m.fit}

u1.fit<-gam.uri2(uri_s1);u1.fit$Region="Seoul"
u2.fit<-gam.uri2(uri_s2);u2.fit$Region="Busan"
u3.fit<-gam.uri2(uri_s3);u3.fit$Region="Daegu"
u4.fit<-gam.uri2(uri_s4);u4.fit$Region="Incheon"
u5.fit<-gam.uri2(uri_s5);u5.fit$Region="Gwangju"
u6.fit<-gam.uri2(uri_s6);u6.fit$Region="Daejeon"
u7.fit<-gam.uri2(uri_s7);u7.fit$Region="Ulsan"

uri.fit<-rbind(u1.fit,u2.fit,u3.fit,u4.fit,u5.fit,u6.fit,u7.fit)

#meta analysis : Total 

m01<-subset(uri.fit,lag=="lag04" & category=="URI1 prp adjusted")
m02<-subset(uri.fit,lag=="lag04" & category=="URI2 prp adjusted")
m03<-subset(uri.fit,lag=="lag04" & category=="URI3 prp adjusted")
m04<-subset(uri.fit,lag=="lag04" & category=="URI4 prp adjusted")

uni01 <- with(m01,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni02 <- with(m02,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni03 <- with(m03,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni04 <- with(m04,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))

x11();forest(uni01, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3)
x11();forest(uni02, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3)
x11();forest(uni03, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3)
x11();forest(uni04, transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=2,fontsize=1,1,digits=3)

m01<-subset(uri.fit,category=="URI1 prp adjusted"   & lag=="lag04")
m02<-subset(uri.fit,category=="URI2 prp adjusted"   & lag=="lag04")
m03<-subset(uri.fit,category=="URI3 prp adjusted"   & lag=="lag04")
m04<-subset(uri.fit,category=="URI4 prp adjusted"   & lag=="lag04")

m01$Region=paste0(m01$Region)         ;m01$Region=factor(m01$Region,levels=unique(m01$Region))
m02$Region=paste0(m02$Region," ")     ;m02$Region=factor(m02$Region,levels=unique(m02$Region))
m03$Region=paste0(m03$Region,"  ")    ;m03$Region=factor(m03$Region,levels=unique(m03$Region))
m04$Region=paste0(m04$Region,"   ")   ;m04$Region=factor(m04$Region,levels=unique(m04$Region)) 


row.names(m01)=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan")
row.names(m02)=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan")
row.names(m03)=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan")
row.names(m04)=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan")

m<-rbind(m01,m02,m03,m04)

uni01 <- with(m01,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni02 <- with(m02,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni03 <- with(m03,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni04 <- with(m04,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))

RM.res<-rma(yi=m$Estimate,sei=m$`Std. Error`,data=dat,slab=m$Region)


x11();par(mar=c(4,4,1,2));
forest(RM.res, xlim=c(0.8, 1.2), ylim=c(20, 69), steps=4, rows=c(65:59,53:47,41:35,29:23),refline=1,
       order=c(1:nrow(m)),transf = exp,bg=4,col=2,digits=3,cex=1.1,xlab="", mlab="")
addpoly(uni01,transf=exp,cex=1.25,row=57,col="red",digits=3,mlab="RE Model for Subgroup: URI episode 1 (0-7 days)")
addpoly(uni02,transf=exp,cex=1.25,row=45,col="red",digits=3,mlab="RE Model for Subgroup: URI episode 2 (8-14 days)")
addpoly(uni03,transf=exp,cex=1.25,row=33,col="red",digits=3,mlab="RE Model for Subgroup: URI episode 3 (15-21 days)")
addpoly(uni04,transf=exp,cex=1.25,row=21,col="red",digits=3,mlab="RE Model for Subgroup: URI episode 4 (22-28 day)")


##################################################################################################################################
##################################################################################################################################
###GAM 모델링 - URI 층화 (1~7, 8~14, 15~21, 22~28)########
##########################################################
setwd("C:\\Users\\a\\Desktop\\논문\\미세먼지_AOM\\analysis\\data")
uri_episode<-read.csv('URI5_COUNT.csv',header=T)

uri_episode$key=paste0(uri_episode$SIDO,"-",uri_episode$DATE) ; uri_episode$yyyy=substr(uri_episode$DATE,1,4) 

uri_episode2<-merge(uri_episode,pm2,by="key")
uri_episode2$DATE=as.Date(uri_episode2$DATE)

uri_episode3<-merge(dow,uri_episode2,by="DATE") ; uri_episode3$dow=as.factor(uri_episode3$dow)
uri_episode3$month=month(uri_episode3$DATE)
urie_s1<-subset(uri_episode3,SIDO==11);urie_s2<-subset(uri_episode3,SIDO==26);
urie_s3<-subset(uri_episode3,SIDO==27);urie_s4<-subset(uri_episode3,SIDO==28);
urie_s5<-subset(uri_episode3,SIDO==29);urie_s6<-subset(uri_episode3,SIDO==30);urie_s7<-subset(uri_episode3,SIDO==31)

URI_E1=NULL;URI_E2=NULL;URI_E3=NULL;URI_E4=NULL

gam.uri_epi<-function(data){
  d<-data
  for(i in 20:26){
    f1<-gam(E1 ~ d[,i]+s(as.numeric(as.factor(DATE)),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    f2<-gam(E2 ~ d[,i]+s(as.numeric(as.factor(DATE)),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    f3<-gam(E3 ~ d[,i]+s(as.numeric(as.factor(DATE)),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    f4<-gam(E4 ~ d[,i]+s(as.numeric(as.factor(DATE)),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    
    URI_E1[[i]]<-gam.res(f1);URI_E2[[i]]<-gam.res(f2)
    URI_E3[[i]]<-gam.res(f3);URI_E4[[i]]<-gam.res(f4)
    print(i)}
  
  e1.fit<-as.data.frame(do.call(rbind,URI_E1));e1.fit$category="URI E1" ;e1.fit$lag=names(d)[20:26]
  e2.fit<-as.data.frame(do.call(rbind,URI_E2));e2.fit$category="URI E2" ;e2.fit$lag=names(d)[20:26]
  e3.fit<-as.data.frame(do.call(rbind,URI_E3));e3.fit$category="URI E3" ;e3.fit$lag=names(d)[20:26]
  e4.fit<-as.data.frame(do.call(rbind,URI_E4));e4.fit$category="URI E4" ;e4.fit$lag=names(d)[20:26]
  
  m.fit<-rbind(e1.fit,e2.fit,e3.fit,e4.fit)
  m.fit$RR=exp(m.fit$Estimate)
  m.fit$RR_lci=with(m.fit,exp(Estimate-1.96*`Std. Error`))
  m.fit$RR_uci=with(m.fit,exp(Estimate+1.96*`Std. Error`))
  m.fit}

ue1.fit<-gam.uri_epi(urie_s1);ue1.fit$Region="Seoul"
ue2.fit<-gam.uri_epi(urie_s2);ue2.fit$Region="Busan"
ue3.fit<-gam.uri_epi(urie_s3);ue3.fit$Region="Daegu"
ue4.fit<-gam.uri_epi(urie_s4);ue4.fit$Region="Incheon"
ue5.fit<-gam.uri_epi(urie_s5);ue5.fit$Region="Gwangju"
ue6.fit<-gam.uri_epi(urie_s6);ue6.fit$Region="Daejeon"
ue7.fit<-gam.uri_epi(urie_s7);ue7.fit$Region="Ulsan"

ue.fit<-rbind(ue1.fit,ue2.fit,ue3.fit,ue4.fit,ue5.fit,ue6.fit,ue7.fit)
write.csv(ue.fit,file="ue.fit.csv",row.names=F)
####################################################################################################################################
####################################################################################################################################
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
#####################################################################################################################################
#####################################################################################################################################
#meta analysis : Total 
m01<-subset(gam.fit,category=="Total"   & lag=="lag04")
m02<-subset(gam.fit,category=="Male"    & lag=="lag04")
m03<-subset(gam.fit,category=="Female"  & lag=="lag04")
m04<-subset(fit.season,category=="warm" & lag=="lag04")
m05<-subset(fit.season,category=="cold" & lag=="lag04")
m06<-subset(uri.fit,category=="URI Yes" & lag=="lag04")
m07<-subset(uri.fit,category=="URI No " & lag=="lag04")


m01$Region=paste0(m01$Region)         ;m01$Region=factor(m01$Region,levels=unique(m01$Region))
m02$Region=paste0(m02$Region," ")     ;m02$Region=factor(m02$Region,levels=unique(m02$Region))
m03$Region=paste0(m03$Region,"  ")    ;m03$Region=factor(m03$Region,levels=unique(m03$Region))
m04$Region=paste0(m04$Region,"   ")   ;m04$Region=factor(m04$Region,levels=unique(m04$Region)) 
m05$Region=paste0(m05$Region,"    ")  ;m05$Region=factor(m05$Region,levels=unique(m05$Region))
m06$Region=paste0(m06$Region,"     ") ;m06$Region=factor(m06$Region,levels=unique(m06$Region))
m07$Region=paste0(m07$Region,"      ");m07$Region=factor(m07$Region,levels=unique(m07$Region))

row.names(m01)=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan")
row.names(m02)=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan")
row.names(m03)=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan")
row.names(m04)=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan")
row.names(m05)=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan")
row.names(m06)=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan")
row.names(m07)=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan")

m<-rbind(m01,m02,m03,m04,m05,m06,m07)

uni01 <- with(m01,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni02 <- with(m02,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni03 <- with(m03,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni04 <- with(m04,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni05 <- with(m05,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni06 <- with(m06,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni07 <- with(m07,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))

round(uni05$I2,3)
round(uni05$QE,2)
round(uni05$QEp,3)

RM.res<-rma(yi=m$Estimate,sei=m$`Std. Error`,data=dat,slab=m$Region)
x11();par(mar=c(4,4,1,2));
forest(RM.res, xlim=c(0.8, 1.2), ylim=c(12, 100), steps=4, rows=c(94:88,82:76,70:64,58:52,46:40,34:28,22:16),refline=1,
       order=c(1:nrow(m)),transf = exp,bg=4,col=2,digits=3,cex=0.75,xlab="", mlab="")

addpoly(uni01,transf=exp,cex=0.85,row=61+24.0,col="red",digits=3,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==00.00~"%, "~"Q=2.45, P-value=0.874"))
addpoly(uni02,transf=exp,cex=0.85,row=49+24.0,col="red",digits=3,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==00.00~"%, "~"Q=2.78, P-value=0.835"))
addpoly(uni03,transf=exp,cex=0.85,row=37+24.0,col="red",digits=3,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==00.00~"%, "~"Q=2.76, P-value=0.839"))
addpoly(uni04,transf=exp,cex=0.85,row=25+24.0,col="red",digits=3,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==00.00~"%, "~"Q=4.16, P-value=0.655"))
addpoly(uni05,transf=exp,cex=0.85,row=13+24.0,col="red",digits=3,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==00.00~"%, "~"Q=5.39, P-value=0.495"))
addpoly(uni06,transf=exp,cex=0.85,row=13+12.0,col="red",digits=3,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==15.00~"%, "~"Q=4.46, P-value=0.615"))
addpoly(uni07,transf=exp,cex=0.85,row=13.0,col="red",digits=3   ,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==00.00~"%, "~"Q=2.25, P-value=0.896"))

text(0.8,95.5 , pos=4, cex=1.1, paste("Total"),col="blue",font=4)
text(0.8,83.5 , pos=4, cex=1.1, paste("Boys"),col="blue",font=4)
text(0.8,71.5 , pos=4, cex=1.1, paste("Girls"),col="blue",font=4)
text(0.8,59.5 , pos=4, cex=1.1, paste("Warm season"),col="blue",font=4)
text(0.8,47.5 , pos=4, cex=1.1, paste("Cold season"),col="blue",font=4)
text(0.8,35.5 , pos=4, cex=1.1, paste("Children with URI history"),col="blue",font=4)
text(0.8,23.5 , pos=4, cex=1.1, paste("Children without URI history "),col="blue",font=4)
text(0.8 ,100 , pos=4, cex=1.1, paste("Regions"),font=4)
text(1.16,100 , pos=4, cex=1.1, paste("Relative Risk [95% CI]"),font=4)

tiff(filename="C:\\Users\\a\\Desktop\\집에서\\20200418\\revision_figure\\Fig.2.tiff",width=3600,height=4200,res=400)
par(mar=c(4,4,1,2));
forest(RM.res, xlim=c(0.8, 1.2), ylim=c(12, 99), steps=4, rows=c(94:88,82:76,70:64,58:52,46:40,34:28,22:16),refline=1,
       order=c(1:nrow(m)),transf = exp,bg=4,col=2,digits=3,cex=0.75,xlab="", mlab="")

addpoly(uni01,transf=exp,cex=0.85,row=61+24.0,col="red",digits=3,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==00.00~"%, "~"Q=2.45, P-value=0.874"))
addpoly(uni02,transf=exp,cex=0.85,row=49+24.0,col="red",digits=3,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==00.00~"%, "~"Q=2.78, P-value=0.835"))
addpoly(uni03,transf=exp,cex=0.85,row=37+24.0,col="red",digits=3,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==00.00~"%, "~"Q=2.76, P-value=0.839"))
addpoly(uni04,transf=exp,cex=0.85,row=25+24.0,col="red",digits=3,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==00.00~"%, "~"Q=4.16, P-value=0.655"))
addpoly(uni05,transf=exp,cex=0.85,row=13+24.0,col="red",digits=3,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==00.00~"%, "~"Q=5.39, P-value=0.495"))
addpoly(uni06,transf=exp,cex=0.85,row=13+12.0,col="red",digits=3,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==15.00~"%, "~"Q=4.46, P-value=0.615"))
addpoly(uni07,transf=exp,cex=0.85,row=13.0,col="red",digits=3   ,mlab=expression("\nRandom effect model \nHeterogeneity:"~I^2==00.00~"%, "~"Q=2.25, P-value=0.896"))

text(0.8,95.5 , pos=4, cex=1.0, expression("Total"),col="blue",font=4)
text(0.8,83.2 , pos=4, cex=1.0, expression("Boys"),col="blue",font=4)
text(0.8,71.4 , pos=4, cex=1.0, expression("Girls"),col="blue",font=4)
text(0.8,59.4 , pos=4, cex=1.0, expression("Warm season"),col="blue",font=4)
text(0.8,47.4 , pos=4, cex=1.0, expression("Cold season"),col="blue",font=4)
text(0.8,35.2 , pos=4, cex=1.0, expression("Children with URI history"),col="blue",font=4)
text(0.8,23.2 , pos=4, cex=1.0, expression("Children without URI history "),col="blue",font=4)
text(0.8 ,98.5 , pos=4, cex=1.0, paste("Regions"),font=4)
text(1.11,98.5 , pos=4, cex=1.0, paste("Relative Risk [95% CI]"),font=4)
dev.off()
##################################################################################################################################################################
#5-day moving average meta analysis 
uri<-read.csv("C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_AOM\\analysis\\result\\uri_revise.csv",header=T)
head(ue.fit,1)
u01<-subset(ue.fit,category=="URI E1" & lag=="lag04")
u02<-subset(ue.fit,category=="URI E2" & lag=="lag04")
u03<-subset(ue.fit,category=="URI E3" & lag=="lag04")
u04<-subset(ue.fit,category=="URI E4" & lag=="lag04")

u01$Region=paste0(u01$Region)       ;u01$Region=factor(u01$Region,levels=unique(u01$Region))
u02$Region=paste0(u02$Region," ")   ;u02$Region=factor(u02$Region,levels=unique(u02$Region))
u03$Region=paste0(u03$Region,"  ")  ;u03$Region=factor(u03$Region,levels=unique(u03$Region))
u04$Region=paste0(u04$Region,"   ") ;u04$Region=factor(u04$Region,levels=unique(u04$Region))

u<-rbind(u01,u02,u03,u04)
RM.res<-rma(yi=u$Estimate,sei=`Std. Error`,data=u,slab=u$Region)

uni01 <- with(u01,rma(yi=Estimate, sei=`Std. Error`,slab=Region, measure="RR",digits=5))
uni02 <- with(u02,rma(yi=Estimate, sei=`Std. Error`,slab=Region, measure="RR",digits=5))
uni03 <- with(u03,rma(yi=Estimate, sei=`Std. Error`,slab=Region, measure="RR",digits=5))
uni04 <- with(u04,rma(yi=Estimate, sei=`Std. Error`,slab=Region, measure="RR",digits=5))

x11();par(mar=c(4,4,1,2));
forest(RM.res, xlim=c(0.8, 1.2), ylim=c(20, 69), steps=4, rows=c(65:59,53:47,41:35,29:23),refline=1,
       order=c(1:nrow(u)),transf = exp,bg=4,col=2,digits=3,cex=1.1,xlab="", mlab="")
addpoly(uni01,transf=exp,cex=1.25,row=57,col="red",digits=3,mlab="RE Model for Subgroup: URI episode 1 (0-7 days)")
addpoly(uni02,transf=exp,cex=1.25,row=45,col="red",digits=3,mlab="RE Model for Subgroup: URI episode 2 (8-14 days)")
addpoly(uni03,transf=exp,cex=1.25,row=33,col="red",digits=3,mlab="RE Model for Subgroup: URI episode 3 (15-21 days)")
addpoly(uni04,transf=exp,cex=1.25,row=21,col="red",digits=3,mlab="RE Model for Subgroup: URI episode 4 (22-28 day)")

text(0.8,66.3, pos=4, cex=1.3, paste("URI Episode 1"),col="blue",font=4)
text(0.8,54.3, pos=4, cex=1.3, paste("URI Episode 2"),col="blue",font=4)
text(0.8,42.3, pos=4, cex=1.3, paste("URI Episode 3"),col="blue",font=4)
text(0.8,30.3, pos=4, cex=1.3, paste("URI Episode 4"),col="blue",font=4)
text(0.8,68  , pos=4, cex=1.3, paste("Regions"),font=4)
text(1.115,68 , pos=4, cex=1.3, paste("Relative Risk [95% CI]"),font=4)

tiff(filename="C:\\Users\\a\\Desktop\\집에서\\20200418\\revision_figure\\Fig.3.tiff",width=3600,height=4000,res=300)
par(mar=c(4,4,1,2));
forest(RM.res, xlim=c(0.8, 1.2), ylim=c(20, 69), steps=4, rows=c(65:59,53:47,41:35,29:23),refline=1,
       order=c(1:nrow(u)),transf = exp,bg=4,col=2,digits=3,cex=1.1,xlab="", mlab="")
addpoly(uni01,transf=exp,cex=1.25,row=57,col="red",digits=3,mlab="RE Model for Subgroup: URI episode 1 (0-7 days)")
addpoly(uni02,transf=exp,cex=1.25,row=45,col="red",digits=3,mlab="RE Model for Subgroup: URI episode 2 (8-14 days)")
addpoly(uni03,transf=exp,cex=1.25,row=33,col="red",digits=3,mlab="RE Model for Subgroup: URI episode 3 (15-21 days)")
addpoly(uni04,transf=exp,cex=1.25,row=21,col="red",digits=3,mlab="RE Model for Subgroup: URI episode 4 (22-28 day)")

text(0.8,66.3, pos=4, cex=1.3, paste("URI Episode 1"),col="blue",font=4)
text(0.8,54.3, pos=4, cex=1.3, paste("URI Episode 2"),col="blue",font=4)
text(0.8,42.3, pos=4, cex=1.3, paste("URI Episode 3"),col="blue",font=4)
text(0.8,30.3, pos=4, cex=1.3, paste("URI Episode 4"),col="blue",font=4)
text(0.8,68  , pos=4, cex=1.3, paste("Regions"),font=4)
text(1.115,68 , pos=4, cex=1.3, paste("Relative Risk [95% CI]"),font=4)
dev.off()
#####################################################################################################################################################################
#####################################################################################################################################################################
#two stage Meta analysis using DLNM
dat<-aom_w4
regions<-as.character(unique(dat$SIDO))
dat$region=NA
dat$exposure=dat$pm*10

#시도별로 datalist화 시킴
datalist <- lapply(regions, function(region) dat[dat$SIDO==region,])
names(datalist) <- regions

#coef를 시도별로 넣기, B-spline knot 4 ; 애는 lag에 대한 것 (시점)
coefl1 <- matrix(NA,length(datalist), 4, dimnames=list(regions,paste0("b",seq(4))))
coefl2 <- matrix(NA,length(datalist), 4, dimnames=list(regions,paste0("b",seq(4))))
coefl3 <- matrix(NA,length(datalist), 4, dimnames=list(regions,paste0("b",seq(4))))
coefl4 <- matrix(NA,length(datalist), 4, dimnames=list(regions,paste0("b",seq(4))))
coefl5 <- matrix(NA,length(datalist), 4, dimnames=list(regions,paste0("b",seq(4))))

#coef를 시도별로 넣기, Natural cubic spline ; 애는 var에 대한 것 (노출)
coefv1 <- matrix(NA,length(datalist), 5, dimnames=list(regions,paste0("b",seq(5))))
coefv2 <- matrix(NA,length(datalist), 5, dimnames=list(regions,paste0("b",seq(5))))
coefv3 <- matrix(NA,length(datalist), 5, dimnames=list(regions,paste0("b",seq(5))))
coefv4 <- matrix(NA,length(datalist), 5, dimnames=list(regions,paste0("b",seq(5))))
coefv5 <- matrix(NA,length(datalist), 5, dimnames=list(regions,paste0("b",seq(5))))

Sl1 <- vector("list",length(datalist));names(Sl1) <- regions
Sl2 <- vector("list",length(datalist));names(Sl2) <- regions
Sl3 <- vector("list",length(datalist));names(Sl3) <- regions
Sl4 <- vector("list",length(datalist));names(Sl4) <- regions
Sl5 <- vector("list",length(datalist));names(Sl5) <- regions

Sv1 <- vector("list",length(datalist));names(Sv1) <- regions
Sv2 <- vector("list",length(datalist));names(Sv2) <- regions
Sv3 <- vector("list",length(datalist));names(Sv3) <- regions
Sv4 <- vector("list",length(datalist));names(Sv4) <- regions
Sv5 <- vector("list",length(datalist));names(Sv5) <- regions

bound <- rowMeans(sapply(datalist, function(x) range(x$exposure)))
#argvar : 예측 변수의 공간에 대한 행렬
varknots <- equalknots(bound, fun="bs", degree=2, df=4)
argvar <- list(fun="bs", degree=2, knots=varknots, Bound=bound)

#lag를 몇으로 할지, knots를 몇으로 할지 
lagknots1 <- logknots(4,  df=5, int=T)
lagknots2 <- logknots(7,  df=5, int=T)
lagknots3 <- logknots(14, df=5, int=T)
lagknots4 <- logknots(21, df=5, int=T)
lagknots5 <- logknots(28, df=5, int=T)

arglag1 <- list(fun="ns", knots=lagknots1) 
arglag2 <- list(fun="ns", knots=lagknots2) 
arglag3 <- list(fun="ns", knots=lagknots3) 
arglag4 <- list(fun="ns", knots=lagknots4) 
arglag5 <- list(fun="ns", knots=lagknots5) 

for(i in seq(datalist)) {
  cb1 <- crossbasis(datalist[[i]]$exposure, lag=4,  argvar=argvar, arglag=arglag1)
  cb2 <- crossbasis(datalist[[i]]$exposure, lag=7,  argvar=argvar, arglag=arglag2)
  cb3 <- crossbasis(datalist[[i]]$exposure, lag=14, argvar=argvar, arglag=arglag3)
  cb4 <- crossbasis(datalist[[i]]$exposure, lag=21, argvar=argvar, arglag=arglag4)
  cb5 <- crossbasis(datalist[[i]]$exposure, lag=28, argvar=argvar, arglag=arglag5)
  
  #modeling
  m1 <- gam(TOT ~ cb1 + s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=datalist[[i]])
  m2 <- gam(TOT ~ cb2 + s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=datalist[[i]])
  m3 <- gam(TOT ~ cb3 + s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=datalist[[i]])
  m4 <- gam(TOT ~ cb4 + s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=datalist[[i]])
  m5 <- gam(TOT ~ cb5 + s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=datalist[[i]])
  
  crl1 <- crossreduce(cb1,m1,type="lag",cen=0,value=0,bylag=0.1); crv1 <- crossreduce(cb1,m1,type="var",cen=0,value=4 ,bylag=0.1) 
  crl2 <- crossreduce(cb2,m2,type="lag",cen=0,value=0,bylag=0.1); crv2 <- crossreduce(cb2,m2,type="var",cen=0,value=7 ,bylag=0.1) 
  crl3 <- crossreduce(cb3,m3,type="lag",cen=0,value=0,bylag=0.1); crv3 <- crossreduce(cb3,m3,type="var",cen=0,value=14,bylag=0.1) 
  crl4 <- crossreduce(cb4,m4,type="lag",cen=0,value=0,bylag=0.1); crv4 <- crossreduce(cb4,m4,type="var",cen=0,value=21,bylag=0.1) 
  crl5 <- crossreduce(cb5,m5,type="lag",cen=0,value=0,bylag=0.1); crv5 <- crossreduce(cb5,m5,type="var",cen=0,value=28,bylag=0.1) 
  
  #estimate value
  coefl1[i,] <- coef(crl1) ;coefv1[i,] <- coef(crv1) ;
  coefl2[i,] <- coef(crl2) ;coefv2[i,] <- coef(crv2) ;
  coefl3[i,] <- coef(crl3) ;coefv3[i,] <- coef(crv3) ;
  coefl4[i,] <- coef(crl4) ;coefv4[i,] <- coef(crv4) ;
  coefl5[i,] <- coef(crl5) ;coefv5[i,] <- coef(crv5) ;
  
  #within-region (Co)variance matrices 
  Sl1[[i]] <- vcov(crl1) ;Sv1[[i]] <- vcov(crv1)
  Sl2[[i]] <- vcov(crl2) ;Sv2[[i]] <- vcov(crv2)
  Sl3[[i]] <- vcov(crl3) ;Sv3[[i]] <- vcov(crv3)
  Sl4[[i]] <- vcov(crl4) ;Sv4[[i]] <- vcov(crv4)
  Sl5[[i]] <- vcov(crl5) ;Sv5[[i]] <- vcov(crv5)
  
  print(i)}

mixl1 <- mixmeta(coefl1~1, Sl1, method="ml") ; mixv1 <- mixmeta(coefv1~1, Sv1, method="ml")
mixl2 <- mixmeta(coefl2~1, Sl2, method="ml") ; mixv2 <- mixmeta(coefv2~1, Sv2, method="ml")
mixl3 <- mixmeta(coefl3~1, Sl3, method="ml") ; mixv3 <- mixmeta(coefv3~1, Sv3, method="ml")
mixl4 <- mixmeta(coefl4~1, Sl4, method="ml") ; mixv4 <- mixmeta(coefv4~1, Sv4, method="ml")
mixl5 <- mixmeta(coefl5~1, Sl5, method="ml") ; mixv5 <- mixmeta(coefv5~1, Sv5, method="ml")

print(summary(mixl1), digits=3)
print(summary(mixl2), digits=3)
print(summary(mixl3), digits=3)
print(summary(mixl4), digits=3)
print(summary(mixl5), digits=3)

print(summary(mixv1), digits=3)
print(summary(mixv2), digits=3)
print(summary(mixv3), digits=3)
print(summary(mixv4), digits=3)
print(summary(mixv5), digits=3)

xvar <- seq(bound[1], bound[2], by=0.1)

bvar1  <- do.call("onebasis", c(list(x=xvar), attr(cb1,"argvar")))
bvar2  <- do.call("onebasis", c(list(x=xvar), attr(cb2,"argvar")))
bvar3  <- do.call("onebasis", c(list(x=xvar), attr(cb3,"argvar")))
bvar4  <- do.call("onebasis", c(list(x=xvar), attr(cb4,"argvar")))
bvar5  <- do.call("onebasis", c(list(x=xvar), attr(cb5,"argvar"))))
predpooll1 <- crosspred(bvar1, coef=coef(mixl1), vcov=vcov(mixl1), model.link="log",by=0.1,cen=0)
predpooll2 <- crosspred(bvar2, coef=coef(mixl2), vcov=vcov(mixl2), model.link="log",by=0.1,cen=0)
predpooll3 <- crosspred(bvar3, coef=coef(mixl3), vcov=vcov(mixl3), model.link="log",by=0.1,cen=0)
predpooll4 <- crosspred(bvar4, coef=coef(mixl4), vcov=vcov(mixl4), model.link="log",by=0.1,cen=0)
predpooll5 <- crosspred(bvar5, coef=coef(mixl5), vcov=vcov(mixl5), model.link="log",by=0.1,cen=0)

# predreg1 <- lapply(seq(nrow(coef1)),function(i) crosspred(bvar, coef=coef1[i,],vcov=S1[[i]], model.link="log",cen=0))
# predreg2 <- lapply(seq(nrow(coef2)),function(i) crosspred(bvar, coef=coef2[i,],vcov=S2[[i]], model.link="log",cen=0))
# predreg3 <- lapply(seq(nrow(coef3)),function(i) crosspred(bvar, coef=coef3[i,],vcov=S3[[i]], model.link="log",cen=0))
# predreg4 <- lapply(seq(nrow(coef4)),function(i) crosspred(bvar, coef=coef4[i,],vcov=S4[[i]], model.link="log",cen=0))
# predreg5 <- lapply(seq(nrow(coef5)),function(i) crosspred(bvar, coef=coef5[i,],vcov=S5[[i]], model.link="log",cen=0))

p0 <- crosspred(bvar2, coef=coef(mixl2), vcov=vcov(mixl2), model.link="log",by=0.1,cen=0)
p1 <- crosspred(bvar2, coef=coef(mixl2), vcov=vcov(mixl2), model.link="log",by=0.1,cen=15)
p2 <- crosspred(bvar2, coef=coef(mixl2), vcov=vcov(mixl2), model.link="log",by=0.1,cen=35)

x11();par(mfrow=c(1,2));
plot(p1, "overall", ci=, ylab="Relative Risk for Acute otitis media", ylim=c(.8,1.5), lwd=2,col="blue",
     xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)), main="",cex.lab=1.35,cex.axis=1.35)
plot(p2, "overall", ci=, ylab="Relative Risk for Acute otitis media", ylim=c(.8,1.5), lwd=2,col="blue",
     xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)), main="",cex.lab=1.35,cex.axis=1.35)

x11();
plot(p1, "overall", ci=, ylab="Relative Risk for Acute otitis media", ylim=c(.8,1.5), lwd=2,col="blue",
     xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)), main="",cex.lab=1.35,cex.axis=1.35)
lines(p2,col="red")

lines(predpooll2,col="red",type="l")
lines(predpooll3,col="black",type="l")
lines(predpooll4,col="green",type="l")
lines(predpooll5,col="yellow",type="l")

summary(predpooll1);summary(predpooll2)
summary(predpooll3);summary(predpooll4)

a1<-crossreduce(cb1,m1,type="var",cen=0,value=4,bylag=0.1)
a2<-crossreduce(cb2,m2,type="var",cen=0,value=7,bylag=0.1) 
a3<-crossreduce(cb3,m3,type="var",cen=0,value=14,bylag=0.1) 
a4<-crossreduce(cb4,m4,type="var",cen=0,value=21,bylag=0.1) 
a5<-crossreduce(cb5,m5,type="var",cen=0,value=28,bylag=0.1) 

predpoolv1 <- crosspred(a1$basis, coef=coef(mixv1), vcov=vcov(mixv1), model.link="log" ,by=0.1,cen=0)
predpoolv2 <- crosspred(a2$basis, coef=coef(mixv2), vcov=vcov(mixv2), model.link="log" ,by=0.1,cen=0)
predpoolv3 <- crosspred(a3$basis, coef=coef(mixv3), vcov=vcov(mixv3), model.link="log" ,by=0.1,cen=0)
predpoolv4 <- crosspred(a4$basis, coef=coef(mixv4), vcov=vcov(mixv4), model.link="log" ,by=0.1,cen=0)
predpoolv5 <- crosspred(a5$basis, coef=coef(mixv5), vcov=vcov(mixv5), model.link="log" ,by=0.1,cen=0)

summary(predpoolv1)
summary(predpoolv2)
summary(predpoolv3)
summary(predpoolv4)
summary(predpoolv5)

x11();plot(p0, "overall", ci=, ylab="Relative Risk for Acute otitis media", ylim=c(.8,1.5), lwd=2,col="blue",
           xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)), main="",cex.lab=1.35,cex.axis=1.35)

tiff(filename="C:\\Users\\a\\Desktop\\집에서\\20200418\\revision_figure\\Fig4.tiff",width=2800,height=2800,res=300)
plot(p0, "overall", ci=, ylab="Relative Risk for Acute otitis media", ylim=c(.8,1.5), lwd=2,col="blue",
     xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)), main="",cex.lab=1.35,cex.axis=1.35)
dev.off()
# x11();plot(predpoolv1, ylab="Relative Risk for Acute otitis media", ylim=c(.95,1.1),col="black",xlim=c(0,30),
#            xlab="Lag", main="Pooled and first-stage",cex.lab=1.35,cex.axis=1.35,lwd=4)
# 
# lines(predpoolv2,col="red",lwd=4)
# lines(predpoolv3,col="blue",lwd=4)
# lines(predpoolv4,col="green",lwd=4)
# lines(predpoolv5,col="yellow",lwd=4)

pool_df1<-data.frame(lag=predpoolv1$predvar,
                     e=predpoolv1$allRRfit,
                     lci=predpoolv1$allRRlow,
                     uci=predpoolv1$allRRhigh)

pool_df2<-data.frame(lag=predpoolv2$predvar,
                     e=predpoolv2$allRRfit,
                     lci=predpoolv2$allRRlow,
                     uci=predpoolv2$allRRhigh)

pool_df3<-data.frame(lag=predpoolv3$predvar,
                     e=predpoolv3$allRRfit,
                     lci=predpoolv3$allRRlow,
                     uci=predpoolv3$allRRhigh)

pool_df4<-data.frame(lag=predpoolv4$predvar,
                     e=predpoolv4$allRRfit,
                     lci=predpoolv4$allRRlow,
                     uci=predpoolv4$allRRhigh)

pool_df5<-data.frame(lag=predpoolv5$predvar,
                     e=predpoolv5$allRRfit,
                     lci=predpoolv5$allRRlow,
                     uci=predpoolv5$allRRhigh)

pool_df1$g="Lag period of 04" #4
pool_df2$g="Lag period of 07" #2~7
pool_df3$g="Lag period of 14" #3~7
pool_df4$g="Lag period of 21" #3~9
pool_df5$g="Lag period of 28" #4~9

pool_df<-rbind(pool_df2,pool_df3,pool_df4,pool_df5)

x11();ggplot(pool_df,aes(lag,e,col=g))+geom_line(size=1.1)+ylim(0.975,1.05)+theme_bw(base_size=15)+
  labs(x="Lag",y="Relative Risk")+scale_x_continuous(breaks=1:31-1)+geom_hline(yintercept =1,size=0.9)+scale_color_manual(values=c("red","blue","green","yellow"))+
  theme(legend.title=element_blank())

tiff(filename="C:\\Users\\a\\Desktop\\집에서\\20200418\\revision_figure\\Fig.S8.tiff",width=4000,height=2800,res=300)
ggplot(pool_df,aes(lag,e,col=g))+geom_line(size=1.1)+ylim(0.975,1.05)+theme_bw(base_size=15)+
  labs(x="Lag",y="Relative Risk")+scale_x_continuous(breaks=1:31-1)+geom_hline(yintercept =1,size=0.9)+scale_color_manual(values=c("red","blue","green","yellow"))+
  theme(legend.title=element_blank())
dev.off()

#######################################################################################################################
#######################################################################################################################
pool_cen0<-pool_df

pool1_var15<-predpooll1
pool2_var15<-predpooll2
pool3_var15<-predpooll3
pool4_var15<-predpooll4
pool5_var15<-predpooll5

# x11();plot(predpooll1, "overall", ci=, ylab="Relative Risk for Acute otitis media", ylim=c(.8,1.5), lwd=2,col="blue",
#            xlab=expression(paste(PM[2.5]," (",mu,g/m^3," ) ",concentration)), main="",cex.lab=1.35,cex.axis=1.35)
# 
# lines(predpooll2,col="red",type="l")
# lines(predpooll3,col="black",type="l")
# lines(predpooll4,col="green",type="l")
# lines(predpooll5,col="yellow",type="l")

library(gridExtra)
x11();
grid.arrange(ggplot(pool_cen0,aes(lag,e,col=g))+geom_line(size=1.1)+ylim(0.975,1.05)+theme_bw(base_size=15)+
               labs(x="Lag",y="Relative Risk")+scale_x_continuous(breaks=1:31-1)+geom_hline(yintercept =1,size=0.9)+scale_color_manual(values=c("black","red","blue","green","yellow"))+
               theme(legend.title=element_blank()),
             
             ggplot(pool_cen15,aes(lag,e,col=g))+geom_line(size=1.1)+ylim(0.975,1.05)+theme_bw(base_size=15)+
               labs(x="Lag",y="Relative Risk")+scale_x_continuous(breaks=1:31-1)+geom_hline(yintercept =1,size=0.9)+scale_color_manual(values=c("black","red","blue","green","yellow"))+
               theme(legend.title=element_blank()),
             
             ggplot(pool_cen35,aes(lag,e,col=g))+geom_line(size=1.1)+ylim(0.975,1.05)+theme_bw(base_size=15)+
               labs(x="Lag",y="Relative Risk")+scale_x_continuous(breaks=1:31-1)+geom_hline(yintercept =1,size=0.9)+scale_color_manual(values=c("black","red","blue","green","yellow"))+
               theme(legend.title=element_blank()),ncol=3)

x11();ggplot(pool_cen0,aes(lag,e,col=g))+geom_line(size=1.1)+ylim(0.975,1.05)+theme_bw(base_size=15)+
  labs(x="Lag",y="Relative Risk")+scale_x_continuous(breaks=1:31-1)+geom_hline(yintercept =1,size=0.9)+scale_color_manual(values=c("black","red","blue","green","yellow"))+
  theme(legend.title=element_blank())
#####################################################################################################################################################################
#####################################################################################################################################################################
fit1<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson" ,data=s1.w4)
fit2<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson" ,data=s2.w4)
fit3<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson" ,data=s3.w4)
fit4<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson" ,data=s4.w4)
fit5<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson" ,data=s5.w4)
fit6<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson" ,data=s6.w4)
fit7<-gam(TOT~lag04+s(as.numeric(DATE)  ,k=6*9)+s(AT,k=6)+dow+month,family="quasipoisson" ,data=s7.w4)

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

tiff(filename="C:\\Users\\a\\Desktop\\집에서\\20200418\\revision_figure\\Fig.S7.tiff",width=4000,height=2800,res=300)
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
#############################################################################################################
#############################################################################################################
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

slibrary(MuMIn)
model.sel(m1, m2, m3,rank = QIC)

-2*logLik(m3)+2*5

dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)}

(sum(residuals(m1,"pearson")^2)/m1$df.residual);dfun(m1)
(sum(residuals(m2,"pearson")^2)/m2$df.residual);dfun(m2)
(sum(residuals(m3,"pearson")^2)/m3$df.residual);dfun(m3)

(qAICc(m1,dispersion=dfun(m1),nobs=length(s1.w4$TOT)))
(qAICc(m2,dispersion=dfun(m1),nobs=length(s1.w4$TOT)))
(qAICc(m3,dispersion=dfun(m1),nobs=length(s1.w4$TOT)))

x.quasipoisson <- function(...) {
  res <- quasipoisson(...)
  res$aic <- poisson(...)$aic
  res}

library(MuMIn)
m4<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family = "x.quasipoisson",data=s1.w4,na.action=na.fail)
gg <- dredge(m4,rank="QAIC", chat=dfun(m4))
gg
m5<-gam(TOT ~lag04+s(as.numeric(DATE),k=9*6)+s(AT,k=6)+dow,family="poisson",data=s1.w4,na.action=na.fail)
gg <- dredge(m5,rank="QAIC", chat=dfun(m5))
##################################################################################################################################
##################################################################################################################################
#Monitoring AP
setwd("C:\\Users\\a\\Desktop\\집에서\\20200418\\AP")

ap07<-as.data.frame(read_excel("2007_Monitoring_AP.xlsx",sheet=3))
ap08<-as.data.frame(read_excel("2008_Monitoring_AP.xlsx",sheet=3))
ap09<-as.data.frame(read_excel("2009_Monitoring_AP.xlsx",sheet=3))
ap10<-as.data.frame(read_excel("2010_Monitoring_AP.xlsx",sheet=3))
ap11<-as.data.frame(read_excel("2011_Monitoring_AP.xlsx",sheet=3))
ap12<-as.data.frame(read_excel("2012_Monitoring_AP.xlsx",sheet=3))
ap13<-as.data.frame(read_excel("2013_Monitoring_AP.xlsx",sheet=3))
ap14<-as.data.frame(read_excel("2014_Monitoring_AP.xlsx",sheet=3))
ap15<-as.data.frame(read_excel("2015_Monitoring_AP.xlsx",sheet=3))
ap16<-as.data.frame(read_excel("2016_Monitoring_AP.xlsx",sheet=3))

ap<-rbind(ap07,ap08,ap09,ap10,ap11,ap12,ap13,ap14,ap15,ap16)
ap<-subset(ap,sido %in% c("서울","부산","대구","인천","광주","대전","울산") )

ap$SIDO=with(ap,ifelse(sido=="서울",11,
                       ifelse(sido=="부산",26,
                              ifelse(sido=="대구",27,
                                     ifelse(sido=="인천",28,
                                            ifelse(sido=="광주",29,
                                                   ifelse(sido=="대전",30,31)))))))

ap$key=with(ap,paste0(SIDO,"-",yymmdd))
ap_PM25<-ap %>% select(yymmdd,SIDO,key,PM25) ;names(ap_PM25)[4]="exposure"
ap_PM10<-ap %>% select(yymmdd,SIDO,key,PM10) ;names(ap_PM10)[4]="exposure"
ap_SO2 <-ap %>% select(yymmdd,SIDO,key,SO2)  ;names(ap_SO2)[4]="exposure"
ap_NO2 <-ap %>% select(yymmdd,SIDO,key,NO2)  ;names(ap_NO2)[4]="exposure"
ap_CO  <-ap %>% select(yymmdd,SIDO,key,CO)   ;names(ap_CO)[4]="exposure"
ap_O3  <-ap %>% select(yymmdd,SIDO,key,O3)   ;names(ap_O3)[4]="exposure"

ap1_s1<-subset(ap_PM25,SIDO==11);ap2_s1<-subset(ap_PM10,SIDO==11);ap3_s1<-subset(ap_SO2,SIDO==11)
ap1_s2<-subset(ap_PM25,SIDO==26);ap2_s2<-subset(ap_PM10,SIDO==26);ap3_s2<-subset(ap_SO2,SIDO==26)
ap1_s3<-subset(ap_PM25,SIDO==27);ap2_s3<-subset(ap_PM10,SIDO==27);ap3_s3<-subset(ap_SO2,SIDO==27)
ap1_s4<-subset(ap_PM25,SIDO==28);ap2_s4<-subset(ap_PM10,SIDO==28);ap3_s4<-subset(ap_SO2,SIDO==28)
ap1_s5<-subset(ap_PM25,SIDO==29);ap2_s5<-subset(ap_PM10,SIDO==29);ap3_s5<-subset(ap_SO2,SIDO==29)
ap1_s6<-subset(ap_PM25,SIDO==30);ap2_s6<-subset(ap_PM10,SIDO==30);ap3_s6<-subset(ap_SO2,SIDO==30)
ap1_s7<-subset(ap_PM25,SIDO==31);ap2_s7<-subset(ap_PM10,SIDO==31);ap3_s7<-subset(ap_SO2,SIDO==31)

ap4_s1<-subset(ap_NO2,SIDO==11) ;ap5_s1<-subset(ap_CO,SIDO==11)  ;ap6_s1<-subset(ap_O3,SIDO==11)
ap4_s2<-subset(ap_NO2,SIDO==26) ;ap5_s2<-subset(ap_CO,SIDO==26)  ;ap6_s2<-subset(ap_O3,SIDO==26)
ap4_s3<-subset(ap_NO2,SIDO==27) ;ap5_s3<-subset(ap_CO,SIDO==27)  ;ap6_s3<-subset(ap_O3,SIDO==27)
ap4_s4<-subset(ap_NO2,SIDO==28) ;ap5_s4<-subset(ap_CO,SIDO==28)  ;ap6_s4<-subset(ap_O3,SIDO==28)
ap4_s5<-subset(ap_NO2,SIDO==29) ;ap5_s5<-subset(ap_CO,SIDO==29)  ;ap6_s5<-subset(ap_O3,SIDO==29)
ap4_s6<-subset(ap_NO2,SIDO==30) ;ap5_s6<-subset(ap_CO,SIDO==30)  ;ap6_s6<-subset(ap_O3,SIDO==30)
ap4_s7<-subset(ap_NO2,SIDO==31) ;ap5_s7<-subset(ap_CO,SIDO==31)  ;ap6_s7<-subset(ap_O3,SIDO==31)

ap_mod<-function(data,k){
  d<-data
  d$lag0=with(d,lag(exposure,0))
  d$lag1=with(d,lag(exposure,1))
  d$lag2=with(d,lag(exposure,2))
  d$lag3=with(d,lag(exposure,3))
  d$lag4=with(d,lag(exposure,4))
  d$lag5=with(d,lag(exposure,5))
  d$lag6=with(d,lag(exposure,6))
  d$lag7=with(d,lag(exposure,7))
  
  d$lag01=apply(d %>% select(lag0:lag1),1,mean,na.rm=T)
  d$lag02=apply(d %>% select(lag0:lag2),1,mean,na.rm=T)
  d$lag03=apply(d %>% select(lag0:lag3),1,mean,na.rm=T)
  d$lag04=apply(d %>% select(lag0:lag4),1,mean,na.rm=T)
  d$lag05=apply(d %>% select(lag0:lag5),1,mean,na.rm=T)
  d$lag06=apply(d %>% select(lag0:lag6),1,mean,na.rm=T)
  d$lag07=apply(d %>% select(lag0:lag7),1,mean,na.rm=T)
  d[,4:19]=d[,4:19]/k
  d}

ap_PM25.r<-rbind(ap_mod(ap1_s1,10),ap_mod(ap1_s2,10),ap_mod(ap1_s3,10),ap_mod(ap1_s4,10),
                 ap_mod(ap1_s5,10),ap_mod(ap1_s6,10),ap_mod(ap1_s7,10))

ap_PM10.r<-rbind(ap_mod(ap2_s1,10),ap_mod(ap2_s2,10),ap_mod(ap2_s3,10),ap_mod(ap2_s4,10),
                 ap_mod(ap2_s5,10),ap_mod(ap2_s6,10),ap_mod(ap2_s7,10))

ap_SO2.r<-rbind(ap_mod(ap3_s1,1/1000),ap_mod(ap3_s2,1/1000),ap_mod(ap3_s3,1/1000),ap_mod(ap3_s4,1/1000),
                ap_mod(ap3_s5,1/1000),ap_mod(ap3_s6,1/1000),ap_mod(ap3_s7,1/1000))

ap_NO2.r<-rbind(ap_mod(ap4_s1,1/1000),ap_mod(ap4_s2,1/1000),ap_mod(ap4_s3,1/1000),ap_mod(ap4_s4,1/1000),
                ap_mod(ap4_s5,1/1000),ap_mod(ap4_s6,1/1000),ap_mod(ap4_s7,1/1000))

ap_CO.r<-rbind(ap_mod(ap5_s1,1/1000),ap_mod(ap5_s2,1/1000),ap_mod(ap5_s3,1/1000),ap_mod(ap5_s4,1/1000),
               ap_mod(ap5_s5,1/1000),ap_mod(ap5_s6,1/1000),ap_mod(ap5_s7,1/1000))

ap_O3.r<-rbind(ap_mod(ap6_s1,1/1000),ap_mod(ap6_s2,1/1000),ap_mod(ap6_s3,1/1000),ap_mod(ap6_s4,1/1000),
               ap_mod(ap6_s5,1/1000),ap_mod(ap6_s6,1/1000),ap_mod(ap6_s7,1/1000))

ap_PM25.r <-ap_PM25.r %>% select(-c(yymmdd,SIDO))
ap_PM10.r <-ap_PM10.r %>% select(-c(yymmdd,SIDO))
ap_SO2.r  <-ap_SO2.r  %>% select(-c(yymmdd,SIDO))
ap_NO2.r  <-ap_NO2.r  %>% select(-c(yymmdd,SIDO))
ap_CO.r   <-ap_CO.r   %>% select(-c(yymmdd,SIDO))
ap_O3.r   <-ap_O3.r   %>% select(-c(yymmdd,SIDO))

aom.mm <-aom_w4 %>% select(DATE:yyyy,mtemp:day)

aom_PM25<-merge(aom.mm,ap_PM25.r,by="key",all.x=T)
aom_PM10<-merge(aom.mm,ap_PM10.r,by="key",all.x=T)
aom_SO2 <-merge(aom.mm,ap_SO2.r,by="key",all.x=T)
aom_NO2 <-merge(aom.mm,ap_NO2.r,by="key",all.x=T)
aom_CO  <-merge(aom.mm,ap_CO.r,by="key",all.x=T)
aom_O3  <-merge(aom.mm,ap_O3.r,by="key",all.x=T)

with(aom_PM25,aggregate(lag0*10,list(SIDO),mean,na.rm=T))  ;with(aom_PM25,aggregate(lag0*10,list(SIDO),sd,na.rm=T))
with(aom_PM10,aggregate(lag0*10,list(SIDO),mean,na.rm=T))  ;with(aom_PM10,aggregate(lag0*10,list(SIDO),sd,na.rm=T))
with(aom_SO2 ,aggregate(lag0/1000,list(SIDO),mean,na.rm=T));with(aom_SO2 ,aggregate(lag0/1000,list(SIDO),sd,na.rm=T))
with(aom_NO2 ,aggregate(lag0/1000,list(SIDO),mean,na.rm=T));with(aom_NO2 ,aggregate(lag0/1000,list(SIDO),sd,na.rm=T))
with(aom_CO  ,aggregate(lag0/1000,list(SIDO),mean,na.rm=T));with(aom_CO  ,aggregate(lag0/1000,list(SIDO),sd,na.rm=T))
with(aom_O3  ,aggregate(lag0/1000,list(SIDO),mean,na.rm=T));with(aom_O3  ,aggregate(lag0/1000,list(SIDO),sd,na.rm=T))

mean(aom_PM25$lag0*10,na.rm=T)
sd(aom_PM25$lag0*10,na.rm=T)

mean(aom_PM10$lag0*10,na.rm=T)
sd(aom_PM10$lag0*10,na.rm=T)

mean(aom_SO2$lag0/1000,na.rm=T)
sd(aom_SO2$lag0  /1000,na.rm=T)

mean(aom_NO2$lag0/1000,na.rm=T)
sd(aom_NO2$lag0  /1000,na.rm=T)

mean(aom_CO$lag0/1000,na.rm=T)
sd(aom_CO$lag0  /1000,na.rm=T)

mean(aom_O3$lag0/1000,na.rm=T)
sd(aom_O3$lag0  /1000,na.rm=T)

aom_PM25$season=with(aom_PM25,ifelse(as.numeric(month)>=4 & as.numeric(month) <=9,"warm","cold"))
aom_PM10$season=with(aom_PM10,ifelse(as.numeric(month)>=4 & as.numeric(month) <=9,"warm","cold"))
aom_SO2$season=with(aom_SO2,ifelse(as.numeric(month)>=4 & as.numeric(month) <=9,"warm","cold"))
aom_NO2$season=with(aom_NO2,ifelse(as.numeric(month)>=4 & as.numeric(month) <=9,"warm","cold"))
aom_CO$season =with(aom_CO,ifelse(as.numeric(month)>=4 & as.numeric(month) <=9,"warm","cold"))
aom_O3$season =with(aom_O3,ifelse(as.numeric(month)>=4 & as.numeric(month) <=9,"warm","cold"))

with(aom_PM25,aggregate(lag0*10,list(season),mean,na.rm=T))  ;with(aom_PM25,aggregate(lag0*10,list(season),sd,na.rm=T))
with(aom_PM10,aggregate(lag0*10,list(season),mean,na.rm=T))  ;with(aom_PM10,aggregate(lag0*10,list(season),sd,na.rm=T))
with(aom_SO2 ,aggregate(lag0/1000,list(season),mean,na.rm=T));with(aom_SO2 ,aggregate(lag0/1000,list(season),sd,na.rm=T))
with(aom_NO2 ,aggregate(lag0/1000,list(season),mean,na.rm=T));with(aom_NO2 ,aggregate(lag0/1000,list(season),sd,na.rm=T))
with(aom_CO  ,aggregate(lag0/1000,list(season),mean,na.rm=T));with(aom_CO  ,aggregate(lag0/1000,list(season),sd,na.rm=T))
with(aom_O3  ,aggregate(lag0/1000,list(season),mean,na.rm=T));with(aom_O3  ,aggregate(lag0/1000,list(season),sd,na.rm=T))


s1.PM25<-subset(aom_PM25,SIDO==11); s1.PM10<-subset(aom_PM10,SIDO==11); s1.SO2<-subset(aom_SO2,SIDO==11)
s2.PM25<-subset(aom_PM25,SIDO==26); s2.PM10<-subset(aom_PM10,SIDO==26); s2.SO2<-subset(aom_SO2,SIDO==26)
s3.PM25<-subset(aom_PM25,SIDO==27); s3.PM10<-subset(aom_PM10,SIDO==27); s3.SO2<-subset(aom_SO2,SIDO==27)
s4.PM25<-subset(aom_PM25,SIDO==28); s4.PM10<-subset(aom_PM10,SIDO==28); s4.SO2<-subset(aom_SO2,SIDO==28)
s5.PM25<-subset(aom_PM25,SIDO==29); s5.PM10<-subset(aom_PM10,SIDO==29); s5.SO2<-subset(aom_SO2,SIDO==29)
s6.PM25<-subset(aom_PM25,SIDO==30); s6.PM10<-subset(aom_PM10,SIDO==30); s6.SO2<-subset(aom_SO2,SIDO==30)
s7.PM25<-subset(aom_PM25,SIDO==31); s7.PM10<-subset(aom_PM10,SIDO==31); s7.SO2<-subset(aom_SO2,SIDO==31)

s1.NO2<-subset(aom_NO2,SIDO==11); s1.CO<-subset(aom_CO,SIDO==11); s1.O3<-subset(aom_O3,SIDO==11)
s2.NO2<-subset(aom_NO2,SIDO==26); s2.CO<-subset(aom_CO,SIDO==26); s2.O3<-subset(aom_O3,SIDO==26)
s3.NO2<-subset(aom_NO2,SIDO==27); s3.CO<-subset(aom_CO,SIDO==27); s3.O3<-subset(aom_O3,SIDO==27)
s4.NO2<-subset(aom_NO2,SIDO==28); s4.CO<-subset(aom_CO,SIDO==28); s4.O3<-subset(aom_O3,SIDO==28)
s5.NO2<-subset(aom_NO2,SIDO==29); s5.CO<-subset(aom_CO,SIDO==29); s5.O3<-subset(aom_O3,SIDO==29)
s6.NO2<-subset(aom_NO2,SIDO==30); s6.CO<-subset(aom_CO,SIDO==30); s6.O3<-subset(aom_O3,SIDO==30)
s7.NO2<-subset(aom_NO2,SIDO==31); s7.CO<-subset(aom_CO,SIDO==31); s7.O3<-subset(aom_O3,SIDO==31)


m1=NULL;m2=NULL;m3=NULL;m4=NULL;m5=NULL
gam.sido.m<-function(data){
  d<-data
  
  for(i in 42:48){
    f1<-gam(TOT    ~ d[,i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    f2<-gam(TOT_M  ~ d[,i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    f3<-gam(TOT_F  ~ d[,i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    f4<-gam(AGE0   ~ d[,i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    f5<-gam(AGE13  ~ d[,i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    
    m1[[i]]<-gam.res(f1);m2[[i]]<-gam.res(f2)
    m3[[i]]<-gam.res(f3);m4[[i]]<-gam.res(f4);m5[[i]]<-gam.res(f5)
    print(i)}
  
  m.fit01<-as.data.frame(do.call(rbind,m1));m.fit01$category="Total" ;m.fit01$lag=names(d)[42:48]
  m.fit02<-as.data.frame(do.call(rbind,m2));m.fit02$category="Male"  ;m.fit02$lag=names(d)[42:48]
  m.fit03<-as.data.frame(do.call(rbind,m3));m.fit03$category="Female";m.fit03$lag=names(d)[42:48]
  m.fit04<-as.data.frame(do.call(rbind,m4));m.fit04$category="AGE0"  ;m.fit04$lag=names(d)[42:48]
  m.fit05<-as.data.frame(do.call(rbind,m5));m.fit05$category="AGE013";m.fit05$lag=names(d)[42:48]
  
  m.fit<-rbind(m.fit01,m.fit02,m.fit03,m.fit04,m.fit05)
  
  m.fit$RR=exp(m.fit$Estimate)
  m.fit$RR_lci=with(m.fit,exp(Estimate-1.96*`Std. Error`))
  m.fit$RR_uci=with(m.fit,exp(Estimate+1.96*`Std. Error`))
  m.fit}

m1.fit<-gam.sido.m(s1.PM10);m1.fit$Region="Seoul"
m2.fit<-gam.sido.m(s2.PM10);m2.fit$Region="Busan"
m3.fit<-gam.sido.m(s3.PM10);m3.fit$Region="Daegu"
m4.fit<-gam.sido.m(s4.PM10);m4.fit$Region="Incheon"
m5.fit<-gam.sido.m(s5.PM10);m5.fit$Region="Gwangju"
m6.fit<-gam.sido.m(s6.PM10);m6.fit$Region="Daejeon"
m7.fit<-gam.sido.m(s7.PM10);m7.fit$Region="Ulsan"

gam.fit2<-rbind(m1.fit,m2.fit,m3.fit,m4.fit,m5.fit,m6.fit,m7.fit)

m1.fit<-gam.sido.m(s1.SO2);m1.fit$Region="Seoul"
m2.fit<-gam.sido.m(s2.SO2);m2.fit$Region="Busan"
m3.fit<-gam.sido.m(s3.SO2);m3.fit$Region="Daegu"
m4.fit<-gam.sido.m(s4.SO2);m4.fit$Region="Incheon"
m5.fit<-gam.sido.m(s5.SO2);m5.fit$Region="Gwangju"
m6.fit<-gam.sido.m(s6.SO2);m6.fit$Region="Daejeon"
m7.fit<-gam.sido.m(s7.SO2);m7.fit$Region="Ulsan"

gam.fit3<-rbind(m1.fit,m2.fit,m3.fit,m4.fit,m5.fit,m6.fit,m7.fit)

m1.fit<-gam.sido.m(s1.NO2);m1.fit$Region="Seoul"
m2.fit<-gam.sido.m(s2.NO2);m2.fit$Region="Busan"
m3.fit<-gam.sido.m(s3.NO2);m3.fit$Region="Daegu"
m4.fit<-gam.sido.m(s4.NO2);m4.fit$Region="Incheon"
m5.fit<-gam.sido.m(s5.NO2);m5.fit$Region="Gwangju"
m6.fit<-gam.sido.m(s6.NO2);m6.fit$Region="Daejeon"
m7.fit<-gam.sido.m(s7.NO2);m7.fit$Region="Ulsan"

gam.fit4<-rbind(m1.fit,m2.fit,m3.fit,m4.fit,m5.fit,m6.fit,m7.fit)

m1.fit<-gam.sido.m(s1.CO);m1.fit$Region="Seoul"
m2.fit<-gam.sido.m(s2.CO);m2.fit$Region="Busan"
m3.fit<-gam.sido.m(s3.CO);m3.fit$Region="Daegu"
m4.fit<-gam.sido.m(s4.CO);m4.fit$Region="Incheon"
m5.fit<-gam.sido.m(s5.CO);m5.fit$Region="Gwangju"
m6.fit<-gam.sido.m(s6.CO);m6.fit$Region="Daejeon"
m7.fit<-gam.sido.m(s7.CO);m7.fit$Region="Ulsan"

gam.fit5<-rbind(m1.fit,m2.fit,m3.fit,m4.fit,m5.fit,m6.fit,m7.fit)

m1.fit<-gam.sido.m(s1.O3);m1.fit$Region="Seoul"
m2.fit<-gam.sido.m(s2.O3);m2.fit$Region="Busan"
m3.fit<-gam.sido.m(s3.O3);m3.fit$Region="Daegu"
m4.fit<-gam.sido.m(s4.O3);m4.fit$Region="Incheon"
m5.fit<-gam.sido.m(s5.O3);m5.fit$Region="Gwangju"
m6.fit<-gam.sido.m(s6.O3);m6.fit$Region="Daejeon"
m7.fit<-gam.sido.m(s7.O3);m7.fit$Region="Ulsan"

gam.fit6<-rbind(m1.fit,m2.fit,m3.fit,m4.fit,m5.fit,m6.fit,m7.fit)

gam.fit2$exposure="PM10"
gam.fit3$exposure="SO2"
gam.fit4$exposure="NO2"
gam.fit5$exposure="CO"
gam.fit6$exposure="O3"

gam.mm<-rbind(gam.fit2,
              gam.fit3,
              gam.fit4,
              gam.fit5,
              gam.fit6)

write.csv(gam.mm,file="gam.mm.csv",row.names=F)
##################################################################################################################################
##################################################################################################################################
mm<-read.csv("C:\\Users\\a\\Desktop\\집에서\\20200418\\AP\\gam.mm.csv",header=T)

gam.fit
mm01<-subset(gam.fit,category=="Total" & lag=="lag04")
mm01$exposure="PM2.5"
mm02<-subset(mm,category=="Total" & lag=="lag04" & exposure=="PM10")
mm03<-subset(mm,category=="Total" & lag=="lag04" & exposure=="SO2")
mm04<-subset(mm,category=="Total" & lag=="lag04" & exposure=="NO2")
mm05<-subset(mm,category=="Total" & lag=="lag04" & exposure=="CO")
mm06<-subset(mm,category=="Total" & lag=="lag04" & exposure=="O3")


mm01$Std..Error
uni01 <- with(mm01,rma(yi=Estimate, sei=Std..Error, slab=Region, measure="RR",digits=5,method="REML"))
uni02 <- with(mm02,rma(yi=Estimate, sei=Std..Error, slab=Region, measure="RR",digits=5,method="REML"))
uni03 <- with(mm03,rma(yi=Estimate, sei=Std..Error, slab=Region, measure="RR",digits=5,method="REML"))
uni04 <- with(mm04,rma(yi=Estimate, sei=Std..Error, slab=Region, measure="RR",digits=5,method="REML"))
uni05 <- with(mm05,rma(yi=Estimate, sei=Std..Error, slab=Region, measure="RR",digits=5,method="REML"))
uni06 <- with(mm06,rma(yi=Estimate, sei=Std..Error, slab=Region, measure="RR",digits=5,method="REML"))

mm01$Region=paste0(mm01$Region)        ;mm01$Region=factor(mm01$Region,levels=unique(mm01$Region))
mm02$Region=paste0(mm02$Region," ")    ;mm02$Region=factor(mm02$Region,levels=unique(mm02$Region))
mm03$Region=paste0(mm03$Region,"  ")   ;mm03$Region=factor(mm03$Region,levels=unique(mm03$Region))
mm04$Region=paste0(mm04$Region,"   ")  ;mm04$Region=factor(mm04$Region,levels=unique(mm04$Region))
mm05$Region=paste0(mm05$Region,"    ") ;mm05$Region=factor(mm05$Region,levels=unique(mm05$Region))
mm06$Region=paste0(mm06$Region,"     ");mm06$Region=factor(mm06$Region,levels=unique(mm06$Region))

m<-rbind(mm01%>% select(Estimate:category,RR:Region),
         mm02%>% select(Estimate:category,RR:Region),
         mm03%>% select(Estimate:category,RR:Region),
         mm04%>% select(Estimate:category,RR:Region),
         mm05%>% select(Estimate:category,RR:Region),
         mm06%>% select(Estimate:category,RR:Region))

RM.res<-rma(yi=m$Estimate,sei=m$Std..Error,data=m,slab=m$Region)

tiff(filename="C:\\Users\\a\\Desktop\\집에서\\20200418\\revision_figure\\Fig.SS.tiff",width=3200,height=4000,res=300)
par(mar=c(4,4,1,2));
forest(RM.res, xlim=c(0.8, 1.22), ylim=c(20, 95), steps=4, rows=c(89:83,77:71,65:59,53:47,41:35,29:23),refline=1,
       order=c(1:nrow(m)),transf = exp,bg=4,col=2,digits=4,cex=0.8,xlab="", mlab="")

addpoly(uni01,transf=exp,cex=1,row=56+24.5,col="red",digits=3,mlab=expression(paste("RE Model for ",PM[2.5])))
addpoly(uni02,transf=exp,cex=1,row=44+24.5,col="red",digits=3,mlab=expression(paste("RE Model for ",PM[10])))
addpoly(uni03,transf=exp,cex=1,row=32+24.5,col="red",digits=3,mlab=expression(paste("RE Model for ",SO[2])))
addpoly(uni04,transf=exp,cex=1,row=20+24.5,col="red",digits=3,mlab=expression(paste("RE Model for ",NO[2])))
addpoly(uni05,transf=exp,cex=1,row=08+24.5,col="red",digits=3,mlab=expression(paste("RE Model for ",CO)))
addpoly(uni06,transf=exp,cex=1,row=08+12.5,col="red",digits=3,mlab=expression(paste("RE Model for ",O3[3])))

text(0.8,90.3, pos=4, cex=1.3, expression(paste(PM[2.5])),col="blue",font=5)
text(0.8,78.3, pos=4, cex=1.3, expression(paste(PM[10])),col="blue",font=4)
text(0.8,66.3, pos=4, cex=1.3, expression(paste(SO[2])),col="blue",font=4)
text(0.8,54.3, pos=4, cex=1.3, expression(paste(NO[2])),col="blue",font=4)
text(0.8,42.3, pos=4, cex=1.3, expression(paste(CO)),col="blue",font=4)
text(0.8,30.3, pos=4, cex=1.3, expression(paste(O[3])),col="blue",font=4)
text(0.8,94  , pos=4, cex=1.3, paste("Regions"),font=4)
text(1.12,94 , pos=4, cex=1.3, paste("Relative Risk [95% CI]"),font=4)
dev.off()
##################################################################################################################################
##################################################################################################################################
#CMAQ VS MONITORING FOR PM2.5 (15~16 yr)
PM25_C1516<-subset(aom_w4,DATE>="2015-01-01")
PM25_M1516<-subset(aom_PM25,DATE>="2015-01-01")

c_s1<-subset(PM25_C1516,SIDO==11);m_s1<-subset(PM25_M1516,SIDO==11);
c_s2<-subset(PM25_C1516,SIDO==26);m_s2<-subset(PM25_M1516,SIDO==26);
c_s3<-subset(PM25_C1516,SIDO==27);m_s3<-subset(PM25_M1516,SIDO==27);
c_s4<-subset(PM25_C1516,SIDO==28);m_s4<-subset(PM25_M1516,SIDO==28);
c_s5<-subset(PM25_C1516,SIDO==29);m_s5<-subset(PM25_M1516,SIDO==29);
c_s6<-subset(PM25_C1516,SIDO==30);m_s6<-subset(PM25_M1516,SIDO==30);
c_s7<-subset(PM25_C1516,SIDO==31);m_s7<-subset(PM25_M1516,SIDO==31);

gam1516_c<-function(data){
  d<-data
  for(i in 32:38){
    f1<-gam(TOT    ~ d[,i]+s(as.numeric(DATE),k=2*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    
    m1[[i]]<-gam.res(f1);
    print(i)}
  m.fit<-as.data.frame(do.call(rbind,m1))
  m.fit$category="Total"
  m.fit$RR=exp(m.fit$Estimate)
  m.fit$RR_lci=with(m.fit,exp(Estimate-1.96*`Std. Error`))
  m.fit$RR_uci=with(m.fit,exp(Estimate+1.96*`Std. Error`))
  m.fit}

gam1516_m<-function(data){
  d<-data
  for(i in 42:48){
    f2<-gam(TOT    ~ d[,i]+s(as.numeric(DATE),k=2*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    m2[[i]]<-gam.res(f2);
    print(i)}
  m.fit<-as.data.frame(do.call(rbind,m2))
  m.fit$category="Total"
  m.fit$RR=exp(m.fit$Estimate)
  m.fit$RR_lci=with(m.fit,exp(Estimate-1.96*`Std. Error`))
  m.fit$RR_uci=with(m.fit,exp(Estimate+1.96*`Std. Error`))
  m.fit}


c_r1<-gam1516_c(c_s1); c_r1$Region="Seoul"
c_r2<-gam1516_c(c_s2); c_r2$Region="Busan"
c_r3<-gam1516_c(c_s3); c_r3$Region="Daegu"
c_r4<-gam1516_c(c_s4); c_r4$Region="Incheon"
c_r5<-gam1516_c(c_s5); c_r5$Region="Gwangju"
c_r6<-gam1516_c(c_s6); c_r6$Region="Daejeon"
c_r7<-gam1516_c(c_s7); c_r7$Region="Ulsan"

m_r1<-gam1516_c(m_s1); m_r1$Region="Seoul"
m_r2<-gam1516_c(m_s2); m_r2$Region="Busan"
m_r3<-gam1516_c(m_s3); m_r3$Region="Daegu"
m_r4<-gam1516_c(m_s4); m_r4$Region="Incheon"
m_r5<-gam1516_c(m_s5); m_r5$Region="Gwangju"
m_r6<-gam1516_c(m_s6); m_r6$Region="Daejeon"
m_r7<-gam1516_c(m_s7); m_r7$Region="Ulsan"

c_r<-rbind(c_r1,c_r2,c_r3,c_r4,c_r5,c_r6,c_r7)
m_r<-rbind(m_r1,m_r2,m_r3,m_r4,m_r5,m_r6,m_r7)

c_r$gubun="CMAQ"
m_r$gubun="Monitoring"

cm<-rbind(c_r,m_r)
write.csv(cm,file="cm.csv",row.names=F)

cor_cm1<-cbind(c_s1$pm,m_s1$lag0) %>% cor(use="complete.obs")
cor_cm2<-cbind(c_s2$pm,m_s2$lag0) %>% cor(use="complete.obs")
cor_cm3<-cbind(c_s3$pm,m_s3$lag0) %>% cor(use="complete.obs")
cor_cm4<-cbind(c_s4$pm,m_s4$lag0) %>% cor(use="complete.obs")
cor_cm5<-cbind(c_s5$pm,m_s5$lag0) %>% cor(use="complete.obs")
cor_cm6<-cbind(c_s6$pm,m_s6$lag0) %>% cor(use="complete.obs")
cor_cm7<-cbind(c_s7$pm,m_s7$lag0) %>% cor(use="complete.obs")

rbind(cor_cm1[1,2],cor_cm2[1,2],cor_cm3[1,2],cor_cm4[1,2],cor_cm5[1,2],cor_cm6[1,2],cor_cm7[1,2])

cs1<-c_s1 %>% select(DATE,pm);
cs2<-c_s2 %>% select(DATE,pm);
cs3<-c_s3 %>% select(DATE,pm);
cs4<-c_s4 %>% select(DATE,pm);
cs5<-c_s5 %>% select(DATE,pm);
cs6<-c_s6 %>% select(DATE,pm);
cs7<-c_s7 %>% select(DATE,pm);

ms1<-m_s1 %>% select(DATE,lag0);ms1$gubun="Monitoring";
ms2<-m_s2 %>% select(DATE,lag0);ms2$gubun="Monitoring";
ms3<-m_s3 %>% select(DATE,lag0);ms3$gubun="Monitoring";
ms4<-m_s4 %>% select(DATE,lag0);ms4$gubun="Monitoring";
ms5<-m_s5 %>% select(DATE,lag0);ms5$gubun="Monitoring";
ms6<-m_s6 %>% select(DATE,lag0);ms6$gubun="Monitoring";
ms7<-m_s7 %>% select(DATE,lag0);ms7$gubun="Monitoring";

cm1<-cbind(cs1,pm2=ms1$lag0);cm1[,2:3]=cm1[,2:3]*10
cm2<-cbind(cs2,pm2=ms2$lag0);cm2[,2:3]=cm2[,2:3]*10
cm3<-cbind(cs3,pm2=ms3$lag0);cm3[,2:3]=cm3[,2:3]*10
cm4<-cbind(cs4,pm2=ms4$lag0);cm4[,2:3]=cm4[,2:3]*10
cm5<-cbind(cs5,pm2=ms5$lag0);cm5[,2:3]=cm5[,2:3]*10
cm6<-cbind(cs6,pm2=ms6$lag0);cm6[,2:3]=cm6[,2:3]*10
cm7<-cbind(cs7,pm2=ms7$lag0);cm7[,2:3]=cm7[,2:3]*10

g1<-ggplot(cm1,aes(pm,pm2))+geom_point()+theme_gray(base_size=15)+geom_abline(col="red",size=1)+
  ylim(0,80)+geom_text(label="Region :Seoul \n Correlation: 0.989",x=20,y=70,size=5)+
  labs(x=expression(paste("CMAQ ",PM[2.5]," (",mu,g/m^3,")")),y=expression(paste("Monitoring  ",PM[2.5]," (",mu,g/m^3,")")))
g2<-ggplot(cm2,aes(pm,pm2))+geom_point()+theme_gray(base_size=15)+geom_abline(col="red",size=1)+
  ylim(0,80)+geom_text(label="Region :Busan \n Correlation: 0.986",x=20,y=70,size=5)+
  labs(x=expression(paste("CMAQ ",PM[2.5]," (",mu,g/m^3,")")),y=expression(paste("Monitoring  ",PM[2.5]," (",mu,g/m^3,")")))
g3<-ggplot(cm3,aes(pm,pm2))+geom_point()+theme_gray(base_size=15)+geom_abline(col="red",size=1)+
  ylim(0,80)+geom_text(label="Region :Daegu \n Correlation: 0.993",x=20,y=70,size=5)+
  labs(x=expression(paste("CMAQ ",PM[2.5]," (",mu,g/m^3,")")),y=expression(paste("Monitoring  ",PM[2.5]," (",mu,g/m^3,")")))
g4<-ggplot(cm4,aes(pm,pm2))+geom_point()+theme_gray(base_size=15)+geom_abline(col="red",size=1)+
  ylim(0,80)+geom_text(label="Region :Incheon \n Correlation: 0.982",x=20,y=70,size=5)+
  labs(x=expression(paste("CMAQ ",PM[2.5]," (",mu,g/m^3,")")),y=expression(paste("Monitoring  ",PM[2.5]," (",mu,g/m^3,")")))
g5<-ggplot(cm5,aes(pm,pm2))+geom_point()+theme_gray(base_size=15)+geom_abline(col="red",size=1)+
  ylim(0,80)+geom_text(label="Region :Gwangju \n Correlation: 0.984",x=20,y=70,size=5)+
  labs(x=expression(paste("CMAQ ",PM[2.5]," (",mu,g/m^3,")")),y=expression(paste("Monitoring  ",PM[2.5]," (",mu,g/m^3,")")))
g6<-ggplot(cm6,aes(pm,pm2))+geom_point()+theme_gray(base_size=15)+geom_abline(col="red",size=1)+
  ylim(0,80)+geom_text(label="Region :Daejeon \n Correlation: 0.969",x=20,y=70,size=5)+
  labs(x=expression(paste("CMAQ ",PM[2.5]," (",mu,g/m^3,")")),y=expression(paste("Monitoring  ",PM[2.5]," (",mu,g/m^3,")")))
g7<-ggplot(cm7,aes(pm,pm2))+geom_point()+theme_gray(base_size=15)+geom_abline(col="red",size=1)+
  ylim(0,80)+geom_text(label="Region :Ulsan \n Correlation: 0.993",x=20,y=70,size=5)+
  labs(x=expression(paste("CMAQ ",PM[2.5]," (",mu,g/m^3,")")),y=expression(paste("Monitoring  ",PM[2.5]," (",mu,g/m^3,")")))

x11();grid.arrange(g1,g2,g3,g4,g5,g6,g7,ncol=3)

tiff(filename="C:\\Users\\a\\Desktop\\집에서\\20200418\\revision_figure\\Fig.S9.tiff",width=4000,height=4000,res=300)
grid.arrange(g1,g2,g3,g4,g5,g6,g7,ncol=3)
dev.off()
##################################################################################################################################
##################################################################################################################################
#Correlation
aom.m2 <-aom_w4 %>% arrange(SIDO,DATE)

cor(cbind(aom.m2$lag04,aom_PM10$lag04,aom_SO2$lag04,aom_NO2$lag04,aom_CO$lag04,aom_O3$lag04),use="complete.obs")
cor(cbind(aom.m2$pm,aom_PM10$lag0,aom_SO2$lag0,aom_NO2$lag0,aom_CO$lag0,aom_O3$lag0),use="complete.obs") %>% View()

cor.test(aom.m2$pm,aom_PM10$lag0,use="complete.obs")
cor.test(aom.m2$pm,aom_SO2$lag0,use="complete.obs")
cor.test(aom.m2$pm,aom_NO2$lag0,use="complete.obs")
cor.test(aom.m2$pm,aom_CO$lag0,use="complete.obs")
cor.test(aom.m2$pm,aom_O3$lag0,use="complete.obs") #*

cor.test(aom_PM10$lag0,aom_SO2$lag0,use="complete.obs")
cor.test(aom_PM10$lag0,aom_NO2$lag0,use="complete.obs")
cor.test(aom_PM10$lag0,aom_CO$lag0,use="complete.obs")
cor.test(aom_PM10$lag0,aom_O3$lag0,use="complete.obs")

cor.test(aom_SO2$lag0,aom_NO2$lag0,use="complete.obs")
cor.test(aom_SO2$lag0,aom_CO$lag0,use="complete.obs")
cor.test(aom_SO2$lag0,aom_O3$lag0,use="complete.obs")

cor.test(aom_NO2$lag0,aom_CO$lag0,use="complete.obs")
cor.test(aom_NO2$lag0,aom_O3$lag0,use="complete.obs")

cor.test(aom_CO$lag0,aom_O3$lag0,use="complete.obs")

##################################################################################################################################
##################################################################################################################################
#Two-pollutant model
t=NULL
two.p<-function(data1,data2){
  
  d1<-data1
  d2<-data2 %>% select(lag01:lag07)
  
  
  d<-cbind(d1,d2)
  
  for(i in 32:38){
    f1<-gam(TOT    ~ d[,i]+d[,16+i]+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+month,family="quasipoisson",data=d)
    t[[i]]<-gam.res(f1);
    print(i)}
  
  m.fit<-as.data.frame(do.call(rbind,t))
  m.fit$category="Total"
  m.fit$RR=exp(m.fit$Estimate)
  m.fit$RR_lci=with(m.fit,exp(Estimate-1.96*`Std. Error`))
  m.fit$RR_uci=with(m.fit,exp(Estimate+1.96*`Std. Error`))
  m.fit}

two1.s1<-two.p(s1.w4,s1.SO2); two1.s1$Region="Seoul"
two1.s2<-two.p(s2.w4,s2.SO2); two1.s2$Region="Busan"
two1.s3<-two.p(s3.w4,s3.SO2); two1.s3$Region="Daegu"
two1.s4<-two.p(s4.w4,s4.SO2); two1.s4$Region="Incheon"
two1.s5<-two.p(s5.w4,s5.SO2); two1.s5$Region="Gwangju"
two1.s6<-two.p(s6.w4,s6.SO2); two1.s6$Region="Daejeon"
two1.s7<-two.p(s7.w4,s7.SO2); two1.s7$Region="Ulsan"

two1<-rbind(two1.s1,two1.s2,two1.s3,two1.s4,two1.s5,two1.s6,two1.s7)

two2.s1<-two.p(s1.w4,s1.NO2); two2.s1$Region="Seoul"
two2.s2<-two.p(s2.w4,s2.NO2); two2.s2$Region="Busan"
two2.s3<-two.p(s3.w4,s3.NO2); two2.s3$Region="Daegu"
two2.s4<-two.p(s4.w4,s4.NO2); two2.s4$Region="Incheon"
two2.s5<-two.p(s5.w4,s5.NO2); two2.s5$Region="Gwangju"
two2.s6<-two.p(s6.w4,s6.NO2); two2.s6$Region="Daejeon"
two2.s7<-two.p(s7.w4,s7.NO2); two2.s7$Region="Ulsan"

two2<-rbind(two2.s1,two2.s2,two2.s3,two2.s4,two2.s5,two2.s6,two2.s7)

two3.s1<-two.p(s1.w4,s1.CO); two3.s1$Region="Seoul"
two3.s2<-two.p(s2.w4,s2.CO); two3.s2$Region="Busan"
two3.s3<-two.p(s3.w4,s3.CO); two3.s3$Region="Daegu"
two3.s4<-two.p(s4.w4,s4.CO); two3.s4$Region="Incheon"
two3.s5<-two.p(s5.w4,s5.CO); two3.s5$Region="Gwangju"
two3.s6<-two.p(s6.w4,s6.CO); two3.s6$Region="Daejeon"
two3.s7<-two.p(s7.w4,s7.CO); two3.s7$Region="Ulsan"

two3<-rbind(two3.s1,two3.s2,two3.s3,two3.s4,two3.s5,two3.s6,two3.s7)

two4.s1<-two.p(s1.w4,s1.O3); two4.s1$Region="Seoul"
two4.s2<-two.p(s2.w4,s2.O3); two4.s2$Region="Busan"
two4.s3<-two.p(s3.w4,s3.O3); two4.s3$Region="Daegu"
two4.s4<-two.p(s4.w4,s4.O3); two4.s4$Region="Incheon"
two4.s5<-two.p(s5.w4,s5.O3); two4.s5$Region="Gwangju"
two4.s6<-two.p(s6.w4,s6.O3); two4.s6$Region="Daejeon"
two4.s7<-two.p(s7.w4,s7.O3); two4.s7$Region="Ulsan"

two4<-rbind(two4.s1,two4.s2,two4.s3,two4.s4,two4.s5,two4.s6,two4.s7)

two1$adjusted="SO2";two1$lag=1:7
two2$adjusted="NO2";two2$lag=1:7
two3$adjusted="CO" ;two3$lag=1:7
two4$adjusted="O3" ;two4$lag=1:7

two<-rbind(two1,two2,two3,two4)

write.csv(two,file="two.csv",row.names=F,na="")
##################################################################################################################################
##################################################################################################################################
#Two-pollutant model meta analysis
mm  <-subset(gam.fit,category=="Total" & lag=="lag04")
mt01<-subset(two1, lag==4);mt02<-subset(two2, lag==4)
mt03<-subset(two3, lag==4);mt04<-subset(two4, lag==4)

uni00 <- with(mm  ,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni01 <- with(mt01,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni02 <- with(mt02,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni03 <- with(mt03,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))
uni04 <- with(mt04,rma(yi=Estimate, sei=`Std. Error`, slab=Region, measure="RR",digits=5,method="REML"))

mm$Region=paste0(mm$Region)            ;mm$Region=factor(mm$Region,levels=unique(mm$Region))
mt01$Region=paste0(mt01$Region," ")    ;mt01$Region=factor(mt01$Region,levels=unique(mt01$Region))
mt02$Region=paste0(mt02$Region,"  ")   ;mt02$Region=factor(mt02$Region,levels=unique(mt02$Region))
mt03$Region=paste0(mt03$Region,"   ")  ;mt03$Region=factor(mt03$Region,levels=unique(mt03$Region))
mt04$Region=paste0(mt04$Region,"    ") ;mt04$Region=factor(mt04$Region,levels=unique(mt04$Region))

m<-rbind(mm  %>% select(Estimate:category,RR:Region),
         mt01%>% select(Estimate:category,RR:Region),
         mt02%>% select(Estimate:category,RR:Region),
         mt03%>% select(Estimate:category,RR:Region),
         mt04%>% select(Estimate:category,RR:Region))

RM.res<-rma(yi=m$Estimate,sei=m$`Std. Error`,data=m,slab=m$Region)

x11();par(mar=c(4,4,1,2));
forest(RM.res, xlim=c(0.8, 1.2), ylim=c(9, 69), steps=5, rows=c(65:59,53:47,41:35,29:23,17:11),refline=1,
       order=c(1:nrow(m)),transf = exp,bg=4,col=2,digits=3,cex=1.1,xlab="", mlab="")
addpoly(uni00,transf=exp,cex=1.25,row=57,col="red",digits=3,mlab="RE Model for PM2.5")
addpoly(uni01,transf=exp,cex=1.25,row=45,col="red",digits=3,mlab="RE Model for PM2.5+SO2")
addpoly(uni02,transf=exp,cex=1.25,row=33,col="red",digits=3,mlab="RE Model for PM2.5+NO2")
addpoly(uni03,transf=exp,cex=1.25,row=21,col="red",digits=3,mlab="RE Model for PM2.5+CO")
addpoly(uni04,transf=exp,cex=1.25,row=9 ,col="red",digits=3,mlab="RE Model for PM2.5+O3")

text(0.8,66.3, pos=4, cex=1.3, paste("Single-pollutant model"),col="blue",font=4)
text(0.8,54.3, pos=4, cex=1.3, paste("Two-pollutant model"),col="blue",font=4)
text(0.8,42.3, pos=4, cex=1.3, paste("Two-pollutant model"),col="blue",font=4)
text(0.8,30.3, pos=4, cex=1.3, paste("Two-pollutant model"),col="blue",font=4)
text(0.8,18.3, pos=4, cex=1.3, paste("Two-pollutant model"),col="blue",font=4)
text(0.8,68  , pos=4, cex=1.3, paste("Regions"),font=4)
text(1.115,68 , pos=4, cex=1.3, paste("Relative Risk [95% CI]"),font=4)

tiff(filename="C:\\Users\\a\\Desktop\\집에서\\20200418\\revision_figure\\Fig.5.tiff",width=3600,height=4000,res=300)
par(mar=c(4,4,1,2));
forest(RM.res, xlim=c(0.8, 1.2), ylim=c(9, 69), steps=5, rows=c(65:59,53:47,41:35,29:23,17:11),refline=1,
       order=c(1:nrow(m)),transf = exp,bg=4,col=2,digits=3,cex=1.1,xlab="", mlab="")

addpoly(uni00,transf=exp,cex=1.25,row=57,col="red",digits=3,mlab=expression(paste("RE Model for Unadjusted ",PM[2.5])))
addpoly(uni01,transf=exp,cex=1.25,row=45,col="red",digits=3,mlab=expression(paste("RE Model for ",PM[2.5]," adjusted for ",SO[2])))
addpoly(uni02,transf=exp,cex=1.25,row=33,col="red",digits=3,mlab=expression(paste("RE Model for ",PM[2.5]," adjusted for ",NO[2])))
addpoly(uni03,transf=exp,cex=1.25,row=21,col="red",digits=3,mlab=expression(paste("RE Model for ",PM[2.5]," adjusted for ",CO)))
addpoly(uni04,transf=exp,cex=1.25,row=9 ,col="red",digits=3,mlab=expression(paste("RE Model for ",PM[2.5]," adjusted for ",O[3])))

text(0.8,66.3, pos=4, cex=1.3, paste("Single-pollutant model"),col="blue",font=4)
text(0.8,54.3, pos=4, cex=1.3, paste("Two-pollutant model"),col="blue",font=4)
text(0.8,42.3, pos=4, cex=1.3, paste("Two-pollutant model"),col="blue",font=4)
text(0.8,30.3, pos=4, cex=1.3, paste("Two-pollutant model"),col="blue",font=4)
text(0.8,18.3, pos=4, cex=1.3, paste("Two-pollutant model"),col="blue",font=4)
text(0.8,68  , pos=4, cex=1.3, paste("Regions"),font=4)
text(1.115,68 , pos=4, cex=1.3, paste("Relative Risk [95% CI]"),font=4)
dev.off()
