#------------------------------------------------------------------------------#
#작업 디렉토리 (국건영 자료 보관 위치)
setwd("D:\\EUMC\\데이터관리\\국민건강영양조사\\검진")
#------------------------------------------------------------------------------#
#Library 설정
library(dplyr)     ;library(lubridate)
library(ggplot2)   ;library(readxl)
library(gridExtra) ;library(readxl)
library(survey)    ;library(lmtest)
#------------------------------------------------------------------------------#
#국건영 자료 불러오기 
hn16<-read.csv("HN16_ALL.csv",header=T);nrow(hn16)
hn17<-read.csv("HN17_ALL.csv",header=T);nrow(hn17)
hn18<-read.csv("HN18_ALL.csv",header=T);nrow(hn18)

#0 없음, 1 있음, 8 비해당, 9 모름, 무응답
hn16$DI1_dg #고혈압
hn16$DI2_dg #이상지질혈증 
hn16$DI3_dg #뇌졸중
hn16$DI4_dg #심근경색증
hn16$DI5_dg #협심증
hn16$DE1_dg #당뇨

addmargins(table(hn16$DI1_dg))
addmargins(table(hn16$DI2_dg))
addmargins(table(hn16$DI3_dg))
addmargins(table(hn16$DI4_dg))
addmargins(table(hn16$DI5_dg))
addmargins(table(hn16$DE1_dg))
addmargins(table(hn16$DI1_dg))

#현재 유병 
addmargins(table(hn16$DI1_pr))

#음주 
hn16$BD1 #평생 음주경험 (1: 마셔본적없음, 2:있음, 8:비해당,9 모름)
hn16$BD1_11 #1년간 음주 빈도 1 : 최근 1년간 전혀, 2: 월 1회, 3: 월 1회정도, 4 월2-4회, 5: 주2-3회 정도
#                6 : 주 4회 이상 8: 비해당, 9: 모름, 무응답

#흡연 1:5갑(100개비)미만, 2: 5갑(100개비) 이상,3 피운적 없음 8, 비해당, 
hn16.r<-hn16 %>% select(ID,year,region,sex,age,HE_sbp,HE_dbp,incm,ho_incm,kstrata,psu,wt_itvex,
                        edu,occp,HE_ht,HE_wt,HE_BMI,HE_chol,HE_TG,HE_glu,HE_BUN,HE_crea,
                        DI1_dg,DI2_dg,DI3_dg,DI4_dg,DI5_dg,DE1_dg,
                        DI1_pr,DI2_pr,DI3_pr,DI4_pr,DI5_pr,DE1_pr,BD1_11,BS3_1)
hn17.r<-hn17 %>% select(ID,year,region,sex,age,HE_sbp,HE_dbp,incm,ho_incm,kstrata,psu,wt_itvex,
                        edu,occp,HE_ht,HE_wt,HE_BMI,HE_chol,HE_TG,HE_glu,HE_BUN,HE_crea,
                        DI1_dg,DI2_dg,DI3_dg,DI4_dg,DI5_dg,DE1_dg,
                        DI1_pr,DI2_pr,DI3_pr,DI4_pr,DI5_pr,DE1_pr,BD1_11,BS3_1)
hn18.r<-hn18 %>% select(ID,year,region,sex,age,HE_sbp,HE_dbp,incm,ho_incm,kstrata,psu,wt_itvex,
                        edu,occp,HE_ht,HE_wt,HE_BMI,HE_chol,HE_TG,HE_glu,HE_BUN,HE_crea,
                        DI1_dg,DI2_dg,DI3_dg,DI4_dg,DI5_dg,DE1_dg,
                        DI1_pr,DI2_pr,DI3_pr,DI4_pr,DI5_pr,DE1_pr,BD1_11,BS3_1)

#자료 병합 
hn<-as.data.frame(rbind(hn16.r,hn17.r,hn18.r))
#복합 표본 설계 분석을 위한 통합 가중치
hn$T_wt_itvex=hn$wt_itvex/3
#------------------------------------------------------------------------------#
#대기오염 모니터링 자료 
pm15<-read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2015\\2015_모니터링농도.xlsx",sheet=3)
pm16<-read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2016\\2016_모니터링농도.xlsx",sheet=3)
pm17<-read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2017\\2017_모니터링농도.xlsx",sheet=3)
pm18<-read_excel("D:\\EUMC\\데이터관리\\Mornitoring_data\\Air_Korea_확정데이터\\2018\\2018_모니터링농도.xlsx",sheet=3)

pm<-rbind(pm15,pm16,pm17,pm18)
pm$yymmdd=ymd(pm$yymmdd)
pm$year=year(pm$yymmdd)

#연평균 자료로 산출
agg_ap1<-aggregate(pm$PM25,list(pm$year,pm$sido),mean,na.rm=T)
agg_ap2<-aggregate(pm$PM10,list(pm$year,pm$sido),mean,na.rm=T)
agg_ap3<-aggregate(pm$SO2,list(pm$year,pm$sido),mean,na.rm=T)
agg_ap4<-aggregate(pm$NO2,list(pm$year,pm$sido),mean,na.rm=T)
agg_ap5<-aggregate(pm$CO ,list(pm$year,pm$sido),mean,na.rm=T)
agg_ap6<-aggregate(pm$O3 ,list(pm$year,pm$sido),mean,na.rm=T)

names(agg_ap1)=c("year","sido","PM25")
names(agg_ap2)=c("year","sido","PM10")
names(agg_ap3)=c("year","sido","SO2")
names(agg_ap4)=c("year","sido","NO2")
names(agg_ap5)=c("year","sido","CO")
names(agg_ap6)=c("year","sido","O3")

agg_ap<-cbind(agg_ap1,
              PM10=agg_ap2$PM10,
              SO2 =agg_ap3$SO2,
              NO2 =agg_ap4$NO2,
              CO  =agg_ap5$CO,
              O3  =agg_ap6$O3)

head(agg_ap)

#지역별 노출 연계하기위해 코드변경 
hn$region=with(hn,ifelse(region==1 ,"서울",region))
hn$region=with(hn,ifelse(region==2 ,"부산",region))
hn$region=with(hn,ifelse(region==3 ,"대구",region))
hn$region=with(hn,ifelse(region==4 ,"인천",region))
hn$region=with(hn,ifelse(region==5 ,"광주",region))
hn$region=with(hn,ifelse(region==6 ,"대전",region))
hn$region=with(hn,ifelse(region==7 ,"울산",region))
hn$region=with(hn,ifelse(region==8 ,"세종",region))
hn$region=with(hn,ifelse(region==9 ,"경기",region))
hn$region=with(hn,ifelse(region==10,"강원",region))
hn$region=with(hn,ifelse(region==11,"충북",region))
hn$region=with(hn,ifelse(region==12,"충남",region))
hn$region=with(hn,ifelse(region==13,"전북",region))
hn$region=with(hn,ifelse(region==14,"전남",region))
hn$region=with(hn,ifelse(region==15,"경북",region))
hn$region=with(hn,ifelse(region==16,"경남",region))
hn$region=with(hn,ifelse(region==17,"제주",region)) 

hn$key     =with(hn,paste0(year,"-",region))

agg_ap$key =with(agg_ap,paste0(year,"-",sido))
agg_ap     =agg_ap %>% select(-year)
agg_ap$sido=factor(agg_ap$sido)

View(agg_ap)

sido<-factor(unique(agg_pm$sido))

#노출 농도 Lag 별 산출 
agg_list=NULL
agg.f<-function(x){
  
  s<-subset(agg_ap,sido==x)
  s$lag1_PM25=lag(s$PM25,1);s$lag2_PM25=lag(s$PM25,2);s$lag3_PM25=lag(s$PM25,3)
  s$lag1_PM10=lag(s$PM10,1);s$lag2_PM10=lag(s$PM10,2);s$lag3_PM10=lag(s$PM10,3)
  s$lag1_SO2 =lag(s$SO2 ,1);s$lag2_SO2=lag(s$SO2,2);s$lag3_SO2=lag(s$SO2,3)
  s$lag1_NO2 =lag(s$NO2 ,1);s$lag2_NO2=lag(s$NO2,2);s$lag3_NO2=lag(s$NO2,3)
  s$lag1_CO  =lag(s$CO  ,1);s$lag2_CO =lag(s$CO ,2);s$lag3_CO =lag(s$CO ,3)
  s$lag1_O3  =lag(s$O3  ,1);s$lag2_O3 =lag(s$O3 ,2);s$lag3_O3 =lag(s$O3 ,3)
  
  s$lag01_PM25=apply(s %>% select(PM25,lag1_PM25)          ,1,mean,na.rm=T)
  s$lag02_PM25=apply(s %>% select(PM25,lag1_PM25:lag2_PM25),1,mean,na.rm=T)
  s$lag03_PM25=apply(s %>% select(PM25,lag1_PM25:lag3_PM25),1,mean,na.rm=T)
  s$lag01_PM10=apply(s %>% select(PM10,lag1_PM10)          ,1,mean,na.rm=T)
  s$lag02_PM10=apply(s %>% select(PM10,lag1_PM10:lag2_PM10),1,mean,na.rm=T)
  s$lag03_PM10=apply(s %>% select(PM10,lag1_PM10:lag3_PM10),1,mean,na.rm=T)
  s$lag01_SO2 =apply(s %>% select(SO2 ,lag1_SO2)           ,1,mean,na.rm=T)
  s$lag02_SO2 =apply(s %>% select(SO2 ,lag1_SO2:lag2_SO2)  ,1,mean,na.rm=T)
  s$lag03_SO2 =apply(s %>% select(SO2 ,lag1_SO2:lag3_SO2)  ,1,mean,na.rm=T)
  s$lag01_NO2 =apply(s %>% select(NO2 ,lag1_NO2)           ,1,mean,na.rm=T)
  s$lag02_NO2 =apply(s %>% select(NO2 ,lag1_NO2:lag2_NO2)  ,1,mean,na.rm=T)
  s$lag03_NO2 =apply(s %>% select(NO2 ,lag1_NO2:lag3_NO2)  ,1,mean,na.rm=T)
  s$lag01_CO  =apply(s %>% select(CO  ,lag1_CO)            ,1,mean,na.rm=T)
  s$lag02_CO  =apply(s %>% select(CO  ,lag1_CO:lag2_CO)    ,1,mean,na.rm=T)
  s$lag03_CO  =apply(s %>% select(CO  ,lag1_CO:lag3_CO)    ,1,mean,na.rm=T)
  s$lag01_O3  =apply(s %>% select(O3  ,lag1_O3)            ,1,mean,na.rm=T)
  s$lag02_O3  =apply(s %>% select(O3  ,lag1_O3:lag2_O3)    ,1,mean,na.rm=T)
  s$lag03_O3  =apply(s %>% select(O3  ,lag1_O3:lag3_O3)    ,1,mean,na.rm=T)
  
  s}

z<-subset(agg_ap,sido=="세종")
z$lag1_PM25=lag(z$PM25,1);z$lag2_PM25=lag(z$PM25,2) ;z$lag3_PM25=lag(z$PM25,3)
z$lag1_PM10=lag(z$PM10,1);z$lag2_PM10=lag(z$PM10,2) ;z$lag3_PM10=lag(z$PM10,3)
z$lag1_SO2 =lag(z$SO2 ,1);z$lag2_SO2 =lag(z$SO2,2)  ;z$lag3_SO2 =lag(z$SO2,3)
z$lag1_NO2 =lag(z$NO2 ,1);z$lag2_NO2 =lag(z$NO2,2)  ;z$lag3_NO2 =lag(z$NO2,3)
z$lag1_CO  =lag(z$CO  ,1);z$lag2_CO  =lag(z$CO ,2)  ;z$lag3_CO  =lag(z$CO ,3)
z$lag1_O3  =lag(z$O3  ,1);z$lag2_O3  =lag(z$O3 ,2)  ;z$lag3_O3  =lag(z$O3 ,3)

z$lag01_PM25=apply(z %>% select(PM25,lag1_PM25)          ,1,mean,na.rm=T)
z$lag02_PM25=apply(z %>% select(PM25,lag1_PM25:lag2_PM25),1,mean,na.rm=T)
z$lag03_PM25=apply(z %>% select(PM25,lag1_PM25:lag3_PM25),1,mean,na.rm=T)
z$lag01_PM10=apply(z %>% select(PM10,lag1_PM10)          ,1,mean,na.rm=T)
z$lag02_PM10=apply(z %>% select(PM10,lag1_PM10:lag2_PM10),1,mean,na.rm=T)
z$lag03_PM10=apply(z %>% select(PM10,lag1_PM10:lag3_PM10),1,mean,na.rm=T)
z$lag01_SO2 =apply(z %>% select(SO2 ,lag1_SO2)           ,1,mean,na.rm=T)
z$lag02_SO2 =apply(z %>% select(SO2 ,lag1_SO2:lag2_SO2)  ,1,mean,na.rm=T)
z$lag03_SO2 =apply(z %>% select(SO2 ,lag1_SO2:lag3_SO2)  ,1,mean,na.rm=T)
z$lag01_NO2 =apply(z %>% select(NO2 ,lag1_NO2)           ,1,mean,na.rm=T)
z$lag02_NO2 =apply(z %>% select(NO2 ,lag1_NO2:lag2_NO2)  ,1,mean,na.rm=T)
z$lag03_NO2 =apply(z %>% select(NO2 ,lag1_NO2:lag3_NO2)  ,1,mean,na.rm=T)
z$lag01_CO  =apply(z %>% select(CO  ,lag1_CO)            ,1,mean,na.rm=T)
z$lag02_CO  =apply(z %>% select(CO  ,lag1_CO:lag2_CO)    ,1,mean,na.rm=T)
z$lag03_CO  =apply(z %>% select(CO  ,lag1_CO:lag3_CO)    ,1,mean,na.rm=T)
z$lag01_O3  =apply(z %>% select(O3  ,lag1_O3)            ,1,mean,na.rm=T)
z$lag02_O3  =apply(z %>% select(O3  ,lag1_O3:lag2_O3)    ,1,mean,na.rm=T)
z$lag03_O3  =apply(z %>% select(O3  ,lag1_O3:lag3_O3)    ,1,mean,na.rm=T)

agg.ap.r<-rbind(agg.f("서울"),agg.f("부산"),agg.f("대구"),
                agg.f("인천"),agg.f("광주"),agg.f("대전"),
                agg.f("울산"),agg.f("경기"),agg.f("강원"),
                agg.f("충북"),agg.f("충남"),agg.f("전북"),
                agg.f("전남"),agg.f("경북"),agg.f("경남"),agg.f("제주"),z)

m1<-aggregate(agg.ap.r$PM25,list(agg.ap.r$sido),mean)
m2<-aggregate(agg.ap.r$PM10,list(agg.ap.r$sido),mean)
m3<-aggregate(agg.ap.r$SO2,list(agg.ap.r$sido),mean)
m4<-aggregate(agg.ap.r$NO2,list(agg.ap.r$sido),mean)
m5<-aggregate(agg.ap.r$CO,list(agg.ap.r$sido),mean)
m6<-aggregate(agg.ap.r$O3,list(agg.ap.r$sido),mean)

sd1<-aggregate(agg.ap.r$PM25,list(agg.ap.r$sido),sd)
sd2<-aggregate(agg.ap.r$PM10,list(agg.ap.r$sido),sd)
sd3<-aggregate(agg.ap.r$SO2,list(agg.ap.r$sido),sd)
sd4<-aggregate(agg.ap.r$NO2,list(agg.ap.r$sido),sd)
sd5<-aggregate(agg.ap.r$CO,list(agg.ap.r$sido),sd)
sd6<-aggregate(agg.ap.r$O3,list(agg.ap.r$sido),sd)

cbind(m1,m2$x,m3$x,m4$x,m5$x,m6$x) %>% View
cbind(sd1,sd2$x,sd3$x,sd4$x,sd5$x,sd6$x) %>% View

agg.ap.r$PM25 %>% mean
agg.ap.r$PM10 %>% mean
agg.ap.r$SO2 %>% mean
agg.ap.r$NO2 %>% mean
agg.ap.r$CO %>% mean
agg.ap.r$O3 %>% mean

agg.ap.r$PM25 %>% sd
agg.ap.r$PM10 %>% sd
agg.ap.r$SO2 %>% sd
agg.ap.r$NO2 %>% sd
agg.ap.r$CO %>% sd
agg.ap.r$O3 %>% sd


hn.r<-merge(hn,agg.ap.r,by="key",all.x=T)

hn.r$underlying_disease1=with(hn.r,ifelse(apply(hn.r %>% dplyr:: select(DI1_dg:DE1_dg),1,sum,na.rm=T)==0,0,
                                          ifelse(DI1_dg==1 | DI2_dg==1 | DI3_dg==1 | DI4_dg==1 |DI5_dg==1 | DE1_dg==1,1,9)))

hn.r$underlying_disease2=with(hn.r,ifelse(apply(hn.r %>% dplyr::select(DI1_pr:DE1_pr),1,sum,na.rm=T)==0,0,
                                          ifelse(DI1_pr==1 | DI2_pr==1 | DI3_pr==1 | DI4_pr==1 |DI5_pr==1 | DE1_pr==1,1,9)))

#연령 20세 이상 대상
hn.r<-subset(hn.r,age>=20)
hn.r<-subset(hn.r,sido!="세종")
hn.r<-subset(hn.r,sido!="제주")

#BUN 계산, 크레아티닌 보정 (단순 version)
hn.r$HE_BUN.cr=with(hn.r,HE_BUN/HE_crea)

#eGFR 계산, MDRD
#MDRD4 계산이 186에서 175로 변경되었음 IDMS랑 관련이 있음
#IDMS(Isotope dilution mass spectrometry) 
#과거에는 186 곱해서 계산 지금은 175로 변경 
#Modification of Diet in Renal Disease (MDRD)-4 equation
#175 * Scr^(-1.154) * age^(-0.203)*1.212 
hn.r$eGFR=with(hn.r,ifelse(sex==1,(175*(HE_crea)^(-1.154))*age^(-0.203),NA))
hn.r$eGFR=with(hn.r,ifelse(sex==2,(175*(HE_crea)^(-1.154))*(age^(-0.203))*0.742,eGFR))

#eGFR 60 이상인 경우에 대해서 CKD-EPI formula 적용
#k값 (남자면 0.9, 여자면 0.7로 나눔)
hn.r$Scr_k    =with(hn.r,ifelse(sex==1,HE_crea/0.9,HE_crea/0.7))
hn.r$indicator=1

#남자고 eGFR이 60 초과라면
hn.r$CKD_EPI=with(hn.r,
                  ifelse(eGFR>60 & sex==1,
                         141*(apply(hn.r %>% dplyr:: select(Scr_k,indicator),1,min)^(-0.411))*
                           (apply(hn.r %>% dplyr:: select(Scr_k,indicator),1,max)^(-1.209))*(0.993^age),
                         ifelse(eGFR>60 & sex==2,
                                141*(apply(hn.r %>% dplyr:: select(Scr_k,indicator),1,min)^(-0.329))*
                                  (apply(hn.r %>% dplyr:: select(Scr_k,indicator),1,max)^(-1.209))*(0.993^age)*1.018,
                                eGFR)))

ifelse(hn.r$eGFR>=60,0,1) %>% table
ifelse(hn.r$CKD_EPI>=60,0,1) %>% table

subset(hn.r,sex==1)$eGFR %>% mean
subset(hn.r,sex==2)$eGFR %>% mean

#eGFR 수준에 따라 계급화 -> CKD 단계
hn.r$eGFR_g=factor(with(hn.r,ifelse(eGFR>=60,0,1)))
hn.r$eGFR_4g=factor(with(hn.r,ifelse(eGFR>=60,0,
                                     ifelse(eGFR>=30 & eGFR<60,1,
                                            ifelse(eGFR>=15 & eGFR<30,2,3)))))

table(hn.r$eGFR_g)
table(hn.r$eGFR_4g)

hn.r$CKD_EPI_g=factor(with(hn.r,ifelse(CKD_EPI>=60,0,1)))
hn.r$CKD_EPI_4g=factor(with(hn.r,ifelse(CKD_EPI>=60,0,
                                        ifelse(CKD_EPI>=30 & CKD_EPI<60,1,
                                               ifelse(CKD_EPI>=15 & CKD_EPI<30,2,3)))))

table(hn.r$eGFR_g)
table(hn.r$eGFR_4g)

table(hn.r$CKD_EPI_g)
table(hn.r$CKD_EPI_4g)

#0이 있으면 로그변환시 무한대로 가니까 값 보정 
#치우쳐져 있는 자료 -> 자료변환 
hn.r$ln_BUN.cr=log(hn.r$HE_BUN.cr+0.0001)
hn.r$ln_crea  =log(hn.r$HE_crea+0.0001)



#결측치 있는 자료 지우고 분석에 이용할 자료만 
nrow(hn.r)
hn.r<-hn.r[complete.cases(hn.r %>% dplyr:: select(key:PM25,underlying_disease1,underlying_disease2,BD1_11,BS3_1)),]
nrow(hn.r)
hn.r<-subset(hn.r,hn.r$BD1_11!=9)
hn.r<-subset(hn.r,hn.r$BS3_1 !=9)
nrow(hn.r)
#분포만 보기 
x11();par(mfrow=c(1,3))
hist(log(hn.r$HE_BUN.cr),n=100,main="Log BUN.cr",xlab="BUN.cr",cex.lab=1.4,cex.axis=1.4,col="gray80");
hist(log(hn.r$HE_crea),n=100,main="Log creatinine",xlab="Creatinine",cex.lab=1.4,cex.axis=1.4,col="gray80");
hist(hn.r$eGFR,n=100,xlim=c(0,250),main="eGFR",xlab="eGFR",cex.lab=1.4,cex.axis=1.4,col="gray80")

write.csv(hn.r,file="국건영_EGFR_AP.csv",row.names=F,na="")

#--------------------------------------------------------------------------------#
#신장기능 지표 분포 & PM2.5(검사 당해년도 노출 농도)와 관련성
x11();grid.arrange(ggplot(hn.r,aes(ln_BUN.cr))+geom_histogram()+stat_bin(bins=100)+theme_gray(base_size=20)+
                     labs(x="Log BUN.cr",y="Count"),
                   ggplot(hn.r,aes(PM25,ln_BUN.cr))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(PM[2.5] *" (" * mu*"g/m"^3*")"),y="Log BUN.cr"),ncol=2)

x11();grid.arrange(ggplot(hn.r,aes(ln_crea))+geom_histogram()+stat_bin(bins=100)+theme_gray(base_size=20)+
                     labs(x="Log creatinine",y="Count"),
                   ggplot(hn.r,aes(PM25,ln_crea))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(PM[2.5] *" (" * mu*"g/m"^3*")"),y="Log creatinine"),ncol=2)

x11();grid.arrange(ggplot(hn.r,aes(eGFR))+geom_histogram()+stat_bin(bins=100)+theme_gray(base_size=20)+
                     labs(x="eGFR",y="Count")+xlim(0,250),
                   ggplot(hn.r,aes(PM25,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(PM[2.5] *" (" * mu*"g/m"^3*")"),y="eGFR")+ylim(75,125),ncol=2)

x11();grid.arrange(ggplot(hn.r,aes(eGFR))+geom_histogram()+stat_bin(bins=100)+theme_gray(base_size=20)+
                     labs(x="eGFR",y="Count")+xlim(0,250),
                   ggplot(hn.r,aes(PM25,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(PM[2.5] *" (" * mu*"g/m"^3*")"),y="eGFR"),ncol=2)

#eGFR 수준에 따라 정상군, 악화군 
eg1<-subset(hn.r,eGFR<60)  #악화
eg2<-subset(hn.r,eGFR>=60) #정상

#eGFR 층화하여서 볼 
x11();grid.arrange(ggplot(hn.r,aes(PM25,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+
                     labs(x=expression(PM[2.5] *" (" * mu*"g/m"^3*")"),y="eGFR"),
                   ggplot(eg1,aes(PM25,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+
                     labs(x=expression(PM[2.5] *" (" * mu*"g/m"^3*")"),y="eGFR"),
                   ggplot(eg2,aes(PM25,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+ylim(60,200)+
                     labs(x=expression(PM[2.5] *" (" * mu*"g/m"^3*")"),y="eGFR"),ncol=3)

hn.r$so

x11();grid.arrange(ggplot(hn.r,aes(PM10,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+
                     labs(x=expression(PM[10] *" (" * mu*"g/m"^3*")"),y="eGFR"),
                   ggplot(eg1,aes(PM10,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+
                     labs(x=expression(PM[10] *" (" * mu*"g/m"^3*")"),y="eGFR"),
                   ggplot(eg2,aes(PM10,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+ylim(60,200)+
                     labs(x=expression(PM[10] *" (" * mu*"g/m"^3*")"),y="eGFR"),ncol=3)

x11();grid.arrange(ggplot(hn.r,aes(SO2,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+
                     labs(x=expression(SO[2] *" (ppb)"),y="eGFR"),
                   ggplot(eg1,aes(SO2,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+
                     labs(x=expression(SO[2] *" (ppb)"),y="eGFR"),
                   ggplot(eg2,aes(SO2,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+ylim(60,200)+
                     labs(x=expression(SO[2] *" (ppb)"),y="eGFR"),ncol=3)

x11();grid.arrange(ggplot(hn.r,aes(NO2,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+
                     labs(x=expression(NO[2] *" (ppb)"),y="eGFR"),
                   ggplot(eg1,aes(NO2,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+
                     labs(x=expression(NO[2] *" (ppb)"),y="eGFR"),
                   ggplot(eg2,aes(NO2,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+ylim(60,200)+
                     labs(x=expression(NO[2] *" (ppb)"),y="eGFR"),ncol=3)

x11();grid.arrange(ggplot(hn.r,aes(CO,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+
                     labs(x=expression(CO[] *" (ppb)"),y="eGFR"),
                   ggplot(eg1,aes(CO,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+
                     labs(x=expression(CO[] *" (ppb)"),y="eGFR"),
                   ggplot(eg2,aes(CO,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+ylim(60,200)+
                     labs(x=expression(CO[] *" (ppb)"),y="eGFR"),ncol=3)

x11();grid.arrange(ggplot(hn.r,aes(O3,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+
                     labs(x=expression(O[3] *" (ppb)"),y="eGFR"),
                   ggplot(eg1,aes(O3,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+
                     labs(x=expression(O[3] *" (ppb)"),y="eGFR"),
                   ggplot(eg2,aes(O3,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+ylim(60,200)+
                     labs(x=expression(O[3] *" (ppb)"),y="eGFR"),ncol=3)

#------------------------------------------------------------------------------#
#복합표본 설계 

#복합표본분석 준비
com.svy<-svydesign(ids    =~psu,       #집락
                   strata =~kstrata,   #층
                   weights=~T_wt_itvex,#가중치
                   data   =hn.r)

svytotal(~sex==2,design=com.svy)
round(rbind(svytable(~sex,design=com.svy),
            svytable(~sex,design=com.svy,Ntotal=T)),3)
?svytable
svyvar(~sex,com.svy)

round(rbind(svytable(~sex,design=com.svy),
            svytable(~sex,design=com.svy,Ntotal=T)),3)
svytotal(~sex==1,design=com.svy,na.rm=T)

round(rbind(svytable(~underlying_disease2,design=com.svy),
            svytable(~underlying_disease2,design=com.svy,Ntotal=T)),3)
svytotal(~underlying_disease2==1,design=com.svy,na.rm=T)

round(rbind(svytable(~edu,design=com.svy),
            svytable(~edu,design=com.svy,Ntotal=T)),3)
svytotal(~edu==1,design=com.svy,na.rm=T)
svytotal(~edu==2,design=com.svy,na.rm=T)
svytotal(~edu==3,design=com.svy,na.rm=T)
svytotal(~edu==4,design=com.svy,na.rm=T)

round(rbind(svytable(~incm,design=com.svy),
            svytable(~incm,design=com.svy,Ntotal=T)),3)
svytotal(~incm==1,design=com.svy,na.rm=T)
svytotal(~incm==2,design=com.svy,na.rm=T)
svytotal(~incm==3,design=com.svy,na.rm=T)
svytotal(~incm==4,design=com.svy,na.rm=T)

round(rbind(svytable(~BD1_11,design=com.svy),
            svytable(~BD1_11,design=com.svy,Ntotal=T)),3)
svytotal(~BD1_11==1,design=com.svy,na.rm=T)
svytotal(~BD1_11==2,design=com.svy,na.rm=T)
svytotal(~BD1_11==3,design=com.svy,na.rm=T)
svytotal(~BD1_11==4,design=com.svy,na.rm=T)
svytotal(~BD1_11==5,design=com.svy,na.rm=T)
svytotal(~BD1_11==6,design=com.svy,na.rm=T)
svytotal(~BD1_11==8,design=com.svy,na.rm=T)

round(rbind(svytable(~BS3_1,design=com.svy),
            svytable(~BS3_1,design=com.svy,Ntotal=T)),3)
svytotal(~BS3_1==1,design=com.svy,na.rm=T)
svytotal(~BS3_1==2,design=com.svy,na.rm=T)
svytotal(~BS3_1==3,design=com.svy,na.rm=T)
svytotal(~BS3_1==8,design=com.svy,na.rm=T)

round(rbind(svytable(~eGFR_g,design=com.svy),
            svytable(~eGFR_g,design=com.svy,Ntotal=T)),3)
svytotal(~eGFR_g==1,design=com.svy,na.rm=T)
svytotal(~eGFR_g==0,design=com.svy,na.rm=T)



#가중되지 않은 빈도
addmargins(table(hn.r$sex))
prop.table(table(hn.r$sex))

#복합표본 그룹별 평균
svyby(~eGFR,by=~sex,com.svy,svymean) %>% round(3)
#------------------------------------------------------------------------------#
#선형회귀분석 eGFR & PM2.5

#혈압(SBP)은 모델 비교시 주요인자 아니라서 제외 -> LR test
#지역은 모델에서 대기오염과 공선성이 높아서 제외 VIF로 평가 

#fit1 : Crude model
#fit2 : adjusted model: age,sex,BMI,income,education,Triglyceride+underlying disease,drinking, smoking
#가중치 적용x

b.fita1<-lm(eGFR~PM25,data=hn.r);b.fita2<-lm(eGFR~PM10,data=hn.r)
b.fita3<-lm(eGFR~SO2 ,data=hn.r);fita4<-lm(eGFR~NO2 ,data=hn.r)
b.fita5<-lm(eGFR~CO  ,data=hn.r);b.fita6<-lm(eGFR~O3  ,data=hn.r)

b.fitb1<-lm(eGFR~PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb2<-lm(eGFR~PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb3<-lm(eGFR~SO2+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb4<-lm(eGFR~NO2+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb5<-lm(eGFR~CO+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb6<-lm(eGFR~O3+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)

b.result<-as.data.frame(rbind(summary(b.fita1)$coeff[2,],summary(b.fita2)$coeff[2,],
                              summary(b.fita3)$coeff[2,],summary(b.fita4)$coeff[2,],
                              summary(b.fita5)$coeff[2,],summary(b.fita6)$coeff[2,],
                              summary(b.fitb1)$coeff[2,],summary(b.fitb2)$coeff[2,],
                              summary(b.fitb3)$coeff[2,],summary(b.fitb4)$coeff[2,],
                              summary(b.fitb5)$coeff[2,],summary(b.fitb6)$coeff[2,]) %>% round(3))

b.result$model=rep(c("Crude model","Adjusted model"),each=6)
b.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
b.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
b.result$E      =(b.result$Estimate*b.result$uc) %>% round(3)
b.result$lci     =(b.result$uc*(b.result$Estimate-1.96*b.result$`Std. Error`))%>% round(3)
b.result$uci     =(b.result$uc*(b.result$Estimate+1.96*b.result$`Std. Error`))%>% round(3)

#복합표본분석-선형 회귀
#표준오차 확인 
svy.fita1<-svyglm(eGFR~PM25,data=hn.r,com.svy);svy.fita2<-svyglm(eGFR~PM10,data=hn.r,com.svy)
svy.fita3<-svyglm(eGFR~SO2,data=hn.r,com.svy) ;svy.fita4<-svyglm(eGFR~NO2,data=hn.r,com.svy)
svy.fita5<-svyglm(eGFR~CO,data=hn.r,com.svy)  ;svy.fita6<-svyglm(eGFR~O3,data=hn.r,com.svy)

svy.fitb1<-svyglm(eGFR~PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb2<-svyglm(eGFR~PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb3<-svyglm(eGFR~SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb4<-svyglm(eGFR~NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb5<-svyglm(eGFR~CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb6<-svyglm(eGFR~O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

b.result;s.result
View(b.result)
View(s.result)
#복합표본에서 다중공선성 검토 - 그래도 지역 빼야함 
# eliminate cases with missing values

V <- Vmat(mobj = svy.fit3,
          stvar = "kstrata",
          clvar = "psu")

# construct X matrix using model.matrix from stats package
X3 <- model.matrix(~ NO2+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+
                     factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),
                   data = data.frame(hn.r))

#공분산 행렬, 가중치, 데이터 셋 구성하여 모델링
svy_vif<-svyvif(X = X3[,-1], w = hn.r$T_wt_itvex, V = V)
View(svy_vif)
#------------------------------------------------------------------------------#
with(hn.r,aggregate(eGFR_g,list(sido),table))

#가중치 적용x, 신장기능 지표,eGFR 로지스틱회귀
fita1<-glm(eGFR_g~PM25,data=hn.r,family="binomial");fita2<-glm(eGFR_g~PM10,data=hn.r,family="binomial")
fita3<-glm(eGFR_g~SO2,data=hn.r,family="binomial") ;fita4<-glm(eGFR_g~NO2,data=hn.r,family="binomial")
fita5<-glm(eGFR_g~CO,data=hn.r,family="binomial")  ;fita6<-glm(eGFR_g~O3,data=hn.r,family="binomial")

fitb1<-glm(eGFR_g~PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
fitb2<-glm(eGFR_g~PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
fitb3<-glm(eGFR_g~SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
fitb4<-glm(eGFR_g~NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
fitb5<-glm(eGFR_g~CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
fitb6<-glm(eGFR_g~O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")

b.result<-as.data.frame(rbind(summary(fita1)$coeff[2,],summary(fita2)$coeff[2,],
                              summary(fita3)$coeff[2,],summary(fita4)$coeff[2,],
                              summary(fita5)$coeff[2,],summary(fita6)$coeff[2,],
                              summary(fitb1)$coeff[2,],summary(fitb2)$coeff[2,],
                              summary(fitb3)$coeff[2,],summary(fitb4)$coeff[2,],
                              summary(fitb5)$coeff[2,],summary(fitb6)$coeff[2,]) %>% round(3))

b.result$model=rep(c("Crude model","Adjusted model"),each=6)
b.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
b.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
b.result$OR      =exp(b.result$Estimate*b.result$uc) %>% round(3)
b.result$lci     =exp(b.result$uc*(b.result$Estimate-1.96*b.result$`Std. Error`))%>% round(3)
b.result$uci     =exp(b.result$uc*(b.result$Estimate+1.96*b.result$`Std. Error`))%>% round(3)

#가중치 적용, 신장기능 지표,eGFR 로지스틱회귀
svy.fita1<-svyglm(eGFR_g~PM25,data=hn.r,com.svy,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~PM10,data=hn.r,com.svy,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~SO2,data=hn.r,com.svy ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~NO2,data=hn.r,com.svy ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~CO,data=hn.r,com.svy  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~O3,data=hn.r,com.svy  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

b.result;s.result

View(b.result)
View(s.result)
#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
#Linear regression Lag effect 
b.fita1<-lm(eGFR~lag1_PM25,data=hn.r);b.fita2<-lm(eGFR~lag1_PM10,data=hn.r)
b.fita3<-lm(eGFR~lag1_SO2 ,data=hn.r);b.fita4<-lm(eGFR~lag1_NO2 ,data=hn.r)
b.fita5<-lm(eGFR~lag1_CO  ,data=hn.r);b.fita6<-lm(eGFR~lag1_O3  ,data=hn.r)

b.fitb1<-lm(eGFR~lag1_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb2<-lm(eGFR~lag1_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb3<-lm(eGFR~lag1_SO2+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb4<-lm(eGFR~lag1_NO2+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb5<-lm(eGFR~lag1_CO+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb6<-lm(eGFR~lag1_O3+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)

b.result<-as.data.frame(rbind(summary(b.fita1)$coeff[2,],summary(b.fita2)$coeff[2,],
                              summary(b.fita3)$coeff[2,],summary(b.fita4)$coeff[2,],
                              summary(b.fita5)$coeff[2,],summary(b.fita6)$coeff[2,],
                              summary(b.fitb1)$coeff[2,],summary(b.fitb2)$coeff[2,],
                              summary(b.fitb3)$coeff[2,],summary(b.fitb4)$coeff[2,],
                              summary(b.fitb5)$coeff[2,],summary(b.fitb6)$coeff[2,]) %>% round(3))

b.result$model=rep(c("Crude model","Adjusted model"),each=6)
b.result$exposure=c("lag1_PM2.5","lag1_PM10","lag1_SO2","lag1_NO2","lag1_CO","lag1_O3")
b.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
b.result$E      =(b.result$Estimate*b.result$uc) %>% round(3)
b.result$lci     =(b.result$uc*(b.result$Estimate-1.96*b.result$`Std. Error`))%>% round(3)
b.result$uci     =(b.result$uc*(b.result$Estimate+1.96*b.result$`Std. Error`))%>% round(3)

#복합표본분석-선형 회귀
#표준오차 확인 
svy.fita1<-svyglm(eGFR~lag1_PM25,data=hn.r,com.svy);svy.fita2<-svyglm(eGFR~lag1_PM10,data=hn.r,com.svy)
svy.fita3<-svyglm(eGFR~lag1_SO2,data=hn.r,com.svy) ;svy.fita4<-svyglm(eGFR~lag1_NO2,data=hn.r,com.svy)
svy.fita5<-svyglm(eGFR~lag1_CO,data=hn.r,com.svy)  ;svy.fita6<-svyglm(eGFR~lag1_O3,data=hn.r,com.svy)

svy.fitb1<-svyglm(eGFR~lag1_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb2<-svyglm(eGFR~lag1_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb3<-svyglm(eGFR~lag1_SO2+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb4<-svyglm(eGFR~lag1_NO2+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb5<-svyglm(eGFR~lag1_CO+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb6<-svyglm(eGFR~lag1_O3+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

b.result;s.result

View(b.result)
View(s.result)
#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
#Linear regression Moving average effect

#Lag effect 
b.fita1<-lm(eGFR~lag01_PM25,data=hn.r);b.fita2<-lm(eGFR~lag01_PM10,data=hn.r)
b.fita3<-lm(eGFR~lag01_SO2 ,data=hn.r);b.fita4<-lm(eGFR~lag01_NO2 ,data=hn.r)
b.fita5<-lm(eGFR~lag01_CO  ,data=hn.r);b.fita6<-lm(eGFR~lag01_O3  ,data=hn.r)

b.fitb1<-lm(eGFR~lag01_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb2<-lm(eGFR~lag01_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb3<-lm(eGFR~lag01_SO2+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb4<-lm(eGFR~lag01_NO2+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb5<-lm(eGFR~lag01_CO+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
b.fitb6<-lm(eGFR~lag01_O3+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)

b.result<-as.data.frame(rbind(summary(b.fita1)$coeff[2,],summary(b.fita2)$coeff[2,],
                              summary(b.fita3)$coeff[2,],summary(b.fita4)$coeff[2,],
                              summary(b.fita5)$coeff[2,],summary(b.fita6)$coeff[2,],
                              summary(b.fitb1)$coeff[2,],summary(b.fitb2)$coeff[2,],
                              summary(b.fitb3)$coeff[2,],summary(b.fitb4)$coeff[2,],
                              summary(b.fitb5)$coeff[2,],summary(b.fitb6)$coeff[2,]) %>% round(3))

b.result$model=rep(c("Crude model","Adjusted model"),each=6)
b.result$exposure=c("lag01_PM2.5","lag01_PM10","lag01_SO2","lag01_NO2","lag01_CO","lag01_O3")
b.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
b.result$E      =(b.result$Estimate*b.result$uc) %>% round(3)
b.result$lci     =(b.result$uc*(b.result$Estimate-1.96*b.result$`Std. Error`))%>% round(3)
b.result$uci     =(b.result$uc*(b.result$Estimate+1.96*b.result$`Std. Error`))%>% round(3)

#복합표본분석-선형 회귀
#표준오차 확인 
svy.fita1<-svyglm(eGFR~lag01_PM25,data=hn.r,com.svy);svy.fita2<-svyglm(eGFR~lag01_PM10,data=hn.r,com.svy)
svy.fita3<-svyglm(eGFR~lag01_SO2,data=hn.r,com.svy) ;svy.fita4<-svyglm(eGFR~lag01_NO2,data=hn.r,com.svy)
svy.fita5<-svyglm(eGFR~lag01_CO,data=hn.r,com.svy)  ;svy.fita6<-svyglm(eGFR~lag01_O3,data=hn.r,com.svy)

svy.fitb1<-svyglm(eGFR~lag01_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb2<-svyglm(eGFR~lag01_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb3<-svyglm(eGFR~lag01_SO2 +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb4<-svyglm(eGFR~lag01_NO2 +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb5<-svyglm(eGFR~lag01_CO  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)
svy.fitb6<-svyglm(eGFR~lag01_O3  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("lag01_PM2.5","lag01_PM10","lag01_SO2","lag01_NO2","lag01_CO","lag01_O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E       =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

b.result;s.result
View(b.result)
View(s.result)
#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
#Logistic regression Lag effect 
b.fita1<-glm(eGFR_g~lag1_PM25,data=hn.r,family="binomial");b.fita2<-glm(eGFR_g~lag1_PM10,data=hn.r,family="binomial")
b.fita3<-glm(eGFR_g~lag1_SO2 ,data=hn.r,family="binomial");b.fita4<-glm(eGFR_g~lag1_NO2 ,data=hn.r,family="binomial")
b.fita5<-glm(eGFR_g~lag1_CO  ,data=hn.r,family="binomial");b.fita6<-glm(eGFR_g~lag1_O3  ,data=hn.r,family="binomial")

b.fitb1<-glm(eGFR_g~lag1_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
b.fitb2<-glm(eGFR_g~lag1_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
b.fitb3<-glm(eGFR_g~lag1_SO2 +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
b.fitb4<-glm(eGFR_g~lag1_NO2 +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
b.fitb5<-glm(eGFR_g~lag1_CO  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
b.fitb6<-glm(eGFR_g~lag1_O3  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")

b.result<-as.data.frame(rbind(summary(b.fita1)$coeff[2,],summary(b.fita2)$coeff[2,],
                              summary(b.fita3)$coeff[2,],summary(b.fita4)$coeff[2,],
                              summary(b.fita5)$coeff[2,],summary(b.fita6)$coeff[2,],
                              summary(b.fitb1)$coeff[2,],summary(b.fitb2)$coeff[2,],
                              summary(b.fitb3)$coeff[2,],summary(b.fitb4)$coeff[2,],
                              summary(b.fitb5)$coeff[2,],summary(b.fitb6)$coeff[2,]) %>% round(3))

b.result$model=rep(c("Crude model","Adjusted model"),each=6)
b.result$exposure=c("lag1_PM2.5","lag1_PM10","lag1_SO2","lag1_NO2","lag1_CO","lag1_O3")
b.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
b.result$OR      =exp(b.result$Estimate*b.result$uc) %>% round(3)
b.result$lci     =exp(b.result$uc*(b.result$Estimate-1.96*b.result$`Std. Error`))%>% round(3)
b.result$uci     =exp(b.result$uc*(b.result$Estimate+1.96*b.result$`Std. Error`))%>% round(3)

#복합표본분석-로지스틱 회귀
#표준오차 확인 
svy.fita1<-svyglm(eGFR_g~lag1_PM25,data=hn.r,com.svy,family="quasibinomial");svy.fita2<-svyglm(eGFR_g~lag1_PM10,data=hn.r,com.svy,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~lag1_SO2 ,data=hn.r,com.svy,family="quasibinomial");svy.fita4<-svyglm(eGFR_g~lag1_NO2 ,data=hn.r,com.svy,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~lag1_CO  ,data=hn.r,com.svy,family="quasibinomial");svy.fita6<-svyglm(eGFR_g~lag1_O3  ,data=hn.r,com.svy,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~lag1_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~lag1_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~lag1_SO2 +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~lag1_NO2 +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~lag1_CO  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~lag1_O3  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

b.result;s.result
View(b.result)
View(s.result)
#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
#Logistic regression Moving average effect
b.fita1<-glm(eGFR_g~lag01_PM25,data=hn.r,family="binomial");b.fita2<-glm(eGFR_g~lag01_PM10,data=hn.r,family="binomial")
b.fita3<-glm(eGFR_g~lag01_SO2 ,data=hn.r,family="binomial");b.fita4<-glm(eGFR_g~lag01_NO2 ,data=hn.r,family="binomial")
b.fita5<-glm(eGFR_g~lag01_CO  ,data=hn.r,family="binomial");b.fita6<-glm(eGFR_g~lag01_O3  ,data=hn.r,family="binomial")

b.fitb1<-glm(eGFR_g~lag01_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
b.fitb2<-glm(eGFR_g~lag01_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
b.fitb3<-glm(eGFR_g~lag01_SO2 +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
b.fitb4<-glm(eGFR_g~lag01_NO2 +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
b.fitb5<-glm(eGFR_g~lag01_CO  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
b.fitb6<-glm(eGFR_g~lag01_O3  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")

b.result<-as.data.frame(rbind(summary(b.fita1)$coeff[2,],summary(b.fita2)$coeff[2,],
                              summary(b.fita3)$coeff[2,],summary(b.fita4)$coeff[2,],
                              summary(b.fita5)$coeff[2,],summary(b.fita6)$coeff[2,],
                              summary(b.fitb1)$coeff[2,],summary(b.fitb2)$coeff[2,],
                              summary(b.fitb3)$coeff[2,],summary(b.fitb4)$coeff[2,],
                              summary(b.fitb5)$coeff[2,],summary(b.fitb6)$coeff[2,]) %>% round(3))

b.result$model=rep(c("Crude model","Adjusted model"),each=6)
b.result$exposure=c("lag01_PM2.5","lag01_PM10","lag01_SO2","lag01_NO2","lag01_CO","lag01_O3")
b.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
b.result$OR      =exp(b.result$Estimate*b.result$uc) %>% round(3)
b.result$lci     =exp(b.result$uc*(b.result$Estimate-1.96*b.result$`Std. Error`))%>% round(3)
b.result$uci     =exp(b.result$uc*(b.result$Estimate+1.96*b.result$`Std. Error`))%>% round(3)

#복합표본분석-선형 회귀
#표준오차 확인 
svy.fita1<-svyglm(eGFR_g~lag01_PM25,data=hn.r,com.svy,family="quasibinomial");svy.fita2<-svyglm(eGFR_g~lag01_PM10,data=hn.r,com.svy,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~lag01_SO2,data=hn.r,com.svy ,family="quasibinomial") ;svy.fita4<-svyglm(eGFR_g~lag01_NO2,data=hn.r,com.svy,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~lag01_CO,data=hn.r,com.svy  ,family="quasibinomial")  ;svy.fita6<-svyglm(eGFR_g~lag01_O3,data=hn.r,com.svy,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~lag01_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~lag01_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~lag01_SO2 +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~lag01_NO2 +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~lag01_CO  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~lag01_O3  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("lag01_PM2.5","lag01_PM10","lag01_SO2","lag01_NO2","lag01_CO","lag01_O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

b.result;s.result

View(b.result)
View(s.result)
