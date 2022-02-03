#------------------------------------------------------------------------------#
#작업 디렉토리 (국건영 자료 보관 위치)
setwd("D:\\EUMC\\데이터관리\\국민건강영양조사\\검진")
#------------------------------------------------------------------------------#
#Library 설정
pacman::p_load("dplyr","ggplot2","reshape2","mgcv","sqldf","psych","Rmisc","lsmeans",
               "doBy","RColorBrewer","lubridate","caret","e1071","lmtest","readxl","splines",
               "metafor","mixmeta","data.table","stringr","lme4","gee","geepack","ggrepel","MuMIn",
               "qcc","extrafont","scales","gamm4","gridExtra","survey")
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

addmargins(table(hn16$DI1_dg));addmargins(table(hn16$DI2_dg))
addmargins(table(hn16$DI3_dg));addmargins(table(hn16$DI4_dg))
addmargins(table(hn16$DI5_dg));addmargins(table(hn16$DE1_dg))
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

agg.ap.r$PM25 %>% mean;agg.ap.r$PM25 %>% sd
agg.ap.r$PM10 %>% mean;agg.ap.r$PM10 %>% sd
agg.ap.r$SO2 %>% mean ;agg.ap.r$SO2 %>% sd
agg.ap.r$NO2 %>% mean ;agg.ap.r$NO2 %>% sd
agg.ap.r$CO %>% mean  ;agg.ap.r$CO %>% sd
agg.ap.r$O3 %>% mean  ;agg.ap.r$O3 %>% sd

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

#eGFR 수준에 따라 계급화 -> CKD 단계
hn.r$eGFR_g=factor(with(hn.r,ifelse(eGFR>=60,0,1)))
hn.r$eGFR_4g=factor(with(hn.r,ifelse(eGFR>=60,0,
                                     ifelse(eGFR>=30 & eGFR<60,1,
                                            ifelse(eGFR>=15 & eGFR<30,2,3)))))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#취합 업데이트 
setwd("D:\\EUMC\\논문\\연구논문\\PM2.5_Kidney_disease\\1.KNHANES_AP\\data")
hn.r<-read.csv("국건영_EGFR_AP.csv")

# #국건영 자료 불러오기 
setwd("D:\\EUMC\\데이터관리\\국민건강영양조사\\검진")
hn16_a<-read.csv("HN16_ALL.csv",header=T)
hn17_a<-read.csv("HN17_ALL.csv",header=T)
hn18_a<-read.csv("HN18_ALL.csv",header=T)

hn16.add<-hn16_a %>% select(ID,HE_HDL_st2,HE_LDL_drct)
hn17.add<-hn17_a %>% select(ID,HE_HDL_st2,HE_LDL_drct)
hn18.add<-hn18_a %>% select(ID,HE_HDL_st2,HE_LDL_drct)

hn.add<-rbind(hn16.add,hn17.add,hn18.add)

hn.r<-hn.r %>% left_join(hn.add,by="ID")

hn.r$LDL=ifelse(hn.r$HE_TG<400,hn.r$HE_chol-hn.r$HE_HDL_st2-hn.r$HE_TG/5,hn.r$HE_LDL_drct)

# write.csv(hn.r,file="D:\\EUMC\\논문\\연구논문\\PM2.5_Kidney_disease\\1.KNHANES_AP\\data\\국건영_EGFR_AP_LDL_add.csv")
# hn.r<-filter(hn.r,LDL>0)

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#변수들 정리 
#CKD-EPI 추가 
hn.r$eGFR_epi<-with(hn.r,ifelse(sex==2 & HE_crea<=0.7,144*((HE_crea/0.7)^(-0.329))*0.993^(age),
                                ifelse(sex==2 & HE_crea>0.7,144*((HE_crea/0.7)^(-1.209))*0.993^(age),
                                       ifelse(sex==1 & HE_crea<=0.9,141*((HE_crea/0.9)^(-0.411))*0.993^(age),
                                              141*((HE_crea/0.9)^(-1.209))*0.993^(age)))))
names(hn.r)
#CKD-EPI로 추정한 GFR 이진분류
hn.r$eGFR_g2=ifelse(hn.r$eGFR_epi<60,1,0)

#노출 사분위수 범주화
hn.r$PM25_4Q=ntile(hn.r$PM25,4) %>% as.factor
hn.r$PM10_4Q=ntile(hn.r$PM10,4) %>% as.factor
hn.r$SO2_4Q =ntile(hn.r$SO2 ,4) %>% as.factor
hn.r$NO2_4Q =ntile(hn.r$NO2 ,4) %>% as.factor
hn.r$CO_4Q  =ntile(hn.r$CO  ,4) %>% as.factor
hn.r$O3_4Q  =ntile(hn.r$O3  ,4) %>% as.factor

#흡연력 
hn.r$smoking=ifelse(hn.r$BS3_1<=2,1,
                    ifelse(hn.r$BS3_1==3,2,3)) %>% as.factor

#음주력 
hn.r$drink=ifelse(hn.r$BD1_11==1 | hn.r$BD1_11==8,1,2) %>% as.factor

#연령 그룹, 20???64 / ≥ 65
hn.r$age_group=ifelse(hn.r$age<65,1,2) %>% as.factor

#BMI group, < 25 ≥ 25
hn.r$BMI_group=ifelse(hn.r$HE_BMI<25,1,2) %>% as.factor

#--------------------------------------------------------------------------------#
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

x11();grid.arrange(ggplot(hn.r,aes(eGFR_epi))+geom_histogram()+stat_bin(bins=100)+theme_gray(base_size=20)+
                     labs(x="eGFR",y="Count")+xlim(0,250),
                   ggplot(hn.r,aes(PM25,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(PM[2.5] *" (" * mu*"g/m"^3*")"),y="eGFR")+ylim(75,125),ncol=2)

x11();grid.arrange(ggplot(hn.r,aes(eGFR_epi))+geom_histogram()+stat_bin(bins=100)+theme_gray(base_size=20)+
                     labs(x="eGFR",y="Count")+xlim(0,250),
                   ggplot(hn.r,aes(PM25,eGFR))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(PM[2.5] *" (" * mu*"g/m"^3*")"),y="eGFR"),ncol=2)
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#Table 1에서 테이블 표시 할때
#(1) non-CKD vs CKD 구분이 아니라 non-CKD 군에서 %, CKD 그룹군에서 % 산출
#(2) p-value는 non-CKD vs CKD p-value 볼 것 

#non-weighted 
tb.func<-function(data,X1,X2){
  
dd<-data  

dd$X1<-X1
dd$X2<-X2
  
tb      <-with(dd,table(X1,X2))
tb.chisq<-chisq.test(tb)
tb.chisq$p.value

sub1<-with(subset(dd,X2==0),table(X1))
sub2<-with(subset(dd,X2==1),table(X1))

tb_per<-round(cbind(cbind(prop.table(sub1)),cbind(prop.table(sub2)))*100,2)

cbind(cbind(tb[,1]),
      cbind(tb_per[,1]),
      cbind(tb[,2]),
      cbind(tb_per[,2]),
      c(tb.chisq$p.value %>% round(3)))
}

tb.func(hn.r,hn.r$sex,hn.r$eGFR_g2)
tb.func(hn.r,hn.r$underlying_disease2,hn.r$eGFR_g2)
tb.func(hn.r,hn.r$edu,hn.r$eGFR_g2)
tb.func(hn.r,hn.r$incm,hn.r$eGFR_g2)

#Smoking status, Current smoker / Ex-smoker / Non-smoker
tb.func(hn.r,hn.r$smoking,hn.r$eGFR_g2)

#Frequency of drinking, < 1 per month or none / ≥ 1 per month
tb.func(hn.r,hn.r$drink,hn.r$eGFR_g2)

tb.func(hn.r,hn.r$age_group,hn.r$eGFR_g2)
tb.func(hn.r,hn.r$BMI_group,hn.r$eGFR_g2)

#연속형 
with(hn.r,aggregate(age,list(eGFR_g2),mean))   ;with(hn.r,aggregate(age,list(eGFR_g2),sd))
with(hn.r,aggregate(HE_BMI,list(eGFR_g2),mean));with(hn.r,aggregate(HE_BMI,list(eGFR_g2),sd))
with(hn.r,aggregate(HE_TG,list(eGFR_g2),mean)) ;with(hn.r,aggregate(HE_TG,list(eGFR_g2),sd))

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#eGFR 수준에 따라 정상군, 악화군 
eg1<-subset(hn.r,eGFR_g2==1)  #악화
eg2<-subset(hn.r,eGFR_g2==0) #정상

x11();grid.arrange(ggplot(hn.r,aes(PM25,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(PM[2.5] *" (" * mu*"g/m"^3*")"),y="eGFR"),
                   ggplot(eg1,aes(PM25,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(PM[2.5] *" (" * mu*"g/m"^3*")"),y="eGFR"),
                   ggplot(eg2,aes(PM25,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+ylim(60,200)+labs(x=expression(PM[2.5] *" (" * mu*"g/m"^3*")"),y="eGFR"),ncol=3)

x11();grid.arrange(ggplot(hn.r,aes(PM10,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(PM[10] *" (" * mu*"g/m"^3*")"),y="eGFR"),
                   ggplot(eg1,aes(PM10,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(PM[10] *" (" * mu*"g/m"^3*")"),y="eGFR"),
                   ggplot(eg2,aes(PM10,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+ylim(60,200)+labs(x=expression(PM[10] *" (" * mu*"g/m"^3*")"),y="eGFR"),ncol=3)

x11();grid.arrange(ggplot(hn.r,aes(SO2,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(SO[2] *" (ppb)"),y="eGFR"),
                   ggplot(eg1,aes(SO2,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(SO[2] *" (ppb)"),y="eGFR"),
                   ggplot(eg2,aes(SO2,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+ylim(60,200)+labs(x=expression(SO[2] *" (ppb)"),y="eGFR"),ncol=3)

x11();grid.arrange(ggplot(hn.r,aes(NO2,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(NO[2] *" (ppb)"),y="eGFR"),
                   ggplot(eg1,aes(NO2,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(NO[2] *" (ppb)"),y="eGFR"),
                   ggplot(eg2,aes(NO2,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+ylim(60,200)+labs(x=expression(NO[2] *" (ppb)"),y="eGFR"),ncol=3)

x11();grid.arrange(ggplot(hn.r,aes(CO,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(CO[] *" (ppb)"),y="eGFR"),
                   ggplot(eg1,aes(CO,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(CO[] *" (ppb)"),y="eGFR"),
                   ggplot(eg2,aes(CO,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+ylim(60,200)+labs(x=expression(CO[] *" (ppb)"),y="eGFR"),ncol=3)

x11();grid.arrange(ggplot(hn.r,aes(O3,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(O[3] *" (ppb)"),y="eGFR"),
                   ggplot(eg1,aes(O3,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+labs(x=expression(O[3] *" (ppb)"),y="eGFR"),
                   ggplot(eg2,aes(O3,eGFR_epi))+geom_point()+stat_smooth(method="lm")+
                     theme_gray(base_size=20)+ylim(60,200)+labs(x=expression(O[3] *" (ppb)"),y="eGFR"),ncol=3)

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
fit1<-gam(eGFR_epi~s(PM25,k=4,fx=T)+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
fit2<-gam(eGFR_epi~s(PM10,k=4,fx=T)+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
fit3<-gam(eGFR_epi~s(SO2,k=4,fx=T)+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
fit4<-gam(eGFR_epi~s(NO2,k=4,fx=T)+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
fit5<-gam(eGFR_epi~s(CO,k=4,fx=T)+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)
fit6<-gam(eGFR_epi~s(O3,k=4,fx=T)+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r)

x11();par(mfrow=c(2,3))
plot(fit1,scheme=1,ylim=c(-5,8),cex.lab=1.45,cex.axis=1.4,ylab="Estimate",xlab=expression(paste(PM[2.5])));abline(h=0,col="red")
plot(fit2,scheme=1,ylim=c(-5,8),cex.lab=1.45,cex.axis=1.4,ylab="Estimate",xlab=expression(paste(PM[10]))) ;abline(h=0,col="red")
plot(fit3,scheme=1,ylim=c(-5,8),cex.lab=1.45,cex.axis=1.4,ylab="Estimate",xlab=expression(paste(SO[2])))  ;abline(h=0,col="red") 
plot(fit4,scheme=1,ylim=c(-5,8),cex.lab=1.45,cex.axis=1.4,ylab="Estimate",xlab=expression(paste(NO[2])))  ;abline(h=0,col="red")
plot(fit5,scheme=1,ylim=c(-5,8),cex.lab=1.45,cex.axis=1.4,ylab="Estimate",xlab=expression(paste(CO[])))   ;abline(h=0,col="red")
plot(fit6,scheme=1,ylim=c(-5,8),cex.lab=1.45,cex.axis=1.4,ylab="Estimate",xlab=expression(paste(O[3])))   ;abline(h=0,col="red")

fit1<-gam(eGFR_g2~s(PM25,k=4,fx=T)+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
fit2<-gam(eGFR_g2~s(PM10,k=4,fx=T)+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
fit3<-gam(eGFR_g2~s(SO2,k=4,fx=T)+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
fit4<-gam(eGFR_g2~s(NO2,k=4,fx=T)+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
fit5<-gam(eGFR_g2~s(CO,k=4,fx=T)+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")
fit6<-gam(eGFR_g2~s(O3,k=4,fx=T)+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,family="binomial")

x11();par(mfrow=c(2,3))
plot(fit1,scheme=1,cex.lab=1.45,cex.axis=1.4,ylab="Log Odds",xlab=expression(paste(PM[2.5])));abline(h=0,col="red")
plot(fit2,scheme=1,cex.lab=1.45,cex.axis=1.4,ylab="Log Odds",xlab=expression(paste(PM[10]))) ;abline(h=0,col="red")
plot(fit3,scheme=1,cex.lab=1.45,cex.axis=1.4,ylab="Log Odds",xlab=expression(paste(SO[2])))  ;abline(h=0,col="red") 
plot(fit4,scheme=1,cex.lab=1.45,cex.axis=1.4,ylab="Log Odds",xlab=expression(paste(NO[2])))  ;abline(h=0,col="red")
plot(fit5,scheme=1,cex.lab=1.45,cex.axis=1.4,ylab="Log Odds",xlab=expression(paste(CO[])))   ;abline(h=0,col="red")
plot(fit6,scheme=1,cex.lab=1.45,cex.axis=1.4,ylab="Log Odds",xlab=expression(paste(O[3])))   ;abline(h=0,col="red")

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#노출 단위 정리
#The estimates in the models were evaluated for PM2.5 and PM10 per 10 μg/m3 increase, 
#SO2 per 1 part per billion (ppb) increase, NO2 and O3 per 10 ppb increase and 
#CO per 0.1 part per million (ppm) increase, respectively.

hn.r$PM25_lag0_r=hn.r$PM25/10
hn.r$PM10_lag0_r=hn.r$PM10/10
hn.r$SO2_lag0_r =hn.r$SO2*1000
hn.r$NO2_lag0_r =hn.r$NO2*100
hn.r$CO_lag0_r  =hn.r$CO*10
hn.r$O3_lag0_r  =hn.r$O3*100

hn.r$PM25_lag1_r=hn.r$lag1_PM25/10
hn.r$PM10_lag1_r=hn.r$lag1_PM10/10
hn.r$SO2_lag1_r =hn.r$lag1_SO2*1000
hn.r$NO2_lag1_r =hn.r$lag1_NO2*100
hn.r$CO_lag1_r  =hn.r$lag1_CO*10
hn.r$O3_lag1_r  =hn.r$lag1_O3*100

hn.r$PM25_lag01_r=hn.r$lag01_PM25/10
hn.r$PM10_lag01_r=hn.r$lag01_PM10/10
hn.r$SO2_lag01_r =hn.r$lag01_SO2*1000
hn.r$NO2_lag01_r =hn.r$lag01_NO2*100
hn.r$CO_lag01_r  =hn.r$lag01_CO*10
hn.r$O3_lag01_r  =hn.r$lag01_O3*100

#복합표본 설계
com.svy<-svydesign(ids    =~psu,       #집락
                   strata =~kstrata,   #층
                   weights=~T_wt_itvex,#가중치
                   data   =hn.r)

#복합표본 표: 성별
svytotal(~sex==2,design=com.svy)
round(rbind(svytable(~sex,design=com.svy),
            svytable(~sex,design=com.svy,Ntotal=T)),3)

#복합표본 표: 기저 질환
round(rbind(svytable(~underlying_disease2,design=com.svy),
            svytable(~underlying_disease2,design=com.svy,Ntotal=T)),3)

svytotal(~underlying_disease2==1,design=com.svy,na.rm=T)

#복합표본 표: 교육 수준
round(rbind(svytable(~edu,design=com.svy),
            svytable(~edu,design=com.svy,Ntotal=T)),3)

svytotal(~edu==1,design=com.svy,na.rm=T);svytotal(~edu==2,design=com.svy,na.rm=T)
svytotal(~edu==3,design=com.svy,na.rm=T);svytotal(~edu==4,design=com.svy,na.rm=T)

#복합표본 표: 월 소득 수준
round(rbind(svytable(~incm,design=com.svy),
            svytable(~incm,design=com.svy,Ntotal=T)),3)

svytotal(~incm==1,design=com.svy,na.rm=T);svytotal(~incm==2,design=com.svy,na.rm=T)
svytotal(~incm==3,design=com.svy,na.rm=T);svytotal(~incm==4,design=com.svy,na.rm=T)

#복합표본 표: 흡연력
round(rbind(svytable(~smoking,design=com.svy),
            svytable(~smoking,design=com.svy,Ntotal=T)),3)

svytotal(~smoking==1,design=com.svy,na.rm=T);svytotal(~smoking==2,design=com.svy,na.rm=T)

#복합표본 표: 음주력 
round(rbind(svytable(~drink,design=com.svy),
            svytable(~drink,design=com.svy,Ntotal=T)),3)

svytotal(~drink==1,design=com.svy,na.rm=T);svytotal(~drink==2,design=com.svy,na.rm=T)


#복합표본 표: CKD 유무 eGFR 60 기준 
round(rbind(svytable(~eGFR_g2,design=com.svy),
            svytable(~eGFR_g2,design=com.svy,Ntotal=T)),3)

svytotal(~eGFR_g2==0,design=com.svy,na.rm=T)
svytotal(~eGFR_g2==1,design=com.svy,na.rm=T)

svytotal(~eGFR_g==1,design=com.svy,na.rm=T)

#복합표본 그룹별 평균
svyby(~eGFR_epi,by=~sex,com.svy,svymean) %>% round(3)

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#복합표본 설계 -성별
hn.sex1<-subset(hn.r,sex==1);
hn.sex2<-subset(hn.r,sex==2)

#복합표본 설계 -기저질환 
hn.ud1<-subset(hn.r,underlying_disease2==1)
hn.ud2<-subset(hn.r,underlying_disease2==9)

#복합표본 설계 -교육수준
hn.edu1<-subset(hn.r,edu==1);hn.edu2<-subset(hn.r,edu==2)
hn.edu3<-subset(hn.r,edu==3);hn.edu4<-subset(hn.r,edu==4)

#복합표본 설계 -소득수준 
hn.incm1<-subset(hn.r,incm==1);hn.incm2<-subset(hn.r,incm==2)
hn.incm3<-subset(hn.r,incm==3);hn.incm4<-subset(hn.r,incm==4)

#복합표본 설계 -흡연력 
hn.smk1<-subset(hn.r,smoking==1)
hn.smk2<-subset(hn.r,smoking==2)
hn.smk3<-subset(hn.r,smoking==3)

#복합표본 설계 -음주력 
hn.drk1<-subset(hn.r,drink==1)
hn.drk2<-subset(hn.r,drink==2)

#복합표본 설계 -연령 그룹 (20-64, 65>=)
hn.age1<-subset(hn.r,age_group==1);
hn.age2<-subset(hn.r,age_group==2)

#복합표본 설계 -BMI 25기준 
hn.bmi1<-subset(hn.r,BMI_group==1);
hn.bmi2<-subset(hn.r,BMI_group==2)

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#하위그룹분석 -복합표본:성별
com.svy.sex1<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.sex1)
com.svy.sex2<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.sex2)

#하위그룹분석 -복합표본:기저질환 
com.svy.ud1<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.ud1)
com.svy.ud2<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.ud2)

#하위그룹분석 -복합표본:교육수준
com.svy.edu1<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.edu1)
com.svy.edu2<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.edu2)
com.svy.edu3<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.edu3)
com.svy.edu4<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.edu4)

#하위그룹분석 -복합표본:소득수준 
com.svy.incm1<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.incm1)
com.svy.incm2<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.incm2)
com.svy.incm3<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.incm3)
com.svy.incm4<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.incm4)

#하위그룹분석 -복합표본:흡연력 
# options(survey.adjust.domain.lonely=TRUE)
# options(survey.lonely.psu="adjust")

com.svy.smk1<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.smk1)
com.svy.smk2<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.smk2)
com.svy.smk3<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.smk3)

#하위그룹분석 -복합표본:음주력 
com.svy.drk1<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.drk1)
com.svy.drk2<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.drk2)

#하위그룹분석 -복합표본:연령 그룹 
com.svy.age1<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.age1)
com.svy.age2<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.age2)

#하위그룹분석 -복합표본:BMI 그룹 
com.svy.bmi1<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.bmi1)
com.svy.bmi2<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.bmi2)

#복합표본 성별 테이블 & 카이제곱검정 (Pearson's X^2: Rao & Scott adjustment)
svytotal(~eGFR_g2==1,design=com.svy.sex1,na.rm=T)
svytotal(~eGFR_g2==1,design=com.svy.sex2,na.rm=T)
svychisq(~sex+eGFR_g2,com.svy)$p.value %>% round(3)

#복합표본 기저질환 테이블 & 카이제곱검정 (Pearson's X^2: Rao & Scott adjustment)
svytotal(~eGFR_g2==1,design=com.svy.ud1,na.rm=T)
svytotal(~eGFR_g2==1,design=com.svy.ud2,na.rm=T)
svychisq(~underlying_disease2+eGFR_g2,com.svy)$p.value %>% round(3)

#복합표본 교육수준 테이블 & 카이제곱검정 (Pearson's X^2: Rao & Scott adjustment)
svytotal(~eGFR_g2==1,design=com.svy.edu1,na.rm=T)
svytotal(~eGFR_g2==1,design=com.svy.edu2,na.rm=T)
svytotal(~eGFR_g2==1,design=com.svy.edu3,na.rm=T)
svytotal(~eGFR_g2==1,design=com.svy.edu4,na.rm=T)
svychisq(~edu+eGFR_g2,com.svy)$p.value %>% round(3)

#복합표본 소득수준 테이블 & 카이제곱검정 (Pearson's X^2: Rao & Scott adjustment)
svytotal(~eGFR_g2==1,design=com.svy.incm1,na.rm=T)
svytotal(~eGFR_g2==1,design=com.svy.incm2,na.rm=T)
svytotal(~eGFR_g2==1,design=com.svy.incm3,na.rm=T)
svytotal(~eGFR_g2==1,design=com.svy.incm4,na.rm=T)
svychisq(~incm+eGFR_g2,com.svy)$p.value %>% round(3)

#복합표본 흡연력 테이블 & 카이제곱검정 (Pearson's X^2: Rao & Scott adjustment)
svytotal(~eGFR_g2==1,design=com.svy.smk1,na.rm=T);
svytotal(~eGFR_g2==1,design=com.svy.smk2,na.rm=T)
svytotal(~eGFR_g2==1,design=com.svy.smk3,na.rm=T)
svychisq(~smoking+eGFR_g2,com.svy)$p.value %>% round(3)

#복합표본 음주력 테이블 & 카이제곱검정 (Pearson's X^2: Rao & Scott adjustment)
svytotal(~eGFR_g2==1,design=com.svy.drk1,na.rm=T);
svytotal(~eGFR_g2==1,design=com.svy.drk2,na.rm=T)
svychisq(~drink+eGFR_g2,com.svy)$p.value %>% round(3)

#복합표본 연령 그룹별 테이블 & 카이제곱검정 (Pearson's X^2: Rao & Scott adjustment)
svytotal(~eGFR_g2==1,design=com.svy.age1,na.rm=T);
svytotal(~eGFR_g2==1,design=com.svy.age2,na.rm=T)
svychisq(~age_group+eGFR_g2,com.svy)$p.value %>% round(3)

#복합표본 BMI 그룹별 테이블 & 카이제곱검정 (Pearson's X^2: Rao & Scott adjustment)
svytotal(~eGFR_g2==1,design=com.svy.bmi1,na.rm=T);
svytotal(~eGFR_g2==1,design=com.svy.bmi2,na.rm=T)
svychisq(~BMI_group+eGFR_g2,com.svy)$p.value %>% round(3)

hn.ckd1<-subset(hn.r,eGFR_g2==0)
hn.ckd2<-subset(hn.r,eGFR_g2==1)

com.svy.ckd1<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.ckd1)
com.svy.ckd2<-svydesign(ids=~psu,strata =~kstrata,weights=~T_wt_itvex,data=hn.ckd2)

svymean(~age,by=e,design=com.svy.ckd1)
svymean(~age,by=e,design=com.svy.ckd2)

svymean(~HE_BMI,by=e,design=com.svy.ckd1)
svymean(~HE_BMI,by=e,design=com.svy.ckd2)

svymean(~HE_TG,by=e,design=com.svy.ckd1)
svymean(~HE_TG,by=e,design=com.svy.ckd2)

t.test(age~eGFR_g2,hn.r)
t.test(HE_BMI~eGFR_g2,hn.r)
t.test(HE_TG~eGFR_g2,hn.r)

svyttest(age~eGFR_g2,com.svy)
svyttest(HE_BMI~eGFR_g2,com.svy)
svyttest(HE_TG~eGFR_g2,com.svy)

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#선형회귀분석 eGFR & PM2.5

#혈압(SBP)은 모델 비교시 주요인자 아니라서 제외 -> LR test
#지역은 모델에서 대기오염과 공선성이 높아서 제외 VIF로 평가 

#model 1 : Crude model
#model 2 : adjusted model: age,sex,BMI,income,education,Triglyceride+underlying disease,drinking, smoking

#Lag0
#eGFR, model 1 : Crude model
a1.fit1_lag0<-svyglm(eGFR_epi~PM25_lag0_r,data=hn.r,com.svy,family="gaussian")
a1.fit2_lag0<-svyglm(eGFR_epi~PM10_lag0_r,data=hn.r,com.svy,family="gaussian")
a1.fit3_lag0<-svyglm(eGFR_epi~SO2_lag0_r ,data=hn.r,com.svy,family="gaussian")
a1.fit4_lag0<-svyglm(eGFR_epi~NO2_lag0_r ,data=hn.r,com.svy,family="gaussian")
a1.fit5_lag0<-svyglm(eGFR_epi~CO_lag0_r  ,data=hn.r,com.svy,family="gaussian")
a1.fit6_lag0<-svyglm(eGFR_epi~O3_lag0_r  ,data=hn.r,com.svy,family="gaussian")

#eGFR, model 2 : adjusted model: age,sex,BMI,income,education,Triglyceride+underlying disease,drinking, smoking
a2.fit1_lag0<-svyglm(eGFR_epi~PM25_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit2_lag0<-svyglm(eGFR_epi~PM10_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit3_lag0<-svyglm(eGFR_epi~SO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit4_lag0<-svyglm(eGFR_epi~NO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit5_lag0<-svyglm(eGFR_epi~CO_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit6_lag0<-svyglm(eGFR_epi~O3_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")

#CKD(by CKD-EPI), model 1 : Crude model
b1.fit1_lag0<-svyglm(eGFR_g2~PM25_lag0_r,data=hn.r,com.svy,family="quasibinomial")
b1.fit2_lag0<-svyglm(eGFR_g2~PM10_lag0_r,data=hn.r,com.svy,family="quasibinomial")
b1.fit3_lag0<-svyglm(eGFR_g2~SO2_lag0_r ,data=hn.r,com.svy,family="quasibinomial")
b1.fit4_lag0<-svyglm(eGFR_g2~NO2_lag0_r ,data=hn.r,com.svy,family="quasibinomial")
b1.fit5_lag0<-svyglm(eGFR_g2~CO_lag0_r  ,data=hn.r,com.svy,family="quasibinomial")
b1.fit6_lag0<-svyglm(eGFR_g2~O3_lag0_r  ,data=hn.r,com.svy,family="quasibinomial")

#CKD(by CKD-EPI), model 2 : adjusted model: age,sex,BMI,income,education,Triglyceride+underlying disease,drinking, smoking
b2.fit1_lag0<-svyglm(eGFR_g2~PM25_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit2_lag0<-svyglm(eGFR_g2~PM10_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit3_lag0<-svyglm(eGFR_g2~SO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit4_lag0<-svyglm(eGFR_g2~NO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit5_lag0<-svyglm(eGFR_g2~CO_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit6_lag0<-svyglm(eGFR_g2~O3_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

#Lag1
#eGFR, model 1 : Crude model
a1.fit1_lag1<-svyglm(eGFR_epi~PM25_lag1_r,data=hn.r,com.svy,family="gaussian")
a1.fit2_lag1<-svyglm(eGFR_epi~PM10_lag1_r,data=hn.r,com.svy,family="gaussian")
a1.fit3_lag1<-svyglm(eGFR_epi~SO2_lag1_r ,data=hn.r,com.svy,family="gaussian")
a1.fit4_lag1<-svyglm(eGFR_epi~NO2_lag1_r ,data=hn.r,com.svy,family="gaussian")
a1.fit5_lag1<-svyglm(eGFR_epi~CO_lag1_r  ,data=hn.r,com.svy,family="gaussian")
a1.fit6_lag1<-svyglm(eGFR_epi~O3_lag1_r  ,data=hn.r,com.svy,family="gaussian")

#eGFR, model 2 : adjusted model: age,sex,BMI,income,education,Triglyceride+underlying disease,drinking, smoking
a2.fit1_lag1<-svyglm(eGFR_epi~PM25_lag1_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit2_lag1<-svyglm(eGFR_epi~PM10_lag1_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit3_lag1<-svyglm(eGFR_epi~SO2_lag1_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit4_lag1<-svyglm(eGFR_epi~NO2_lag1_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit5_lag1<-svyglm(eGFR_epi~CO_lag1_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit6_lag1<-svyglm(eGFR_epi~O3_lag1_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")

#CKD(by CKD-EPI), model 1 : Crude model
b1.fit1_lag1<-svyglm(eGFR_g2~PM25_lag1_r,data=hn.r,com.svy,family="quasibinomial")
b1.fit2_lag1<-svyglm(eGFR_g2~PM10_lag1_r,data=hn.r,com.svy,family="quasibinomial")
b1.fit3_lag1<-svyglm(eGFR_g2~SO2_lag1_r ,data=hn.r,com.svy,family="quasibinomial")
b1.fit4_lag1<-svyglm(eGFR_g2~NO2_lag1_r ,data=hn.r,com.svy,family="quasibinomial")
b1.fit5_lag1<-svyglm(eGFR_g2~CO_lag1_r  ,data=hn.r,com.svy,family="quasibinomial")
b1.fit6_lag1<-svyglm(eGFR_g2~O3_lag1_r  ,data=hn.r,com.svy,family="quasibinomial")

#CKD(by CKD-EPI), model 2 : adjusted model: age,sex,BMI,income,education,Triglyceride+underlying disease,drinking, smoking
b2.fit1_lag1<-svyglm(eGFR_g2~PM25_lag1_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit2_lag1<-svyglm(eGFR_g2~PM10_lag1_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit3_lag1<-svyglm(eGFR_g2~SO2_lag1_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit4_lag1<-svyglm(eGFR_g2~NO2_lag1_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit5_lag1<-svyglm(eGFR_g2~CO_lag1_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit6_lag1<-svyglm(eGFR_g2~O3_lag1_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#Lag01
#eGFR, model 1 : Crude model
a1.fit1_lag01<-svyglm(eGFR_epi~PM25_lag01_r,data=hn.r,com.svy,family="gaussian")
a1.fit2_lag01<-svyglm(eGFR_epi~PM10_lag01_r,data=hn.r,com.svy,family="gaussian")
a1.fit3_lag01<-svyglm(eGFR_epi~SO2_lag01_r ,data=hn.r,com.svy,family="gaussian")
a1.fit4_lag01<-svyglm(eGFR_epi~NO2_lag01_r ,data=hn.r,com.svy,family="gaussian")
a1.fit5_lag01<-svyglm(eGFR_epi~CO_lag01_r  ,data=hn.r,com.svy,family="gaussian")
a1.fit6_lag01<-svyglm(eGFR_epi~O3_lag01_r  ,data=hn.r,com.svy,family="gaussian")

#eGFR, model 2 : adjusted model: age,sex,BMI,income,education,Triglyceride+underlying disease,drinking, smoking
a2.fit1_lag01<-svyglm(eGFR_epi~PM25_lag01_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit2_lag01<-svyglm(eGFR_epi~PM10_lag01_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit3_lag01<-svyglm(eGFR_epi~SO2_lag01_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit4_lag01<-svyglm(eGFR_epi~NO2_lag01_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit5_lag01<-svyglm(eGFR_epi~CO_lag01_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")
a2.fit6_lag01<-svyglm(eGFR_epi~O3_lag01_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="gaussian")

#CKD(by CKD-EPI), model 1 : Crude model
b1.fit1_lag01<-svyglm(eGFR_g2~PM25_lag01_r,data=hn.r,com.svy,family="quasibinomial")
b1.fit2_lag01<-svyglm(eGFR_g2~PM10_lag01_r,data=hn.r,com.svy,family="quasibinomial")
b1.fit3_lag01<-svyglm(eGFR_g2~SO2_lag01_r ,data=hn.r,com.svy,family="quasibinomial")
b1.fit4_lag01<-svyglm(eGFR_g2~NO2_lag01_r ,data=hn.r,com.svy,family="quasibinomial")
b1.fit5_lag01<-svyglm(eGFR_g2~CO_lag01_r  ,data=hn.r,com.svy,family="quasibinomial")
b1.fit6_lag01<-svyglm(eGFR_g2~O3_lag01_r  ,data=hn.r,com.svy,family="quasibinomial")

#CKD(by CKD-EPI), model 2 : adjusted model: age,sex,BMI,income,education,Triglyceride+underlying disease,drinking, smoking
b2.fit1_lag01<-svyglm(eGFR_g2~PM25_lag01_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit2_lag01<-svyglm(eGFR_g2~PM10_lag01_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit3_lag01<-svyglm(eGFR_g2~SO2_lag01_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit4_lag01<-svyglm(eGFR_g2~NO2_lag01_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit5_lag01<-svyglm(eGFR_g2~CO_lag01_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")
b2.fit6_lag01<-svyglm(eGFR_g2~O3_lag01_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(smoking)+factor(drink),data=hn.r,com.svy,family="quasibinomial")

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#결과 취합 
func.res1<-function(out){
  out2<-as.data.frame(t(summary(out)$coeff[2,]))
  out2$Est=with(out2,Estimate) %>% round(2)
  out2$lci=with(out2,Estimate-1.96*`Std. Error`) %>% round(2)
  out2$uci=with(out2,Estimate+1.96*`Std. Error`) %>% round(2)
  out2}
func.res2<-function(out){
  out2<-as.data.frame(t(summary(out)$coeff[2,]))
  out2$OR =with(out2,exp(Estimate)) %>% round(2)
  out2$lci=with(out2,exp(Estimate-1.96*`Std. Error`)) %>% round(2)
  out2$uci=with(out2,exp(Estimate+1.96*`Std. Error`)) %>% round(2)
  out2}

a1.fit_lag0<-rbind(func.res1(a1.fit1_lag0),func.res1(a1.fit2_lag0),func.res1(a1.fit3_lag0),func.res1(a1.fit4_lag0),func.res1(a1.fit5_lag0),func.res1(a1.fit6_lag0)) %>% as.data.frame
a2.fit_lag0<-rbind(func.res1(a2.fit1_lag0),func.res1(a2.fit2_lag0),func.res1(a2.fit3_lag0),func.res1(a2.fit4_lag0),func.res1(a2.fit5_lag0),func.res1(a2.fit6_lag0)) %>% as.data.frame
b1.fit_lag0<-rbind(func.res2(b1.fit1_lag0),func.res2(b1.fit2_lag0),func.res2(b1.fit3_lag0),func.res2(b1.fit4_lag0),func.res2(b1.fit5_lag0),func.res2(b1.fit6_lag0)) %>% as.data.frame
b2.fit_lag0<-rbind(func.res2(b2.fit1_lag0),func.res2(b2.fit2_lag0),func.res2(b2.fit3_lag0),func.res2(b2.fit4_lag0),func.res2(b2.fit5_lag0),func.res2(b2.fit6_lag0)) %>% as.data.frame
 
a1.fit_lag1<-rbind(func.res1(a1.fit1_lag1),func.res1(a1.fit2_lag1),func.res1(a1.fit3_lag1),func.res1(a1.fit4_lag1),func.res1(a1.fit5_lag1),func.res1(a1.fit6_lag1)) %>% as.data.frame
a2.fit_lag1<-rbind(func.res1(a2.fit1_lag1),func.res1(a2.fit2_lag1),func.res1(a2.fit3_lag1),func.res1(a2.fit4_lag1),func.res1(a2.fit5_lag1),func.res1(a2.fit6_lag1)) %>% as.data.frame
b1.fit_lag1<-rbind(func.res2(b1.fit1_lag1),func.res2(b1.fit2_lag1),func.res2(b1.fit3_lag1),func.res2(b1.fit4_lag1),func.res2(b1.fit5_lag1),func.res2(b1.fit6_lag1)) %>% as.data.frame
b2.fit_lag1<-rbind(func.res2(b2.fit1_lag1),func.res2(b2.fit2_lag1),func.res2(b2.fit3_lag1),func.res2(b2.fit4_lag1),func.res2(b2.fit5_lag1),func.res2(b2.fit6_lag1)) %>% as.data.frame

a1.fit_lag01<-rbind(func.res1(a1.fit1_lag01),func.res1(a1.fit2_lag01),func.res1(a1.fit3_lag01),func.res1(a1.fit4_lag01),func.res1(a1.fit5_lag01),func.res1(a1.fit6_lag01)) %>% as.data.frame
a2.fit_lag01<-rbind(func.res1(a2.fit1_lag01),func.res1(a2.fit2_lag01),func.res1(a2.fit3_lag01),func.res1(a2.fit4_lag01),func.res1(a2.fit5_lag01),func.res1(a2.fit6_lag01)) %>% as.data.frame
b1.fit_lag01<-rbind(func.res2(b1.fit1_lag01),func.res2(b1.fit2_lag01),func.res2(b1.fit3_lag01),func.res2(b1.fit4_lag01),func.res2(b1.fit5_lag01),func.res2(b1.fit6_lag01)) %>% as.data.frame
b2.fit_lag01<-rbind(func.res2(b2.fit1_lag01),func.res2(b2.fit2_lag01),func.res2(b2.fit3_lag01),func.res2(b2.fit4_lag01),func.res2(b2.fit5_lag01),func.res2(b2.fit6_lag01)) %>% as.data.frame

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#subgroup analysis




#-----------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------#
#Two-pollutant
t1<-svyglm(eGFR_epi~PM25_lag0_r+PM10_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t2<-svyglm(eGFR_epi~PM25_lag0_r+SO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t3<-svyglm(eGFR_epi~PM25_lag0_r+NO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t4<-svyglm(eGFR_epi~PM25_lag0_r+CO_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t5<-svyglm(eGFR_epi~PM25_lag0_r+O3_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)

tt_pm25<-as.data.frame(rbind(summary(t1)$coeff[2,],summary(t2)$coeff[2,],
                             summary(t3)$coeff[2,],summary(t4)$coeff[2,],summary(t5)$coeff[2,]))

t1<-svyglm(eGFR_epi~PM10_lag0_r+PM25_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t2<-svyglm(eGFR_epi~PM10_lag0_r+SO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t3<-svyglm(eGFR_epi~PM10_lag0_r+NO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t4<-svyglm(eGFR_epi~PM10_lag0_r+CO_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t5<-svyglm(eGFR_epi~PM10_lag0_r+O3_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)

tt_pm10<-as.data.frame(rbind(summary(t1)$coeff[2,],summary(t2)$coeff[2,],
                             summary(t3)$coeff[2,],summary(t4)$coeff[2,],summary(t5)$coeff[2,]))

t1<-svyglm(eGFR_epi~SO2_lag0_r+PM25_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t2<-svyglm(eGFR_epi~SO2_lag0_r+PM10_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t3<-svyglm(eGFR_epi~SO2_lag0_r+NO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t4<-svyglm(eGFR_epi~SO2_lag0_r+CO_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t5<-svyglm(eGFR_epi~SO2_lag0_r+O3_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)

tt_so2<-as.data.frame(rbind(summary(t1)$coeff[2,],summary(t2)$coeff[2,],
                            summary(t3)$coeff[2,],summary(t4)$coeff[2,],summary(t5)$coeff[2,]))

t1<-svyglm(eGFR_epi~NO2_lag0_r+PM25_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t2<-svyglm(eGFR_epi~NO2_lag0_r+PM10_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t3<-svyglm(eGFR_epi~NO2_lag0_r+SO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t4<-svyglm(eGFR_epi~NO2_lag0_r+CO_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t5<-svyglm(eGFR_epi~NO2_lag0_r+O3_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)

tt_no2<-as.data.frame(rbind(summary(t1)$coeff[2,],summary(t2)$coeff[2,],
                            summary(t3)$coeff[2,],summary(t4)$coeff[2,],summary(t5)$coeff[2,]))


t1<-svyglm(eGFR_epi~CO_lag0_r+PM25_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t2<-svyglm(eGFR_epi~CO_lag0_r+PM10_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t3<-svyglm(eGFR_epi~CO_lag0_r+SO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t4<-svyglm(eGFR_epi~CO_lag0_r+NO2_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t5<-svyglm(eGFR_epi~CO_lag0_r+O3_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)

tt_co<-as.data.frame(rbind(summary(t1)$coeff[2,],summary(t2)$coeff[2,],
                           summary(t3)$coeff[2,],summary(t4)$coeff[2,],summary(t5)$coeff[2,]))


t1<-svyglm(eGFR_epi~O3_lag0_r+PM25_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t2<-svyglm(eGFR_epi~O3_lag0_r+PM10_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t3<-svyglm(eGFR_epi~O3_lag0_r+SO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t4<-svyglm(eGFR_epi~O3_lag0_r+NO2_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)
t5<-svyglm(eGFR_epi~O3_lag0_r+CO_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy)

tt_o3<-as.data.frame(rbind(summary(t1)$coeff[2,],summary(t2)$coeff[2,],
                           summary(t3)$coeff[2,],summary(t4)$coeff[2,],summary(t5)$coeff[2,]))


tt_pm25$exposure="PM[2.5]";tt_pm25$adjusted=c("+PM[10]","+SO[2]","+NO[2]","+CO","+O[3]")
tt_pm10$exposure="PM[10]" ;tt_pm10$adjusted=c("+PM[2.5]","+SO[2]","+NO[2]","+CO","+O[3]")
tt_so2 $exposure="SO[2]"  ;tt_so2 $adjusted=c("+PM[2.5]","+PM[10]","+NO[2]","+CO","+O[3]")
tt_no2 $exposure="NO[2]"  ;tt_no2 $adjusted=c("+PM[2.5]","+PM[10]","+SO[2]","+CO","+O[3]")
tt_co  $exposure="CO"     ;tt_co  $adjusted=c("+PM[2.5]","+PM[10]","+SO[2]","+NO[2]","+O[3]")
tt_o3  $exposure="O[3]"   ;tt_o3  $adjusted=c("+PM[2.5]","+PM[10]","+SO[2]","+NO[2]","+CO")

two_poll_linear<-rbind(tt_pm25,tt_pm10,tt_so2,tt_no2,tt_co,tt_o3)
two_poll_linear$lci=two_poll_linear$Estimate-1.96*two_poll_linear$`Std. Error`
two_poll_linear$uci=two_poll_linear$Estimate+1.96*two_poll_linear$`Std. Error`

two_poll_linear$exposure=factor(two_poll_linear$exposure,levels=unique(two_poll_linear$exposure))
two_poll_linear$adjusted=factor(two_poll_linear$adjusted,levels=unique(two_poll_linear$adjusted))

# write.csv(two_poll_linear,file="D:\\EUMC\\논문\\연구논문\\PM2.5_Kidney_disease\\two_poll_linear.csv",row.names=F,na="")

two_poll_linear<-read_excel("D:\\EUMC\\논문\\연구논문\\PM2.5_Kidney_disease\\1.KNHANES_AP\\data\\KNHANES_Kidney_disease.xlsx",sheet="multi_linear")

two_poll_linear$exposure=factor(two_poll_linear$exposure,levels=unique(two_poll_linear$exposure))
two_poll_linear$adjusted=factor(two_poll_linear$adjusted,levels=unique(two_poll_linear$adjusted))


two_poll_linear2<-two_poll_linear %>% filter(!(exposure=="PM[2.5]" & adjusted=="+PM[10]" | 
                                                 exposure=="PM[10]"  & adjusted=="+PM[2.5]" | 
                                                 exposure=="NO[2]"   & adjusted=="+O[3]"  |
                                                 exposure=="O[3]"    & adjusted=="+NO[2]"))


two_poll_linear2$exposure=factor(two_poll_linear2$exposure,levels=unique(two_poll_linear2$exposure))
two_poll_linear2$adjusted=factor(two_poll_linear2$adjusted,levels=c("Unadjusted","+PM[2.5]","+PM[10]",
                                                                    "+SO[2]","+NO[2]","+CO","+O[3]"))


x11();ggplot(two_poll_linear2,aes(adjusted,Estimate))+geom_point(size=4.5)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,lwd=1.1)+
  facet_wrap(.~exposure,scales="free",labeller = label_parsed)+
  theme_gray(base_size = 20)+geom_hline(yintercept=0,col="red")+
  labs(x="",y="Esimate (95% Confidence intervals)")+
  scale_x_discrete(breaks=levels(factor(two_poll_linear2$adjusted)),
                   labels = c("Unadjusted",
                              expression(+PM[2.5]),
                              expression(+PM[10]),
                              expression(+SO[2]),
                              expression(+NO[2]),
                              expression(+CO),
                              expression(+O[3])))

ggplot(two_poll_linear2,aes(adjusted,Estimate))+geom_point(size=4.5)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,lwd=1.1)+
  facet_wrap(.~exposure,scales="free",labeller = label_parsed)+
  theme_gray(base_size = 20)+geom_hline(yintercept=0,col="red")+
  labs(x="",y="Esimate (95% Confidence intervals)")+
  scale_x_discrete(breaks=levels(factor(two_poll_linear2$adjusted)),
                   labels = c("Unadjusted",
                              expression(+PM[2.5]),
                              expression(+PM[10]),
                              expression(+SO[2]),
                              expression(+NO[2]),
                              expression(+CO),
                              expression(+O[3])))

ggsave("D:\\EUMC\\논문\\연구논문\\PM2.5_Kidney_disease\\1.KNHANES_AP\\Figure\\Fig 2.tiff",
       width=20,height=10,dpi=300)


#-----------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------#
#Two-pollutant -logit

t1<-svyglm(eGFR_g2~PM25_lag0_r+PM10_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t2<-svyglm(eGFR_g2~PM25_lag0_r+SO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t3<-svyglm(eGFR_g2~PM25_lag0_r+NO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t4<-svyglm(eGFR_g2~PM25_lag0_r+CO_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t5<-svyglm(eGFR_g2~PM25_lag0_r+O3_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")

tt_pm25<-as.data.frame(rbind(summary(t1)$coeff[2,],summary(t2)$coeff[2,],
                             summary(t3)$coeff[2,],summary(t4)$coeff[2,],summary(t5)$coeff[2,]))

t1<-svyglm(eGFR_g2~PM10_lag0_r+PM25_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t2<-svyglm(eGFR_g2~PM10_lag0_r+SO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t3<-svyglm(eGFR_g2~PM10_lag0_r+NO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t4<-svyglm(eGFR_g2~PM10_lag0_r+CO_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t5<-svyglm(eGFR_g2~PM10_lag0_r+O3_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")

tt_pm10<-as.data.frame(rbind(summary(t1)$coeff[2,],summary(t2)$coeff[2,],
                             summary(t3)$coeff[2,],summary(t4)$coeff[2,],summary(t5)$coeff[2,]))

t1<-svyglm(eGFR_g2~SO2_lag0_r+PM25_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t2<-svyglm(eGFR_g2~SO2_lag0_r+PM10_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t3<-svyglm(eGFR_g2~SO2_lag0_r+NO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t4<-svyglm(eGFR_g2~SO2_lag0_r+CO_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t5<-svyglm(eGFR_g2~SO2_lag0_r+O3_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")

tt_so2<-as.data.frame(rbind(summary(t1)$coeff[2,],summary(t2)$coeff[2,],
                            summary(t3)$coeff[2,],summary(t4)$coeff[2,],summary(t5)$coeff[2,]))

t1<-svyglm(eGFR_g2~NO2_lag0_r+PM25_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t2<-svyglm(eGFR_g2~NO2_lag0_r+PM10_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t3<-svyglm(eGFR_g2~NO2_lag0_r+SO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t4<-svyglm(eGFR_g2~NO2_lag0_r+CO_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t5<-svyglm(eGFR_g2~NO2_lag0_r+O3_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")

tt_no2<-as.data.frame(rbind(summary(t1)$coeff[2,],summary(t2)$coeff[2,],
                            summary(t3)$coeff[2,],summary(t4)$coeff[2,],summary(t5)$coeff[2,]))


t1<-svyglm(eGFR_g2~CO_lag0_r+PM25_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t2<-svyglm(eGFR_g2~CO_lag0_r+PM10_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t3<-svyglm(eGFR_g2~CO_lag0_r+SO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t4<-svyglm(eGFR_g2~CO_lag0_r+NO2_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t5<-svyglm(eGFR_g2~CO_lag0_r+O3_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")

tt_co<-as.data.frame(rbind(summary(t1)$coeff[2,],summary(t2)$coeff[2,],
                           summary(t3)$coeff[2,],summary(t4)$coeff[2,],summary(t5)$coeff[2,]))


t1<-svyglm(eGFR_g2~O3_lag0_r+PM25_lag0_r+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t2<-svyglm(eGFR_g2~O3_lag0_r+PM10_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t3<-svyglm(eGFR_g2~O3_lag0_r+SO2_lag0_r +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t4<-svyglm(eGFR_g2~O3_lag0_r+NO2_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")
t5<-svyglm(eGFR_g2~O3_lag0_r+CO_lag0_r  +age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+drink+smoking,data=hn.r,com.svy,family="quasibinomial")

tt_o3<-as.data.frame(rbind(summary(t1)$coeff[2,],summary(t2)$coeff[2,],
                           summary(t3)$coeff[2,],summary(t4)$coeff[2,],summary(t5)$coeff[2,]))


tt_pm25$exposure="PM[2.5]";tt_pm25$adjusted=c("+PM[10]","+SO[2]","+NO[2]","+CO","+O[3]")
tt_pm10$exposure="PM[10]" ;tt_pm10$adjusted=c("+PM[2.5]","+SO[2]","+NO[2]","+CO","+O[3]")
tt_so2 $exposure="SO[2]"  ;tt_so2 $adjusted=c("+PM[2.5]","+PM[10]","+NO[2]","+CO","+O[3]")
tt_no2 $exposure="NO[2]"  ;tt_no2 $adjusted=c("+PM[2.5]","+PM[10]","+SO[2]","+CO","+O[3]")
tt_co  $exposure="CO"     ;tt_co  $adjusted=c("+PM[2.5]","+PM[10]","+SO[2]","+NO[2]","+O[3]")
tt_o3  $exposure="O[3]"   ;tt_o3  $adjusted=c("+PM[2.5]","+PM[10]","+SO[2]","+NO[2]","+CO")

two_poll_logit<-rbind(tt_pm25,tt_pm10,tt_so2,tt_no2,tt_co,tt_o3)
two_poll_logit$OR=exp(two_poll_logit$Estimate)
two_poll_logit$lci=exp(two_poll_logit$Estimate-1.96*two_poll_logit$`Std. Error`)
two_poll_logit$uci=exp(two_poll_logit$Estimate+1.96*two_poll_logit$`Std. Error`)

two_poll_logit$exposure=factor(two_poll_logit$exposure,levels=unique(two_poll_logit$exposure))
two_poll_logit$adjusted=factor(two_poll_logit$adjusted,levels=unique(two_poll_logit$adjusted))

# write.csv(two_poll_logit,file="D:\\EUMC\\논문\\연구논문\\PM2.5_Kidney_disease\\two_poll_logit.csv",row.names=F,na="")

two_poll_logit<-read_excel("D:\\EUMC\\논문\\연구논문\\PM2.5_Kidney_disease\\1.KNHANES_AP\\data\\KNHANES_Kidney_disease.xlsx",sheet="multi_logit")

two_poll_logit$exposure=factor(two_poll_logit$exposure,levels=unique(two_poll_logit$exposure))
two_poll_logit$adjusted=factor(two_poll_logit$adjusted,levels=unique(two_poll_logit$adjusted))

two_poll_logit2<-two_poll_logit %>% filter(!(exposure=="PM[2.5]" & adjusted=="+PM[10]" | 
                                                 exposure=="PM[10]"  & adjusted=="+PM[2.5]" | 
                                                 exposure=="NO[2]"   & adjusted=="+O[3]"  |
                                                 exposure=="O[3]"    & adjusted=="+NO[2]"))


two_poll_logit2$exposure=factor(two_poll_logit2$exposure,levels=unique(two_poll_logit2$exposure))
two_poll_logit2$adjusted=factor(two_poll_logit2$adjusted,levels=c("Unadjusted","+PM[2.5]","+PM[10]",
                                                                    "+SO[2]","+NO[2]","+CO","+O[3]"))


x11();ggplot(two_poll_logit2,aes(adjusted,OR))+geom_point(size=4.5)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,lwd=1.1)+
  facet_wrap(.~exposure,scales="free",labeller = label_parsed)+
  theme_gray(base_size = 20)+geom_hline(yintercept=1,col="red")+
  labs(x="",y="Odds ratio (95% Confidence intervals)")+
  scale_x_discrete(breaks=levels(factor(two_poll_logit2$adjusted)),
                   labels = c("Unadjusted",
                              expression(+PM[2.5]),
                              expression(+PM[10]),
                              expression(+SO[2]),
                              expression(+NO[2]),
                              expression(+CO),
                              expression(+O[3])))

ggplot(two_poll_logit2,aes(adjusted,OR))+geom_point(size=4.5)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,lwd=1.1)+
  facet_wrap(.~exposure,scales="free",labeller = label_parsed)+
  theme_gray(base_size = 20)+geom_hline(yintercept=1,col="red")+
  labs(x="",y="Odds ratio (95% Confidence intervals)")+
  scale_x_discrete(breaks=levels(factor(two_poll_logit2$adjusted)),
                   labels = c("Unadjusted",
                              expression(+PM[2.5]),
                              expression(+PM[10]),
                              expression(+SO[2]),
                              expression(+NO[2]),
                              expression(+CO),
                              expression(+O[3])))

ggsave("D:\\EUMC\\논문\\연구논문\\PM2.5_Kidney_disease\\1.KNHANES_AP\\Figure\\Fig 3.tiff",
       width=20,height=10,dpi=300)
