#library
library(readxl)    ;library(ggplot2)
library(lubridate) ;library(dplyr)
library(stringr)   ;library(Epi)
library(survival)

#working directory
setwd("D:\\EUMC\\논문\\연구논문\\PM2.5_Kawasaki_disease\\data\\EUMC_EMR")

#면역글로불린 G (IgG)는 항체 개별형 중 하나
#동일한 중쇄 두 개와 경쇄 두개로 된 단량체이고, 일반적인 항체의 형태인 Y혀으로 배열되어 있음
#각 IgG에는 항원 결합자리 두개가 있음, 사람의 혈청에 있는 면역글로불린 총량 중 75%를 차지

#면역글로불린 주사 맞은 사람만 포함시키면 진짜 가와사끼
#의심될 경우도 진단명을 넣음 

#원인 미상, 합병증이 관상동맥확장 (coronary aneurysm)

#우리나 같은 경우 초여름, 겨울에 호발한다고 알려져있음
#알려져 있는 관련 바이러스: enterovirus, adenovirus, human rhinovirus, coronavirus
#처방코드 WIVG

#Data exploration
dat<-as.data.frame(read_excel("PGE_M303_Kawasaki_2000_2020_3차_pw_out.xlsx",sheet=1))

#number of cases : raw data
nrow(dat)              #4,191

subset(dat,year %in% c(NA,2006:2016) )$year %>% length

dat<-subset(dat,year %in% c(NA,2006:2016) )

unique(dat$ID) %>% length

#number of patients : raw data
length(unique(dat$ID)) #2,617
dat %>% select(ID,age,admission_date,discharge_date) %>%  arrange(ID,admission_date,desc(discharge_date)) %>% View

addmargins(table(dat$IVIG_prescription))
table(dat$IVIG_prescription) %>% sum
#VENOBULIN 
#IV Globulin-S  아이비글로불린에스엔주 (사람 면역글로불린주)
#IV Globulin-SN
table(dat$IVIG_Prescription_note) %>% View
###############################################################################################
###############################################################################################
#date format
age_df         =as.data.frame(do.call(rbind,lapply(strsplit(dat$age," "),rbind)))
dat$age_year   =as.numeric(gsub("세","",age_df$V1))
dat$age_mon    =as.numeric(gsub("개월","",age_df$V2))
dat$age_mon.t  =with(dat,age_year*12+age_month)

#상병 시작일 / 상병 종료일 
dat$start_date=ymd(dat$start_date)
dat$end_date  =ymd(dat$end_date)  # 하나 이상하게 표기 ("-"로 표기 되어있음)

#입원 시작일 / 입원 종료일
dat$admission_date=ymd(dat$admission_date)
dat$discharge_date=ymd(dat$discharge_date)

#애는 입원일 기준으로 
dat$year      =year(dat$admission_date)
dat$month     =month(dat$admission_date)
dat$day       =day(dat$admission_date)
dat$dow       =weekdays(dat$admission_date)

dat$SIDO=substr(dat$address,1,2)
dat$event=1
dat$date=dat$admission_date

#거주지 주소, 시도,행정구 단위로 
sido.l=NULL
gu.l  =NULL
for(i in 1:nrow(dat)){
  a<-strsplit(dat$address," ")[[i]]
  si  <-a[1]
  gu  <-a[2]
  sido.l[i]<-si
  gu.l[i]  <-gu
  print(i)}

dat$SIDO %>% table
dat$SGG=gu.l

#변수 순서 정렬 , 보기 편할려고 
dat<-dat %>% select(ID,SIDO,SGG,sex,age_year,year,month,day,dow,date,event,admission_date,discharge_date,start_date:IVIG_prescription_start_date,
                    `RV-PCR_prescription`:I254,age_mon,age_mon.t,address,postal_code) %>% arrange(ID,age_year,admission_date,desc(discharge_date))

dat<-read.csv("D:\\EUMC\\논문\\연구논문\\PM2.5_Kawasaki_disease\\data\\EUMC_EMR\\EUMC_Kawasaki.csv")
###############################################################################################
###############################################################################################
#CMAQ PM2.5 data
pm25.rr<-read.csv("D:\\EUMC\\논문\\연구논문\\PM2.5_Kawasaki_disease\\data\\EUMC_EMR\\CMAQ_PM25_rev.csv")
aggregate(pm25.rr$key,list(pm25.rr$key),length) %>% View

pm25.rr<-pm25.rr %>% dplyr:: select(-c(date,SIDO,SIGUNGU,key2))

###############################################################################################
###############################################################################################
#Air pollution data
setwd("D:\\EUMC\\논문\\연구논문\\PM2.5_Kawasaki_disease\\data")
ap2001_2019<-read.csv('ap2001_2019.csv')

###############################################################################################
###############################################################################################
#Meteorological data
setwd("D:\\EUMC\\논문\\연구논문\\PM2.5_Kawasaki_disease\\data")
wea_2000_2020<-read.csv("D:\\EUMC\\논문\\연구논문\\PM2.5_Kawasaki_disease\\data\\EUMC_EMR\\wea_2000_2020.csv",header=T)

###############################################################################################
###############################################################################################
#외래/입원 불분명
#Method1: 5세 이하, Case 정의하지 않고 주어진 자료 그대로 & KD 시점은 start_date 기준
setwd("D:\\EUMC\\논문\\연구논문\\PM2.5_Kawasaki_disease\\data\\EUMC_EMR")

#조건 1 : 같은사람이 같은날와서 같은날 퇴원한 경우 제외 (중복 자료 제외)

dat$def1=with(dat,paste0(ID,"-",admission_date,"-",discharge_date))
dat.c1<-dat[!duplicated(dat$def1),] %>% arrange(ID,age_year,admission_date,desc(discharge_date)) # 4,191 -> 3,214

dat.c1 %>% select(ID,admission_date,discharge_date) %>% arrange(ID,admission_date,desc(discharge_date)) %>%  View

nrow(dat)
nrow(dat.c1)

dat.c1$ID %>% unique %>% length

#조건 2: 면역글로불린 처방을 받은 환자 
is.na(dat.c1$IVIG_prescription) %>% table
is.na(dat.c1$IVIG_Prescription_note) %>% table

dat.c1$IVIG_prescription %>% table
dat.c1$IVIG_Prescription_note %>% table


#VENOBULIN 관련된 코드는 관련성 거의 없다고 보면됨 
dat.c2<-dat.c1[!is.na(dat.c1$IVIG_prescription),]%>% arrange(ID,age_year,admission_date,desc(discharge_date))
dat.c3<-dat.c2[-unique(c(grep("VENOBULIN",dat.c2$IVIG_Prescription_note),grep("녹십자",dat.c2$IVIG_Prescription_note))),]

nrow(dat.c2)
nrow(dat.c3)

dat.c4<-dat.c3[unique(grep("WIVG",dat.c3$IVIG_prescription)),]

table(dat.c4$IVIG_prescription)
nrow(dat.c2) #3,048
nrow(dat.c3) #1,081
nrow(dat.c4) #947

dat.c4$ID %>% unique %>% length

#조건 3: 성, 연령, 거주지 정보 없는 경우 제외 
dat.c4$year
dat.c5<-dat.c4[complete.cases(dat.c4 %>% select(SIDO,SGG,sex,age_year,year,admission_date)),]

dat.c5$ID %>% unique %>% length

#연도는 2001~2017까지 (PM2.5 제외로 볼 때, PM10, SO2, NO2, CO)
#연령은 10세 까지 해서 0-1,2-5,6-10세 층화 

dat.c6<-subset(dat.c5,age_year<=10) 
dat.c6<-subset(dat.c6,year<=2017) %>%  arrange(ID,age_year,admission_date,desc(discharge_date))

nrow(dat.c6)

dat.c6$ID %>% unique %>% length
nrow(dat.c6)
table(dat.c6$year)
table(dat.c6$sex)
table(dat.c6$SIDO)
table(dat.c6$age_year)

dat.c7<-subset(dat.c6,SIDO %in% c("서울","경기","인천"))

dat.c7$admission_date=ymd(dat.c7$admission_date)
dat.c7$date=ymd(dat.c7$admission_date)

dat.c7$ID %>% unique %>% length

dat.c7<-subset(dat.c7,year>=2006)
dat.c7<-subset(dat.c7,year<=2016)

View(dat.c7)

nrow(dat.c7)

table(dat.c7$age_year)
table(dat.c7$hospital)

#Time-stratified (시간 층화하여 자료 셋 구성)
#code 알면 수정, 몰라서 for문으로 작성 
#년도, 월 동일하고 같은 요일에 해당하면서 주만 다르게
#기준은 발생일 기준 전후로 4주까지 설정하여 control 군이 총 3~4개

m1=NULL
for(i in 1:nrow(dat.c7)){
  a1<-dat.c7[i,]; a1$date=a1$date-28
  a2<-dat.c7[i,]; a2$date=a2$date-21
  a3<-dat.c7[i,]; a3$date=a3$date-14
  a4<-dat.c7[i,]; a4$date=a4$date-7
  a5<-dat.c7[i,]; a5$date=a5$date-0
  a6<-dat.c7[i,]; a6$date=a6$date+7
  a7<-dat.c7[i,]; a7$date=a7$date+14
  a8<-dat.c7[i,]; a8$date=a8$date+21
  a9<-dat.c7[i,]; a9$date=a9$date+28
  
  aa<-rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9)
  aa$TF=with(aa,ifelse(ifelse(year==year(date),T,F) & ifelse(month==month(date),T,F)==T,T,F))
  
  b<-filter(aa,TF==T)
  b$event=with(b,ifelse(ifelse(admission_date==date,T,F)==T,1,0))
  m1[[i]]<-b
  
  print(i)}

m1.rev<-as.data.frame(do.call(rbind,m1))


# write.csv(m1.rev,file="Method1.csv",row.names = F,na="")
# m1.rev<-read.csv('Method1.csv',header=T)

m1.rev$date=ymd(m1.rev$date)

#case:1, control: 3~4,
table(aggregate(m1.rev$def1,list(m1.rev$def1),length)[,2]-1)
###############################################################################################

subset(m1.rev,event==1) %>% nrow

m1.rev<-m1.rev %>% arrange(ID,date)

m1.rev$key =with(m1.rev,paste0(date,"-",SIDO,"-",SGG))
m1.rev$key2=with(m1.rev,paste0(date,"-",SIDO))

#기상변수 연계
m1.rev2<-merge(m1.rev,wea_2000_2020,by="key2",all.x=T)

#대기오염 연계
# ap2001_2019$date=ymd(ap2001_2019$yymmdd)
m1.rev3<-merge(m1.rev2,pm25.rr,by="key")
m1.rev3<-m1.rev3 %>% select(ID:lag014_PM25,key,key2) %>% arrange(ID,admission_date)

# ap2001_2019$year=year(ymd(ap2001_2019$yymmdd))
# ap2005_2017<-subset(ap2001_2019,year>=2005 & year<=2017)
# iqr1<-IQR(ap2005_2017$PM10,na.rm=T)
# iqr2<-IQR(ap2005_2017$SO2,na.rm=T)
# iqr3<-IQR(ap2005_2017$NO2,na.rm=T)
# iqr4<-IQR(ap2005_2017$CO,na.rm=T)
# iqr5<-IQR(ap2005_2017$O3,na.rm=T)

# final set
raw<-m1.rev3
subset(raw,event==1) %>% nrow
with(raw,aggregate(raw$event,list(raw$SIDO),table))

table(raw$year)
table(raw$age_year)
table(raw$sex)
table(raw$SIDO)

with(raw,aggregate(PM25,list(SIDO,event),mean))
with(raw,aggregate(PM25,list(SIDO,event),sd))

m1.s1<-subset(raw,SIDO %in% c("경기"))
m1.s2<-subset(raw,SIDO %in% c("서울"))
m1.s3<-subset(raw,SIDO %in% c("인천"))

m1.s1.e0<-subset(m1.s1,event==0)
m1.s1.e1<-subset(m1.s1,event==1)
m1.s2.e0<-subset(m1.s2,event==0)
m1.s2.e1<-subset(m1.s2,event==1)
m1.s3.e0<-subset(m1.s3,event==0)
m1.s3.e1<-subset(m1.s3,event==1)

t.test(m1.s1.e0$PM25,m1.s1.e1$PM25)
t.test(m1.s2.e0$PM25,m1.s2.e1$PM25)
t.test(m1.s3.e0$PM25,m1.s3.e1$PM25)

summary(raw$PM25)  ;sd(raw$PM25,na.rm=T)  ;IQR(raw$PM25,na.rm=T)
summary(m1.s1$PM25);sd(m1.s1$PM25,na.rm=T);IQR(m1.s1$PM25,na.rm=T)
summary(m1.s2$PM25);sd(m1.s2$PM25,na.rm=T);IQR(m1.s2$PM25,na.rm=T)
summary(m1.s3$PM25);sd(m1.s3$PM25,na.rm=T);IQR(m1.s3$PM25,na.rm=T)

summary(raw$meantemp)  ;sd(raw$meantemp,na.rm=T)  ;IQR(raw$meantemp,na.rm=T)
summary(m1.s1$meantemp);sd(m1.s1$meantemp,na.rm=T);IQR(m1.s1$meantemp,na.rm=T)
summary(m1.s2$meantemp);sd(m1.s2$meantemp,na.rm=T);IQR(m1.s2$meantemp,na.rm=T)
summary(m1.s3$meantemp);sd(m1.s3$meantemp,na.rm=T);IQR(m1.s3$meantemp,na.rm=T)

summary(raw$meanhumi)  ;sd(raw$meanhumi,na.rm=T)  ;IQR(raw$meanhumi,na.rm=T)
summary(m1.s1$meanhumi);sd(m1.s1$meanhumi,na.rm=T);IQR(m1.s1$meanhumi,na.rm=T)
summary(m1.s2$meanhumi);sd(m1.s2$meanhumi,na.rm=T);IQR(m1.s2$meanhumi,na.rm=T)
summary(m1.s3$meanhumi);sd(m1.s3$meanhumi,na.rm=T);IQR(m1.s3$meanhumi,na.rm=T)

head(m1.rev3)
z<-subset(m1.rev3,SIDO=="서울" & year>=2015)
z

head(ap2001_2019)

ap2001_2019$key2=paste0(ap2001_2019$yymmdd,"-서울")

mpm<-ap2001_2019 %>% select(key2,PM25,yymmdd)

mpm<-subset(mpm,substr(mpm$yymmdd,1,4)>=2014)

names(mpm)[2]="MPM25"
mpm$lag1_mpm<-lag(mpm$MPM25,1)
mpm$lag2_mpm<-lag(mpm$MPM25,2)
mpm$lag3_mpm<-lag(mpm$MPM25,3)
mpm$lag4_mpm<-lag(mpm$MPM25,4)
mpm$lag5_mpm<-lag(mpm$MPM25,5)
mpm$lag6_mpm<-lag(mpm$MPM25,6)
mpm$lag7_mpm<-lag(mpm$MPM25,7)
mpm$lag8_mpm<-lag(mpm$MPM25,8)
mpm$lag9_mpm<-lag(mpm$MPM25,9)
mpm$lag10_mpm<-lag(mpm$MPM25,10)
mpm$lag11_mpm<-lag(mpm$MPM25,11)
mpm$lag12_mpm<-lag(mpm$MPM25,12)
mpm$lag13_mpm<-lag(mpm$MPM25,13)
mpm$lag14_mpm<-lag(mpm$MPM25,14)

mpm$lag01_mpm<-apply(mpm %>% select(MPM25,lag1_mpm),1,mean,na.rm=T)
mpm$lag02_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag2_mpm),1,mean,na.rm=T)
mpm$lag03_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag3_mpm),1,mean,na.rm=T)
mpm$lag04_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag4_mpm),1,mean,na.rm=T)
mpm$lag05_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag5_mpm),1,mean,na.rm=T)
mpm$lag06_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag6_mpm),1,mean,na.rm=T)
mpm$lag07_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag7_mpm),1,mean,na.rm=T)
mpm$lag08_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag8_mpm),1,mean,na.rm=T)
mpm$lag09_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag9_mpm),1,mean,na.rm=T)
mpm$lag010_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag10_mpm),1,mean,na.rm=T)
mpm$lag011_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag11_mpm),1,mean,na.rm=T)
mpm$lag012_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag12_mpm),1,mean,na.rm=T)
mpm$lag013_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag13_mpm),1,mean,na.rm=T)
mpm$lag014_mpm<-apply(mpm %>% select(MPM25,lag1_mpm:lag14_mpm),1,mean,na.rm=T)
mpm<-subset(mpm,substr(mpm$yymmdd,1,4)>=2015)
View(mpm)
z2<-merge(z,mpm,by="key2",all.x=T)
z2

q1<-clogit(event~PM25     +lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)
q2<-clogit(event~lag01_PM25+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)
q3<-clogit(event~lag02_PM25+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)
q4<-clogit(event~lag03_PM25+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)
q5<-clogit(event~lag04_PM25+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)
q6<-clogit(event~lag05_PM25+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)
q7<-clogit(event~lag06_PM25+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)

w1<-clogit(event~MPM25+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)
w2<-clogit(event~lag01_mpm+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)
w3<-clogit(event~lag02_mpm+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)
w4<-clogit(event~lag03_mpm+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)
w5<-clogit(event~lag04_mpm+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)
w6<-clogit(event~lag05_mpm+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)
w7<-clogit(event~lag06_mpm+lag0_meantemp +lag0_meanhumi +(strata(ID)),data=z2)

q<-as.data.frame(rbind(summary(q1)$coeff[1,],
                       summary(q2)$coeff[1,],
                       summary(q3)$coeff[1,],
                       summary(q4)$coeff[1,],
                       summary(q5)$coeff[1,],
                       summary(q6)$coeff[1,],
                       summary(q7)$coeff[1,]))

w<-as.data.frame(rbind(summary(w1)$coeff[1,],
                       summary(w2)$coeff[1,],
                       summary(w3)$coeff[1,],
                       summary(w4)$coeff[1,],
                       summary(w5)$coeff[1,],
                       summary(w6)$coeff[1,],
                       summary(w7)$coeff[1,]))

q$OR =exp(q$coef*10)
q$lci=exp(10*q$coef-1.96*q$`se(coef)`*10)
q$uci=exp(10*q$coef+1.96*q$`se(coef)`*10)

w$OR =exp(w$coef*10)
w$lci=exp(10*w$coef-1.96*w$`se(coef)`*10)
w$uci=exp(10*w$coef+1.96*w$`se(coef)`*10)

x11()

######################################################################################################
######################################################################################################
#single lag PM25
fit0 <-clogit(event~PM25      +lag0_meantemp +lag0_meanhumi +(strata(ID)),data=m1.rev3)
fit1 <-clogit(event~lag1_PM25 +lag1_meantemp +lag1_meanhumi +(strata(ID)),data=m1.rev3)
fit2 <-clogit(event~lag2_PM25 +lag2_meantemp +lag2_meanhumi +(strata(ID)),data=m1.rev3)
fit3 <-clogit(event~lag3_PM25 +lag3_meantemp +lag3_meanhumi +(strata(ID)),data=m1.rev3)
fit4 <-clogit(event~lag4_PM25 +lag4_meantemp +lag4_meanhumi +(strata(ID)),data=m1.rev3)
fit5 <-clogit(event~lag5_PM25 +lag5_meantemp +lag5_meanhumi +(strata(ID)),data=m1.rev3)
fit6 <-clogit(event~lag6_PM25 +lag6_meantemp +lag6_meanhumi +(strata(ID)),data=m1.rev3)
fit7 <-clogit(event~lag7_PM25 +lag7_meantemp +lag7_meanhumi +(strata(ID)),data=m1.rev3)
fit8 <-clogit(event~lag8_PM25 +lag8_meantemp +lag8_meanhumi +(strata(ID)),data=m1.rev3)
fit9 <-clogit(event~lag9_PM25 +lag9_meantemp +lag9_meanhumi +(strata(ID)),data=m1.rev3)
fit10<-clogit(event~lag10_PM25+lag10_meantemp+lag10_meanhumi+(strata(ID)),data=m1.rev3)
fit11<-clogit(event~lag11_PM25+lag11_meantemp+lag11_meanhumi+(strata(ID)),data=m1.rev3)
fit12<-clogit(event~lag12_PM25+lag12_meantemp+lag12_meanhumi+(strata(ID)),data=m1.rev3)
fit13<-clogit(event~lag13_PM25+lag13_meantemp+lag13_meanhumi+(strata(ID)),data=m1.rev3)
fit14<-clogit(event~lag14_PM25+lag14_meantemp+lag14_meanhumi+(strata(ID)),data=m1.rev3)

result1.s<-as.data.frame(rbind(summary(fit0)$coeff[1,] ,summary(fit1)$coeff[1,] ,summary(fit2)$coeff[1,] ,summary(fit3)$coeff[1,],
                               summary(fit4)$coeff[1,] ,summary(fit5)$coeff[1,] ,summary(fit6)$coeff[1,] ,summary(fit7)$coeff[1,],
                               summary(fit8)$coeff[1,] ,summary(fit9)$coeff[1,] ,summary(fit10)$coeff[1,],summary(fit11)$coeff[1,],
                               summary(fit12)$coeff[1,],summary(fit13)$coeff[1,],summary(fit14)$coeff[1,]))

result1.s$`Pr(>|z|)`=round(result1.s$`Pr(>|z|)`,3)
result1.s$sign=ifelse(result1.s$`Pr(>|z|)`<0.05,"*","")

#moving avergae
fit1 <-clogit(event~lag01_PM25 +lag01_meantemp +lag01_meanhumi +(strata(ID)),data=m1.rev3)
fit2 <-clogit(event~lag02_PM25 +lag02_meantemp +lag02_meanhumi +(strata(ID)),data=m1.rev3)
fit3 <-clogit(event~lag03_PM25 +lag03_meantemp +lag03_meanhumi +(strata(ID)),data=m1.rev3)
fit4 <-clogit(event~lag04_PM25 +lag04_meantemp +lag04_meanhumi +(strata(ID)),data=m1.rev3)
fit5 <-clogit(event~lag05_PM25 +lag05_meantemp +lag05_meanhumi +(strata(ID)),data=m1.rev3)
fit6 <-clogit(event~lag06_PM25 +lag06_meantemp +lag06_meanhumi +(strata(ID)),data=m1.rev3)
fit7 <-clogit(event~lag07_PM25 +lag07_meantemp +lag07_meanhumi +(strata(ID)),data=m1.rev3)
fit8 <-clogit(event~lag08_PM25 +lag08_meantemp +lag08_meanhumi +(strata(ID)),data=m1.rev3)
fit9 <-clogit(event~lag09_PM25 +lag09_meantemp +lag09_meanhumi +(strata(ID)),data=m1.rev3)
fit10<-clogit(event~lag010_PM25+lag010_meantemp+lag010_meanhumi+(strata(ID)),data=m1.rev3)
fit11<-clogit(event~lag011_PM25+lag011_meantemp+lag011_meanhumi+(strata(ID)),data=m1.rev3)
fit12<-clogit(event~lag012_PM25+lag012_meantemp+lag012_meanhumi+(strata(ID)),data=m1.rev3)
fit13<-clogit(event~lag013_PM25+lag013_meantemp+lag013_meanhumi+(strata(ID)),data=m1.rev3)
fit14<-clogit(event~lag014_PM25+lag014_meantemp+lag014_meanhumi+(strata(ID)),data=m1.rev3)

result1.m<-as.data.frame(rbind(summary(fit1)$coeff[1,] ,summary(fit2)$coeff[1,] ,summary(fit3)$coeff[1,] ,summary(fit4)$coeff[1,],
                               summary(fit5)$coeff[1,] ,summary(fit6)$coeff[1,] ,summary(fit7)$coeff[1,] ,summary(fit8)$coeff[1,],
                               summary(fit9)$coeff[1,] ,summary(fit10)$coeff[1,],summary(fit11)$coeff[1,],summary(fit12)$coeff[1,],
                               summary(fit13)$coeff[1,],summary(fit14)$coeff[1,]))

result1.m$`Pr(>|z|)`=round(result1.m$`Pr(>|z|)`,3)
result1.m$sign=ifelse(result1.m$`Pr(>|z|)`<0.05,"*","")

#OR, CI 계산 
result1.s$OR=round(with(result1.s,exp(coef*10)),2)
result1.m$OR=round(with(result1.m,exp(coef*10)),2)

result1.s$LCI=round(with(result1.s,exp(coef*10-1.96*`se(coef)`*10)),2)
result1.m$LCI=round(with(result1.m,exp(coef*10-1.96*`se(coef)`*10)),2)

result1.s$UCI=round(with(result1.s,exp(coef*10+1.96*`se(coef)`*10)),2)
result1.m$UCI=round(with(result1.m,exp(coef*10+1.96*`se(coef)`*10)),2)

View(rbind(result1.s,result1.m))

result1.s$obs=1:15-1; result1.m$obs=paste0(0,"-",1:14)

#그림 예시로
result1.m$obs=factor(result1.m$obs,levels=unique(result1.m$obs))
x11();ggplot(result1.m,aes(obs,OR))+geom_point(size=3)+geom_errorbar(aes(ymin=LCI,ymax=UCI),lwd=0.9,width=0.3)+
  theme_gray(base_size=20)+labs(x="",y="Odds Ratio (95% Confidence interval)")+
  geom_hline(yintercept = 1,col="red",linetype=1)

x11();ggplot(result1.s,aes(obs,OR))+geom_point(size=3)+geom_errorbar(aes(ymin=LCI,ymax=UCI),lwd=0.9,width=0.3)+
  theme_gray(base_size=20)+labs(x="",y="Odds Ratio (95% Confidence interval)")+
  geom_hline(yintercept = 1,col="red",linetype=1)

d1<-subset(raw,SIDO %in% c("경기"))
d2<-subset(raw,SIDO %in% c("서울"))
d3<-subset(raw,SIDO %in% c("인천"))

d21<-subset(d1,age_year<=1)
d22<-subset(d1,age_year<5 & age_year>=1)
d23<-subset(d1,age_year<=10 & age_year>5)


f1<-clogit(event~lag04_PM25 +lag04_meantemp +lag04_meanhumi +(strata(ID)),data=d1)
f2<-clogit(event~lag04_PM25 +lag04_meantemp +lag04_meanhumi +(strata(ID)),data=d2)
f3<-clogit(event~lag04_PM25 +lag04_meantemp +lag04_meanhumi +(strata(ID)),data=d3)

re<-as.data.frame(rbind(summary(f1)$coeff[1,],
                        summary(f2)$coeff[1,],
                        summary(f3)$coeff[1,]))


re$OR=round(with(re,exp(coef*10)),2)
re$LCI=round(with(re,exp(coef*10-1.96*`se(coef)`*10)),2)
re$UCI=round(with(re,exp(coef*10+1.96*`se(coef)`*10)),2)

re$coef.r=re$coef*10
re$se    =re$`se(coef)`*10
library(metafor)

re$region=c("경기","서울","인천")

mytransf <- function(x)
  (exp(x))

re<-subset(re,region!="인천")

uni1<-with(re,rma(yi=coef.r,sei=se,slab=region,digits=3,method="REML"))

x11();forest(uni1,refline=1, bg=4, col=2,cex.lab=1.1,transf = mytransf,
             cex.axis=1.1,cex=1,fontsize=0.8,digits=2,main="",psize=1,mlab="RE Model for Total")

######################################################################################################
######################################################################################################
#single lag PM25

raw$season=ifelse(raw$month>=4 & raw$month<=9,"warm","cold")
# m1.rev3<-subset(raw,season=="cold")
raw$ag=ifelse(raw$age_year<1,1,
              ifelse(raw$age_year>4,3,2))
table(raw$ag)
table(raw$age_year)

raw$SIDO
d1<-subset(raw,SIDO=="경기")
d2<-subset(raw,SIDO=="서울")
d3<-subset(raw,SIDO=="인천")

# m1.rev3<-subset(raw,season=="cold")
m1.rev3<-subset(d1,season=="cold")

m1.rev3<-subset(raw,ag==3)

fit0 <-clogit(event~PM25      +lag0_meantemp +lag0_meanhumi +(strata(ID)),data=m1.rev3)
fit1 <-clogit(event~lag1_PM25 +lag1_meantemp +lag1_meanhumi +(strata(ID)),data=m1.rev3)
fit2 <-clogit(event~lag2_PM25 +lag2_meantemp +lag2_meanhumi +(strata(ID)),data=m1.rev3)
fit3 <-clogit(event~lag3_PM25 +lag3_meantemp +lag3_meanhumi +(strata(ID)),data=m1.rev3)
fit4 <-clogit(event~lag4_PM25 +lag4_meantemp +lag4_meanhumi +(strata(ID)),data=m1.rev3)
fit5 <-clogit(event~lag5_PM25 +lag5_meantemp +lag5_meanhumi +(strata(ID)),data=m1.rev3)
fit6 <-clogit(event~lag6_PM25 +lag6_meantemp +lag6_meanhumi +(strata(ID)),data=m1.rev3)
fit7 <-clogit(event~lag7_PM25 +lag7_meantemp +lag7_meanhumi +(strata(ID)),data=m1.rev3)
fit8 <-clogit(event~lag8_PM25 +lag8_meantemp +lag8_meanhumi +(strata(ID)),data=m1.rev3)
fit9 <-clogit(event~lag9_PM25 +lag9_meantemp +lag9_meanhumi +(strata(ID)),data=m1.rev3)
fit10<-clogit(event~lag10_PM25+lag10_meantemp+lag10_meanhumi+(strata(ID)),data=m1.rev3)
fit11<-clogit(event~lag11_PM25+lag11_meantemp+lag11_meanhumi+(strata(ID)),data=m1.rev3)
fit12<-clogit(event~lag12_PM25+lag12_meantemp+lag12_meanhumi+(strata(ID)),data=m1.rev3)
fit13<-clogit(event~lag13_PM25+lag13_meantemp+lag13_meanhumi+(strata(ID)),data=m1.rev3)
fit14<-clogit(event~lag14_PM25+lag14_meantemp+lag14_meanhumi+(strata(ID)),data=m1.rev3)

result1.s<-as.data.frame(rbind(summary(fit0)$coeff[1,] ,summary(fit1)$coeff[1,],
                               summary(fit2)$coeff[1,] ,summary(fit3)$coeff[1,],
                               summary(fit4)$coeff[1,] ,summary(fit5)$coeff[1,],
                               summary(fit6)$coeff[1,] ,summary(fit7)$coeff[1,],
                               summary(fit8)$coeff[1,] ,summary(fit9)$coeff[1,],
                               summary(fit10)$coeff[1,],summary(fit11)$coeff[1,],
                               summary(fit12)$coeff[1,],summary(fit13)$coeff[1,],
                               summary(fit14)$coeff[1,]))

result1.s$`Pr(>|z|)`=round(result1.s$`Pr(>|z|)`,3)
result1.s$sign=ifelse(result1.s$`Pr(>|z|)`<0.05,"*","")

#moving avergae
fit1 <-clogit(event~lag01_PM25 +lag01_meantemp +lag01_meanhumi +(strata(ID)),data=m1.rev3)
fit2 <-clogit(event~lag02_PM25 +lag02_meantemp +lag02_meanhumi +(strata(ID)),data=m1.rev3)
fit3 <-clogit(event~lag03_PM25 +lag03_meantemp +lag03_meanhumi +(strata(ID)),data=m1.rev3)
fit4 <-clogit(event~lag04_PM25 +lag04_meantemp +lag04_meanhumi +(strata(ID)),data=m1.rev3)
fit5 <-clogit(event~lag05_PM25 +lag05_meantemp +lag05_meanhumi +(strata(ID)),data=m1.rev3)
fit6 <-clogit(event~lag06_PM25 +lag06_meantemp +lag06_meanhumi +(strata(ID)),data=m1.rev3)
fit7 <-clogit(event~lag07_PM25 +lag07_meantemp +lag07_meanhumi +(strata(ID)),data=m1.rev3)
fit8 <-clogit(event~lag08_PM25 +lag08_meantemp +lag08_meanhumi +(strata(ID)),data=m1.rev3)
fit9 <-clogit(event~lag09_PM25 +lag09_meantemp +lag09_meanhumi +(strata(ID)),data=m1.rev3)
fit10<-clogit(event~lag010_PM25+lag010_meantemp+lag010_meanhumi+(strata(ID)),data=m1.rev3)
fit11<-clogit(event~lag011_PM25+lag011_meantemp+lag011_meanhumi+(strata(ID)),data=m1.rev3)
fit12<-clogit(event~lag012_PM25+lag012_meantemp+lag012_meanhumi+(strata(ID)),data=m1.rev3)
fit13<-clogit(event~lag013_PM25+lag013_meantemp+lag013_meanhumi+(strata(ID)),data=m1.rev3)
fit14<-clogit(event~lag014_PM25+lag014_meantemp+lag014_meanhumi+(strata(ID)),data=m1.rev3)

result1.m<-as.data.frame(rbind(summary(fit1)$coeff[1,] ,summary(fit2)$coeff[1,],
                               summary(fit3)$coeff[1,] ,summary(fit4)$coeff[1,],
                               summary(fit5)$coeff[1,] ,summary(fit6)$coeff[1,],
                               summary(fit7)$coeff[1,] ,summary(fit8)$coeff[1,],
                               summary(fit9)$coeff[1,] ,summary(fit10)$coeff[1,],
                               summary(fit11)$coeff[1,],summary(fit12)$coeff[1,],
                               summary(fit13)$coeff[1,],summary(fit14)$coeff[1,]))

result1.m$`Pr(>|z|)`=round(result1.m$`Pr(>|z|)`,3)
result1.m$sign=ifelse(result1.m$`Pr(>|z|)`<0.05,"*","")

result1.s
result1.m

#OR, CI 계산 
result1.s$OR=round(with(result1.s,exp(coef*10)),2)
result1.m$OR=round(with(result1.m,exp(coef*10)),2)

result1.s$LCI=round(with(result1.s,exp(coef*10-1.96*`se(coef)`*10)),2)
result1.m$LCI=round(with(result1.m,exp(coef*10-1.96*`se(coef)`*10)),2)

result1.s$UCI=round(with(result1.s,exp(coef*10+1.96*`se(coef)`*10)),2)
result1.m$UCI=round(with(result1.m,exp(coef*10+1.96*`se(coef)`*10)),2)

View(rbind(result1.s,result1.m))

aggregate(raw$event,list(raw$sex),table)
aggregate(raw$event,list(raw$season),table)
aggregate(raw$event,list(raw$ag),table)
#################################################################################
#################################################################################
setwd("D:\\EUMC\\논문\\연구논문\\PM2.5_Kawasaki_disease\\result")

fig<-read.csv("PM_Kawasaki_result.csv")

fig
s<-subset(fig,gubun=="single")
m<-subset(fig,gubun!="single")

s$Lag=factor(s$Lag,levels=unique(s$Lag))
m$Lag=factor(m$Lag,levels=unique(m$Lag))

s1<-subset(s,Group=="Total")
m1<-subset(m,Lag %in% c("0-1","0-6","0-13"))

x11();ggplot(s1,aes(Lag,OR))+geom_point(size=4)+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),width=0.2,lwd=0.8)+
  geom_hline(yintercept=1,col="red",lwd=1.1)+
  theme_minimal(base_size=25)+labs(x="Lag days",y="Odds ratio (95% confidence interval)")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

tiff(filename="D:\\EUMC\\논문\\연구논문\\PM2.5_Kawasaki_disease\\Fig.S3.tiff",width=5400,height=3200,res=300)
ggplot(s1,aes(Lag,OR))+geom_point(size=4)+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),width=0.2,lwd=0.8)+
  geom_hline(yintercept=1,col="red",lwd=1.1)+
  theme_minimal(base_size=25)+labs(x="Lag days",y="Odds ratio (95% confidence interval)")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

x11();ggplot(m1,aes(Lag,OR))+geom_point(size=4)+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),width=0.2,lwd=0.8)+
  geom_hline(yintercept=1,col="red")+theme_gray(base_size=25)+labs(x="",y="Odds Ratio (95% Confidence interval)")


s2<-subset(s,Group %in%  c("Male","Female"))
m2<-subset(m,Group %in%  c("Male","Female"))

s2$Group=factor(s2$Group,levels=unique(s2$Group))
m2$Group=factor(m2$Group,levels=unique(m2$Group))

x11();ggplot(s2,aes(Lag,OR,group=Group,col=Group))+geom_point(size=4,aes(shape=Group),position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),position=position_dodge(0.5),width=0.2,lwd=0.8)+
  geom_hline(yintercept=1,col="red")+theme_gray(base_size=25)+labs(x="",y="Odds Ratio (95% Confidence interval)")+
  theme(legend.title = element_blank())

x11();ggplot(m2,aes(Lag,OR,group=Group,col=Group))+geom_point(size=4,aes(shape=Group),position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),position=position_dodge(0.5),width=0.2,lwd=0.8)+
  geom_hline(yintercept=1,col="red")+theme_gray(base_size=25)+labs(x="",y="Odds Ratio (95% Confidence interval)")+
  theme(legend.title = element_blank())

s3<-subset(s,Group %in%  c("Warm season","Cold season"))
m3<-subset(m,Group %in%  c("Warm season","Cold season"))

s3$Group=factor(s3$Group,levels=unique(s3$Group))
m3$Group=factor(m3$Group,levels=unique(m3$Group))

x11();ggplot(s3,aes(Lag,OR,group=Group,col=Group))+geom_point(size=4,aes(shape=Group),position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),position=position_dodge(0.5),width=0.2,lwd=0.8)+
  geom_hline(yintercept=1,col="red")+theme_gray(base_size=25)+labs(x="",y="Odds Ratio (95% Confidence interval)")+
  theme(legend.title = element_blank())

x11();ggplot(m3,aes(Lag,OR,group=Group,col=Group))+geom_point(size=4,aes(shape=Group),position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),position=position_dodge(0.5),width=0.2,lwd=0.8)+
  geom_hline(yintercept=1,col="red")+theme_gray(base_size=25)+labs(x="",y="Odds Ratio (95% Confidence interval)")+
  theme(legend.title = element_blank())

s4<-subset(s,Group %in%  c("<1","1-4","5-10"))
m4<-subset(m,Group %in%  c("<1","1-4","5-10"))

s4$Group=factor(s4$Group,levels=unique(s4$Group))
m4$Group=factor(m4$Group,levels=unique(m4$Group))

x11();ggplot(s4,aes(Lag,OR,group=Group,col=Group))+geom_point(size=4,aes(shape=Group),position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),position=position_dodge(0.5),width=0.2,lwd=0.8)+
  geom_hline(yintercept=1,col="red")+theme_gray(base_size=25)+labs(x="",y="Odds Ratio (95% Confidence interval)")+
  theme(legend.title = element_blank())

x11();ggplot(m4,aes(Lag,OR,group=Group,col=Group))+geom_point(size=4,aes(shape=Group),position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),position=position_dodge(0.5),width=0.2,lwd=0.8)+
  geom_hline(yintercept=1,col="red")+theme_gray(base_size=25)+labs(x="",y="Odds Ratio (95% Confidence interval)")+
  theme(legend.title = element_blank())

####################################################################################################
####################################################################################################
fig<-as.data.frame(read_excel("PM_Kawasaki_result.xlsx",sheet=3))

s<-subset(fig,gubun=="single")
m<-subset(fig,gubun!="single")

s$Lag=factor(s$Lag,levels=unique(s$Lag))
m$Lag=factor(m$Lag,levels=unique(m$Lag))

s$Region=factor(s$Region,levels=unique(s$Region))
m$Region=factr(m$Region,levels=unique(m$Region))

s1<-subset(s,Group=="Total")
m1<-subset(m,Group=="Total")

x11();ggplot(s1,aes(Lag,OR,group=Region,col=Region))+geom_point(size=4,aes(shape=Region),position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),position=position_dodge(0.5),width=0.2,lwd=0.8)+
  geom_hline(yintercept=1,col="red")+theme_gray(base_size=25)+labs(x="",y="Odds Ratio (95% Confidence interval)")+
  theme(legend.title = element_blank())

x11();ggplot(m1,aes(Lag,OR,group=Region,col=Region))+geom_point(size=4,aes(shape=Region),position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),position=position_dodge(0.5),width=0.2,lwd=0.8)+
  geom_hline(yintercept=1,col="red")+theme_gray(base_size=25)+labs(x="",y="Odds Ratio (95% Confidence interval)")+
  theme(legend.title = element_blank())


########################################################################################################################
########################################################################################################################
s1<-subset(s,Lag==0)
m1<-subset(m,Lag %in% c("0-1","0-6","0-13"))
sm<-rbind(s1,m1)
sm$Lag=as.character(sm$Lag)

sm$Lag=ifelse(sm$Lag=="0","Current day                    (Lag 0)",sm$Lag)
sm$Lag=ifelse(sm$Lag=="0-1","2-day moving average   (Lag 0-1)",sm$Lag)
sm$Lag=ifelse(sm$Lag=="0-6","7-day moving average   (Lag 0-6)",sm$Lag)
sm$Lag=ifelse(sm$Lag=="0-13","14-day moving avergae (Lag 0-13)",sm$Lag)
sm$Lag=factor(sm$Lag,levels=unique(sm$Lag))
sm$Group=as.character(sm$Group)
sm$Group=ifelse(sm$Group=="Male","Boys",sm$Group)
sm$Group=ifelse(sm$Group=="Female","Girls",sm$Group)
sm$Group=factor(sm$Group,levels=unique(sm$Group))

x11();ggplot(sm,aes(Group,OR,group=Lag))+geom_point(size=4,aes(shape=Lag),position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),width=0.2,lwd=0.8,position=position_dodge(0.5))+
  geom_hline(yintercept=1,col="red",lwd=1.1)+
  theme_minimal(base_size=25)+labs(x="",y="Odds ratio (95% confidence interval)")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.title=element_blank(),
        legend.justification=c(1,1),legend.position=c(1,1))+
  scale_shape_manual(values=c(16,17,15,18))

tiff(filename="D:\\EUMC\\논문\\연구논문\\PM2.5_Kawasaki_disease\\Fig2.tiff",width=5400,height=3200,res=300)
ggplot(sm,aes(Group,OR,group=Lag))+geom_point(size=4,aes(shape=Lag),position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=LCI,ymax=UCI),width=0.2,lwd=0.8,position=position_dodge(0.5))+
  geom_hline(yintercept=1,col="red",lwd=1.1)+
  theme_minimal(base_size=25)+labs(x="",y="Odds ratio (95% confidence interval)")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.title=element_blank(),
        legend.justification=c(1,1),legend.position=c(1,1))+
  scale_shape_manual(values=c(16,17,15,18))
dev.off()
########################################################################################################################
########################################################################################################################
setwd("D:\\EUMC\\논문\\연구논문\\대기오염_가와사키병\\data\\EUMC_EMR")

pm25<-read.csv("CMAQ_2006_2016_daily_lag.csv")


pm25$date=ymd(pm25$date)

pm25.r<-pm25 %>% select(date,SIDO,SIGUNGU,pm) %>% mutate(key=paste0(date,"-",substr(SIDO,1,2),"-",SIGUNGU))

View(pm25.r)

ag1<-aggregate(pm25.r$key,list(pm25.r$key),length)
View(ag1)

#같은 시에 여러구는 종합하기
pm25.rr<-with(pm25.r,aggregate(pm,list(key,date,SIDO,SIGUNGU),mean))


names(pm25.rr)=c("key","date","SIDO","SIGUNGU","PM25")

pm25.rr$key2=paste0(pm25.rr$SIDO,"-",pm25.rr$SIGUNGU)

n<-unique(pm25.rr$key2)

nm=NULL
for(i in 1:length(n)){
  
  a<-subset(pm25.rr,key2==n[i])
  
  a$lag1_PM25 =lag(a$PM25,1)
  a$lag2_PM25 =lag(a$PM25,2)
  a$lag3_PM25 =lag(a$PM25,3)
  a$lag4_PM25 =lag(a$PM25,4)
  a$lag5_PM25 =lag(a$PM25,5)
  a$lag6_PM25 =lag(a$PM25,6)
  a$lag7_PM25 =lag(a$PM25,7)
  a$lag8_PM25 =lag(a$PM25,8)
  a$lag9_PM25 =lag(a$PM25,9)
  a$lag10_PM25=lag(a$PM25,10)
  a$lag11_PM25=lag(a$PM25,11)
  a$lag12_PM25=lag(a$PM25,12)
  a$lag13_PM25=lag(a$PM25,13)
  a$lag14_PM25=lag(a$PM25,14)
  
  a$lag01_PM25 =apply(a %>% select(PM25,lag1_PM25),1,mean,na.rm=T)
  a$lag02_PM25 =apply(a %>% select(PM25,lag1_PM25:lag2_PM25),1,mean,na.rm=T)
  a$lag03_PM25 =apply(a %>% select(PM25,lag1_PM25:lag3_PM25),1,mean,na.rm=T)
  a$lag04_PM25 =apply(a %>% select(PM25,lag1_PM25:lag4_PM25),1,mean,na.rm=T)
  a$lag05_PM25 =apply(a %>% select(PM25,lag1_PM25:lag5_PM25),1,mean,na.rm=T)
  a$lag06_PM25 =apply(a %>% select(PM25,lag1_PM25:lag6_PM25),1,mean,na.rm=T)
  a$lag07_PM25 =apply(a %>% select(PM25,lag1_PM25:lag7_PM25),1,mean,na.rm=T)
  a$lag08_PM25 =apply(a %>% select(PM25,lag1_PM25:lag8_PM25),1,mean,na.rm=T)
  a$lag09_PM25 =apply(a %>% select(PM25,lag1_PM25:lag9_PM25),1,mean,na.rm=T)
  a$lag010_PM25=apply(a %>% select(PM25,lag1_PM25:lag10_PM25),1,mean,na.rm=T)
  a$lag011_PM25=apply(a %>% select(PM25,lag1_PM25:lag11_PM25),1,mean,na.rm=T)
  a$lag012_PM25=apply(a %>% select(PM25,lag1_PM25:lag12_PM25),1,mean,na.rm=T)
  a$lag013_PM25=apply(a %>% select(PM25,lag1_PM25:lag13_PM25),1,mean,na.rm=T)
  a$lag014_PM25=apply(a %>% select(PM25,lag1_PM25:lag14_PM25),1,mean,na.rm=T)
  
  nm[[i]]<-a
  print(i)}

pm25.rrr<-as.data.frame(do.call(rbind,nm))

write.csv(pm25.rrr,file="CMAQ_PM25_rev.csv",row.names=F,na="")
