#-------------------------------------------------------------------------#
#폭염 질병청 공단자료 -5세 미만 영유아 & 온열질환관련 입원 논문작성용 코드
#-------------------------------------------------------------------------#
#library
pacman::p_load("dplyr","ggplot2","reshape2","sqldf","RColorBrewer","lubridate","lmtest","readxl","survival",
               "splines","data.table","stringr","tidyr","extrafont","scales","gridExtra","Epi","tsModel",
               "mgcv","gamm4","metafor","dlnm","survival","splines")

#-------------------------------------------------------------------------#

#-------------------------------------------------------------------------#
setwd("D:\\EUMC\\질병관리청\\폭염연구\\공단자료")
child_ts<-read.csv("child_ts.csv")
child_ts$ddate=as.Date(child_ts$ddate)

#dataset n; 1826*17=31,042, 2015-01-01~2019-12-31까지 기간*17개 도시


child_ts %>% group_by(year,month) %>% summarise(meanT=mean(meantemp_lag0,na.rm=T),
                                                maxT=mean(maxtemp_lag0,na.rm=T)) %>% View

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#노출 자료
temp_outcome<-read_excel("D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\heattable.xlsx",sheet=1)

to<-melt(temp_outcome %>% dplyr:: select(-c(note)),id.vars=c("category","month"))
to$month=ifelse(as.numeric(gsub("월","",to$month))<10,paste0(0,as.numeric(gsub("월","",to$month))),
                as.numeric(gsub("월","",to$month)))
to$yymm=ymd(paste0(substr(to$variable,2,5),"-",to$month,"-",01))

heatrelated_month_yr<-to %>% filter(category %in% c("heatrelated"))

exp <-to %>% filter(category %in% c("meanT","maxT"))
exp$gubun=rep(c("평균기온(°C)","최고기온(°C)"),each=12)
exp$gubun=factor(exp$gubun,levels=unique(exp$gubun))

#그림 수정 
exp1<-exp %>% filter(category=="meanT")
exp2<-exp %>% filter(category=="maxT")

exp2$fillcol=c("orange","orange","orange","orange","orange","red","red","red","orange","orange","orange","orange")

#월별 그림 ver1, 5세 미만 영유아 온열질환 관련 입원 발생 건수 (겹쳐서)
x11();ggplot(heatrelated_month_yr,aes(yymm,value))+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")+
  geom_bar(data=exp2,aes(x=yymm,y = value*60),stat="identity",fill=exp2$fillcol,binwidth=0.2)+
  scale_y_continuous(sec.axis= sec_axis(~. /60, name = "Temperauture(°C)"),labels=comma)+
  geom_point(size=5)+geom_line(size=2)+labs(x="Date",y="Counts ")+
  scale_fill_manual(values=c("red","orange"),label=c("red","orange"))

#월별 건수_최고기온 
tiff(file="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\Figure\\월별 건수_최고기온.tiff",
     width=6000, height=3000,res=300)
ggplot(heatrelated_month_yr,aes(yymm,value))+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")+
  geom_bar(data=exp2,aes(x=yymm,y = value*60),stat="identity",fill=exp2$fillcol,binwidth=0.2)+
  scale_y_continuous(sec.axis= sec_axis(~. /60, name = "Temperauture(°C)"),labels=comma)+
  geom_point(size=5)+geom_line(size=2)+labs(x="Date",y="Counts ")+
  scale_fill_manual(values=c("red","orange"),label=c("red","orange"))
dev.off()

#월별 그림 ver2, 5세 미만 영유아 온열질환 관련 입원 발생 건수 (나눠서) 
exp$gubun=ifelse(exp$category=="maxT","Maxium Temperature","Mean Temperature")
exp$gubun=factor(exp$gubun,levels=unique(exp$gubun))
gexp<-ggplot(exp,aes(yymm,value,col=gubun))+geom_point(size=5)+geom_line(size=2)+labs(x="",y="Temperauture (°C)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),legend.position = "top")+
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")+
  scale_color_manual(values=c("orange","red"))

heat_g<-ggplot(heatrelated_month_yr,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="Date",y="Counts ")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

x11();grid.arrange(gexp,heat_g,ncol=1)
#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
#메인으로 사용할 자료
#전체 자료에서 데이터 클렌징 하고, 같은날 중복 환자 제거, 에피소드 정리해서
#온열관련질환(T67, E86)으로 추출된 고유 환자는 65,582건임 
#총 60,196건인데 이중 여름기간만 하면 17,179

#total: 60,196 (2015~2019)
#summer period (6,7,8):17,179
heatdat<-child_ts %>% dplyr::select(SIDO_KN:SIDO,heatrelated_CHILD_TOT:pm25_lag06)

heatdat$area=factor(heatdat$area,levels=unique(heatdat$area))

#전체기간 일일건수 도시별 평균/분산 
cbind(mean=aggregate(heatdat$outcome,list(heatdat$area),mean,na.rm=T),
      var=aggregate(heatdat$outcome,list(heatdat$area),var,na.rm=T)$x)

mean(heatdat$outcome,na.rm=T)
var(heatdat$outcome,na.rm=T)



#전국 취합한 outcome 카운트
aggC<-aggregate(heatdat$outcome,list(heatdat$ddate),sum)

#전국에서 일 최고기온 제일 높았을 때
aggT<-aggregate(heatdat$maxtemp_lag0,list(heatdat$ddate),max,na.rm=T)

names(aggC)=c("date","counts")
names(aggT)=c("date","maxT")

aggCT<-aggC %>% left_join(aggT,by="date")
aggCT$heat=as.factor(ifelse(aggCT$maxT>=33,"Maximum temperature ≥ 33°C","Maximum temperature < 33°C"))
aggCT$year =year(aggCT$date)
aggCT$month=month(aggCT$date)
aggCT2<-subset(aggCT,month %in% c(6,7,8))

aggCT2 %>% group_by(month,year) %>% summarise(sum(counts))


#전국 일별 온열질환 입원 합계
#전국 일별 일최고기온 (33도 이상은 빨강 표시, 그외 오렌지색)
#빈도는 점으로 표시 

x11();ggplot(aggCT,aes(date,maxT,fill=heat))+
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")+
  geom_bar(stat="identity",col=ifelse(aggCT$maxT>=33,"red","orange"))+
  scale_y_continuous(sec.axis= sec_axis(~. *5, name = "Daily counts"),labels=comma)+
  geom_point(data=aggCT,aes(date,counts/5),size=2)+labs(x="Date",y="Maximum temperature (°C)")+
  scale_fill_manual(values=c("orange","red"))+
  theme_gray(base_size=25)+theme(legend.position = "top",
                                 legend.title=element_blank(),
                                 axis.text.x = element_text(angle = 90, vjust = 0.5))

theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

##GAMM,노출 반응 그림저장, 단일 지연; 온열질환 관련
tiff(file="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\Figure\\일일건수_최고기온.tiff",
     width=6000, height=3000,res=300)
ggplot(aggCT,aes(date,maxT,fill=heat))+
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")+
  geom_bar(stat="identity",col=ifelse(aggCT$maxT>=33,"red","orange"))+
  scale_y_continuous(sec.axis= sec_axis(~. *5, name = "Daily counts"),labels=comma)+
  geom_point(data=aggCT,aes(date,counts/5),size=2)+labs(x="Date",y="Maximum temperature (°C)")+
  scale_fill_manual(values=c("orange","red"))+
  theme_gray(base_size=25)+theme(legend.position = "top",
                                 legend.title=element_blank(),
                                 axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()


agg_yr_month<-aggregate(heatdat$outcome,list(heatdat$year,heatdat$month),sum)

names(agg_yr_month)=c("year","month","count")

agg_yr_month %>% filter(month %in% c(6,7,8) )

#전체 기간에 대해서 검토 
heatdat %>% dplyr:: group_by(area) %>% summarise(mean=mean(heatrelated_CHILD_TOT),var=var(heatrelated_CHILD_TOT))
heatdat%>% dplyr::  group_by(ddate) %>% summarise(meanT=mean(meantemp_lag0,na.rm=T),
                                                  maxT =mean(maxtemp_lag0,na.rm=T),
                                                  pm25=mean(pm25_lag0,na.rm=T),
                                                  meanhumi=mean(meanhumi_lag0,na.rm=T),
                                                  ap=mean(meanpress1_lag0,na.rm=T),
                                                  windspeed=mean(windspeed_lag0,na.rm=T),
                                                  dewtemp=mean(dewtemp_lag0),na.rm=T) %>% dplyr::select(meanT:dewtemp) %>% 
  cor(method="spearman",use="complete.obs")

#폭염기간에 대해서 검토 

heatdat %>% filter(month %in% c(6,7,8))  %>%   
  dplyr:: group_by(area) %>% summarise(mean=mean(heatrelated_CHILD_TOT),var=var(heatrelated_CHILD_TOT))

heatdat %>% filter(month %in% c(6,7,8))  %>% 
  dplyr::  group_by(ddate) %>% summarise(meanT=mean(meantemp_lag0,na.rm=T),
                                         maxT =mean(maxtemp_lag0,na.rm=T),
                                         pm25=mean(pm25_lag0,na.rm=T),
                                         meanhumi=mean(meanhumi_lag0,na.rm=T),
                                         ap=mean(meanpress1_lag0,na.rm=T),
                                         windspeed=mean(windspeed_lag0,na.rm=T),
                                         dewtemp=mean(dewtemp_lag0),na.rm=T) %>% dplyr::select(meanT:dewtemp) %>% 
  cor(method="spearman",use="complete.obs")

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#전체 일괄적으로 GAMM으로 추정하기 
#Exposure-repsonse curve, GAMM
#결측 제거, 세종, 제주 제외, 6~8월만
tt<-heatdat[complete.cases(heatdat),] %>% filter(!area %in% c("세종","제주")) %>% filter(month %in% c(6:8))
tt$outcome=tt$heatrelated_CHILD_TOT
tt$area=factor(tt$area)


filter(heatdat, month %in% c(6,7,8))$heatrelated_CHILD_M %>% sum
filter(heatdat, month %in% c(6,7,8))$heatrelated_CHILD_F %>% sum
filter(heatdat, month %in% c(6,7,8) & !area %in% c("세종","제주"))$heatrelated_CHILD_M %>% sum
filter(heatdat, month %in% c(6,7,8) & !area %in% c("세종","제주"))$heatrelated_CHILD_F %>% sum

heatdat %>% dplyr:: filter(month %in% c(6,7,8)) %>%  group_by(area) %>% summarise(sum(outcome))
heatdat %>% dplyr:: filter(month %in% c(6,7,8)) %>%  group_by(month,year) %>% summarise(sum(outcome))
heatdat %>% dplyr:: filter(month %in% c(6,7,8) & !area %in% c("세종","제주")) %>%  group_by(year) %>% summarise(sum(heatrelated_CHILD_M))
heatdat %>% dplyr:: filter(month %in% c(6,7,8) & !area %in% c("세종","제주")) %>%  group_by(year) %>% summarise(sum(heatrelated_CHILD_F))

heatdat %>% dplyr:: filter(month %in% c(6,7,8) & !area %in% c("세종","제주"))%>%  group_by(year)  %>% summarise(sum(outcome))
heatdat$area=factor(heatdat$area,levels=unique(heatdat$area))
heatdat %>% dplyr:: filter(month %in% c(6,7,8) & !area %in% c("세종","제주"))%>%  group_by(year,area)  %>% summarise(sum(outcome)) %>% View

#폭염 기간 (6~8월) 일일건수 도시별 평균/분산
cbind(mean=aggregate(tt$outcome,list(tt$area),mean,na.rm=T),
      var=aggregate(tt$outcome,list(tt$area),var,na.rm=T)$x)

mean(tt$outcome,na.rm=T)
var(tt$outcome,na.rm=T)

#노출자료 도시별 요약 통계
cbind(aggregate(tt$maxtemp_lag0,list(tt$area),mean,na.rm=T),
      sd=aggregate(tt$maxtemp_lag0,list(tt$area),sd,na.rm=T)$x)

cbind(aggregate(tt$pm25_lag0,list(tt$area),mean,na.rm=T),
      sd=aggregate(tt$pm25_lag0,list(tt$area),sd,na.rm=T)$x)

cbind(aggregate(tt$meanhumi_lag0,list(tt$area),mean,na.rm=T),
      sd=aggregate(tt$meanhumi_lag0,list(tt$area),sd,na.rm=T)$x)

cbind(aggregate(tt$dewtemp_lag0,list(tt$area),mean,na.rm=T),
      sd=aggregate(tt$dewtemp_lag0,list(tt$area),sd,na.rm=T)$x)

cbind(aggregate(tt$windspeed_lag0,list(tt$area),mean,na.rm=T),
      sd=aggregate(tt$windspeed_lag0,list(tt$area),sd,na.rm=T)$x)

with(tt,cbind(m=mean(maxtemp_lag0,na.rm=T),sd=sd(maxtemp_lag0,na.rm=T)))
with(tt,cbind(m=mean(pm25_lag0,na.rm=T),sd=sd(pm25_lag0,na.rm=T)))
with(tt,cbind(m=mean(meanhumi_lag0,na.rm=T),sd=sd(meanhumi_lag0,na.rm=T)))
with(tt,cbind(m=mean(dewtemp_lag0,na.rm=T),sd=sd(dewtemp_lag0,na.rm=T)))
with(tt,cbind(m=mean(windspeed_lag0,na.rm=T),sd=sd(windspeed_lag0,na.rm=T)))

#노출 변수 요약 함수 생성
summ.func<-function(x){
  x2<-as.numeric(na.omit(x))
  n   =sum(!is.na(x))
  Mean=mean(x2)
  SD  =sd(x2)
  SE  =sd(x2)/sqrt(length(x2))
  LCI =t.test(x2)$conf.int[1]
  UCI =t.test(x2)$conf.int[2]
  CV=sd(x2)/mean(x2)
  Min =min(x2)
  Max =max(x2)
  p25<-quantile(na.omit(x2))[2] %>% as.numeric
  p50<-quantile(na.omit(x2))[3] %>% as.numeric
  p75<-quantile(na.omit(x2))[4] %>% as.numeric
  GM=geometric.mean(x2)
  GeoSD=exp(sd(log(x2)))
  GeoCV=sqrt(exp(sd(log(x2))^2)-1)
  
  Skewness=skewness(x2,na.rm=T)
  Kurtosis=kurtosis(x2,na.rm=T)
  #Geo S.E
  GeoSE<-sd(log(x2))/sqrt(length(x2))
  
  #GEO 95% CI
  tval<-qt(0.975,df=length(x2)-1) #t값(two-sided)
  
  Geo_lci=geometric.mean(x2)/exp(tval*GeoSE) #Geo 95% Lower CI
  Geo_uci=geometric.mean(x2)*exp(tval*GeoSE) #Geo 95% Upper CI
  
  as.data.frame(cbind(n,Mean,SD,SE,tval,LCI,UCI,CV,Skewness,Kurtosis,
                      Min,p25,p50,p75,Max,GM,GeoSD,GeoCV,GeoSE,Geo_lci,Geo_uci))}

#노출자료 요약 통계
rbind(summ.func(tt$maxtemp_lag0),
      summ.func(tt$pm25_lag0),
      summ.func(tt$meanhumi_lag0),
      summ.func(tt$dewtemp_lag0),
      summ.func(tt$windspeed_lag0))
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 온열질환 관련
tt.fig=NULL

#GAMM, 단일지연 
tt.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

#GAMM, 이동평균
tt.fig$er01<-gamm(outcome~s(maxtemp_lag01)+s(time,k=2*5)+s(meanhumi_lag01)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er02<-gamm(outcome~s(maxtemp_lag02)+s(time,k=2*5)+s(meanhumi_lag02)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er03<-gamm(outcome~s(maxtemp_lag03)+s(time,k=2*5)+s(meanhumi_lag03)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er04<-gamm(outcome~s(maxtemp_lag04)+s(time,k=2*5)+s(meanhumi_lag04)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er05<-gamm(outcome~s(maxtemp_lag05)+s(time,k=2*5)+s(meanhumi_lag05)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er06<-gamm(outcome~s(maxtemp_lag06)+s(time,k=2*5)+s(meanhumi_lag06)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.fig$er07<-gamm(outcome~s(maxtemp_lag07)+s(time,k=2*5)+s(meanhumi_lag07)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,노출 반응 그림저장, 단일 지연; 온열질환 관련
png(file="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\Figure\\온열질환(heatrelated).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.3,0.3));abline(h=0,col="red")
dev.off()

##GAMM,노출 반응 그림저장, 단일 지연; 온열질환 관련
tiff(file="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\Figure\\온열질환(heatrelated).tiff",width=6000, height=3000,res=300)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.3,0.3));abline(h=0,col="red")
dev.off()

##GAMM,노출 반응 그림저장, 이동평균; 온열질환 관련
png(file="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\Figure\\온열질환(heatrelated)(이동평균).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt.fig$er01$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag01",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er02$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag02",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er03$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag03",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er04$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag04",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er05$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag05",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er06$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag06",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er07$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag07",ylim=c(-0.3,0.3));abline(h=0,col="red")
dev.off()

##GAMM,노출 반응 그림저장, 이동평균 ; 온열질환 관련
tiff(file="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\Figure\\온열질환(heatrelated)(이동평균).tiff",width=6000, height=3000,res=300)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt.fig$er01$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag01",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er02$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag02",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er03$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag03",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er04$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag04",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er05$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag05",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er06$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag06",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt.fig$er07$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag07",ylim=c(-0.3,0.3));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#질환별로 도시별 노출-반응 그림 그리기 
sido.gam.func<-function(dataset,text,ylimin,ylimax){
  dd<-dataset
  sido01<-dd %>% filter(area=="서울");sido02<-dd %>% filter(area=="부산")
  sido03<-dd %>% filter(area=="대구");sido04<-dd %>% filter(area=="인천")
  sido05<-dd %>% filter(area=="광주");sido06<-dd %>% filter(area=="대전")
  sido07<-dd %>% filter(area=="울산");sido08<-dd %>% filter(area=="경기")
  sido09<-dd %>% filter(area=="강원");sido10<-dd %>% filter(area=="충북")
  sido11<-dd %>% filter(area=="충남");sido12<-dd %>% filter(area=="전북")
  sido13<-dd %>% filter(area=="전남");sido14<-dd %>% filter(area=="경북")
  sido15<-dd %>% filter(area=="경남")
  
  gam01<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido01,family="poisson")
  gam02<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido02,family="poisson")
  gam03<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido03,family="poisson")
  gam04<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido04,family="poisson")
  gam05<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido05,family="poisson")
  gam06<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido06,family="poisson")
  gam07<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido07,family="poisson")
  gam08<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido08,family="poisson")
  gam09<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido09,family="poisson")
  gam10<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido10,family="poisson")
  gam11<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido11,family="poisson")
  gam12<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido12,family="poisson")
  gam13<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido13,family="poisson")
  gam14<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido14,family="poisson")
  gam15<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido15,family="poisson")
  
  png.save<-paste0("D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\Figure\\",text,".png")
  
  png(file=png.save,width=1600, height=800)
  par(mfrow=c(3,5),mar=c(5,5,5,5),oma=c(3,3,2,3))
  plot(gam01,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Seoul",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam02,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Busan",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam03,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Daegu",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam04,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Incheon",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam05,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Gwangju",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam06,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Daejeon",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam07,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Ulsan",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam08,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Gyeeonggi-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam09,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Gangwon-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam10,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Chungcheongbuk-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam11,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Chungcheongnam-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam12,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Jeollabuk-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam13,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Jeollanam-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam14,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Gyeongsangbuk-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam15,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Gyeongsangnam-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  dev.off()
  
  png.save<-paste0("D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\Figure\\",text,".tiff")
  tiff(file=png.save,width=6600, height=4200,res=300)
  par(mfrow=c(3,5),mar=c(5,5,5,5),oma=c(3,3,2,3))
  plot(gam01,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Seoul",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam02,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Busan",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam03,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Daegu",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam04,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Incheon",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam05,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Gwangju",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam06,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Daejeon",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam07,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Ulsan",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam08,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Gyeeonggi-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam09,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Gangwon-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam10,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Chungcheongbuk-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam11,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Chungcheongnam-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam12,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Jeollabuk-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam13,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Jeollanam-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam14,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Gyeongsangbuk-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam15,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Gyeongsangnam-do",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  dev.off()
  
}
sido.gam.func(tt,"도시별온열질환관련",-1.0,1.0)

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#내용 수정좀 하기 
##GAMM,모델링, 단일 지연; 온열질환 관련
tt.res=NULL
tt.res$fit0<-gamm(outcome~maxtemp_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit1<-gamm(outcome~maxtemp_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit2<-gamm(outcome~maxtemp_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit3<-gamm(outcome~maxtemp_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit4<-gamm(outcome~maxtemp_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit5<-gamm(outcome~maxtemp_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit6<-gamm(outcome~maxtemp_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit7<-gamm(outcome~maxtemp_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 이동평균; 온열질환 관련
tt.res$fit01<-gamm(outcome~maxtemp_lag01+s(time,k=2*5)+s(meanhumi_lag01)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit02<-gamm(outcome~maxtemp_lag02+s(time,k=2*5)+s(meanhumi_lag02)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit03<-gamm(outcome~maxtemp_lag03+s(time,k=2*5)+s(meanhumi_lag03)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit04<-gamm(outcome~maxtemp_lag04+s(time,k=2*5)+s(meanhumi_lag04)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit05<-gamm(outcome~maxtemp_lag05+s(time,k=2*5)+s(meanhumi_lag05)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit06<-gamm(outcome~maxtemp_lag06+s(time,k=2*5)+s(meanhumi_lag06)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$fit07<-gamm(outcome~maxtemp_lag07+s(time,k=2*5)+s(meanhumi_lag07)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 폭염 28도
tt.res$heat28_0<-gamm(outcome~heat28_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat28_1<-gamm(outcome~heat28_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat28_2<-gamm(outcome~heat28_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat28_3<-gamm(outcome~heat28_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat28_4<-gamm(outcome~heat28_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat28_5<-gamm(outcome~heat28_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat28_6<-gamm(outcome~heat28_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat28_7<-gamm(outcome~heat28_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 폭염 29도
tt.res$heat29_0<-gamm(outcome~heat29_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat29_1<-gamm(outcome~heat29_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat29_2<-gamm(outcome~heat29_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat29_3<-gamm(outcome~heat29_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat29_4<-gamm(outcome~heat29_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat29_5<-gamm(outcome~heat29_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat29_6<-gamm(outcome~heat29_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat29_7<-gamm(outcome~heat29_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 폭염 30도
tt.res$heat30_0<-gamm(outcome~heat30_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat30_1<-gamm(outcome~heat30_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat30_2<-gamm(outcome~heat30_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat30_3<-gamm(outcome~heat30_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat30_4<-gamm(outcome~heat30_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat30_5<-gamm(outcome~heat30_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat30_6<-gamm(outcome~heat30_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat30_7<-gamm(outcome~heat30_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 폭염 31도
tt.res$heat31_0<-gamm(outcome~heat31_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat31_1<-gamm(outcome~heat31_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat31_2<-gamm(outcome~heat31_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat31_3<-gamm(outcome~heat31_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat31_4<-gamm(outcome~heat31_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat31_5<-gamm(outcome~heat31_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat31_6<-gamm(outcome~heat31_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat31_7<-gamm(outcome~heat31_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 폭염 32도
tt.res$heat32_0<-gamm(outcome~heat32_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat32_1<-gamm(outcome~heat32_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat32_2<-gamm(outcome~heat32_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat32_3<-gamm(outcome~heat32_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat32_4<-gamm(outcome~heat32_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat32_5<-gamm(outcome~heat32_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat32_6<-gamm(outcome~heat32_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat32_7<-gamm(outcome~heat32_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 폭염 33도
tt.res$heat33_0<-gamm(outcome~heat33_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat33_1<-gamm(outcome~heat33_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat33_2<-gamm(outcome~heat33_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat33_3<-gamm(outcome~heat33_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat33_4<-gamm(outcome~heat33_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat33_5<-gamm(outcome~heat33_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat33_6<-gamm(outcome~heat33_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt.res$heat33_7<-gamm(outcome~heat33_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

tt.single<-as.data.frame(rbind(summary(tt.res$fit0$gam)$p.table[2,],summary(tt.res$fit1$gam)$p.table[2,],
                               summary(tt.res$fit2$gam)$p.table[2,],summary(tt.res$fit3$gam)$p.table[2,],
                               summary(tt.res$fit4$gam)$p.table[2,],summary(tt.res$fit5$gam)$p.table[2,],
                               summary(tt.res$fit6$gam)$p.table[2,],summary(tt.res$fit7$gam)$p.table[2,]))

tt.moving<-as.data.frame(rbind(summary(tt.res$fit01$gam)$p.table[2,],
                               summary(tt.res$fit02$gam)$p.table[2,],summary(tt.res$fit03$gam)$p.table[2,],
                               summary(tt.res$fit04$gam)$p.table[2,],summary(tt.res$fit05$gam)$p.table[2,],
                               summary(tt.res$fit06$gam)$p.table[2,],summary(tt.res$fit07$gam)$p.table[2,]))

tt.heat28<-as.data.frame(rbind(summary(tt.res$heat28_0$gam)$p.table[2,],summary(tt.res$heat28_1$gam)$p.table[2,],
                               summary(tt.res$heat28_2$gam)$p.table[2,],summary(tt.res$heat28_3$gam)$p.table[2,],
                               summary(tt.res$heat28_4$gam)$p.table[2,],summary(tt.res$heat28_5$gam)$p.table[2,],
                               summary(tt.res$heat28_6$gam)$p.table[2,],summary(tt.res$heat28_7$gam)$p.table[2,]))

tt.heat29<-as.data.frame(rbind(summary(tt.res$heat29_0$gam)$p.table[2,],summary(tt.res$heat29_1$gam)$p.table[2,],
                               summary(tt.res$heat29_2$gam)$p.table[2,],summary(tt.res$heat29_3$gam)$p.table[2,],
                               summary(tt.res$heat29_4$gam)$p.table[2,],summary(tt.res$heat29_5$gam)$p.table[2,],
                               summary(tt.res$heat29_6$gam)$p.table[2,],summary(tt.res$heat29_7$gam)$p.table[2,]))

tt.heat30<-as.data.frame(rbind(summary(tt.res$heat30_0$gam)$p.table[2,],summary(tt.res$heat30_1$gam)$p.table[2,],
                               summary(tt.res$heat30_2$gam)$p.table[2,],summary(tt.res$heat30_3$gam)$p.table[2,],
                               summary(tt.res$heat30_4$gam)$p.table[2,],summary(tt.res$heat30_5$gam)$p.table[2,],
                               summary(tt.res$heat30_6$gam)$p.table[2,],summary(tt.res$heat30_7$gam)$p.table[2,]))

tt.heat31<-as.data.frame(rbind(summary(tt.res$heat31_0$gam)$p.table[2,],summary(tt.res$heat31_1$gam)$p.table[2,],
                               summary(tt.res$heat31_2$gam)$p.table[2,],summary(tt.res$heat31_3$gam)$p.table[2,],
                               summary(tt.res$heat31_4$gam)$p.table[2,],summary(tt.res$heat31_5$gam)$p.table[2,],
                               summary(tt.res$heat31_6$gam)$p.table[2,],summary(tt.res$heat31_7$gam)$p.table[2,]))

tt.heat32<-as.data.frame(rbind(summary(tt.res$heat32_0$gam)$p.table[2,],summary(tt.res$heat32_1$gam)$p.table[2,],
                               summary(tt.res$heat32_2$gam)$p.table[2,],summary(tt.res$heat32_3$gam)$p.table[2,],
                               summary(tt.res$heat32_4$gam)$p.table[2,],summary(tt.res$heat32_5$gam)$p.table[2,],
                               summary(tt.res$heat32_6$gam)$p.table[2,],summary(tt.res$heat32_7$gam)$p.table[2,]))

tt.heat33<-as.data.frame(rbind(summary(tt.res$heat33_0$gam)$p.table[2,],summary(tt.res$heat33_1$gam)$p.table[2,],
                               summary(tt.res$heat33_2$gam)$p.table[2,],summary(tt.res$heat33_3$gam)$p.table[2,],
                               summary(tt.res$heat33_4$gam)$p.table[2,],summary(tt.res$heat33_5$gam)$p.table[2,],
                               summary(tt.res$heat33_6$gam)$p.table[2,],summary(tt.res$heat33_7$gam)$p.table[2,]))


tt.single$label="single";tt.single$lag=paste0("lag" ,1:8-1)
tt.moving$label="moving";tt.moving$lag=paste0("lag0",1:7)

tt.heat28$label="single";tt.heat28$lag=paste0("lag" ,1:8-1)
tt.heat29$label="single";tt.heat29$lag=paste0("lag" ,1:8-1)
tt.heat30$label="single";tt.heat30$lag=paste0("lag" ,1:8-1)
tt.heat31$label="single";tt.heat31$lag=paste0("lag" ,1:8-1)
tt.heat32$label="single";tt.heat32$lag=paste0("lag" ,1:8-1)
tt.heat33$label="single";tt.heat33$lag=paste0("lag" ,1:8-1)

tt.single$exposure="maxT"
tt.moving$exposure="maxT"
tt.heat28$exposure="heat28"
tt.heat29$exposure="heat29"
tt.heat30$exposure="heat30"
tt.heat31$exposure="heat31"
tt.heat32$exposure="heat32"
tt.heat33$exposure="heat33"

as.data.frame(rbind(tt.single,tt.moving,tt.heat28,tt.heat29,
                    tt.heat30,tt.heat31,tt.heat32,tt.heat33))

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#pm 넣은 모델, 안넣은 모델 민감도 분석 

#First stage, 시도별 분석
#질환별로 도시별 노출-반응 그림 그리기 
#보정변수:PM2.5, Time(K=2*5),S(meanhumi),S(Windspeed),DOW(요일)
heatdat$outcome=heatdat$heatrelated_CHILD_TOT

heatdat$outM=heatdat$heatrelated_CHILD_M
heatdat$outF=heatdat$heatrelated_CHILD_F

sido.gam<-function(dat){
  
  #일 최고기온 단일지연
  fit_lag0<-gam(outcome~maxtemp_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="poisson")
  fit_lag1<-gam(outcome~maxtemp_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="poisson")
  fit_lag2<-gam(outcome~maxtemp_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="poisson")
  fit_lag3<-gam(outcome~maxtemp_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="poisson")
  fit_lag4<-gam(outcome~maxtemp_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="poisson")
  fit_lag5<-gam(outcome~maxtemp_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dat,family="poisson")
  fit_lag6<-gam(outcome~maxtemp_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dat,family="poisson")
  fit_lag7<-gam(outcome~maxtemp_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dat,family="poisson")
  
  #일 최고기온 이동평균 
  fit_lag01<-gam(outcome~maxtemp_lag01+pm25_lag01+s(time,k=2*5)+s(meanhumi_lag01)+s(windspeed_lag01)+dow,data=dat,family="poisson")
  fit_lag02<-gam(outcome~maxtemp_lag02+pm25_lag02+s(time,k=2*5)+s(meanhumi_lag02)+s(windspeed_lag02)+dow,data=dat,family="poisson")
  fit_lag03<-gam(outcome~maxtemp_lag03+pm25_lag03+s(time,k=2*5)+s(meanhumi_lag03)+s(windspeed_lag03)+dow,data=dat,family="poisson")
  fit_lag04<-gam(outcome~maxtemp_lag04+pm25_lag04+s(time,k=2*5)+s(meanhumi_lag04)+s(windspeed_lag04)+dow,data=dat,family="poisson")
  fit_lag05<-gam(outcome~maxtemp_lag05+pm25_lag05+s(time,k=2*5)+s(meanhumi_lag05)+s(windspeed_lag05)+dow,data=dat,family="poisson")
  fit_lag06<-gam(outcome~maxtemp_lag06+pm25_lag06+s(time,k=2*5)+s(meanhumi_lag06)+s(windspeed_lag06)+dow,data=dat,family="poisson")
  fit_lag07<-gam(outcome~maxtemp_lag07+pm25_lag06+s(time,k=2*5)+s(meanhumi_lag07)+s(windspeed_lag07)+dow,data=dat,family="poisson")
  
  #폭염 28도 단일지연
  heat28_lag0<-gam(outcome~heat28_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="poisson")
  heat28_lag1<-gam(outcome~heat28_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="poisson")
  heat28_lag2<-gam(outcome~heat28_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="poisson")
  heat28_lag3<-gam(outcome~heat28_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="poisson")
  heat28_lag4<-gam(outcome~heat28_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="poisson")
  heat28_lag5<-gam(outcome~heat28_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dat,family="poisson")
  heat28_lag6<-gam(outcome~heat28_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dat,family="poisson")
  heat28_lag7<-gam(outcome~heat28_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dat,family="poisson")
  
  #폭염 29도 단일지연
  heat29_lag0<-gam(outcome~heat29_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="poisson")
  heat29_lag1<-gam(outcome~heat29_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="poisson")
  heat29_lag2<-gam(outcome~heat29_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="poisson")
  heat29_lag3<-gam(outcome~heat29_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="poisson")
  heat29_lag4<-gam(outcome~heat29_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="poisson")
  heat29_lag5<-gam(outcome~heat29_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dat,family="poisson")
  heat29_lag6<-gam(outcome~heat29_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dat,family="poisson")
  heat29_lag7<-gam(outcome~heat29_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dat,family="poisson")
  
  #폭염 30도 단일지연
  heat30_lag0<-gam(outcome~heat30_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="poisson")
  heat30_lag1<-gam(outcome~heat30_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="poisson")
  heat30_lag2<-gam(outcome~heat30_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="poisson")
  heat30_lag3<-gam(outcome~heat30_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="poisson")
  heat30_lag4<-gam(outcome~heat30_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="poisson")
  heat30_lag5<-gam(outcome~heat30_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dat,family="poisson")
  heat30_lag6<-gam(outcome~heat30_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dat,family="poisson")
  heat30_lag7<-gam(outcome~heat30_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dat,family="poisson")
  
  #폭염 31도 단일지연
  heat31_lag0<-gam(outcome~heat31_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="poisson")
  heat31_lag1<-gam(outcome~heat31_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="poisson")
  heat31_lag2<-gam(outcome~heat31_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="poisson")
  heat31_lag3<-gam(outcome~heat31_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="poisson")
  heat31_lag4<-gam(outcome~heat31_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="poisson")
  heat31_lag5<-gam(outcome~heat31_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dat,family="poisson")
  heat31_lag6<-gam(outcome~heat31_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dat,family="poisson")
  heat31_lag7<-gam(outcome~heat31_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dat,family="poisson")
  
  #폭염 32도 단일지연
  heat32_lag0<-gam(outcome~heat32_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="poisson")
  heat32_lag1<-gam(outcome~heat32_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="poisson")
  heat32_lag2<-gam(outcome~heat32_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="poisson")
  heat32_lag3<-gam(outcome~heat32_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="poisson")
  heat32_lag4<-gam(outcome~heat32_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="poisson")
  heat32_lag5<-gam(outcome~heat32_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dat,family="poisson")
  heat32_lag6<-gam(outcome~heat32_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dat,family="poisson")
  heat32_lag7<-gam(outcome~heat32_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dat,family="poisson")
  
  #폭염 33도 단일지연
  heat33_lag0<-gam(outcome~heat33_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="poisson")
  heat33_lag1<-gam(outcome~heat33_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="poisson")
  heat33_lag2<-gam(outcome~heat33_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="poisson")
  heat33_lag3<-gam(outcome~heat33_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="poisson")
  heat33_lag4<-gam(outcome~heat33_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="poisson")
  heat33_lag5<-gam(outcome~heat33_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dat,family="poisson")
  heat33_lag6<-gam(outcome~heat33_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dat,family="poisson")
  heat33_lag7<-gam(outcome~heat33_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dat,family="poisson")
  
  
  #result table
  fit.tb0<-as.data.frame(cbind(summary(fit_lag0)$p.table[2:3,],gcv=fit_lag0$gcv.ubre,deviance=fit_lag0$deviance,r_sq=summary(fit_lag0)$r.sq))
  fit.tb1<-as.data.frame(cbind(summary(fit_lag1)$p.table[2:3,],gcv=fit_lag1$gcv.ubre,deviance=fit_lag1$deviance,r_sq=summary(fit_lag1)$r.sq))
  fit.tb2<-as.data.frame(cbind(summary(fit_lag2)$p.table[2:3,],gcv=fit_lag2$gcv.ubre,deviance=fit_lag2$deviance,r_sq=summary(fit_lag2)$r.sq))
  fit.tb3<-as.data.frame(cbind(summary(fit_lag3)$p.table[2:3,],gcv=fit_lag3$gcv.ubre,deviance=fit_lag3$deviance,r_sq=summary(fit_lag3)$r.sq))
  fit.tb4<-as.data.frame(cbind(summary(fit_lag4)$p.table[2:3,],gcv=fit_lag4$gcv.ubre,deviance=fit_lag4$deviance,r_sq=summary(fit_lag4)$r.sq))
  fit.tb5<-as.data.frame(cbind(summary(fit_lag5)$p.table[2:3,],gcv=fit_lag5$gcv.ubre,deviance=fit_lag5$deviance,r_sq=summary(fit_lag5)$r.sq))
  fit.tb6<-as.data.frame(cbind(summary(fit_lag6)$p.table[2:3,],gcv=fit_lag6$gcv.ubre,deviance=fit_lag6$deviance,r_sq=summary(fit_lag6)$r.sq))
  fit.tb7<-as.data.frame(cbind(summary(fit_lag7)$p.table[2:3,],gcv=fit_lag7$gcv.ubre,deviance=fit_lag7$deviance,r_sq=summary(fit_lag7)$r.sq))
  
  fit.tb01<-as.data.frame(cbind(summary(fit_lag01)$p.table[2:3,],gcv=fit_lag01$gcv.ubre,deviance=fit_lag01$deviance,r_sq=summary(fit_lag01)$r.sq))
  fit.tb02<-as.data.frame(cbind(summary(fit_lag02)$p.table[2:3,],gcv=fit_lag02$gcv.ubre,deviance=fit_lag02$deviance,r_sq=summary(fit_lag02)$r.sq))
  fit.tb03<-as.data.frame(cbind(summary(fit_lag03)$p.table[2:3,],gcv=fit_lag03$gcv.ubre,deviance=fit_lag03$deviance,r_sq=summary(fit_lag03)$r.sq))
  fit.tb04<-as.data.frame(cbind(summary(fit_lag04)$p.table[2:3,],gcv=fit_lag04$gcv.ubre,deviance=fit_lag04$deviance,r_sq=summary(fit_lag04)$r.sq))
  fit.tb05<-as.data.frame(cbind(summary(fit_lag05)$p.table[2:3,],gcv=fit_lag05$gcv.ubre,deviance=fit_lag05$deviance,r_sq=summary(fit_lag05)$r.sq))
  fit.tb06<-as.data.frame(cbind(summary(fit_lag06)$p.table[2:3,],gcv=fit_lag06$gcv.ubre,deviance=fit_lag06$deviance,r_sq=summary(fit_lag06)$r.sq))
  fit.tb07<-as.data.frame(cbind(summary(fit_lag07)$p.table[2:3,],gcv=fit_lag07$gcv.ubre,deviance=fit_lag07$deviance,r_sq=summary(fit_lag07)$r.sq))
  
  heat28.tb0<-as.data.frame(cbind(summary(heat28_lag0)$p.table[2:3,],gcv=heat28_lag0$gcv.ubre,deviance=heat28_lag0$deviance,r_sq=summary(heat28_lag0)$r.sq))
  heat28.tb1<-as.data.frame(cbind(summary(heat28_lag1)$p.table[2:3,],gcv=heat28_lag1$gcv.ubre,deviance=heat28_lag1$deviance,r_sq=summary(heat28_lag1)$r.sq))
  heat28.tb2<-as.data.frame(cbind(summary(heat28_lag2)$p.table[2:3,],gcv=heat28_lag2$gcv.ubre,deviance=heat28_lag2$deviance,r_sq=summary(heat28_lag2)$r.sq))
  heat28.tb3<-as.data.frame(cbind(summary(heat28_lag3)$p.table[2:3,],gcv=heat28_lag3$gcv.ubre,deviance=heat28_lag3$deviance,r_sq=summary(heat28_lag3)$r.sq))
  heat28.tb4<-as.data.frame(cbind(summary(heat28_lag4)$p.table[2:3,],gcv=heat28_lag4$gcv.ubre,deviance=heat28_lag4$deviance,r_sq=summary(heat28_lag4)$r.sq))
  heat28.tb5<-as.data.frame(cbind(summary(heat28_lag5)$p.table[2:3,],gcv=heat28_lag5$gcv.ubre,deviance=heat28_lag5$deviance,r_sq=summary(heat28_lag5)$r.sq))
  heat28.tb6<-as.data.frame(cbind(summary(heat28_lag6)$p.table[2:3,],gcv=heat28_lag6$gcv.ubre,deviance=heat28_lag6$deviance,r_sq=summary(heat28_lag6)$r.sq))
  heat28.tb7<-as.data.frame(cbind(summary(heat28_lag7)$p.table[2:3,],gcv=heat28_lag7$gcv.ubre,deviance=heat28_lag7$deviance,r_sq=summary(heat28_lag7)$r.sq))
  
  heat29.tb0<-as.data.frame(cbind(summary(heat29_lag0)$p.table[2:3,],gcv=heat29_lag0$gcv.ubre,deviance=heat29_lag0$deviance,r_sq=summary(heat29_lag0)$r.sq))
  heat29.tb1<-as.data.frame(cbind(summary(heat29_lag1)$p.table[2:3,],gcv=heat29_lag1$gcv.ubre,deviance=heat29_lag1$deviance,r_sq=summary(heat29_lag1)$r.sq))
  heat29.tb2<-as.data.frame(cbind(summary(heat29_lag2)$p.table[2:3,],gcv=heat29_lag2$gcv.ubre,deviance=heat29_lag2$deviance,r_sq=summary(heat29_lag2)$r.sq))
  heat29.tb3<-as.data.frame(cbind(summary(heat29_lag3)$p.table[2:3,],gcv=heat29_lag3$gcv.ubre,deviance=heat29_lag3$deviance,r_sq=summary(heat29_lag3)$r.sq))
  heat29.tb4<-as.data.frame(cbind(summary(heat29_lag4)$p.table[2:3,],gcv=heat29_lag4$gcv.ubre,deviance=heat29_lag4$deviance,r_sq=summary(heat29_lag4)$r.sq))
  heat29.tb5<-as.data.frame(cbind(summary(heat29_lag5)$p.table[2:3,],gcv=heat29_lag5$gcv.ubre,deviance=heat29_lag5$deviance,r_sq=summary(heat29_lag5)$r.sq))
  heat29.tb6<-as.data.frame(cbind(summary(heat29_lag6)$p.table[2:3,],gcv=heat29_lag6$gcv.ubre,deviance=heat29_lag6$deviance,r_sq=summary(heat29_lag6)$r.sq))
  heat29.tb7<-as.data.frame(cbind(summary(heat29_lag7)$p.table[2:3,],gcv=heat29_lag7$gcv.ubre,deviance=heat29_lag7$deviance,r_sq=summary(heat29_lag7)$r.sq))
  
  heat30.tb0<-as.data.frame(cbind(summary(heat30_lag0)$p.table[2:3,],gcv=heat30_lag0$gcv.ubre,deviance=heat30_lag0$deviance,r_sq=summary(heat30_lag0)$r.sq))
  heat30.tb1<-as.data.frame(cbind(summary(heat30_lag1)$p.table[2:3,],gcv=heat30_lag1$gcv.ubre,deviance=heat30_lag1$deviance,r_sq=summary(heat30_lag1)$r.sq))
  heat30.tb2<-as.data.frame(cbind(summary(heat30_lag2)$p.table[2:3,],gcv=heat30_lag2$gcv.ubre,deviance=heat30_lag2$deviance,r_sq=summary(heat30_lag2)$r.sq))
  heat30.tb3<-as.data.frame(cbind(summary(heat30_lag3)$p.table[2:3,],gcv=heat30_lag3$gcv.ubre,deviance=heat30_lag3$deviance,r_sq=summary(heat30_lag3)$r.sq))
  heat30.tb4<-as.data.frame(cbind(summary(heat30_lag4)$p.table[2:3,],gcv=heat30_lag4$gcv.ubre,deviance=heat30_lag4$deviance,r_sq=summary(heat30_lag4)$r.sq))
  heat30.tb5<-as.data.frame(cbind(summary(heat30_lag5)$p.table[2:3,],gcv=heat30_lag5$gcv.ubre,deviance=heat30_lag5$deviance,r_sq=summary(heat30_lag5)$r.sq))
  heat30.tb6<-as.data.frame(cbind(summary(heat30_lag6)$p.table[2:3,],gcv=heat30_lag6$gcv.ubre,deviance=heat30_lag6$deviance,r_sq=summary(heat30_lag6)$r.sq))
  heat30.tb7<-as.data.frame(cbind(summary(heat30_lag7)$p.table[2:3,],gcv=heat30_lag7$gcv.ubre,deviance=heat30_lag7$deviance,r_sq=summary(heat30_lag7)$r.sq))
  
  heat31.tb0<-as.data.frame(cbind(summary(heat31_lag0)$p.table[2:3,],gcv=heat31_lag0$gcv.ubre,deviance=heat31_lag0$deviance,r_sq=summary(heat31_lag0)$r.sq))
  heat31.tb1<-as.data.frame(cbind(summary(heat31_lag1)$p.table[2:3,],gcv=heat31_lag1$gcv.ubre,deviance=heat31_lag1$deviance,r_sq=summary(heat31_lag1)$r.sq))
  heat31.tb2<-as.data.frame(cbind(summary(heat31_lag2)$p.table[2:3,],gcv=heat31_lag2$gcv.ubre,deviance=heat31_lag2$deviance,r_sq=summary(heat31_lag2)$r.sq))
  heat31.tb3<-as.data.frame(cbind(summary(heat31_lag3)$p.table[2:3,],gcv=heat31_lag3$gcv.ubre,deviance=heat31_lag3$deviance,r_sq=summary(heat31_lag3)$r.sq))
  heat31.tb4<-as.data.frame(cbind(summary(heat31_lag4)$p.table[2:3,],gcv=heat31_lag4$gcv.ubre,deviance=heat31_lag4$deviance,r_sq=summary(heat31_lag4)$r.sq))
  heat31.tb5<-as.data.frame(cbind(summary(heat31_lag5)$p.table[2:3,],gcv=heat31_lag5$gcv.ubre,deviance=heat31_lag5$deviance,r_sq=summary(heat31_lag5)$r.sq))
  heat31.tb6<-as.data.frame(cbind(summary(heat31_lag6)$p.table[2:3,],gcv=heat31_lag6$gcv.ubre,deviance=heat31_lag6$deviance,r_sq=summary(heat31_lag6)$r.sq))
  heat31.tb7<-as.data.frame(cbind(summary(heat31_lag7)$p.table[2:3,],gcv=heat31_lag7$gcv.ubre,deviance=heat31_lag7$deviance,r_sq=summary(heat31_lag7)$r.sq))
  
  heat32.tb0<-as.data.frame(cbind(summary(heat32_lag0)$p.table[2:3,],gcv=heat32_lag0$gcv.ubre,deviance=heat32_lag0$deviance,r_sq=summary(heat32_lag0)$r.sq))
  heat32.tb1<-as.data.frame(cbind(summary(heat32_lag1)$p.table[2:3,],gcv=heat32_lag1$gcv.ubre,deviance=heat32_lag1$deviance,r_sq=summary(heat32_lag1)$r.sq))
  heat32.tb2<-as.data.frame(cbind(summary(heat32_lag2)$p.table[2:3,],gcv=heat32_lag2$gcv.ubre,deviance=heat32_lag2$deviance,r_sq=summary(heat32_lag2)$r.sq))
  heat32.tb3<-as.data.frame(cbind(summary(heat32_lag3)$p.table[2:3,],gcv=heat32_lag3$gcv.ubre,deviance=heat32_lag3$deviance,r_sq=summary(heat32_lag3)$r.sq))
  heat32.tb4<-as.data.frame(cbind(summary(heat32_lag4)$p.table[2:3,],gcv=heat32_lag4$gcv.ubre,deviance=heat32_lag4$deviance,r_sq=summary(heat32_lag4)$r.sq))
  heat32.tb5<-as.data.frame(cbind(summary(heat32_lag5)$p.table[2:3,],gcv=heat32_lag5$gcv.ubre,deviance=heat32_lag5$deviance,r_sq=summary(heat32_lag5)$r.sq))
  heat32.tb6<-as.data.frame(cbind(summary(heat32_lag6)$p.table[2:3,],gcv=heat32_lag6$gcv.ubre,deviance=heat32_lag6$deviance,r_sq=summary(heat32_lag6)$r.sq))
  heat32.tb7<-as.data.frame(cbind(summary(heat32_lag7)$p.table[2:3,],gcv=heat32_lag7$gcv.ubre,deviance=heat32_lag7$deviance,r_sq=summary(heat32_lag7)$r.sq))
  
  heat33.tb0<-as.data.frame(cbind(summary(heat33_lag0)$p.table[2:3,],gcv=heat33_lag0$gcv.ubre,deviance=heat33_lag0$deviance,r_sq=summary(heat33_lag0)$r.sq))
  heat33.tb1<-as.data.frame(cbind(summary(heat33_lag1)$p.table[2:3,],gcv=heat33_lag1$gcv.ubre,deviance=heat33_lag1$deviance,r_sq=summary(heat33_lag1)$r.sq))
  heat33.tb2<-as.data.frame(cbind(summary(heat33_lag2)$p.table[2:3,],gcv=heat33_lag2$gcv.ubre,deviance=heat33_lag2$deviance,r_sq=summary(heat33_lag2)$r.sq))
  heat33.tb3<-as.data.frame(cbind(summary(heat33_lag3)$p.table[2:3,],gcv=heat33_lag3$gcv.ubre,deviance=heat33_lag3$deviance,r_sq=summary(heat33_lag3)$r.sq))
  heat33.tb4<-as.data.frame(cbind(summary(heat33_lag4)$p.table[2:3,],gcv=heat33_lag4$gcv.ubre,deviance=heat33_lag4$deviance,r_sq=summary(heat33_lag4)$r.sq))
  heat33.tb5<-as.data.frame(cbind(summary(heat33_lag5)$p.table[2:3,],gcv=heat33_lag5$gcv.ubre,deviance=heat33_lag5$deviance,r_sq=summary(heat33_lag5)$r.sq))
  heat33.tb6<-as.data.frame(cbind(summary(heat33_lag6)$p.table[2:3,],gcv=heat33_lag6$gcv.ubre,deviance=heat33_lag6$deviance,r_sq=summary(heat33_lag6)$r.sq))
  heat33.tb7<-as.data.frame(cbind(summary(heat33_lag7)$p.table[2:3,],gcv=heat33_lag7$gcv.ubre,deviance=heat33_lag7$deviance,r_sq=summary(heat33_lag7)$r.sq))
  
  fit.tb0$label=row.names(fit.tb0);fit.tb1$label=row.names(fit.tb1)
  fit.tb2$label=row.names(fit.tb2);fit.tb3$label=row.names(fit.tb3)
  fit.tb4$label=row.names(fit.tb4);fit.tb5$label=row.names(fit.tb5)
  fit.tb6$label=row.names(fit.tb6);fit.tb7$label=row.names(fit.tb7)
  
  fit.tb01$label=row.names(fit.tb01);fit.tb02$label=row.names(fit.tb02)
  fit.tb03$label=row.names(fit.tb03);fit.tb04$label=row.names(fit.tb04)
  fit.tb05$label=row.names(fit.tb05);fit.tb06$label=row.names(fit.tb06)
  fit.tb07$label=row.names(fit.tb07)
  
  heat28.tb0$label=row.names(heat28.tb0);heat28.tb1$label=row.names(heat28.tb1)
  heat28.tb2$label=row.names(heat28.tb2);heat28.tb3$label=row.names(heat28.tb3)
  heat28.tb4$label=row.names(heat28.tb4);heat28.tb5$label=row.names(heat28.tb5)
  heat28.tb6$label=row.names(heat28.tb6);heat28.tb7$label=row.names(heat28.tb7)
  
  heat29.tb0$label=row.names(heat29.tb0);heat29.tb1$label=row.names(heat29.tb1)
  heat29.tb2$label=row.names(heat29.tb2);heat29.tb3$label=row.names(heat29.tb3)
  heat29.tb4$label=row.names(heat29.tb4);heat29.tb5$label=row.names(heat29.tb5)
  heat29.tb6$label=row.names(heat29.tb6);heat29.tb7$label=row.names(heat29.tb7)
  
  heat30.tb0$label=row.names(heat30.tb0);heat30.tb1$label=row.names(heat30.tb1)
  heat30.tb2$label=row.names(heat30.tb2);heat30.tb3$label=row.names(heat30.tb3)
  heat30.tb4$label=row.names(heat30.tb4);heat30.tb5$label=row.names(heat30.tb5)
  heat30.tb6$label=row.names(heat30.tb6);heat30.tb7$label=row.names(heat30.tb7)
  
  heat31.tb0$label=row.names(heat31.tb0);heat31.tb1$label=row.names(heat31.tb1)
  heat31.tb2$label=row.names(heat31.tb2);heat31.tb3$label=row.names(heat31.tb3)
  heat31.tb4$label=row.names(heat31.tb4);heat31.tb5$label=row.names(heat31.tb5)
  heat31.tb6$label=row.names(heat31.tb6);heat31.tb7$label=row.names(heat31.tb7)
  
  heat32.tb0$label=row.names(heat32.tb0);heat32.tb1$label=row.names(heat32.tb1)
  heat32.tb2$label=row.names(heat32.tb2);heat32.tb3$label=row.names(heat32.tb3)
  heat32.tb4$label=row.names(heat32.tb4);heat32.tb5$label=row.names(heat32.tb5)
  heat32.tb6$label=row.names(heat32.tb6);heat32.tb7$label=row.names(heat32.tb7)
  
  heat33.tb0$label=row.names(heat33.tb0);heat33.tb1$label=row.names(heat33.tb1)
  heat33.tb2$label=row.names(heat33.tb2);heat33.tb3$label=row.names(heat33.tb3)
  heat33.tb4$label=row.names(heat33.tb4);heat33.tb5$label=row.names(heat33.tb5)
  heat33.tb6$label=row.names(heat33.tb6);heat33.tb7$label=row.names(heat33.tb7)
  
  
  fit.tb.single<-rbind(fit.tb0,fit.tb1,fit.tb2,fit.tb3,fit.tb4,fit.tb5,fit.tb6,fit.tb7)
  fit.tb.moving<-rbind(fit.tb01,fit.tb02,fit.tb03,fit.tb04,fit.tb05,fit.tb06,fit.tb07)
  
  heat28.tb.single<-rbind(heat28.tb0,heat28.tb1,heat28.tb2,heat28.tb3,heat28.tb4,heat28.tb5,heat28.tb6,heat28.tb7)
  heat29.tb.single<-rbind(heat29.tb0,heat29.tb1,heat29.tb2,heat29.tb3,heat29.tb4,heat29.tb5,heat29.tb6,heat29.tb7)
  heat30.tb.single<-rbind(heat30.tb0,heat30.tb1,heat30.tb2,heat30.tb3,heat30.tb4,heat30.tb5,heat30.tb6,heat30.tb7)
  heat31.tb.single<-rbind(heat31.tb0,heat31.tb1,heat31.tb2,heat31.tb3,heat31.tb4,heat31.tb5,heat31.tb6,heat31.tb7)
  heat32.tb.single<-rbind(heat32.tb0,heat32.tb1,heat32.tb2,heat32.tb3,heat32.tb4,heat32.tb5,heat32.tb6,heat32.tb7)
  heat33.tb.single<-rbind(heat33.tb0,heat33.tb1,heat33.tb2,heat33.tb3,heat33.tb4,heat33.tb5,heat33.tb6,heat33.tb7)
  
  fit.tb.single$gubun="single"
  fit.tb.moving$gubun="moving"
  
  heat28.tb.single$gubun="single"
  heat29.tb.single$gubun="single"
  heat30.tb.single$gubun="single"
  heat31.tb.single$gubun="single"
  heat32.tb.single$gubun="single"
  heat33.tb.single$gubun="single"
  
  fit.tb.single$lag=paste0("lag",1:8-1)
  fit.tb.moving$lag=paste0("lag0",1:7)
  
  heat28.tb.single$lag=paste0("lag",1:8-1)
  heat29.tb.single$lag=paste0("lag",1:8-1)
  heat30.tb.single$lag=paste0("lag",1:8-1)
  heat31.tb.single$lag=paste0("lag",1:8-1)
  heat32.tb.single$lag=paste0("lag",1:8-1)
  heat33.tb.single$lag=paste0("lag",1:8-1)
  
  fit.tb.single$exposure=c("maxT","pm25")
  fit.tb.moving$exposure=c("maxT","pm25")
  heat28.tb.single$exposure=c("heat28","pm25")
  heat29.tb.single$exposure=c("heat29","pm25")
  heat30.tb.single$exposure=c("heat30","pm25")
  heat31.tb.single$exposure=c("heat31","pm25")
  heat32.tb.single$exposure=c("heat32","pm25")
  heat33.tb.single$exposure=c("heat33","pm25")
  
  res<-rbind(fit.tb.single,fit.tb.moving,heat28.tb.single,heat29.tb.single,
             heat30.tb.single,heat31.tb.single,heat32.tb.single,heat33.tb.single)
  res$sido=unique(dat$area);res}

heatdat2<-subset(heatdat,month %in% c(6,7,8))

s01<-subset(heatdat2,area=="서울");s02<-subset(heatdat2,area=="부산")
s03<-subset(heatdat2,area=="대구");s04<-subset(heatdat2,area=="인천")
s05<-subset(heatdat2,area=="광주");s06<-subset(heatdat2,area=="대전")
s07<-subset(heatdat2,area=="울산");s08<-subset(heatdat2,area=="경기")
s09<-subset(heatdat2,area=="강원");s10<-subset(heatdat2,area=="충북")
s11<-subset(heatdat2,area=="충남");s12<-subset(heatdat2,area=="전북")
s13<-subset(heatdat2,area=="전남");s14<-subset(heatdat2,area=="경북")
s15<-subset(heatdat2,area=="경남")

heat_tb<-function(data){
  dd<-data
  df28<-as.data.frame(rbind(table(dd$heat28_lag0),table(dd$heat28_lag1),table(dd$heat28_lag2),table(dd$heat28_lag3),
                            table(dd$heat28_lag4),table(dd$heat28_lag5),table(dd$heat28_lag6),table(dd$heat28_lag7)))
  df29<-as.data.frame(rbind(table(dd$heat29_lag0),table(dd$heat29_lag1),table(dd$heat29_lag2),table(dd$heat29_lag3),
                            table(dd$heat29_lag4),table(dd$heat29_lag5),table(dd$heat29_lag6),table(dd$heat29_lag7)))
  df30<-as.data.frame(rbind(table(dd$heat30_lag0),table(dd$heat30_lag1),table(dd$heat30_lag2),table(dd$heat30_lag3),
                            table(dd$heat30_lag4),table(dd$heat30_lag5),table(dd$heat30_lag6),table(dd$heat30_lag7)))
  df31<-as.data.frame(rbind(table(dd$heat31_lag0),table(dd$heat31_lag1),table(dd$heat31_lag2),table(dd$heat31_lag3),
                            table(dd$heat31_lag4),table(dd$heat31_lag5),table(dd$heat31_lag6),table(dd$heat31_lag7)))
  df32<-as.data.frame(rbind(table(dd$heat32_lag0),table(dd$heat32_lag1),table(dd$heat32_lag2),table(dd$heat32_lag3),
                            table(dd$heat32_lag4),table(dd$heat32_lag5),table(dd$heat32_lag6),table(dd$heat32_lag7)))
  df33<-as.data.frame(rbind(table(dd$heat33_lag0),table(dd$heat33_lag1),table(dd$heat33_lag2),table(dd$heat33_lag3),
                            table(dd$heat33_lag4),table(dd$heat33_lag5),table(dd$heat33_lag6),table(dd$heat33_lag7)))
  
  cbind(df28,df29,df30,df31,df32,df33)}

heat_tb01<-heat_tb(s01);heat_tb01$label="seoul"
heat_tb02<-heat_tb(s02);heat_tb02$label="busan"
heat_tb03<-heat_tb(s03);heat_tb03$label="daegu"
heat_tb04<-heat_tb(s04);heat_tb04$label="incheon"
heat_tb05<-heat_tb(s05);heat_tb05$label="gwangju"
heat_tb06<-heat_tb(s06);heat_tb06$label="daejeon"
heat_tb07<-heat_tb(s07);heat_tb07$label="ulsan"
heat_tb08<-heat_tb(s08);heat_tb08$label="Gyeeonggi-do"
heat_tb09<-heat_tb(s09);heat_tb09$label="Gangwon-do"
heat_tb10<-heat_tb(s10);heat_tb10$label="Chungcheongbuk-do"
heat_tb11<-heat_tb(s11);heat_tb11$label="Chungcheongnam-do"
heat_tb12<-heat_tb(s12);heat_tb12$label="Jeollabuk-do"
heat_tb13<-heat_tb(s13);heat_tb13$label="Jeollanam-do"
heat_tb14<-heat_tb(s14);heat_tb14$label="Gyeongsangbuk-do"
heat_tb15<-heat_tb(s15);heat_tb15$label="Gyeongsangnam-do"

sido.heat_tb<-rbind(heat_tb01,heat_tb02,heat_tb03,heat_tb04,heat_tb05,
                    heat_tb06,heat_tb07,heat_tb08,heat_tb09,heat_tb10,
                    heat_tb11,heat_tb12,heat_tb13,heat_tb14,heat_tb15)

write.csv(sido.heat_tb,file="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\도시별_폭염정의별빈도.csv",row.names=F,na="")

#시도별 모델링
sido01<-sido.gam(s01);sido02<-sido.gam(s02);sido03<-sido.gam(s03)
sido04<-sido.gam(s04);sido05<-sido.gam(s05);sido06<-sido.gam(s06)
sido07<-sido.gam(s07);sido08<-sido.gam(s08);sido09<-sido.gam(s09)
sido10<-sido.gam(s10);sido11<-sido.gam(s11);sido12<-sido.gam(s12)
sido13<-sido.gam(s13);sido14<-sido.gam(s14);sido15<-sido.gam(s15)

sido.tb<-rbind(sido01,sido02,sido03,sido04,sido05,sido06,sido07,sido08,
               sido09,sido10,sido11,sido12,sido13,sido14,sido15)

write.csv(sido.tb,file="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\온열질환_시도별_TS결과.csv",row.names=F,na="")

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

#Second stage, 메타분석
tt.sido.tb<-read.csv("D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\온열질환_시도별_TS결과.csv")

tt.sido.tb_m<-read.csv("D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\온열질환_시도별_TS결과_남.csv")
tt.sido.tb_f<-read.csv("D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\온열질환_시도별_TS결과_여.csv")

tt.sido.tb$SE  =tt.sido.tb$Std..Error
tt.sido.tb_m$SE=tt.sido.tb_m$Std..Error
tt.sido.tb_f$SE=tt.sido.tb_f$Std..Error

meta_func<-function(dataset){
  uni0 <- with(dataset %>% filter(label=="maxtemp_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni1 <- with(dataset %>% filter(label=="maxtemp_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni2 <- with(dataset %>% filter(label=="maxtemp_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni3 <- with(dataset %>% filter(label=="maxtemp_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni4 <- with(dataset %>% filter(label=="maxtemp_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni5 <- with(dataset %>% filter(label=="maxtemp_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni6 <- with(dataset %>% filter(label=="maxtemp_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni7 <- with(dataset %>% filter(label=="maxtemp_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.uni<-as.data.frame(rbind(with(uni0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.uni$lag=paste0("lag",1:8-1)
  single.uni$exposure="maxT"
  
  uni01 <- with(dataset %>% filter(label=="maxtemp_lag01"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni02 <- with(dataset %>% filter(label=="maxtemp_lag02"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni03 <- with(dataset %>% filter(label=="maxtemp_lag03"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni04 <- with(dataset %>% filter(label=="maxtemp_lag04"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni05 <- with(dataset %>% filter(label=="maxtemp_lag05"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni06 <- with(dataset %>% filter(label=="maxtemp_lag06"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni07 <- with(dataset %>% filter(label=="maxtemp_lag07"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  moving.uni<-as.data.frame(rbind(with(uni01,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni02,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni03,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni04,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni05,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni06,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni07,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  moving.uni$lag=paste0("lag0",1:7)
  moving.uni$exposure="maxT"
  
  heat28_0 <- with(dataset %>% filter(label=="heat28_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_1 <- with(dataset %>% filter(label=="heat28_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_2 <- with(dataset %>% filter(label=="heat28_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_3 <- with(dataset %>% filter(label=="heat28_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_4 <- with(dataset %>% filter(label=="heat28_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_5 <- with(dataset %>% filter(label=="heat28_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_6 <- with(dataset %>% filter(label=="heat28_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_7 <- with(dataset %>% filter(label=="heat28_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.heat28<-as.data.frame(rbind(with(heat28_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.heat28$lag=paste0("lag",1:8-1)
  single.heat28$exposure="heat28"
  
  heat29_0 <- with(dataset %>% filter(label=="heat29_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_1 <- with(dataset %>% filter(label=="heat29_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_2 <- with(dataset %>% filter(label=="heat29_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_3 <- with(dataset %>% filter(label=="heat29_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_4 <- with(dataset %>% filter(label=="heat29_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_5 <- with(dataset %>% filter(label=="heat29_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_6 <- with(dataset %>% filter(label=="heat29_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_7 <- with(dataset %>% filter(label=="heat29_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.heat29<-as.data.frame(rbind(with(heat29_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.heat29$lag=paste0("lag",1:8-1)
  single.heat29$exposure="heat29"
  
  heat30_0 <- with(dataset %>% filter(label=="heat30_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_1 <- with(dataset %>% filter(label=="heat30_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_2 <- with(dataset %>% filter(label=="heat30_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_3 <- with(dataset %>% filter(label=="heat30_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_4 <- with(dataset %>% filter(label=="heat30_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_5 <- with(dataset %>% filter(label=="heat30_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_6 <- with(dataset %>% filter(label=="heat30_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_7 <- with(dataset %>% filter(label=="heat30_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.heat30<-as.data.frame(rbind(with(heat30_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.heat30$lag=paste0("lag",1:8-1)
  single.heat30$exposure="heat30"
  
  heat31_0 <- with(dataset %>% filter(label=="heat31_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_1 <- with(dataset %>% filter(label=="heat31_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_2 <- with(dataset %>% filter(label=="heat31_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_3 <- with(dataset %>% filter(label=="heat31_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_4 <- with(dataset %>% filter(label=="heat31_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_5 <- with(dataset %>% filter(label=="heat31_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_6 <- with(dataset %>% filter(label=="heat31_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_7 <- with(dataset %>% filter(label=="heat31_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.heat31<-as.data.frame(rbind(with(heat31_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.heat31$lag=paste0("lag",1:8-1)
  single.heat31$exposure="heat31"
  
  heat32_0 <- with(dataset %>% filter(label=="heat32_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_1 <- with(dataset %>% filter(label=="heat32_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_2 <- with(dataset %>% filter(label=="heat32_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_3 <- with(dataset %>% filter(label=="heat32_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_4 <- with(dataset %>% filter(label=="heat32_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_5 <- with(dataset %>% filter(label=="heat32_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_6 <- with(dataset %>% filter(label=="heat32_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_7 <- with(dataset %>% filter(label=="heat32_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.heat32<-as.data.frame(rbind(with(heat32_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.heat32$lag=paste0("lag",1:8-1)
  single.heat32$exposure="heat32"
  
  heat33_0 <- with(dataset %>% filter(label=="heat33_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_1 <- with(dataset %>% filter(label=="heat33_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_2 <- with(dataset %>% filter(label=="heat33_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_3 <- with(dataset %>% filter(label=="heat33_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_4 <- with(dataset %>% filter(label=="heat33_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_5 <- with(dataset %>% filter(label=="heat33_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_6 <- with(dataset %>% filter(label=="heat33_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_7 <- with(dataset %>% filter(label=="heat33_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.heat33<-as.data.frame(rbind(with(heat33_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.heat33$lag=paste0("lag",1:8-1)
  single.heat33$exposure="heat33"
  
  rbind(single.uni,moving.uni,
        single.heat28,single.heat29,
        single.heat30,single.heat31,
        single.heat32,single.heat33)
}

meta1<-meta_func(tt.sido.tb) #온열질환
meta_m<-meta_func(tt.sido.tb_m) #온열질환
meta_f<-meta_func(tt.sido.tb_f) #온열질환

meta1$RR =exp(meta1$beta)
meta1$lci=exp(meta1$beta-1.96*meta1$se)
meta1$uci=exp(meta1$beta+1.96*meta1$se)

meta_m$RR =exp(meta_m$beta)
meta_m$lci=exp(meta_m$beta-1.96*meta_m$se)
meta_m$uci=exp(meta_m$beta+1.96*meta_m$se)

meta_f$RR =exp(meta_f$beta)
meta_f$lci=exp(meta_f$beta-1.96*meta_f$se)
meta_f$uci=exp(meta_f$beta+1.96*meta_f$se)

meta_m$sex="Boys"
meta_f$sex="Girls"

meta_sex<-rbind(meta_m,meta_f)
meta_sex$sex=factor(meta_sex$sex,levels=unique(meta_sex$sex))


meta2.r<-subset(meta_sex,exposure!="maxT")

# write.csv(meta1,file="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\meta_reulst_heatrelated.csv",row.names=F,na="")
# write.csv(meta_sex,file="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\meta_reulst_heatrelated_sex.csv",row.names=F,na="")

meta1.r<-subset(meta1,exposure=="maxT")
meta1.r$category=c(rep("Single lag",8),rep("Moving average",7))

meta1.1<-subset(meta1.r,category=="Single lag")
meta1.2<-subset(meta1.r,category=="Moving average")

meta2.r<-subset(meta_sex,exposure=="maxT")
meta2.r$category=c(rep("Single lag",8),rep("Moving average",7))

meta2.1<-subset(meta2.r,category=="Single lag")
meta2.2<-subset(meta2.r,category=="Moving average")

#전체 메타,단일지연
gs1<-ggplot(meta1.1,aes(lag,RR))+geom_point(size=6)+theme_bw(base_size=25)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,lwd=1.1)+facet_wrap(~category)+labs(x="",y="Relative Risk (95% CI)")+
  geom_hline(yintercept=1,col="black",linetype=2)+coord_cartesian(ylim=c(0.99,1.04))+
  geom_text(aes(lag,uci+0.003),label=round(meta1.1$RR,3),size=4)+
  geom_text(aes(lag,uci+0.002),label=paste0("(",round(meta1.1$lci,3),", ",round(meta1.1$uci,3),")"),size=4)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#전체 메타,이동평균
gm1<-ggplot(meta1.2,aes(lag,RR))+geom_point(size=6)+theme_bw(base_size=25)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,lwd=1.1)+facet_wrap(~category)+labs(x="",y="")+
  geom_hline(yintercept=1,col="black",linetype=2)+coord_cartesian(ylim=c(0.99,1.04))+
  geom_text(aes(lag,uci+0.003),label=round(meta1.2$RR,3),size=4)+
  geom_text(aes(lag,uci+0.002),label=paste0("(",round(meta1.2$lci,3),", ",round(meta1.2$uci,3),")"),size=4)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

x11();grid.arrange(gs1,gm1,ncol=2)

##GAMM,노출 반응 그림저장, Meta_온열질환(전체)_PM25보정_MaxT
tiff(file="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\Figure\\Meta_온열질환(전체)_PM25보정_MaxT.tiff",
     width=6000, height=3000,res=300)
grid.arrange(gs1,gm1,ncol=2)
dev.off()

#성별 메타,단일지연
meta2.1
gsex<-ggplot(meta2.1,aes(lag,RR,fill=sex))+
  geom_point(size=6,aes(shape=sex,col=sex),position=position_dodge(0.5))+
  theme_bw(base_size=25)+
  geom_errorbar(aes(ymin=lci,ymax=uci),col=rep(c("#2b61ff","#f94580"),each=8),width=0.2,lwd=1.1,position=position_dodge(0.5))+
  labs(x="",y="Relative Risk (95% CI)")+
  geom_hline(yintercept=1,col="black",linetype=2)+coord_cartesian(ylim=c(0.99,1.04))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.title=element_blank())+scale_fill_manual(values = c(1,2))+
  scale_color_manual(values=c("#2b61ff","#f94580"))

x11();gsex

##GAMM,노출 반응 그림저장, Meta_온열질환(전체)_PM25보정_성별
tiff(file="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\Figure\\Meta_온열질환(전체)_PM25보정_성별.tiff",
     width=6000, height=3000,res=300)
gsex
dev.off()

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
library(forestplot)

#자료불러오기 
setwd("D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\")
d1<-read_excel("D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\KCDC_Heatwave_children_results.xlsx",sheet="메타_온열질환전체") #Sheet별로 변경 

#테이블 정렬 하기위해 제목 아래 한칸씩 띄어쓸 부분 지정 
subgps <- c(2:9,12:19,22:29,32:39,42:49,52:59)
d1$value[subgps] <- paste("  ",d1$value[subgps]) 

d1$rr_text=paste0(round(d1$RR,2),", 95% CI:",round(d1$LCI,2),", ",round(d1$UCI,2))
d1$rr_text=paste0(round(d1$RR,2))
d1$CI95   =paste0(as.character(round(d1$LCI,2)),", ",as.character(round(d1$UCI,2)))

#제목에 쓰인 부분은 지우기 
d1[grep("Heatwave",d1$value),]$rr_text=NA
d1[grep("Heatwave",d1$value),]$CI95   =NA
d1$rr_text=ifelse(d1$TF==0,NA,d1$rr_text)
d1$CI95=ifelse(d1$TF==0,NA,d1$CI95)

grep("lag7",d1$value)


d1.revise<-d1[-grep("lag7",d1$value),]

#그림에 표현할 텍스트
tabletext <- cbind(c("Categories","\n",d1.revise$value),
                   c("Relative Risk","\n",d1.revise$rr_text),
                   c("95% CI","\n",d1.revise$CI95))



#forestplot() 내에 옵션 fpColors은 열로 묶인 자료에 대해서(그룹별 자료) 일괄적으로 색깔지정
#유의한 그룹/ 유의하지 않은 그룹으로 지정해서 열로 묶기
est<-as.data.frame(cbind(RR=c(NA,NA,d1.revise$RR),lci=c(NA,NA,d1.revise$LCI),uci=c(NA,NA,d1.revise$UCI)))
signi<-ifelse(est$lci>1,1,0)
signi[is.na(signi)]<-0
est$signi=signi

est1<-est;est2<-est

est1$RR =with(est1,ifelse(signi==0,NA,RR))
est1$lci=with(est1,ifelse(signi==0,NA,lci))
est1$uci=with(est1,ifelse(signi==0,NA,uci))

est2$RR =with(est2,ifelse(signi==1 ,NA,RR))
est2$lci=with(est2,ifelse(signi==1,NA,lci))
est2$uci=with(est2,ifelse(signi==1,NA,uci))


x11();forestplot(labeltext=tabletext, graph.pos=4, 
                 mean =cbind(est1$RR,est2$RR), 
                 lower=cbind(est1$lci,est2$lci),
                 upper=cbind(est1$uci,est2$uci),
                 title="",
                 xlab="Relative risk (95% Confidence intervals)",
                 txt_gp=fpTxtGp(label=gpar(cex=1.2),
                                ticks=gpar(cex=1.1),
                                xlab=gpar(cex = 1.1),
                                title=gpar(cex = 1.4)),
                 col=fpColors(box=c("red","black"), lines=c("red","black"), zero = "gray50"),
                 xticks=c(0.9,1,1.2),
                 zero=1, cex=0.8, lineheight = "auto", boxsize=0.4, colgap=unit(5,"mm"),
                 lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2,)


#Save the current figures
tiff(filename="D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\Figure\\forestplot_tot.tiff",width=3600,height=4200,res=300)
forestplot(labeltext=tabletext, graph.pos=4, 
           mean =cbind(est1$RR,est2$RR), 
           lower=cbind(est1$lci,est2$lci),
           upper=cbind(est1$uci,est2$uci),
           title="",
           xlab="Relative risk (95% Confidence intervals)",
           txt_gp=fpTxtGp(label=gpar(cex=1.2),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.1),
                          title=gpar(cex = 1.4)),
           col=fpColors(box=c("red","black"), lines=c("red","black"), zero = "gray50"),
           xticks=c(0.9,1,1.2),
           zero=1, cex=0.8, lineheight = "auto", boxsize=0.4, colgap=unit(5,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2,)

dev.off()









