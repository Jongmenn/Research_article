#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#
#라이브러리
library(ggplot2)  ;library(dplyr)
library(lubridate);library(geepack)
library(MatchIt)  ;library(optmatch)
library(stringr)  ;library(pROC)
library(sandwich) ;library(lmtest)
library(survival)
#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#
#working directory
setwd("C:\\Users\\Administrator\\Desktop\\WHL\\data")
dat<-read.csv("final_data.csv")

#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#
#변수정리 
dat$DLV_DT=ymd(dat$DLV_DT)
dat[is.na(dat$multi),]$multi<-9
addmargins(table(dat$multi))

#가구소득분류 정리, missing은 결측으로 유지 
dat$ses=NA
dat_ses1<-subset(dat,SES05 %in% c(0,1,2,11,12))
dat_ses2<-subset(dat,SES05 %in% c(3,4,13,14))
dat_ses3<-subset(dat,SES05 %in% c(5,15))
dat_ses4<-subset(dat,!SES05 %in% c(0,1,2,3,4,5,11,12,13,14,15))

dat_ses1$ses=1
dat_ses2$ses=2
dat_ses3$ses=3
dat_ses4$ses=9

dat<-rbind(dat_ses1,dat_ses2,dat_ses3,dat_ses4)

dat$parity_g=ifelse(dat$PARITY>=2,1,0)
dat$COVID_PERIOD=factor(dat$COVID_PERIOD,levels=c("Pre delta","Delta","Omicron"))

#--------------------------------------------------------------------------------------#
#첫 코로나 확진(1/20일)보다 이전에 분만 기록 있는 대상자 제외
dat$keep=ifelse(dat$DLV_DT-as.Date("2020-01-20")<0,1,0)
dat2<-dat %>% filter(keep==0)

#GA_wk, sex_t
addmargins(table(dat2$multi))
addmargins(table(dat2$ses))          #가입자구분별 소득분위
addmargins(table(dat2$FOREIGNER_YN)) #외국인

table(dat2$COVID_PERIOD)
#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
#각 질환별 COVID19 감염에 따른 빈도 
s01<-subset(dat2,COVID_PERIOD=="Pre delta")
s02<-subset(dat2,COVID_PERIOD=="Delta")
s03<-subset(dat2,COVID_PERIOD=="Omicron")

s01<-s01 %>% select(OUT1:OUT16,Out17,OUT20,OUT21,OUT22:OUT31,CHILD_ID:multi,PARITY:parity_g)
s02<-s02 %>% select(OUT1:OUT16,Out17,OUT20,OUT21,OUT22:OUT31,CHILD_ID:multi,PARITY:parity_g)
s03<-s03 %>% select(OUT1:OUT16,Out17,OUT20,OUT21,OUT22:OUT31,CHILD_ID:multi,PARITY:parity_g)


t.list1=NULL;t.list2=NULL;t.list3=NULL

for(i in c(1:29)){
  v11<-ifelse(s01$COVID19==0 & s01[,i]==0,1,0) %>% sum
  v12<-ifelse(s01$COVID19==0 & s01[,i]==1,1,0) %>% sum
  v13<-ifelse(s01$COVID19==1 & s01[,i]==0,1,0) %>% sum
  v14<-ifelse(s01$COVID19==1 & s01[,i]==1,1,0) %>% sum
  
  v21<-ifelse(s02$COVID19==0 & s02[,i]==0,1,0) %>% sum
  v22<-ifelse(s02$COVID19==0 & s02[,i]==1,1,0) %>% sum
  v23<-ifelse(s02$COVID19==1 & s02[,i]==0,1,0) %>% sum
  v24<-ifelse(s02$COVID19==1 & s02[,i]==1,1,0) %>% sum
  
  v31<-ifelse(s03$COVID19==0 & s03[,i]==0,1,0) %>% sum
  v32<-ifelse(s03$COVID19==0 & s03[,i]==1,1,0) %>% sum
  v33<-ifelse(s03$COVID19==1 & s03[,i]==0,1,0) %>% sum
  v34<-ifelse(s03$COVID19==1 & s03[,i]==1,1,0) %>% sum
  
  RR1=((v14)/(v13+v14))/((v12)/(v11+v12))
  RR2=((v24)/(v23+v24))/((v22)/(v21+v22))
  RR3=((v34)/(v33+v34))/((v32)/(v31+v32))
  
  t.list1[[i]]<-data.frame(v1=v11,v2=v12,v3=v13,v4=v14,label=names(s01)[i],RR=RR1,period="Pre delta")
  t.list2[[i]]<-data.frame(v1=v21,v2=v22,v3=v23,v4=v24,label=names(s02)[i],RR=RR2,period="Delta")
  t.list3[[i]]<-data.frame(v1=v31,v2=v32,v3=v33,v4=v34,label=names(s03)[i],RR=RR3,period="Omicron")
  
  print(i)
}

t.s01<-as.data.frame(do.call(rbind,t.list1))
t.s02<-as.data.frame(do.call(rbind,t.list2))
t.s03<-as.data.frame(do.call(rbind,t.list3))


zz<-rbind(t.s01,t.s02,t.s03)
write.csv(zz,file="zz.csv",row.names=F,na="")
d<-as.data.frame(aggregate(s01[,1]~COVID19,data=s01,table))
#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
#결과변수 병합하고 보기 
#결과1: 모든 질환 중 하나라도 해당하면 1, 아니면 0 
s01$Outcome1<-ifelse(apply(s01 %>% select(OUT1:OUT31),1,sum)>=1,1,0)
s02$Outcome1<-ifelse(apply(s02 %>% select(OUT1:OUT31),1,sum)>=1,1,0)
s03$Outcome1<-ifelse(apply(s03 %>% select(OUT1:OUT31),1,sum)>=1,1,0)

#결과2: 산모질환만 (조산 고려)
s01$Outcome2<-ifelse(apply(s01 %>% select(OUT1:OUT11,Out17),1,sum)>=1,1,0)
s02$Outcome2<-ifelse(apply(s02 %>% select(OUT1:OUT11,Out17),1,sum)>=1,1,0)
s03$Outcome2<-ifelse(apply(s03 %>% select(OUT1:OUT11,Out17),1,sum)>=1,1,0)

#결과3: 아이질환만
s01$Outcome3<-ifelse(apply(s01 %>% select(OUT12,OUT13,OUT15,OUT22:OUT31),1,sum)>=1,1,0)
s02$Outcome3<-ifelse(apply(s02 %>% select(OUT12,OUT13,OUT15,OUT22:OUT31),1,sum)>=1,1,0)
s03$Outcome3<-ifelse(apply(s03 %>% select(OUT12,OUT13,OUT15,OUT22:OUT31),1,sum)>=1,1,0)


o1<-as.data.frame(rbind(addmargins(table(s01$Outcome1)),
                        addmargins(table(s02$Outcome1)),
                        addmargins(table(s03$Outcome1))))
o2<-as.data.frame(rbind(addmargins(table(s01$Outcome2)),
                        addmargins(table(s02$Outcome2)),
                        addmargins(table(s03$Outcome2))))
o3<-as.data.frame(rbind(addmargins(table(s01$Outcome3)),
                        addmargins(table(s02$Outcome3)),
                        addmargins(table(s03$Outcome3))))

144:146

t.list1=NULL;t.list2=NULL;t.list3=NULL

for(i in c(145:147)){
  v11<-ifelse(s01$COVID19==0 & s01[,i]==0,1,0) %>% sum
  v12<-ifelse(s01$COVID19==0 & s01[,i]==1,1,0) %>% sum
  v13<-ifelse(s01$COVID19==1 & s01[,i]==0,1,0) %>% sum
  v14<-ifelse(s01$COVID19==1 & s01[,i]==1,1,0) %>% sum
  
  v21<-ifelse(s02$COVID19==0 & s02[,i]==0,1,0) %>% sum
  v22<-ifelse(s02$COVID19==0 & s02[,i]==1,1,0) %>% sum
  v23<-ifelse(s02$COVID19==1 & s02[,i]==0,1,0) %>% sum
  v24<-ifelse(s02$COVID19==1 & s02[,i]==1,1,0) %>% sum
  
  v31<-ifelse(s03$COVID19==0 & s03[,i]==0,1,0) %>% sum
  v32<-ifelse(s03$COVID19==0 & s03[,i]==1,1,0) %>% sum
  v33<-ifelse(s03$COVID19==1 & s03[,i]==0,1,0) %>% sum
  v34<-ifelse(s03$COVID19==1 & s03[,i]==1,1,0) %>% sum
  
  RR1=((v14)/(v13+v14))/((v12)/(v11+v12))
  RR2=((v24)/(v23+v24))/((v22)/(v21+v22))
  RR3=((v34)/(v33+v34))/((v32)/(v31+v32))
  
  t.list1[[i]]<-data.frame(v1=v11,v2=v12,v3=v13,v4=v14,label=names(s01)[i],RR=RR1,period="Pre delta")
  t.list2[[i]]<-data.frame(v1=v21,v2=v22,v3=v23,v4=v24,label=names(s02)[i],RR=RR2,period="Delta")
  t.list3[[i]]<-data.frame(v1=v31,v2=v32,v3=v33,v4=v34,label=names(s03)[i],RR=RR3,period="Omicron")
  
  print(i)
}

zz<-rbind(do.call(rbind,t.list1),
          do.call(rbind,t.list2),
          do.call(rbind,t.list3))

write.csv(zz,file="zzz.csv",row.names=F,na="")

#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
addmargins(table(s01$COVID19))
addmargins(table(s02$COVID19))
addmargins(table(s03$COVID19))

fit1<-glm(Outcome1~COVID19+YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
            BD19+BD21+factor(multi)+factor(ses),data=s01,family="binomial")

fit2<-glm(Outcome1~COVID19+YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
            FULLY_PRE_COVID_VAC+BD19+BD21+factor(multi)+factor(ses),data=s02,family="binomial")

fit3<-glm(Outcome1~COVID19+YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
            FULLY_PRE_COVID_VAC+BD19+BD21+factor(multi)+factor(ses),data=s03,family="binomial")

summary(fit1)
addmargins(table(dat2$FULLY_PRE_COVID_VAC,dat2$COVID19))

res<-as.data.frame(rbind(summary(fit1)$coeff[2,],
                         summary(fit2)$coeff[2,],
                         summary(fit3)$coeff[2,]))

res$period=c("Pre delta","Delta","Omicron")
res$OR    =exp(res$Estimate)
res$lci   =exp(res$Estimate-1.96*res$`Std. Error`)
res$uci   =exp(res$Estimate+1.96*res$`Std. Error`)

res<-res %>% dplyr:: select(period,Estimate:`Pr(>|z|)`,OR:uci)

write.csv(res,file="res.csv",row.names=F,na="")

#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
#변수 초기 고정
ps.fit1<-glm(COVID19~YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
               BD19+BD21+factor(multi)+factor(ses),data=s01,family="binomial")

ps.fit2<-glm(COVID19~YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
               BD19+BD21+factor(multi)+factor(ses),data=s02,family="binomial")

ps.fit3<-glm(COVID19~YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
               FULLY_PRE_COVID_VAC+BD19+BD21+factor(multi)+factor(ses),data=s03,family="binomial")

s01.ps<-predict(ps.fit1,type="response")
s02.ps<-predict(ps.fit2,type="response")
s03.ps<-predict(ps.fit3,type="response")

s01$ps=s01.ps
s02$ps=s02.ps
s03$ps=s03.ps

roc1<-roc(COVID19~ps,data=s01)
roc2<-roc(COVID19~ps,data=s02)
roc3<-roc(COVID19~ps,data=s03)

x11();plot(roc1,legacy.axes=T,cex.lab=1.4,cex.axis=1.4)
x11();plot(roc2,legacy.axes=T,cex.lab=1.4,cex.axis=1.4)
x11();plot(roc3,legacy.axes=T,cex.lab=1.4,cex.axis=1.4)

roc1
roc2
roc3
#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
#Pre-delta
den.s01.1<-data.frame(x=density(s01$ps)$x,y=density(s01$ps)$y,group="All")
den.s01.2<-data.frame(x=density(subset(s01,COVID19==1)$ps)$x,y=density(subset(s01,COVID19==1)$ps)$y,group="COVID-19")
den.s01.3<-data.frame(x=density(subset(s01,COVID19==0)$ps)$x,y=density(subset(s01,COVID19==0)$ps)$y,group="Non COVID-19")

#Delta
# den.s02.1<-data.frame(x=density(s02$ps)$x,y=density(s02$ps)$y,group="All")
den.s02.2<-data.frame(x=density(subset(s02,COVID19==1)$ps)$x,y=density(subset(s02,COVID19==1)$ps)$y,group="COVID-19")
den.s02.3<-data.frame(x=density(subset(s02,COVID19==0)$ps)$x,y=density(subset(s02,COVID19==0)$ps)$y,group="Non COVID-19")

#Omicron
# den.s03.1<-data.frame(x=density(s03$ps)$x,y=density(s03$ps)$y,group="All")
den.s03.2<-data.frame(x=density(subset(s03,COVID19==1)$ps)$x,y=density(subset(s03,COVID19==1)$ps)$y,group="COVID-19")
den.s03.3<-data.frame(x=density(subset(s03,COVID19==0)$ps)$x,y=density(subset(s03,COVID19==0)$ps)$y,group="Non COVID-19")

den.s01<-rbind(den.s01.2,den.s01.3)
den.s02<-rbind(den.s02.2,den.s02.3)
den.s03<-rbind(den.s03.2,den.s03.3)

den.s01$group=factor(den.s01$group,levels=c("COVID-19","Non COVID-19"))
den.s02$group=factor(den.s02$group,levels=c("COVID-19","Non COVID-19"))
den.s03$group=factor(den.s03$group,levels=c("COVID-19","Non COVID-19"))

x11();ggplot(den.s01,aes(x,y,col=group))+theme_bw(base_size=20)+geom_line(lwd=2)+
  scale_y_continuous(breaks=NULL)+
  # scale_x_continuous(limit=c(-0.1,1.1),breaks = seq(c(0.1:1),by=0.1))+
  scale_color_manual(values=c("COVID-19"="#D55E00",
                              "Non COVID-19"="#0072B2"))+
  labs(x="Propensity score",y="Distribution")


x11();ggplot(den.s02,aes(x,y,col=group))+theme_bw(base_size=20)+geom_line(lwd=2)+
  scale_y_continuous(breaks=NULL)+
  # scale_x_continuous(limit=c(-0.1,1.1),breaks = seq(c(0.1:1),by=0.1))+
  scale_color_manual(values=c("COVID-19"="#D55E00",
                              "Non COVID-19"="#0072B2"))+
  labs(x="Propensity score",y="Distribution")


x11();ggplot(den.s03,aes(x,y,col=group))+theme_bw(base_size=20)+geom_line(lwd=2)+
  scale_y_continuous(breaks=NULL)+
  # scale_x_continuous(limit=c(-0.1,1.1),breaks = seq(c(0.1:1),by=0.1))+
  scale_color_manual(values=c("COVID-19"="#D55E00",
                              "Non COVID-19"="#0072B2"))+
  labs(x="Propensity score",y="Distribution")

write.csv(den.s01,file="den.s01.csv",row.names=F,na="")
write.csv(den.s02,file="den.s02.csv",row.names=F,na="")
write.csv(den.s03,file="den.s03.csv",row.names=F,na="")

den.s01$dataset="s01"
den.s02$dataset="s02"
den.s03$dataset="s03"

dd<-rbind(den.s01,den.s02,den.s03)
write.csv(dd,file="dd.csv",row.names=F,na="")

#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
#Weighting 미리 만들고, 나중에 골라 쓰기, 나중에 매칭하기
#IPTW, for ATE
s01$iptw=NA;s02$iptw=NA;s03$iptw=NA

#1/PS ofr treated (COVID-19) & 1/(1-PS) for untreated (Non COVID-19)
s01$iptw=with(s01,ifelse(COVID19==1,1/ps,1/(1-ps)))
s02$iptw=with(s02,ifelse(COVID19==1,1/ps,1/(1-ps)))
s03$iptw=with(s03,ifelse(COVID19==1,1/ps,1/(1-ps)))

#SMRW, for ATT
s01$smrw=NA;s02$smrw=NA;s03$smrw=NA

s01$smrw=with(s01,ifelse(COVID19==1,1,ps/(1-ps)))
s02$smrw=with(s02,ifelse(COVID19==1,1,ps/(1-ps)))
s03$smrw=with(s03,ifelse(COVID19==1,1,ps/(1-ps)))

#Numerator weight for stablization
s01$prob=NA;s02$prob=NA;s03$prob=NA
s01$prob=with(s01,ifelse(COVID19==1,sum(COVID19==1)/length(COVID19),sum(COVID19==0)/length(COVID19)))
s02$prob=with(s02,ifelse(COVID19==1,sum(COVID19==1)/length(COVID19),sum(COVID19==0)/length(COVID19)))
s03$prob=with(s03,ifelse(COVID19==1,sum(COVID19==1)/length(COVID19),sum(COVID19==0)/length(COVID19)))

#IPTW stablized with marginal probabilities of assigned treatment
s01$iptwstab=with(s01,iptw*prob)
s02$iptwstab=with(s02,iptw*prob)
s03$iptwstab=with(s03,iptw*prob)

#Matching weights (이건 안쓸듯)
s01$matchWeightNumerator<-with(s01,pmin(ps,1-ps))
s02$matchWeightNumerator<-with(s02,pmin(ps,1-ps))
s03$matchWeightNumerator<-with(s03,pmin(ps,1-ps))

s01$matchWeight=with(s01,matchWeightNumerator*iptw)
s02$matchWeight=with(s02,matchWeightNumerator*iptw)
s03$matchWeight=with(s03,matchWeightNumerator*iptw)

s01 %>% select(iptw,iptwstab,smrw,matchWeight) %>% summary
s02 %>% select(iptw,iptwstab,smrw,matchWeight) %>% summary
s03 %>% select(iptw,iptwstab,smrw,matchWeight) %>% summary

#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#

ps.fit1<-glm(COVID19~YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
               BD19+BD21+factor(multi)+factor(ses),data=s01,family="binomial")

ps.fit2<-glm(COVID19~YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
               FULLY_PRE_COVID_VAC+BD19+BD21+factor(multi)+factor(ses),data=s02,family="binomial")

ps.fit3<-glm(COVID19~YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
               FULLY_PRE_COVID_VAC+BD19+BD21+factor(multi)+factor(ses),data=s03,family="binomial")


#greedy matching 1:1, or 1:2
#Optimal 1:1 main으로
library(randomForest)
library(glmnet)


matchit.s01<-matchit(COVID19~YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
                       BD19+BD21+factor(multi)+factor(ses),method="nearest",estimated="ATT",
                     data=s01,ratio=1,replacement=T,caliper=0.2)

matchit.s02<-matchit(COVID19~YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
                       FULLY_PRE_COVID_VAC+BD19+BD21+factor(multi)+factor(ses),method="nearest",estimated="ATT",
                     data=s02,ratio=1,replacement=T,caliper=0.2)

matchit.s03<-matchit(COVID19~YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
                       FULLY_PRE_COVID_VAC+BD19+BD21+factor(multi)+factor(ses),method="nearest",estimated="ATT",
                     data=s03,ratio=1,replacement=T,caliper=0.2)

summary(matchit.s01)
summary(matchit.s02)
summary(matchit.s03)

saveRDS(matchit.s01,file="matchit.s01_20220726.rds")
saveRDS(matchit.s02,file="matchit.s02_20220726.rds")
saveRDS(matchit.s03,file="matchit.s03_20220726.rds")

#population density, green space, population, local tax, 병상수 

#PS macthing 후 데이터 
matched.s01<-match.data(matchit.s01) %>% arrange(subclass)
matched.s02<-match.data(matchit.s02) %>% arrange(subclass)
matched.s03<-match.data(matchit.s03) %>% arrange(subclass)

x11();plot(matchit.s01,type="jitter",interactive=F,cex.axis=1.4,cex.lab=1.5,cex.main=2,pch=1)
x11();plot(matchit.s02,type="jitter",interactive=F,cex.axis=1.4,cex.lab=1.5,cex.main=2,pch=1)
x11();plot(matchit.s03,type="jitter",interactive=F,cex.axis=1.4,cex.lab=1.5,cex.main=2,pch=1)

write.csv(matched.s01,file="matched.s01_20220726.csv",row.names=F,na="")
write.csv(matched.s02,file="matched.s02_20220726.csv",row.names=F,na="")
write.csv(matched.s03,file="matched.s03_20220726.csv",row.names=F,na="")


d1<-data.frame(x=density(subset(matched.s03,COVID19==1)$ps)$x,y=density(subset(matched.s03,COVID19==1)$ps)$y,group="COVID-19")
d2<-data.frame(x=density(subset(matched.s03,COVID19==0)$ps)$x,y=density(subset(matched.s03,COVID19==0)$ps)$y,group="Non COVID-19")

dd<-rbind(d1,d2)
x11();ggplot(dd,aes(x,y,col=group))+theme_bw(base_size=20)+geom_line(lwd=2)+
  scale_y_continuous(breaks=NULL)+
  # scale_x_continuous(limit=c(-0.1,1.1),breaks = seq(c(0.1:1),by=0.1))+
  scale_color_manual(values=c("COVID-19"="#D55E00",
                              "Non COVID-19"="#0072B2"))+
  labs(x="Propensity score",y="Distribution")+facet_wrap(~group)

#Afrer PS matching
aps1<-as.data.frame(summary(matchit.s01)$sum.matched)
aps2<-as.data.frame(summary(matchit.s02)$sum.matched)
aps3<-as.data.frame(summary(matchit.s03)$sum.matched)

aps1$group=1
aps2$group=2
aps3$group=3

aps<-rbind(aps1,aps2,aps3)
write.csv(aps,file="aps.csv")
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
fit1<-glm(OUT11~COVID19+YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
            BD19+BD21+factor(multi)+factor(ses),data=matched.s01,family="binomial")

fit2<-glm(OUT11~COVID19+YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
            FULLY_PRE_COVID_VAC+BD19+BD21+factor(multi)+factor(ses),data=matched.s02,family="binomial")

fit3<-glm(OUT11~COVID19+YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
            BD19+BD21+factor(multi)+factor(ses),data=matched.s03,family="binomial")

fit3.0<-glm(Out17~COVID19+YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
              BD19+BD21+factor(multi)+factor(ses),data=subset(matched.s03,FULLY_PRE_COVID_VAC==0),family="binomial")
fit3.1<-glm(Out17~COVID19+YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
              BD19+BD21+factor(multi)+factor(ses),data=subset(matched.s03,FULLY_PRE_COVID_VAC==1),family="binomial")

summary(fit3.0)$coeff[2,]
summary(fit3.1)$coeff[2,]

summary(fit3)
res<-as.data.frame(rbind(summary(fit1)$coeff[2,],
                         summary(fit2)$coeff[2,],
                         summary(fit3)$coeff[2,]))

res$period=c("Pre delta","Delta","Omicron")
res$OR    =exp(res$Estimate)
res$lci   =exp(res$Estimate-1.96*res$`Std. Error`)
res$uci   =exp(res$Estimate+1.96*res$`Std. Error`)

res<-res %>% dplyr:: select(period,Estimate:`Pr(>|z|)`,OR:uci)
write.csv(res,file="res.csv",row.names=F,na="")
#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
#iptw 
fit1<-glm(Out17~COVID19+YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
            BD19+BD21+factor(multi)+factor(ses),weights=iptw,data=s01,family="binomial")

fit2<-glm(Out17~COVID19+YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
            FULLY_PRE_COVID_VAC+BD19+BD21+factor(multi)+factor(ses),weights=iptw,data=s02,family="binomial")

fit3<-glm(Out17~COVID19+YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
            FULLY_PRE_COVID_VAC+BD19+BD21+factor(multi)+factor(ses),weights=iptw,data=s03,family="binomial")

res<-as.data.frame(rbind(summary(fit1)$coeff[2,],
                         summary(fit2)$coeff[2,],
                         summary(fit3)$coeff[2,]))

res$period=c("Pre delta","Delta","Omicron")
res$OR    =exp(res$Estimate)
res$lci   =exp(res$Estimate-1.96*res$`Std. Error`)
res$uci   =exp(res$Estimate+1.96*res$`Std. Error`)

res<-res %>% dplyr:: select(period,Estimate:`Pr(>|z|)`,OR:uci)
write.csv(res,file="res.csv",row.names=F,na="")

#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
#각 질환별 COVID19 감염에 따른 빈도 
s01<-subset(dat2,COVID_PERIOD=="Pre delta")
s02<-subset(dat2,COVID_PERIOD=="Delta")
s03<-subset(dat2,COVID_PERIOD=="Omicron")

s01<-s01 %>% select(OUT1:OUT16,Out17,OUT20,OUT21,OUT22:OUT31,CHILD_ID:multi,PARITY:parity_g)
s02<-s02 %>% select(OUT1:OUT16,Out17,OUT20,OUT21,OUT22:OUT31,CHILD_ID:multi,PARITY:parity_g)
s03<-s03 %>% select(OUT1:OUT16,Out17,OUT20,OUT21,OUT22:OUT31,CHILD_ID:multi,PARITY:parity_g)

YEND_STD_AGE+factor(CHD_SEX)+parity_g+FOREIGNER_YN+
  FULLY_PRE_COVID_VAC+BD19+BD21+factor(multi)+factor(ses)


aggregate(YEND_STD_AGE~COVID19,data=s03,mean)
aggregate(YEND_STD_AGE~COVID19,data=s03,sd)


d1<-as.data.frame(rbind(with(s01,addmargins(table(parity_g,COVID19))),
                        with(s01,addmargins(table(CHD_SEX,COVID19))),
                        with(s01,addmargins(table(FOREIGNER_YN,COVID19))),
                        with(s01,addmargins(table(FULLY_PRE_COVID_VAC,COVID19))),
                        with(s01,addmargins(table(BD19,COVID19))),
                        with(s01,addmargins(table(BD21,COVID19))),
                        with(s01,addmargins(table(multi,COVID19))),
                        with(s01,addmargins(table(ses,COVID19)))))

d2<-as.data.frame(rbind(with(s02,addmargins(table(parity_g,COVID19))),
                        with(s02,addmargins(table(CHD_SEX,COVID19))),
                        with(s02,addmargins(table(FOREIGNER_YN,COVID19))),
                        with(s02,addmargins(table(FULLY_PRE_COVID_VAC,COVID19))),
                        with(s02,addmargins(table(BD19,COVID19))),
                        with(s02,addmargins(table(BD21,COVID19))),
                        with(s02,addmargins(table(multi,COVID19))),
                        with(s02,addmargins(table(ses,COVID19)))))

d3<-as.data.frame(rbind(with(s03,addmargins(table(parity_g,COVID19))),
                        with(s03,addmargins(table(CHD_SEX,COVID19))),
                        with(s03,addmargins(table(FOREIGNER_YN,COVID19))),
                        with(s03,addmargins(table(FULLY_PRE_COVID_VAC,COVID19))),
                        with(s03,addmargins(table(BD19,COVID19))),
                        with(s03,addmargins(table(BD21,COVID19))),
                        with(s03,addmargins(table(multi,COVID19))),
                        with(s03,addmargins(table(ses,COVID19)))))

write.csv(d1,file="d1.csv")
write.csv(d2,file="d2.csv")
write.csv(d3,file="d3.csv")
