#TOT, AGE0, AGE13, TOT_M, TOT_F
# s1.w4.w<-subset(s1.w4,month %in% c(4:9)); s1.w4.c<-subset(s1.w4,month %in% c(1:3,10:12));
# s2.w4.w<-subset(s2.w4,month %in% c(4:9)); s2.w4.c<-subset(s2.w4,month %in% c(1:3,10:12));
# s3.w4.w<-subset(s3.w4,month %in% c(4:9)); s3.w4.c<-subset(s3.w4,month %in% c(1:3,10:12));
# s4.w4.w<-subset(s4.w4,month %in% c(4:9)); s4.w4.c<-subset(s4.w4,month %in% c(1:3,10:12));
# s5.w4.w<-subset(s5.w4,month %in% c(4:9)); s5.w4.c<-subset(s5.w4,month %in% c(1:3,10:12));
# s6.w4.w<-subset(s6.w4,month %in% c(4:9)); s6.w4.c<-subset(s6.w4,month %in% c(1:3,10:12));
# s7.w4.w<-subset(s7.w4,month %in% c(4:9)); s7.w4.c<-subset(s7.w4,month %in% c(1:3,10:12));

setwd("C:\\Users\\Jongmenn\\Desktop\\논문\\미세먼지_AOM\\analysis\\data")
uri<-read.csv("AOM_URI_COUNT_191210.CSV")

s1.u<-subset(uri,SIDO==11);s2.u<-subset(uri,SIDO==26)
s3.u<-subset(uri,SIDO==27);s4.u<-subset(uri,SIDO==28)
s5.u<-subset(uri,SIDO==29);s6.u<-subset(uri,SIDO==30);s7.u<-subset(uri,SIDO==31)

s1.u$key=paste0(s1.u$SIDO,"-",s1.u$DATE);s1.u<-s1.u %>% select(key,TOT4_Y,TOT4_N)
s2.u$key=paste0(s2.u$SIDO,"-",s2.u$DATE);s2.u<-s2.u %>% select(key,TOT4_Y,TOT4_N)
s3.u$key=paste0(s3.u$SIDO,"-",s3.u$DATE);s3.u<-s3.u %>% select(key,TOT4_Y,TOT4_N)
s4.u$key=paste0(s4.u$SIDO,"-",s4.u$DATE);s4.u<-s4.u %>% select(key,TOT4_Y,TOT4_N)
s5.u$key=paste0(s5.u$SIDO,"-",s5.u$DATE);s5.u<-s5.u %>% select(key,TOT4_Y,TOT4_N)
s6.u$key=paste0(s6.u$SIDO,"-",s6.u$DATE);s6.u<-s6.u %>% select(key,TOT4_Y,TOT4_N)
s7.u$key=paste0(s7.u$SIDO,"-",s7.u$DATE);s7.u<-s7.u %>% select(key,TOT4_Y,TOT4_N)

# s1.w4<-merge(s1.w4,s1.u,by="key",all.x=T)
# s2.w4<-merge(s2.w4,s2.u,by="key",all.x=T)
# s3.w4<-merge(s3.w4,s3.u,by="key",all.x=T)
# s4.w4<-merge(s4.w4,s4.u,by="key",all.x=T)
# s5.w4<-merge(s5.w4,s5.u,by="key",all.x=T)
# s6.w4<-merge(s6.w4,s6.u,by="key",all.x=T)
# s7.w4<-merge(s7.w4,s7.u,by="key",all.x=T)

z1<-summary(gam(TOT4_N~lag03+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+as.factor(month),family="quasipoisson",data=s1.w4))$'p.table'[2,]
z2<-summary(gam(TOT4_N~lag03+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+as.factor(month),family="quasipoisson",data=s2.w4))$'p.table'[2,]
z3<-summary(gam(TOT4_N~lag03+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+as.factor(month),family="quasipoisson",data=s3.w4))$'p.table'[2,]
z4<-summary(gam(TOT4_N~lag03+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+as.factor(month),family="quasipoisson",data=s4.w4))$'p.table'[2,]
z5<-summary(gam(TOT4_N~lag03+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+as.factor(month),family="quasipoisson",data=s5.w4))$'p.table'[2,]
z6<-summary(gam(TOT4_N~lag03+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+as.factor(month),family="quasipoisson",data=s6.w4))$'p.table'[2,]
z7<-summary(gam(TOT4_N~lag03+s(as.numeric(DATE),k=6*9,fx=T)+s(AT,k=6,fx=T)+dow+as.factor(month),family="quasipoisson",data=s7.w4))$'p.table'[2,]

zz<-as.data.frame(rbind(z1,z2,z3,z4,z5,z6,z7))
zz$RR    =round(exp(zz$Estimate),3)
zz$RR_lci=round(exp(zz$Estimate-1.96*zz$`Std. Error`),3)
zz$RR_uci=round(exp(zz$Estimate+1.96*zz$`Std. Error`),3)
zz$RR_CI =with(zz,paste0("(",ifelse(RR_lci==1,"1.000",RR_lci),", ",ifelse(RR_uci==1,"1.000",RR_uci),")"))
zz

zz$SIDO=factor(c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"),levels=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan"))
uni<-with(zz,rma(yi=Estimate, sei=`Std. Error`, slab=SIDO, measure="RR",digits=5,method="ML"))

uni.d<-with(uni,as.data.frame(cbind(meta_e=b,meta_LCI=ci.lb,meta_UCI=ci.ub,meta_se=se,pval=ifelse(round(pval,3)<=0.001,"<0.001",round(pval,3)))))

uni.d$RR    =round(exp(as.numeric(as.character(uni.d$V1))),3)
uni.d$RR_lci=round(exp(as.numeric(as.character(uni.d$meta_LCI))),3)
uni.d$RR_uci=round(exp(as.numeric(as.character(uni.d$meta_UCI))),3)

uni.d$RR_CI =with(uni.d,paste0("(",ifelse(RR_lci==1,"1.000",RR_lci),", ",ifelse(RR_uci==1,"1.000",RR_uci),")"))
uni.d

