#폭염 정의를 percentile로 조정한 것으로 평가
#ALL
tt$outcome=tt$heatrelated_CHILD_TOT
s01<-subset(tt,SIDO_KN=="서울");s02<-subset(tt,SIDO_KN=="부산")
s03<-subset(tt,SIDO_KN=="대구");s04<-subset(tt,SIDO_KN=="인천")
s05<-subset(tt,SIDO_KN=="광주");s06<-subset(tt,SIDO_KN=="대전")
s07<-subset(tt,SIDO_KN=="울산");s08<-subset(tt,SIDO_KN=="경기")
s09<-subset(tt,SIDO_KN=="강원");s10<-subset(tt,SIDO_KN=="충북")
s11<-subset(tt,SIDO_KN=="충남");s12<-subset(tt,SIDO_KN=="전북")
s13<-subset(tt,SIDO_KN=="전남");s14<-subset(tt,SIDO_KN=="경북")
s15<-subset(tt,SIDO_KN=="경남")

s01$exp=round(s01$maxtemp_lag2)
s02$exp=round(s02$maxtemp_lag2)
s03$exp=round(s03$maxtemp_lag2)
s04$exp=round(s04$maxtemp_lag2)
s05$exp=round(s05$maxtemp_lag2)
s06$exp=round(s06$maxtemp_lag2)
s07$exp=round(s07$maxtemp_lag2)
s08$exp=round(s08$maxtemp_lag2)
s09$exp=round(s09$maxtemp_lag2)
s10$exp=round(s10$maxtemp_lag2)
s11$exp=round(s11$maxtemp_lag2)
s12$exp=round(s12$maxtemp_lag2)
s13$exp=round(s13$maxtemp_lag2)
s14$exp=round(s14$maxtemp_lag2)
s15$exp=round(s15$maxtemp_lag2)

er1 <-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s01,family="quasipoisson")
er2 <-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s02,family="quasipoisson")
er3 <-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s03,family="quasipoisson")
er4 <-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s04,family="quasipoisson")
er5 <-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s05,family="quasipoisson")
er6 <-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s06,family="quasipoisson")
er7 <-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s07,family="quasipoisson")
er8 <-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s08,family="quasipoisson")
er9 <-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s09,family="quasipoisson")
er10<-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s10,family="quasipoisson")
er11<-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s11,family="quasipoisson")
er12<-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s12,family="quasipoisson")
er13<-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s13,family="quasipoisson")
er14<-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s14,family="quasipoisson")
er15<-gam(outcome~s(maxtemp_lag2)+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=s15,family="quasipoisson")

p01<-plot(er1,select=1) ;p02<-plot(er2,select=1) ;p03<-plot(er3,select=1);
p04<-plot(er4,select=1) ;p05<-plot(er5,select=1) ;p06<-plot(er6,select=1)
p07<-plot(er7,select=1) ;p08<-plot(er8,select=1) ;p09<-plot(er9,select=1);
p10<-plot(er10,select=1);p11<-plot(er11,select=1);p12<-plot(er12,select=1)
p13<-plot(er13,select=1);p14<-plot(er14,select=1);p15<-plot(er15,select=1)


g01<-data.frame(RR=exp(p01[[1]]$fit),se=p01[[1]]$se,exp=p01[[1]]$x,lci=exp(p01[[1]]$fit-1.96*p01[[1]]$se),uci=exp(p01[[1]]$fit+1.96*p01[[1]]$se))
g02<-data.frame(RR=exp(p02[[1]]$fit),se=p02[[1]]$se,exp=p02[[1]]$x,lci=exp(p02[[1]]$fit-1.96*p02[[1]]$se),uci=exp(p02[[1]]$fit+1.96*p02[[1]]$se))
g03<-data.frame(RR=exp(p03[[1]]$fit),se=p03[[1]]$se,exp=p03[[1]]$x,lci=exp(p03[[1]]$fit-1.96*p03[[1]]$se),uci=exp(p03[[1]]$fit+1.96*p03[[1]]$se))
g04<-data.frame(RR=exp(p04[[1]]$fit),se=p04[[1]]$se,exp=p04[[1]]$x,lci=exp(p04[[1]]$fit-1.96*p04[[1]]$se),uci=exp(p04[[1]]$fit+1.96*p04[[1]]$se))
g05<-data.frame(RR=exp(p05[[1]]$fit),se=p05[[1]]$se,exp=p05[[1]]$x,lci=exp(p05[[1]]$fit-1.96*p05[[1]]$se),uci=exp(p05[[1]]$fit+1.96*p05[[1]]$se))
g06<-data.frame(RR=exp(p06[[1]]$fit),se=p06[[1]]$se,exp=p06[[1]]$x,lci=exp(p06[[1]]$fit-1.96*p06[[1]]$se),uci=exp(p06[[1]]$fit+1.96*p06[[1]]$se))
g07<-data.frame(RR=exp(p07[[1]]$fit),se=p07[[1]]$se,exp=p07[[1]]$x,lci=exp(p07[[1]]$fit-1.96*p07[[1]]$se),uci=exp(p07[[1]]$fit+1.96*p07[[1]]$se))
g08<-data.frame(RR=exp(p08[[1]]$fit),se=p08[[1]]$se,exp=p08[[1]]$x,lci=exp(p08[[1]]$fit-1.96*p08[[1]]$se),uci=exp(p08[[1]]$fit+1.96*p08[[1]]$se))
g09<-data.frame(RR=exp(p09[[1]]$fit),se=p09[[1]]$se,exp=p09[[1]]$x,lci=exp(p09[[1]]$fit-1.96*p09[[1]]$se),uci=exp(p09[[1]]$fit+1.96*p09[[1]]$se))
g10<-data.frame(RR=exp(p10[[1]]$fit),se=p10[[1]]$se,exp=p10[[1]]$x,lci=exp(p10[[1]]$fit-1.96*p10[[1]]$se),uci=exp(p10[[1]]$fit+1.96*p10[[1]]$se))
g11<-data.frame(RR=exp(p11[[1]]$fit),se=p11[[1]]$se,exp=p11[[1]]$x,lci=exp(p11[[1]]$fit-1.96*p11[[1]]$se),uci=exp(p11[[1]]$fit+1.96*p11[[1]]$se))
g12<-data.frame(RR=exp(p12[[1]]$fit),se=p12[[1]]$se,exp=p12[[1]]$x,lci=exp(p12[[1]]$fit-1.96*p12[[1]]$se),uci=exp(p12[[1]]$fit+1.96*p12[[1]]$se))
g13<-data.frame(RR=exp(p13[[1]]$fit),se=p13[[1]]$se,exp=p13[[1]]$x,lci=exp(p13[[1]]$fit-1.96*p13[[1]]$se),uci=exp(p13[[1]]$fit+1.96*p13[[1]]$se))
g14<-data.frame(RR=exp(p14[[1]]$fit),se=p14[[1]]$se,exp=p14[[1]]$x,lci=exp(p14[[1]]$fit-1.96*p14[[1]]$se),uci=exp(p14[[1]]$fit+1.96*p14[[1]]$se))
g15<-data.frame(RR=exp(p15[[1]]$fit),se=p15[[1]]$se,exp=p15[[1]]$x,lci=exp(p15[[1]]$fit-1.96*p15[[1]]$se),uci=exp(p15[[1]]$fit+1.96*p15[[1]]$se))

g01$outcome=1;g02$outcome=1;g03$outcome=1
g04$outcome=1;g05$outcome=1;g06$outcome=1
g07$outcome=1;g08$outcome=1;g09$outcome=1
g10$outcome=1;g11$outcome=1;g12$outcome=1
g13$outcome=1;g14$outcome=1;g15$outcome=1

x11();ggplot(s01,aes(exp,outcome))+geom_histogram(aes(y=..density..),fill="orange",binwidth = 1)+
  geom_hline(yintercept = 1/5,col="red",linetype=2)+
  geom_vline(xintercept = 33,linetype=2)+
  scale_x_continuous(breaks=19:40)+
  coord_cartesian(ylim=c(0,0.6))+
  labs(x="Daily maximum temperature (℃)",y="Density",title="Seoul")+
  geom_line(data=g01,aes(exp,RR/5),size=1)+
  geom_ribbon(data=g01,aes(ymin=g01$lci/5,ymax=g01$uci/5),alpha=0.2)+
  scale_y_continuous(sec.axis=sec_axis(~.*5,name="Relative risk (95% Confidence interavls)"))+
  theme_gray(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  

x11();ggplot(s02,aes(exp,outcome))+geom_histogram(aes(y=..density..),fill="orange",binwidth = 1)+
  geom_hline(yintercept = 1/5,col="red",linetype=2)+
  geom_vline(xintercept = 33,linetype=2)+
  scale_x_continuous(breaks=19:40)+
  coord_cartesian(ylim=c(0,0.6))+
  labs(x="Daily maximum temperature (℃)",y="Density",title="Busan")+
  geom_line(data=g02,aes(exp,RR/5),size=1)+
  geom_ribbon(data=g02,aes(ymin=g02$lci/5,ymax=g02$uci/5),alpha=0.2)+
  scale_y_continuous(sec.axis=sec_axis(~.*5,name="Relative risk (95% Confidence interavls)"))+
  theme_gray(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

x11();ggplot(s02,aes(exp,outcome))+geom_histogram(aes(y=..density..),fill="orange",binwidth = 1)+
  geom_hline(yintercept = 1/5,col="red",linetype=2)+
  geom_vline(xintercept = 33,linetype=2)+
  scale_x_continuous(breaks=19:40)+
  coord_cartesian(ylim=c(0,0.6))+
  labs(x="Daily maximum temperature (℃)",y="Density",title="Busan")+
  geom_line(data=g02,aes(exp,RR/5),size=1)+
  geom_ribbon(data=g02,aes(ymin=g02$lci/5,ymax=g02$uci/5),alpha=0.2)+
  scale_y_continuous(sec.axis=sec_axis(~.*5,name="Relative risk (95% Confidence interavls)"))+
  theme_gray(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


rbind(table(s01$p70D2_lag0),table(s02$p70D2_lag0),table(s03$p70D2_lag0),
      table(s04$p70D2_lag0),table(s05$p70D2_lag0),table(s06$p70D2_lag0),
      table(s07$p70D2_lag0),table(s08$p70D2_lag0),table(s09$p70D2_lag0),
      table(s10$p70D2_lag0),table(s11$p70D2_lag0),table(s12$p70D2_lag0),
      table(s13$p70D2_lag0),table(s14$p70D2_lag0),table(s15$p70D2_lag0))

rbind(table(s01$p70D3_lag0),table(s02$p70D3_lag0),table(s03$p70D3_lag0),
      table(s04$p70D3_lag0),table(s05$p70D3_lag0),table(s06$p70D3_lag0),
      table(s07$p70D3_lag0),table(s08$p70D3_lag0),table(s09$p70D3_lag0),
      table(s10$p70D3_lag0),table(s11$p70D3_lag0),table(s12$p70D3_lag0),
      table(s13$p70D3_lag0),table(s14$p70D3_lag0),table(s15$p70D3_lag0))

rbind(table(s01$p75D2_lag0),table(s02$p75D2_lag0),table(s03$p75D2_lag0),
      table(s04$p75D2_lag0),table(s05$p75D2_lag0),table(s06$p75D2_lag0),
      table(s07$p75D2_lag0),table(s08$p75D2_lag0),table(s09$p75D2_lag0),
      table(s10$p75D2_lag0),table(s11$p75D2_lag0),table(s12$p75D2_lag0),
      table(s13$p75D2_lag0),table(s14$p75D2_lag0),table(s15$p75D2_lag0))

rbind(table(s01$p75D3_lag0),table(s02$p75D3_lag0),table(s03$p75D3_lag0),
      table(s04$p75D3_lag0),table(s05$p75D3_lag0),table(s06$p75D3_lag0),
      table(s07$p75D3_lag0),table(s08$p75D3_lag0),table(s09$p75D3_lag0),
      table(s10$p75D3_lag0),table(s11$p75D3_lag0),table(s12$p75D3_lag0),
      table(s13$p75D3_lag0),table(s14$p75D3_lag0),table(s15$p75D3_lag0))


rbind(table(s01$p80D2_lag0),table(s02$p80D2_lag0),table(s03$p80D2_lag0),
      table(s04$p80D2_lag0),table(s05$p80D2_lag0),table(s06$p80D2_lag0),
      table(s07$p80D2_lag0),table(s08$p80D2_lag0),table(s09$p80D2_lag0),
      table(s10$p80D2_lag0),table(s11$p80D2_lag0),table(s12$p80D2_lag0),
      table(s13$p80D2_lag0),table(s14$p80D2_lag0),table(s15$p80D2_lag0))

rbind(table(s01$p80D3_lag0),table(s02$p80D3_lag0),table(s03$p80D3_lag0),
      table(s04$p80D3_lag0),table(s05$p80D3_lag0),table(s06$p80D3_lag0),
      table(s07$p80D3_lag0),table(s08$p80D3_lag0),table(s09$p80D3_lag0),
      table(s10$p80D3_lag0),table(s11$p80D3_lag0),table(s12$p80D3_lag0),
      table(s13$p80D3_lag0),table(s14$p80D3_lag0),table(s15$p80D3_lag0))

rbind(table(s01$p85D2_lag0),table(s02$p85D2_lag0),table(s03$p85D2_lag0),
      table(s04$p85D2_lag0),table(s05$p85D2_lag0),table(s06$p85D2_lag0),
      table(s07$p85D2_lag0),table(s08$p85D2_lag0),table(s09$p85D2_lag0),
      table(s10$p85D2_lag0),table(s11$p85D2_lag0),table(s12$p85D2_lag0),
      table(s13$p85D2_lag0),table(s14$p85D2_lag0),table(s15$p85D2_lag0))

rbind(table(s01$p85D3_lag0),table(s02$p85D3_lag0),table(s03$p85D3_lag0),
      table(s04$p85D3_lag0),table(s05$p85D3_lag0),table(s06$p85D3_lag0),
      table(s07$p85D3_lag0),table(s08$p85D3_lag0),table(s09$p85D3_lag0),
      table(s10$p85D3_lag0),table(s11$p85D3_lag0),table(s12$p85D3_lag0),
      table(s13$p85D3_lag0),table(s14$p85D3_lag0),table(s15$p85D3_lag0))

rbind(table(s01$p90D2_lag0),table(s02$p90D2_lag0),table(s03$p90D2_lag0),
      table(s04$p90D2_lag0),table(s05$p90D2_lag0),table(s06$p90D2_lag0),
      table(s07$p90D2_lag0),table(s08$p90D2_lag0),table(s09$p90D2_lag0),
      table(s10$p90D2_lag0),table(s11$p90D2_lag0),table(s12$p90D2_lag0),
      table(s13$p90D2_lag0),table(s14$p90D2_lag0),table(s15$p90D2_lag0))

rbind(table(s01$p90D3_lag0),table(s02$p90D3_lag0),table(s03$p90D3_lag0),
      table(s04$p90D3_lag0),table(s05$p90D3_lag0),table(s06$p90D3_lag0),
      table(s07$p90D3_lag0),table(s08$p90D3_lag0),table(s09$p90D3_lag0),
      table(s10$p90D3_lag0),table(s11$p90D3_lag0),table(s12$p90D3_lag0),
      table(s13$p90D3_lag0),table(s14$p90D3_lag0),table(s15$p90D3_lag0))

rbind(table(s01$heat33D2_lag0),table(s02$heat33D2_lag0),table(s03$heat33D2_lag0),
      table(s04$heat33D2_lag0),table(s05$heat33D2_lag0),table(s06$heat33D2_lag0),
      table(s07$heat33D2_lag0),table(s08$heat33D2_lag0),table(s09$heat33D2_lag0),
      table(s10$heat33D2_lag0),table(s11$heat33D2_lag0),table(s12$heat33D2_lag0),
      table(s13$heat33D2_lag0),table(s14$heat33D2_lag0),table(s15$heat33D2_lag0))

rbind(table(s01$heat33D3_lag0),table(s02$heat33D3_lag0),table(s03$heat33D3_lag0),
      table(s04$heat33D3_lag0),table(s05$heat33D3_lag0),table(s06$heat33D3_lag0),
      table(s07$heat33D3_lag0),table(s08$heat33D3_lag0),table(s09$heat33D3_lag0),
      table(s10$heat33D3_lag0),table(s11$heat33D3_lag0),table(s12$heat33D3_lag0),
      table(s13$heat33D3_lag0),table(s14$heat33D3_lag0),table(s15$heat33D3_lag0))


sido.gam<-function(dat){
  
  h26_lag0<-gam(outcome~heat26D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  h26_lag1<-gam(outcome~heat26D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  h26_lag2<-gam(outcome~heat26D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  h26_lag3<-gam(outcome~heat26D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  h26_lag4<-gam(outcome~heat26D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  h27_lag0<-gam(outcome~heat27D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  h27_lag1<-gam(outcome~heat27D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  h27_lag2<-gam(outcome~heat27D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  h27_lag3<-gam(outcome~heat27D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  h27_lag4<-gam(outcome~heat27D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  h28_lag0<-gam(outcome~heat28D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  h28_lag1<-gam(outcome~heat28D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  h28_lag2<-gam(outcome~heat28D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  h28_lag3<-gam(outcome~heat28D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  h28_lag4<-gam(outcome~heat28D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  h29_lag0<-gam(outcome~heat29D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  h29_lag1<-gam(outcome~heat29D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  h29_lag2<-gam(outcome~heat29D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  h29_lag3<-gam(outcome~heat29D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  h29_lag4<-gam(outcome~heat29D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  h30_lag0<-gam(outcome~heat30D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  h30_lag1<-gam(outcome~heat30D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  h30_lag2<-gam(outcome~heat30D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  h30_lag3<-gam(outcome~heat30D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  h30_lag4<-gam(outcome~heat30D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  h31_lag0<-gam(outcome~heat31D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  h31_lag1<-gam(outcome~heat31D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  h31_lag2<-gam(outcome~heat31D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  h31_lag3<-gam(outcome~heat31D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  h31_lag4<-gam(outcome~heat31D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  h32_lag0<-gam(outcome~heat32D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  h32_lag1<-gam(outcome~heat32D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  h32_lag2<-gam(outcome~heat32D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  h32_lag3<-gam(outcome~heat32D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  h32_lag4<-gam(outcome~heat32D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  h33_lag0<-gam(outcome~heat33D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  h33_lag1<-gam(outcome~heat33D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  h33_lag2<-gam(outcome~heat33D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  h33_lag3<-gam(outcome~heat33D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  h33_lag4<-gam(outcome~heat33D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  #60th percenitle
  p60_lag0<-gam(outcome~p60D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  p60_lag1<-gam(outcome~p60D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  p60_lag2<-gam(outcome~p60D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  p60_lag3<-gam(outcome~p60D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  p60_lag4<-gam(outcome~p60D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  #65th percenitle
  p65_lag0<-gam(outcome~p65D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  p65_lag1<-gam(outcome~p65D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  p65_lag2<-gam(outcome~p65D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  p65_lag3<-gam(outcome~p65D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  p65_lag4<-gam(outcome~p65D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  #70th percenitle
  p70_lag0<-gam(outcome~p70D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  p70_lag1<-gam(outcome~p70D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  p70_lag2<-gam(outcome~p70D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  p70_lag3<-gam(outcome~p70D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  p70_lag4<-gam(outcome~p70D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  #75th percenitle
  p75_lag0<-gam(outcome~p75D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  p75_lag1<-gam(outcome~p75D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  p75_lag2<-gam(outcome~p75D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  p75_lag3<-gam(outcome~p75D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  p75_lag4<-gam(outcome~p75D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  #80th percenitle
  p80_lag0<-gam(outcome~p80D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  p80_lag1<-gam(outcome~p80D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  p80_lag2<-gam(outcome~p80D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  p80_lag3<-gam(outcome~p80D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  p80_lag4<-gam(outcome~p80D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  #85th percenitle
  p85_lag0<-gam(outcome~p85D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  p85_lag1<-gam(outcome~p85D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  p85_lag2<-gam(outcome~p85D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  p85_lag3<-gam(outcome~p85D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  p85_lag4<-gam(outcome~p85D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  #90th percenitle
  p90_lag0<-gam(outcome~p90D3_lag0+pm25_lag0+s(time,k=4*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dat,family="quasipoisson")
  p90_lag1<-gam(outcome~p90D3_lag1+pm25_lag1+s(time,k=4*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dat,family="quasipoisson")
  p90_lag2<-gam(outcome~p90D3_lag2+pm25_lag2+s(time,k=4*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dat,family="quasipoisson")
  p90_lag3<-gam(outcome~p90D3_lag3+pm25_lag3+s(time,k=4*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dat,family="quasipoisson")
  p90_lag4<-gam(outcome~p90D3_lag4+pm25_lag4+s(time,k=4*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dat,family="quasipoisson")
  
  #result table
  h26.tb0<-as.data.frame(cbind(summary(h26_lag0)$p.table[2:3,],gcv=h26_lag0$gcv.ubre,deviance=h26_lag0$deviance,r_sq=summary(h26_lag0)$r.sq))
  h26.tb1<-as.data.frame(cbind(summary(h26_lag1)$p.table[2:3,],gcv=h26_lag1$gcv.ubre,deviance=h26_lag1$deviance,r_sq=summary(h26_lag1)$r.sq))
  h26.tb2<-as.data.frame(cbind(summary(h26_lag2)$p.table[2:3,],gcv=h26_lag2$gcv.ubre,deviance=h26_lag2$deviance,r_sq=summary(h26_lag2)$r.sq))
  h26.tb3<-as.data.frame(cbind(summary(h26_lag3)$p.table[2:3,],gcv=h26_lag3$gcv.ubre,deviance=h26_lag3$deviance,r_sq=summary(h26_lag3)$r.sq))
  h26.tb4<-as.data.frame(cbind(summary(h26_lag4)$p.table[2:3,],gcv=h26_lag4$gcv.ubre,deviance=h26_lag4$deviance,r_sq=summary(h26_lag4)$r.sq))
  
  h27.tb0<-as.data.frame(cbind(summary(h27_lag0)$p.table[2:3,],gcv=h27_lag0$gcv.ubre,deviance=h27_lag0$deviance,r_sq=summary(h27_lag0)$r.sq))
  h27.tb1<-as.data.frame(cbind(summary(h27_lag1)$p.table[2:3,],gcv=h27_lag1$gcv.ubre,deviance=h27_lag1$deviance,r_sq=summary(h27_lag1)$r.sq))
  h27.tb2<-as.data.frame(cbind(summary(h27_lag2)$p.table[2:3,],gcv=h27_lag2$gcv.ubre,deviance=h27_lag2$deviance,r_sq=summary(h27_lag2)$r.sq))
  h27.tb3<-as.data.frame(cbind(summary(h27_lag3)$p.table[2:3,],gcv=h27_lag3$gcv.ubre,deviance=h27_lag3$deviance,r_sq=summary(h27_lag3)$r.sq))
  h27.tb4<-as.data.frame(cbind(summary(h27_lag4)$p.table[2:3,],gcv=h27_lag4$gcv.ubre,deviance=h27_lag4$deviance,r_sq=summary(h27_lag4)$r.sq))
  
  h28.tb0<-as.data.frame(cbind(summary(h28_lag0)$p.table[2:3,],gcv=h28_lag0$gcv.ubre,deviance=h28_lag0$deviance,r_sq=summary(h28_lag0)$r.sq))
  h28.tb1<-as.data.frame(cbind(summary(h28_lag1)$p.table[2:3,],gcv=h28_lag1$gcv.ubre,deviance=h28_lag1$deviance,r_sq=summary(h28_lag1)$r.sq))
  h28.tb2<-as.data.frame(cbind(summary(h28_lag2)$p.table[2:3,],gcv=h28_lag2$gcv.ubre,deviance=h28_lag2$deviance,r_sq=summary(h28_lag2)$r.sq))
  h28.tb3<-as.data.frame(cbind(summary(h28_lag3)$p.table[2:3,],gcv=h28_lag3$gcv.ubre,deviance=h28_lag3$deviance,r_sq=summary(h28_lag3)$r.sq))
  h28.tb4<-as.data.frame(cbind(summary(h28_lag4)$p.table[2:3,],gcv=h28_lag4$gcv.ubre,deviance=h28_lag4$deviance,r_sq=summary(h28_lag4)$r.sq))
  
  h29.tb0<-as.data.frame(cbind(summary(h29_lag0)$p.table[2:3,],gcv=h29_lag0$gcv.ubre,deviance=h29_lag0$deviance,r_sq=summary(h29_lag0)$r.sq))
  h29.tb1<-as.data.frame(cbind(summary(h29_lag1)$p.table[2:3,],gcv=h29_lag1$gcv.ubre,deviance=h29_lag1$deviance,r_sq=summary(h29_lag1)$r.sq))
  h29.tb2<-as.data.frame(cbind(summary(h29_lag2)$p.table[2:3,],gcv=h29_lag2$gcv.ubre,deviance=h29_lag2$deviance,r_sq=summary(h29_lag2)$r.sq))
  h29.tb3<-as.data.frame(cbind(summary(h29_lag3)$p.table[2:3,],gcv=h29_lag3$gcv.ubre,deviance=h29_lag3$deviance,r_sq=summary(h29_lag3)$r.sq))
  h29.tb4<-as.data.frame(cbind(summary(h29_lag4)$p.table[2:3,],gcv=h29_lag4$gcv.ubre,deviance=h29_lag4$deviance,r_sq=summary(h29_lag4)$r.sq))
  
  h30.tb0<-as.data.frame(cbind(summary(h30_lag0)$p.table[2:3,],gcv=h30_lag0$gcv.ubre,deviance=h30_lag0$deviance,r_sq=summary(h30_lag0)$r.sq))
  h30.tb1<-as.data.frame(cbind(summary(h30_lag1)$p.table[2:3,],gcv=h30_lag1$gcv.ubre,deviance=h30_lag1$deviance,r_sq=summary(h30_lag1)$r.sq))
  h30.tb2<-as.data.frame(cbind(summary(h30_lag2)$p.table[2:3,],gcv=h30_lag2$gcv.ubre,deviance=h30_lag2$deviance,r_sq=summary(h30_lag2)$r.sq))
  h30.tb3<-as.data.frame(cbind(summary(h30_lag3)$p.table[2:3,],gcv=h30_lag3$gcv.ubre,deviance=h30_lag3$deviance,r_sq=summary(h30_lag3)$r.sq))
  h30.tb4<-as.data.frame(cbind(summary(h30_lag4)$p.table[2:3,],gcv=h30_lag4$gcv.ubre,deviance=h30_lag4$deviance,r_sq=summary(h30_lag4)$r.sq))
  
  h31.tb0<-as.data.frame(cbind(summary(h31_lag0)$p.table[2:3,],gcv=h31_lag0$gcv.ubre,deviance=h31_lag0$deviance,r_sq=summary(h31_lag0)$r.sq))
  h31.tb1<-as.data.frame(cbind(summary(h31_lag1)$p.table[2:3,],gcv=h31_lag1$gcv.ubre,deviance=h31_lag1$deviance,r_sq=summary(h31_lag1)$r.sq))
  h31.tb2<-as.data.frame(cbind(summary(h31_lag2)$p.table[2:3,],gcv=h31_lag2$gcv.ubre,deviance=h31_lag2$deviance,r_sq=summary(h31_lag2)$r.sq))
  h31.tb3<-as.data.frame(cbind(summary(h31_lag3)$p.table[2:3,],gcv=h31_lag3$gcv.ubre,deviance=h31_lag3$deviance,r_sq=summary(h31_lag3)$r.sq))
  h31.tb4<-as.data.frame(cbind(summary(h31_lag4)$p.table[2:3,],gcv=h31_lag4$gcv.ubre,deviance=h31_lag4$deviance,r_sq=summary(h31_lag4)$r.sq))
  
  h32.tb0<-as.data.frame(cbind(summary(h32_lag0)$p.table[2:3,],gcv=h32_lag0$gcv.ubre,deviance=h32_lag0$deviance,r_sq=summary(h32_lag0)$r.sq))
  h32.tb1<-as.data.frame(cbind(summary(h32_lag1)$p.table[2:3,],gcv=h32_lag1$gcv.ubre,deviance=h32_lag1$deviance,r_sq=summary(h32_lag1)$r.sq))
  h32.tb2<-as.data.frame(cbind(summary(h32_lag2)$p.table[2:3,],gcv=h32_lag2$gcv.ubre,deviance=h32_lag2$deviance,r_sq=summary(h32_lag2)$r.sq))
  h32.tb3<-as.data.frame(cbind(summary(h32_lag3)$p.table[2:3,],gcv=h32_lag3$gcv.ubre,deviance=h32_lag3$deviance,r_sq=summary(h32_lag3)$r.sq))
  h32.tb4<-as.data.frame(cbind(summary(h32_lag4)$p.table[2:3,],gcv=h32_lag4$gcv.ubre,deviance=h32_lag4$deviance,r_sq=summary(h32_lag4)$r.sq))
  
  h33.tb0<-as.data.frame(cbind(summary(h33_lag0)$p.table[2:3,],gcv=h33_lag0$gcv.ubre,deviance=h33_lag0$deviance,r_sq=summary(h33_lag0)$r.sq))
  h33.tb1<-as.data.frame(cbind(summary(h33_lag1)$p.table[2:3,],gcv=h33_lag1$gcv.ubre,deviance=h33_lag1$deviance,r_sq=summary(h33_lag1)$r.sq))
  h33.tb2<-as.data.frame(cbind(summary(h33_lag2)$p.table[2:3,],gcv=h33_lag2$gcv.ubre,deviance=h33_lag2$deviance,r_sq=summary(h33_lag2)$r.sq))
  h33.tb3<-as.data.frame(cbind(summary(h33_lag3)$p.table[2:3,],gcv=h33_lag3$gcv.ubre,deviance=h33_lag3$deviance,r_sq=summary(h33_lag3)$r.sq))
  h33.tb4<-as.data.frame(cbind(summary(h33_lag4)$p.table[2:3,],gcv=h33_lag4$gcv.ubre,deviance=h33_lag4$deviance,r_sq=summary(h33_lag4)$r.sq))
  
  p60.tb0<-as.data.frame(cbind(summary(p60_lag0)$p.table[2:3,],gcv=p60_lag0$gcv.ubre,deviance=p60_lag0$deviance,r_sq=summary(p60_lag0)$r.sq))
  p60.tb1<-as.data.frame(cbind(summary(p60_lag1)$p.table[2:3,],gcv=p60_lag1$gcv.ubre,deviance=p60_lag1$deviance,r_sq=summary(p60_lag1)$r.sq))
  p60.tb2<-as.data.frame(cbind(summary(p60_lag2)$p.table[2:3,],gcv=p60_lag2$gcv.ubre,deviance=p60_lag2$deviance,r_sq=summary(p60_lag2)$r.sq))
  p60.tb3<-as.data.frame(cbind(summary(p60_lag3)$p.table[2:3,],gcv=p60_lag3$gcv.ubre,deviance=p60_lag3$deviance,r_sq=summary(p60_lag3)$r.sq))
  p60.tb4<-as.data.frame(cbind(summary(p60_lag4)$p.table[2:3,],gcv=p60_lag4$gcv.ubre,deviance=p60_lag4$deviance,r_sq=summary(p60_lag4)$r.sq))
  
  p65.tb0<-as.data.frame(cbind(summary(p65_lag0)$p.table[2:3,],gcv=p65_lag0$gcv.ubre,deviance=p65_lag0$deviance,r_sq=summary(p65_lag0)$r.sq))
  p65.tb1<-as.data.frame(cbind(summary(p65_lag1)$p.table[2:3,],gcv=p65_lag1$gcv.ubre,deviance=p65_lag1$deviance,r_sq=summary(p65_lag1)$r.sq))
  p65.tb2<-as.data.frame(cbind(summary(p65_lag2)$p.table[2:3,],gcv=p65_lag2$gcv.ubre,deviance=p65_lag2$deviance,r_sq=summary(p65_lag2)$r.sq))
  p65.tb3<-as.data.frame(cbind(summary(p65_lag3)$p.table[2:3,],gcv=p65_lag3$gcv.ubre,deviance=p65_lag3$deviance,r_sq=summary(p65_lag3)$r.sq))
  p65.tb4<-as.data.frame(cbind(summary(p65_lag4)$p.table[2:3,],gcv=p65_lag4$gcv.ubre,deviance=p65_lag4$deviance,r_sq=summary(p65_lag4)$r.sq))
  
  p70.tb0<-as.data.frame(cbind(summary(p70_lag0)$p.table[2:3,],gcv=p70_lag0$gcv.ubre,deviance=p70_lag0$deviance,r_sq=summary(p70_lag0)$r.sq))
  p70.tb1<-as.data.frame(cbind(summary(p70_lag1)$p.table[2:3,],gcv=p70_lag1$gcv.ubre,deviance=p70_lag1$deviance,r_sq=summary(p70_lag1)$r.sq))
  p70.tb2<-as.data.frame(cbind(summary(p70_lag2)$p.table[2:3,],gcv=p70_lag2$gcv.ubre,deviance=p70_lag2$deviance,r_sq=summary(p70_lag2)$r.sq))
  p70.tb3<-as.data.frame(cbind(summary(p70_lag3)$p.table[2:3,],gcv=p70_lag3$gcv.ubre,deviance=p70_lag3$deviance,r_sq=summary(p70_lag3)$r.sq))
  p70.tb4<-as.data.frame(cbind(summary(p70_lag4)$p.table[2:3,],gcv=p70_lag4$gcv.ubre,deviance=p70_lag4$deviance,r_sq=summary(p70_lag4)$r.sq))
  
  p75.tb0<-as.data.frame(cbind(summary(p75_lag0)$p.table[2:3,],gcv=p75_lag0$gcv.ubre,deviance=p75_lag0$deviance,r_sq=summary(p75_lag0)$r.sq))
  p75.tb1<-as.data.frame(cbind(summary(p75_lag1)$p.table[2:3,],gcv=p75_lag1$gcv.ubre,deviance=p75_lag1$deviance,r_sq=summary(p75_lag1)$r.sq))
  p75.tb2<-as.data.frame(cbind(summary(p75_lag2)$p.table[2:3,],gcv=p75_lag2$gcv.ubre,deviance=p75_lag2$deviance,r_sq=summary(p75_lag2)$r.sq))
  p75.tb3<-as.data.frame(cbind(summary(p75_lag3)$p.table[2:3,],gcv=p75_lag3$gcv.ubre,deviance=p75_lag3$deviance,r_sq=summary(p75_lag3)$r.sq))
  p75.tb4<-as.data.frame(cbind(summary(p75_lag4)$p.table[2:3,],gcv=p75_lag4$gcv.ubre,deviance=p75_lag4$deviance,r_sq=summary(p75_lag4)$r.sq))
  
  p80.tb0<-as.data.frame(cbind(summary(p80_lag0)$p.table[2:3,],gcv=p80_lag0$gcv.ubre,deviance=p80_lag0$deviance,r_sq=summary(p80_lag0)$r.sq))
  p80.tb1<-as.data.frame(cbind(summary(p80_lag1)$p.table[2:3,],gcv=p80_lag1$gcv.ubre,deviance=p80_lag1$deviance,r_sq=summary(p80_lag1)$r.sq))
  p80.tb2<-as.data.frame(cbind(summary(p80_lag2)$p.table[2:3,],gcv=p80_lag2$gcv.ubre,deviance=p80_lag2$deviance,r_sq=summary(p80_lag2)$r.sq))
  p80.tb3<-as.data.frame(cbind(summary(p80_lag3)$p.table[2:3,],gcv=p80_lag3$gcv.ubre,deviance=p80_lag3$deviance,r_sq=summary(p80_lag3)$r.sq))
  p80.tb4<-as.data.frame(cbind(summary(p80_lag4)$p.table[2:3,],gcv=p80_lag4$gcv.ubre,deviance=p80_lag4$deviance,r_sq=summary(p80_lag4)$r.sq))
  
  p85.tb0<-as.data.frame(cbind(summary(p85_lag0)$p.table[2:3,],gcv=p85_lag0$gcv.ubre,deviance=p85_lag0$deviance,r_sq=summary(p85_lag0)$r.sq))
  p85.tb1<-as.data.frame(cbind(summary(p85_lag1)$p.table[2:3,],gcv=p85_lag1$gcv.ubre,deviance=p85_lag1$deviance,r_sq=summary(p85_lag1)$r.sq))
  p85.tb2<-as.data.frame(cbind(summary(p85_lag2)$p.table[2:3,],gcv=p85_lag2$gcv.ubre,deviance=p85_lag2$deviance,r_sq=summary(p85_lag2)$r.sq))
  p85.tb3<-as.data.frame(cbind(summary(p85_lag3)$p.table[2:3,],gcv=p85_lag3$gcv.ubre,deviance=p85_lag3$deviance,r_sq=summary(p85_lag3)$r.sq))
  p85.tb4<-as.data.frame(cbind(summary(p85_lag4)$p.table[2:3,],gcv=p85_lag4$gcv.ubre,deviance=p85_lag4$deviance,r_sq=summary(p85_lag4)$r.sq))
  
  p90.tb0<-as.data.frame(cbind(summary(p90_lag0)$p.table[2:3,],gcv=p90_lag0$gcv.ubre,deviance=p90_lag0$deviance,r_sq=summary(p90_lag0)$r.sq))
  p90.tb1<-as.data.frame(cbind(summary(p90_lag1)$p.table[2:3,],gcv=p90_lag1$gcv.ubre,deviance=p90_lag1$deviance,r_sq=summary(p90_lag1)$r.sq))
  p90.tb2<-as.data.frame(cbind(summary(p90_lag2)$p.table[2:3,],gcv=p90_lag2$gcv.ubre,deviance=p90_lag2$deviance,r_sq=summary(p90_lag2)$r.sq))
  p90.tb3<-as.data.frame(cbind(summary(p90_lag3)$p.table[2:3,],gcv=p90_lag3$gcv.ubre,deviance=p90_lag3$deviance,r_sq=summary(p90_lag3)$r.sq))
  p90.tb4<-as.data.frame(cbind(summary(p90_lag4)$p.table[2:3,],gcv=p90_lag4$gcv.ubre,deviance=p90_lag4$deviance,r_sq=summary(p90_lag4)$r.sq))
  
  h26.tb0$label=row.names(h26.tb0);h26.tb1$label=row.names(h26.tb1)
  h26.tb2$label=row.names(h26.tb2);h26.tb3$label=row.names(h26.tb3)
  h26.tb4$label=row.names(h26.tb4);
  
  h27.tb0$label=row.names(h27.tb0);h27.tb1$label=row.names(h27.tb1)
  h27.tb2$label=row.names(h27.tb2);h27.tb3$label=row.names(h27.tb3)
  h27.tb4$label=row.names(h27.tb4);
  
  h28.tb0$label=row.names(h28.tb0);h28.tb1$label=row.names(h28.tb1)
  h28.tb2$label=row.names(h28.tb2);h28.tb3$label=row.names(h28.tb3)
  h28.tb4$label=row.names(h28.tb4);
  
  h29.tb0$label=row.names(h29.tb0);h29.tb1$label=row.names(h29.tb1)
  h29.tb2$label=row.names(h29.tb2);h29.tb3$label=row.names(h29.tb3)
  h29.tb4$label=row.names(h29.tb4);
  
  h30.tb0$label=row.names(h30.tb0);h30.tb1$label=row.names(h30.tb1)
  h30.tb2$label=row.names(h30.tb2);h30.tb3$label=row.names(h30.tb3)
  h30.tb4$label=row.names(h30.tb4);
  
  h31.tb0$label=row.names(h31.tb0);h31.tb1$label=row.names(h31.tb1)
  h31.tb2$label=row.names(h31.tb2);h31.tb3$label=row.names(h31.tb3)
  h31.tb4$label=row.names(h31.tb4);
  
  h32.tb0$label=row.names(h32.tb0);h32.tb1$label=row.names(h32.tb1)
  h32.tb2$label=row.names(h32.tb2);h32.tb3$label=row.names(h32.tb3)
  h32.tb4$label=row.names(h32.tb4);
  
  h33.tb0$label=row.names(h33.tb0);h33.tb1$label=row.names(h33.tb1)
  h33.tb2$label=row.names(h33.tb2);h33.tb3$label=row.names(h33.tb3)
  h33.tb4$label=row.names(h33.tb4);
  
  p60.tb0$label=row.names(p60.tb0);p60.tb1$label=row.names(p60.tb1)
  p60.tb2$label=row.names(p60.tb2);p60.tb3$label=row.names(p60.tb3)
  p60.tb4$label=row.names(p60.tb4);
  
  p65.tb0$label=row.names(p65.tb0);p65.tb1$label=row.names(p65.tb1)
  p65.tb2$label=row.names(p65.tb2);p65.tb3$label=row.names(p65.tb3)
  p65.tb4$label=row.names(p65.tb4);
  
  p70.tb0$label=row.names(p70.tb0);p70.tb1$label=row.names(p70.tb1)
  p70.tb2$label=row.names(p70.tb2);p70.tb3$label=row.names(p70.tb3)
  p70.tb4$label=row.names(p70.tb4);
  
  p75.tb0$label=row.names(p75.tb0);p75.tb1$label=row.names(p75.tb1)
  p75.tb2$label=row.names(p75.tb2);p75.tb3$label=row.names(p75.tb3)
  p75.tb4$label=row.names(p75.tb4);
  
  p80.tb0$label=row.names(p80.tb0);p80.tb1$label=row.names(p80.tb1)
  p80.tb2$label=row.names(p80.tb2);p80.tb3$label=row.names(p80.tb3)
  p80.tb4$label=row.names(p80.tb4);
  
  p85.tb0$label=row.names(p85.tb0);p85.tb1$label=row.names(p85.tb1)
  p85.tb2$label=row.names(p85.tb2);p85.tb3$label=row.names(p85.tb3)
  p85.tb4$label=row.names(p85.tb4)
  
  p90.tb0$label=row.names(p90.tb0);p90.tb1$label=row.names(p90.tb1)
  p90.tb2$label=row.names(p90.tb2);p90.tb3$label=row.names(p90.tb3)
  p90.tb4$label=row.names(p90.tb4)
  
  h26.tb.single<-rbind(h26.tb0,h26.tb1,h26.tb2,h26.tb3,h26.tb4)
  h27.tb.single<-rbind(h27.tb0,h27.tb1,h27.tb2,h27.tb3,h27.tb4)
  h28.tb.single<-rbind(h28.tb0,h28.tb1,h28.tb2,h28.tb3,h28.tb4)
  h29.tb.single<-rbind(h29.tb0,h29.tb1,h29.tb2,h29.tb3,h29.tb4)
  h30.tb.single<-rbind(h30.tb0,h30.tb1,h30.tb2,h30.tb3,h30.tb4)
  h31.tb.single<-rbind(h31.tb0,h31.tb1,h31.tb2,h31.tb3,h31.tb4)
  h32.tb.single<-rbind(h32.tb0,h32.tb1,h32.tb2,h32.tb3,h32.tb4)
  h33.tb.single<-rbind(h33.tb0,h33.tb1,h33.tb2,h33.tb3,h33.tb4)
  
  p60.tb.single<-rbind(p60.tb0,p60.tb1,p60.tb2,p60.tb3,p60.tb4)
  p65.tb.single<-rbind(p65.tb0,p65.tb1,p65.tb2,p65.tb3,p65.tb4)
  p70.tb.single<-rbind(p70.tb0,p70.tb1,p70.tb2,p70.tb3,p70.tb4)
  p75.tb.single<-rbind(p75.tb0,p75.tb1,p75.tb2,p75.tb3,p75.tb4)
  p80.tb.single<-rbind(p80.tb0,p80.tb1,p80.tb2,p80.tb3,p80.tb4)
  p85.tb.single<-rbind(p85.tb0,p85.tb1,p85.tb2,p85.tb3,p85.tb4)
  p90.tb.single<-rbind(p90.tb0,p90.tb1,p90.tb2,p90.tb3,p90.tb4)
  
  h26.tb.single$gubun="single";
  h27.tb.single$gubun="single"; h28.tb.single$gubun="single"
  h29.tb.single$gubun="single"; h30.tb.single$gubun="single"
  h31.tb.single$gubun="single"; h32.tb.single$gubun="single"
  h33.tb.single$gubun="single"
  
  p60.tb.single$gubun="single";p65.tb.single$gubun="single";
  p70.tb.single$gubun="single";p75.tb.single$gubun="single";
  p80.tb.single$gubun="single";p85.tb.single$gubun="single";
  p90.tb.single$gubun="single";
  
  h26.tb.single$lag=paste0("lag",1:5-1);
  h27.tb.single$lag=paste0("lag",1:5-1);h28.tb.single$lag=paste0("lag",1:5-1);
  h29.tb.single$lag=paste0("lag",1:5-1);h30.tb.single$lag=paste0("lag",1:5-1);
  h31.tb.single$lag=paste0("lag",1:5-1);h32.tb.single$lag=paste0("lag",1:5-1);
  h33.tb.single$lag=paste0("lag",1:5-1);
  
  p60.tb.single$lag=paste0("lag",1:5-1);p65.tb.single$lag=paste0("lag",1:5-1);
  p70.tb.single$lag=paste0("lag",1:5-1);p75.tb.single$lag=paste0("lag",1:5-1);
  p80.tb.single$lag=paste0("lag",1:5-1);p85.tb.single$lag=paste0("lag",1:5-1);
  p90.tb.single$lag=paste0("lag",1:5-1);
  
  h26.tb.single$exposure=c("p60D3","pm25");
  h27.tb.single$exposure=c("p60D3","pm25");h28.tb.single$exposure=c("p65D3","pm25");
  h29.tb.single$exposure=c("p60D3","pm25");h30.tb.single$exposure=c("p65D3","pm25");
  h31.tb.single$exposure=c("p65D3","pm25");h32.tb.single$exposure=c("p65D3","pm25");
  h33.tb.single$exposure=c("p65D3","pm25");
  
  p60.tb.single$exposure=c("p60D3","pm25");p65.tb.single$exposure=c("p65D3","pm25");
  p70.tb.single$exposure=c("p70D3","pm25");p75.tb.single$exposure=c("p75D3","pm25");
  p80.tb.single$exposure=c("p80D3","pm25");p85.tb.single$exposure=c("p85D3","pm25");
  p90.tb.single$exposure=c("p90D3","pm25");
  
  res<-rbind(h26.tb.single,h27.tb.single,h28.tb.single,h29.tb.single,
             h30.tb.single,h31.tb.single,h32.tb.single,h33.tb.single,
             p60.tb.single,p65.tb.single,p70.tb.single,p75.tb.single,
             p80.tb.single,p85.tb.single,p90.tb.single)
  res$sido=unique(dat$area);res}

#시도별 모델링
sido01<-sido.gam(s01);
sido02<-sido.gam(s02);
sido03<-sido.gam(s03)
sido04<-sido.gam(s04);
sido05<-sido.gam(s05);
sido06<-sido.gam(s06)
sido07<-sido.gam(s07);
sido08<-sido.gam(s08);
sido09<-sido.gam(s09)
sido10<-sido.gam(s10);
sido11<-sido.gam(s11);
sido12<-sido.gam(s12)
sido13<-sido.gam(s13);
sido14<-sido.gam(s14);
sido15<-sido.gam(s15)

sido.tb<-rbind(sido01,sido02,sido03,sido04,sido05,sido06,sido07,sido08,
               sido09,sido10,sido11,sido12,sido13,sido14,sido15)

setwd("D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\Sumbission\\Chemosphere\\result")
write.csv(sido.tb,file="도시별_폭염_percentile3일이상_QUASI.csv",row.names=F,na="",fileEncoding = "euc-kr")

#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
#Second stage, 메타분석
# tt.sido.tb1<-read.csv("D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\도시별_폭염_percentile3일이상.csv")

tt.sido.tb1<-sido.tb
tt.sido.tb1$`Std. Error`
tt.sido.tb1$SE=tt.sido.tb1$`Std. Error`

meta_func<-function(dataset){
  
  h26_0<-with(dataset %>% filter(label=="heat26D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h26_1<-with(dataset %>% filter(label=="heat26D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h26_2<-with(dataset %>% filter(label=="heat26D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h26_3<-with(dataset %>% filter(label=="heat26D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h26_4<-with(dataset %>% filter(label=="heat26D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.h26<-as.data.frame(rbind(with(h26_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h26_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h26_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h26_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h26_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.h26$lag=paste0("lag",1:5-1);single.h26$exposure="h26"
  
  h27_0<-with(dataset %>% filter(label=="heat27D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h27_1<-with(dataset %>% filter(label=="heat27D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h27_2<-with(dataset %>% filter(label=="heat27D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h27_3<-with(dataset %>% filter(label=="heat27D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h27_4<-with(dataset %>% filter(label=="heat27D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.h27<-as.data.frame(rbind(with(h27_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h27_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h27_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h27_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h27_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.h27$lag=paste0("lag",1:5-1);single.h27$exposure="h27"
  
  h28_0<-with(dataset %>% filter(label=="heat28D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h28_1<-with(dataset %>% filter(label=="heat28D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h28_2<-with(dataset %>% filter(label=="heat28D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h28_3<-with(dataset %>% filter(label=="heat28D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h28_4<-with(dataset %>% filter(label=="heat28D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.h28<-as.data.frame(rbind(with(h28_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h28_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h28_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h28_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h28_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.h28$lag=paste0("lag",1:5-1);single.h28$exposure="h28"
  
  h29_0<-with(dataset %>% filter(label=="heat29D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h29_1<-with(dataset %>% filter(label=="heat29D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h29_2<-with(dataset %>% filter(label=="heat29D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h29_3<-with(dataset %>% filter(label=="heat29D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h29_4<-with(dataset %>% filter(label=="heat29D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.h29<-as.data.frame(rbind(with(h29_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h29_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h29_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h29_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h29_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.h29$lag=paste0("lag",1:5-1);single.h29$exposure="h29"
  
  h30_0<-with(dataset %>% filter(label=="heat30D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h30_1<-with(dataset %>% filter(label=="heat30D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h30_2<-with(dataset %>% filter(label=="heat30D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h30_3<-with(dataset %>% filter(label=="heat30D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h30_4<-with(dataset %>% filter(label=="heat30D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.h30<-as.data.frame(rbind(with(h30_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h30_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h30_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h30_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h30_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.h30$lag=paste0("lag",1:5-1);single.h30$exposure="h30"
  
  h31_0<-with(dataset %>% filter(label=="heat31D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h31_1<-with(dataset %>% filter(label=="heat31D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h31_2<-with(dataset %>% filter(label=="heat31D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h31_3<-with(dataset %>% filter(label=="heat31D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h31_4<-with(dataset %>% filter(label=="heat31D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.h31<-as.data.frame(rbind(with(h31_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h31_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h31_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h31_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h31_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.h31$lag=paste0("lag",1:5-1);single.h31$exposure="h31"
  
  h32_0<-with(dataset %>% filter(label=="heat32D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h32_1<-with(dataset %>% filter(label=="heat32D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h32_2<-with(dataset %>% filter(label=="heat32D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h32_3<-with(dataset %>% filter(label=="heat32D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h32_4<-with(dataset %>% filter(label=="heat32D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.h32<-as.data.frame(rbind(with(h32_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h32_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h32_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h32_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h32_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.h32$lag=paste0("lag",1:5-1);single.h32$exposure="h32"
  
  h33_0<-with(dataset %>% filter(label=="heat33D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h33_1<-with(dataset %>% filter(label=="heat33D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h33_2<-with(dataset %>% filter(label=="heat33D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h33_3<-with(dataset %>% filter(label=="heat33D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  h33_4<-with(dataset %>% filter(label=="heat33D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.h33<-as.data.frame(rbind(with(h33_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h33_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h33_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h33_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(h33_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.h33$lag=paste0("lag",1:5-1);single.h33$exposure="h33"
  
  p60_0<-with(dataset %>% filter(label=="p60D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p60_1<-with(dataset %>% filter(label=="p60D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p60_2<-with(dataset %>% filter(label=="p60D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p60_3<-with(dataset %>% filter(label=="p60D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p60_4<-with(dataset %>% filter(label=="p60D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.p60<-as.data.frame(rbind(with(p60_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p60_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p60_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p60_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p60_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.p60$lag=paste0("lag",1:5-1);single.p60$exposure="p60"
  
  p65_0<-with(dataset %>% filter(label=="p65D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p65_1<-with(dataset %>% filter(label=="p65D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p65_2<-with(dataset %>% filter(label=="p65D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p65_3<-with(dataset %>% filter(label=="p65D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p65_4<-with(dataset %>% filter(label=="p65D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.p65<-as.data.frame(rbind(with(p65_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p65_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p65_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p65_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p65_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.p65$lag=paste0("lag",1:5-1);single.p65$exposure="p65"
  
  p70_0<-with(dataset %>% filter(label=="p70D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p70_1<-with(dataset %>% filter(label=="p70D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p70_2<-with(dataset %>% filter(label=="p70D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p70_3<-with(dataset %>% filter(label=="p70D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p70_4<-with(dataset %>% filter(label=="p70D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.p70<-as.data.frame(rbind(with(p70_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p70_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p70_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p70_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p70_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.p70$lag=paste0("lag",1:5-1);single.p70$exposure="p70"
  
  p75_0<-with(dataset %>% filter(label=="p75D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p75_1<-with(dataset %>% filter(label=="p75D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p75_2<-with(dataset %>% filter(label=="p75D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p75_3<-with(dataset %>% filter(label=="p75D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p75_4<-with(dataset %>% filter(label=="p75D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.p75<-as.data.frame(rbind(with(p75_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p75_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p75_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p75_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p75_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.p75$lag=paste0("lag",1:5-1);single.p75$exposure="p75"
  
  p80_0<-with(dataset %>% filter(label=="p80D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p80_1<-with(dataset %>% filter(label=="p80D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p80_2<-with(dataset %>% filter(label=="p80D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p80_3<-with(dataset %>% filter(label=="p80D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p80_4<-with(dataset %>% filter(label=="p80D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.p80<-as.data.frame(rbind(with(p80_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p80_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p80_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p80_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p80_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.p80$lag=paste0("lag",1:5-1);single.p80$exposure="p80"
  
  p85_0<-with(dataset %>% filter(label=="p85D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p85_1<-with(dataset %>% filter(label=="p85D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p85_2<-with(dataset %>% filter(label=="p85D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p85_3<-with(dataset %>% filter(label=="p85D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p85_4<-with(dataset %>% filter(label=="p85D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.p85<-as.data.frame(rbind(with(p85_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p85_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p85_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p85_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p85_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.p85$lag=paste0("lag",1:5-1);single.p85$exposure="p85"
  
  p90_0<-with(dataset %>% filter(label=="p90D3_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p90_1<-with(dataset %>% filter(label=="p90D3_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p90_2<-with(dataset %>% filter(label=="p90D3_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p90_3<-with(dataset %>% filter(label=="p90D3_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  p90_4<-with(dataset %>% filter(label=="p90D3_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML", control=list(maxiter=1000)))
  
  single.p90<-as.data.frame(rbind(with(p90_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p90_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p90_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p90_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(p90_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.p90$lag=paste0("lag",1:5-1);single.p90$exposure="p90"
  
  rbind(single.h26,single.h27,single.h28,single.h29,single.h30,
        single.h31,single.h32,single.h33,
        single.p60,single.p65,single.p70,single.p75,
        single.p80,single.p85,single.p90)
}

meta1<-meta_func(tt.sido.tb1) 

meta1$RR =exp(meta1$beta);meta1$lci=exp(meta1$beta-1.96*meta1$se);meta1$uci=exp(meta1$beta+1.96*meta1$se)

write.csv(meta1,file="meta2_D3_QUASI.csv",row.names=F,na="")


