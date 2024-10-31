pacman::p_load(dplyr,ggplot2,gridExtra,readxl,lubridate)

setwd("D:\\EUMC\\논문\\연구논문\\AP_Puberty\\분석")

d<-read_excel("AP_Puberty_results.xlsx",sheet="Cox_PH_results")
d$Sex=factor(d$Sex,levels=unique(d$Sex))

dd1<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP=="PM2.5")
dd2<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP=="PM10")
dd3<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP=="SO2")
dd4<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP=="NO2")
dd5<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP=="O3")

d$Exposure=factor(d$Exposure,levels=unique(d$Exposure))
d$AP=factor(d$AP,levels=unique(d$AP))

# dd<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP %in% c("PM2.5","PM10"))
dd<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model3")


x11();ggplot(dd,aes(AP,HR,group=Exposure,shape=Exposure))+geom_point(size=3,position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=HR_LCI,ymax=HR_UCI),width=0.2,lwd=1.1,position=position_dodge(0.5))+facet_wrap(~Sex)+
  geom_hline(yintercept=1,col="red")+
  scale_shape_manual(values=c(16,15,17,18))+
  theme_gray(base_size=25)+labs(x="",y="Hazard ratio (95% Confidence intervals)")+
  theme(legend.position="top",legend.title=element_blank())


x11();ggplot(dd,aes(AP,HR_rev,group=Exposure,shape=Exposure))+geom_point(size=3,position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=HR_rev_L,ymax=HR_rev_U),width=0.2,lwd=1.1,position=position_dodge(0.5))+facet_wrap(~Sex)+
  geom_hline(yintercept=1,col="red")+
  scale_shape_manual(values=c(16,15,17,18))+
  theme_gray(base_size=25)+labs(x="",y="Hazard ratio (95% Confidence intervals)")+
  theme(legend.position="top",legend.title=element_blank())

x11();ggplot(dd1,aes(Exposure,HR))+geom_point(size=4)+
  geom_errorbar(aes(ymin=HR_LCI,ymax=HR_UCI),width=0.2,lwd=1.1)+facet_wrap(~Sex)+
  geom_hline(yintercept=1,col="red")+
  theme_gray(base_size=25)+labs(x="",y="Hazard ratio (95% Confidence intervals)")

x11();ggplot(dd2,aes(Exposure,HR))+geom_point(size=4)+
  geom_errorbar(aes(ymin=HR_LCI,ymax=HR_UCI),width=0.2,lwd=1.1)+facet_wrap(~Sex)+
  geom_hline(yintercept=1,col="red")+
  theme_gray(base_size=25)+labs(x="",y="Hazard ratio (95% Confidence intervals)")

x11();ggplot(dd3,aes(Exposure,HR))+geom_point(size=4)+
  geom_errorbar(aes(ymin=HR_LCI,ymax=HR_UCI),width=0.2,lwd=1.1)+facet_wrap(~Sex)+
  geom_hline(yintercept=1,col="red")+
  theme_gray(base_size=25)+labs(x="",y="Hazard ratio (95% Confidence intervals)")

x11();ggplot(dd4,aes(Exposure,HR))+geom_point(size=4)+
  geom_errorbar(aes(ymin=HR_LCI,ymax=HR_UCI),width=0.2,lwd=1.1)+facet_wrap(~Sex)+
  geom_hline(yintercept=1,col="red")+
  theme_gray(base_size=25)+labs(x="",y="Hazard ratio (95% Confidence intervals)")

x11();ggplot(dd5,aes(Exposure,HR))+geom_point(size=4)+
  geom_errorbar(aes(ymin=HR_LCI,ymax=HR_UCI),width=0.2,lwd=1.1)+facet_wrap(~Sex)+
  geom_hline(yintercept=1,col="red")+
  theme_gray(base_size=25)+labs(x="",y="Hazard ratio (95% Confidence intervals)")

#-----------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------#

er<-read_excel("AP_Puberty_results.xlsx",sheet="노출반응")
h<-read_excel("AP_Puberty_results.xlsx",sheet="event_ER_hist")

table(er$exposure)
er_boys1<-subset(er,lag=="48-month" & Sex!="Girls" & exposure=="PM2.5")
er_boys2<-subset(er,lag=="48-month" & Sex!="Girls" & exposure=="PM10")
er_boys3<-subset(er,lag=="48-month" & Sex!="Girls" & exposure=="SO2")
er_boys4<-subset(er,lag=="48-month" & Sex!="Girls" & exposure=="NO2")
er_boys5<-subset(er,lag=="48-month" & Sex!="Girls" & exposure=="O3")
h_boys1 <-subset(h,lag=="48-month" & Sex!="Girls" & exposure=="PM2.5")
h_boys2 <-subset(h,lag=="48-month" & Sex!="Girls" & exposure=="PM10")
h_boys3 <-subset(h,lag=="48-month" & Sex!="Girls" & exposure=="SO2")
h_boys4 <-subset(h,lag=="48-month" & Sex!="Girls" & exposure=="NO2")
h_boys5 <-subset(h,lag=="48-month" & Sex!="Girls" & exposure=="O3")

er_girls1<-subset(er,lag=="48-month" & Sex=="Girls" & exposure=="PM2.5")
er_girls2<-subset(er,lag=="48-month" & Sex=="Girls" & exposure=="PM10")
er_girls3<-subset(er,lag=="48-month" & Sex=="Girls" & exposure=="SO2")
er_girls4<-subset(er,lag=="48-month" & Sex=="Girls" & exposure=="NO2")
er_girls5<-subset(er,lag=="48-month" & Sex=="Girls" & exposure=="O3")
h_girls1 <-subset(h,lag=="48-month" & Sex=="Girls" & exposure=="PM2.5")
h_girls2 <-subset(h,lag=="48-month" & Sex=="Girls" & exposure=="PM10")
h_girls3 <-subset(h,lag=="48-month" & Sex=="Girls" & exposure=="SO2")
h_girls4 <-subset(h,lag=="48-month" & Sex=="Girls" & exposure=="NO2")
h_girls5 <-subset(h,lag=="48-month" & Sex=="Girls" & exposure=="O3")

x11();grid.arrange(
ggplot(h_boys1,aes(x,density))+geom_bar(data=h_boys1 ,aes(x,density),stat="identity",fill="#1E28D9"),
ggplot(h_boys2,aes(x,density))+geom_bar(data=h_boys2 ,aes(x,density),stat="identity",fill="#1E28D9"),
ggplot(h_boys3,aes(x,density))+geom_bar(data=h_boys3 ,aes(x,density),stat="identity",fill="#1E28D9"),
ggplot(h_boys4,aes(x,density))+geom_bar(data=h_boys4 ,aes(x,density),stat="identity",fill="#1E28D9"),
ggplot(h_boys5,aes(x,density))+geom_bar(data=h_boys5 ,aes(x,density),stat="identity",fill="#1E28D9"))

head(h_boys1)

fig1<-ggplot(er_boys1,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(PM[2.5]," (",mu,g/m^3,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_boys1 ,aes(x,y/1000),stat="identity",fill="#1E28D9")+
  scale_y_continuous(sec.axis= sec_axis(~. *1000, name = "Frequency"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig2<-ggplot(er_boys2,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(PM[10]," (",mu,g/m^3,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_boys2 ,aes(x,y/1000),stat="identity",fill="#1E28D9")+
  scale_y_continuous(sec.axis= sec_axis(~. *1000, name = "Frequency"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig3<-ggplot(er_boys3,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(SO[2]," (",ppb,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_boys3 ,aes(x,y/1000),stat="identity",fill="#1E28D9")+
  scale_y_continuous(sec.axis= sec_axis(~. *1000, name = "Frequency"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig4<-ggplot(er_boys4,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(NO[2]," (",ppb,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_boys4 ,aes(x,y/1000),stat="identity",fill="#1E28D9")+
  scale_y_continuous(sec.axis= sec_axis(~. *1000, name = "Frequency"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig5<-ggplot(er_boys5,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(O[3]," (",ppb,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_boys5 ,aes(x,y/1000),stat="identity",fill="#1E28D9")+
  scale_y_continuous(sec.axis= sec_axis(~. *1000, name = "Frequency"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

x11();grid.arrange(fig1,fig2,fig3,fig4,fig5,ncol=3)

#F25E86
fig1<-ggplot(er_girls1,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(PM[2.5]," (",mu,g/m^3,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_girls1,aes(x,y/10000),stat="identity",fill="#F25E86")+
  scale_y_continuous(sec.axis= sec_axis(~. *10000, name = "Frequency"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig2<-ggplot(er_girls2,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(PM[10]," (",mu,g/m^3,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_girls2,aes(x,y/10000),stat="identity",fill="#F25E86")+
  scale_y_continuous(sec.axis= sec_axis(~. *10000, name = "Frequency"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig3<-ggplot(er_girls3,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(SO[2]," (",ppb,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_girls3,aes(x,y/10000),stat="identity",fill="#F25E86")+
  scale_y_continuous(sec.axis= sec_axis(~. *10000, name = "Frequency"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig4<-ggplot(er_girls4,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(NO[2]," (",ppb,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_girls4,aes(x,y/10000),stat="identity",fill="#F25E86")+
  scale_y_continuous(sec.axis= sec_axis(~. *10000, name = "Frequency"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig5<-ggplot(er_girls5,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(O3[3]," (",ppb,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_girls5,aes(x,y/10000),stat="identity",fill="#F25E86")+
  scale_y_continuous(sec.axis= sec_axis(~. *10000, name = "Frequency"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

x11();grid.arrange(fig1,fig2,fig3,fig4,fig5,ncol=3)

#-----------------------------------------------------------------------------------------------#
#저체중아 구분
d<-read_excel("AP_Puberty_results.xlsx",sheet="Cox_PH_LBW_results")

d$Sex=factor(d$Sex,levels=unique(d$Sex))
d$Exposure=factor(d$Exposure,levels=unique(d$Exposure))
d$AP=factor(d$AP,levels=unique(d$AP))
# dd<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP %in% c("PM2.5","PM10"))
dd<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model3")
dd$LBW=factor(dd$LBW,levels=unique(dd$LBW))
dd1<-subset(dd,Sex=="Boys")
dd2<-subset(dd,Sex=="Girls")

x11();ggplot(dd1,aes(AP,HR,shape=Exposure))+
  geom_point(size=3,position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=HR_LCI,ymax=HR_UCI),width=0.2,lwd=1.1,position=position_dodge(0.5))+facet_wrap(~LBW)+
  geom_hline(yintercept=1,col="red")+
  scale_shape_manual(values=c(16,15,17,18))+
  theme_gray(base_size=25)+labs(x="",y="Hazard ratio (95% Confidence intervals)")+
  theme(legend.position="top",legend.title=element_blank())

x11();ggplot(dd2,aes(AP,HR,shape=Exposure))+
  geom_point(size=3,position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=HR_LCI,ymax=HR_UCI),width=0.2,lwd=1.1,position=position_dodge(0.5))+facet_wrap(~LBW)+
  geom_hline(yintercept=1,col="red")+
  scale_shape_manual(values=c(16,15,17,18))+
  theme_gray(base_size=25)+labs(x="",y="Hazard ratio (95% Confidence intervals)")+
  theme(legend.position="top",legend.title=element_blank())

#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
mat<-rbind(c(1.000,0.983,0.805,0.701,0.014),
           c(0.983,1.000,0.813,0.694,0.014),
           c(0.805,0.813,1.000,0.879,-0.353),
           c(0.701,0.694,0.879,1.000,-0.596),
           c(0.014,0.014,-0.353,-0.596,1.000))

row.names(mat)=c("PM2.5","PM10","SO2","NO2","O3")
colnames(mat)=c("PM2.5","PM10","SO2","NO2","O3")

x11();ggcorrplot::ggcorrplot(mat,lab=T,type="lower",insig="blank",lab_size=7)

#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
multi<-read_excel("AP_Puberty_results.xlsx",sheet="Two-pollutant")
multi$Sex=factor(multi$Sex,levels=unique(multi$Sex))

multi1<-subset(multi,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP1=="PM2.5" & Sex %in% c("Boys","Girls"))
multi2<-subset(multi,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP1=="PM10")
multi3<-subset(multi,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP1=="SO2")
multi4<-subset(multi,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP1=="NO2")
multi5<-subset(multi,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP1=="O3")

multi$Exposure=factor(multi$Exposure,levels=unique(multi$Exposure))
multi$AP1=factor(multi$AP1,levels=unique(multi$AP1))

# dd<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP %in% c("PM2.5","PM10"))
multi<-subset(multi,Dataset=="Dataset2: 상병+약제" & Model=="Model4")

x11();ggplot(multi,aes(AP1,HR_rev,group=Exposure,shape=Exposure))+geom_point(size=3,position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=HR_rev_L,ymax=HR_rev_U),width=0.2,lwd=1.1,position=position_dodge(0.5))+facet_wrap(~Sex)+
  geom_hline(yintercept=1,col="red")+
  scale_shape_manual(values=c(16,15,17,18))+
  theme_gray(base_size=25)+labs(x="",y="Hazard ratio (95% Confidence intervals)")+
  theme(legend.position="top",legend.title=element_blank())

#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
#city specific
d<-read_excel("AP_Puberty_results.xlsx",sheet="city_specific")

dd1<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP=="PM2.5")
dd2<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP=="PM10")
dd3<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP=="SO2")
dd4<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP=="NO2")
dd5<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP=="O3")

d$Exposure=factor(d$Exposure,levels=unique(d$Exposure))
d$AP=factor(d$AP,levels=unique(d$AP))

# dd<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP %in% c("PM2.5","PM10"))
dd<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & Sex %in% c("Boys","Girls"))

city1<-subset(dd,Exposure=="48-month" & Sex=="Boys")
city2<-subset(dd,Exposure=="48-month" & Sex=="Girls")

# View(city1)
head(city1)
table(dd$AP)

city_ap1<-subset(dd,Exposure=="36-month" & Sex=="Boys" & AP=="PM2.5")
city_ap2<-subset(dd,Exposure=="36-month" & Sex=="Boys" & AP=="PM10")
city_ap3<-subset(dd,Exposure=="36-month" & Sex=="Boys" & AP=="SO2")
city_ap4<-subset(dd,Exposure=="36-month" & Sex=="Boys" & AP=="NO2")
city_ap5<-subset(dd,Exposure=="36-month" & Sex=="Boys" & AP=="O3")

m_ap1 <- with(city_ap1,rma(yi=Estimate, sei=SE, slab=City, measure="RR",digits=5,method="REML"))
m_ap2 <- with(city_ap2,rma(yi=Estimate, sei=SE, slab=City, measure="RR",digits=5,method="REML"))
m_ap3 <- with(city_ap3,rma(yi=Estimate, sei=SE, slab=City, measure="RR",digits=5,method="REML"))
m_ap4 <- with(city_ap4,rma(yi=Estimate, sei=SE, slab=City, measure="RR",digits=5,method="REML"))
m_ap5 <- with(city_ap5,rma(yi=Estimate, sei=SE, slab=City, measure="RR",digits=5,method="REML"))

city_ap1<-subset(dd,Exposure=="36-month" & Sex!="Boys" & AP=="PM2.5")
city_ap2<-subset(dd,Exposure=="36-month" & Sex!="Boys" & AP=="PM10")
city_ap3<-subset(dd,Exposure=="36-month" & Sex!="Boys" & AP=="SO2")
city_ap4<-subset(dd,Exposure=="36-month" & Sex!="Boys" & AP=="NO2")
city_ap5<-subset(dd,Exposure=="36-month" & Sex!="Boys" & AP=="O3")

w_ap1 <- with(city_ap1,rma(yi=Estimate, sei=SE, slab=City, measure="RR",digits=5,method="REML"))
w_ap2 <- with(city_ap2,rma(yi=Estimate, sei=SE, slab=City, measure="RR",digits=5,method="REML"))
w_ap3 <- with(city_ap3,rma(yi=Estimate, sei=SE, slab=City, measure="RR",digits=5,method="REML"))
w_ap4 <- with(city_ap4,rma(yi=Estimate, sei=SE, slab=City, measure="RR",digits=5,method="REML"))
w_ap5 <- with(city_ap5,rma(yi=Estimate, sei=SE, slab=City, measure="RR",digits=5,method="REML"))

# x11();forest(m_ap1,transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=1,fontsize=1,1,digits=3,
#              showweights=TRUE)
# x11();forest(m_ap2,transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=1,fontsize=1,1,digits=3,
#              showweights=TRUE)


# x11();forest(w_ap1,transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=1,fontsize=1,1,digits=3,
#              showweights=TRUE)
# x11();forest(w_ap2,transf=exp, refline=1, bg=4, col=2,cex.lab=1.6,cex.axis=1.6,cex=1,fontsize=1,1,digits=3,
#              showweights=TRUE)

zz<-rbind(rbind(c(m_ap1$beta,m_ap1$se),
      c(m_ap2$beta,m_ap2$se),
      c(m_ap3$beta,m_ap3$se),
      c(m_ap4$beta,m_ap4$se),
      c(m_ap5$beta,m_ap5$se)),
rbind(c(w_ap1$beta,w_ap1$se),
      c(w_ap2$beta,w_ap2$se),
      c(w_ap3$beta,w_ap3$se),
      c(w_ap4$beta,w_ap4$se),
      c(w_ap5$beta,w_ap5$se)))
write.csv(zz,file="zz.csv")
