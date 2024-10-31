setwd("D:\\EUMC\\논문\\연구논문\\IESEH")
library(readxl)
d<-read_excel("AP_Puberty_results_231111.xlsx",sheet="Cox_PH_results")
d$Sex=factor(d$Sex,levels=unique(d$Sex))

dd1<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model3" & AP=="PM2.5")
dd2<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model3" & AP=="PM10")
dd3<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model3" & AP=="SO2")
dd4<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model3" & AP=="NO2")
dd5<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model3" & AP=="O3")

d$Exposure=factor(d$Exposure,levels=unique(d$Exposure))
d$AP=factor(d$AP,levels=unique(d$AP))

# dd<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model4" & AP %in% c("PM2.5","PM10"))
dd<-subset(d,Dataset=="Dataset2: 상병+약제" & Model=="Model3")

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

er<-read_excel("AP_Puberty_results_231111.xlsx",sheet="노출반응")
h<-read_excel("AP_Puberty_results_231111.xlsx",sheet="event_ER_hist")

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

fig1<-ggplot(er_boys1,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(PM[2.5]," (",mu,g/m^3,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(xlim=c(14,33),ylim=c(0,1.5))+
  geom_bar(data=h_boys1 ,aes(x,density),stat="identity",fill="#1E28D9")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig2<-ggplot(er_boys2,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(PM[10]," (",mu,g/m^3,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(xlim=c(35,max(h_boys2$x)),ylim=c(0,1.5))+
  geom_bar(data=h_boys2 ,aes(x,density),stat="identity",fill="#1E28D9")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig3<-ggplot(er_boys3,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(SO[2]," (",ppb,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(c(min(h_boys3$x),8),ylim=c(0,1.5))+
  geom_bar(data=h_boys3 ,aes(x,density),stat="identity",fill="#1E28D9")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig4<-ggplot(er_boys4,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(NO[2]," (",ppb,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(c(9,40),ylim=c(0,1.5))+
  geom_bar(data=h_boys4 ,aes(x,density),stat="identity",fill="#1E28D9")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig5<-ggplot(er_boys5,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(O[3]," (",ppb,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(c(min(h_boys5$x),32),ylim=c(0,1.5))+
  geom_bar(data=h_boys5 ,aes(x,density),stat="identity",fill="#1E28D9")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

x11();grid.arrange(fig1,fig2,fig3,fig4,fig5,ncol=3)

#그림파일 안깨지개 tiff 파일로 
tiff(width=6600, height=6000, res = 500, 
     filename = "D:\\EUMC\\논문\\연구논문\\IESEH\\boys_ER.tiff", #저장디렉터리 및 저장할 이름.tiff(tiff 파일형식)
     compression = "none", pointsize = 10)
grid.arrange(fig1,fig2,fig3,fig4,fig5,ncol=3)
dev.off()

#F25E86
fig1<-ggplot(er_girls1,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(PM[2.5]," (",mu,g/m^3,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(xlim=c(17,32),ylim=c(0,1.5))+
  geom_bar(data=h_girls1 ,aes(x,density),stat="identity",fill="#F25E86")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig2<-ggplot(er_girls2,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(PM[10]," (",mu,g/m^3,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(xlim=c(30,65),ylim=c(0,1.5))+
  geom_bar(data=h_girls2 ,aes(x,density),stat="identity",fill="#F25E86")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig3<-ggplot(er_girls3,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(SO[2]," (",ppb,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_girls3 ,aes(x,density),stat="identity",fill="#F25E86")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig4<-ggplot(er_girls4,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(NO[2]," (",ppb,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_girls4 ,aes(x,density),stat="identity",fill="#F25E86")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

fig5<-ggplot(er_girls5,aes(x,exp(y)))+geom_line(size=1.1)+
  geom_line(aes(x,exp(y-1.96*se)),linetype=2,size=1.1)+
  geom_line(aes(x,exp(y+1.96*se)),linetype=2,size=1.1)+labs(x=expression(paste(O3[3]," (",ppb,")")),y="Hazard ratio (95% CI)")+
  theme_bw(base_size=22)+coord_cartesian(ylim=c(0,1.5))+
  geom_bar(data=h_girls5 ,aes(x,density),stat="identity",fill="#F25E86")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

x11();grid.arrange(fig1,fig2,fig3,fig4,fig5,ncol=3)
#그림파일 안깨지개 tiff 파일로 
tiff(width=6600, height=6000, res = 500, 
     filename = "D:\\EUMC\\논문\\연구논문\\IESEH\\girls_ER.tiff", #저장디렉터리 및 저장할 이름.tiff(tiff 파일형식)
     compression = "none", pointsize = 10)
grid.arrange(fig1,fig2,fig3,fig4,fig5,ncol=3)
dev.off()
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

x11();ggplot(dd1,aes(AP,HR_rev,shape=Exposure))+
  geom_point(size=3,position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=HR_rev_L,ymax=HR_rev_U),width=0.2,lwd=1.1,position=position_dodge(0.5))+facet_wrap(~LBW)+
  geom_hline(yintercept=1,col="red")+
  scale_shape_manual(values=c(16,15,17,18))+
  theme_gray(base_size=25)+labs(x="",y="Hazard ratio (95% Confidence intervals)")+
  theme(legend.position="top",legend.title=element_blank())

x11();ggplot(dd2,aes(AP,HR_rev,shape=Exposure))+
  geom_point(size=3,position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=HR_rev_L,ymax=HR_rev_U),width=0.2,lwd=1.1,position=position_dodge(0.5))+facet_wrap(~LBW)+
  geom_hline(yintercept=1,col="red")+
  scale_shape_manual(values=c(16,15,17,18))+
  theme_gray(base_size=25)+labs(x="",y="Hazard ratio (95% Confidence intervals)")+
  theme(legend.position="top",legend.title=element_blank())
