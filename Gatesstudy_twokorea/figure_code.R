#######################
###게이츠 재단 연구 ###
#######################

#-----------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------#
#library
pacman::p_load("dplyr","ggplot2","reshape2","mgcv","sqldf","psych","Rmisc","lsmeans",
               "doBy","RColorBrewer","lubridate","caret","e1071","lmtest","readxl","splines",
               "metafor","mixmeta","data.table","stringr","lme4","gee","geepack","ggrepel","MuMIn",
               "qcc","extrafont","scales","gamm4","gridExtra","survey","plotrix","trend","tsModel")

#---------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------#
setwd("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data")
dat<-read_excel("게이츠재단연구_2021_정리.xlsx",sheet="게이츠재단연구_2021_정리")

table(dat$country)

s1<-subset(dat,country=="North Korea") #북한
s2<-subset(dat,country!="South Korea") #남한

x11();par(mfrow=c(1,2));hist(log(s1$Infant_m));hist(log(s2$Infant_m))

f1<-lm(log(Infant_m)~WB_PM25,data=s1)
f2<-lm(log(Infant_m)~WB_PM25,data=s2)

f1<-lm(log(Infant_m)~WB_PM25,data=s1)
f2<-lm(log(Infant_m)~OECD2_PM25,data=s2)

summary(f1)
summary(f2)

sens.slope(na.omit(s1$ALRI_rate))
mk.test(na.omit(s1$ALRI_rate))

x11();ggplot(dat,aes(year,Sepsis_rate,col=country))+geom_point(size=3)+geom_line(size=1.1)+theme_minimal(base_size=15)+
  labs(x="Year",y="Sepsis and other infections (per 1,000 death)")+
  facet_wrap(~country,scales="free",ncol=1)+scale_color_manual(values=c("red","blue"))+
  theme(legend.position="top",
        legend.title=element_blank(),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+scale_x_continuous(breaks=2000:2017)

#-----------------------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------------------#
#Table 1
setwd("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data\\자료정리\\Final")

dat<-read_excel("Gates_north_south_korea.xlsx")

ss1<-sens.slope(dat$life_n)
ss2<-sens.slope(dat$life_s)

mk1<-mk.test(dat$life_n)
mk2<-mk.test(dat$life_s)

lm1<-lm(life_n~year,dat)
lm2<-lm(life_s~year,dat)

trend.tb1<-data.frame(sen    =ss1$estimates,
                      sen_lci=ss1$conf.int[1],
                      sen_uci=ss1$conf.int[2],
                      mk     =mk1$statistic,
                      mk_p   =round(mk1$p.value,3),
                      lm     =lm1$coefficients[2],
                      lm_lci =lm1$coeff[2]-1.96*summary(lm1)$coeff[2,2],
                      lm_uci =lm1$coeff[2]+1.96*summary(lm1)$coeff[2,2])

trend.tb2<-data.frame(sen    =ss2$estimates,
                      sen_lci=ss2$conf.int[1],
                      sen_uci=ss2$conf.int[2],
                      mk     =mk2$statistic,
                      mk_p   =round(mk2$p.value,3),
                      lm     =lm2$coefficients[2],
                      lm_lci =lm2$coeff[2]-1.96*summary(lm2)$coeff[2,2],
                      lm_uci =lm2$coeff[2]+1.96*summary(lm2)$coeff[2,2])

rbind(trend.tb1,trend.tb2)

#---------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------#
format(1000000, big.mark = " ", scientific = FALSE)
#북한: 식량 난으로 인한 초과사망 및 출생 손실 추정
df<-data.frame(year=c(1994:2005),
               y=c(8,34,61,88,74,62,51,40,30,20,11,3,NA,2,3,17,32,29,18,12,7,5,2,NA),
               category=rep(c("Estimating excess deaths","Estimating birth loss"),each=12))

df$y2=df$y*1000
x11();ggplot(df,aes(year,y2,col=category))+geom_line(size=1.5)+scale_x_continuous(breaks=c(1994:2005))+
  labs(x="Year",y="N")+theme_minimal(base_size=25)+
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",scientific = FALSE))+
  geom_vline(xintercept = 1995,col="grey",size=5,alpha=0.5)+
  geom_vline(xintercept = 2000,col="grey",size=5,alpha=0.5)+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        legend.title = element_blank(),legend.position = "top",panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text = element_text())+
  geom_segment(aes(x=1995,y=90000,xend=2000,yend=90000),col="orange",size=1.5, arrow=arrow(ends="both",type="closed"))+
  geom_text(aes(x=1997.5,y=92500),label="The North Korean famine",size=8,col="black")+
  scale_color_manual(values=c("red","blue"))

ggsave(paste0("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S1.tiff"),width=18,height=10,dpi=300)
#---------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#
setwd("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data")
dat<-read_excel("남북_비교.xlsx")


dat<-subset(dat,Country!="OECD average")
dat$Country=factor(dat$Country,levels=c("South Korea","North Korea"))
x11();ggplot(dat,aes(Year,x,col=Country))+geom_line(size=1.5)+scale_x_continuous(breaks=c(1990:2019))+
  labs(x="Year",y="Mortality rate, infant (per 1,000 live births)")+theme_minimal(base_size=18)+
  geom_vline(xintercept = 1995,col="grey",size=5,alpha=0.5)+
  geom_vline(xintercept = 2000,col="grey",size=5,alpha=0.5)+
  geom_segment(aes(x=1990,xend=2019,y=4.1,yend=4.1),col="black",size=1.5)+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        legend.title = element_blank(),legend.position = "top",
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.text = element_text())+
  
  geom_segment(aes(x=1995,y=56,xend=2000,yend=56),col="orange",size=1, arrow=arrow(ends="both",type="closed"))+
  geom_text(aes(x=1997.5,y=58),label="The North Korean famine",size=5,col="black")+
  geom_text(aes(x=2015,y=6.5),label="OECD average: 4.1 (2018 year)",size=5,col="black")+
  scale_color_manual(values=c("blue","red"))+
  scale_y_continuous(breaks=c(5,10,15,20,25,30,35,40,45,50,55,60,65))

ggsave(paste0("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S2.tiff"),width=18,height=10,dpi=300)
#---------------------------------------------------------------------------------------------------------------#
setwd("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data")
dat<-read_excel("남북_비교.xlsx",sheet=2)
dat$Country=factor(dat$Country,levels=c("South Korea","North Korea"))
x11();ggplot(dat,aes(x=연도,y=x,fill=Country))+geom_bar(stat="identity",position = 'dodge')+
  scale_x_continuous(breaks=c(2001:2018))+theme_minimal(base_size=22)+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text = element_text())+labs(x="Year",y="Prevalence of undernourishment (% of population)")+
  scale_fill_manual(values=c("blue","red"))
ggsave(paste0("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S3.tiff"),width=18,height=10,dpi=300)
#---------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------#
setwd("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data")
dat<-read_excel("남북_비교.xlsx",sheet=3)

dd<-dat %>% arrange(infant_m)
dd$country=factor(dd$country,levels=dd$country)

x11();ggplot(dd,aes(x=country,y=infant_m,fill=country))+geom_bar(stat="identity",position = 'dodge')+coord_flip()+
  theme_minimal(base_size=22)+
  geom_text(aes(y=infant_m+0.2,label = round(infant_m,2)),size=5)+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        legend.title = element_blank(),panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text = element_text())+labs(x="",y="Infant mortality rate (2018 years) (unit: 1,000 birth)")

#Another colors 
x11();ggplot(dd,aes(x=country,y=infant_m,fill=country))+geom_bar(stat="identity",position = 'dodge')+coord_flip()+
  theme_minimal(base_size=22)+
  geom_text(aes(y=infant_m+0.4,label = round(infant_m,2)),size=5)+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        legend.title = element_blank(),legend.position = "none",
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text = element_text())+labs(x="",y="Infant mortality rate (2018 years) (unit: 1,000 live births)")+
  scale_fill_manual(values=c(rep("darkgreen",each=11),
                             rep("blue"),rep("darkgreen",each=16),
                             rep("orange"),rep("darkgreen",each=9)))

#---------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------#
setwd("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data")

dat<-read_excel("남북_비교.xlsx",sheet=4)
d2<-subset(dat,Country %in% c("South Korea","North Korea","OECD??average"))
d2$Country=factor(d2$Country,levels=c("South Korea","North Korea","OECD??average"))

x11();ggplot(d2,aes(x=year,y=x,col=Country))+geom_line(size=1.4)+theme_bw(base_size=25)+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(),
        legend.position = "top")+scale_color_manual(values=c("blue","red","orange"))+
  labs(x="Year",y=expression(paste(PM[2.5]," (",mu,g/m^3,")")))+ylim(5,50)+
  geom_hline(yintercept=10,col="black",size=1.4)+
  geom_text(x=2018,y=8.5,size=7,label="WHO air quality guidelines",show.legend = F,col="black")

ggsave(paste0("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S4.tiff"),width=18,height=10,dpi=300)
#---------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------#
setwd("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data")

dat<-read_excel("남북_비교.xlsx",sheet="인구통계자료")
dat$s_tfr=as.numeric(dat$s_tfr)
dat$north =as.numeric(dat$north)
dat$north_m=as.numeric(dat$north_m)
dat$north_f=as.numeric(dat$north_f)
dat$south =as.numeric(dat$south)
dat$south_m=as.numeric(dat$south_m)
dat$south_f=as.numeric(dat$south_f)

apply(dat %>% select(south:s_tfr),2,mean) %>% round(1)
apply(dat %>% select(south:s_tfr),2,sd)   %>% round(2)

trend_compare<- function(var1,var2){
  ss1<-sens.slope(var1)
  ss2<-sens.slope(var2)
  
  mk1<-mk.test(var1)
  mk2<-mk.test(var2)
  
  lm1<-lm(var1~year,dat)
  lm2<-lm(var2~year,dat)
  
  trend.tb1<-data.frame(sen    =ss1$estimates %>% round(1),
                        sen_lci=ss1$conf.int[1]%>% round(1),
                        sen_uci=ss1$conf.int[2]%>% round(1),
                        mk     =mk1$statistic %>% round(1),
                        mk_p   =round(mk1$p.value,3),
                        lm     =lm1$coefficients[2]%>% round(1),
                        lm_lci =round(lm1$coeff[2]-1.96*summary(lm1)$coeff[2,2],1),
                        lm_uci =round(lm1$coeff[2]+1.96*summary(lm1)$coeff[2,2],1))
  
  trend.tb2<-data.frame(sen    =ss2$estimates %>% round(1),
                        sen_lci=ss2$conf.int[1] %>% round(1),
                        sen_uci=ss2$conf.int[2] %>% round(1),
                        mk     =mk2$statistic %>% round(1),
                        mk_p   =round(mk2$p.value,3),
                        lm     =lm2$coefficients[2] %>% round(1),
                        lm_lci =round(lm2$coeff[2]-1.96*summary(lm2)$coeff[2,2],1),
                        lm_uci =round(lm2$coeff[2]+1.96*summary(lm2)$coeff[2,2],1))
  
  rbind(trend.tb1,trend.tb2) }

trend_compare(dat$north,dat$south)
trend_compare(dat$north_m,dat$south_m)
trend_compare(dat$north_f,dat$south_f)
trend_compare(dat$n_tfr,dat$s_tfr)

#---------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------#
setwd("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data")

dat<-read_excel("남북_비교.xlsx",sheet="연료사용")
dat1<-subset(dat,group==1 & year %in% c(2000:2018))
x11();ggplot(dat1,aes(year,value,fill=category))+geom_bar(stat="identity")+facet_wrap(~country,scales="free")+
  labs(x="Year",y="Consumption")+theme_gray(base_size=20)+theme(legend.title = element_blank(),
                                                                legend.position = "top")

dat1$country=factor(dat1$country,levels=c("South Korea","North Korea"))
x11();ggplot(dat1,aes(year,value,col=category))+geom_line(size=2)+facet_wrap(~country,scales="free")+
  labs(x="Year",y="Consumption")+theme_gray(base_size=20)+theme(legend.title = element_blank(),
                                                                legend.position = "top")
ggsave(paste0("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S11.tiff"),width=18,height=10,dpi=300)


x11();ggplot(dat1,aes(year,value,fill=country))+geom_bar(stat="identity",position="dodge")+
  facet_wrap(~category,scales="free")+labs(x="Year",y="Coal consumption (Mst)")+theme_gray(base_size=20)+
  theme(legend.title = element_blank(),legend.position = "top")+
  scale_fill_manual(values=c("red","blue"))+theme_gray(base_size=20)+theme(legend.title = element_blank(),
                                                                           legend.position = "top")



dat2<-subset(dat,group==2 & year %in% c(2000:2018))
x11();ggplot(dat2,aes(year,value,fill=category))+geom_bar(stat="identity")+facet_wrap(~country,scales="free")+
  labs(x="Year",y="Coal consumption (Mst)")+theme_gray(base_size=20)+theme(legend.title = element_blank(),
                                                                           legend.position = "top")


x11();ggplot(dat2,aes(year,value,col=category))+geom_line(size=2)+facet_wrap(~country,scales="free")+
  labs(x="Year",y="Coal consumption (Mst)")+theme_gray(base_size=20)+theme(legend.title = element_blank(),
                                                                           legend.position = "top")

dat2$country=factor(dat2$country,levels=c("South Korea","North Korea"))
x11();ggplot(dat2,aes(year,value,fill=country))+geom_bar(stat="identity",position="dodge")+
  facet_wrap(~category,scales="free")+labs(x="Year",y="Coal consumption (Mst)")+theme_gray(base_size=20)+
  theme(legend.title = element_blank(),legend.position = "top")+
  scale_fill_manual(values=c("blue","red"))+theme_gray(base_size=20)+theme(legend.title = element_blank(),
                                                                           legend.position = "top")
ggsave(paste0("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S12.tiff"),width=18,height=10,dpi=300)

dat3<-subset(dat,group==3 & year %in% c(2000:2018))
x11();ggplot(dat3,aes(year,value,fill=category))+geom_bar(stat="identity")+facet_wrap(~country,scales="free")+
  labs(x="Year",y="Coal consumption (Mst)")+theme_gray(base_size=20)+theme(legend.title = element_blank(),
                                                                           legend.position = "top")


x11();ggplot(dat3,aes(year,value,col=category))+geom_line(size=2)+facet_wrap(~country,scales="free")+
  labs(x="Year",y="Coal consumption (Mst)")+theme_gray(base_size=20)+theme(legend.title = element_blank(),
                                                                           legend.position = "top")

dat3$country=factor(dat3$country,levels=c("South Korea","North Korea"))
x11();ggplot(dat3,aes(year,value,fill=country))+geom_bar(stat="identity",position="dodge")+
  facet_wrap(~category,scales="free")+labs(x="Year",y="Electricity Generation (billion kWh)")+theme_gray(base_size=20)+
  theme(legend.title = element_blank(),legend.position = "top")+
  scale_fill_manual(values=c("blue","red"))+theme_gray(base_size=20)+theme(legend.title = element_blank(),
                                                                           legend.position = "top")
ggsave(paste0("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S13.tiff"),width=18,height=10,dpi=300)
#---------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------#
setwd("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data")
dat<-read_excel("남북_비교.xlsx",sheet="mics")

dat$category=factor(dat$category,levels=unique(dat$category))
dat$city    =factor(dat$city,levels=unique(dat$city))

dat1<-subset(dat,group==1)
dat2<-subset(dat,group==2)
dat3<-subset(dat,group==3)
dat4<-subset(dat,group==4)
dat5<-subset(dat,group==5)
dat6<-subset(dat,group==6)
dat7<-subset(dat,group==7)
dat8<-subset(dat,group==8)


x11();grid.arrange(ggplot(dat1,aes(category,value,fill=city))+
                     geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
                     labs(x="",y="Percentage (%)")+
                     theme(legend.title = element_blank())+geom_text(x="CD player",y=100,label="(a)",size=8),
                   ggplot(dat2,aes(category,value,fill=city))+
                     geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
                     labs(x="",y="Percentage (%)")+
                     theme(legend.title = element_blank())+geom_text(x="CD player",y=100,label="(b)",size=8))


x11();grid.arrange(ggplot(dat3,aes(category,value,fill=city))+
                     geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
                     labs(x="",y="Percentage (%)")+
                     theme(legend.title = element_blank())+geom_text(x="Farm animals/Live stocks",y=75,label="(c)",size=8),
                   ggplot(dat4,aes(category,value,fill=city))+
                     geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
                     labs(x="",y="Percentage (%)")+
                     theme(legend.title = element_blank())+geom_text(x="Farm animals/Live stocks",y=60,label="(d)",size=8))

x11();grid.arrange(ggplot(dat5,aes(category,value,fill=city))+
                     geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
                     labs(x="",y="Percentage (%)")+
                     theme(legend.title = element_blank())+geom_text(x="Mobile telephone",y=82,label="(e)",size=8),
                   ggplot(dat6,aes(category,value,fill=city))+
                     geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
                     labs(x="",y="Percentage (%)")+
                     theme(legend.title = element_blank())+geom_text(x="Mobile telephone",y=82,label="(f)",size=8))

tiff("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S16.tiff",
     units="in", width=20, height=10, res=300, compression = 'lzw')
grid.arrange(ggplot(dat5,aes(category,value,fill=city))+
               geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
               labs(x="",y="Percentage (%)")+
               theme(legend.title = element_blank())+geom_text(x="Mobile telephone",y=82,label="(a)",size=8),
             ggplot(dat6,aes(category,value,fill=city))+
               geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
               labs(x="",y="Percentage (%)")+
               theme(legend.title = element_blank())+geom_text(x="Mobile telephone",y=82,label="(b)",size=8))  
dev.off()

x11();grid.arrange(ggplot(dat7,aes(category,value,fill=city))+
                     geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
                     labs(x="",y="Percentage (%)")+
                     theme(legend.title = element_blank())+geom_text(x="high",y=55,label="(a)",size=8),
                   ggplot(dat8,aes(category,value,fill=city))+
                     geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
                     labs(x="",y="Percentage (%)")+
                     theme(legend.title = element_blank())+geom_text(x="high",y=75,label="(b)",size=8))

tiff("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S17.tiff",
     units="in", width=20, height=10, res=300, compression = 'lzw')
grid.arrange(ggplot(dat7,aes(category,value,fill=city))+
               geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
               labs(x="",y="Percentage (%)")+
               theme(legend.title = element_blank())+geom_text(x="high",y=55,label="(a)",size=8),
             ggplot(dat8,aes(category,value,fill=city))+
               geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
               labs(x="",y="Percentage (%)")+
               theme(legend.title = element_blank())+geom_text(x="high",y=75,label="(b)",size=8))
dev.off()
#---------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------#
setwd("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data")
dat<-read_excel("남북_비교.xlsx",sheet="mics_water")

dat$category=factor(dat$category,levels=unique(dat$category))
dat$city    =factor(dat$city,levels=unique(dat$city))

dat1<-subset(dat,group==1)
dat2<-subset(dat,group==2)
dat3<-subset(dat,group==3)
dat4<-subset(dat,group==4)


x11();grid.arrange(ggplot(dat1,aes(category,value,fill=city))+
                     geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
                     labs(x="",y="Percentage (%)")+
                     theme(legend.title = element_blank())+
                     geom_text(x="House hold population with TTC in source water",y=80,label="(a)",size=7),
                   ggplot(dat2,aes(category,value,fill=city))+
                     geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
                     labs(x="",y="Percentage (%)")+
                     theme(legend.title = element_blank())+
                     geom_text(x="House hold population with TTC in source water",y=80,label="(b)",size=7))

tiff("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S14.tiff",
     units="in", width=20, height=10, res=300, compression = 'lzw')
grid.arrange(ggplot(dat1,aes(category,value,fill=city))+
               geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
               labs(x="",y="Percentage (%)")+
               theme(legend.title = element_blank())+
               geom_text(x="House hold population with TTC in source water",y=80,label="(a)",size=7),
             ggplot(dat2,aes(category,value,fill=city))+
               geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
               labs(x="",y="Percentage (%)")+
               theme(legend.title = element_blank())+
               geom_text(x="House hold population with TTC in source water",y=80,label="(b)",size=7))    
dev.off()


x11();grid.arrange(ggplot(dat3,aes(category,value,fill=city))+
                     geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
                     labs(x="",y="Percentage (%)")+
                     theme(legend.title = element_blank())+
                     geom_text(x="House hold population with TTC in source water",y=70,label="(a)",size=8),
                   ggplot(dat4,aes(category,value,fill=city))+
                     geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
                     labs(x="",y="Percentage (%)")+
                     theme(legend.title = element_blank())+
                     geom_text(x="House hold population with TTC in source water",y=70,label="(b)",size=8))

tiff("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S15.tiff",
     units="in", width=20, height=10, res=300, compression = 'lzw')
grid.arrange(ggplot(dat3,aes(category,value,fill=city))+
               geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
               labs(x="",y="Percentage (%)")+
               theme(legend.title = element_blank())+
               geom_text(x="House hold population with TTC in source water",y=70,label="(a)",size=8),
             ggplot(dat4,aes(category,value,fill=city))+
               geom_bar(stat="identity",position="dodge")+theme_gray(base_size=20)+
               labs(x="",y="Percentage (%)")+
               theme(legend.title = element_blank())+
               geom_text(x="House hold population with TTC in source water",y=70,label="(b)",size=8))
dev.off()
#---------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------#
#Time-plot
dat<-read_excel("게이츠재단연구_2021_정리.xlsx",sheet="게이츠재단연구_2021_정리")
dat<-subset(dat,year<=2017)

a01<-dat %>% select(year,country,Infant_m)    ;a01$gubun="Infant Mortality"
a02<-dat %>% select(year,country,Neo_m)       ;a02$gubun="Neonatal Mortality"
a03<-dat %>% select(year,country,Und_m)       ;a03$gubun="Under???five Mortality"
a04<-dat %>% select(year,country,SBR_rate)    ;a04$gubun="Stillbirth"
a05<-dat %>% select(year,country,ALRI_rate)   ;a05$gubun="Acute lower respiratory infections"
a06<-dat %>% select(year,country,CA_rate)     ;a06$gubun="Congenitial anomalies"
a07<-dat %>% select(year,country,Pre_rate)    ;a07$gubun="Prematurity"
a08<-dat %>% select(year,country,BA_rate)     ;a08$gubun="Birth Asphyxia"
a09<-dat %>% select(year,country,DD_rate)     ;a09$gubun="Diarrhoeal Disease"
a10<-dat %>% select(year,country,Mea_rate)    ;a10$gubun="Meningitis/Encephalitis"
a11<-dat %>% select(year,country,Sepsis_rate) ;a11$gubun="Sepsis and other infections"
a12<-dat %>% select(year,country,Ane)         ;a12$gubun="Anemia"
a13<-dat %>% select(year,country,Over)        ;a13$gubun="Overweight"
a14<-dat %>% select(year,country,Obe)         ;a14$gubun="Obesity"
a15<-dat %>% select(year,country,Thin)        ;a15$gubun="Thinness"

names(a01)[3]="disease";names(a02)[3]="disease";names(a03)[3]="disease";names(a04)[3]="disease"
names(a05)[3]="disease";names(a06)[3]="disease";names(a07)[3]="disease";names(a08)[3]="disease"
names(a09)[3]="disease";names(a10)[3]="disease";names(a11)[3]="disease";names(a12)[3]="disease"
names(a13)[3]="disease";names(a14)[3]="disease";names(a15)[3]="disease"

a13$disease<-as.numeric(gsub("\\[.*?\\]", "", a13$disease))
a14$disease<-as.numeric(gsub("\\[.*?\\]", "", a14$disease))
a15$disease<-as.numeric(gsub("\\[.*?\\]", "", a15$disease))

a04$disease=as.numeric(a04$disease)

#trend test 이부분 에러 나는데 확인 필요 !!
trend.func<-function(data){
  s1<-subset(data,country=="North Korea")
  s2<-subset(data,country!="North Korea")
  
  s1<-s1[complete.cases(s1$disease),]
  s2<-s2[complete.cases(s2$disease),]
  
  ss1<-sens.slope(s1$disease);ss2<-sens.slope(s2$disease)
  
  mk1<-mk.test(s1$disease)    ;mk2<-mk.test(s2$disease)
  f1<-lm(disease~year,data=s1);f2<-lm(disease~year,data=s2)
  
  d1<-data.frame(sen=ss1$estimates,lci=ss1$conf.int[1],uci=ss1$conf.int[2],
                 mk =mk1$statistic,pval=mk1$p.value,
                 beta=summary(f1)$coeff[2,1],
                 lci=summary(f1)$coeff[2,1]-1.96*summary(f1)$coeff[2,2],
                 uci=summary(f1)$coeff[2,1]+1.96*summary(f1)$coeff[2,2])
  
  d2<-data.frame(sen=ss2$estimates,lci=ss2$conf.int[1],uci=ss2$conf.int[2],
                 mk =mk2$statistic,pval=mk2$p.value,
                 beta=summary(f2)$coeff[2,1],
                 lci=summary(f2)$coeff[2,1]-1.96*summary(f2)$coeff[2,2],
                 uci=summary(f2)$coeff[2,1]+1.96*summary(f2)$coeff[2,2])
  
  rbind(d1,d2)}

res<-rbind(trend.func(a01) %>% round(3),trend.func(a02) %>% round(3),
           trend.func(a03) %>% round(3),trend.func(a04) %>% round(3),
           trend.func(a05) %>% round(3),trend.func(a06) %>% round(3),
           trend.func(a07) %>% round(3),trend.func(a08) %>% round(3),
           trend.func(a09) %>% round(3),trend.func(a10) %>% round(3),
           trend.func(a11) %>% round(3),trend.func(a12) %>% round(3),
           trend.func(a13) %>% round(3),trend.func(a14) %>% round(3),trend.func(a15) %>% round(3))

# write.csv(res,file="res.csv")
#---------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------#
#Mortality : Infant/Neonatal/Under five / cause-specific mortality
aa<-rbind(a01,a02,a03,a04,a05,a06,a07,a08,a09,a10,a11)
aa$disease=as.numeric(aa$disease)
aa$gubun  =factor(aa$gubun,levels=unique(aa$gubun))
aa$country=factor(aa$country,levels=c("South Korea","North Korea"))
f1<-ggplot(aa,aes(year,disease,col=country))+labs(x="Year",y="Mortality rate (per 1,000 live births)",title="")+
  geom_point(size=4,aes(shape=country))+geom_line(size=1.2)+theme_minimal(base_size=20)+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text = element_text())+scale_color_manual(values=c("blue","red"))+facet_wrap(~gubun,scales="free")
x11();f1
ggsave(paste0("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure 1.tiff"),width=18,height=10,dpi=300)

#-------------------------------------------------------------------------------------------#
#Prevalence : Anemia/Overweight/Obesity/Thiness
aa<-rbind(a12,a13,a14,a15)
aa$disease=as.numeric(aa$disease)
aa$gubun  =factor(aa$gubun,levels=unique(aa$gubun))

aa$country=factor(aa$country,levels=c("South Korea","North Korea"))
f1<-ggplot(aa,aes(year,disease,col=country))+labs(x="Year",y="Prevalence rate (%)",title="")+
  geom_point(size=4,aes(shape=country))+geom_line(size=1.2)+theme_minimal(base_size=20)+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text = element_text())+scale_color_manual(values=c("blue","red"))+facet_wrap(~gubun,scales="free")
x11();f1
ggsave(paste0("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure 2.tiff"),width=18,height=10,dpi=300)
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#Correlation matrix plot
s1<-subset(dat,country=="North Korea");cs1<-s1 %>% select(Infant_m:Sepsis_rate,Ane:Thin,WB_PM25,CO2,N2O,Met,FF)
s2<-subset(dat,country!="North Korea");cs2<-s2 %>% select(Infant_m:Sepsis_rate,Ane:Thin,WB_PM25,CO2,N2O,Met,FF)

names(cs1) %>% length
n=c("Infant mortality (per 1,000)","Neonatal moratality (per 1,000)", "Under 5 years mortality (per 1,000)","Stillbirth (per 1,000)",
    "ALRI","Congenital Anomalies","Prematurity","Birth Asphyxia","Diarrhoeal Disease",
    "Meningitis_Encephalitis","Sepsis and other infections","Anemia","Overweights","Obesity","Thinness",
    "PM2.5","CO2","N2O","Methane","Fossil fuels")

names(cs1)=n;names(cs2)=n

cs1$Anemia     =as.numeric(gsub("\\[.*?\\]", "", cs1$Anemia))
cs1$Overweights=as.numeric(gsub("\\[.*?\\]", "", cs1$Overweights))
cs1$Obesity    =as.numeric(gsub("\\[.*?\\]", "", cs1$Obesity))
cs1$Thinness   =as.numeric(gsub("\\[.*?\\]", "", cs1$Thinness))

cs2$Anemia     =as.numeric(gsub("\\[.*?\\]", "", cs2$Anemia))
cs2$Overweights=as.numeric(gsub("\\[.*?\\]", "", cs2$Overweights))
cs2$Obesity    =as.numeric(gsub("\\[.*?\\]", "", cs2$Obesity))
cs2$Thinness   =as.numeric(gsub("\\[.*?\\]", "", cs2$Thinness))

cs1$`Stillbirth (per 1,000)`=as.numeric(cs1$`Stillbirth (per 1,000)`)
cs2$`Stillbirth (per 1,000)`=as.numeric(cs2$`Stillbirth (per 1,000)`)

cormat1<-cs1 %>%  cor(use="complete.obs",method="spearman") %>% round(1)
cormat2<-cs2 %>%  cor(use="complete.obs",method="spearman") %>% round(1)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)}

melted_cormat1 <- melt(get_lower_tri(cormat1), na.rm = TRUE)
melted_cormat2 <- melt(get_lower_tri(cormat2), na.rm = TRUE)

# Heatmap
p1<-ggplot(data = melted_cormat1, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+labs(x="",y="")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="(a) North Korea            \nSpearman's Correlation") +
  theme_minimal(base_size=18)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 15, hjust = 1))+
  coord_fixed()+geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),
        panel.grid.major = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(),axis.ticks = element_blank(),
        legend.justification = c(1, 0),legend.position = c(0.5, 0.7),legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,title.position = "top", title.hjust = 0.5))

p2<-ggplot(data = melted_cormat2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+labs(x="",y="")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="(b) South Korea            \nSpearman's Correlation") +
  theme_minimal(base_size=18)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 15, hjust = 1))+
  coord_fixed()+geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),
        panel.grid.major = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(),axis.ticks = element_blank(),
        legend.justification = c(1, 0),legend.position = c(0.5, 0.7),legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,title.position = "top", title.hjust = 0.5))

x11();grid.arrange(p2,p1,ncol=2)

tiff("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure 3.tiff",
     units="in", width=20, height=10, res=300, compression = 'lzw')
grid.arrange(p2,p1,ncol=2)
dev.off()

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#건강영향 Linear regression 

s1$Ane  =as.numeric(gsub("\\[.*?\\]", "",s1$Ane))
s1$Over =as.numeric(gsub("\\[.*?\\]", "",s1$Over))
s1$Obe  =as.numeric(gsub("\\[.*?\\]", "",s1$Obe))
s1$Thin =as.numeric(gsub("\\[.*?\\]", "",s1$Thin))

s2$Ane  =as.numeric(gsub("\\[.*?\\]", "",s2$Ane))
s2$Over =as.numeric(gsub("\\[.*?\\]", "",s2$Over))
s2$Obe  =as.numeric(gsub("\\[.*?\\]", "",s2$Obe))
s2$Thin =as.numeric(gsub("\\[.*?\\]", "",s2$Thin))

fit<-glm(Infant_m~WB_PM25,data=s1)

link="gaussian"
y1=s1$Ane

glm.func<-function(y1,y2,link){
  fit1<-glm(y1~WB_PM25,data=s1,family=link)
  fit2<-glm(y2~WB_PM25,data=s2,family=link)
  
  d1<-cbind(N=length(fit1$residuals),as.data.frame(t(summary(fit1)$coeff[2,])),link=link)
  d2<-cbind(N=length(fit2$residuals),as.data.frame(t(summary(fit2)$coeff[2,])),link=link)
  rbind(d1,d2)}

s1$SBR_rate=as.numeric(s1$SBR_rate)
s2$SBR_rate=as.numeric(s2$SBR_rate)


mod1.r01<-glm.func(s1$Ane        ,s2$Ane        ,"gaussian");mod1.r01$label="Anemia"
mod1.r02<-glm.func(s1$Over       ,s2$Over       ,"gaussian");mod1.r02$label="Overweight"
mod1.r03<-glm.func(s1$Obe        ,s2$Obe        ,"gaussian");mod1.r03$label="Obesity"
mod1.r04<-glm.func(s1$Thin       ,s2$Thin       ,"gaussian");mod1.r04$label="Thinness"
mod1.r05<-glm.func(s1$Infant_m   ,s2$Infant_m   ,"gaussian");mod1.r05$label="Infant Mortality"
mod1.r06<-glm.func(s1$Neo_m      ,s2$Neo_m      ,"gaussian");mod1.r06$label="Neonatal Mortality"
mod1.r07<-glm.func(s1$Und_m      ,s2$Und_m      ,"gaussian");mod1.r07$label="Under???five Mortality"
mod1.r08<-glm.func(s1$SBR_rate   ,s2$SBR_rate   ,"gaussian");mod1.r08$label="Stillbirth"
mod1.r09<-glm.func(s1$ALRI_rate  ,s2$ALRI_rate  ,"gaussian");mod1.r09$label="Acute lower respiratory infections"
mod1.r10<-glm.func(s1$CA_rate    ,s2$CA_rate    ,"gaussian");mod1.r10$label="Congenitial anomalies"
mod1.r11<-glm.func(s1$Pre_rate   ,s2$Pre_rate   ,"gaussian");mod1.r11$label="Prematurity"
mod1.r12<-glm.func(s1$BA_rate    ,s2$BA_rate    ,"gaussian");mod1.r12$label="Birth Asphyxia"
mod1.r13<-glm.func(s1$DD_rate    ,s2$DD_rate    ,"gaussian");mod1.r13$label="Diarrhoeal Disease"
mod1.r14<-glm.func(s1$Mea_rate   ,s2$Mea_rate   ,"gaussian");mod1.r14$label="Meningitis/Encephalitis"
mod1.r15<-glm.func(s1$Sepsis_rate,s2$Sepsis_rate,"gaussian");mod1.r15$label="Sepsis and other infections"

res1<-rbind(mod1.r01,mod1.r02,mod1.r03,mod1.r04,mod1.r05,
            mod1.r06,mod1.r07,mod1.r08,mod1.r09,mod1.r10,
            mod1.r11,mod1.r12,mod1.r13,mod1.r14,mod1.r15)

mod2.r01<-glm.func(s1$Ane        ,s2$Ane        ,"poisson");mod2.r01$label="Anemia"
mod2.r02<-glm.func(s1$Over       ,s2$Over       ,"poisson");mod2.r02$label="Overweight"
mod2.r03<-glm.func(s1$Obe        ,s2$Obe        ,"poisson");mod2.r03$label="Obesity"
mod2.r04<-glm.func(s1$Thin       ,s2$Thin       ,"poisson");mod2.r04$label="Thinness"
mod2.r05<-glm.func(s1$Infant_m   ,s2$Infant_m   ,"poisson");mod2.r05$label="Infant Mortality"
mod2.r06<-glm.func(s1$Neo_m      ,s2$Neo_m      ,"poisson");mod2.r06$label="Neonatal Mortality"
mod2.r07<-glm.func(s1$Und_m      ,s2$Und_m      ,"poisson");mod2.r07$label="Under???five Mortality"
mod2.r08<-glm.func(s1$SBR_rate   ,s2$SBR_rate   ,"poisson");mod2.r08$label="Stillbirth"
mod2.r09<-glm.func(s1$ALRI_rate  ,s2$ALRI_rate  ,"poisson");mod2.r09$label="Acute lower respiratory infections"
mod2.r10<-glm.func(s1$CA_rate    ,s2$CA_rate    ,"poisson");mod2.r10$label="Congenitial anomalies"
mod2.r11<-glm.func(s1$Pre_rate   ,s2$Pre_rate   ,"poisson");mod2.r11$label="Prematurity"
mod2.r12<-glm.func(s1$BA_rate    ,s2$BA_rate    ,"poisson");mod2.r12$label="Birth Asphyxia"
mod2.r13<-glm.func(s1$DD_rate    ,s2$DD_rate    ,"poisson");mod2.r13$label="Diarrhoeal Disease"
mod2.r14<-glm.func(s1$Mea_rate   ,s2$Mea_rate   ,"poisson");mod2.r14$label="Meningitis/Encephalitis"
mod2.r15<-glm.func(s1$Sepsis_rate,s2$Sepsis_rate,"poisson");mod2.r15$label="Sepsis and other infections"

res2<-rbind(mod2.r01,mod2.r02,mod2.r03,mod2.r04,mod2.r05,
            mod2.r06,mod2.r07,mod2.r08,mod2.r09,mod2.r10,
            mod2.r11,mod2.r12,mod2.r13,mod2.r14,mod2.r15)

# write.csv(res1,file="res1.csv",row.names=F,na="")
# write.csv(res2,file="res2.csv",row.names=F,na="")
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#disease pattern, 북한이랑 유사한 3국가만 - 사망률
dp<-read_excel("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data\\diseaes_pattern.xlsx",sheet=1)

dp.m<-dp %>% melt(id.vars=c("OBS","country","gubun","disease"))

dp.m$value=as.numeric(dp.m$value)
names(dp.m)[5]=c("year")
dp.m$year =as.numeric(as.character(dp.m$year))

dp.m$disease=factor(dp.m$disease,levels=unique(dp.m$disease))


x11();ggplot(dp.m,aes(year,value,col=country))+
  labs(x="Year",y="Mortality rate (per 1,000 live births)",title="")+
  geom_point(size=3,aes(shape=country))+
  geom_line(size=1.2)+theme_minimal(base_size=20)+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text = element_text())+scale_color_manual(values=c("orange","red","darkgreen","purple"))+
  facet_wrap(~disease,scales="free")
ggsave(paste0("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S7.tiff"),width=18,height=10,dpi=300)
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#disease pattern, 북한이랑 유사한 3국가만 - 유병률
dp<-read_excel("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data\\diseaes_pattern.xlsx",sheet=2)

dp.m<-dp %>% melt(id.vars=c("OBS","country","gubun","disease"))

dp.m$value=as.numeric(gsub("\\[.*?\\]", "",dp.m$value))
names(dp.m)[5]=c("year")
dp.m$year =as.numeric(as.character(dp.m$year))

dp.m$disease=factor(dp.m$disease,levels=unique(dp.m$disease))


x11();ggplot(dp.m,aes(year,value,col=country))+
  labs(x="Year",y="Prevalence (%)",title="")+
  geom_point(size=3,aes(shape=country))+
  geom_line(size=1.2)+theme_minimal(base_size=20)+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text = element_text())+scale_color_manual(values=c("orange","red","darkgreen","purple"))+
  facet_wrap(~disease,scales="free")
ggsave(paste0("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S8.tiff"),width=18,height=10,dpi=300)
#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#disease pattern, 북한이랑 유사한 3국가만 - 유병률
dp<-read_excel("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data\\diseaes_pattern.xlsx",sheet=3)

dp.m<-dp %>% melt(id.vars=c("OBS","country","gubun","disease"))

dp.m$value=as.numeric(gsub("\\[.*?\\]", "",dp.m$value))
names(dp.m)[5]=c("year")
dp.m$year =as.numeric(as.character(dp.m$year))

dp.m$disease=factor(dp.m$disease,levels=unique(dp.m$disease))

dp.m<-rbind(subset(dp.m,country!="South Korea"),
            subset(dp.m,country=="South Korea"))

dp.m$country=factor(dp.m$country,levels=unique(dp.m$country))
unique(dp.m$country) %>% length

x11();ggplot(dp.m,aes(year,value,col=country))+
  labs(x="Year",y="Prevalence (%)",title="")+
  geom_line(size=1)+theme_minimal(base_size=20)+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text = element_text())+scale_color_manual(values=c(rep("gray",34),"blue"))+
  facet_wrap(~disease,scales="free")

x11();ggplot(dp.m,aes(year,value,col=country))+
  labs(x="Year",y="Prevalence (%)",title="")+
  geom_line(size=1)+theme_minimal(base_size=20)+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text = element_text())+
  facet_wrap(~disease,scales="free")


s1<-subset(dp.m,country=="South Korea")
s2<-subset(dp.m,country!="South Korea")

x11();grid.arrange(ggplot(s1,aes(year,value,col=country))+
                     labs(x="Year",y="Prevalence (%)",title="")+
                     geom_line(size=1)+theme_minimal(base_size=20)+
                     theme(panel.border = element_blank(),
                           panel.grid.major = element_blank(),
                           legend.title = element_blank(),
                           legend.position = "top",
                           panel.grid.minor = element_blank(), 
                           axis.line = element_line(colour = "black"),
                           legend.text = element_text())+scale_color_manual(values=c("blue"))+
                     facet_wrap(~disease,scales="free"),
                   ggplot(s2,aes(year,value,col=country))+
                     labs(x="Year",y="Prevalence (%)",title="")+
                     geom_line(size=1)+theme_minimal(base_size=20)+
                     theme(panel.border = element_blank(),
                           panel.grid.major = element_blank(),
                           legend.title = element_blank(),
                           legend.position = "top",
                           panel.grid.minor = element_blank(), 
                           axis.line = element_line(colour = "black"),
                           legend.text = element_text())+
                     facet_wrap(~disease,scales="free"),ncol=2)

tiff("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S10.tiff",
     units="in", width=20, height=10, res=300, compression = 'lzw')
grid.arrange(ggplot(s1,aes(year,value,col=country))+
               labs(x="Year",y="Prevalence (%)",title="")+
               geom_line(size=1)+theme_minimal(base_size=20)+
               theme(panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     legend.title = element_blank(),
                     legend.position = "top",
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.text = element_text())+scale_color_manual(values=c("blue"))+
               facet_wrap(~disease,scales="free"),
             ggplot(s2,aes(year,value,col=country))+
               labs(x="Year",y="Prevalence (%)",title="")+
               geom_line(size=1)+theme_minimal(base_size=20)+
               theme(panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     legend.title = element_blank(),
                     legend.position = "top",
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.text = element_text())+
               facet_wrap(~disease,scales="free"),ncol=2)
dev.off()

#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#


#------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------#
#disease pattern, 북한이랑 유사한 3국가만 - 유병률
dp<-read_excel("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\data\\diseaes_pattern.xlsx",sheet=4)

dp.m<-dp %>% melt(id.vars=c("OBS","country","gubun","disease"))

dp.m$value=as.numeric(dp.m$value)
names(dp.m)[5]=c("year")
dp.m$year =as.numeric(as.character(dp.m$year))

dp.m$disease=factor(dp.m$disease,levels=unique(dp.m$disease))

dp.m<-rbind(subset(dp.m,country!="South Korea"),
            subset(dp.m,country=="South Korea"))

dp.m$disease=factor(dp.m$disease,levels=unique(dp.m$disease))
dp.m$country=factor(dp.m$country,levels=unique(dp.m$country))

x11();ggplot(dp.m,aes(year,value,col=country))+
  labs(x="Year",y="Prevalence (%)",title="")+
  geom_line(size=1)+theme_minimal(base_size=20)+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.text = element_text())+scale_color_manual(values=c(rep("gray",34),"blue"))+
  facet_wrap(~disease,scales="free")
ggsave(paste0("D:\\EUMC\\논문\\연구논문\\게이츠재단연구\\Figure\\Figure.S9.tiff"),width=18,height=10,dpi=300)