setwd("D:\\SNU\\연구\\건강나이")

dat1<-read_excel("NHIS_HA_20250320.xlsx",sheet="건강참조테이블_남성")
dat2<-read_excel("NHIS_HA_20250320.xlsx",sheet="건강참조테이블_여성")

dat3<-read_excel("NHIS_HA_20250320.xlsx",sheet="BA_남성테이블")
dat4<-read_excel("NHIS_HA_20250320.xlsx",sheet="BA_여성테이블")

dat1$variable=factor(dat1$variable,levels=unique(dat1$variable))


dat1$age=as.numeric(ifelse(dat1$age=="80+","80",dat1$age))
dat2$age=as.numeric(ifelse(dat2$age=="80+","80",dat2$age))

dd<-rbind(dat1 %>% mutate(sex="CA - Men"),dat2 %>% mutate(sex="CA - Women"),
          dat3 %>% mutate(sex="BA - Men"),dat4 %>% mutate(sex="BA - Women"))


# x11();ggplot(subset(dd,variable=="Body mass index"),
#              aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
#   coord_cartesian(ylim=c(15,25))+
#   scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Body mass index (kg/m2)",
#                                                  title="(a) Body mass index")+theme_gray(base_size=16)+
#   theme(legend.position = "top",legend.title=element_blank())+
#   scale_color_manual(values=c("#00a9ff","#ff68a1",
#                               "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
#                                                                                "#11F091","#F04911"))+
#   guides(color = guide_legend(ncol = 2)) 


g1<-ggplot(subset(dd,variable=="Body mass index"),
           aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  coord_cartesian(ylim=c(15,25))+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Body mass index (kg/m2)",
                                                 title="(a) Body mass index")+theme_gray(base_size=14)+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))


g2<-ggplot(subset(dd,variable=="SBP"),
           aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="SBP (mmHg)",
                                                 title="(b) SBP")+theme_gray(base_size=14)+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g3<-ggplot(subset(dd,variable=="DBP"),
           aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="DBP (mmHg)",
                                                 title="(c) DBP")+theme_gray(base_size=14)+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g4<-ggplot(subset(dd,variable=="Hemoglobin"),
           aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Hemoglobin (g/dL)",
                                                 title="(d) Hemoglobin")+theme_gray(base_size=14)+
  coord_cartesian(ylim=c(10,20))+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g5<-ggplot(subset(dd,variable=="Fasting serum glucose"),
           aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Fasting serum glucose (g/dL)",
                                                 title="(e) Fasting serum glucose")+theme_gray(base_size=14)+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g6<-ggplot(subset(dd,variable=="Total cholesterol"),
           aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Total cholesterol (mg/dL)",
                                                 title="(f) Total cholesterol")+theme_gray(base_size=14)+
  coord_cartesian(ylim=c(150,250))+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g7<-ggplot(subset(dd,variable=="GOT"),
           aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="GOT (U/L)",
                                                 title="(g) GOT")+theme_gray(base_size=14)+
  coord_cartesian(ylim=c(10,50))+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g8<-ggplot(subset(dd,variable=="GPT"),
           aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="GPT (U/L)",
                                                 title="(h) GPT")+theme_gray(base_size=14)+
  coord_cartesian(ylim=c(10,50))+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g9<-ggplot(subset(dd,variable=="GGT"),
           aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="GGT (U/L)",
                                                 title="(i) GGT")+theme_gray(base_size=14)+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(10,50))+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g10<-ggplot(subset(dd,variable=="Waist circumference"),
            aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Waist circumference (cm)",
                                                 title="(j) Waist circumference")+theme_gray(base_size=14)+
  coord_cartesian(ylim=c(50,90))+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g11<-ggplot(subset(dd,variable=="Triglyceride"),
            aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Triglyceride (mg/dL)",
                                                 title="(k) Triglyceride")+theme_gray(base_size=14)+
  coord_cartesian(ylim=c(50,200))+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g12<-ggplot(subset(dd,variable=="HDL cholesterol"),
            aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="HDL cholesterol (mg/dL)",
                                                 title="(l) HDL cholesterol")+theme_gray(base_size=14)+
  coord_cartesian(ylim=c(30,100))+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g13<-ggplot(subset(dd,variable=="LDL cholesterol"),
            aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="LDL cholesterol (mg/dL)",
                                                 title="(m) LDL cholesterol")+theme_gray(base_size=14)+
  coord_cartesian(ylim=c(55,160))+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g14<-ggplot(subset(dd,variable=="Creatinine"),
            aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Creatinine (mg/dL)",
                                                 title="(n) Creatinine")+theme_gray(base_size=14)+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0.5,2))+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

g15<-ggplot(subset(dd,variable=="eGFR"),
            aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam")+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="eGFR (mL/min/1.73 m2)",
                                                 title="(o) eGFR")+theme_gray(base_size=14)+
  guides(color = guide_legend(ncol = 2))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))


x11();grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,ncol=5)


medi1<-ggplot(subset(dd,variable=="Total healthcare expenditure"),
              aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam",se=F)+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Total healthcare expenditure",
                                                 title="(a) Total healthcare expenditure")+theme_gray(base_size=20)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

medi2<-ggplot(subset(dd,variable=="Out-of-pocket expenditure"),
              aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam",se=F)+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Out-of-pocket expenditure",
                                                 title="(b) Out-of-pocket expenditure")+theme_gray(base_size=20)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

medi3<-ggplot(subset(dd,variable=="Insurance-covered expenditure"),
              aes(age,Mean,col=sex,fill=sex,group=sex))+stat_smooth(method="gam",se=F)+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Insurance-covered expenditure",
                                                 title="(c) Insurance-covered expenditure")+theme_gray(base_size=20)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1",
                              "#11F091","#F04911"))+scale_fill_manual(values=c("#00a9ff","#ff68a1",
                                                                               "#11F091","#F04911"))

x11();grid.arrange(medi1,medi2,medi3,ncol=3)

medi<-subset(dd,variable %in% c("Total healthcare expenditure",
                                "Out-of-pocket expenditure",
                                "Insurance-covered expenditure"))
head(medi)
x11();ggplot(medi,aes(age,P50,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Healthcare expenditure (KRW)")+theme_gray(base_size=30)+
  theme(legend.position = "top",legend.title=element_blank())+facet_wrap(~sex)+
  scale_y_continuous(labels = scales::comma)


#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#Men
z01<-subset(dat1, variable%in% ("Body mass index")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z02<-subset(dat1, variable%in% ("SBP")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z03<-subset(dat1, variable%in% ("DBP")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z04<-subset(dat1, variable%in% ("Hemoglobin")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z05<-subset(dat1, variable%in% ("Fasting serum glucose")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z06<-subset(dat1, variable%in% ("Total cholesterol")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z07<-subset(dat1, variable%in% ("GOT")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z08<-subset(dat1, variable%in% ("GPT")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z09<-subset(dat1, variable%in% ("GGT")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z10<-subset(dat1, variable%in% ("Waist circumference")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z11<-subset(dat1, variable%in% ("Triglyceride")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z12<-subset(dat1, variable%in% ("HDL cholesterol")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z13<-subset(dat1, variable%in% ("LDL cholesterol")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z14<-subset(dat1, variable%in% ("Creatinine")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z15<-subset(dat1, variable%in% ("eGFR")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)

zz01<-reshape2::melt(z01,id="age")
zz02<-reshape2::melt(z02,id="age")
zz03<-reshape2::melt(z03,id="age")
zz04<-reshape2::melt(z04,id="age")
zz05<-reshape2::melt(z05,id="age")
zz06<-reshape2::melt(z06,id="age")
zz07<-reshape2::melt(z07,id="age")
zz08<-reshape2::melt(z08,id="age")
zz09<-reshape2::melt(z09,id="age")
zz10<-reshape2::melt(z10,id="age")
zz11<-reshape2::melt(z11,id="age")
zz12<-reshape2::melt(z12,id="age")
zz13<-reshape2::melt(z13,id="age")
zz14<-reshape2::melt(z14,id="age")
zz15<-reshape2::melt(z15,id="age")

m01<-ggplot(zz01,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Body mass index (kg/m2)",
                                                 title="(a) Body mass index")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m02<-ggplot(zz02,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="SBP (mmHg)",
                                                 title="(b) SBP")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


m03<-ggplot(zz03,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="DBP (mmHg)",
                                                 title="(c) DBP")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


m04<-ggplot(zz04,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Hemoglobin (g/dL)",
                                                 title="(d) Hemoglobin")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m05<-ggplot(zz05,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Fasting serum glucose (g/dL)",
                                                 title="(e) Fasting serum glucose")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


m06<-ggplot(zz06,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Total cholesterol (mg/dL)",
                                                 title="(f) Total cholesterol")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


m07<-ggplot(zz07,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="GOT (U/L)",
                                                 title="(g) GOT")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m08<-ggplot(zz08,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="GPT (U/L)",
                                                 title="(h) GPT")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


m09<-ggplot(zz09,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="GGT (U/L)",
                                                 title="(i) GGT")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m10<-ggplot(zz10,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Waist circumference (cm)",
                                                 title="(j) Waist circumference")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


m11<-ggplot(zz11,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Triglyceride (mg/dL)",
                                                 title="(k) Triglyceride")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m12<-ggplot(zz12,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="HDL cholesterol (mg/dL)",
                                                 title="(l) HDL cholesterol")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m13<-ggplot(zz13,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="LDL cholesterol (mg/dL)",
                                                 title="(m) LDL cholesterol")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m14<-ggplot(zz14,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Creatinine (mg/dL)",
                                                 title="(n) Creatinine")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m15<-ggplot(zz15,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="eGFR (mL/min/1.73 m2)",
                                                 title="(a) eGFR")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


x11();grid.arrange(m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12,m13,m14,m15,ncol=5)


#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#Women
z01<-subset(dat2, variable%in% ("Body mass index")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z02<-subset(dat2, variable%in% ("SBP")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z03<-subset(dat2, variable%in% ("DBP")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z04<-subset(dat2, variable%in% ("Hemoglobin")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z05<-subset(dat2, variable%in% ("Fasting serum glucose")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z06<-subset(dat2, variable%in% ("Total cholesterol")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z07<-subset(dat2, variable%in% ("GOT")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z08<-subset(dat2, variable%in% ("GPT")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z09<-subset(dat2, variable%in% ("GGT")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z10<-subset(dat2, variable%in% ("Waist circumference")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z11<-subset(dat2, variable%in% ("Triglyceride")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z12<-subset(dat2, variable%in% ("HDL cholesterol")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z13<-subset(dat2, variable%in% ("LDL cholesterol")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z14<-subset(dat2, variable%in% ("Creatinine")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z15<-subset(dat2, variable%in% ("eGFR")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)

zz01<-reshape2::melt(z01,id="age")
zz02<-reshape2::melt(z02,id="age")
zz03<-reshape2::melt(z03,id="age")
zz04<-reshape2::melt(z04,id="age")
zz05<-reshape2::melt(z05,id="age")
zz06<-reshape2::melt(z06,id="age")
zz07<-reshape2::melt(z07,id="age")
zz08<-reshape2::melt(z08,id="age")
zz09<-reshape2::melt(z09,id="age")
zz10<-reshape2::melt(z10,id="age")
zz11<-reshape2::melt(z11,id="age")
zz12<-reshape2::melt(z12,id="age")
zz13<-reshape2::melt(z13,id="age")
zz14<-reshape2::melt(z14,id="age")
zz15<-reshape2::melt(z15,id="age")

m01<-ggplot(zz01,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Body mass index (kg/m2)",
                                                 title="(a) Body mass index")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m02<-ggplot(zz02,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="SBP (mmHg)",
                                                 title="(b) SBP")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


m03<-ggplot(zz03,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="DBP (mmHg)",
                                                 title="(c) DBP")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


m04<-ggplot(zz04,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Hemoglobin (g/dL)",
                                                 title="(d) Hemoglobin")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m05<-ggplot(zz05,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Fasting serum glucose (g/dL)",
                                                 title="(e) Fasting serum glucose")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


m06<-ggplot(zz06,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Total cholesterol (mg/dL)",
                                                 title="(f) Total cholesterol")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


m07<-ggplot(zz07,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="GOT (U/L)",
                                                 title="(g) GOT")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m08<-ggplot(zz08,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="GPT (U/L)",
                                                 title="(h) GPT")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


m09<-ggplot(zz09,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="GGT (U/L)",
                                                 title="(i) GGT")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m10<-ggplot(zz10,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Waist circumference (cm)",
                                                 title="(j) Waist circumference")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())


m11<-ggplot(zz11,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Triglyceride (mg/dL)",
                                                 title="(k) Triglyceride")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m12<-ggplot(zz12,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="HDL cholesterol (mg/dL)",
                                                 title="(l) HDL cholesterol")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m13<-ggplot(zz13,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="LDL cholesterol (mg/dL)",
                                                 title="(m) LDL cholesterol")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m14<-ggplot(zz14,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Creatinine (mg/dL)",
                                                 title="(n) Creatinine")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

m15<-ggplot(zz15,aes(age,value,col=variable,group=variable))+geom_point()+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="eGFR (mL/min/1.73 m2)",
                                                 title="(a) eGFR")+theme_gray(base_size=13)+
  theme(legend.position = "top",legend.title=element_blank())

x11();grid.arrange(m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12,m13,m14,m15,ncol=5)


#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#Men
a01<-subset(dat1, variable%in% ("Total healthcare expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
a02<-subset(dat1, variable%in% ("Out-of-pocket expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
a03<-subset(dat1, variable%in% ("Insurance-covered expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)

aa01<-reshape2::melt(a01,id="age")
aa02<-reshape2::melt(a02,id="age")
aa03<-reshape2::melt(a03,id="age")

b01<-subset(dat3, variable%in% ("Total healthcare expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
b02<-subset(dat3, variable%in% ("Out-of-pocket expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
b03<-subset(dat3, variable%in% ("Insurance-covered expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)

bb01<-reshape2::melt(b01,id="age")
bb02<-reshape2::melt(b02,id="age")
bb03<-reshape2::melt(b03,id="age")

m01<-ggplot(aa01,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Total healthcare expenditure (KRW)",
                                                 title="(d)CA - Men, Total healthcare expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))

m02<-ggplot(aa02,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Out-of-pocket expenditure (KRW)",
                                                 title="(e)CA - Men, Out-of-pocket expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))

m03<-ggplot(aa03,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Insurance-covered expenditure (KRW)",
                                                 title="(f)CA - Men, Insurance-covered expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))

m04<-ggplot(bb01,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Total healthcare expenditure (KRW)",
                                                 title="(a)BA - Men, Total healthcare expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))

m05<-ggplot(bb02,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Out-of-pocket expenditure (KRW)",
                                                 title="(b)BA - Men, Out-of-pocket expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))

m06<-ggplot(bb03,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Insurance-covered expenditure (KRW)",
                                                 title="(C)BA - Men, Insurance-covered expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))


x11();grid.arrange(grid.arrange(m04,m05,m06,ncol=3),
                   grid.arrange(m01,m02,m03,ncol=3))

#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#Women
a01<-subset(dat2, variable%in% ("Total healthcare expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
a02<-subset(dat2, variable%in% ("Out-of-pocket expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
a03<-subset(dat2, variable%in% ("Insurance-covered expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)

aa01<-reshape2::melt(a01,id="age")
aa02<-reshape2::melt(a02,id="age")
aa03<-reshape2::melt(a03,id="age")

b01<-subset(dat4, variable%in% ("Total healthcare expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
b02<-subset(dat4, variable%in% ("Out-of-pocket expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
b03<-subset(dat4, variable%in% ("Insurance-covered expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)

bb01<-reshape2::melt(b01,id="age")
bb02<-reshape2::melt(b02,id="age")
bb03<-reshape2::melt(b03,id="age")

m01<-ggplot(aa01,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Total healthcare expenditure (KRW)",
                                                 title="(d)CA - Women, Total healthcare expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))

m02<-ggplot(aa02,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Out-of-pocket expenditure (KRW)",
                                                 title="(e)CA - Women, Out-of-pocket expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))

m03<-ggplot(aa03,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Insurance-covered expenditure (KRW)",
                                                 title="(f)CA - Women, Insurance-covered expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))

m04<-ggplot(bb01,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Total healthcare expenditure (KRW)",
                                                 title="(a)BA - Women, Total healthcare expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))

m05<-ggplot(bb02,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Out-of-pocket expenditure (KRW)",
                                                 title="(b)BA - Women, Out-of-pocket expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))

m06<-ggplot(bb03,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam")+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="Insurance-covered expenditure (KRW)",
                                                 title="(C)BA - Women, Insurance-covered expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))


x11();grid.arrange(grid.arrange(m04,m05,m06,ncol=3),
                   grid.arrange(m01,m02,m03,ncol=3))

#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#Ranking
rank1<-read_excel("NHIS_HA_20250320.xlsx",sheet="남성_건강등수_지표별평균")
rank2<-read_excel("NHIS_HA_20250320.xlsx",sheet="여성_건강등수_지표별평균")

head(rank1)
head(rank2)
ranking<-rbind(rank1,rank2)

# x11();ggplot(ranking,aes(rank,V1,col=sex,fill=sex,group=sex))+facet_wrap(~age)+
#   scale_x_continuous(breaks=seq(1,100,10))+stat_smooth()

ranking$age=ifelse(ranking$age=="80","80+",ranking$age)

x11();ggplot(ranking,aes(rank,V1,col=age,fill=age,group=age))+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam",se=F)+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="Body mass index (kg/m2)",title="(a) Body mass index")+
  theme(legend.title = element_blank(),legend.position = c(0.25, 0.95))+
  guides(color = guide_legend(nrow = 3))

x11();ggplot(ranking,aes(rank,V2,col=age,fill=age,group=age))+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam",se=F)+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="SBP (mmHg)",title="(b) SBP")+
  theme(legend.title = element_blank(),legend.position = c(0.25, 0.95))+
  guides(color = guide_legend(nrow = 3))

x11();ggplot(ranking,aes(rank,V1,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="Body mass index (kg/m2)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V2,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="SBP (mmHg)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V3,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="DBP (mmHg)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V4,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="Hemoglobin (g/dL)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V5,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="Fasting serum glucose (g/dL)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V6,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="Total cholesterol (mg/dL)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V7,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="GOT (U/L)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V8,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="GPT (U/L)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V9,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="GGT (U/L)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V10,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="Waist circumference (cm)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V11,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="Triglyceride (mg/dL)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V12,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="HDL cholesterol (mg/dL)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V13,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="LDL cholesterol (mg/dL)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V14,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="Creatinine (mg/dL)")+
  coord_cartesian(ylim=c(0.5,2))+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))

x11();ggplot(ranking,aes(rank,V15,col=sex,group=sex))+facet_wrap(~age)+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam")+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="eGFR (mL/min/1.73 m2)")+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("#00a9ff","#ff68a1"))+scale_fill_manual(values=c("#00a9ff","#ff68a1"))
