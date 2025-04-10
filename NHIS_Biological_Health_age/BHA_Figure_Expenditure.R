pacman::p_load("dplyr",readxl,ggplot2,lubridate,gridExtra,scales)
setwd("D:\\SNU\\연구\\건강나이")

dat1<-read_excel("NHIS_HA_20250409.xlsx",sheet="건강참조테이블_남성")
dat2<-read_excel("NHIS_HA_20250409.xlsx",sheet="건강참조테이블_여성")

dat3<-read_excel("NHIS_HA_20250409.xlsx",sheet="BA_남성테이블")
dat4<-read_excel("NHIS_HA_20250409.xlsx",sheet="BA_여성테이블")

dat1$variable=factor(dat1$variable,levels=unique(dat1$variable))

dat1$age=as.numeric(ifelse(dat1$age=="80+","80",dat1$age))
dat2$age=as.numeric(ifelse(dat2$age=="80+","80",dat2$age))

dd<-rbind(dat1 %>% mutate(sex="CA - Men",gender="Men"),dat2 %>% mutate(sex="CA - Women",gender="Women"),
          dat3 %>% mutate(sex="BA - Men",gender="Men"),dat4 %>% mutate(sex="BA - Women",gender="Women"))


g1<-ggplot(subset(dd,variable=="Body mass index"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  coord_cartesian(ylim=c(15,25),xlim=c(40,80))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="Body mass index (kg/m2)",
                                                 title="(A) Body mass index")+theme_gray(base_size=15)+
  guides(color = guide_legend(ncol = 4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))
  
g2<-ggplot(subset(dd,variable=="Waist circumference"),
            aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="Waist circumference (cm)",
                                                 title="(B) Waist circumference")+theme_gray(base_size=15)+
  coord_cartesian(ylim=c(50,90),xlim=c(40,80))+
  guides(color = guide_legend(ncol = 4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g3<-ggplot(subset(dd,variable=="SBP"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  coord_cartesian(xlim=c(40,80))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="SBP (mmHg)",
                                                 title="(C) SBP")+theme_gray(base_size=15)+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g4<-ggplot(subset(dd,variable=="DBP"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="DBP (mmHg)",
                                                 title="(D) DBP")+theme_gray(base_size=15)+
  coord_cartesian(xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g5<-ggplot(subset(dd,variable=="Fasting serum glucose"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  coord_cartesian(xlim=c(40,80))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="Fasting serum glucose (g/dL)",
                                                 title="(E) Fasting serum glucose")+theme_gray(base_size=15)+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))


g6<-ggplot(subset(dd,variable=="Hemoglobin"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="Hemoglobin (g/dL)",
                                                 title="(F) Hemoglobin")+theme_gray(base_size=15)+
  coord_cartesian(ylim=c(10,20),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g7<-ggplot(subset(dd,variable=="GOT"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="GOT (U/L)",
                                                 title="(G) GOT")+theme_gray(base_size=15)+
  coord_cartesian(ylim=c(10,50),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g8<-ggplot(subset(dd,variable=="GPT"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="GPT (U/L)",
                                                 title="(H) GPT")+theme_gray(base_size=15)+
  coord_cartesian(ylim=c(10,50),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g9<-ggplot(subset(dd,variable=="GGT"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="GGT (U/L)",
                                                 title="(I) GGT")+theme_gray(base_size=15)+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(10,50),xlim=c(40,80))+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g10<-ggplot(subset(dd,variable=="Total cholesterol"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="Total cholesterol (mg/dL)",
                                                 title="(J) Total cholesterol")+theme_gray(base_size=15)+
  coord_cartesian(ylim=c(150,250),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g11<-ggplot(subset(dd,variable=="HDL cholesterol"),
            aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="HDL cholesterol (mg/dL)",
                                                 title="(K) HDL cholesterol")+theme_gray(base_size=15)+
  coord_cartesian(ylim=c(30,100),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g12<-ggplot(subset(dd,variable=="LDL cholesterol"),
            aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="LDL cholesterol (mg/dL)",
                                                 title="(L) LDL cholesterol")+theme_gray(base_size=15)+
  coord_cartesian(ylim=c(55,150),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g13<-ggplot(subset(dd,variable=="Triglyceride"),
            aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="Triglyceride (mg/dL)",
                                                 title="(M) Triglyceride")+theme_gray(base_size=15)+
  coord_cartesian(ylim=c(50,200),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g14<-ggplot(subset(dd,variable=="Creatinine"),
            aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="Creatinine (mg/dL)",
                                                 title="(N) Creatinine")+theme_gray(base_size=15)+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0.5,2),xlim=c(40,80))+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))


g15<-ggplot(subset(dd,variable=="eGFR"),
            aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F)+
  coord_cartesian(xlim=c(40,80))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="eGFR (mL/min/1.73 m2)",
                                                 title="(O) eGFR")+theme_gray(base_size=15)+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

x11();g15
x11();grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,ncol=5)


# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, g15)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(g1)

# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}

# 📌 3️⃣ 그래프 배치 (범례는 따로 추가)
x11() # 창 띄우기
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 5)), # 범례 없는 그래프 배치
  ncol = 1, heights = c(1, 10)  # 그래프 10배, 범례 1배 크기로 조절
)

# 📌 3️⃣ 그래프 배치 (범례는 따로 추가)
x11() # 창 띄우기
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 2)), # 범례 없는 그래프 배치
  ncol = 1, heights = c(1, 10)  # 그래프 10배, 범례 1배 크기로 조절
)

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\plot_rev.tiff",
     width=14,height=24,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 2)), # 범례 없는 그래프 배치
  ncol = 1, heights = c(0.5, 10)  # 그래프 10배, 범례 1배 크기로 조절
)
dev.off()




#------------------------------------------------------------------------------#
dd<-rbind(dat1 %>% mutate(sex="Men",gender="Men"),
          dat2 %>% mutate(sex="Women",gender="Women"))


g1<-ggplot(subset(dd,variable=="Body mass index"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  coord_cartesian(ylim=c(15,25),xlim=c(40,80))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Body mass index (kg/m2)",
                                                 title="(A) Body mass index")+theme_gray(base_size=18)+
  guides(color = guide_legend(ncol = 4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g2<-ggplot(subset(dd,variable=="Waist circumference"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Waist circumference (cm)",
                                                 title="(B) Waist circumference")+theme_gray(base_size=18)+
  coord_cartesian(ylim=c(50,90),xlim=c(40,80))+
  guides(color = guide_legend(ncol = 4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g3<-ggplot(subset(dd,variable=="SBP"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  coord_cartesian(xlim=c(40,80))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="SBP (mmHg)",
                                                 title="(C) SBP")+theme_gray(base_size=18)+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g4<-ggplot(subset(dd,variable=="DBP"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="DBP (mmHg)",
                                                 title="(D) DBP")+theme_gray(base_size=18)+
  coord_cartesian(xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g5<-ggplot(subset(dd,variable=="Fasting serum glucose"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  coord_cartesian(xlim=c(40,80))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Fasting serum glucose (g/dL)",
                                                 title="(E) Fasting serum glucose")+theme_gray(base_size=18)+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))


g6<-ggplot(subset(dd,variable=="Hemoglobin"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Hemoglobin (g/dL)",
                                                 title="(F) Hemoglobin")+theme_gray(base_size=18)+
  coord_cartesian(ylim=c(10,20),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g7<-ggplot(subset(dd,variable=="GOT"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="GOT (U/L)",
                                                 title="(G) GOT")+theme_gray(base_size=18)+
  coord_cartesian(ylim=c(10,50),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g8<-ggplot(subset(dd,variable=="GPT"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="GPT (U/L)",
                                                 title="(H) GPT")+theme_gray(base_size=18)+
  coord_cartesian(ylim=c(10,50),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g9<-ggplot(subset(dd,variable=="GGT"),
           aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="GGT (U/L)",
                                                 title="(I) GGT")+theme_gray(base_size=18)+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(10,50),xlim=c(40,80))+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g10<-ggplot(subset(dd,variable=="Total cholesterol"),
            aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Total cholesterol (mg/dL)",
                                                 title="(J) Total cholesterol")+theme_gray(base_size=18)+
  coord_cartesian(ylim=c(150,250),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g11<-ggplot(subset(dd,variable=="HDL cholesterol"),
            aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="HDL cholesterol (mg/dL)",
                                                 title="(K) HDL cholesterol")+theme_gray(base_size=18)+
  coord_cartesian(ylim=c(30,100),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g12<-ggplot(subset(dd,variable=="LDL cholesterol"),
            aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="LDL cholesterol (mg/dL)",
                                                 title="(L) LDL cholesterol")+theme_gray(base_size=18)+
  coord_cartesian(ylim=c(55,150),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g13<-ggplot(subset(dd,variable=="Triglyceride"),
            aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Triglyceride (mg/dL)",
                                                 title="(M) Triglyceride")+theme_gray(base_size=18)+
  coord_cartesian(ylim=c(50,200),xlim=c(40,80))+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

g14<-ggplot(subset(dd,variable=="Creatinine"),
            aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Creatinine (mg/dL)",
                                                 title="(N) Creatinine")+theme_gray(base_size=18)+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0.5,2),xlim=c(40,80))+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))


g15<-ggplot(subset(dd,variable=="eGFR"),
            aes(age,Mean,col=sex,fill=sex,group=sex,linetype = sex))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  coord_cartesian(xlim=c(40,80))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="eGFR (mL/min/1.73 m2)",
                                                 title="(O) eGFR")+theme_gray(base_size=18)+
  guides(color = guide_legend(ncol =4))+theme(legend.position = "top",legend.title=element_blank())+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

x11();grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,ncol=3)


# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, g15)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(g1)

# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}
tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\plot_rev2.tiff",
     width=14,height=24,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), # 범례 없는 그래프 배치
  ncol = 1, heights = c(0.5, 10)  # 그래프 10배, 범례 1배 크기로 조절
)
dev.off()




#------------------------------------------------------------------------------#

medi1<-ggplot(subset(dd,variable=="Total healthcare expenditure" & age %in% c(40:80) ),
              aes(age,P50,col=sex,fill=sex,group=sex,linetype = sex))+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  coord_cartesian(xlim=c(40,80),ylim=c(100000,1500000))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="Total healthcare expenditure (KRW)",
                                                 title="(A1) Total healthcare expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  guides(color = guide_legend(ncol = 4))+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_y_continuous(labels = scales::comma)+facet_wrap(~gender)+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

medi2<-ggplot(subset(dd,variable=="Out-of-pocket expenditure" & age %in% c(40:80) ),
              aes(age,P50,col=sex,fill=sex,group=sex,linetype = sex))+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  coord_cartesian(xlim=c(40,80),ylim=c(100000,1500000))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="Out-of-pocket expenditure (KRW)",
                                                 title="(A2) Out-of-pocket expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  guides(color = guide_legend(ncol = 4))+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_y_continuous(labels = scales::comma)+facet_wrap(~gender)+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))

medi3<-ggplot(subset(dd,variable=="Insurance-covered expenditure" & age %in% c(40:80) ),
              aes(age,P50,col=sex,fill=sex,group=sex,linetype = sex))+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  coord_cartesian(xlim=c(40,80),ylim=c(100000,1500000))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="Insurance-covered expenditure (KRW)",
                                                 title="(A3) Insurance-covered expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  guides(color = guide_legend(ncol = 4))+
  scale_color_manual(values=c("blue","red","#800080","black"))+
  scale_fill_manual(values=c("blue","red","#800080","black"))+
  scale_y_continuous(labels = scales::comma)+facet_wrap(~gender)+
  scale_linetype_manual(values=c("solid","solid","dashed","dashed"))


# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(medi1,medi2,medi3)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(medi1)

# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}

# 📌 3️⃣ 그래프 배치 (범례는 따로 추가)
x11() # 창 띄우기
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), # 범례 없는 그래프 배치
  ncol = 1, heights = c(1, 10)  # 그래프 10배, 범례 1배 크기로 조절
)

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\plot2.tiff",
     width=20,height=12,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), # 범례 없는 그래프 배치
  ncol = 1, heights = c(1, 10)  # 그래프 10배, 범례 1배 크기로 조절
)
dev.off()


x11();grid.arrange(medi1,medi2,medi3,ncol=3)

medi<-subset(dd,variable %in% c("Total healthcare expenditure",
                                "Out-of-pocket expenditure",
                                "Insurance-covered expenditure"))
head(medi)
x11();ggplot(subset(medi,age %in% c(40:80)),aes(age,P50,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F,size=1.1)+
  coord_cartesian(xlim=c(40,80))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Age",y="Healthcare expenditure (KRW)")+theme_gray(base_size=30)+
  theme(legend.position = "top",legend.title=element_blank())+facet_wrap(~sex)+
  scale_y_continuous(labels = scales::comma)


#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#Chronological age
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

m01<-ggplot(zz01,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Body mass index (kg/m2)",
                                                 title="(A) Body mass index")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+  guides(color = guide_legend(ncol = 9))+
  coord_cartesian(xlim=c(40,80))

m02<-ggplot(zz10,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Waist circumference (cm)",
                                                 title="(B) Waist circumference")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


m03<-ggplot(zz02,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="SBP (mmHg)",
                                                 title="(C) SBP")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


m04<-ggplot(zz03,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="DBP (mmHg)",
                                                 title="(D) DBP")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m05<-ggplot(zz05,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Fasting serum glucose (g/dL)",
                                                 title="(E) Fasting serum glucose")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m06<-ggplot(zz04,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Hemoglobin (g/dL)",
                                                 title="(F) Hemoglobin")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m07<-ggplot(zz07,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="GOT (U/L)",
                                                 title="(G) GOT")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m08<-ggplot(zz08,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="GPT (U/L)",
                                                 title="(H) GPT")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m09<-ggplot(zz09,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="GGT (U/L)",
                                                 title="(I) GGT")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m10<-ggplot(zz06,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Total cholesterol (mg/dL)",
                                                 title="(J) Total cholesterol")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


m11<-ggplot(zz12,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="HDL cholesterol (mg/dL)",
                                                 title="(K) HDL cholesterol")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m12<-ggplot(zz15,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="LDL cholesterol (mg/dL)",
                                                 title="(L) LDL cholesterol")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m13<-ggplot(zz11,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Triglyceride (mg/dL)",
                                                 title="(M) Triglyceride")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m14<-ggplot(zz14,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="loess",se=F,span=0.9)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Creatinine (mg/dL)",
                                                 title="(N) Creatinine")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m15<-ggplot(zz15,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="eGFR (mL/min/1.73 m2)",
                                                 title="(O) eGFR")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))



# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12,m13,m14,m15)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(m01)

# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}

# 📌 3️⃣ 그래프 배치 (범례는 따로 추가)
x11() # 창 띄우기
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), # 범례 없는 그래프 배치
  ncol = 1, heights = c(1, 10)  # 그래프 10배, 범례 1배 크기로 조절
)

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\suppl1_ca.tiff",
     width=15,height=20,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), # 범례 없는 그래프 배치
  ncol = 1, heights = c(1, 10)  # 그래프 10배, 범례 1배 크기로 조절
)
dev.off()

x11();grid.arrange(m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12,m13,m14,m15,ncol=5)


#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#Chronological age
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


m01<-ggplot(zz01,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Body mass index (kg/m2)",
                                                 title="(A) Body mass index")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+  guides(color = guide_legend(ncol = 9))+
  coord_cartesian(xlim=c(40,80))

m02<-ggplot(zz10,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Waist circumference (cm)",
                                                 title="(B) Waist circumference")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


m03<-ggplot(zz02,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="SBP (mmHg)",
                                                 title="(C) SBP")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


m04<-ggplot(zz03,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="DBP (mmHg)",
                                                 title="(D) DBP")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m05<-ggplot(zz05,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Fasting serum glucose (g/dL)",
                                                 title="(E) Fasting serum glucose")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m06<-ggplot(zz04,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Hemoglobin (g/dL)",
                                                 title="(F) Hemoglobin")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m07<-ggplot(zz07,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="GOT (U/L)",
                                                 title="(G) GOT")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m08<-ggplot(zz08,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="GPT (U/L)",
                                                 title="(H) GPT")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m09<-ggplot(zz09,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="GGT (U/L)",
                                                 title="(I) GGT")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m10<-ggplot(zz06,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Total cholesterol (mg/dL)",
                                                 title="(J) Total cholesterol")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


m11<-ggplot(zz12,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="HDL cholesterol (mg/dL)",
                                                 title="(K) HDL cholesterol")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m12<-ggplot(zz15,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="LDL cholesterol (mg/dL)",
                                                 title="(L) LDL cholesterol")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m13<-ggplot(zz11,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Triglyceride (mg/dL)",
                                                 title="(M) Triglyceride")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m14<-ggplot(zz14,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="loess",se=F,span=0.9)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="Creatinine (mg/dL)",
                                                 title="(N) Creatinine")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m15<-ggplot(zz15,aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="eGFR (mL/min/1.73 m2)",
                                                 title="(O) eGFR")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))



# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12,m13,m14,m15)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(m01)

# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}


tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\suppl2_ca.tiff",
     width=15,height=20,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), # 범례 없는 그래프 배치
  ncol = 1, heights = c(1, 10)  # 그래프 10배, 범례 1배 크기로 조절
)
dev.off()


#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#Biolgocial age
#Men
dat1
z01<-subset(dat3, variable%in% ("Body mass index")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z02<-subset(dat3, variable%in% ("SBP")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z03<-subset(dat3, variable%in% ("DBP")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z04<-subset(dat3, variable%in% ("Hemoglobin")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z05<-subset(dat3, variable%in% ("Fasting serum glucose")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z06<-subset(dat3, variable%in% ("Total cholesterol")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z07<-subset(dat3, variable%in% ("GOT")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z08<-subset(dat3, variable%in% ("GPT")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z09<-subset(dat3, variable%in% ("GGT")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z10<-subset(dat3, variable%in% ("Waist circumference")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z11<-subset(dat3, variable%in% ("Triglyceride")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z12<-subset(dat3, variable%in% ("HDL cholesterol")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z13<-subset(dat3, variable%in% ("LDL cholesterol")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z14<-subset(dat3, variable%in% ("Creatinine")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
z15<-subset(dat3, variable%in% ("eGFR")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)

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

dat3


m01<-ggplot(zz01,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Body mass index (kg/m2)",
                                                 title="(A) Body mass index")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+  guides(color = guide_legend(ncol = 9))+
  coord_cartesian(xlim=c(40,80))

m02<-ggplot(zz10,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Waist circumference (cm)",
                                                 title="(B) Waist circumference")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


m03<-ggplot(zz02,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="SBP (mmHg)",
                                                 title="(C) SBP")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


m04<-ggplot(zz03,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="DBP (mmHg)",
                                                 title="(D) DBP")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m05<-ggplot(zz05,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Fasting serum glucose (g/dL)",
                                                 title="(E) Fasting serum glucose")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m06<-ggplot(zz04,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Hemoglobin (g/dL)",
                                                 title="(F) Hemoglobin")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m07<-ggplot(zz07,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="GOT (U/L)",
                                                 title="(G) GOT")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m08<-ggplot(zz08,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="GPT (U/L)",
                                                 title="(H) GPT")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m09<-ggplot(zz09,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="GGT (U/L)",
                                                 title="(I) GGT")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m10<-ggplot(zz06,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Total cholesterol (mg/dL)",
                                                 title="(J) Total cholesterol")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


m11<-ggplot(zz12,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="HDL cholesterol (mg/dL)",
                                                 title="(K) HDL cholesterol")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m12<-ggplot(zz15,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="LDL cholesterol (mg/dL)",
                                                 title="(L) LDL cholesterol")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m15<-ggplot(zz11,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Triglyceride (mg/dL)",
                                                 title="(M) Triglyceride")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m14<-ggplot(zz14,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Creatinine (mg/dL)",
                                                 title="(N) Creatinine")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m15<-ggplot(zz15,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="eGFR (mL/min/1.73 m2)",
                                                 title="(O) eGFR")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))



# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12,m13,m14,m15)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(m01)

# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}

# 📌 3️⃣ 그래프 배치 (범례는 따로 추가)
x11() # 창 띄우기
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), # 범례 없는 그래프 배치
  ncol = 1, heights = c(1, 10)  # 그래프 10배, 범례 1배 크기로 조절
)

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\suppl1.tiff",
     width=15,height=20,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 2)), # 범례 없는 그래프 배치
  ncol = 1, heights = c(1, 10)  # 그래프 10배, 범례 1배 크기로 조절
)
dev.off()



x11();grid.arrange(m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12,m13,m14,m15,ncol=5)


#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#Biological age
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


m01<-ggplot(zz01,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Body mass index (kg/m2)",
                                                 title="(A) Body mass index")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+  guides(color = guide_legend(ncol = 9))+
  coord_cartesian(xlim=c(40,80))

m02<-ggplot(zz10,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Waist circumference (cm)",
                                                 title="(B) Waist circumference")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


m03<-ggplot(zz02,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="SBP (mmHg)",
                                                 title="(C) SBP")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


m04<-ggplot(zz03,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="DBP (mmHg)",
                                                 title="(D) DBP")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m05<-ggplot(zz05,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Fasting serum glucose (g/dL)",
                                                 title="(E) Fasting serum glucose")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m06<-ggplot(zz04,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Hemoglobin (g/dL)",
                                                 title="(F) Hemoglobin")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m07<-ggplot(zz07,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="GOT (U/L)",
                                                 title="(G) GOT")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m08<-ggplot(zz08,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="GPT (U/L)",
                                                 title="(H) GPT")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m09<-ggplot(zz09,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="GGT (U/L)",
                                                 title="(I) GGT")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m10<-ggplot(zz06,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Total cholesterol (mg/dL)",
                                                 title="(J) Total cholesterol")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


m11<-ggplot(zz12,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="HDL cholesterol (mg/dL)",
                                                 title="(K) HDL cholesterol")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m12<-ggplot(zz15,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="LDL cholesterol (mg/dL)",
                                                 title="(L) LDL cholesterol")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m15<-ggplot(zz11,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Triglyceride (mg/dL)",
                                                 title="(M) Triglyceride")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m14<-ggplot(zz14,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="Creatinine (mg/dL)",
                                                 title="(N) Creatinine")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))

m15<-ggplot(zz15,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Biological health age",y="eGFR (mL/min/1.73 m2)",
                                                 title="(O) eGFR")+theme_gray(base_size=15)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(xlim=c(40,80))


# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(m01,m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,m12,m13,m14,m15)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(m01)

# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}


tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\suppl2.tiff",
     width=15,height=20,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 2)), # 범례 없는 그래프 배치
  ncol = 1, heights = c(1, 10)  # 그래프 10배, 범례 1배 크기로 조절
)
dev.off()


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

aa01$variable=factor(aa01$variable,levels=unique(aa01$variable))
aa02$variable=factor(aa02$variable,levels=unique(aa02$variable))
aa03$variable=factor(aa03$variable,levels=unique(aa03$variable))

bb01$variable=factor(bb01$variable,levels=unique(bb01$variable))
bb02$variable=factor(bb02$variable,levels=unique(bb02$variable))
bb03$variable=factor(bb03$variable,levels=unique(bb03$variable))

m01<-ggplot(aa01,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(B1) Men, Total healthcare expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+
  guides(color = guide_legend(ncol = 9))

m02<-ggplot(aa02,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(B2) Men, Out-of-pocket expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+
  guides(color = guide_legend(ncol = 9))

m03<-ggplot(aa03,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(B3) Men, Insurance-covered expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+
  guides(color = guide_legend(ncol = 9))

m04<-ggplot(bb01,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="KRW",
                                                 title="(B)BA - Men, Total healthcare expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+
  guides(color = guide_legend(ncol = 9))

m05<-ggplot(bb02,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="KRW",
                                                 title="(B)BA - Men, Out-of-pocket expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+
  guides(color = guide_legend(ncol = 9))

m06<-ggplot(bb03,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="KRW",
                                                 title="(B)BA - Men, Insurance-covered expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+
  guides(color = guide_legend(ncol = 9))


x11();grid.arrange(grid.arrange(m04,m05,m06,ncol=3),
                   grid.arrange(m01,m02,m03,ncol=3))


#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#Women
c01<-subset(dat2, variable%in% ("Total healthcare expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
c02<-subset(dat2, variable%in% ("Out-of-pocket expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
c03<-subset(dat2, variable%in% ("Insurance-covered expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)

cc01<-reshape2::melt(c01,id="age")
cc02<-reshape2::melt(c02,id="age")
cc03<-reshape2::melt(c03,id="age")

d01<-subset(dat2, variable%in% ("Total healthcare expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
d02<-subset(dat2, variable%in% ("Out-of-pocket expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)
d03<-subset(dat2, variable%in% ("Insurance-covered expenditure")) %>% select(age,P10,P20,P30,P40,P50,P60,P70,P80,P90)

dd01<-reshape2::melt(d01,id="age")
dd02<-reshape2::melt(d02,id="age")
dd03<-reshape2::melt(d03,id="age")

m07<-ggplot(cc01,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(C1) Women, Total healthcare expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(ncol = 9))

m08<-ggplot(cc02,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(C2) Women, Out-of-pocket expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(ncol = 9))

m09<-ggplot(cc03,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(C3) Women, Insurance-covered expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(ncol = 9))

m10<-ggplot(dd01,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="KRW",
                                                 title="(a)BA - Women, Total healthcare expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(ncol = 9))

m11<-ggplot(dd02,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="KRW",
                                                 title="(b)BA - Women, Out-of-pocket expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(ncol = 9))

m12<-ggplot(dd03,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F)+
  scale_x_continuous(breaks = seq(25,90,5))+labs(x="Age",y="KRW",
                                                 title="(C)BA - Women, Insurance-covered expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(ncol = 9))

#--------------------------------------------------------------------------------------------------------#

m01<-ggplot(aa01,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(A1) Men, Total healthcare expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+
  guides(color = guide_legend(ncol = 9))

m02<-ggplot(aa02,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(A2) Men, Out-of-pocket expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+
  guides(color = guide_legend(ncol = 9))

m03<-ggplot(aa03,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(A3) Men, Insurance-covered expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+
  guides(color = guide_legend(ncol = 9))


m07<-ggplot(cc01,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(B1) Women, Total healthcare expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(ncol = 9))

m08<-ggplot(cc02,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(B2) Women, Out-of-pocket expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(ncol = 9))

m09<-ggplot(cc03,aes(age,value,col=variable,fill=variable,group=variable))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(B3) Women, Insurance-covered expenditure")+theme_gray(base_size=16)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(ncol = 9))

# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(m01,m07)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(m01)

x11(); grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 2)), 
  ncol = 1, heights = c(1, 10)  # 범례와 그래프 비율 조정
)



# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(medi1,medi2,medi3)
plots2 <- list(m01,m02,m03,m07,m08,m09)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))
plots_no_legend2 <- lapply(plots2, function(p) p + theme(legend.position = "none"))

# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(medi1)
legend2 <- g_legend(m01)

# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}

x11(); grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), 
  legend2, 
  do.call(arrangeGrob, c(plots_no_legend2, ncol = 3)), 
  ncol = 1, heights = c(1, 8, 1, 10)  # 범례와 그래프 비율 조정
)

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\plot3.tiff",
     width=20,height=16,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), 
  legend2, 
  do.call(arrangeGrob, c(plots_no_legend2, ncol = 3)), 
  ncol = 1, heights = c(1, 8, 1, 12)  # 범례와 그래프 비율 조정
)
dev.off()
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
head(ranking)
x11();ggplot(ranking,aes(rank,V1,col=sex,fill=sex,group=sex))+
  scale_x_continuous(breaks=c(1,10,20,30,40,50,60,70,80,90,100))+stat_smooth(method="gam",se=F)+theme_gray(base_size=15)+
  labs(x="Cardiovascular Risk health group",y="Body mass index (kg/m2)",title="(a) Body mass index")+
  theme(legend.title = element_blank(),legend.position = c(0.25, 0.95))+
  guides(color = guide_legend(nrow = 3))+facet_wrap(~age)


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


#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#
#등수별 비용
setwd("D:\\SNU\\연구\\건강나이")
library(readxl)
library(ggplot2)
library(dplyr)
d1<-read_excel("NHIS_HA_20250409.xlsx",sheet="P50_COST_CA_M")
d2<-read_excel("NHIS_HA_20250409.xlsx",sheet="P50_COST_CA_F")
d3<-read_excel("NHIS_HA_20250409.xlsx",sheet="P50_COST_BA_M")
d4<-read_excel("NHIS_HA_20250409.xlsx",sheet="P50_COST_BA_F")

d1<-d1[complete.cases(d1$CVD_SCORE_RANK),];d1$rank=d1$CVD_SCORE_RANK+1
d2<-d2[complete.cases(d2$CVD_SCORE_RANK),];d2$rank=d2$CVD_SCORE_RANK+1
d3<-d3[complete.cases(d3$CVD_SCORE_RANK),];d3$rank=d3$CVD_SCORE_RANK+1
d4<-d4[complete.cases(d4$CVD_SCORE_RANK),];d4$rank=d4$CVD_SCORE_RANK+1

d1$sex="Men";d2$sex="Women"
d3$sex="Men";d4$sex="Women"

d1_r<-subset(d1,rank %in% c(1,10,20,30,40,50,60,70,80,90,100))
d2_r<-subset(d2,rank %in% c(1,10,20,30,40,50,60,70,80,90,100))
d3_r<-subset(d3,rank %in% c(1,10,20,30,40,50,60,70,80,90,100))
d4_r<-subset(d4,rank %in% c(1,10,20,30,40,50,60,70,80,90,100))

ca<-rbind(d1_r,d2_r)
ba<-rbind(d3_r,d4_r)

ca1<-subset(ca) %>% mutate(value=P50_TOTAL_FEE,g="Total healthcare expenditure") %>%  select(rank,sex,LABEL,value,g)
ca2<-subset(ca) %>% mutate(value=P50_PRI_FEE,g="Out-of-pocket expenditure") %>%  select(rank,sex,LABEL,value,g)
ca3<-subset(ca) %>% mutate(value=P50_INSUR_FEE,g="Insurance-covered expenditure") %>%  select(rank,sex,LABEL,value,g)

ba1<-subset(ba) %>% mutate(value=P50_TOTAL_FEE,g="Total healthcare expenditure") %>%  select(rank,sex,LABEL,value,g)
ba2<-subset(ba) %>% mutate(value=P50_PRI_FEE,g="Out-of-pocket expenditure") %>%  select(rank,sex,LABEL,value,g)
ba3<-subset(ba) %>% mutate(value=P50_INSUR_FEE,g="Insurance-covered expenditure") %>%  select(rank,sex,LABEL,value,g)

cca<-rbind(ca1,ca2,ca3)
bba<-rbind(ba1,ba2,ba3)

cca$g=factor(cca$g,levels=unique(cca$g))
bba$g=factor(bba$g,levels=unique(bba$g))

cca$rank=factor(cca$rank,levels=unique(cca$rank))
bba$rank=factor(bba$rank,levels=unique(bba$rank))

x11();grid.arrange(
  ggplot(subset(cca,g=="Total healthcare expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 2))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="",y="Total Health Expenditure")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) , 
  ggplot(subset(cca,g=="Out-of-pocket expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
    stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
    guides(color = guide_legend(nrow = 2))+coord_cartesian(ylim=c(100000,1500000))+
    labs(x="",y="Total Health Expenditure")+
    theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
    theme(legend.position = "top",legend.title = element_blank(),
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank()) , 
  ggplot(subset(cca,g=="Insurance-covered expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
    stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
    guides(color = guide_legend(nrow = 2))+coord_cartesian(ylim=c(100000,1500000))+
    labs(x="",y="Total Health Expenditure")+
    theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
    theme(legend.position = "top",legend.title = element_blank(),
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank()),ncol=3)


ca_expend1<-ggplot(subset(cca,g=="Total healthcare expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 2))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Chronological age",title="(A) Total health expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
ca_expend2<-ggplot(subset(cca,g=="Out-of-pocket expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 2))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Chronological age",title="(B) Out-of-pocket expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) 
ca_expend3<-ggplot(subset(cca,g=="Insurance-covered expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 2))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Chronological age",title="(C) Insurance-covered expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())

# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(ca_expend1,ca_expend2,ca_expend3)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))


# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(ca_expend1)


# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}

x11(); grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), 
  ncol = 1, heights = c(1, 8)  # 범례와 그래프 비율 조정
)

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\expend_rank_ca.tiff",
     width=30,height=12,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), 
  ncol = 1, heights = c(1, 10)  # 범례와 그래프 비율 조정
)
dev.off()

ba_expend1<-ggplot(subset(bba,g=="Total healthcare expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 2))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Biological age",title="(A) Total health expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
ba_expend2<-ggplot(subset(bba,g=="Out-of-pocket expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 2))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Biological age",title="(B) Out-of-pocket expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) 
ba_expend3<-ggplot(subset(bba,g=="Insurance-covered expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 2))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Biological age",title="(C) Insurance-covered expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())

# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(ba_expend1,ba_expend2,ba_expend3)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))


# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(ba_expend1)


# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\expend_rank_ba.tiff",
     width=30,height=12,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), 
  ncol = 1, heights = c(1, 10)  # 범례와 그래프 비율 조정
)
dev.off()
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
d1_r<-subset(d1,rank %in% c(1,10,50,90,100))
d2_r<-subset(d2,rank %in% c(1,10,50,90,100))
d3_r<-subset(d3,rank %in% c(1,10,50,90,100))
d4_r<-subset(d4,rank %in% c(1,10,50,90,100))

ca<-rbind(d1_r,d2_r)
ba<-rbind(d3_r,d4_r)

ca1<-subset(ca) %>% mutate(value=P50_TOTAL_FEE,g="Total healthcare expenditure") %>%  select(rank,sex,LABEL,value,g)
ca2<-subset(ca) %>% mutate(value=P50_PRI_FEE,g="Out-of-pocket expenditure") %>%  select(rank,sex,LABEL,value,g)
ca3<-subset(ca) %>% mutate(value=P50_INSUR_FEE,g="Insurance-covered expenditure") %>%  select(rank,sex,LABEL,value,g)

ba1<-subset(ba) %>% mutate(value=P50_TOTAL_FEE,g="Total healthcare expenditure") %>%  select(rank,sex,LABEL,value,g)
ba2<-subset(ba) %>% mutate(value=P50_PRI_FEE,g="Out-of-pocket expenditure") %>%  select(rank,sex,LABEL,value,g)
ba3<-subset(ba) %>% mutate(value=P50_INSUR_FEE,g="Insurance-covered expenditure") %>%  select(rank,sex,LABEL,value,g)

cca<-rbind(ca1,ca2,ca3)
bba<-rbind(ba1,ba2,ba3)

cca$g=factor(cca$g,levels=unique(cca$g))
bba$g=factor(bba$g,levels=unique(bba$g))

cca$rank=factor(cca$rank,levels=unique(cca$rank))
bba$rank=factor(bba$rank,levels=unique(bba$rank))

ca_expend1<-ggplot(subset(cca,g=="Total healthcare expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Chronological age",title="(A) Total health expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
ca_expend2<-ggplot(subset(cca,g=="Out-of-pocket expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Chronological age",title="(B) Out-of-pocket expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) 
ca_expend3<-ggplot(subset(cca,g=="Insurance-covered expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Chronological age",title="(C) Insurance-covered expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())

# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(ca_expend1,ca_expend2,ca_expend3)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))


# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(ca_expend1)


# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)}

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\expend_rank_ca2.tiff",
     width=30,height=12,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), 
  ncol = 1, heights = c(1, 10)  # 범례와 그래프 비율 조정
)
dev.off()

ba_expend1<-ggplot(subset(bba,g=="Total healthcare expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Biological age",title="(A) Total health expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())
ba_expend2<-ggplot(subset(bba,g=="Out-of-pocket expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Biological age",title="(B) Out-of-pocket expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) 
ba_expend3<-ggplot(subset(bba,g=="Insurance-covered expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Biological age",title="(C) Insurance-covered expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())

# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(ba_expend1,ba_expend2,ba_expend3)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))


# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(ca_expend1)


# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\expend_rank_ba2.tiff",
     width=30,height=12,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), 
  ncol = 1, heights = c(1, 10)  # 범례와 그래프 비율 조정
)
dev.off()

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#3개 그룹만 고려: 1등, 50등, 100등 
d1_r<-subset(d1,rank %in% c(1,50,100))
d2_r<-subset(d2,rank %in% c(1,50,100))
d3_r<-subset(d3,rank %in% c(1,50,100))
d4_r<-subset(d4,rank %in% c(1,50,100))

ca<-rbind(d1_r,d2_r)
ba<-rbind(d3_r,d4_r)

ca1<-subset(ca) %>% mutate(value=P50_TOTAL_FEE,g="Total healthcare expenditure") %>%  select(rank,sex,LABEL,value,g)
ca2<-subset(ca) %>% mutate(value=P50_PRI_FEE,g="Out-of-pocket expenditure") %>%  select(rank,sex,LABEL,value,g)
ca3<-subset(ca) %>% mutate(value=P50_INSUR_FEE,g="Insurance-covered expenditure") %>%  select(rank,sex,LABEL,value,g)

ba1<-subset(ba) %>% mutate(value=P50_TOTAL_FEE,g="Total healthcare expenditure") %>%  select(rank,sex,LABEL,value,g)
ba2<-subset(ba) %>% mutate(value=P50_PRI_FEE,g="Out-of-pocket expenditure") %>%  select(rank,sex,LABEL,value,g)
ba3<-subset(ba) %>% mutate(value=P50_INSUR_FEE,g="Insurance-covered expenditure") %>%  select(rank,sex,LABEL,value,g)

cca<-rbind(ca1,ca2,ca3)
bba<-rbind(ba1,ba2,ba3)

cca$g=factor(cca$g,levels=unique(cca$g))
bba$g=factor(bba$g,levels=unique(bba$g))

cca$rank=factor(cca$rank,levels=unique(cca$rank))
bba$rank=factor(bba$rank,levels=unique(bba$rank))

ca_expend1<-ggplot(subset(cca,g=="Total healthcare expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Chronological age",title="(A) Total health expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())+
  scale_color_manual(values=c("1"="red","50"="#00BA38","100"="blue"))
ca_expend2<-ggplot(subset(cca,g=="Out-of-pocket expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Chronological age",title="(B) Out-of-pocket expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +
  scale_color_manual(values=c("1"="red","50"="#00BA38","100"="blue"))
ca_expend3<-ggplot(subset(cca,g=="Insurance-covered expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Chronological age",title="(C) Insurance-covered expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())+
  scale_color_manual(values=c("1"="red","50"="#00BA38","100"="blue"))

# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(ca_expend1,ca_expend2,ca_expend3)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))


# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(ca_expend1)


# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)}

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\expend_rank_ca3.tiff",
     width=30,height=12,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), 
  ncol = 1, heights = c(1, 10)  # 범례와 그래프 비율 조정
)
dev.off()

ba_expend1<-ggplot(subset(bba,g=="Total healthcare expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Biological age",title="(A) Total health expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())+
  scale_color_manual(values=c("1"="red","50"="#00BA38","100"="blue"))
ba_expend2<-ggplot(subset(bba,g=="Out-of-pocket expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Biological age",title="(B) Out-of-pocket expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +
  scale_color_manual(values=c("1"="red","50"="#00BA38","100"="blue"))
ba_expend3<-ggplot(subset(bba,g=="Insurance-covered expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Biological age",title="(C) Insurance-covered expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())+
  scale_color_manual(values=c("1"="red","50"="#00BA38","100"="blue"))

# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(ba_expend1,ba_expend2,ba_expend3)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))


# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(ca_expend1)


# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\expend_rank_ba3.tiff",
     width=30,height=12,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), 
  ncol = 1, heights = c(1, 10)  # 범례와 그래프 비율 조정
)
dev.off()


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
d1_r<-subset(d1,rank %in% c(1,10,50,90,100))
d2_r<-subset(d2,rank %in% c(1,10,50,90,100))
d3_r<-subset(d3,rank %in% c(1,10,50,90,100))
d4_r<-subset(d4,rank %in% c(1,10,50,90,100))

ca<-rbind(d1_r,d2_r)
ba<-rbind(d3_r,d4_r)

ca1<-subset(ca) %>% mutate(value=P50_TOTAL_FEE,g="Total healthcare expenditure") %>%  select(rank,sex,LABEL,value,g)
ca2<-subset(ca) %>% mutate(value=P50_PRI_FEE,g="Out-of-pocket expenditure") %>%  select(rank,sex,LABEL,value,g)
ca3<-subset(ca) %>% mutate(value=P50_INSUR_FEE,g="Insurance-covered expenditure") %>%  select(rank,sex,LABEL,value,g)

ba1<-subset(ba) %>% mutate(value=P50_TOTAL_FEE,g="Total healthcare expenditure") %>%  select(rank,sex,LABEL,value,g)
ba2<-subset(ba) %>% mutate(value=P50_PRI_FEE,g="Out-of-pocket expenditure") %>%  select(rank,sex,LABEL,value,g)
ba3<-subset(ba) %>% mutate(value=P50_INSUR_FEE,g="Insurance-covered expenditure") %>%  select(rank,sex,LABEL,value,g)

cca<-rbind(ca1,ca2,ca3)
bba<-rbind(ba1,ba2,ba3)

cca$g=factor(cca$g,levels=unique(cca$g))
bba$g=factor(bba$g,levels=unique(bba$g))

cca$rank=factor(cca$rank,levels=unique(cca$rank))
bba$rank=factor(bba$rank,levels=unique(bba$rank))

cca$rank
ca_expend1<-ggplot(subset(cca,g=="Total healthcare expenditure"),
                   aes(LABEL,value,col=rank,size=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Chronological age",title="(A) Total health expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())+
  scale_color_manual(values=c("1"="red","10"="orange","50"="#00BA38","90"="deepskyblue","100"="blue"))+
  scale_size_manual(values = c("1"=2, "10"=1.1, "50"=1.1,"90"=1.1,"100"=2))

ca_expend2<-ggplot(subset(cca,g=="Out-of-pocket expenditure"),
                   aes(LABEL,value,col=rank,size=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Chronological age",title="(B) Out-of-pocket expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +
  scale_color_manual(values=c("1"="red","10"="orange","50"="#00BA38","90"="deepskyblue","100"="blue"))+
  scale_size_manual(values = c("1"=2, "10"=1.1, "50"=1.1,"90"=1.1,"100"=2))
  
ca_expend3<-ggplot(subset(cca,g=="Insurance-covered expenditure"),
                   aes(LABEL,value,col=rank,size=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Chronological age",title="(C) Insurance-covered expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
    theme(legend.position = "top",legend.title = element_blank(),
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank()) +
    scale_color_manual(values=c("1"="red","10"="orange","50"="#00BA38","90"="deepskyblue","100"="blue"))+
    scale_size_manual(values = c("1"=2, "10"=1.1, "50"=1.1,"90"=1.1,"100"=2))

# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(ca_expend1,ca_expend2,ca_expend3)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))


# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(ca_expend1)


# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)}

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\expend_rank_ca4.tiff",
     width=30,height=12,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), 
  ncol = 1, heights = c(1, 10)  # 범례와 그래프 비율 조정
)
dev.off()

ba_expend1<-ggplot(subset(bba,g=="Total healthcare expenditure"),
                   aes(LABEL,value,col=rank,size=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Biological age",title="(A) Total health expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())+
  scale_color_manual(values=c("1"="red","10"="orange","50"="#00BA38","90"="deepskyblue","100"="blue"))+
  scale_size_manual(values = c("1"=2, "10"=1.1, "50"=1.1,"90"=1.1,"100"=2))

ba_expend2<-ggplot(subset(bba,g=="Out-of-pocket expenditure"),
                   aes(LABEL,value,col=rank,size=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Biological age",title="(B) Out-of-pocket expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +
  scale_color_manual(values=c("1"="red","10"="orange","50"="#00BA38","90"="deepskyblue","100"="blue"))+
  scale_size_manual(values = c("1"=2, "10"=1.1, "50"=1.1,"90"=1.1,"100"=2))

ba_expend3<-ggplot(subset(bba,g=="Insurance-covered expenditure"),
                   aes(LABEL,value,col=rank,size=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,1500000))+
  labs(x="Biological age",title="(C) Insurance-covered expenditure",y="KRW")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank()) +
  scale_color_manual(values=c("1"="red","10"="orange","50"="#00BA38","90"="deepskyblue","100"="blue"))+
  scale_size_manual(values = c("1"=2, "10"=1.1, "50"=1.1,"90"=1.1,"100"=2))

# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(ba_expend1,ba_expend2,ba_expend3)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))


# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(ca_expend1)


# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\expend_rank_ba4.tiff",
     width=30,height=12,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), 
  ncol = 1, heights = c(1, 10)  # 범례와 그래프 비율 조정
)
dev.off()






#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#25.04.10 수정 

m01<-ggplot(aa01,aes(age,value,col=variable,group=variable))+stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(A1) Men, Total healthcare expenditure")+
  theme_gray(base_size=20)+
  theme(legend.position = "top")+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+
  guides(color = guide_legend(ncol = 9,title="Decile"))

m07<-ggplot(cc01,aes(age,value,col=variable,fill=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(A2) Women, Total healthcare expenditure")+
  theme_gray(base_size=20)+
  theme(legend.position = "top",legend.title=element_blank())+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(ncol = 9))

aa01$sex="Men"
cc01$sex="Women"

aacc<-rbind(aa01,cc01)

gaacc<-ggplot(aacc,aes(age,value,col=variable,group=variable))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(A)  Total healthcare expenditure by age and decile")+
  theme_bw(base_size=22)+
  theme(legend.position = "top")+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(ncol = 9,title="Decile"))+
  theme(legend.position = "top",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())

x11();gaacc
x11();cca_t1
cca_t1<-ggplot(subset(cca,g=="Total healthcare expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 2))+coord_cartesian(ylim=c(100000,2000000))+
  labs(x="Chronological age",y="KRW",title="(B) Median total healthcare expenditure by chronological age and rank")+  
  guides(color = guide_legend(ncol = 11,title="Rank"))+
  theme_bw(base_size=22)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())

bba_t1<-ggplot(subset(bba,g=="Total healthcare expenditure"),aes(LABEL,value,col=rank))+facet_wrap(~sex)+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 2))+coord_cartesian(ylim=c(100000,2000000))+
  labs(x="Biological age",y="KRW",title="(C) Median total healthcare expenditure by biological age and rank")+ 
  guides(color = guide_legend(ncol = 11,title="Rank"))+
  theme_bw(base_size=22)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",legend.title = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())

# x11();cca_t1
# x11();bba_t1
# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(gaacc)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))
plots2 <- list(cca_t1,bba_t1)
plots_no_legend2 <- lapply(plots2, function(p) p + theme(legend.position = "none"))


# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend  <- g_legend(gaacc)
legend2 <- g_legend(cca_t1)

x11(); grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 2)), 
  ncol = 1, heights = c(1, 10)  # 범례와 그래프 비율 조정
)

x11(); grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 2)), 
  legend2, 
  do.call(arrangeGrob, c(plots_no_legend2, ncol = 2)), 
  ncol = 2, heights = c(1, 10, 1, 10)  # 범례와 그래프 비율 조정
)
x11();gaacc
x11();cca_t1
x11();bba_t1
grid

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\expenditure_250410.tiff",
     width=22,height=22,units="in",res=300,compression="lzw")
grid.arrange(
  legend, 
  do.call(arrangeGrob, c(plots_no_legend, ncol = 1)), 
  legend2, 
  do.call(arrangeGrob, c(plots_no_legend2, ncol = 1)), 
  ncol = 1, heights = c(1, 10, 1, 16)  # 범례와 그래프 비율 조정
)
dev.off()


gaacc<-ggplot(aacc,aes(age,value,col=variable,group=variable))+facet_wrap(~sex,ncol=1)+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(A)  Total healthcare expenditure by age and decile")+
  theme_bw(base_size=18)+
  theme(legend.position = "top")+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(ncol = 9,title="Decile"))+
  theme(legend.position = "top",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank())


gaacc1<-ggplot(subset(aacc,sex=="Men"),aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(A1) Men, total healthcare expenditure by age and decile")+
  theme_bw(base_size=18)+
  theme(legend.position = "top")+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(nrow = 1,title="Decile"))+
  theme(legend.position = "top",panel.grid.major = element_blank(),  panel.grid.minor = element_blank())

gaacc2<-ggplot(subset(aacc,sex=="Men"),aes(age,value,col=variable,group=variable))+
  stat_smooth(method="gam",se=F, formula = y ~ s(x, k = 3))+
  scale_x_continuous(breaks = seq(40,80,5))+labs(x="Chronological age",y="KRW",
                                                 title="(A2) Women, total healthcare expenditure by age and decile")+
  theme_bw(base_size=18)+
  theme(legend.position = "top")+
  coord_cartesian(ylim=c(0,5000000))+
  scale_y_continuous(labels = scales::comma,breaks=seq(0,5000000,by=1000000))+coord_cartesian(ylim=c(0,5000000))+
  guides(color = guide_legend(nrow = 1,title="Decile"))+
  theme(legend.position = "top",panel.grid.major = element_blank(),  panel.grid.minor = element_blank())


cca_t1<-ggplot(subset(cca,g=="Total healthcare expenditure" & sex=="Men"),aes(LABEL,value,col=rank))+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,2000000))+
  labs(x="Chronological age",y="KRW",title="(B1) Men, median total healthcare expenditure by chronological age and rank")+  
  guides(color = guide_legend(nrow = 1,title="Rank"))+
  theme_bw(base_size=18)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",panel.grid.major = element_blank(),  panel.grid.minor = element_blank())
cca_t2<-ggplot(subset(cca,g=="Total healthcare expenditure" & sex=="Women"),aes(LABEL,value,col=rank))+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,2000000))+
  labs(x="Chronological age",y="KRW",title="(B2) Women, median total healthcare expenditure by chronological age and rank")+  
  guides(color = guide_legend(nrow = 1,title="Rank"))+
  theme_bw(base_size=18)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",panel.grid.major = element_blank(),  panel.grid.minor = element_blank())

bba_t1<-ggplot(subset(bba,g=="Total healthcare expenditure"& sex=="Men"& LABEL %in% c(40:80)),aes(LABEL,value,col=rank))+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,2000000))+
  labs(x="Biological age",y="KRW",title="(C1) Men, median total healthcare expenditure by biological age and rank")+ 
  guides(color = guide_legend(nrow = 1,title="Rank"))+
  theme_bw(base_size=18)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",panel.grid.major = element_blank(),  panel.grid.minor = element_blank())

 
bba_t2<-ggplot(subset(bba,g=="Total healthcare expenditure"& sex=="Women" & LABEL %in% c(40:80)),aes(LABEL,value,col=rank))+
  stat_smooth(method="gam",se=F,size=1.1, formula = y ~ s(x, k = 3))+
  guides(color = guide_legend(nrow = 1))+coord_cartesian(ylim=c(100000,2000000))+
  labs(x="Biological age",y="KRW",title="(C2) Women, median total healthcare expenditure by biological age and rank")+ 
  guides(color = guide_legend(nrow = 1,title="Rank"))+
  theme_bw(base_size=18)+scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "top",panel.grid.major = element_blank(),  panel.grid.minor = element_blank())

x11();grid.arrange(
  gaacc1,cca_t1,bba_t1,
  gaacc2,cca_t2,bba_t2,ncol=3)


plots <- list(gaacc1,gaacc2)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))
plots2 <- list(cca_t1,bba_t1,cca_t2,bba_t2)
plots_no_legend2 <- lapply(plots2, function(p) p + theme(legend.position = "none"))

# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend  <- g_legend(gaacc1)
legend2 <- g_legend(cca_t1)

x11(); grid.arrange(
  arrangeGrob(
    arrangeGrob(legend, do.call(arrangeGrob, c(plots_no_legend, ncol = 1)), ncol = 1, 
                heights = c(1, 10)),  # 첫 번째 그룹 (범례 작게)
    arrangeGrob(legend2, do.call(arrangeGrob, c(plots_no_legend2, ncol = 2)), ncol = 1, 
                heights = c(1, 10)),  # 두 번째 그룹 (범례 작게)
    ncol = 2 , widths = c(2, 2) # 두 개의 그룹을 열로 정렬
  )
)

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\expenditure_250410_rev3.tiff",
     width=36,height=16,units="in",res=300,compression="lzw")
grid.arrange(
  arrangeGrob(
    arrangeGrob(legend, do.call(arrangeGrob, c(plots_no_legend, ncol = 1)), ncol = 1, 
                heights = c(1, 10)),  # 첫 번째 그룹 (범례 작게)
    arrangeGrob(legend2, do.call(arrangeGrob, c(plots_no_legend2, ncol = 2)), ncol = 1, 
                heights = c(1, 10)),  # 두 번째 그룹 (범례 작게)
    ncol = 2, widths = c(1, 2)  # 두 개의 그룹을 열로 정렬
  )
)
dev.off()
