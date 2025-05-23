setwd("D:\\SNU\\연구\\건강나이\\반출_하은희_오종민_250521")

library(readxl)
library(ggplot2)
library(dplyr)

z<-read_excel("NHIS_HA_20250521_OUT.xlsx",sheet='Sheet4')
z<-subset(z,FU<=5)
names(z)
z$CA_SE   =with(z,SD_CA   /sqrt(N_CA))
z$BA_SE   =with(z,SD_BA   /sqrt(N_BA))
z$AA_SE   =with(z,SD_AA   /sqrt(N_AA))
z$SBP_SE  =with(z,SD_SBP  /sqrt(N_SBP))
z$DBP_SE  =with(z,SD_DBP  /sqrt(N_DBP))
z$HGB_SE  =with(z,SD_HGB  /sqrt(N_HGB))
z$FBS_SE  =with(z,SD_FBS  /sqrt(N_FBS))
z$TCHOL_SE=with(z,SD_TCHOL/sqrt(N_TCHOL))
z$GOT_SE  =with(z,SD_GOT  /sqrt(N_GOT))
z$GPT_SE  =with(z,SD_GPT  /sqrt(N_GPT))
z$GGT_SE  =with(z,SD_GGT  /sqrt(N_GGT))
z$WSTC_SE =with(z,SD_WSTC /sqrt(N_WSTC))
z$TG_SE   =with(z,SD_TG   /sqrt(N_TG))
z$HDL_SE  =with(z,SD_HDL  /sqrt(N_HDL))
z$LDL_SE  =with(z,SD_LDL  /sqrt(N_LDL))
z$CRTN_SE =with(z,SD_CRTN /sqrt(N_CRTN))
z$GFR_SE  =with(z,SD_GFR  /sqrt(N_GFR))
z$BMI_SE  =with(z,SD_BMI  /sqrt(N_BMI))

z %>% dplyr:: select(AA_G,CRTN,CRTN_SE,SD_CRTN,N_CRTN) %>% View

names(z)
head(z)
x11();ggplot(z,aes(FU,CA,group=AA_G,col=AA_G,fill=AA_G))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=CA-1.96*CA_SE,ymax=CA+1.96*CA_SE), alpha = 0.2)+
  scale_color_manual(values=c("red","black","blue"))+
  scale_fill_manual(values=c("red","black","blue"))+facet_wrap(~Sex)+
  labs(x="Time")+theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())+
  labs(y="Chronological age (years)")

x11();ggplot(z,aes(FU,CA,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=CA-1.96*CA_SE,ymax=CA+1.96*CA_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time")+theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())+
  labs(y="Chronological age (years)")


x11();ggplot(z,aes(FU,BA,group=AA_G,col=AA_G,fill=AA_G))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=BA-1.96*BA_SE,ymax=BA+1.96*BA_SE), alpha = 0.2)+
  scale_color_manual(values=c("red","black","blue"))+
  scale_fill_manual(values=c("red","black","blue"))+facet_wrap(~Sex)+
  labs(x="Time")+theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())+
  labs(y="Biological age (years)")
  
x11();ggplot(z,aes(FU,BMI,group=AA_G,col=AA_G,fill=AA_G))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=BMI-1.96*BMI_SE,ymax=BMI+1.96*BMI_SE), alpha = 0.2)+
  scale_color_manual(values=c("red","black","blue"))+
  scale_fill_manual(values=c("red","black","blue"))+facet_wrap(~Sex)+
  labs(x="Time")+theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())+
  labs(y="Body mass index")

x11();ggplot(z,aes(FU,BMI,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=BMI-1.96*BMI_SE,ymax=BMI+1.96*BMI_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="Body mass index (kg/m2)",title="(A) Body mass index")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,WSTC,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=WSTC-1.96*WSTC_SE,ymax=WSTC+1.96*WSTC_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="Waist circumference (cm)",title="(B) Waist circumference")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,SBP,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=SBP-1.96*SBP_SE,ymax=SBP+1.96*SBP_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="SBP (mmHg)",title="(C) SBP")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,DBP,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=DBP-1.96*DBP_SE,ymax=DBP+1.96*DBP_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="DBP (mmHg)",title="(C) DBP")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,DBP,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=DBP-1.96*DBP_SE,ymax=DBP+1.96*DBP_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="DBP (mmHg)",title="(D) DBP")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,FBS,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=FBS-1.96*FBS_SE,ymax=FBS+1.96*FBS_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="Fasting serum glucose (g/dL)",title="(E) Fasting serum glucose")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,HGB,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=HGB-1.96*HGB_SE,ymax=HGB+1.96*HGB_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="Hemoglobin (g/dL)",title="(F) Hemoglobin")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,GOT,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=GOT-1.96*GOT_SE,ymax=GOT+1.96*GOT_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="GOT (U/L)",title="(G) GOT")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,GPT,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=GPT-1.96*GPT_SE,ymax=GPT+1.96*GPT_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="GPT (U/L)",title="(H) GPT")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,GGT,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=GGT-1.96*GGT_SE,ymax=GGT+1.96*GGT_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="GGT (U/L)",title="(I) GGT")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,TCHOL,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=TCHOL-1.96*TCHOL_SE,ymax=TCHOL+1.96*TCHOL_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="TCHOL (mg/dL)",title="(J) Total cholesterol")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,HDL,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=HDL-1.96*HDL_SE,ymax=HDL+1.96*HDL_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="HDL (mg/dL)",title="(K) HDL cholesterol")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,LDL,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=LDL-1.96*LDL_SE,ymax=LDL+1.96*LDL_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="LDL (mg/dL)",title="(L) LDL cholesterol")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,TG,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=TG-1.96*TG_SE,ymax=TG+1.96*TG_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="TG (mg/dL)",title="(M) Triglyceride")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,CRTN,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=CRTN-1.96*CRTN_SE,ymax=CRTN+1.96*CRTN_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="CRTN (mg/dL)",title="(N) Creatinine")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())

x11();ggplot(z,aes(FU,GFR,group=Sex,col=Sex,fill=Sex))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=GFR-1.96*GFR_SE,ymax=GFR+1.96*GFR_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+
  labs(x="Time",y="GFR (mg/dL)",title="(O) eGFR")+
  theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
g00<-ggplot(z,aes(FU,BMI,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+
  geom_ribbon(aes(ymin=BMI-1.96*BMI_SE,ymax=BMI+1.96*BMI_SE), alpha = 0.2)+
  scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+
  facet_wrap(~AA_G)+labs(x="Time",y="Body mass index (kg/m2)",title="(A) Body mass index")+
  theme_gray(base_size=30)+theme(legend.position = "top",legend.title = element_blank())


g01<-ggplot(z,aes(FU,BMI,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=BMI-1.96*BMI_SE,ymax=BMI+1.96*BMI_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="Body mass index (kg/m2)",title="(A) Body mass index")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g02<-ggplot(z,aes(FU,WSTC,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=WSTC-1.96*WSTC_SE,ymax=WSTC+1.96*WSTC_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="Waist circumference (cm)",title="(B) Waist circumference")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g03<-ggplot(z,aes(FU,SBP,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=SBP-1.96*SBP_SE,ymax=SBP+1.96*SBP_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="SBP (mmHg)",title="(C) SBP")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g04<-ggplot(z,aes(FU,DBP,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=DBP-1.96*DBP_SE,ymax=DBP+1.96*DBP_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="DBP (mmHg)",title="(D) DBP")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g05<-ggplot(z,aes(FU,FBS,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=FBS-1.96*FBS_SE,ymax=FBS+1.96*FBS_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="Fasting serum glucose (g/dL)",title="(E) Fasting serum glucose")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g06<-ggplot(z,aes(FU,HGB,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=HGB-1.96*HGB_SE,ymax=HGB+1.96*HGB_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="Hemoglobin (g/dL)",title="(F) Hemoglobin")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g07<-ggplot(z,aes(FU,GOT,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=GOT-1.96*GOT_SE,ymax=GOT+1.96*GOT_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="GOT (U/L)",title="(G) GOT")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g08<-ggplot(z,aes(FU,GPT,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=GPT-1.96*GPT_SE,ymax=GPT+1.96*GPT_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="GPT (U/L)",title="(H) GPT")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g09<-ggplot(z,aes(FU,GGT,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=GGT-1.96*GGT_SE,ymax=GGT+1.96*GGT_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="GGT (U/L)",title="(I) GGT")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g10<-ggplot(z,aes(FU,TCHOL,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=TCHOL-1.96*TCHOL_SE,ymax=TCHOL+1.96*TCHOL_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="TCHOL (mg/dL)",title="(J) Total cholesterol")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g11<-ggplot(z,aes(FU,HDL,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=HDL-1.96*HDL_SE,ymax=HDL+1.96*HDL_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="HDL (mg/dL)",title="(K) HDL cholesterol")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g12<-ggplot(z,aes(FU,LDL,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=LDL-1.96*LDL_SE,ymax=LDL+1.96*LDL_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="LDL (mg/dL)",title="(L) LDL cholesterol")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())

g13<-ggplot(z,aes(FU,TG,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=TG-1.96*TG_SE,ymax=TG+1.96*TG_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="Time",y="TG (mg/dL)",title="(M) Triglyceride")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g14<-ggplot(z,aes(FU,CRTN,group=Sex,col=Sex,fill=Sex))+geom_line(size=1.1)+geom_ribbon(aes(ymin=CRTN-1.96*CRTN_SE,ymax=CRTN+1.96*CRTN_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="Time",y="CRTN (mg/dL)",title="(N) Creatinine")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g15<-ggplot(z,aes(FU,GFR,group=Sex,col=Sex,fill=Sex)) +geom_line(size=1.1)+geom_ribbon(aes(ymin=GFR-1.96*GFR_SE,ymax=GFR+1.96*GFR_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="Time",y="GFR (mg/dL)",title="(O) eGFR")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())


# 범례 추출 함수 (gridExtra 활용)
g_legend <- function(a_gplot){
  tmp <- ggplotGrob(a_gplot + theme(legend.position="top"))$grobs
  legend <- tmp[[which(sapply(tmp, function(x) x$name) == "guide-box")]]
  return(legend)
}


# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(g01,g02,g03,g04,g05,g06,g07,g08,g09,g10,g11,g12,g13,g14,g15)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(g00)


x11(); grid.arrange(
  
  arrangeGrob(legend, do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), ncol = 1, 
              heights = c(1, 10))
)
tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\Manuscript\\Submission\\The_Lancet_Public_Health\\Trajectory.tiff",
     width=18,height=22,units="in",res=300,compression="lzw")
grid.arrange(
  arrangeGrob(legend, do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), ncol = 1, 
              heights = c(1, 10)))
dev.off()

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
z<-read_excel("NHIS_HA_20250521_OUT.xlsx",sheet='Sheet9')
z<-subset(z,FU<=5)
names(z)[1]="AA_G"

z$CA_SE   =with(z,SD_CA   /sqrt(N_CA))
z$BA_SE   =with(z,SD_BA   /sqrt(N_BA))
z$AA_SE   =with(z,SD_AA   /sqrt(N_AA))
z$SBP_SE  =with(z,SD_SBP  /sqrt(N_SBP))
z$DBP_SE  =with(z,SD_DBP  /sqrt(N_DBP))
z$HGB_SE  =with(z,SD_HGB  /sqrt(N_HGB))
z$FBS_SE  =with(z,SD_FBS  /sqrt(N_FBS))
z$TCHOL_SE=with(z,SD_TCHOL/sqrt(N_TCHOL))
z$GOT_SE  =with(z,SD_GOT  /sqrt(N_GOT))
z$GPT_SE  =with(z,SD_GPT  /sqrt(N_GPT))
z$GGT_SE  =with(z,SD_GGT  /sqrt(N_GGT))
z$WSTC_SE =with(z,SD_WSTC /sqrt(N_WSTC))
z$TG_SE   =with(z,SD_TG   /sqrt(N_TG))
z$HDL_SE  =with(z,SD_HDL  /sqrt(N_HDL))
z$LDL_SE  =with(z,SD_LDL  /sqrt(N_LDL))
z$CRTN_SE =with(z,SD_CRTN /sqrt(N_CRTN))
z$GFR_SE  =with(z,SD_GFR  /sqrt(N_GFR))
z$BMI_SE  =with(z,SD_BMI  /sqrt(N_BMI))

z$CRTN
z$CRTN-1.96*z$CRTN_SE
z$CRTN+1.96*z$CRTN_SE

x11();ggplot(z,aes(FU,CA,group=AA_G,col=AA_G,fill=AA_G))+
  geom_line(size=1.1)+
  geom_ribbon(aes(ymin=CA-1.96*CA_SE,ymax=CA+1.96*CA_SE), alpha = 0.2)+
  scale_color_manual(values=c("red","black","blue"))+
  scale_fill_manual(values=c("red","black","blue"))+facet_wrap(~CA_G)+
  labs(x="Time")+theme_gray(base_size=25)+theme(legend.position = "top",legend.title = element_blank())+
  labs(y="Chronological age (years)")


g00<-ggplot(z,aes(FU,BMI,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=BMI-1.96*BMI_SE,ymax=BMI+1.96*BMI_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="Body mass index (kg/m2)",title="(A) Body mass index")+theme_gray(base_size=20)+
  theme(legend.position = "top",legend.title = element_blank())

g01<-ggplot(z,aes(FU,BMI,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=BMI-1.96*BMI_SE,ymax=BMI+1.96*BMI_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="Body mass index (kg/m2)",title="(A) Body mass index")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g02<-ggplot(z,aes(FU,WSTC,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=WSTC-1.96*WSTC_SE,ymax=WSTC+1.96*WSTC_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="Waist circumference (cm)",title="(B) Waist circumference")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g03<-ggplot(z,aes(FU,SBP,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=SBP-1.96*SBP_SE,ymax=SBP+1.96*SBP_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="SBP (mmHg)",title="(C) SBP")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g04<-ggplot(z,aes(FU,DBP,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=DBP-1.96*DBP_SE,ymax=DBP+1.96*DBP_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="DBP (mmHg)",title="(D) DBP")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g05<-ggplot(z,aes(FU,FBS,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=FBS-1.96*FBS_SE,ymax=FBS+1.96*FBS_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="Fasting serum glucose (g/dL)",title="(E) Fasting serum glucose")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g06<-ggplot(z,aes(FU,HGB,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=HGB-1.96*HGB_SE,ymax=HGB+1.96*HGB_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="Hemoglobin (g/dL)",title="(F) Hemoglobin")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g07<-ggplot(z,aes(FU,GOT,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=GOT-1.96*GOT_SE,ymax=GOT+1.96*GOT_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="GOT (U/L)",title="(G) GOT")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g08<-ggplot(z,aes(FU,GPT,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=GPT-1.96*GPT_SE,ymax=GPT+1.96*GPT_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="GPT (U/L)",title="(H) GPT")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g09<-ggplot(z,aes(FU,GGT,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=GGT-1.96*GGT_SE,ymax=GGT+1.96*GGT_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="GGT (U/L)",title="(I) GGT")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g10<-ggplot(z,aes(FU,TCHOL,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=TCHOL-1.96*TCHOL_SE,ymax=TCHOL+1.96*TCHOL_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="TCHOL (mg/dL)",title="(J) Total cholesterol")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g11<-ggplot(z,aes(FU,HDL,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=HDL-1.96*HDL_SE,ymax=HDL+1.96*HDL_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="HDL (mg/dL)",title="(K) HDL cholesterol")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g12<-ggplot(z,aes(FU,LDL,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=LDL-1.96*LDL_SE,ymax=LDL+1.96*LDL_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="",y="LDL (mg/dL)",title="(L) LDL cholesterol")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())

g13<-ggplot(z,aes(FU,TG,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=TG-1.96*TG_SE,ymax=TG+1.96*TG_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="Time",y="TG (mg/dL)",title="(M) Triglyceride")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g14<-ggplot(z,aes(FU,CRTN,group=CA_G,col=CA_G,fill=CA_G))+geom_line(size=1.1)+geom_ribbon(aes(ymin=CRTN-1.96*CRTN_SE,ymax=CRTN+1.96*CRTN_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="Time",y="CRTN (mg/dL)",title="(N) Creatinine")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())
g15<-ggplot(z,aes(FU,GFR,group=CA_G,col=CA_G,fill=CA_G)) +geom_line(size=1.1)+geom_ribbon(aes(ymin=GFR-1.96*GFR_SE,ymax=GFR+1.96*GFR_SE), alpha = 0.2)+scale_color_manual(values=c("blue","red"))+scale_fill_manual(values=c("blue","red"))+facet_wrap(~AA_G)+labs(x="Time",y="GFR (mg/dL)",title="(O) eGFR")+theme_gray(base_size=20)+theme(legend.position = "top",legend.title = element_blank())

# 📌 1️⃣ 개별 그래프에서 범례 제거 (theme(legend.position = "none"))
plots <- list(g01,g02,g03,g04,g05,g06,g07,g08,g09,g10,g11,g12,g13,g14,g15)
plots_no_legend <- lapply(plots, function(p) p + theme(legend.position = "none"))

# 📌 2️⃣ 하나의 그래프에서만 범례 가져오기 (g1 기준)
legend <- g_legend(g00)

tiff(filename="D:\\EUMC\\논문\\연구논문\\건강나이\\Manuscript\\Submission\\The_Lancet_Public_Health\\Trajectory2.tiff",
     width=18,height=22,units="in",res=300,compression="lzw")
grid.arrange(
  arrangeGrob(legend, do.call(arrangeGrob, c(plots_no_legend, ncol = 3)), ncol = 1, 
              heights = c(1, 10)))
dev.off()
