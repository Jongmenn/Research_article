#Sub group analysis

hn.r$age %>% table


hn.r$age_group<-ifelse(hn.r$age<65,1,2)

hn.sex1<-subset(hn.r,sex==1)
hn.sex2<-subset(hn.r,sex==2)

hn.age1<-subset(hn.r,age_group==1)
hn.age2<-subset(hn.r,age_group==2)

summary(hn.age1$age)

#복합표본분석 준비
com.svy.sex1<-svydesign(ids    =~psu,       #집락
                        strata =~kstrata,   #층
                        weights=~T_wt_itvex,#가중치
                        data   =hn.sex1)
com.svy.sex2<-svydesign(ids    =~psu,       #집락
                        strata =~kstrata,   #층
                        weights=~T_wt_itvex,#가중치
                        data   =hn.sex2)
#복합표본분석 준비
com.svy.age1<-svydesign(ids    =~psu,       #집락
                        strata =~kstrata,   #층
                        weights=~T_wt_itvex,#가중치
                        data   =hn.age1)
com.svy.age2<-svydesign(ids    =~psu,       #집락
                        strata =~kstrata,   #층
                        weights=~T_wt_itvex,#가중치
                        data   =hn.age2)

#-----------------------------------------------------------------------------------------------------------#
#Men -linear
library(splines)

#복합표본분석-선형 회귀
svy.fita1<-svyglm(eGFR~PM25,data=hn.r,com.svy.sex1);svy.fita2<-svyglm(eGFR~PM10,data=hn.r,com.svy.sex1)
svy.fita3<-svyglm(eGFR~SO2,data=hn.r,com.svy.sex1) ;svy.fita4<-svyglm(eGFR~NO2,data=hn.r,com.svy.sex1)
svy.fita5<-svyglm(eGFR~CO,data=hn.r,com.svy.sex1)  ;svy.fita6<-svyglm(eGFR~O3,data=hn.r,com.svy.sex1)

svy.fitb1<-svyglm(eGFR~PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb2<-svyglm(eGFR~PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb3<-svyglm(eGFR~SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb4<-svyglm(eGFR~NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb5<-svyglm(eGFR~CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb6<-svyglm(eGFR~O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)


#Women -linear
svy.fita1<-svyglm(eGFR~PM25,data=hn.r,com.svy.sex2);svy.fita2<-svyglm(eGFR~PM10,data=hn.r,com.svy.sex2)
svy.fita3<-svyglm(eGFR~SO2,data=hn.r,com.svy.sex2) ;svy.fita4<-svyglm(eGFR~NO2,data=hn.r,com.svy.sex2)
svy.fita5<-svyglm(eGFR~CO,data=hn.r,com.svy.sex2)  ;svy.fita6<-svyglm(eGFR~O3,data=hn.r,com.svy.sex2)

svy.fitb1<-svyglm(eGFR~PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb2<-svyglm(eGFR~PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb3<-svyglm(eGFR~SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb4<-svyglm(eGFR~NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb5<-svyglm(eGFR~CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb6<-svyglm(eGFR~O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)

#-----------------------------------------------------------------------------------------------------------#
#Men -linear lag1
#복합표본분석-선형 회귀
svy.fita1<-svyglm(eGFR~lag1_PM25,data=hn.r,com.svy.sex1);svy.fita2<-svyglm(eGFR~lag1_PM10,data=hn.r,com.svy.sex1)
svy.fita3<-svyglm(eGFR~lag1_SO2,data=hn.r,com.svy.sex1) ;svy.fita4<-svyglm(eGFR~lag1_NO2,data=hn.r,com.svy.sex1)
svy.fita5<-svyglm(eGFR~lag1_CO,data=hn.r,com.svy.sex1)  ;svy.fita6<-svyglm(eGFR~lag1_O3,data=hn.r,com.svy.sex1)

svy.fitb1<-svyglm(eGFR~lag1_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb2<-svyglm(eGFR~lag1_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb3<-svyglm(eGFR~lag1_SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb4<-svyglm(eGFR~lag1_NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb5<-svyglm(eGFR~lag1_CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb6<-svyglm(eGFR~lag1_O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)


#Women -linear lag1
svy.fita1<-svyglm(eGFR~lag1_PM25,data=hn.r,com.svy.sex2);svy.fita2<-svyglm(eGFR~lag1_PM10,data=hn.r,com.svy.sex2)
svy.fita3<-svyglm(eGFR~lag1_SO2,data=hn.r,com.svy.sex2) ;svy.fita4<-svyglm(eGFR~lag1_NO2,data=hn.r,com.svy.sex2)
svy.fita5<-svyglm(eGFR~lag1_CO,data=hn.r,com.svy.sex2)  ;svy.fita6<-svyglm(eGFR~lag1_O3,data=hn.r,com.svy.sex2)

svy.fitb1<-svyglm(eGFR~lag1_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb2<-svyglm(eGFR~lag1_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb3<-svyglm(eGFR~lag1_SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb4<-svyglm(eGFR~lag1_NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb5<-svyglm(eGFR~lag1_CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb6<-svyglm(eGFR~lag1_O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)

#-----------------------------------------------------------------------------------------------------------#
#Men -linear lag01
#복합표본분석-선형 회귀
svy.fita1<-svyglm(eGFR~lag01_PM25,data=hn.r,com.svy.sex1);svy.fita2<-svyglm(eGFR~lag01_PM10,data=hn.r,com.svy.sex1)
svy.fita3<-svyglm(eGFR~lag01_SO2,data=hn.r,com.svy.sex1) ;svy.fita4<-svyglm(eGFR~lag01_NO2,data=hn.r,com.svy.sex1)
svy.fita5<-svyglm(eGFR~lag01_CO,data=hn.r,com.svy.sex1)  ;svy.fita6<-svyglm(eGFR~lag01_O3,data=hn.r,com.svy.sex1)

svy.fitb1<-svyglm(eGFR~lag01_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb2<-svyglm(eGFR~lag01_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb3<-svyglm(eGFR~lag01_SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb4<-svyglm(eGFR~lag01_NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb5<-svyglm(eGFR~lag01_CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)
svy.fitb6<-svyglm(eGFR~lag01_O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)


#Women -linear lag01
svy.fita1<-svyglm(eGFR~lag01_PM25,data=hn.r,com.svy.sex2);svy.fita2<-svyglm(eGFR~lag01_PM10,data=hn.r,com.svy.sex2)
svy.fita3<-svyglm(eGFR~lag01_SO2,data=hn.r,com.svy.sex2) ;svy.fita4<-svyglm(eGFR~lag01_NO2,data=hn.r,com.svy.sex2)
svy.fita5<-svyglm(eGFR~lag01_CO,data=hn.r,com.svy.sex2)  ;svy.fita6<-svyglm(eGFR~lag01_O3,data=hn.r,com.svy.sex2)

svy.fitb1<-svyglm(eGFR~lag01_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb2<-svyglm(eGFR~lag01_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb3<-svyglm(eGFR~lag01_SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb4<-svyglm(eGFR~lag01_NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb5<-svyglm(eGFR~lag01_CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)
svy.fitb6<-svyglm(eGFR~lag01_O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)



#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#
#가중치 적용, 신장기능 지표,eGFR 로지스틱회귀
#Men logistic
svy.fita1<-svyglm(eGFR_g~PM25,data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~PM10,data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~SO2,data=hn.r,com.svy.sex1 ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~NO2,data=hn.r,com.svy.sex1 ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~CO,data=hn.r,com.svy.sex1  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~O3,data=hn.r,com.svy.sex1  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)

#Women -logistic
svy.fita1<-svyglm(eGFR_g~PM25,data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~PM10,data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~SO2,data=hn.r,com.svy.sex2 ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~NO2,data=hn.r,com.svy.sex2 ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~CO,data=hn.r,com.svy.sex2  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~O3,data=hn.r,com.svy.sex2  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)


#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#
#가중치 적용, 신장기능 지표,eGFR 로지스틱회귀
#Men logistic
svy.fita1<-svyglm(eGFR_g~lag1_PM25,data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~lag1_PM10,data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~lag1_SO2,data=hn.r,com.svy.sex1 ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~lag1_NO2,data=hn.r,com.svy.sex1 ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~lag1_CO,data=hn.r,com.svy.sex1  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~lag1_O3,data=hn.r,com.svy.sex1  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~lag1_PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~lag1_PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~lag1_SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~lag1_NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~lag1_CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~lag1_O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)

#Women -logistic
svy.fita1<-svyglm(eGFR_g~lag1_PM25,data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~lag1_PM10,data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~lag1_SO2,data=hn.r,com.svy.sex2 ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~lag1_NO2,data=hn.r,com.svy.sex2 ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~lag1_CO,data=hn.r,com.svy.sex2  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~lag1_O3,data=hn.r,com.svy.sex2  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~lag1_PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~lag1_PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~lag1_SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~lag1_NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~lag1_CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~lag1_O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)

#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#
#가중치 적용, 신장기능 지표,eGFR 로지스틱회귀
#Men logistic
svy.fita1<-svyglm(eGFR_g~lag01_PM25,data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~lag01_PM10,data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~lag01_SO2,data=hn.r,com.svy.sex1 ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~lag01_NO2,data=hn.r,com.svy.sex1 ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~lag01_CO,data=hn.r,com.svy.sex1  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~lag01_O3,data=hn.r,com.svy.sex1  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~lag01_PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~lag01_PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~lag01_SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~lag01_NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~lag01_CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~lag01_O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex1,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)

#Women -logistic
svy.fita1<-svyglm(eGFR_g~lag01_PM25,data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~lag01_PM10,data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~lag01_SO2,data=hn.r,com.svy.sex2 ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~lag01_NO2,data=hn.r,com.svy.sex2 ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~lag01_CO,data=hn.r,com.svy.sex2  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~lag01_O3,data=hn.r,com.svy.sex2  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~lag01_PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~lag01_PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~lag01_SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~lag01_NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~lag01_CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~lag01_O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.sex2,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)





#-----------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------#
#서브그룹 연령 
#Age 20-64
#복합표본분석-선형 회귀
svy.fita1<-svyglm(eGFR~PM25,data=hn.r,com.svy.age1);svy.fita2<-svyglm(eGFR~PM10,data=hn.r,com.svy.age1)
svy.fita3<-svyglm(eGFR~SO2,data=hn.r,com.svy.age1) ;svy.fita4<-svyglm(eGFR~NO2,data=hn.r,com.svy.age1)
svy.fita5<-svyglm(eGFR~CO,data=hn.r,com.svy.age1)  ;svy.fita6<-svyglm(eGFR~O3,data=hn.r,com.svy.age1)

svy.fitb1<-svyglm(eGFR~PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb2<-svyglm(eGFR~PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb3<-svyglm(eGFR~SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb4<-svyglm(eGFR~NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb5<-svyglm(eGFR~CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb6<-svyglm(eGFR~O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)


#Age 65>=
svy.fita1<-svyglm(eGFR~PM25,data=hn.r,com.svy.age2);svy.fita2<-svyglm(eGFR~PM10,data=hn.r,com.svy.age2)
svy.fita3<-svyglm(eGFR~SO2,data=hn.r,com.svy.age2) ;svy.fita4<-svyglm(eGFR~NO2,data=hn.r,com.svy.age2)
svy.fita5<-svyglm(eGFR~CO,data=hn.r,com.svy.age2)  ;svy.fita6<-svyglm(eGFR~O3,data=hn.r,com.svy.age2)

svy.fitb1<-svyglm(eGFR~PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb2<-svyglm(eGFR~PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb3<-svyglm(eGFR~SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb4<-svyglm(eGFR~NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb5<-svyglm(eGFR~CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb6<-svyglm(eGFR~O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)

#-----------------------------------------------------------------------------------------------------------#
#Age 20-64 -linear lag1
#복합표본분석-선형 회귀
svy.fita1<-svyglm(eGFR~lag1_PM25,data=hn.r,com.svy.age1);svy.fita2<-svyglm(eGFR~lag1_PM10,data=hn.r,com.svy.age1)
svy.fita3<-svyglm(eGFR~lag1_SO2,data=hn.r,com.svy.age1) ;svy.fita4<-svyglm(eGFR~lag1_NO2,data=hn.r,com.svy.age1)
svy.fita5<-svyglm(eGFR~lag1_CO,data=hn.r,com.svy.age1)  ;svy.fita6<-svyglm(eGFR~lag1_O3,data=hn.r,com.svy.age1)

svy.fitb1<-svyglm(eGFR~lag1_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb2<-svyglm(eGFR~lag1_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb3<-svyglm(eGFR~lag1_SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb4<-svyglm(eGFR~lag1_NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb5<-svyglm(eGFR~lag1_CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb6<-svyglm(eGFR~lag1_O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)


#Age 65>= -linear lag1
svy.fita1<-svyglm(eGFR~lag1_PM25,data=hn.r,com.svy.age2);svy.fita2<-svyglm(eGFR~lag1_PM10,data=hn.r,com.svy.age2)
svy.fita3<-svyglm(eGFR~lag1_SO2,data=hn.r,com.svy.age2) ;svy.fita4<-svyglm(eGFR~lag1_NO2,data=hn.r,com.svy.age2)
svy.fita5<-svyglm(eGFR~lag1_CO,data=hn.r,com.svy.age2)  ;svy.fita6<-svyglm(eGFR~lag1_O3,data=hn.r,com.svy.age2)

svy.fitb1<-svyglm(eGFR~lag1_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb2<-svyglm(eGFR~lag1_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb3<-svyglm(eGFR~lag1_SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb4<-svyglm(eGFR~lag1_NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb5<-svyglm(eGFR~lag1_CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb6<-svyglm(eGFR~lag1_O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)

#-----------------------------------------------------------------------------------------------------------#
#Age 20-64 -linear lag01
#복합표본분석-선형 회귀
svy.fita1<-svyglm(eGFR~lag01_PM25,data=hn.r,com.svy.age1);svy.fita2<-svyglm(eGFR~lag01_PM10,data=hn.r,com.svy.age1)
svy.fita3<-svyglm(eGFR~lag01_SO2,data=hn.r,com.svy.age1) ;svy.fita4<-svyglm(eGFR~lag01_NO2,data=hn.r,com.svy.age1)
svy.fita5<-svyglm(eGFR~lag01_CO,data=hn.r,com.svy.age1)  ;svy.fita6<-svyglm(eGFR~lag01_O3,data=hn.r,com.svy.age1)

svy.fitb1<-svyglm(eGFR~lag01_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb2<-svyglm(eGFR~lag01_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb3<-svyglm(eGFR~lag01_SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb4<-svyglm(eGFR~lag01_NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb5<-svyglm(eGFR~lag01_CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)
svy.fitb6<-svyglm(eGFR~lag01_O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)


#Age 65>= -linear lag01
svy.fita1<-svyglm(eGFR~lag01_PM25,data=hn.r,com.svy.age2);svy.fita2<-svyglm(eGFR~lag01_PM10,data=hn.r,com.svy.age2)
svy.fita3<-svyglm(eGFR~lag01_SO2,data=hn.r,com.svy.age2) ;svy.fita4<-svyglm(eGFR~lag01_NO2,data=hn.r,com.svy.age2)
svy.fita5<-svyglm(eGFR~lag01_CO,data=hn.r,com.svy.age2)  ;svy.fita6<-svyglm(eGFR~lag01_O3,data=hn.r,com.svy.age2)

svy.fitb1<-svyglm(eGFR~lag01_PM25+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb2<-svyglm(eGFR~lag01_PM10+age+sex+HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb3<-svyglm(eGFR~lag01_SO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb4<-svyglm(eGFR~lag01_NO2+age+sex +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb5<-svyglm(eGFR~lag01_CO+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)
svy.fitb6<-svyglm(eGFR~lag01_O3+age+sex  +HE_BMI+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2)

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$E      =(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)



#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#
#가중치 적용, 신장기능 지표,eGFR 로지스틱회귀
#Age 20-64 logistic
svy.fita1<-svyglm(eGFR_g~PM25,data=hn.r,com.svy.age1,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~PM10,data=hn.r,com.svy.age1,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~SO2,data=hn.r,com.svy.age1 ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~NO2,data=hn.r,com.svy.age1 ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~CO,data=hn.r,com.svy.age1  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~O3,data=hn.r,com.svy.age1  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)

#Age 65>= -logistic
svy.fita1<-svyglm(eGFR_g~PM25,data=hn.r,com.svy.age2,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~PM10,data=hn.r,com.svy.age2,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~SO2,data=hn.r,com.svy.age2 ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~NO2,data=hn.r,com.svy.age2 ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~CO,data=hn.r,com.svy.age2  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~O3,data=hn.r,com.svy.age2  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)


#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#
#가중치 적용, 신장기능 지표,eGFR 로지스틱회귀
#Age 20-64 logistic
svy.fita1<-svyglm(eGFR_g~lag1_PM25,data=hn.r,com.svy.age1,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~lag1_PM10,data=hn.r,com.svy.age1,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~lag1_SO2,data=hn.r,com.svy.age1 ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~lag1_NO2,data=hn.r,com.svy.age1 ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~lag1_CO,data=hn.r,com.svy.age1  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~lag1_O3,data=hn.r,com.svy.age1  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~lag1_PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~lag1_PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~lag1_SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~lag1_NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~lag1_CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~lag1_O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)

#Age 65>= -logistic
svy.fita1<-svyglm(eGFR_g~lag1_PM25,data=hn.r,com.svy.age2,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~lag1_PM10,data=hn.r,com.svy.age2,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~lag1_SO2,data=hn.r,com.svy.age2 ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~lag1_NO2,data=hn.r,com.svy.age2 ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~lag1_CO,data=hn.r,com.svy.age2  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~lag1_O3,data=hn.r,com.svy.age2  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~lag1_PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~lag1_PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~lag1_SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~lag1_NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~lag1_CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~lag1_O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)

#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#
#가중치 적용, 신장기능 지표,eGFR 로지스틱회귀
#Age 20-64 logistic
svy.fita1<-svyglm(eGFR_g~lag01_PM25,data=hn.r,com.svy.age1,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~lag01_PM10,data=hn.r,com.svy.age1,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~lag01_SO2,data=hn.r,com.svy.age1 ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~lag01_NO2,data=hn.r,com.svy.age1 ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~lag01_CO,data=hn.r,com.svy.age1  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~lag01_O3,data=hn.r,com.svy.age1  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~lag01_PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~lag01_PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~lag01_SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~lag01_NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~lag01_CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~lag01_O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age1,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)

#Age 65>= -logistic
svy.fita1<-svyglm(eGFR_g~lag01_PM25,data=hn.r,com.svy.age2,family="quasibinomial")
svy.fita2<-svyglm(eGFR_g~lag01_PM10,data=hn.r,com.svy.age2,family="quasibinomial")
svy.fita3<-svyglm(eGFR_g~lag01_SO2,data=hn.r,com.svy.age2 ,family="quasibinomial")
svy.fita4<-svyglm(eGFR_g~lag01_NO2,data=hn.r,com.svy.age2 ,family="quasibinomial")
svy.fita5<-svyglm(eGFR_g~lag01_CO,data=hn.r,com.svy.age2  ,family="quasibinomial")
svy.fita6<-svyglm(eGFR_g~lag01_O3,data=hn.r,com.svy.age2  ,family="quasibinomial")

svy.fitb1<-svyglm(eGFR_g~lag01_PM25+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb2<-svyglm(eGFR_g~lag01_PM10+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb3<-svyglm(eGFR_g~lag01_SO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb4<-svyglm(eGFR_g~lag01_NO2+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb5<-svyglm(eGFR_g~lag01_CO+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")
svy.fitb6<-svyglm(eGFR_g~lag01_O3+age+sex+HE_BMI+HE_sbp+factor(incm)+factor(edu)+HE_TG+factor(underlying_disease2)+factor(BD1_11)+factor(BS3_1),data=hn.r,com.svy.age2,family="quasibinomial")

s.result<-as.data.frame(rbind(summary(svy.fita1)$coeff[2,],summary(svy.fita2)$coeff[2,],
                              summary(svy.fita3)$coeff[2,],summary(svy.fita4)$coeff[2,],
                              summary(svy.fita5)$coeff[2,],summary(svy.fita6)$coeff[2,],
                              summary(svy.fitb1)$coeff[2,],summary(svy.fitb2)$coeff[2,],
                              summary(svy.fitb3)$coeff[2,],summary(svy.fitb4)$coeff[2,],
                              summary(svy.fitb5)$coeff[2,],summary(svy.fitb6)$coeff[2,]) %>% round(3))

s.result$model=rep(c("Crude model","Adjusted model"),each=6)
s.result$exposure=c("PM2.5","PM10","SO2","NO2","CO","O3")
s.result$uc      =c(10,10,1/1000,1/100,1/10,1/1000)
s.result$OR      =exp(s.result$Estimate*s.result$uc) %>% round(3)
s.result$lci     =exp(s.result$uc*(s.result$Estimate-1.96*s.result$`Std. Error`))%>% round(3)
s.result$uci     =exp(s.result$uc*(s.result$Estimate+1.96*s.result$`Std. Error`))%>% round(3)

View(s.result)
