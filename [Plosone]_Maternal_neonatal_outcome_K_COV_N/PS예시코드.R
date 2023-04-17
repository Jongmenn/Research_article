#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#library
pacman::p_load(optmatch,MatchIt,dplyr,tableone,ggplot2,
               geepack,lmtest,sandwich,Matching,lubridate,pROC)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#example dataset
dat<-MatchIt::lalonde

#re78, outcome variable
dat$outcome=ifelse(dat$re78>median(dat$re78),0,1)

fit<-glm(outcome~treat,data=dat,family="binomial")
GetConfInt(fit)

dat$race    =factor(dat$race)
dat$married =factor(dat$married)
dat$nodegree=factor(dat$nodegree)

#PS model
ps.mod<-glm(treat~age+educ+race+married+nodegree+re74+re75,family="binomial",data=dat)

ps<-predict(ps.mod,type="response")
dat$ps=ps

roc.mod<-roc(treat~ps,data=dat)
x11();plot(roc.mod,legacy.axes=T,cex.lab=1.4,cex.axis=1.4)

#------------------------------------------------------------------------------#
#Plot PS distribution by treatment groups
CreateDataset <- function(data = dat, ps = "ps", weights = "one") {
  ## Density for overall cohort
  dfDataAll      <-suppressWarnings(density(x = data[,ps], weights = data[,weights]))
  dfDataAll      <-data.frame(dfDataAll[c("x","y")])
  dfDataAll$group<-"All"
  
  ## Density for RHC
  dfData1 <-suppressWarnings(density(x = subset(data,treat==1)[,ps],weights=subset(data,treat==1)[,weights]))
  dfData1 <-data.frame(dfDataRhc[c("x","y")])
  dfData1$group<- "Treatment"
  
  ## Density for No RHC
  dfData0      <-suppressWarnings(density(x=subset(data,treat==0)[,ps],weights=subset(data,treat!=1)[,weights]))
  dfData0      <-data.frame(dfDataNoRhc[c("x","y")])
  dfData0$group<-"No Treatment"
  
  ## combine datasets
  dfData <- rbind(dfDataAll, dfData1, dfData0)
  
  ## Return
  dfData
}
CreateDataset(data=dat,ps="ps",weights = "one")

x11();ggplot(dat,aes(ps))+geom_density()



subset(dat,treat==1)$ps
# df1=data.frame(ps=dat$ps,group="All")
df1=data.frame(x=density(dat$ps)$x,
               y=density(dat$ps)$y,group="All")

df2=data.frame(x=density(subset(dat,treat==1)$ps)$x,
               y=density(subset(dat,treat==1)$ps)$y,group="Treatment")

df3=data.frame(x=density(subset(dat,treat==0)$ps)$x,
               y=density(subset(dat,treat==0)$ps)$y,group="No treatment")


ps.df<-rbind(df1,df2,df3)
ps.df$group=factor(ps.df$group)
head(ps.df)
x11();ggplot(ps.df,mapping = aes(x = x, y = y, color = group))+
  theme_bw(base_size=20)+scale_y_continuous(breaks = NULL) +
  geom_line(lwd=2)+
  scale_x_continuous(limit =c(-0.1,1.1),breaks=seq(c(0.1:1),by=0.1))+
  scale_color_manual(values=c("All"      ="#999999",
                              "Treatment"="#D55E00",
                              "No treatment"="#0072B2"))+
  labs(x="Propensity score",y="Distribution")


#Plot like mirror image
ps.df2<-subset(ps.df,group!="All")
ps.mirror<-within(ps.df2,{y[group!= "Treatment"]<- (y * -1)[group!= "Treatment"]})

x11();ggplot(ps.mirror,mapping = aes(x = x, y = y, color = group))+
  theme_bw(base_size=20)+scale_y_continuous(breaks = NULL) +
  geom_line(lwd=2)+geom_hline(yintercept=0,lwd=1)+
  scale_x_continuous(limit =c(-0.1,1.1),breaks=seq(c(0.1:1),by=0.1))+
  scale_color_manual(values=c("Treatment"="#D55E00",
                              "No treatment"="#0072B2"))+
  labs(x="Propensity score",y="Distribution")

x11();ggplot(ps.mirror,mapping = aes(x = x, y = y, color = group))+
  theme_bw(base_size=20)+scale_y_continuous(breaks = NULL) +
  geom_line(lwd=2)+geom_hline(yintercept=0,lwd=1)+
  scale_x_continuous(limit =c(-0.1,1.1),breaks=seq(c(0.1:1),by=0.1))+
  scale_color_manual(values=c("Treatment"="#D55E00",
                              "No treatment"="#0072B2"))+
  labs(x="Propensity score",y="Distribution")+coord_flip()

#PS adjustment
mod1<-glm(outcome~treat,data=dat,family="binomial")
mod2<-glm(outcome~treat+ps,data=dat,family="binomial")
mod3<-glm(outcome~treat+ps+I(ps^2),data=dat,family="binomial")

GetConfInt(mod1)
GetConfInt(mod2)
GetConfInt(mod3)

h1<-hist(subset(dat,treat==1)$ps)
h2<-hist(subset(dat,treat==0)$ps)



df2=data.frame(x=density(subset(dat,treat==1)$ps)$x,
               y=density(subset(dat,treat==1)$ps)$y,group="Treatment")

df3=data.frame(x=density(subset(dat,treat==0)$ps)$x,
               y=density(subset(dat,treat==0)$ps)$y,group="No treatment")

#------------------------------------------------------------------------------#
#Weighting

#IPTW(inverse probability of treatment assignment weights)/ ATE
dat$iptw=NA
#1/PS for treated & 1/(1-PS) for untreated
dat$iptw=ifelse(dat$treat==1,1/ps,1/(1-ps))

#SMRW(standardized mortality ratio weight) / ATT
dat$smrw=NA
dat$smrw=ifelse(dat$treat==1,1,ps/(1-ps))

#Numerator weight for stabilization
prob<-NA
dat$prob=ifelse(dat$treat==1,sum(dat$treat==1)/length(dat$treat),
                sum(dat$treat==0)/length(dat$treat))

#IPTW stabilized with marginal probabilities of assigned treatment
dat$iptwstab=with(dat,iptw*prob)

#Matching weights
dat$matchWeightNumerator<-pmin(ps,1-ps)
dat$matchWeight=dat$matchWeightNumerator*dat$iptw

dat %>% dplyr:: select(iptw,iptwstab,smrw,matchWeight) %>% summary
#------------------------------------------------------------------------------#
#glm에서 가중치 부여해서 계산한 값 (IPTW)이나
#svyglm design에서 모델링 한거나 결과값 동일 
glm.obj  <-glm(outcome~treat,weights=iptw,family="binomial",data=dat)
beta.iptw<-coef(glm.obj)
SE       <-sqrt(diag(vcovHC(glm.obj,type="HC0")))


exp(beta.iptw[2])
lci<-exp(beta.iptw[2]-1.96*SE[2])
uci<-exp(beta.iptw[2]+1.96*SE[2])
cbind(exp(beta.iptw[2]),lci,uci)


library(survey)
weighteddata<-svydesign(ids=~1,data=dat,weights=~iptw)
summary(weighteddata)
weighteddata$variables
weightedtable<-svyCreateTableOne(vars="married",strata="treat",
                                 data=weighteddata,test=F)
print(weightedtable,smd=T)

library(ipw)
iptw.model<-ipwpoint(exposure=treat,family="binomial",link="logit",
                     denominator=~age+educ+race+married+nodegree+re74+re75,data=dat)


# x11();ipwplot(weights=iptw.model$ipw.weights,logscale=F,main="weights",xlim=c(0,22))
# x11();ipwplot(weights = dat$iptw,logscale=F)

svyglm(outcome~treat,
       design=svydesign(ids=~1,data=dat,weights=~iptw),
       family="binomial")

#위에서 처럼 직접 계산안하고 바로 구하기 
library(sandwich)
library(lmtest)
coeftest(glm.obj,vcov=sandwich)

#SMR weight
smrw.model<-glm(outcome~treat,data=dat,weights=smrw,family="binomial")

coeftest(glm.obj   ,vcov=sandwich) #ATE, (IPTW)
coeftest(smrw.model,vcov=sandwich) #ATT, (SMR weights)

#------------------------------------------------------------------------------#
#PS matching, default options
#method: "nearest"(default), "optimal", "full","exact",...,
#estimated: ATT, "ATE","ATC"
#ratio: matching 몇대몇으로 할건지? ratio=1 (default, 1:1)
#greedy nearest matching
# 1:1 greedy NN matching on the PS
set.seed(1)
mdata1<-matchit(treat~age+educ+race+married+nodegree+re74+re75,
                method="nearest",estimated="ATT",
                data=dat,ratio=1,replacement=F,caliper=0.2)

#Optimal matching
set.seed(1)
mdata2<-matchit(treat~age+educ+race+married+nodegree+re74+re75,
                method="optimal",estimated="ATT",
                data=dat,ratio=1,replacement=F)

summary(mdata1)
summary(mdata2)
x11();plot(mdata1,type="jitter",interactive=F,cex.axis=1.4,cex.lab=1.5,cex.main=2,pch=1)
x11();plot(mdata2,type="jitter",interactive=F,cex.axis=1.4,cex.lab=1.5,cex.main=2,pch=1)

match.d1<-match.data(mdata1)
match.d2<-match.data(mdata2)

md1.1<-subset(match.d1,treat==1)
md1.0<-subset(match.d1,treat==0)

md2.1<-subset(match.d2,treat==1)
md2.0<-subset(match.d2,treat==0)

submd1.1<-data.frame(x=density(md1.1$ps)$x,y=density(md1.1$ps)$y,group="Treatment")
submd1.0<-data.frame(x=density(md1.0$ps)$x,y=density(md1.0$ps)$y,group="No treatment")

submd2.1<-data.frame(x=density(md2.1$ps)$x,y=density(md2.1$ps)$y,group="Treatment")
submd2.0<-data.frame(x=density(md2.0$ps)$x,y=density(md2.0$ps)$y,group="No treatment")

match.density1<-rbind(submd1.1,submd1.0)
match.density2<-rbind(submd2.1,submd2.0)
#------------------------------------------------------------------------------#
#PS plot (treatment vs no treatment)
x11();ggplot(match.density1,mapping = aes(x = x, y = y, color = group))+
  theme_bw(base_size=20)+scale_y_continuous(breaks = NULL) +
  geom_line(lwd=2)+
  scale_x_continuous(limit =c(-0.1,1.1),breaks=seq(c(0.1:1),by=0.1))+
  scale_color_manual(values=c("All"      ="#999999",
                              "Treatment"="#D55E00",
                              "No treatment"="#0072B2"))+
  labs(x="Propensity score",y="Distribution")

x11();ggplot(match.density2,mapping = aes(x = x, y = y, color = group))+
  theme_bw(base_size=20)+scale_y_continuous(breaks = NULL) +
  geom_line(lwd=2)+
  scale_x_continuous(limit =c(-0.1,1.1),breaks=seq(c(0.1:1),by=0.1))+
  scale_color_manual(values=c("All"      ="#999999",
                              "Treatment"="#D55E00",
                              "No treatment"="#0072B2"))+
  labs(x="Propensity score",y="Distribution")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

with(match.d1,table(outcome,treat))
with(dat,table(outcome,treat))

match.d1<-match.data(mdata1)
match.d1<-match.d1 %>% arrange(subclass)
match.d1$treat=factor(match.d1$treat)

geeglm(outcome~treat,corstr="exchangeable",id=subclass,data=match.d1)

head(match.d1)
geeglm(outcome~treat+age+educ+race+married+nodegree+re74+re75,id=subclass,
       family=binomial(link="logit"),
       corstr="exchangeable",
       scale.fix = F,
       data=match.d1)

geeglm(formula   = outcome ~ treat,
       family    = binomial(link = "logit"),
       id        = subclass,
       data      = match.d1,
       corstr    = "exchangeable",
       scale.fix = FALSE)

library(survival)
clogit(as.numeric(outcome)~treat+strata(subclass),data=match.d1)
glm(outcome~treat,family="binomial",data=match.d1)


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
mdata<-matchit(treat~age+factor(race)+factor(nodegree)+
                 re74+re75,
               method="nearest",estimated="ATT",
               data=dat,ratio=1,replacement=F,caliper=0.2)
summary(mdata)
x11();plot(mdata,type="jitter",interactive=F,
           cex.axis=1.4,cex.lab=1.5,cex.main=2,pch=1)

z<-match.data(mdata)

out<-CreateTableOne(vars=c("age","re74","re75","race","nodegree"),
                    data=z,
                    factorVars=c("race","nodegree"),
                    strata="treat") 
z2<-print(out,smd=T)
summary(out)
write.csv(z2,file="D:\\EXAMPLE.CSV",row.names=T,na="")

summary(mdata,standardize = T)

m1<-glm(married~factor(treat),
        family="binomial",data=dat)

m2<-glm(married~factor(treat)+age+factor(race)+factor(nodegree)+re74+re75,
        family="binomial",data=dat)

m1<-glm(married~factor(treat),
        family="binomial",data=dat)
summary(m1)
summary(m2)

#---------------------------------------------------------------#
#---------------------------------------------------------------#
mdata<-matchit(treat~age+factor(race)+factor(nodegree)+
                 re74+re78+re78,
               method="optimal",estimated="ATT",
               data=dat,ratio=1,replacement=F)


