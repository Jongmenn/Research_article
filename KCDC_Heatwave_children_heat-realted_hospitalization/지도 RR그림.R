#------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------#
#라이브러리
pacman::p_load(ggmap,raster,rgeos,maptools,rgdal,dplyr,lubridate,ggrepel)


rr.result<-read_excel("D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\KCDC_Heatwave_children_results.xlsx",sheet="TS_도시별_미세먼지보정")
rr.result<-rr.result %>% filter(exposure=="maxT")
rr.result$RR=exp(rr.result$Estimate)

rr.result$sido=ifelse(rr.result$sido==11,"서울",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==26,"부산",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==27,"대구",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==28,"인천",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==29,"광주",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==30,"대전",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==31,"울산",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==41,"경기",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==42,"강원",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==43,"충북",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==44,"충남",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==45,"전북",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==46,"전남",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==47,"경북",rr.result$sido)
rr.result$sido=ifelse(rr.result$sido==48,"경남",rr.result$sido)

effectsize<-rr.result %>% filter(exposure=="maxT" & gubun=="single")
# effectsize$lag=paste0("lag",1:8-1)

#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#지도
korea<-shapefile("D:\\EUMC\\지도\\CTPRVN_201602\\TL_SCCO_CTPRVN.shp")
# korea<-spTransform(korea,CRS("+proj=longlat"))
korea_map<-fortify(korea)

#지도
korea<-shapefile("D:\\EUMC\\지도\\CTPRVN_201405\\TL_SCCO_CTPRVN.shp")
korea_map<-fortify(korea)

effectsize$sido=with(effectsize,ifelse(sido=="강원",0 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="경기",1 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="경남",2 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="경북",3 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="서울",8 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="부산",7 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="대구",5 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="인천",11,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="광주",4 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="대전",6 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="세종",9 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="울산",10,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="전남",13,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="전북",12,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="제주",14,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="충남",15,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="충북",16,effectsize$sido))
effectsize$sido=as.numeric(effectsize$sido)


effectsize$RR=exp(effectsize$Estimate)

ifelse(korea_map$id=="0" ,subset(effectsize,lag==k & sido==0)$RR
       
       #PM 농도 겹쳐서 그리기 전체 지역  (연구 지역만 표현할 때)
       mapping_year_func<-function(k){
         korea_map$RR=NA
         
         korea_map$RR=ifelse(korea_map$id=="0" ,subset(effectsize,lag==k & sido==0)$RR  ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="1" ,subset(effectsize,lag==k & sido==1)$RR  ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="2" ,subset(effectsize,lag==k & sido==2)$RR  ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="3" ,subset(effectsize,lag==k & sido==3)$RR  ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="4" ,subset(effectsize,lag==k & sido==4)$RR  ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="5" ,subset(effectsize,lag==k & sido==5)$RR  ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="6" ,subset(effectsize,lag==k & sido==6)$RR  ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="7" ,subset(effectsize,lag==k & sido==7)$RR  ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="8" ,subset(effectsize,lag==k & sido==8)$RR  ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="9" ,subset(effectsize,lag==k & sido==9)$RR  ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="10",subset(effectsize,lag==k & sido==10)$RR ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="11",subset(effectsize,lag==k & sido==11)$RR ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="12",subset(effectsize,lag==k & sido==12)$RR ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="13",subset(effectsize,lag==k & sido==13)$RR ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="14",subset(effectsize,lag==k & sido==14)$RR ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="15",subset(effectsize,lag==k & sido==15)$RR ,korea_map$RR)
         korea_map$RR=ifelse(korea_map$id=="16",subset(effectsize,lag==k & sido==16)$RR ,korea_map$RR)
         
         korea_map$label=NA
         korea_map$label=ifelse(korea_map$id==0 ,"Gangwon-do"      ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==1 ,"Gyeonggi-do"     ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==2 ,"Gyeongsangnam-do",korea_map$label)
         korea_map$label=ifelse(korea_map$id==3 ,"Gyeongsangbuk-do",korea_map$label)
         korea_map$label=ifelse(korea_map$id==8 ,"Seoul"           ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==7 ,"Busan"           ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==5 ,"Daegu"           ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==11,"Incheon"         ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==4 ,"Gwangju"         ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==6 ,"Daejeon"         ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==10,"Ulsan"           ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==9 ,"Sejong"          ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==12,"Jeollanam-do"    ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==13,"Jeollabuk-do"    ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==14,"Jeju"            ,korea_map$label)
         korea_map$label=ifelse(korea_map$id==15,"Chungcheongnam-do",korea_map$label)
         korea_map$label=ifelse(korea_map$id==16,"Chungcheongbuk-do",korea_map$label)
         korea_map$label=factor(korea_map$label,levels=unique(korea_map$label))
         korea_map}
       
       #연도별 노출농도 붙이는 함수 적용 
       korea_map0<-mapping_year_func("lag0");
       korea_map1<-mapping_year_func("lag1");
       korea_map2<-mapping_year_func("lag2");
       korea_map3<-mapping_year_func("lag3");
       korea_map4<-mapping_year_func("lag4");
       korea_map5<-mapping_year_func("lag5");
       korea_map6<-mapping_year_func("lag6");
       korea_map7<-mapping_year_func("lag7");
       
       #단일년도 한해만 잡아서 지도 레이블 이용
       mht.cent <- korea_map0 %>% group_by(label) %>% summarize(long = median(long), lat = median(lat))
       mht.cent<-cbind(aggregate(korea_map0$long,list(korea_map0$label),median),
                       lat=aggregate(korea_map0$lat,list(korea_map0$label),median)$x)
       
       names(mht.cent)[1:2]=c("label","long")
       
       k1<-subset(korea_map0,id!=4);k2<-subset(korea_map0,id==4);korea_tmap0<-rbind(k1,k2)
       k1<-subset(korea_map1,id!=4);k2<-subset(korea_map1,id==4);korea_tmap1<-rbind(k1,k2)
       k1<-subset(korea_map2,id!=4);k2<-subset(korea_map2,id==4);korea_tmap2<-rbind(k1,k2)
       k1<-subset(korea_map3,id!=4);k2<-subset(korea_map3,id==4);korea_tmap3<-rbind(k1,k2)
       k1<-subset(korea_map4,id!=4);k2<-subset(korea_map4,id==4);korea_tmap4<-rbind(k1,k2)
       k1<-subset(korea_map5,id!=4);k2<-subset(korea_map5,id==4);korea_tmap5<-rbind(k1,k2)
       k1<-subset(korea_map6,id!=4);k2<-subset(korea_map6,id==4);korea_tmap6<-rbind(k1,k2)
       k1<-subset(korea_map7,id!=4);k2<-subset(korea_map7,id==4);korea_tmap7<-rbind(k1,k2)
       
       korea_tmap0$group=with(korea_tmap0,factor(group,levels=(unique(group))))
       korea_tmap1$group=with(korea_tmap1,factor(group,levels=(unique(group))))
       korea_tmap2$group=with(korea_tmap2,factor(group,levels=(unique(group))))
       korea_tmap3$group=with(korea_tmap3,factor(group,levels=(unique(group))))
       korea_tmap4$group=with(korea_tmap4,factor(group,levels=(unique(group))))
       korea_tmap5$group=with(korea_tmap5,factor(group,levels=(unique(group))))
       korea_tmap6$group=with(korea_tmap6,factor(group,levels=(unique(group))))
       korea_tmap7$group=with(korea_tmap7,factor(group,levels=(unique(group))))
       
       disease_map<-function(dataset,yl,yu,text1,text2){
         ggplot()+geom_polygon(data=dataset,aes(x=long,y=lat,group=group,fill=RR),col="black")+
           theme_minimal(base_size=15)+geom_text_repel(aes(label = label,x=long,y=lat),colour = "black", data = mht.cent, size =3.5,fontface = 'bold')+
           scale_fill_gradient(low="white",high="red",na.value="gray",limits=c(yl,yu),name=text1)+
           theme(axis.line = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),
                 legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                 panel.border = element_blank(),panel.background = element_blank())+
           labs(x="",y="",title = text2,plot.title = element_text(vjust = 0.5),size=20)}
       
       
       g0<-disease_map(korea_tmap0,0.95,1.06,"Relative risk","Lag0")
       g1<-disease_map(korea_tmap1,0.95,1.06,"Relative risk","Lag1")
       g2<-disease_map(korea_tmap2,0.95,1.06,"Relative risk","Lag2")
       g3<-disease_map(korea_tmap3,0.95,1.06,"Relative risk","Lag3")
       g4<-disease_map(korea_tmap4,0.95,1.06,"Relative risk","Lag4")
       g5<-disease_map(korea_tmap5,0.95,1.06,"Relative risk","Lag5")
       g6<-disease_map(korea_tmap6,0.95,1.06,"Relative risk","Lag6")
       g7<-disease_map(korea_tmap7,0.95,1.06,"Relative risk","Lag7")
       
       
       x11();grid.arrange(g0,g1,g2,g3,g4,g5,g6,g7,ncol=4)
       
       
       g8<-ggplot()+geom_polygon(data=korea_tmap0,aes(x=long,y=lat,group=group,fill=RR),col="black")+
         theme_minimal(base_size=15)+geom_text_repel(aes(label = label,x=long,y=lat),colour = "black", data = mht.cent, size =3.5,fontface = 'bold')+
         scale_fill_gradient(low="white",high="red",na.value="gray",limits=c(0.95,1.06),name="Relative Risk")+
         theme(axis.line = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),
               panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
               panel.border = element_blank(),panel.background = element_blank())+
         labs(x="",y="",title = "text2",plot.title = element_text(vjust = 0.5),size=20)
       x11();g8
       
       