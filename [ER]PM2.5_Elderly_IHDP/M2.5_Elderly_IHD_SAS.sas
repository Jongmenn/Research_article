/**********************************************************************************************************************************************************/
/*원자료 data 라이브러리 (append  자료) */
libname   RAW   '/userdata01/room009/data_source/user_data/181130B_서울대_미세먼지 인체건강영향평가_홍윤철(오종민)_E8542/analysis';  
libname   RAW2 '/userdata01/room009/data_source/user_data/181130B_서울대_미세먼지 인체건강영향평가_홍윤철(오종민)_E8542';  
libname   H1      '/userdata01/room009/가설1';      /*예전 자료 라이브러리*/
libname   H2      '/userdata01/room009/가설2';       /*예전 자료 라이브러리*/
libname   H3      '/userdata01/room009/가설3';       /*예전 자료 라이브러리*/
libname   A        '/userdata01/room009/오종민/IHD퇴원후사망'; 
/**********************************************************************************************************************************************************/

/****************************************/
/********IHD 환자 퇴원 후 사망***********/
/****************************************/

/*코드 새로 작성 */

*IHD 환자 정의 ;
%Macro IHD(TABLE1,TABLE2);
data A.&table1 ; set RAW.&table2;
IF FORM_CD IN("02","03") AND "I20" <= SUBSTR(SICK_SYM1,1,3)<="I25" THEN K1=2; ELSE K1=0; /*입원 이면서 주상병 STROKE 코드, K1은 주상병에 존재하면 2*/
IF FORM_CD IN("02","03") AND "I20" <= SUBSTR(SICK_SYM2,1,3)<="I25" THEN K2=1; ELSE K2=0; /*입원 이면서 부상병 STROKE 코드, K2은 부상병에 존재하면 1*/
ICD_RANK=K1+K2; IF ICD_RANK>0; /*ICD_RANK 주+부상병 중요도 순위 나타냄*/
DROP MCARE_SUBJ_CD HSPTZ_PATH_TYPE ED_RC_TOT_AMT EDC_SBA EDC_INSUR_BRDN_AMT DISP_SUBJ_TYPE SICK_SYM4 SICK_SYM5;
RUN;
%MEND IHD;
%IHD(TOTAL_IHD_08,T20_2008); %IHD(TOTAL_IHD_09,T20_2009)
%IHD(TOTAL_IHD_10,T20_2010); %IHD(TOTAL_IHD_11,T20_2011)
%IHD(TOTAL_IHD_12,T20_2012); %IHD(TOTAL_IHD_13,T20_2013)
%IHD(TOTAL_IHD_14,T20_2014); %IHD(TOTAL_IHD_15,T20_2015)
%IHD(TOTAL_IHD_16,T20_2016); %IHD(TOTAL_IHD_17,T20_2017)

/*자격이랑 MERGE KEY 생성 : PKEY*/
DATA A.Total_IHD; SET A.TOTAL_IHD_08 A.TOTAL_IHD_09 A.TOTAL_IHD_10 A.TOTAL_IHD_11 A.TOTAL_IHD_12 A.TOTAL_IHD_13 A.TOTAL_IHD_14
                     A.TOTAL_IHD_15 A.TOTAL_IHD_16 A.TOTAL_IHD_17; RUN;
DATA A.TOTAL_IHD;SET A.TOTAL_IHD;  PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4)) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO);RUN;
/**********************************************************************************************************************************************************/

/*자격 새로 정의 2008~2017*/
/*7대 도시 시군구, 65세 이상 연령, 보험비 분류 1~4 */
/*성별, 보험비, 시도 결측 제외 */
DATA A.BFC_REV;
SET RAW.BFC;
IF WKPLC_RVSN_CTRB_PT_TYPE_CD ^=".";
IF WKPLC_RVSN_CTRB_PT_TYPE_CD <=5 THEN INCOME="INC1" ;
IF WKPLC_RVSN_CTRB_PT_TYPE_CD >=6   & WKPLC_RVSN_CTRB_PT_TYPE_CD<=10 THEN INCOME="INC2" ;
IF WKPLC_RVSN_CTRB_PT_TYPE_CD >=11 & WKPLC_RVSN_CTRB_PT_TYPE_CD<=15 THEN INCOME="INC3" ;
IF WKPLC_RVSN_CTRB_PT_TYPE_CD >=16 THEN INCOME="INC4";
SIDO=SUBSTR(RVSN_ADDR_CD,1,2);

PKEY=COMPRESS(STD_YYYY)||COMPRESS("-")||COMPRESS(INDI_DSCM_NO);

IF SIDO IN (11,26,27,28,29,30,31);
IF YEND_STD_AGE >=65;
IF SEX_TYPE^="";
RENAME SEX_TYPE=SEX;
RENAME RVSN_ADDR_CD=SGG;
RENAME YEND_STD_AGE=AGE;
DROP CLFY_CD JIKYEOK_DTL_CD JIKYEOK_DTL_CD INDTP_CD  ;RUN;
PROC SORT DATA= A.BFC_REV; BY INDI_DSCM_NO;RUN;
PROC FREQ DATA= A.BFC_REV; TABLES INCOME   ;RUN;
/**********************************************************************************************************************************************************/

/*2008년부터 65세 이상 이면서 7대 도시 거주에 해당하지 않는 사람  */
/*중간에  타지역 이사는 배제하기 위해 (자료가 7대 도시 정보로 추출해서)*/
DATA A.BASE_NOT_ID; SET RAW.BFC;
SIDO=SUBSTR(RVSN_ADDR_CD,1,2);
IF SIDO NOT IN (11,26,27,28,29,30,31); 
IF YEND_STD_AGE >=65;
IF SEX_TYPE^=""; 
KEEP INDI_DSCM_NO  SIDO; RUN;

PROC SORT DATA= A.BASE_NOT_ID NODUPKEY OUT=A.BASE_NOT_ID; BY INDI_DSCM_NO; RUN;
DATA A.BASE_NOT_ID; SET A.BASE_NOT_ID;
K=0; KEEP INDI_DSCM_NO K; RUN;

/*7대 도시 외에 시도 이동한 경우 제외 */
PROC SQL; CREATE TABLE A.TOTAL_IHD  AS SELECT * FROM A.TOTAL_IHD AS A LEFT JOIN A.BASE_NOT_ID AS B ON A.INDI_DSCM_NO = B.INDI_DSCM_NO; QUIT;
DATA A.TOTAL_IHD2; SET A.TOTAL_IHD; IF K^=0;  DROP K; RUN;

/*자격이랑  MERGE BY : PKEY*/
PROC SQL; CREATE TABLE A.TOTAL_IHD3  AS SELECT * FROM A.TOTAL_IHD2,  A.BFC_REV   WHERE TOTAL_IHD2.PKEY   = BFC_REV.PKEY;QUIT;
PROC FREQ DATA=A.TOTAL_IHD3; TABLES STD_YYYY/LIST; RUN;

/**********************************************************************************************************************************************************/
/*STEP 1 : 데이터 클리닝 */
DATA A.TOTAL_IHD4; SET A.TOTAL_IHD3;
IF INDI_DSCM_NO="" THEN DELETE;     /*ID 무효 제외*/
IF FORM_CD IN ("02","03");                 /*무효한 진료 형태 제외*/ 
/*무효한 진료일자 제외*/
IF "2008" <= SUBSTR(MDCARE_STRT_DT,1,4) <="2017" AND "01" <=SUBSTR(MDCARE_STRT_DT,5,2) <="12" AND "01" <= SUBSTR(MDCARE_STRT_DT,7,2) <="31"; 
/*무효한 최초 진료일자 제외*/
IF "1899" <= SUBSTR(FST_HSPTZ_DT,1,4)     <="2017" AND "01" <=SUBSTR(FST_HSPTZ_DT,5,2)      <="12" AND "01" <= SUBSTR(FST_HSPTZ_DT,7,2) <="31"      
THEN FST_HSPTZ_DT=FST_HSPTZ_DT; ELSE FST_HSPTZ_DT=""; /*무효한 입내원 일수 제외*/
IF VSHSP_DD_CNT="" THEN DELETE;
IF VSHSP_DD_CNT=0 THEN VSHSP_DD_CNT=1; /*입내원 일수 0인 경우 입원을 했다가 검진 진료로 마침 0=>1 코딩*/
IF 0 <=AGE <=130 ;                      /*무효 나이 제외*/
IF SEX IN ("1","2");                         /*성별 무효 제외*/
IF SGG="" THEN DELETE; RUN;       /*시군구 무효 제외*/
/**********************************************************************************************************************************************************/

/*STEP 2 : 필요변수 추출 ,진료개시일자 계산  */
DATA A.TOTAL_IHD4; SET A.TOTAL_IHD4;
FORMAT MDCARE FST MDCARE_DATE FST_DATE DATE1 YYMMDD10.;

MDCARE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
FST      =MDY(SUBSTR(FST_HSPTZ_DT,5,2),SUBSTR(FST_HSPTZ_DT,7,2),SUBSTR(FST_HSPTZ_DT,1,4));
IF FST^="" THEN FST_STATUS=1; ELSE FST_STATUS=0;
/*진료개시일자 계산*/
IF MDCARE="" THEN MDCARE_DATE=FST; ELSE MDCARE_DATE=MDCARE;
IF FST^=""      THEN FST_DATE=FST;        ELSE FST_DATE=MDCARE;

DATE1=MIN(FST_DATE,MDCARE_DATE);         /*진료 개시일자*/
DIFF_PLUS=MDCARE_DATE-DATE1;               /*최초로 진료 받은 일 - 입원일*/
CNT_DD=DIFF_PLUS+VSHSP_DD_CNT; RUN; /*요양일 계산*/
/**********************************************************************************************************************************************************/

/*STEP3 : 입내원 일수 계산 /  데이터 정렬 */
PROC SORT DATA=A.TOTAL_IHD4 ; BY INDI_DSCM_NO MDCARE_DATE CNT_DD SICK_SYM1; RUN; 

/*진료일과 개인아이디로 새로운 KEY 만듬(중복제거위해)*/
DATA A.TOTAL_IHD4 ; SET A.TOTAL_IHD4; DKEY=COMPRESS(MDCARE_DATE) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO); RUN;
/*데이터 정렬 : 1) 날짜+개별 ID 고려 2) 입내원 일수 내림 차순 , 3) 주+부상병 순위 내림 차순*/
PROC SORT DATA=A.TOTAL_IHD4; BY DKEY DESCENDING CNT_DD DESCENDING ICD_RANK; RUN; 

/*위 정렬한 데이터 에서 날짜+개별 아이디 기준으로 첫행이 아니면 제외*/
DATA A.TOTAL_IHD5; SET A.TOTAL_IHD4; BY DKEY; IF FIRST.DKEY^=1 THEN DELETE; DROP DKEY ICD_RANK; RUN;
proc sort data=A.TOTAL_IHD5; by indi_dscm_no MDCARE_DATE ;run; /*데이터 정렬  ID, 진료 개시일 순*/

/**********************************************************************************************************************************************************/
/*STEP4 : EPISODE 계산 */
DATA A.TOTAL_IHD6; 
FORMAT R START_DATE DATE1_DISCHARGE YYMMDD10.;
RETAIN R D START_DATE MONEY; SET A.TOTAL_IHD5; BY INDI_DSCM_NO;
IF FIRST.INDI_DSCM_NO=1 AND LAST.INDI_DSCM_NO=1 THEN DO;
IKEEP=1; R=DATE1+CNT_DD-1; D=CNT_DD; START_DATE=DATE1; MONEY=ED_RC_TOT_AMT; END; ELSE DO;
IF FIRST.INDI_DSCM_NO=1 AND LAST.INDI_DSCM_NO^=1 THEN DO;
IKEEP=1;  R=DATE1+CNT_DD-1; D=CNT_DD; START_DATE=DATE1; MONEY=ED_RC_TOT_AMT; END; ELSE DO;
K=DATE1-R;  /*연속된 입내원 일수를 마지막 퇴원한 날짜-최초 입원 날짜로 계산*/
IF K<=2 THEN DO; IKEEP=0; 
IF DATE1+CNT_DD-1 <R THEN D=D;
IF DATE1+CNT_DD-1 <R THEN MONEY=MONEY+ED_RC_TOT_AMT; ELSE DO;
R=DATE1+CNT_DD-1; D=R-START_DATE+1;
END; END; ELSE DO;
IKEEP=1;  R=DATE1+CNT_DD-1; D=CNT_DD; START_DATE=DATE1;
MONEY=ED_RC_TOT_AMT; END; END; END;
DATE1_DISCHARGE=DATE1+CNT_DD-1; RUN;

/*sort by id and descending MDCARE_date*/
PROC SORT DATA=A.TOTAL_IHD6; BY INDI_DSCM_NO DESCENDING MDCARE_DATE DESCENDING CNT_DD; RUN;

/*PUT MAX VALUE FOR IKEEP=0*/
DATA A.TOTAL_IHD7;
FORMAT DISCHARGEDATE YYMMDD10.; RETAIN MAXD; SET A.TOTAL_IHD6; BY INDI_DSCM_NO;
IF FIRST.INDI_DSCM_NO=1 AND IKEEP=0 THEN MAXD=D; ELSE DO;;
IF FIRST.INDI_DSCM_NO=1 AND IKEEP=1 THEN MAXD=0;
ELSE DO; IF IKEEP=0 THEN DO; MAXD=MAX(D,MAXD); END; ELSE DO; MAXD=0; END;END;END;
/*keep the first events record information with the total inpatient days from the last event*/
IKEEP2=LAG(IKEEP);
IF FIRST.INDI_DSCM_NO=1 THEN ILOGKEEP=2; ELSE DO;
IF IKEEP2=0 THEN ILOGKEEP=1;
ELSE ILOGKEEP=2; END; D2=LAG(MAXD);
IF IKEEP=1 AND ILOGKEEP=2 THEN D=CNT_DD; ELSE DO;
IF IKEEP=1 AND ILOGKEEP=1 THEN D=D2; END;
IF IKEEP2=1;
DISCHARGEDATE=START_DATE+D-1;
DROP R MAXD IKEEP IKEEP2 ILOGKEEP D2 K CNT_DD D DATE1_DISCHARGE ;RUN;
/**********************************************************************************************************************************************************/

/*STEP 5 사망일자 붙히기 */
/*연구기간 2008-2017 로 산출하고 노출 값 붙힐때는(2008-2016까지만 붙이기)*/
/*가장 마지막 퇴원일의 하나만 남기기 */

PROC SORT DATA=A.TOTAL_IHD7; BY INDI_DSCM_NO START_DATE; RUN;

DATA A.TOTAL_IHD7; SET A.TOTAL_IHD7;
YEAR=YEAR(DISCHARGEDATE); 
drop ED_RC_TOT_AMT money; run;

/*2008년 기준으로 연령이 65세 이상인자격 */
DATA B_AG_ID08 ; SET A.BFC_REV; IF STD_YYYY=2008 & AGE>=65 ;KEEP INDI_DSCM_NO AGE STD_YYYY; RUN;
DATA B_AG_ID09 ; SET A.BFC_REV; IF STD_YYYY=2009 & AGE>=66 ;KEEP INDI_DSCM_NO AGE STD_YYYY; RUN;
DATA B_AG_ID10 ; SET A.BFC_REV; IF STD_YYYY=2010 & AGE>=67 ;KEEP INDI_DSCM_NO AGE STD_YYYY; RUN;
DATA B_AG_ID11 ; SET A.BFC_REV; IF STD_YYYY=2011 & AGE>=68 ;KEEP INDI_DSCM_NO AGE STD_YYYY; RUN;
DATA B_AG_ID12 ; SET A.BFC_REV; IF STD_YYYY=2012 & AGE>=69 ;KEEP INDI_DSCM_NO AGE STD_YYYY; RUN;
DATA B_AG_ID13 ; SET A.BFC_REV; IF STD_YYYY=2013 & AGE>=70 ;KEEP INDI_DSCM_NO AGE STD_YYYY; RUN;
DATA B_AG_ID14 ; SET A.BFC_REV; IF STD_YYYY=2014 & AGE>=71 ;KEEP INDI_DSCM_NO AGE STD_YYYY; RUN;
DATA B_AG_ID15 ; SET A.BFC_REV; IF STD_YYYY=2015 & AGE>=72 ;KEEP INDI_DSCM_NO AGE STD_YYYY; RUN;
DATA B_AG_ID16 ; SET A.BFC_REV; IF STD_YYYY=2016 & AGE>=73 ;KEEP INDI_DSCM_NO AGE STD_YYYY; RUN;
DATA B_AG_ID17 ; SET A.BFC_REV; IF STD_YYYY=2017 & AGE>=74 ;KEEP INDI_DSCM_NO AGE STD_YYYY; RUN;

DATA A.B_AG_ID; SET B_AG_ID08 B_AG_ID09 B_AG_ID10 B_AG_ID11 B_AG_ID12 B_AG_ID13 B_AG_ID14
                                       B_AG_ID15 B_AG_ID16 B_AG_ID17; RUN;
PROC SORT DATA= A.B_AG_ID; BY INDI_DSCM_NO STD_YYYY; RUN;
PROC SORT DATA= A.B_AG_ID NODUPKEY OUT=A.B_AG_ID_U; BY INDI_DSCM_NO; RUN;
PROC FREQ DATA=A.B_AG_ID; TABLES AGE; RUN;

proc sql; create table a.total_IHD8 as select * from a.total_ihd7 where indi_dscm_no in (select INDI_DSCM_NO FROM B_AG_ID); quit;

/*ID 순으로, 마지막 퇴원일 내림차순 (마지막 퇴원) */
PROC SORT DATA=A.TOTAL_IHD8; BY INDI_DSCM_NO DESCENDING DISCHARGEDATE;RUN; 

DATA A.TOTAL_IHD9; SET A.TOTAL_IHD8;
BY INDI_DSCM_NO; IF FIRST.INDI_DSCM_NO=1;
DROP SICK_SYM3  K1 K2 MDCARE_DATE FST_DATE  FST_STATUS DIFF_PLUS ;RUN; /*변수 제거 */

PROC SQL;  CREATE TABLE A.TOTAL_IHD10 AS SELECT * FROM A.TOTAL_IHD9 as A LEFT JOIN RAW.TG_DTH AS B ON A.INDI_DSCM_NO = B.INDI_DSCM_NO; QUIT;

proc freq data=a.total_ihd10; tables year; run;
/**********************************************************************************************************************************************************/

PROC SQL; CREATE TABLE A.TOTAL_IHD11 AS SELECT * FROM A.TOTAL_IHD10 as A LEFT JOIN RAW.final_cod_db AS B ON A.INDI_DSCM_NO = B.INDI_DSCM_NO; QUIT;

/*2008~2016까지만 */
DATA A.TOTAL_IHD12; SET A.TOTAL_IHD11;
IF DTH_CODE_NEW2="";
IF  SUBSTR(PUT(DISCHARGEDATE,yymmddn8.),1,4)<=2016; RUN;

/***********************************************************************************************************/

/*STEP6 EVENT ALL-cause mortality(사망원인 연계전)*/

/**생존 : 퇴원 ~ 2016-12-31 , 사망 : 퇴원~사망시점 **/

/*2017년  이후 또는 2007년 미만 사망일자 미싱값, EVENT는 1 ,Non-event 는 0 */
DATA TOTAL_IHD13; SET A.TOTAL_IHD12;
CD=PUT(DISCHARGEDATE,yymmddn8.);  /*퇴원한 년월일 명시 */
DTH_Y     =SUBSTR(DTH_ASSMD_DT,1,4); /*년월만 계산하기위한 변수 만듬 */
IF DTH_Y^=" " THEN EVENT=1; ELSE EVENT=0; /*사망이면 1, 아니면 0*/
IF DTH_ASSMD_DT^=. THEN EVENT=1; ELSE EVENT=0; /* 모든원인 사망이면 1, 아니면 0*/
IF DTH_Y   =2017   THEN DTH_ASSMD_DT=. ; ELSE DTH_ASSMD_DT=EVENT;
IF DTH_Y >=2017   THEN EVENT=0; ELSE EVENT=EVENT; /*사망년월이 연구기간내에 해당하지 않으면 절단 => EVENT=0*/

IF EVENT=1 AND SUBSTR(DTH_CODE_NEW1,1,1)="I"                                                                       THEN CVD_death=1; ELSE CVD_death=0;
IF EVENT=1 AND SUBSTR(DTH_CODE_NEW1,1,3) IN ('I60','I61','I62','I63','I64','I65','I66','I67','I68','I69') THEN Stroke_death=1; ELSE Stroke_death=0;
IF EVENT=1 AND SUBSTR(DTH_CODE_NEW1,1,3) IN ('I60','I61','I62')                                                 THEN H_stroke_death=1;ELSE H_stroke_death=0;
IF EVENT=1 AND SUBSTR(DTH_CODE_NEW1,1,3) IN ('I63','I64','I65','I66')                                          THEN I_stroke_death=1;ELSE I_stroke_death=0;
IF EVENT=1 AND SUBSTR(DTH_CODE_NEW1,1,3) IN ('I20','I21','I22','I23','I24','I25')                            THEN IHD_death=1; ELSE IHD_death=0;

/*사망날짜가 퇴원날짜보다 빠른경우 (즉, 사망하고나서 퇴원하는 경우)-> 이런 경우에는 퇴원날짜를 사망날짜로 변경*/
CAL_YM=SUBSTR(DTH_ASSMD_DT,1,6); /*사망 년월*/
IF EVENT=1 & CAL_YM<SUBSTR(CD,1,6) THEN CD=DTH_ASSMD_DT; ELSE CD=CD;

/*ADJ_YMD 변수 생성 ; 퇴원시점 부터 생존 시간 계산 */
ADJ_Y=SUBSTR(CD,1,4);  /*퇴원한 년*/
ADJ_M=SUBSTR(CD,5,2); /*퇴원한 월*/
CAL_Y =SUBSTR(CAL_YM,1,4);  /*사망한 년*/
CAL_M=SUBSTR(CAL_YM,5,2); /*사망한 월*/

IF EVENT=1 & CAL_YM <SUBSTR(CD,1,6) THEN ADJ_Y =CAL_Y ; ELSE ADJ_Y=ADJ_Y;
IF EVENT=1 & CAL_YM <SUBSTR(CD,1,6) THEN ADJ_M=CAL_M; ELSE ADJ_M=ADJ_M;

IF EVENT=0 THEN T_Y=2016-ADJ_Y ;  /*EVENT 0 이면 연구기간 마지막년도에서 퇴원년도 빼줌*/
IF EVENT=0 THEN T_M=12-ADJ_M+1; /*EVENT 0 이면 연구기간 마지막 월에서 퇴원년월 빼줌 +1 (같은 월에 퇴원은 1)*/

IF EVENT=1 & ADJ_M-CAL_M <=0 THEN T_Y=CAL_Y-ADJ_Y;                    /*EVENT 1 이고 퇴원한 월 이 사망 월 보다 같거나 작으면 사망년도 - 퇴원년도 빼줌*/
IF EVENT=1 & ADJ_M-CAL_M <=0 THEN T_M=CAL_M-ADJ_M+1;             /*EVENT 1 이고 퇴원한 월 이 사망 월 보다 같거나 작거나 같으면 사망년월 - 퇴원년월 + 1*/
IF EVENT=1 & ADJ_M-CAL_M   >0 THEN T_Y=CAL_Y-ADJ_Y;                    /*EVENT 1 이고 퇴원한 월 이 사망 월 보다 크면 사망년도- 퇴원년도 빼줌*/
IF EVENT=1 & ADJ_M-CAL_M   >0 THEN T_M=12+CAL_M-ADJ_M+1;         /*EVENT 1 이고 퇴원한 월 이 사망 월 보다 크면 사망년월+12 - 퇴원년월 +1*/

TIME=T_Y*12+T_M; /*TIME ; MIN-MAX (1-120)*/

IF EVENT=1 THEN  KEY=COMPRESS(SUBSTR(DTH_ASSMD_DT,1,6))|| COMPRESS("-") || COMPRESS(SGG);
IF EVENT=0 THEN  KEY=COMPRESS("201612-")||COMPRESS(SGG) ; /*노출 값에 맞춰서 절단 시점 붙임*/

TKEY=COMPRESS(SUBSTR(CD,1,6)) || COMPRESS("-") || COMPRESS(SGG); /* Time varying 노출값이랑 연계할 KEY ; 년월+시군구*/RUN;

PROC FREQ DATA= A.TOTAL_IHD13; TABLES TIME; RUN;
PROC FREQ DATA=A.TOTAL_IHD13; TABLES EVENT; RUN; /*전체 사망 시점에대해서 구해줌 */

PROC FREQ DATA=A.TOTAL_IHD13; TABLES EVENT;RUN;
PROC FREQ DATA=A.TOTAL_IHD13; TABLES CVD_DEATH;RUN;
PROC FREQ DATA=A.TOTAL_IHD13; TABLES STROKE_DEATH;RUN;
PROC FREQ DATA=A.TOTAL_IHD13; TABLES IHD_DEATH;RUN;

/*EVENT  에 따라  변경 */
DATA A.TOT_ALL; SET A.TOTAL_IHD13; RUN;
DATA A.TOT_CVD; SET A.TOTAL_IHD13;
IF event=1 & cvd_death=1 THEN EVENT=1; ELSE EVENT=0; RUN;
DATA A.TOT_STROKE; SET A.TOTAL_IHD13;
IF event=1 & STROKE_death=1 THEN EVENT=1; ELSE EVENT=0; RUN;
DATA A.TOT_IHD; SET A.TOTAL_IHD13;
IF event=1 & IHD_death=1 THEN EVENT=1; ELSE EVENT=0; RUN;

PROC FREQ DATA= A.TOT_CVD; TABLES EVENT; RUN;
PROC FREQ DATA= A.TOT_STROKE; TABLES EVENT; RUN;
PROC FREQ DATA= A.TOT_IHD; TABLES EVENT; RUN;

/*시군구별 통계치*/
DATA A.SGG_STAT; SET H1.SGG_STAT; RUN;

/*자격은 제일 빠른 년도 기준으로 정보 만들기 */
 PROC SORT DATA=A.BFC_REV NODUPKEY OUT=A.BFC_REV2; BY INDI_DSCM_NO; QUIT;

DATA A.BFC_REV3; SET A.BFC_REV2;
AGE_YY= STD_YYYY;
KEEP INDI_DSCM_NO SEX AGE_YY AGE INCOME GAIBJA_TYPE; RUN;

/*Competing risk data set*/
DATA A.TOT_ALL; SET A.TOTAL_IHD13; RUN;

/*-----------------------------------------------------------------------------------------------------------------------*/
/*Competing risk , Fine & Grays models로 볼 때 */
data a.COM_all; set a.tot_all ;
if event=1 & cvd_death=1    then c_event1=1; if event=1 & cvd_death=0    then c_event1=2; if event=0 then c_event1=0;
if event=1 & stroke_death=1 then c_event2=1; if event=1 & stroke_death=0 then c_event2=2; if event=0 then c_event2=0;
if event=1 & ihd_death=1     then c_event3=1; if event=1 & ihd_death=0     then c_event3=2; if event=0 then c_event3=0;
run;

proc freq data=a.COM_all; tables c_event1; run;
proc freq data=a.COM_all; tables c_event2; run;
proc freq data=a.COM_all; tables c_event3; run;

/*SGG STATISTICS MERGE*/
PROC SQL; CREATE TABLE A.COM_ALL AS SELECT * FROM A.COM_ALL , A.SGG_STAT WHERE COM_ALL.SGG = SGG_STAT.CODE; QUIT;

/*사망 시점에 대해서 노출 연계*/
PROC SQL; CREATE TABLE A.COM_ALL2 AS SELECT * FROM A.COM_ALL, A.PM25 WHERE COM_ALL.KEY =PM25.KEY; QUIT;
DATA A.COM_ALL2; SET A.COM_ALL2;
WKEY=SUBSTR(TKEY,1,9); RUN;
PROC SQL; CREATE TABLE A.COM_ALL2 AS SELECT * FROM A.COM_ALL2, A.WEA WHERE COM_ALL2.WKEY=WEA.KEY; QUIT;

/*퇴원 시점에 대해서 노출 연계*/
PROC SQL; CREATE TABLE A.COM_ALL3 AS SELECT * FROM A.COM_ALL, A.PM25 WHERE COM_ALL.TKEY =PM25.KEY; QUIT;

DATA A.COM_ALL3; SET A.COM_ALL3;
WKEY=SUBSTR(TKEY,1,9); RUN;

PROC SQL; CREATE TABLE A.COM_ALL3 AS SELECT * FROM A.COM_ALL3, A.WEA WHERE COM_ALL3.WKEY=WEA.KEY; QUIT;


PROC PHREG DATA=a.COM_ALL2 ;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*EVENT(0)= PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag0_mtemp Lag0_mhumi  ; RUN;

PROC PHREG DATA=a.COM_ALL2 ;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*EVENT(0)= LAG05_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag0_mtemp Lag0_mhumi  ; RUN;
PROC PHREG DATA=a.COM_ALL3 ;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*EVENT(0)= LAG05_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag0_mtemp Lag0_mhumi  ; RUN;

PROC PHREG DATA=a.COM_ALL3 ;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*EVENT(0)= PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag0_mtemp Lag0_mhumi  ; RUN;

/****************************************************************************/
/*사망 시점에 대해서 modeling*/
/*Competing risk model event CVD*/
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT1(0)= PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag0_mtemp Lag0_mhumi  / eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT1(0)= LAG01_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag01_mtemp Lag01_mhumi / eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT1(0)= LAG02_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag02_mtemp Lag02_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT1(0)= LAG03_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag03_mtemp Lag03_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT1(0)= LAG04_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag04_mtemp Lag04_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT1(0)= LAG05_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag05_mtemp Lag05_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT1(0)= LAG06_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag06_mtemp Lag06_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT1(0)= LAG011_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag011_mtemp Lag011_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT1(0)= LAG023_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag023_mtemp Lag023_mhumi/ eventcode=1 ; RUN;

/*Competing risk model event Stroke*/
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT2(0)= PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag0_mtemp Lag0_mhumi  / eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT2(0)= LAG01_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag01_mtemp Lag01_mhumi / eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT2(0)= LAG02_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag02_mtemp Lag02_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT2(0)= LAG03_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag03_mtemp Lag03_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT2(0)= LAG04_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag04_mtemp Lag04_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT2(0)= LAG05_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag05_mtemp Lag05_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT2(0)= LAG06_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag06_mtemp Lag06_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT2(0)= LAG011_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag011_mtemp Lag011_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT2(0)= LAG023_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag023_mtemp Lag023_mhumi/ eventcode=1 ; RUN;



/*Competing risk model event IHD*/
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT3(0)= PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag0_mtemp Lag0_mhumi  / eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT3(0)= LAG01_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag01_mtemp Lag01_mhumi / eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT3(0)= LAG02_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag02_mtemp Lag02_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT3(0)= LAG03_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag03_mtemp Lag03_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT3(0)= LAG04_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag04_mtemp Lag04_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT3(0)= LAG05_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag05_mtemp Lag05_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT3(0)= LAG06_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag06_mtemp Lag06_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT3(0)= LAG011_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag011_mtemp Lag011_mhumi/ eventcode=1 ; RUN;
PROC PHREG DATA=a.COM_ALL3 plots(overlay=stratum)=cif;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL TIME*C_EVENT3(0)= LAG023_PM25 SEX AGE INCOME SIDO SMOKING high_edu  income_month Lag023_mtemp Lag023_mhumi/ eventcode=1 ; RUN;


/*summary */
PROC FREQ DATA=A.COM_ALL3; TABLES AGE; RUN;
PROC FREQ DATA=A.COM_ALL3; TABLES SEX; RUN;
PROC FREQ DATA=A.COM_ALL3; TABLES INCME; RUN;
PROC FREQ DATA=A.COM_ALL3; TABLES SIDO; RUN;
PROC FREQ DATA=A.COM_ALL3; TABLES SGG; RUN;
PROC MEANS DATA=A.COM_ALL3 N MEAN MEDIAN STDDEV P1 P5 P10 P25 P50 P75 P90 P95 P99  MIN MAX; VAR AGE  ; RUN;
PROC MEANS DATA=A.COM_ALL3 N MEAN MEDIAN STDDEV P1 P5 P10 P25 P50 P75 P90 P95 P99  MIN MAX; VAR SMOKING ; RUN;
PROC MEANS DATA=A.COM_ALL3 N MEAN MEDIAN STDDEV P1 P5 P10 P25 P50 P75 P90 P95 P99  MIN MAX; VAR high_edu ; RUN;
PROC MEANS DATA=A.COM_ALL3 N MEAN MEDIAN STDDEV P1 P5 P10 P25 P50 P75 P90 P95 P99  MIN MAX; VAR  income_month ; RUN;

/***************************************************************************************************************************************/
/*Time varying sas macro*/
%macro cpdata(DATA=, TIME=, EVENT=, OUTDATA=); * Seperate event variable from list of censoring;
%LOCAL  CLIST;
%LET CLIST  = %SCAN(&EVENT., 2, "( )");
%LET EVENT = %SCAN(&EVENT., 1, "( )") ;

* REMOVE NON-UNIQUE EVENT TIMES;
PROC SORT DATA= &DATA. (WHERE =(&EVENT. ^in(&CLIST.))) OUT=&OUTDATA. (KEEP=&TIME.) NODUPKEY; BY &TIME. ; RUN;

*CREATE A LIST OF ALL TIME INTERVALS;
*Only include intervals where an event occurs;
DATA &OUTDATA.; SET &OUTDATA. (RENAME =(&TIME. = &TIME.1));
DROP TEMP ;  RETAIN TEMP 0 ;

&TIME.0 = TEMP;
TEMP    = &TIME.1; RUN;

* EXPAND CURRENT DATA SET;
%LET CLIST = %SCAN(&CLIST.,1);
DATA &OUTDATA.; SET &DATA.;

_TEMP_ = &EVENT.;

DROP &TIME. _TEMP_;
DO _POINT_ = 1 TO _NOBS_;

SET &OUTDATA. POINT = _POINT_ NOBS = _NOBS_;
* If current time beyond subjects event time , do not read ;
IF (&TIME. < &TIME.1) THEN LEAVE;
*Determine if current time is the subjects event time ;
IF (&TIME. ^= &TIME.1) THEN &EVENT. = &CLIST.;
IF (&TIME. = &TIME.1) THEN &EVENT. = _TEMP_; 
OUTPUT; END; RUN ;

%MEND CPDATA;

%MACRO TV(TABLE);
DATA A.VAR_DF ; SET SURV;
YY=2008;
MM=1;
T=MM+TIME0;
VAR_Y=FLOOR((T-0.00001)/12); 
VAR_Y2=YY+VAR_Y;
PKEY=COMPRESS(VAR_Y2)||COMPRESS("-")||COMPRESS(INDI_DSCM_NO);
IF MOD(T,12)=0 THEN VAR_M=12 ; ELSE  VAR_M=MOD(T,12);
IF VAR_M<10 THEN VAR_M2=COMPRESS("0") || COMPRESS(VAR_M) ; ELSE VAR_M2=VAR_M;
TIME=TIME1;  RUN;

/*자격 MERGE*/
PROC SQL ; CREATE TABLE A.VAR_DF2 AS SELECT * FROM A.VAR_DF, A.BFC_REV3 WHERE VAR_DF.INDI_DSCM_NO =BFC_REV3.INDI_DSCM_NO;QUIT;

DATA A.VAR_DF3; SET A.VAR_DF2;
TKEY=COMPRESS(VAR_Y2) || COMPRESS(VAR_M2) || COMPRESS("-") || COMPRESS(SGG);
DROP  MM VAR_Y SGG TIME1  T ;RUN;

/*PM MERGE*/
PROC SQL; CREATE TABLE a.&table. AS SELECT * FROM A.VAR_DF3, A.PM25 WHERE VAR_DF3.TKEY=PM25.KEY;QUIT;

PROC SORT DATA =a.&TABLE.; BY INDI_DSCM_NO TIME TKEY; RUN;
 %MEND ;

 /****************************************************************************************************************************/
 /****************************************************************************************************************************/
/*DROP VARIABLE*/
DATA A.TOT_ALL; SET A.TOT_ALL;
DROP DISCHARGEDATE START_DATE SUJIN_POTM_YOYANG_ADDR_CD FORM_CD VSHSP_DD_CNT 
        MDCARE_DD_CNT FST DATE1 MDCARE GAIBJA_TYPE  ; RUN;
DATA A.TOT_CVD; SET A.TOT_CVD;
DROP DISCHARGEDATE START_DATE SUJIN_POTM_YOYANG_ADDR_CD FORM_CD VSHSP_DD_CNT 
        MDCARE_DD_CNT FST DATE1 MDCARE GAIBJA_TYPE  ; RUN;
DATA A.TOT_STROKE; SET A.TOT_STROKE;
DROP DISCHARGEDATE START_DATE SUJIN_POTM_YOYANG_ADDR_CD FORM_CD VSHSP_DD_CNT 
        MDCARE_DD_CNT FST DATE1 MDCARE GAIBJA_TYPE  ; RUN;
DATA A.TOT_IHD; SET A.TOT_IHD;
DROP DISCHARGEDATE START_DATE SUJIN_POTM_YOYANG_ADDR_CD FORM_CD VSHSP_DD_CNT 
        MDCARE_DD_CNT FST DATE1 MDCARE GAIBJA_TYPE  ; RUN;

/*AGE GROUP , BASELINE*/
DATA A.TOT_ALL      ; SET A.TOT_ALL; IF AGE<74 THEN AGE_GROUP=0; else AGE_GROUP=1; RUN;
DATA A.TOT_CVD      ; SET A.TOT_CVD; IF AGE<74 THEN AGE_GROUP=0; else AGE_GROUP=1; RUN;
DATA A.TOT_STROKE; SET A.TOT_STROKE; IF AGE<74 THEN AGE_GROUP=0; else AGE_GROUP=1; RUN;
DATA A.TOT_IHD       ; SET A.TOT_IHD; IF AGE<74 THEN AGE_GROUP=0; else AGE_GROUP=1; RUN;

/*연령그룹별 event*/
PROC FREQ DATA=A.TOT_ALL; TABLES AGE_GROUP; RUN;
PROC FREQ DATA=A.TOT_ALL; TABLES AGE_GROUP*EVENT/LIST; RUN;
PROC FREQ DATA=A.TOT_CVD; TABLES AGE_GROUP*EVENT/LIST; RUN;
PROC FREQ DATA=A.TOT_STROKE; TABLES AGE_GROUP*EVENT/LIST; RUN;
PROC FREQ DATA=A.TOT_IHD; TABLES AGE_GROUP*EVENT/LIST; RUN;

/*Make time-varying data set  */
%cpdata(DATA=A.TOT_ALL       ,TIME=TIME, EVENT=EVENT(0), OUTDATA=SURV); %TV(TV_ALL);
%cpdata(DATA=A.TOT_CVD      , TIME=TIME, EVENT=EVENT(0), OUTDATA=SURV); %TV(TV_CVD);
%cpdata(DATA=A.TOT_STROKE, TIME=TIME, EVENT=EVENT(0), OUTDATA=SURV); %TV(TV_STROKE);
%cpdata(DATA=A.TOT_IHD       , TIME=TIME, EVENT=EVENT(0), OUTDATA=SURV); %TV(TV_IHD);

DATA A.TV_ALL; SET A.TV_ALL;
DROP SIGUNGUDATE income_month CODE pop_65 code2 high_edu smoking standard_death_65 ; RUN;

/*시군구 변수 만들어주기*/
DATA A.TV_ALL       ; SET A.TV_ALL; SGG=SUBSTR(TKEY,8,5)      ; RUN;
DATA A.TV_CVD      ; SET A.TV_CVD; SGG=SUBSTR(TKEY,8,5)      ; RUN;
DATA A.TV_STROKE; SET A.TV_STROKE; SGG=SUBSTR(TKEY,8,5); RUN;
DATA A.TV_IHD       ; SET A.TV_IHD; SGG=SUBSTR(TKEY,8,5)       ; RUN;

/*시군구 통계치  MERGE*/
proc sql; create table  A.TV_ALL        AS SELECT * FROM A.TV_ALL       AS A LEFT JOIN A.SGG_STAT AS B ON A.SGG = B.CODE; QUIT;
proc sql; create table  A.TV_CVD       AS SELECT * FROM A.TV_CVD       AS A LEFT JOIN A.SGG_STAT AS B ON A.SGG = B.CODE; QUIT;
proc sql; create table  A.TV_STROKE AS SELECT * FROM A.TV_STROKE AS A LEFT JOIN A.SGG_STAT AS B ON A.SGG = B.CODE; QUIT;
proc sql; create table  A.TV_IHD        AS SELECT * FROM A.TV_IHD        AS A LEFT JOIN A.SGG_STAT AS B ON A.SGG = B.CODE; QUIT;

/*TIME-VARYING AGE 계산*/
DATA A.TV_ALL       ; SET A.TV_ALL      ; DIFF_YY=SUBSTR(TKEY,1,4)-AGE_YY; TAGE=AGE+DIFF_YY; WKEY=SUBSTR(TKEY,1,9); RUN;
DATA A.TV_CVD      ; SET A.TV_CVD      ; DIFF_YY=SUBSTR(TKEY,1,4)-AGE_YY; TAGE=AGE+DIFF_YY; WKEY=SUBSTR(TKEY,1,9); RUN;
DATA A.TV_STROKE; SET A.TV_STROKE; DIFF_YY=SUBSTR(TKEY,1,4)-AGE_YY; TAGE=AGE+DIFF_YY; WKEY=SUBSTR(TKEY,1,9); RUN;
DATA A.TV_IHD       ; SET A.TV_IHD       ; DIFF_YY=SUBSTR(TKEY,1,4)-AGE_YY; TAGE=AGE+DIFF_YY; WKEY=SUBSTR(TKEY,1,9); RUN;

/*TIME-VARYING 정렬 */
proc sort data=a.tv_all         ; by indi_dscm_no tkey; run;
proc sort data=a.tv_CVD      ; by indi_dscm_no tkey; run;
proc sort data=a.tv_STROKE; by indi_dscm_no tkey; run;
proc sort data=a.tv_IHD       ; by indi_dscm_no tkey; run;

/*TIME-VARYING 기상변수 */
PROC SQL; CREATE TABLE A.TV_ALL        AS SELECT * FROM A.TV_ALL       AS A LEFT JOIN A.WEA AS B ON A.WKEY=B.KEY;QUIT;
PROC SQL; CREATE TABLE A.TV_CVD       AS SELECT * FROM A.TV_CVD       AS A LEFT JOIN A.WEA AS B ON A.WKEY=B.KEY;QUIT;
PROC SQL; CREATE TABLE A.TV_STROKE AS SELECT * FROM A.TV_STROKE AS A LEFT JOIN A.WEA AS B ON A.WKEY=B.KEY;QUIT;
PROC SQL; CREATE TABLE A.TV_IHD        AS SELECT * FROM A.TV_IHD        AS A LEFT JOIN A.WEA AS B ON A.WKEY=B.KEY;QUIT;

/****************************************************************************************************************************/
/****************************************************************************************************************************/
/*TIME-VARYING Data set event table */
PROC FREQ DATA=A.TV_ALL       ; TABLES event;RUN;
PROC FREQ DATA=A.TV_ALL       ; TABLES TAGE;RUN;
PROC FREQ DATA=A.TV_CVD      ; TABLES event;RUN;
PROC FREQ DATA=A.TV_STROKE; TABLES event;RUN;
PROC FREQ DATA=A.TV_IHD       ; TABLES event;RUN;

/*Drop variable*/
DATA A.TV_ALL; SET A.TV_ALL;
DROP MDCARE_STRT_DT FST_HSPTZ_DT CD DTH_Y CAL_YM ADJ_Y ADJ_M CAL_Y CAL_M 
          T_Y T_M VAR_Y2 VAR_M VAR_M2 GAIBJA_TYPE H_STROKE_DEATH I_STROKE_DEATH; RUN;
DATA A.TV_CVD; SET A.TV_CVD;
DROP MDCARE_STRT_DT FST_HSPTZ_DT CD DTH_Y CAL_YM ADJ_Y ADJ_M CAL_Y CAL_M 
         T_Y T_M VAR_Y2 VAR_M VAR_M2 GAIBJA_TYPE H_STROKE_DEATH I_STROKE_DEATH; RUN;
DATA A.TV_STROKE; SET A.TV_STROKE;
DROP MDCARE_STRT_DT FST_HSPTZ_DT CD DTH_Y CAL_YM ADJ_Y ADJ_M CAL_Y CAL_M 
         T_Y T_M VAR_Y2 VAR_M VAR_M2 GAIBJA_TYPE H_STROKE_DEATH I_STROKE_DEATH; RUN;
DATA A.TV_IHD; SET A.TV_IHD;
DROP MDCARE_STRT_DT FST_HSPTZ_DT CD DTH_Y CAL_YM ADJ_Y ADJ_M CAL_Y CAL_M 
         T_Y T_M VAR_Y2 VAR_M VAR_M2 GAIBJA_TYPE H_STROKE_DEATH I_STROKE_DEATH; RUN;

/*Sorting time varying data set */
PROC SORT DATA=a.TV_ALL       ; BY INDI_DSCM_NO STD_YYYY; RUN;
PROC SORT DATA=a.TV_CVD      ; BY INDI_DSCM_NO STD_YYYY; RUN;
PROC SORT DATA=a.TV_STROKE; BY INDI_DSCM_NO STD_YYYY; RUN;
PROC SORT DATA=a.TV_IHD       ; BY INDI_DSCM_NO STD_YYYY; RUN;

/*******************************************************************************************************/
/**Crude model  반대로 유의함  HR 0.9대 **/
%MACRO result0(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= PM25 ;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("PM25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;

%MACRO result1(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag01_PM25;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag01_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;

%MACRO result2(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag02_PM25;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag02_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result3(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag03_PM25;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag03_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result4(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag04_PM25;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag04_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result5(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag05_PM25;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag05_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result6(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag06_PM25;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag06_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result11(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag011_PM25;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag011_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result23(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag023_PM25;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag023_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result35(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag035_PM25;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE4; IF parameter in("lag035_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;

%result0 (tv_ALL,R0); %result1 (tv_ALL,R1)   ; %result2 (tv_ALL,R2); 
%result3 (tv_ALL,R3); %result4 (tv_ALL,R4)   ; %result5 (tv_ALL,R5);
%result6 (tv_ALL,R6); %result11 (tv_ALL,R11); %result23 (tv_ALL,R23);
%result35 (tv_ALL,R35);

DATA A.C_ALL ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="ALL-cause"; model="Crude"; RUN;

%result0 (tv_CVD,R0); %result1 (tv_CVD,R1)   ; %result2 (tv_CVD,R2); 
%result3 (tv_CVD,R3); %result4 (tv_CVD,R4)   ; %result5 (tv_CVD,R5);
%result6 (tv_CVD,R6); %result11 (tv_CVD,R11); %result23 (tv_CVD,R23);
%result35 (tv_CVD,R35);

DATA A.C_CVD ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="CVD-cause"; model="Crude"; RUN;

/*******************************************************************************************************/
/*보정변수: 성별, 연령, 보험료, 시도, 시군구 통계치(흡연율, 교육 수준, 월별 수입) + 기상변수 (기온, 습도)*/
%MACRO result0(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= PM25 SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag0_mtemp Lag0_mhumi ;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("PM25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;

%MACRO result1(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag01_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag01_mtemp Lag01_mhumi;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag01_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;

%MACRO result2(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag02_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag02_mtemp Lag02_mhumi;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag02_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result3(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag03_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag03_mtemp Lag03_mhumi;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag03_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result4(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag04_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag04_mtemp Lag04_mhumi;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag04_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result5(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag05_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag05_mtemp Lag05_mhumi;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag05_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result6(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag06_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag06_mtemp Lag06_mhumi;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag06_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result11(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag011_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag011_mtemp Lag011_mhumi;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag011_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result23(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag023_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag023_mtemp Lag023_mhumi;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE2; IF parameter in("lag023_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;
%MACRO result35(table,M);
PROC PHREG DATA=a.&table.;
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
MODEL (TIME0,TIME)*EVENT(0)= lag035_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag035_mtemp Lag035_mhumi;
ods output Modelinfo=RE0 ;               ods output CensoredSummary=RE1;
ods output ParameterEstimates=RE2;   ods output FitStatistics=RE3; run;

DATA RE4; SET RE4; IF parameter in("lag035_pm25"); DROP label ; run;
proc transpose data=RE3 OUT=trans_result;  id criterion; var withcovariates; run;
data a.&m. ; merge RE1 RE4 trans_result; run; %MEND ;

%result0 (tv_ALL,R0); %result1 (tv_ALL,R1)   ; %result2 (tv_ALL,R2); 
%result3 (tv_ALL,R3); %result4 (tv_ALL,R4)   ; %result5 (tv_ALL,R5);
%result6 (tv_ALL,R6); %result11 (tv_ALL,R11); %result23 (tv_ALL,R23);
%result35 (tv_ALL,R35);

DATA A.ALL ; SET A.ALL_R0 A.ALL_R1 A.ALL_R2 A.ALL_R3 A.ALL_R4 
                          A.ALL_R5 A.ALL_R6 A.ALL_R11 A.ALL_R23 A.ALL_R35;   
LABEL="ALL-cause"; RUN;

%result0 (tv_CVD  ,R0);  %result1 (tv_CVD  ,R1); %result2 (tv_CVD  ,R2);  
%result3 (tv_CVD  ,R3);  %result4 (tv_CVD  ,R4);  %result5 (tv_CVD  ,R5);
%result6 (tv_CVD  ,R6);  %result11 (tv_CVD,R11); %result23 (tv_CVD,R23); 
%result35 (tv_CVD,R35);

DATA A.CVD ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="CVD"; RUN;

%result0 (tv_STROKE  ,R0);  %result1 (tv_STROKE  ,R1);  %result2 (tv_STROKE  ,R2);  
%result3 (tv_STROKE  ,R3);  %result4 (tv_STROKE  ,R4);  %result5 (tv_STROKE  ,R5);
%result6 (tv_STROKE  ,R6);  %result11 (tv_STROKE,R11); %result23 (tv_STROKE,R23); 
%result35 (tv_STROKE,R35);

DATA A.STROKE ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="STROKE"; RUN;

%result0 (tv_IHD  ,R0);  %result1 (tv_IHD  ,R1);  %result2 (tv_IHD  ,R2);  
%result3 (tv_IHD  ,R3);  %result4 (tv_IHD  ,R4);  %result5 (tv_IHD  ,R5);
%result6 (tv_IHD  ,R6);  %result11 (tv_IHD,R11); %result23 (tv_IHD,R23); 
%result35 (tv_IHD,R35);

DATA A.IHD ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="IHD"; RUN;

/************************************************************************************************/
/*성별 층화 */
DATA A.TV_ALL_SEX_M      ; SET A.TV_ALL       ; IF SEX IN (1); RUN;
DATA A.TV_ALL_SEX_F       ; SET A.TV_ALL       ; IF SEX IN (2); RUN;
DATA A.tv_CVD_SEX_M      ; SET A.TV_CVD      ; IF SEX IN (1); RUN;
DATA A.tv_CVD_SEX_F       ; SET A.TV_CVD      ; IF SEX IN (2); RUN;
DATA A.tv_STROKE_SEX_M; SET A.TV_STROKE; IF SEX IN (1); RUN;
DATA A.tv_STROKE_SEX_F ; SET A.TV_STROKE; IF SEX IN (2); RUN;
DATA A.tv_IHD_SEX_M       ; SET A.TV_IHD      ; IF SEX IN (1); RUN;
DATA A.tv_IHD_SEX_F        ; SET A.TV_IHD      ; IF SEX IN (2); RUN;

%result0 (tv_ALL_SEX_M  ,R0);  %result1 (tv_ALL_SEX_M  ,R1); %result2 (tv_ALL_SEX_M  ,R2);  
%result3 (tv_ALL_SEX_M  ,R3);  %result4 (tv_ALL_SEX_M  ,R4);  %result5 (tv_ALL_SEX_M  ,R5);
%result6 (tv_ALL_SEX_M  ,R6);  %result11 (tv_ALL_SEX_M,R11); %result23 (tv_ALL_SEX_M,R23); 
%result35 (tv_ALL_SEX_M,R35);

DATA A.ALL_SEX_M ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="ALL_SEX_M"; RUN;

%result0 (tv_ALL_SEX_F  ,R0);  %result1 (tv_ALL_SEX_F  ,R1); %result2 (tv_ALL_SEX_F  ,R2);  
%result3 (tv_ALL_SEX_F  ,R3);  %result4 (tv_ALL_SEX_F  ,R4);  %result5 (tv_ALL_SEX_F  ,R5);
%result6 (tv_ALL_SEX_F  ,R6);  %result11 (tv_ALL_SEX_F,R11); %result23 (tv_ALL_SEX_F,R23); 
%result35 (tv_ALL_SEX_F,R35);

DATA A.ALL_SEX_F ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="ALL_SEX_F"; RUN;

%result0 (tv_CVD_SEX_M  ,R0);  %result1 (tv_CVD_SEX_M  ,R1); %result2 (tv_CVD_SEX_M  ,R2);  
%result3 (tv_CVD_SEX_M  ,R3);  %result4 (tv_CVD_SEX_M  ,R4);  %result5 (tv_CVD_SEX_M  ,R5);
%result6 (tv_CVD_SEX_M  ,R6);  %result11 (tv_CVD_SEX_M,R11); %result23 (tv_CVD_SEX_M,R23); 
%result35 (tv_CVD_SEX_M,R35);

DATA A.CVD_SEX_M ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="CVD_SEX_M"; RUN;

%result0 (tv_CVD_SEX_F  ,R0);  %result1 (tv_CVD_SEX_F  ,R1); %result2 (tv_CVD_SEX_F  ,R2);  
%result3 (tv_CVD_SEX_F  ,R3);  %result4 (tv_CVD_SEX_F  ,R4);  %result5 (tv_CVD_SEX_F  ,R5);
%result6 (tv_CVD_SEX_F  ,R6);  %result11 (tv_CVD_SEX_F,R11); %result23 (tv_CVD_SEX_F,R23); 
%result35 (tv_CVD_SEX_F,R35);

DATA A.CVD_SEX_F ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="CVD_SEX_F"; RUN;

%result0 (tv_STROKE_SEX_M  ,R0);  %result1 (tv_STROKE_SEX_M  ,R1); %result2 (tv_STROKE_SEX_M  ,R2);  
%result3 (tv_STROKE_SEX_M  ,R3);  %result4 (tv_STROKE_SEX_M  ,R4);  %result5 (tv_STROKE_SEX_M  ,R5);
%result6 (tv_STROKE_SEX_M  ,R6);  %result11 (tv_STROKE_SEX_M,R11); %result23 (tv_STROKE_SEX_M,R23); 
%result35 (tv_STROKE_SEX_M,R35);

DATA A.STROKE_SEX_M ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="STROKE_SEX_M"; RUN;

%result0 (tv_STROKE_SEX_F  ,R0);  %result1 (tv_STROKE_SEX_F  ,R1); %result2 (tv_STROKE_SEX_F  ,R2);  
%result3 (tv_STROKE_SEX_F  ,R3);  %result4 (tv_STROKE_SEX_F  ,R4);  %result5 (tv_STROKE_SEX_F  ,R5);
%result6 (tv_STROKE_SEX_F  ,R6);  %result11 (tv_STROKE_SEX_F,R11); %result23 (tv_STROKE_SEX_F,R23); 
%result35 (tv_STROKE_SEX_F,R35);

DATA A.STROKE_SEX_F ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="STROKE_SEX_F"; RUN;

%result0 (tv_IHD_SEX_M  ,R0);  %result1 (tv_IHD_SEX_M  ,R1); %result2 (tv_IHD_SEX_M  ,R2);  
%result3 (tv_IHD_SEX_M  ,R3);  %result4 (tv_IHD_SEX_M  ,R4);  %result5 (tv_IHD_SEX_M  ,R5);
%result6 (tv_IHD_SEX_M  ,R6);  %result11 (tv_IHD_SEX_M,R11); %result23 (tv_IHD_SEX_M,R23); 
%result35 (tv_IHD_SEX_M,R35);

DATA A.IHD_SEX_M ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="IHD_SEX_M"; RUN;

%result0 (tv_IHD_SEX_F  ,R0);  %result1 (tv_IHD_SEX_F  ,R1); %result2 (tv_IHD_SEX_F  ,R2);  
%result3 (tv_IHD_SEX_F  ,R3);  %result4 (tv_IHD_SEX_F  ,R4);  %result5 (tv_IHD_SEX_F  ,R5);
%result6 (tv_IHD_SEX_F  ,R6);  %result11 (tv_IHD_SEX_F,R11); %result23 (tv_IHD_SEX_F,R23); 
%result35 (tv_IHD_SEX_F,R35);

DATA A.IHD_SEX_F ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;   
LABEL="IHD_SEX_F"; RUN;

/************************************************************************************************/
/*연령 층화 */
DATA A.TV_ALL_AG0; SET A.TV_ALL; IF  AGE_GROUP=0;RUN; 
DATA A.TV_ALL_AG1; SET A.TV_ALL; IF  AGE_GROUP=1;RUN;

DATA A.TV_CVD_AG0; SET A.TV_CVD; IF  AGE_GROUP=0;RUN; 
DATA A.TV_CVD_AG1; SET A.TV_CVD; IF  AGE_GROUP=1;RUN;

DATA A.TV_STROKE_AG0; SET A.TV_STROKE; IF  AGE_GROUP=0;RUN; 
DATA A.TV_STROKE_AG1; SET A.TV_STROKE; IF  AGE_GROUP=1;RUN;

DATA A.TV_IHD_AG0; SET A.TV_IHD; IF  AGE_GROUP=0;RUN; 
DATA A.TV_IHD_AG1; SET A.TV_IHD; IF  AGE_GROUP=1;RUN;

/*Age group, All cause*/
%result0 (tv_ALL_AG0,R0); %result1 (tv_ALL_AG0,R1)   ; %result2 (tv_ALL_AG0,R2); 
%result3 (tv_ALL_AG0,R3); %result4 (tv_ALL_AG0,R4)   ; %result5 (tv_ALL_AG0,R5);
%result6 (tv_ALL_AG0,R6); %result11 (tv_ALL_AG0,R11); %result23 (tv_ALL_AG0,R23); %result35 (tv_ALL_AG0,R35);
DATA A.ALL_AG0 ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;  LABEL="ALL-cause"; RUN;

%result0 (tv_ALL_AG1,R0); %result1 (tv_ALL_AG1,R1)   ; %result2 (tv_ALL_AG1,R2); 
%result3 (tv_ALL_AG1,R3); %result4 (tv_ALL_AG1,R4)   ; %result5 (tv_ALL_AG1,R5);
%result6 (tv_ALL_AG1,R6); %result11 (tv_ALL_AG1,R11); %result23 (tv_ALL_AG1,R23); %result35 (tv_ALL_AG1,R35);
DATA A.ALL_AG1 ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;  LABEL="ALL-cause"; RUN;

/*Age group, CVD*/
%result0 (tv_CVD_AG0,R0); %result1 (tv_CVD_AG0,R1)   ; %result2 (tv_CVD_AG0,R2); 
%result3 (tv_CVD_AG0,R3); %result4 (tv_CVD_AG0,R4)   ; %result5 (tv_CVD_AG0,R5);
%result6 (tv_CVD_AG0,R6); %result11 (tv_CVD_AG0,R11); %result23 (tv_CVD_AG0,R23); %result35 (tv_CVD_AG0,R35);
DATA A.CVD_AG0 ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;  LABEL="CVD-cause"; RUN;

%result0 (tv_CVD_AG1,R0); %result1 (tv_CVD_AG1,R1)   ; %result2 (tv_CVD_AG1,R2); 
%result3 (tv_CVD_AG1,R3); %result4 (tv_CVD_AG1,R4)   ; %result5 (tv_CVD_AG1,R5);
%result6 (tv_CVD_AG1,R6); %result11 (tv_CVD_AG1,R11); %result23 (tv_CVD_AG1,R23); %result35 (tv_CVD_AG1,R35);
DATA A.CVD_AG1 ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;  LABEL="CVD-cause"; RUN;

/*Age group, Stroke*/
%result0 (tv_Stroke_AG0,R0); %result1 (tv_Stroke_AG0,R1)   ; %result2 (tv_Stroke_AG0,R2); 
%result3 (tv_Stroke_AG0,R3); %result4 (tv_Stroke_AG0,R4)   ; %result5 (tv_Stroke_AG0,R5);
%result6 (tv_Stroke_AG0,R6); %result11 (tv_Stroke_AG0,R11); %result23 (tv_Stroke_AG0,R23); %result35 (tv_Stroke_AG0,R35);
DATA A.Stroke_AG0 ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;  LABEL="Stroke-cause"; RUN;

%result0 (tv_Stroke_AG1,R0); %result1 (tv_Stroke_AG1,R1)   ; %result2 (tv_Stroke_AG1,R2); 
%result3 (tv_Stroke_AG1,R3); %result4 (tv_Stroke_AG1,R4)   ; %result5 (tv_Stroke_AG1,R5);
%result6 (tv_Stroke_AG1,R6); %result11 (tv_Stroke_AG1,R11); %result23 (tv_Stroke_AG1,R23); %result35 (tv_Stroke_AG1,R35);
DATA A.Stroke_AG1 ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;  LABEL="Stroke-cause"; RUN;

/*Age group, IHD*/
%result0 (tv_IHD_AG0,R0); %result1 (tv_IHD_AG0,R1)   ; %result2 (tv_IHD_AG0,R2); 
%result3 (tv_IHD_AG0,R3); %result4 (tv_IHD_AG0,R4)   ; %result5 (tv_IHD_AG0,R5);
%result6 (tv_IHD_AG0,R6); %result11 (tv_IHD_AG0,R11); %result23 (tv_IHD_AG0,R23); %result35 (tv_IHD_AG0,R35);
DATA A.IHD_AG0 ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;  LABEL="IHD-cause"; RUN;

%result0 (tv_IHD_AG1,R0); %result1 (tv_IHD_AG1,R1)   ; %result2 (tv_IHD_AG1,R2); 
%result3 (tv_IHD_AG1,R3); %result4 (tv_IHD_AG1,R4)   ; %result5 (tv_IHD_AG1,R5);
%result6 (tv_IHD_AG1,R6); %result11 (tv_IHD_AG1,R11); %result23 (tv_IHD_AG1,R23); %result35 (tv_IHD_AG1,R35);
DATA A.IHD_AG1 ; SET A.R0 A.R1 A.R2 A.R3 A.R4 A.R5 A.R6 A.R11 A.R23 A.R35;  LABEL="IHD-cause"; RUN;


/*Time distribution (If, EVENT=1)*/
data z1; set a.tot_all; if EVENT=1; run;
data z2; set a.tot_CVD; if EVENT=1; run;
data z3; set a.tot_stroke; if EVENT=1; run;
data z4; set a.tot_IHD; if EVENT=1; run;

PROC FREQ DATA=Z1; TABLES TIME; RUN;
PROC FREQ DATA=Z2; TABLES TIME; RUN;
PROC FREQ DATA=Z3; TABLES TIME; RUN;
PROC FREQ DATA=Z4; TABLES TIME; RUN;
PROC MEANS DATA=Z1 N MEAN MEDIAN STDDEV P1 P5 P10 P25 P50 P75 P90 P95 P99  MIN MAX; VAR TIME  ; RUN;
PROC MEANS DATA=Z2 N MEAN MEDIAN STDDEV P1 P5 P10 P25 P50 P75 P90 P95 P99  MIN MAX; VAR TIME  ; RUN;
PROC MEANS DATA=Z3 N MEAN MEDIAN STDDEV P1 P5 P10 P25 P50 P75 P90 P95 P99  MIN MAX; VAR TIME  ; RUN;
PROC MEANS DATA=Z4 N MEAN MEDIAN STDDEV P1 P5 P10 P25 P50 P75 P90 P95 P99  MIN MAX; VAR TIME  ; RUN;

PROC SORT DATA=A.TV_ALL      ; BY INDI_DSCM_NO STD_YYYY;RUN;
PROC SORT DATA=A.TV_CVD      ; BY INDI_DSCM_NO STD_YYYY;RUN;
PROC SORT DATA=A.TV_STROKE; BY INDI_DSCM_NO STD_YYYY;RUN;
PROC SORT DATA=A.TV_IHD       ; BY INDI_DSCM_NO STD_YYYY;RUN;


/*Exposure-response curve , 노출값(6-month, 12-month,24-month 고려)*/
PROC SORT DATA=A.TV_ALL NODUPKEY OUT=TEMP1 ; BY Lag05_PM25; RUN;
PROC SORT DATA=A.TV_ALL NODUPKEY OUT=TEMP2 ; BY Lag011_PM25; RUN;
PROC SORT DATA=A.TV_ALL NODUPKEY OUT=TEMP3 ; BY Lag023_PM25; RUN;

PROC SORT DATA=A.TV_CVD NODUPKEY OUT=TEMP1 ; BY Lag05_PM25; RUN;
PROC SORT DATA=A.TV_CVD NODUPKEY OUT=TEMP2 ; BY Lag011_PM25; RUN;
PROC SORT DATA=A.TV_CVD NODUPKEY OUT=TEMP3 ; BY Lag023_PM25; RUN;

PROC SORT DATA=A.TV_stroke NODUPKEY OUT=TEMP1 ; BY Lag05_PM25; RUN;
PROC SORT DATA=A.TV_stroke NODUPKEY OUT=TEMP2 ; BY Lag011_PM25; RUN;
PROC SORT DATA=A.TV_stroke NODUPKEY OUT=TEMP3 ; BY Lag023_PM25; RUN;

/*노출 값만 가져오기*/
data temp1; set temp1;  keep Lag05_PM25; run;
data temp2; set temp2;  keep Lag011_PM25; run;
data temp3; set temp3;  keep Lag023_PM25; run;

PROC MEANS DATA=temp1 N MEAN MEDIAN STDDEV P1 P5 P10 P25 P50 P75 P90 P95 P99  MIN MAX; VAR lag05_PM25  ; RUN;
PROC MEANS DATA=temp2 N MEAN MEDIAN STDDEV P1 P5 P10 P25 P50 P75 P90 P95 P99  MIN MAX; VAR LAG011_PM25 ; RUN;
PROC MEANS DATA=temp3 N MEAN MEDIAN STDDEV P1 P5 P10 P25 P50 P75 P90 P95 P99  MIN MAX; VAR LAG023_PM25 ; RUN;

/*필요한  */
data A.all_p; set a.tv_all; 
keep indi_dscm_no time0 time event Lag05_PM25 LAG011_PM25 LAG023_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  
income_month Lag05_mtemp Lag011_mtemp Lag023_mtemp Lag05_mhumi Lag011_mhumi Lag023_mhumi; run;

data A.cvd_p; set a.tv_cvd; 
keep indi_dscm_no time0 time event Lag05_PM25 LAG011_PM25 LAG023_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  
income_month Lag05_mtemp Lag011_mtemp Lag023_mtemp Lag05_mhumi Lag011_mhumi Lag023_mhumi; run;

data A.stroke_p; set a.tv_stroke; 
keep indi_dscm_no time0 time event Lag05_PM25 LAG011_PM25 LAG023_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  
income_month Lag05_mtemp Lag011_mtemp Lag023_mtemp Lag05_mhumi Lag011_mhumi Lag023_mhumi; run;

data A.ihd_p; set a.tv_ihd; 
keep indi_dscm_no time0 time event Lag05_PM25 LAG011_PM25 LAG023_PM25 SEX TAGE INCOME SIDO SMOKING high_edu  
income_month Lag05_mtemp Lag011_mtemp Lag023_mtemp Lag05_mhumi Lag011_mhumi Lag023_mhumi; run;

/*************************************************************************************************************************/
/*6-month exposure response, curve all*/
proc phreg data=A.all_p; 
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
EFFECT  SPL=SPLINE(LAG05_PM25/BASIS=TPF(NOINT) NATURALCUBIC DETAILS KNOTMETHOD=RANGEFRACTIONS(0.05 .50  0.95));
MODEL (TIME0,TIME)*EVENT(0)= SPL SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag05_mtemp Lag05_mhumi /RL=WALD;
store  r1; run;

%MACRO EST(REF=13.7, START=13.7, END=48.2,BY=0.1);
%do i=1 % to %eval(%sysfunc(ceil(%sysevalf((&end-&start)/&by)))+1);
%LET VALUE=%Sysevalf((&start-&by)+(&by*&I));
estimate"&value." SPL[-1,&REF] [1,&VALUE] /EXP CL;
%END;
%MEND EST;

ods html select none;
ods rtf select none;
ods dataset Estimates=Estimaes;

PROC PLM restore=r1;
%EST(REF=13.7, START=13.7, END=48.2,BY=0.1);run;

/*12-month exposure response, curve all*/
proc phreg data=A.all_p; 
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
EFFECT  SPL=SPLINE(LAG011_PM25/BASIS=TPF(NOINT) NATURALCUBIC DETAILS KNOTMETHOD=RANGEFRACTIONS(0.05 .50  0.95));
MODEL (TIME0,TIME)*EVENT(0)= SPL SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag011_mtemp Lag011_mhumi /RL=WALD;
store  r2; run;

%MACRO EST(REF=18.67, START=18.67, END=41.9,BY=0.1);
%do i=1 % to %eval(%sysfunc(ceil(%sysevalf((&end-&start)/&by)))+1);
%LET VALUE=%Sysevalf((&start-&by)+(&by*&I));
estimate"&value." SPL[-1,&REF] [1,&VALUE] /EXP CL;
%END;
%MEND EST;

ods html select none;
ods rtf select none;
ods dataset Estimates=Estimaes;

PROC PLM restore=r2;
%EST(REF=18.67, START=18.67, END=41.9,BY=0.1);run;

/*24-month exposure response, curve all*/
proc phreg data=A.all_p; 
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
EFFECT  SPL=SPLINE(LAG023_PM25/BASIS=TPF(NOINT) NATURALCUBIC DETAILS KNOTMETHOD=RANGEFRACTIONS(0.05 .50  0.95));
MODEL (TIME0,TIME)*EVENT(0)= SPL SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag023_mtemp Lag023_mhumi /RL=WALD;
store  r3; run;

%MACRO EST(REF=19.86, START=19.86, END=43.16,BY=0.1);
%do i=1 % to %eval(%sysfunc(ceil(%sysevalf((&end-&start)/&by)))+1);
%LET VALUE=%Sysevalf((&start-&by)+(&by*&I));
estimate"&value." SPL[-1,&REF] [1,&VALUE] /EXP CL;
%END;
%MEND EST;

ods html select none;
ods rtf select none;
ods dataset Estimates=Estimaes;

PROC PLM restore=r3;
%EST(REF=19.86, START=19.86, END=43.16,BY=0.1);run;

/*************************************************************************************************************************/
/*6-month exposure response, curve CVD*/
proc phreg data=A.cvd_p; 
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
EFFECT  SPL=SPLINE(LAG05_PM25/BASIS=TPF(NOINT) NATURALCUBIC DETAILS KNOTMETHOD=RANGEFRACTIONS(0.05 .50  0.95));
MODEL (TIME0,TIME)*EVENT(0)= SPL SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag05_mtemp Lag05_mhumi /RL=WALD;
store  r1; run;

%MACRO EST(REF=13.7, START=13.7, END=48.2,BY=0.1);
%do i=1 % to %eval(%sysfunc(ceil(%sysevalf((&end-&start)/&by)))+1);
%LET VALUE=%Sysevalf((&start-&by)+(&by*&I));
estimate"&value." SPL[-1,&REF] [1,&VALUE] /EXP CL;
%END;
%MEND EST;

ods html select none;
ods rtf select none;
ods dataset Estimates=Estimaes;

PROC PLM restore=r1;
%EST(REF=13.7, START=13.7, END=48.2,BY=0.1);run;

/*12-month exposure response, curve CVD*/
proc phreg data=A.cvd_p; 
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
EFFECT  SPL=SPLINE(LAG011_PM25/BASIS=TPF(NOINT) NATURALCUBIC DETAILS KNOTMETHOD=RANGEFRACTIONS(0.05 .50  0.95));
MODEL (TIME0,TIME)*EVENT(0)= SPL SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag011_mtemp Lag011_mhumi /RL=WALD;
store  r2; run;

%MACRO EST(REF=18.67, START=18.67, END=41.9,BY=0.1);
%do i=1 % to %eval(%sysfunc(ceil(%sysevalf((&end-&start)/&by)))+1);
%LET VALUE=%Sysevalf((&start-&by)+(&by*&I));
estimate"&value." SPL[-1,&REF] [1,&VALUE] /EXP CL;
%END;
%MEND EST;

ods html select none;
ods rtf select none;
ods dataset Estimates=Estimaes;

PROC PLM restore=r2;
%EST(REF=18.67, START=18.67, END=41.9,BY=0.1);run;

/*24-month exposure response, curve CVD*/
proc phreg data=A.cvd_p; 
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
EFFECT  SPL=SPLINE(LAG023_PM25/BASIS=TPF(NOINT) NATURALCUBIC DETAILS KNOTMETHOD=RANGEFRACTIONS(0.05 .50  0.95));
MODEL (TIME0,TIME)*EVENT(0)= SPL SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag023_mtemp Lag023_mhumi /RL=WALD;
store  r3; run;

%MACRO EST(REF=19.86, START=19.86, END=43.16,BY=0.1);
%do i=1 % to %eval(%sysfunc(ceil(%sysevalf((&end-&start)/&by)))+1);
%LET VALUE=%Sysevalf((&start-&by)+(&by*&I));
estimate"&value." SPL[-1,&REF] [1,&VALUE] /EXP CL;
%END;
%MEND EST;

ods html select none;
ods rtf select none;
ods dataset Estimates=Estimaes;

PROC PLM restore=r3;
%EST(REF=19.86, START=19.86, END=43.16,BY=0.1);run;

/*************************************************************************************************************************/
/*6-month exposure response, curve stroke*/
proc phreg data=A.STROKE_p; 
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
EFFECT  SPL=SPLINE(LAG05_PM25/BASIS=TPF(NOINT) NATURALCUBIC DETAILS KNOTMETHOD=RANGEFRACTIONS(0.05 .50  0.95));
MODEL (TIME0,TIME)*EVENT(0)= SPL SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag05_mtemp Lag05_mhumi /RL=WALD;
store  r1; run;

%MACRO EST(REF=13.7, START=13.7, END=48.2,BY=0.1);
%do i=1 % to %eval(%sysfunc(ceil(%sysevalf((&end-&start)/&by)))+1);
%LET VALUE=%Sysevalf((&start-&by)+(&by*&I));
estimate"&value." SPL[-1,&REF] [1,&VALUE] /EXP CL;
%END;
%MEND EST;

ods html select none;
ods rtf select none;
ods dataset Estimates=Estimaes;

PROC PLM restore=r1;
%EST(REF=13.7, START=13.7, END=48.2,BY=0.1);run;

/*12-month exposure response, curve stroke*/
proc phreg data=A.STROKE_p; 
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
EFFECT  SPL=SPLINE(LAG011_PM25/BASIS=TPF(NOINT) NATURALCUBIC DETAILS KNOTMETHOD=RANGEFRACTIONS(0.05 .50  0.95));
MODEL (TIME0,TIME)*EVENT(0)= SPL SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag011_mtemp Lag011_mhumi /RL=WALD;
store  r2; run;

%MACRO EST(REF=18.67, START=18.67, END=41.9,BY=0.1);
%do i=1 % to %eval(%sysfunc(ceil(%sysevalf((&end-&start)/&by)))+1);
%LET VALUE=%Sysevalf((&start-&by)+(&by*&I));
estimate"&value." SPL[-1,&REF] [1,&VALUE] /EXP CL;
%END;
%MEND EST;

ods html select none;
ods rtf select none;
ods dataset Estimates=Estimaes;

PROC PLM restore=r2;
%EST(REF=18.67, START=18.67, END=41.9,BY=0.1);run;

/*24-month exposure response, curve stroke*/
proc phreg data=A.STROKE_p; 
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
EFFECT  SPL=SPLINE(LAG023_PM25/BASIS=TPF(NOINT) NATURALCUBIC DETAILS KNOTMETHOD=RANGEFRACTIONS(0.05 .50  0.95));
MODEL (TIME0,TIME)*EVENT(0)= SPL SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag023_mtemp Lag023_mhumi /RL=WALD;
store  r3; run;

%MACRO EST(REF=19.86, START=19.86, END=43.16,BY=0.1);
%do i=1 % to %eval(%sysfunc(ceil(%sysevalf((&end-&start)/&by)))+1);
%LET VALUE=%Sysevalf((&start-&by)+(&by*&I));
estimate"&value." SPL[-1,&REF] [1,&VALUE] /EXP CL;
%END;
%MEND EST;

ods html select none;
ods rtf select none;
ods dataset Estimates=Estimaes;

PROC PLM restore=r3;
%EST(REF=19.86, START=19.86, END=43.16,BY=0.1);run;

/*************************************************************************************************************************/
/*6-month exposure response, curve IHD*/
proc phreg data=A.IHD_p; 
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
EFFECT  SPL=SPLINE(LAG05_PM25/BASIS=TPF(NOINT) NATURALCUBIC DETAILS KNOTMETHOD=RANGEFRACTIONS(0.05 .50  0.95));
MODEL (TIME0,TIME)*EVENT(0)= SPL SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag05_mtemp Lag05_mhumi /RL=WALD;
store  r1; run;

%MACRO EST(REF=13.7, START=13.7, END=48.2,BY=0.1);
%do i=1 % to %eval(%sysfunc(ceil(%sysevalf((&end-&start)/&by)))+1);
%LET VALUE=%Sysevalf((&start-&by)+(&by*&I));
estimate"&value." SPL[-1,&REF] [1,&VALUE] /EXP CL;
%END;
%MEND EST;

ods html select none;
ods rtf select none;
ods dataset Estimates=Estimaes;

PROC PLM restore=r1;
%EST(REF=13.7, START=13.7, END=48.2,BY=0.1);run;

/*12-month exposure response, curve stroke*/
proc phreg data=A.IHD_p; 
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
EFFECT  SPL=SPLINE(LAG011_PM25/BASIS=TPF(NOINT) NATURALCUBIC DETAILS KNOTMETHOD=RANGEFRACTIONS(0.05 .50  0.95));
MODEL (TIME0,TIME)*EVENT(0)= SPL SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag011_mtemp Lag011_mhumi /RL=WALD;
store  r2; run;

%MACRO EST(REF=18.67, START=18.67, END=41.9,BY=0.1);
%do i=1 % to %eval(%sysfunc(ceil(%sysevalf((&end-&start)/&by)))+1);
%LET VALUE=%Sysevalf((&start-&by)+(&by*&I));
estimate"&value." SPL[-1,&REF] [1,&VALUE] /EXP CL;
%END;
%MEND EST;

ods html select none;
ods rtf select none;
ods dataset Estimates=Estimaes;

PROC PLM restore=r2;
%EST(REF=18.67, START=18.67, END=41.9,BY=0.1);run;

/*24-month exposure response, curve stroke*/
proc phreg data=A.IHD_p; 
CLASS SEX INCOME SIDO /PARAM=REF REF=FIRST;
EFFECT  SPL=SPLINE(LAG023_PM25/BASIS=TPF(NOINT) NATURALCUBIC DETAILS KNOTMETHOD=RANGEFRACTIONS(0.05 .50  0.95));
MODEL (TIME0,TIME)*EVENT(0)= SPL SEX TAGE INCOME SIDO SMOKING high_edu  income_month Lag023_mtemp Lag023_mhumi /RL=WALD;
store  r3; run;

%MACRO EST(REF=19.86, START=19.86, END=43.16,BY=0.1);
%do i=1 % to %eval(%sysfunc(ceil(%sysevalf((&end-&start)/&by)))+1);
%LET VALUE=%Sysevalf((&start-&by)+(&by*&I));
estimate"&value." SPL[-1,&REF] [1,&VALUE] /EXP CL;
%END;
%MEND EST;

ods html select none;
ods rtf select none;
ods dataset Estimates=Estimaes;

PROC PLM restore=r3;
%EST(REF=19.86, START=19.86, END=43.16,BY=0.1);run;


