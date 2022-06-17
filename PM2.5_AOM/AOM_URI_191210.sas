/*이화여대 직업환경의학교실  오종민 */
/*PM AOM  맞춤형  DB*/ 
/*노출자료 : CMAQ PM2.5*/
/**********************************************************************************************************************************************************/
/*data 라이브러리 */
libname dat '/userdata06/room206/data_source/user_data'; /*원 데이터*/
libname a '/userdata06/room206/data_source/외래입원';    
libname b '/userdata06/room206/data_source/입원'; 
/**********************************************************************************************************************************************************/
/**********************************************************************************************************************************************************/

/*고유 대상 ID (15세 이하 남녀 소아 08~17)*/
DATA B.TG; SET DAT.TG; RUN;

/*연도별 자격 자료를 하나로 merge*/
DATA B.BFC; SET DAT.BFC_2008 DAT.BFC_2009 DAT.BFC_2010 DAT.BFC_2011 DAT.BFC_2012 
                          DAT.BFC_2013 DAT.BFC_2014 DAT.BFC_2015 DAT.BFC_2016 DAT.BFC_2017; RUN;

/*자격에 PKEY 추가*/
DATA B.BFC ; SET B.BFC;  PKEY=COMPRESS(STD_YYYY) || COMPRESS("-") ||COMPRESS(INDI_DSCM_NO);RUN;

/*연령 시도 추가*/
DATA B.BFC ; SET B.BFC;
AGE= STD_YYYY-BYEAR;
SIDO=SUBSTR(RVSN_ADDR_CD,1,2);RUN;

/*자격 이용할 테이블*/
data B.JK ; SET B.BFC;  KEEP PKEY STD_YYYY INDI_DSCM_NO SEX_TYPE BYEAR AGE RVSN_ADDR_CD SIDO; RUN;

/**********************************************************************************************************************************************************/
/*단기영향자료분석 ; Time-series */
/*연도별로 질환 추출  */
%Macro AOM(TABLE1,TABLE2);
data B.&table1 ; set A.&table2;
IF FORM_CD IN("02") AND SUBSTR(SICK_SYM1,1,4) IN ("H650","H651","H660") THEN K1=2; ELSE K1=0; /*입원 이면서 주상병 IHD 코드, K1은 주상병에 존재하면 2*/
IF FORM_CD IN("02") AND SUBSTR(SICK_SYM2,1,4) IN ("H650","H651","H660") THEN K2=1; ELSE K2=0; /*입원 이면서 부상병 IHD 코드, K2은 부상병에 존재하면 1*/
ICD_RANK=K1+K2; IF ICD_RANK>0; RUN; /*ICD_RANK 주+부상병 중요도 순위 나타냄*/
%MEND IHD;
%AOM(TOTAL_AOM_08,T20_2008) %AOM(TOTAL_AOM_09,T20_2009)
%AOM(TOTAL_AOM_10,T20_2010) %AOM(TOTAL_AOM_11,T20_2011)
%AOM(TOTAL_AOM_12,T20_2012) %AOM(TOTAL_AOM_13,T20_2013)
%AOM(TOTAL_AOM_14,T20_2014) %AOM(TOTAL_AOM_15,T20_2015)
%AOM(TOTAL_AOM_16,T20_2016) %AOM(TOTAL_AOM_17,T20_2017)

/*전체  질환 에피소드  연도별 테이블 MERGE*/
%MACRO T_append( table ); /*자격 년도별 통합 자료로 만드는  매크로*/
%LET rep08 = _08; %LET rep09 = _09; %LET rep10 = _10; %LET rep11 = _11; %LET rep12 = _12; 
%LET rep13 = _13; %LET rep14 = _14; %LET rep15 = _15; %LET rep16 = _16; %LET rep17 = _17; 
DATA B.&table ; SET                            B.&table&rep08 ; RUN ;
PROC APPEND BASE = B.&table DATA = B.&table&rep09  ; RUN ;
PROC APPEND BASE = B.&table DATA = B.&table&rep10  ; RUN ;
PROC APPEND BASE = B.&table DATA = B.&table&rep11  ; RUN ;
PROC APPEND BASE = B.&table DATA = B.&table&rep12  ; RUN ;
PROC APPEND BASE = B.&table DATA = B.&table&rep13  ; RUN ;
PROC APPEND BASE = B.&table DATA = B.&table&rep14  ; RUN ;
PROC APPEND BASE = B.&table DATA = B.&table&rep15  ; RUN ;
PROC APPEND BASE = B.&table DATA = B.&table&rep16  ; RUN ;
PROC APPEND BASE = B.&table DATA = B.&table&rep17  ; RUN ;
DATA A.TOTAL_AOM ; SET A.TOTAL_AOM;RUN;
%MEND T_append;
%T_append (TOTAL_AOM);

/*자격이랑 MERGE KEY 생성 : PKEY*/
DATA  B.TOTAL_AOM ; SET B.TOTAL_AOM;   PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4)) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO);RUN;

/*자격이랑  MERGE BY : PKEY*/
PROC SQL; CREATE TABLE B.TOTAL_AOM       AS SELECT * FROM B.TOTAL_AOM, B.JK WHERE TOTAL_AOM.PKEY =JK.PKEY;QUIT;
/**********************************************************************************************************************************************************/
/**********************************************************************************************************************************************************/
/*DATA CLEANING ; 단기측정*/
%MACRO STEP1(TABLE,DISEASE);
DATA B.&TABLE&DISEASE; SET B.&TABLE&DISEASE;
IF INDI_DSCM_NO="" THEN DELETE; /*ID 무효 제외*/
IF FORM_CD IN ("02");                 /*무효한 진료 형태 제외*/ 
IF "2008" <= SUBSTR(MDCARE_STRT_DT,1,4) <="2017" AND "01" <=SUBSTR(MDCARE_STRT_DT,5,2) <="12" AND "01" <= SUBSTR(MDCARE_STRT_DT,7,2) <="31"; /*무효한 진료일자 제외*/
IF "1899" <= SUBSTR(FST_HSPTZ_DT,1,4) <="2017" AND "01" <=SUBSTR(FST_HSPTZ_DT,5,2) <="12" AND "01" <= SUBSTR(FST_HSPTZ_DT,7,2) <="31"                /*무효한 최초 진료일자 제외*/
THEN FST_HSPTZ_DT=FST_HSPTZ_DT; ELSE FST_HSPTZ_DT=""; /*무효한 입내원 일수 제외*/
IF VSHSP_DD_CNT="" THEN DELETE;
IF VSHSP_DD_CNT=0 THEN VSHSP_DD_CNT=1;                  /*입내원 일수 0인 경우 입원을 했다가 검진 진료로 마침 0=>1 코딩*/
IF 0 <=AGE <=25 ;                                                    /*무효 나이 제외*/
IF SEX_TYPE IN ("1","2");                                             /*성별 무효 제외*/
IF RVSN_ADDR_CD="" THEN DELETE; RUN;                      /*시군구 무효 제외*/
%MEND S_STEP1;
%STEP1(TOTAL,_AOM);

/**********************************************************************************************************************************************************/
/*필요변수 추출 */
%MACRO STEP2(TABLE1,TABLE);
DATA B.&TABLE1; SET B.&TABLE;
FORMAT MDCARE FST MDCARE_DATE FST_DATE DATE1 YYMMDD10.;

MDCARE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
FST      =MDY(SUBSTR(FST_HSPTZ_DT,5,2),SUBSTR(FST_HSPTZ_DT,7,2),SUBSTR(FST_HSPTZ_DT,1,4));
IF FST^="" THEN FST_STATUS=1; ELSE FST_STATUS=0;
/*진료개시일자 계산*/
IF MDCARE="" THEN MDCARE_DATE=FST; ELSE MDCARE_DATE=MDCARE;
IF FST^=""      THEN FST_DATE=FST;        ELSE FST_DATE=MDCARE;

DATE1=MIN(FST_DATE,MDCARE_DATE); /*진료 개시일자*/
DIFF_PLUS=MDCARE_DATE-DATE1; /*최초로 진료 받은 일 - 입원일*/
CNT_DD=DIFF_PLUS+VSHSP_DD_CNT; RUN; /*요양일 계산*/
%MEND S_STEP2;
%STEP2(AOM1,TOTAL_AOM);

/**********************************************************************************************************************************************************/
/*청구 건수가 연달아 발생하면 하나의 발생으로 간주 하여 계산*/
/*청구 건당 날짜가 같으면 가장 긴 입내원 일수만  KEEP*/
/*만약 청구 건당 날짜와 가장긴 입내원 일수가 같다면 주상병 이 AOM인것만 KEEP*/
/*만약 청구 건당 날짜와 가장긴 입내원 일수가 같고 주상병이 같으면 그중 하나 KEEP*/

/*입내원 일수 계산 */
%MACRO STEP3(TABLE2,TABLE1);
PROC SORT DATA=B.&TABLE1 ; BY INDI_DSCM_NO MDCARE_DATE CNT_DD SICK_SYM1; RUN; /*데이터 정렬*/

DATA B.&TABLE1 ; SET B.&TABLE1;
DKEY=COMPRESS(MDCARE_DATE) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO); RUN; /*진료일과 개인아이디로 새로운 KEY 만듬(중복제거위해)*/

/*데이터 정렬 : 1) 날짜+개별 ID 고려 2) 입내원 일수 내림 차순 , 3) 주+부상병 순위 내림 차순*/
PROC SORT DATA=B.&TABLE1; BY DKEY DESCENDING CNT_DD DESCENDING ICD_RANK; RUN; 

/*위 정렬한 데이터 에서 날짜+개별 아이디 기준으로 첫행이 아니면 제외*/
DATA B.&TABLE2; SET B.&TABLE1; BY DKEY; IF FIRST.DKEY^=1 THEN DELETE;
DROP DKEY ICD_RANK; RUN;

proc sort data=B.&TABLE2; by indi_dscm_no MDCARE_DATE ;run; /*데이터 정렬  ID, 진료 개시일 순*/
%MEND S1_STEP3;
%STEP3(AOM2,AOM1);

/**********************************************************************************************************************************************************/
/*EPISODE 계산 */
%MACRO STEP4(TABLE4,TABLE3,TABLE2,NUMBER);
DATA B.&TABLE3; 
FORMAT R START_DATE DATE1_DISCHARGE YYMMDD10.;
RETAIN R D START_DATE MONEY; SET B.&TABLE2; BY INDI_DSCM_NO;
IF FIRST.INDI_DSCM_NO=1 AND LAST.INDI_DSCM_NO=1 THEN DO;
IKEEP=1; R=DATE1+CNT_DD-1; D=CNT_DD; START_DATE=DATE1; MONEY=ED_RC_TOT_AMT; END; ELSE DO;
IF FIRST.INDI_DSCM_NO=1 AND LAST.INDI_DSCM_NO^=1 THEN DO;
IKEEP=1;  R=DATE1+CNT_DD-1; D=CNT_DD; START_DATE=DATE1; MONEY=ED_RC_TOT_AMT; END; ELSE DO;

K=DATE1-R;  /*연속된 입내원 일수를 마지막 퇴원한 날짜-최초 입원 날짜로 계산*/
IF K<=&NUMBER. THEN DO; IKEEP=0; 
IF DATE1+CNT_DD-1 <R THEN D=D;
IF DATE1+CNT_DD-1 <R THEN MONEY=MONEY+ED_RC_TOT_AMT; ELSE DO;
R=DATE1+CNT_DD-1; D=R-START_DATE+1;
MONEY=ED_RC_TOT_AMT+MONEY; END; END; ELSE DO;
IKEEP=1;  R=DATE1+CNT_DD-1; D=CNT_DD; START_DATE=DATE1;
MONEY=ED_RC_TOT_AMT; END; END; END;
DATE1_DISCHARGE=DATE1+CNT_DD-1; RUN;

/*sort by id and descending MDCARE_date*/
PROC SORT DATA=B.&TABLE3; BY INDI_DSCM_NO DESCENDING MDCARE_DATE DESCENDING CNT_DD; RUN;

/*PUT MAX VALUE FOR IKEEP=0*/
DATA B.&TABLE4;
FORMAT DISCHARGEDATE YYMMDD10.; RETAIN MAXD; SET B.&TABLE3; BY INDI_DSCM_NO;
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
%MEND S_STEP4;
%STEP4 (AOM_W1,AOM3,AOM2,7);
%STEP4 (AOM_W2,AOM3,AOM2,14);
%STEP4 (AOM_W3,AOM3,AOM2,21);
%STEP4 (AOM_W4,AOM3,AOM2,28);

/**********************************************************************************************************************************************************/
/*연령별 구분  */ 
%MACRO STEP5(T2,T1,OUT,K);
DATA A.&T2; SET A.&T1;

SIDO= SUBSTR(RVSN_ADDR_CD,1,2);
IF SIDO IN (&K.);
IF AGE=0 THEN AGE0=1 ; ELSE AGE0=0;
IF AGE=1 THEN AGE1=1 ; ELSE AGE1=0;
IF AGE=2 THEN AGE2=1 ; ELSE AGE2=0;
IF AGE=3 THEN AGE3=1 ; ELSE AGE3=0;
IF AGE>=1 & AGE<=3 THEN AGE13=1; ELSE AGE13=0;
if age >=4 & age <15 then age415=1; else age415=0;
IF AGE<=3 THEN TOT=1; ELSE TOT=0;

IF AGE=0 & SEX_TYPE=1 THEN AGE0_M=1 ; else AGE0_M=0;
IF AGE=0 & SEX_TYPE=2 THEN AGE0_F=1 ; else AGE0_F=0;

IF AGE=1 & SEX_TYPE=1 THEN AGE1_M=1 ; else AGE1_M=0;
IF AGE=1 & SEX_TYPE=2 THEN AGE1_F=1  ; else AGE1_F=0;

IF AGE=2 & SEX_TYPE=1 THEN AGE2_M=1 ; else AGE2_M=0;
IF AGE=2 & SEX_TYPE=2 THEN AGE2_F=1  ; else AGE2_F=0;

IF AGE=3 & SEX_TYPE=1 THEN AGE3_M=1 ; else AGE3_M=0;
IF AGE=3 & SEX_TYPE=2 THEN AGE3_F=1  ; else AGE3_F=0;

IF AGE>=1 & AGE<=3 & SEX_TYPE=1 THEN AGE13_M=1 ; else AGE13_M=0;
IF AGE>=1 & AGE<=3 & SEX_TYPE=2 THEN AGE13_F=1  ; else AGE13_F=0;

IF AGE>=4 & AGE<15 & SEX_TYPE=1 THEN AGE415_M=1 ; else AGE415_M=0;
IF AGE>=4 & AGE<15 & SEX_TYPE=2 THEN AGE415_F=1  ; else AGE415_F=0;

IF AGE<=3 & SEX_TYPE=1 THEN TOT_M=1 ; else TOT_M=0;
IF AGE<=3 & SEX_TYPE=2 THEN TOT_F=1  ; else TOT_F=0;

PROC SQL; 
CREATE TABLE A.&OUT AS SELECT START_DATE AS DATE, SIDO, SUM(AGE0) AS AGE0, SUM(AGE1) AS AGE1, SUM(AGE2) AS AGE2, SUM(AGE3) AS AGE3,
                                                  SUM(AGE13) AS AGE13, SUM(TOT) AS TOT, SUM(AGE0_M) AS AGE0_M, SUM(AGE0_F) AS AGE0_F,
												  SUM(AGE1_M) AS AGE1_M, SUM(AGE1_F) AS AGE1_F, SUM(AGE2_M) AS AGE2_M, SUM(AGE2_F) AS AGE2_F,
												  SUM(AGE3_M) AS AGE3_M, SUM(AGE3_F) AS AGE3_F, SUM(AGE13_M) AS AGE13_M, SUM(AGE13_F) AS AGE13_F,
												  SUM(TOT_M)  AS TOT_M, SUM(TOT_F)  AS TOT_F, sum(age415) as age415, sum(age415_m) as age415_m,
                                                  SUM(AGE415_F) AS AGE415_F FROM A.&T2 GROUP BY START_DATE;QUIT;

PROC SORT DATA= A.&OUT NODUPKEY; BY DATE; RUN;
%MEND STEP5;
/*시도 구분하여 질환별 카운트 자료 추출 */
%STEP5 (AOM_W11,AOM_W1,S1_W1_COUNT,11); 
%STEP5 (AOM_W11,AOM_W1,S2_W1_COUNT,26); 
%STEP5 (AOM_W11,AOM_W1,S3_W1_COUNT,27); 
%STEP5 (AOM_W11,AOM_W1,S4_W1_COUNT,28); 
%STEP5 (AOM_W11,AOM_W1,S5_W1_COUNT,29); 
%STEP5 (AOM_W11,AOM_W1,S6_W1_COUNT,30); 
%STEP5 (AOM_W11,AOM_W1,S7_W1_COUNT,31); 

%STEP5 (AOM_W22,AOM_W2,S1_W2_COUNT,11); 
%STEP5 (AOM_W22,AOM_W2,S2_W2_COUNT,26); 
%STEP5 (AOM_W22,AOM_W2,S3_W2_COUNT,27); 
%STEP5 (AOM_W22,AOM_W2,S4_W2_COUNT,28); 
%STEP5 (AOM_W22,AOM_W2,S5_W2_COUNT,29); 
%STEP5 (AOM_W22,AOM_W2,S6_W2_COUNT,30); 
%STEP5 (AOM_W22,AOM_W2,S7_W2_COUNT,31); 

%STEP5 (AOM_W33,AOM_W3,S1_W3_COUNT,11); 
%STEP5 (AOM_W33,AOM_W3,S2_W3_COUNT,26); 
%STEP5 (AOM_W33,AOM_W3,S3_W3_COUNT,27); 
%STEP5 (AOM_W33,AOM_W3,S4_W3_COUNT,28); 
%STEP5 (AOM_W33,AOM_W3,S5_W3_COUNT,29); 
%STEP5 (AOM_W33,AOM_W3,S6_W3_COUNT,30); 
%STEP5 (AOM_W33,AOM_W3,S7_W3_COUNT,31); 

%STEP5 (AOM_W44,AOM_W4,S1_W4_COUNT,11); 
%STEP5 (AOM_W44,AOM_W4,S2_W4_COUNT,26); 
%STEP5 (AOM_W44,AOM_W4,S3_W4_COUNT,27); 
%STEP5 (AOM_W44,AOM_W4,S4_W4_COUNT,28); 
%STEP5 (AOM_W44,AOM_W4,S5_W4_COUNT,29); 
%STEP5 (AOM_W44,AOM_W4,S6_W4_COUNT,30); 
%STEP5 (AOM_W44,AOM_W4,S7_W4_COUNT,31); 

DATA A.AOM_W1_COUNT; SET A.S1_W1_COUNT A.S2_W1_COUNT A.S3_W1_COUNT A.S4_W1_COUNT A.S5_W1_COUNT A.S6_W1_COUNT A.S7_W1_COUNT; RUN;
DATA A.AOM_W2_COUNT; SET A.S1_W2_COUNT A.S2_W2_COUNT A.S3_W2_COUNT A.S4_W2_COUNT A.S5_W2_COUNT A.S6_W2_COUNT A.S7_W2_COUNT; RUN;
DATA A.AOM_W3_COUNT; SET A.S1_W3_COUNT A.S2_W3_COUNT A.S3_W3_COUNT A.S4_W3_COUNT A.S5_W3_COUNT A.S6_W3_COUNT A.S7_W3_COUNT; RUN;
DATA A.AOM_W4_COUNT; SET A.S1_W4_COUNT A.S2_W4_COUNT A.S3_W4_COUNT A.S4_W4_COUNT A.S5_W4_COUNT A.S6_W4_COUNT A.S7_W4_COUNT; RUN;

proc sort data=a.aom_w1 nodupkey out=a.id1 ; by indi_dscm_no; run;
proc sort data=a.aom_w2 nodupkey out=a.id2 ; by indi_dscm_no; run;
proc sort data=a.aom_w3 nodupkey out=a.id3 ; by indi_dscm_no; run;
proc sort data=a.aom_w4 nodupkey out=a.id4 ; by indi_dscm_no; run;

data a.z; set a.aom_w4;
if std_yyyy<2017;
if sido in (11,26,27,28,29,30,31);
if age <4;
keep indi_dscm_no age sido std_yyyy; 
run;
proc sort data=a.z ; by indi_dscm_no age sido;run;
data a.s4; set a.z; keep indi_dscm_no; run;

proc sql; create table count1 as select INDI_DSCM_NO as id, count(indi_dscm_no) as count from a.s1 group by indi_dscm_no; quit;
proc sql; create table count2 as select INDI_DSCM_NO as id, count(indi_dscm_no) as count from a.s2 group by indi_dscm_no; quit;
proc sql; create table count3 as select INDI_DSCM_NO as id, count(indi_dscm_no) as count from a.s3 group by indi_dscm_no; quit;
proc sql; create table count4 as select INDI_DSCM_NO as id, count(indi_dscm_no) as count from a.s4 group by indi_dscm_no; quit;

proc means data=count1 mean sTd median min max q1 q3 p1 p5 p10 p25 p50 p75 p90 p95 n ;run;
proc means data=count2 mean sTd median min max q1 q3 p1 p5 p10 p25 p50 p75 p90 p95 n ;run;
proc means data=count3 mean sTd median min max q1 q3 p1 p5 p10 p25 p50 p75 p90 p95 n ;run;
proc means data=count4 mean std median min max q1 q3 p1 p5 p10 p25 p50 p75 p90 p95 n ;run;

proc freq data=count4 ;tables count;

/*************************************************************************************************************/
/*************************************************************************************************************/
/*************************************************************************************************************/
/*AOM 환자 중에서 URI를 겪은 적이 있는 환자  몇 퍼센트 인지 ? */
proc sort data=a.s4 out=A.id nodupkey ; by indi_dscm_no; run;

%STEP5 (a1,AOM_W4,S1_W4_COUNT,11); 
%STEP5 (a2,AOM_W4,S2_W4_COUNT,26); 
%STEP5 (a3,AOM_W4,S3_W4_COUNT,27); 
%STEP5 (a4,AOM_W4,S4_W4_COUNT,28); 
%STEP5 (a5,AOM_W4,S5_W4_COUNT,29); 
%STEP5 (a6,AOM_W4,S6_W4_COUNT,30); 
%STEP5 (a7,AOM_W4,S7_W4_COUNT,31); 

%Macro URI(TABLE1,TABLE2);
data A.&table1 ; set A.&table2;
IF FORM_CD IN("02","03") AND SUBSTR(SICK_SYM1,1,3) IN ("J00","J01","J02","J03","J04","J05","J06") THEN K1=2; ELSE K1=0; /*입원 이면서 주상병 IHD 코드, K1은 주상병에 존재하면 2*/
IF FORM_CD IN("02","03") AND SUBSTR(SICK_SYM2,1,3) IN ("J00","J01","J02","J03","J04","J05","J06") THEN K2=1; ELSE K2=0; /*입원 이면서 부상병 IHD 코드, K2은 부상병에 존재하면 1*/
ICD_RANK=K1+K2; IF ICD_RANK>0; RUN; /*ICD_RANK 주+부상병 중요도 순위 나타냄*/
%MEND URI;
%URI(TOTAL_URI_08,T20_2008) %URI(TOTAL_URI_09,T20_2009)
%URI(TOTAL_URI_10,T20_2010) %URI(TOTAL_URI_11,T20_2011)
%URI(TOTAL_URI_12,T20_2012) %URI(TOTAL_URI_13,T20_2013)
%URI(TOTAL_URI_14,T20_2014) %URI(TOTAL_URI_15,T20_2015)
%URI(TOTAL_URI_16,T20_2016) %URI(TOTAL_URI_17,T20_2017)
%T_append (TOTAL_URI);
/*자격이랑 MERGE KEY 생성 : PKEY*/
DATA  A.TOTAL_URI ; SET A.TOTAL_URI;   PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4)) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO);RUN;
/*자격이랑  MERGE BY : PKEY*/
PROC SQL; CREATE TABLE A.TOTAL_URI      AS SELECT * FROM B.TOTAL_URI, A.JK WHERE TOTAL_URI.PKEY =JK.PKEY;QUIT;
/**********************************************************************************************************************************************************/
%STEP1(TOTAL,_URI);
%STEP2(URI1,TOTAL_URI);
%STEP3(URI2,URI1);

DATA A.TOTAL_URI_12; SET A.TOTAL_URI_12;
IF SUBSTR(SICK_SYM1,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM2,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM3,1,4) NOT IN ("H650","H651","H660"); RUN;

DATA A.TOTAL_URI_13; SET A.TOTAL_URI_13;
IF SUBSTR(SICK_SYM1,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM2,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM3,1,4) NOT IN ("H650","H651","H660"); RUN;

DATA A.TOTAL_URI_14; SET A.TOTAL_URI_14;
IF SUBSTR(SICK_SYM1,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM2,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM3,1,4) NOT IN ("H650","H651","H660"); RUN;

DATA A.TOTAL_URI_15; SET A.TOTAL_URI_15;
IF SUBSTR(SICK_SYM1,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM2,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM3,1,4) NOT IN ("H650","H651","H660"); RUN;

DATA A.TOTAL_URI_16; SET A.TOTAL_URI_16;
IF SUBSTR(SICK_SYM1,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM2,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM3,1,4) NOT IN ("H650","H651","H660"); RUN;

PROC SQL; CREATE TABLE A.TOTAL_URI_08 AS SELECT * FROM A.TOTAL_URI_08 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_URI_09 AS SELECT * FROM A.TOTAL_URI_09 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_URI_10 AS SELECT * FROM A.TOTAL_URI_10 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_URI_11 AS SELECT * FROM A.TOTAL_URI_11 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_URI_12 AS SELECT * FROM A.TOTAL_URI_12 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_URI_13 AS SELECT * FROM A.TOTAL_URI_13 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_URI_14 AS SELECT * FROM A.TOTAL_URI_14 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_URI_15 AS SELECT * FROM A.TOTAL_URI_15 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_URI_16 AS SELECT * FROM A.TOTAL_URI_16 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;

DATA A.URI ; SET A.TOTAL_URI_08 A.TOTAL_URI_09 A.TOTAL_URI_10 A.TOTAL_URI_11 A.TOTAL_URI_12 A.TOTAL_URI_13 A.TOTAL_URI_14 A.TOTAL_URI_15 A.TOTAL_URI_16;RUN;

DATA A.URI; SET A.URI; KEEP CMN_KEY INDI_DSCM_NO MDCARE_STRT_DT SICK_SYM1 SICK_SYM2 ; RUN;

DATA A.AOM; SET A.AOM_W4 ; if age<4; KEEP CMN_KEY INDI_DSCM_NO MDCARE_STRT_DT SICK_SYM1 SICK_SYM2; RUN;

DATA A.AOM ; SET A.AOM ; KEY=1; RUN;
DATA A.URI ; SET A.URI ; KEY=0; RUN;

DATA A.DAT ; SET A.AOM A.URI; RUN;

PROC SORT DATA=A.DAT ; BY INDI_DSCM_NO MDCARE_STRT_DT;RUN;

DATA A.DAT1; SET A.DAT;
IF substr(MDCARE_STRT_DT,1,4)<2017;
DAY=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
IF LAG(INDI_DSCM_NO)=INDI_DSCM_NO THEN N1="T" ; ELSE N1="F";  /*이전 시점에 ID가 같은지*/
IF LAG(KEY)=0 & KEY=1 THEN N2="T" ; ELSE N2="F";                        /*이전 시점에 URI 있는 경우 */
IF DAY-LAG(DAY)>=0 & DAY-LAG(DAY)<=7 THEN N3="T" ; ELSE N3="F"; /*이전 시점에 uri 날짜랑 차이 얼마나는지*/
if KEY=1 & n1="T" & N2="T" & N3="T" THEN OUT=1 ; ELSE OUT=0; 
RUN; 

DATA A.DAT2; SET A.DAT;
IF substr(MDCARE_STRT_DT,1,4)<2017;
DAY=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
IF LAG(INDI_DSCM_NO)=INDI_DSCM_NO THEN N1="T" ; ELSE N1="F";  /*이전 시점에 ID가 같은지*/
IF LAG(KEY)=0 & KEY=1 THEN N2="T" ; ELSE N2="F";                        /*이전 시점에 URI 있는 경우 */
IF DAY-LAG(DAY)>=8 & DAY-LAG(DAY)<=14 THEN N3="T" ; ELSE N3="F"; /*이전 시점에 uri 날짜랑 차이 얼마나는지*/
if KEY=1 & n1="T" & N2="T" & N3="T" THEN OUT=1 ; ELSE OUT=0; 
RUN; 
DATA A.DAT3; SET A.DAT;
IF substr(MDCARE_STRT_DT,1,4)<2017;
DAY=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
IF LAG(INDI_DSCM_NO)=INDI_DSCM_NO THEN N1="T" ; ELSE N1="F";  /*이전 시점에 ID가 같은지*/
IF LAG(KEY)=0 & KEY=1 THEN N2="T" ; ELSE N2="F";                        /*이전 시점에 URI 있는 경우 */
IF DAY-LAG(DAY)>=15 & DAY-LAG(DAY)<=21 THEN N3="T" ; ELSE N3="F"; /*이전 시점에 uri 날짜랑 차이 얼마나는지*/
if KEY=1 & n1="T" & N2="T" & N3="T" THEN OUT=1 ; ELSE OUT=0; 
RUN; 
DATA A.DAT4; SET A.DAT;
IF substr(MDCARE_STRT_DT,1,4)<2017;
DAY=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
IF LAG(INDI_DSCM_NO)=INDI_DSCM_NO THEN N1="T" ; ELSE N1="F";  /*이전 시점에 ID가 같은지*/
IF LAG(KEY)=0 & KEY=1 THEN N2="T" ; ELSE N2="F";                        /*이전 시점에 URI 있는 경우 */
IF DAY-LAG(DAY)>=22 & DAY-LAG(DAY)<=28 THEN N3="T" ; ELSE N3="F"; /*이전 시점에 uri 날짜랑 차이 얼마나는지*/
if KEY=1 & n1="T" & N2="T" & N3="T" THEN OUT=1 ; ELSE OUT=0; 
RUN;

PROC SQL; CREATE TABLE Z AS SELECT INDI_DSCM_NO AS ID, COUNT(INDI_DSCM_NO) FROM A.DAT GROUP BY INDI_DSCM_NO; QUIT;
 
DATA A.DAT5; SET A.DAT;
IF substr(MDCARE_STRT_DT,1,4)<2017;
DAY=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
IF LAG(INDI_DSCM_NO)=INDI_DSCM_NO THEN N1="T" ; ELSE N1="F";  /*이전 시점에 ID가 같은지*/
IF LAG(KEY)=0 & KEY=1 THEN N2="T" ; ELSE N2="F";                        /*이전 시점에 URI 있는 경우 */
DF1=DAY-LAG(DAY);
DF2=DAY-LAG2(DAY);
DF3=DAY-LAG3(DAY);
DF4=DAY-LAG4(DAY);
DF5=DAY-LAG5(DAY);
DF6=DAY-LAG6(DAY);
DF7=DAY-LAG7(DAY);
DF8=DAY-LAG8(DAY);
DF9=DAY-LAG9(DAY);
DF10=DAY-LAG10(DAY);
IF LAG(INDI_DSCM_NO)^= INDI_DSCM_NO  THEN DF1=".";
IF LAG2(INDI_DSCM_NO)^=INDI_DSCM_NO THEN DF2=".";
IF LAG3(INDI_DSCM_NO)^=INDI_DSCM_NO THEN DF3=".";
IF LAG4(INDI_DSCM_NO)^=INDI_DSCM_NO THEN DF4=".";
IF LAG5(INDI_DSCM_NO)^=INDI_DSCM_NO THEN DF5=".";
IF LAG6(INDI_DSCM_NO)^=INDI_DSCM_NO THEN DF6=".";
IF LAG7(INDI_DSCM_NO)^=INDI_DSCM_NO THEN DF7=".";
IF LAG8(INDI_DSCM_NO)^=INDI_DSCM_NO THEN DF8=".";
IF LAG9(INDI_DSCM_NO)^=INDI_DSCM_NO THEN DF9=".";
IF LAG10(INDI_DSCM_NO)^=INDI_DSCM_NO THEN DF10=".";
IF KEY^=1 THEN DF1=",";IF KEY^=1 THEN DF2=",";
IF KEY^=1 THEN DF3=",";IF KEY^=1 THEN DF4=",";
IF KEY^=1 THEN DF5=",";IF KEY^=1 THEN DF6=",";
IF KEY^=1 THEN DF7=",";IF KEY^=1 THEN DF8=",";
IF KEY^=1 THEN DF9=",";IF KEY^=1 THEN DF10=",";
IF DF1=<0 THEN DF1=".";IF DF2=<0 THEN DF2=".";
IF DF3=<0 THEN DF3=".";IF DF4=<0 THEN DF4=".";
IF DF5=<0 THEN DF5=".";IF DF6=<0 THEN DF6=".";
IF DF7=<0 THEN DF7=".";IF DF8=<0 THEN DF8=".";
IF DF9=<0 THEN DF9=".";IF DF10=<0 THEN DF10=".";
IF DF1>28 THEN DF1=".";IF DF2>28 THEN DF2=".";
IF DF3>28 THEN DF3=".";IF DF4>28 THEN DF4=".";
IF DF5>28 THEN DF5=".";IF DF6>28 THEN DF6=".";
IF DF7>28 THEN DF7=".";IF DF8>28 THEN DF8=".";
IF DF9>28 THEN DF9=".";IF DF10>28 THEN DF10=".";
MAX_DF=MAX(OF DF1-DF10);
RUN; 

PROC FREQ DATA=A.DAT5 ; TABLES OUT; RUN;

/*uri에 따라 aom 있는군 (1)*/
data a.URI_M; set a.DAT5; 
if key=1;
IF MAX_DF>=0 & MAX_DF<=7 THEN OUT=1; 
ELSE IF MAX_DF>=8 & MAX_DF<=14 THEN OUT=2;
ELSE IF MAX_DF>15 & MAX_DF<=21 THEN OUT=3;
ELSE IF MAX_DF>22 & MAX_DF<=28 THEN OUT=4; ELSE OUT=5;
IF OUT=1 THEN URI1="Y"; ELSE URI1="N";
IF OUT=2 THEN URI2="Y"; ELSE URI2="N";
IF OUT=3 THEN URI3="Y"; ELSE URI3="N";
IF OUT=4 THEN URI4="Y"; ELSE URI4="N";
DROP N1 N2 DF1-DF10;
run;

data a.URI_M1; set a.URI_M; if key=1; if out=1 ;URI1="Y";keep cmn_key URI1;run;
data a.URI_M2; set a.URI_M; if key=1; if out<=2 ;URI2="Y";keep cmn_key URI2;run;
data a.URI_M3; set a.URI_M; if key=1; if out<=3 ;URI3="Y";keep cmn_key URI3;run;
data a.URI_M4; set a.URI_M; if key=1; if out<=4 ;URI4="Y";keep cmn_key URI4;run;

/*기존이랑 MERGE해서 URI 선행에 대해   Y/N 구분 */
proc sql; create table a.uri_aom1 as select * from a.aom_w4 left join a.URI_M1 on aom_w4.cmn_key = URI_M1.cmn_key; quit;
proc sql; create table a.uri_aom2 as select * from a.aom_w4 left join a.URI_M2 on aom_w4.cmn_key = URI_M2.cmn_key; quit;
proc sql; create table a.uri_aom3 as select * from a.aom_w4 left join a.URI_M3 on aom_w4.cmn_key = URI_M3.cmn_key; quit;
proc sql; create table a.uri_aom4 as select * from a.aom_w4 left join a.URI_M4 on aom_w4.cmn_key = URI_M4.cmn_key; quit;

DATA A.URI_AOM1; SET A.URI_AOM1; IF URI1^="Y" THEN URI1="N"; RUN;
DATA A.URI_AOM2; SET A.URI_AOM2; IF URI2^="Y" THEN URI2="N"; RUN;
DATA A.URI_AOM3; SET A.URI_AOM3; IF URI3^="Y" THEN URI3="N"; RUN;
DATA A.URI_AOM4; SET A.URI_AOM4; IF URI4^="Y" THEN URI4="N"; RUN;


PROC FREQ DATA=A.URI_AOM1 ; TABLES URI1; RUN;
PROC FREQ DATA=A.URI_AOM2 ; TABLES URI2; RUN;
PROC FREQ DATA=A.URI_AOM3 ; TABLES URI3; RUN;
PROC FREQ DATA=A.URI_AOM4 ; TABLES URI4; RUN;

data a.uri_aom1; set a.uri_aom1; uri=uri1;run;
data a.uri_aom2; set a.uri_aom2; uri=uri2;run;
data a.uri_aom3; set a.uri_aom3; uri=uri3;run;
data a.uri_aom4; set a.uri_aom4; uri=uri4;run;

/*uri선행에 따라 구분 */
%MACRO STEP6(T2,T1,OUT,K);
DATA A.&T2; SET A.&T1;

SIDO= SUBSTR(RVSN_ADDR_CD,1,2); IF SIDO IN (&K.);

IF AGE<=3 & URI="Y" THEN TOT_Y=1; ELSE TOT_Y=0;
IF AGE<=3 & URI="N" THEN TOT_N=1; ELSE TOT_N=0;

PROC SQL; 
CREATE TABLE A.&OUT AS SELECT START_DATE AS DATE, SIDO,SUM(TOT_Y) AS TOT_Y, SUM(TOT_N) AS TOT_N  FROM A.&T2 GROUP BY START_DATE;QUIT;

PROC SORT DATA= A.&OUT NODUPKEY; BY DATE; RUN;
%MEND STEP6;

%STEP6 (a1,URI_AOM1,URI1_S1,11);  %STEP6 (a1,URI_AOM2,URI2_S1,11); %STEP6 (a1,URI_AOM3,URI3_S1,11); %STEP6 (a1,URI_AOM4,URI4_S1,11); 
%STEP6 (a2,URI_AOM1,URI1_S2,26);  %STEP6 (a2,URI_AOM2,URI2_S2,26); %STEP6 (a2,URI_AOM3,URI3_S2,26); %STEP6 (a2,URI_AOM4,URI4_S2,26); 
%STEP6 (a3,URI_AOM1,URI1_S3,27);  %STEP6 (a3,URI_AOM2,URI2_S3,27); %STEP6 (a3,URI_AOM3,URI3_S3,27); %STEP6 (a3,URI_AOM4,URI4_S3,27);
%STEP6 (a4,URI_AOM1,URI1_S4,28);  %STEP6 (a4,URI_AOM2,URI2_S4,28); %STEP6 (a4,URI_AOM3,URI3_S4,28); %STEP6 (a4,URI_AOM4,URI4_S4,28); 
%STEP6 (a5,URI_AOM1,URI1_S5,29);  %STEP6 (a5,URI_AOM2,URI2_S5,29); %STEP6 (a5,URI_AOM3,URI3_S5,29); %STEP6 (a5,URI_AOM4,URI4_S5,29); 
%STEP6 (a6,URI_AOM1,URI1_S6,30);  %STEP6 (a6,URI_AOM2,URI2_S6,30); %STEP6 (a6,URI_AOM3,URI3_S6,30); %STEP6 (a6,URI_AOM4,URI4_S6,30);
%STEP6 (a7,URI_AOM1,URI1_S7,31);  %STEP6 (a7,URI_AOM2,URI2_S7,31); %STEP6 (a7,URI_AOM3,URI3_S7,31); %STEP6 (a7,URI_AOM4,URI4_S7,31); 

DATA A.URI1_COUNT; SET A.URI1_S1 A.URI1_S2 A.URI1_S3 A.URI1_S4 A.URI1_S5 A.URI1_S6 A.URI1_S7; RUN;
DATA A.URI2_COUNT; SET A.URI2_S1 A.URI2_S2 A.URI2_S3 A.URI2_S4 A.URI2_S5 A.URI2_S6 A.URI2_S7; RUN;
DATA A.URI3_COUNT; SET A.URI3_S1 A.URI3_S2 A.URI3_S3 A.URI3_S4 A.URI3_S5 A.URI3_S6 A.URI3_S7; RUN;
DATA A.URI4_COUNT; SET A.URI4_S1 A.URI4_S2 A.URI4_S3 A.URI4_S4 A.URI4_S5 A.URI4_S6 A.URI4_S7; RUN;

DATA A.URI1_COUNT ; SET A.URI1_COUNT; TOTAL=TOT_Y+TOT_N; RUN;
DATA A.URI2_COUNT ; SET A.URI2_COUNT; TOTAL=TOT_Y+TOT_N; RUN;
DATA A.URI3_COUNT ; SET A.URI3_COUNT; TOTAL=TOT_Y+TOT_N; RUN;
DATA A.URI4_COUNT ; SET A.URI4_COUNT; TOTAL=TOT_Y+TOT_N; RUN;

/*****************************************************************************************************************************************************************************************/
/*****************************************************************************************************************************************************************************************/
/*****************************************************************************************************************************************************************************************/
/*uri선행에 따라 구분 */
%MACRO STEP6(T2,T1,OUT,K);
DATA A.&T2; SET A.&T1;

SIDO= SUBSTR(RVSN_ADDR_CD,1,2); IF SIDO IN (&K.);

IF AGE<=3 & OUT=0 THEN E0=1; ELSE E0=0;
IF AGE<=3 & OUT=1 THEN E1=1; ELSE E1=0;
IF AGE<=3 & OUT=2 THEN E2=1; ELSE E2=0;
IF AGE<=3 & OUT=3 THEN E3=1; ELSE E3=0;
IF AGE<=3 & OUT=4 THEN E4=1; ELSE E4=0;

PROC SQL; 
CREATE TABLE A.&OUT AS SELECT START_DATE AS DATE, SIDO,SUM(E0) AS E0, SUM(E1) AS E1, SUM(E2) AS E2, SUM(E3) AS E3, SUM(E4) AS E4 
FROM A.&T2 GROUP BY START_DATE;QUIT;

PROC SORT DATA= A.&OUT NODUPKEY; BY DATE; RUN;
%MEND STEP6;

%STEP6 (a1,URI_AOM5,URI5_S1,11);  
%STEP6 (a2,URI_AOM5,URI5_S2,26);  
%STEP6 (a3,URI_AOM5,URI5_S3,27);  
%STEP6 (a4,URI_AOM5,URI5_S4,28);  
%STEP6 (a5,URI_AOM5,URI5_S5,29);  
%STEP6 (a6,URI_AOM5,URI5_S6,30);  
%STEP6 (a7,URI_AOM5,URI5_S7,31);  

DATA A.URI5_COUNT; SET A.URI5_S1 A.URI5_S2 A.URI5_S3 A.URI5_S4 A.URI5_S5 A.URI5_S6 A.URI5_S7; RUN;

DATA A.URI5_COUNT ; SET A.URI5_COUNT; TOTAL=E0+E1+E2+E3+E4; RUN;

