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
/*AOM 환자 중에서 Allergic rihinitis를 겪은 적이 있는 환자  몇 퍼센트 인지 ? */
proc sort data=a.s4 out=A.id nodupkey ; by indi_dscm_no; run;

%STEP5 (a1,AOM_W4,S1_W4_COUNT,11); 
%STEP5 (a2,AOM_W4,S2_W4_COUNT,26); 
%STEP5 (a3,AOM_W4,S3_W4_COUNT,27); 
%STEP5 (a4,AOM_W4,S4_W4_COUNT,28); 
%STEP5 (a5,AOM_W4,S5_W4_COUNT,29); 
%STEP5 (a6,AOM_W4,S6_W4_COUNT,30); 
%STEP5 (a7,AOM_W4,S7_W4_COUNT,31); 

%Macro AR(TABLE1,TABLE2);
data A.&table1 ; set A.&table2;
IF FORM_CD IN("02","03") AND SUBSTR(SICK_SYM1,1,4) IN ("J301","J302","J305","J308","J309") THEN K1=2; ELSE K1=0; /*입원 이면서 주상병 IHD 코드, K1은 주상병에 존재하면 2*/
IF FORM_CD IN("02","03") AND SUBSTR(SICK_SYM2,1,4) IN ("J301","J302","J305","J308","J309") THEN K2=1; ELSE K2=0; /*입원 이면서 부상병 IHD 코드, K2은 부상병에 존재하면 1*/
ICD_RANK=K1+K2; IF ICD_RANK>0; RUN; /*ICD_RANK 주+부상병 중요도 순위 나타냄*/
%MEND AR;
%AR(TOTAL_AR_08,T20_2008) %AR(TOTAL_AR_09,T20_2009)
%AR(TOTAL_AR_10,T20_2010) %AR(TOTAL_AR_11,T20_2011)
%AR(TOTAL_AR_12,T20_2012) %AR(TOTAL_AR_13,T20_2013)
%AR(TOTAL_AR_14,T20_2014) %AR(TOTAL_AR_15,T20_2015)
%AR(TOTAL_AR_16,T20_2016) %AR(TOTAL_AR_17,T20_2017)
%T_append (TOTAL_AR);
/*자격이랑 MERGE KEY 생성 : PKEY*/
DATA  A.TOTAL_AR ; SET A.TOTAL_AR;   PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4)) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO);RUN;
/*자격이랑  MERGE BY : PKEY*/
PROC SQL; CREATE TABLE A.TOTAL_AR      AS SELECT * FROM B.TOTAL_AR, A.JK WHERE TOTAL_AR.PKEY =JK.PKEY;QUIT;
/**********************************************************************************************************************************************************/
%STEP1(TOTAL,_AR);
%STEP2(AR1,TOTAL_AR);
%STEP3(AR2,AR1);

DATA A.TOTAL_AR_07; SET A.TOTAL_AR_07;
IF SUBSTR(SICK_SYM1,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM2,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM3,1,4) NOT IN ("H650","H651","H660"); RUN;

DATA A.TOTAL_AR_08; SET A.TOTAL_AR_08;
IF SUBSTR(SICK_SYM1,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM2,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM3,1,4) NOT IN ("H650","H651","H660"); RUN;

DATA A.TOTAL_AR_09; SET A.TOTAL_AR_09;
IF SUBSTR(SICK_SYM1,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM2,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM3,1,4) NOT IN ("H650","H651","H660"); RUN;

DATA A.TOTAL_AR_10; SET A.TOTAL_AR_10;
IF SUBSTR(SICK_SYM1,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM2,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM3,1,4) NOT IN ("H650","H651","H660"); RUN;

DATA A.TOTAL_AR_11; SET A.TOTAL_AR_11;
IF SUBSTR(SICK_SYM1,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM2,1,4) NOT IN ("H650","H651","H660");
IF SUBSTR(SICK_SYM3,1,4) NOT IN ("H650","H651","H660"); RUN;

PROC SQL; CREATE TABLE A.TOTAL_AR_08 AS SELECT * FROM A.TOTAL_AR_08 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_AR_09 AS SELECT * FROM A.TOTAL_AR_09 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_AR_10 AS SELECT * FROM A.TOTAL_AR_10 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_AR_11 AS SELECT * FROM A.TOTAL_AR_11 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_AR_12 AS SELECT * FROM A.TOTAL_AR_12 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_AR_13 AS SELECT * FROM A.TOTAL_AR_13 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_AR_14 AS SELECT * FROM A.TOTAL_AR_14 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_AR_15 AS SELECT * FROM A.TOTAL_AR_15 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;
PROC SQL; CREATE TABLE A.TOTAL_AR_16 AS SELECT * FROM A.TOTAL_AR_16 WHERE INDI_DSCM_NO IN (SELECT ID.INDI_DSCM_NO FROM A.ID); QUIT;

DATA A.AR ; SET A.TOTAL_AR_08 A.TOTAL_AR_09 A.TOTAL_AR_10 A.TOTAL_AR_11 A.TOTAL_AR_12 A.TOTAL_AR_13 A.TOTAL_AR_14 A.TOTAL_AR_15 A.TOTAL_AR_16;RUN;

DATA A.AR; SET A.AR; KEEP CMN_KEY INDI_DSCM_NO MDCARE_STRT_DT SICK_SYM1 SICK_SYM2 ; RUN;

DATA A.AOM; SET A.AOM_W4 ; if age<4; KEEP CMN_KEY INDI_DSCM_NO MDCARE_STRT_DT SICK_SYM1 SICK_SYM2; RUN;

DATA A.AOM ; SET A.AOM ; KEY=1; RUN;
DATA A.AR ; SET A.AR ; KEY=0; RUN;

DATA A.AR_M ; SET A.AOM A.AR; RUN;

PROC SORT DATA=A.AR_M ; BY INDI_DSCM_NO MDCARE_STRT_DT;RUN;

DATA A.AR_M2; SET A.AR_M;
IF substr(MDCARE_STRT_DT,1,4)<2017;
DAY=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
IF LAG(INDI_DSCM_NO)=INDI_DSCM_NO THEN N1="T" ; ELSE N1="F";  /*이전 시점에 ID가 같은지*/
IF LAG(KEY)=0 & KEY=1 THEN N2="T" ; ELSE N2="F";                        /*이전 시점에 allergic rihinitis 있는 경우 */
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

/*uri에 따라 aom 있는군 (1)*/
data a.AR_M3; set a.AR_M2; 
if key=1;
IF MAX_DF>=0 & MAX_DF<=7 THEN OUT=1; 
ELSE IF MAX_DF>=8 & MAX_DF<=14 THEN OUT=2;
ELSE IF MAX_DF>15 & MAX_DF<=21 THEN OUT=3;
ELSE IF MAX_DF>22 & MAX_DF<=28 THEN OUT=4; ELSE OUT=5;
IF OUT=1 THEN AR1="Y"; ELSE AR1="N";
IF OUT=2 THEN AR2="Y"; ELSE AR2="N";
IF OUT=3 THEN AR3="Y"; ELSE AR3="N";
IF OUT=4 THEN AR4="Y"; ELSE AR4="N";
DROP N1 N2 DF1-DF10;
run;
data a.AR_MM1; set a.AR_M3; if key=1; if out=1 ;AR1="Y";keep cmn_key AR1;run;
data a.AR_MM2; set a.AR_M3; if key=1; if out<=2 ;AR2="Y";keep cmn_key AR2;run;
data a.AR_MM3; set a.AR_M3; if key=1; if out<=3 ;AR3="Y";keep cmn_key AR3;run;
data a.AR_MM4; set a.AR_M3; if key=1; if out<=4 ;AR4="Y";keep cmn_key AR4;run;

/*기존이랑 MERGE해서 AR 선행에 대해   Y/N 구분 */
proc sql; create table a.AR_aom1 as select * from a.aom_w4 left join a.AR_MM1 on aom_w4.cmn_key = AR_MM1.cmn_key; quit;
proc sql; create table a.AR_aom2 as select * from a.aom_w4 left join a.AR_MM2 on aom_w4.cmn_key = AR_MM2.cmn_key; quit;
proc sql; create table a.AR_aom3 as select * from a.aom_w4 left join a.AR_MM3 on aom_w4.cmn_key = AR_MM3.cmn_key; quit;
proc sql; create table a.AR_aom4 as select * from a.aom_w4 left join a.AR_MM4 on aom_w4.cmn_key = AR_MM4.cmn_key; quit;

DATA A.AR_AOM1; SET A.AR_AOM1; IF AR1^="Y" THEN AR1="N"; RUN;
DATA A.AR_AOM2; SET A.AR_AOM2; IF AR2^="Y" THEN AR2="N"; RUN;
DATA A.AR_AOM3; SET A.AR_AOM3; IF AR3^="Y" THEN AR3="N"; RUN;
DATA A.AR_AOM4; SET A.AR_AOM4; IF AR4^="Y" THEN AR4="N"; RUN;

PROC FREQ DATA=A.AR_AOM1 ; TABLES AR1; RUN;
PROC FREQ DATA=A.AR_AOM2 ; TABLES AR2; RUN;
PROC FREQ DATA=A.AR_AOM3 ; TABLES AR3; RUN;
PROC FREQ DATA=A.AR_AOM4 ; TABLES AR4; RUN;

data a.ar_aom1; set a.ar_aom1; ar=ar1;run;
data a.ar_aom2; set a.ar_aom2; ar=ar2;run;
data a.ar_aom3; set a.ar_aom3; ar=ar3;run;
data a.ar_aom4; set a.ar_aom4; ar=ar4;run;

/*AR선행에 따라 구분 */
%MACRO STEP6(T2,T1,OUT,K);
DATA A.&T2; SET A.&T1;

SIDO= SUBSTR(RVSN_ADDR_CD,1,2);
IF SIDO IN (&K.);

IF AGE<=3 & AR="Y" THEN TOT_Y=1; ELSE TOT_Y=0;
IF AGE<=3 & AR="N" THEN TOT_N=1; ELSE TOT_N=0;

PROC SQL; 
CREATE TABLE A.&OUT AS SELECT START_DATE AS DATE, SIDO,SUM(TOT_Y) AS TOT_Y, SUM(TOT_N) AS TOT_N  FROM A.&T2 GROUP BY START_DATE;QUIT;

PROC SORT DATA= A.&OUT NODUPKEY; BY DATE; RUN;
%MEND STEP6;

%STEP6 (b1,AR_AOM1,AR1_S1,11);  %STEP6 (b1,AR_AOM2,AR2_S1,11); %STEP6 (b1,AR_AOM3,AR3_S1,11); %STEP6 (b1,AR_AOM4,AR4_S1,11); 
%STEP6 (b2,AR_AOM1,AR1_S2,26);  %STEP6 (b2,AR_AOM2,AR2_S2,26); %STEP6 (b2,AR_AOM3,AR3_S2,26); %STEP6 (b2,AR_AOM4,AR4_S2,26); 
%STEP6 (b3,AR_AOM1,AR1_S3,27);  %STEP6 (b3,AR_AOM2,AR2_S3,27); %STEP6 (b3,AR_AOM3,AR3_S3,27); %STEP6 (b3,AR_AOM4,AR4_S3,27);
%STEP6 (b4,AR_AOM1,AR1_S4,28);  %STEP6 (b4,AR_AOM2,AR2_S4,28); %STEP6 (b4,AR_AOM3,AR3_S4,28); %STEP6 (b4,AR_AOM4,AR4_S4,28); 
%STEP6 (b5,AR_AOM1,AR1_S5,29);  %STEP6 (b5,AR_AOM2,AR2_S5,29); %STEP6 (b5,AR_AOM3,AR3_S5,29); %STEP6 (b5,AR_AOM4,AR4_S5,29); 
%STEP6 (b6,AR_AOM1,AR1_S6,30);  %STEP6 (b6,AR_AOM2,AR2_S6,30); %STEP6 (b6,AR_AOM3,AR3_S6,30); %STEP6 (b6,AR_AOM4,AR4_S6,30);
%STEP6 (b7,AR_AOM1,AR1_S7,31);  %STEP6 (b7,AR_AOM2,AR2_S7,31); %STEP6 (b7,AR_AOM3,AR3_S7,31); %STEP6 (b7,AR_AOM4,AR4_S7,31); 

DATA A.AR1_COUNT; SET A.AR1_S1 A.AR1_S2 A.AR1_S3 A.AR1_S4 A.AR1_S5 A.AR1_S6 A.AR1_S7; RUN;
DATA A.AR2_COUNT; SET A.AR2_S1 A.AR2_S2 A.AR2_S3 A.AR2_S4 A.AR2_S5 A.AR2_S6 A.AR2_S7; RUN;
DATA A.AR3_COUNT; SET A.AR3_S1 A.AR3_S2 A.AR3_S3 A.AR3_S4 A.AR3_S5 A.AR3_S6 A.AR3_S7; RUN;
DATA A.AR4_COUNT; SET A.AR4_S1 A.AR4_S2 A.AR4_S3 A.AR4_S4 A.AR4_S5 A.AR4_S6 A.AR4_S7; RUN;

DATA A.AR1_COUNT ; SET A.AR1_COUNT; TOTAL=TOT_Y+TOT_N; RUN;
DATA A.AR2_COUNT ; SET A.AR2_COUNT; TOTAL=TOT_Y+TOT_N; RUN;
DATA A.AR3_COUNT ; SET A.AR3_COUNT; TOTAL=TOT_Y+TOT_N; RUN;
DATA A.AR4_COUNT ; SET A.AR4_COUNT; TOTAL=TOT_Y+TOT_N; RUN;

/************************************************************************************************************************************************************/
/************************************************************************************************************************************************************/
/************************************************************************************************************************************************************/


/*uri에 따라 aom 있는군 (1)*/
data a.AR_M3; set a.AR_M2; 
if key=1;
IF MAX_DF>=0 & MAX_DF<=7 THEN OUT=1; 
ELSE IF MAX_DF>=8 & MAX_DF<=14 THEN OUT=2;
ELSE IF MAX_DF>15 & MAX_DF<=21 THEN OUT=3;
ELSE IF MAX_DF>22 & MAX_DF<=28 THEN OUT=4; ELSE OUT=".";
IF OUT=1 THEN AR1="Y"; ELSE AR1="N";
IF OUT=2 THEN AR2="Y"; ELSE AR2="N";
IF OUT=3 THEN AR3="Y"; ELSE AR3="N";
IF OUT=4 THEN AR4="Y"; ELSE AR4="N";
DROP N1 N2 DF1-DF10;
run;
data a.AR_MM1; set a.AR_M3; if key=1; if out=1 ;AR1="Y";keep cmn_key AR1;run;
data a.AR_MM2; set a.AR_M3; if key=1; if out=2 ;AR2="Y";keep cmn_key AR2;run;
data a.AR_MM3; set a.AR_M3; if key=1; if out=3 ;AR3="Y";keep cmn_key AR3;run;
data a.AR_MM4; set a.AR_M3; if key=1; if out=4 ;AR4="Y";keep cmn_key AR4;run;
data a.AR_MM5; set a.AR_M3; if key=1; if out<=4 ;keep cmn_key AR1 AR2 AR3 AR4 OUT;run;
/*기존이랑 MERGE해서 AR 선행에 대해   Y/N 구분 */
proc sql; create table a.AR_aom1 as select * from a.aom_w4 left join a.AR_MM1 on aom_w4.cmn_key = AR_MM1.cmn_key; quit;
proc sql; create table a.AR_aom2 as select * from a.aom_w4 left join a.AR_MM2 on aom_w4.cmn_key = AR_MM2.cmn_key; quit;
proc sql; create table a.AR_aom3 as select * from a.aom_w4 left join a.AR_MM3 on aom_w4.cmn_key = AR_MM3.cmn_key; quit;
proc sql; create table a.AR_aom4 as select * from a.aom_w4 left join a.AR_MM4 on aom_w4.cmn_key = AR_MM4.cmn_key; quit;
proc sql; create table a.AR_aom5 as select * from a.aom_w4 left join a.AR_MM5 on aom_w4.cmn_key = AR_MM5.cmn_key; quit;

DATA A.AR_AOM1; SET A.AR_AOM1; IF AR1^="Y" THEN AR1="N"; RUN;
DATA A.AR_AOM2; SET A.AR_AOM2; IF AR2^="Y" THEN AR2="N"; RUN;
DATA A.AR_AOM3; SET A.AR_AOM3; IF AR3^="Y" THEN AR3="N"; RUN;
DATA A.AR_AOM4; SET A.AR_AOM4; IF AR4^="Y" THEN AR4="N"; RUN;
DATA A.AR_AOM5; SET A.AR_AOM5; IF OUT="."   THEN OUT=0; RUN;

PROC FREQ DATA=A.AR_AOM1 ; TABLES AR1; RUN;
PROC FREQ DATA=A.AR_AOM2 ; TABLES AR2; RUN;
PROC FREQ DATA=A.AR_AOM3 ; TABLES AR3; RUN;
PROC FREQ DATA=A.AR_AOM4 ; TABLES AR4; RUN;
PROC FREQ DATA=A.AR_AOM5 ; TABLES OUT; RUN;

data a.ar_aom1; set a.ar_aom1; ar=ar1;run;
data a.ar_aom2; set a.ar_aom2; ar=ar2;run;
data a.ar_aom3; set a.ar_aom3; ar=ar3;run;
data a.ar_aom4; set a.ar_aom4; ar=ar4;run;

/*AR선행에 따라 구분 */
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

%STEP6 (a1,AR_AOM5,AR5_S1,11);  
%STEP6 (a2,AR_AOM5,AR5_S2,26);  
%STEP6 (a3,AR_AOM5,AR5_S3,27);  
%STEP6 (a4,AR_AOM5,AR5_S4,28);  
%STEP6 (a5,AR_AOM5,AR5_S5,29);  
%STEP6 (a6,AR_AOM5,AR5_S6,30);  
%STEP6 (a7,AR_AOM5,AR5_S7,31);  

DATA A.AR5_COUNT; SET A.AR5_S1 A.AR5_S2 A.AR5_S3 A.AR5_S4 A.AR5_S5 A.AR5_S6 A.AR5_S7; RUN;

DATA A.AR5_COUNT ; SET A.AR5_COUNT; TOTAL=E0+E1+E2+E3+E4; RUN;
