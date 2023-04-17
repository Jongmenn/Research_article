

/**********************************************************************************************/
/**********************************************************************************************/

/*연구 대상자 정의
(1) 국민행복카드 DB 있는 임신부 중에서
(2) 엄마-아이 DB랑 연계 가능
(3) 임신 시점을 분만예정일-280일로 계산하고
(4) 자격연계는 임신 시점(3)으로 연계
*/
/**********************************************************************************************/
/**********************************************************************************************/
/*HANA DB SQL 추출 기본 형태*/
/*자료 추출 기본 형태*/
PROC SQL; 
&CONNECT.;
CREATE TABLE T_NAME AS SELECT * FROM CONNECTION TO X1(
--
);DISCONNECT FROM X1;
QUIT; 
/*중복 테이블 있을시 DROP하고 추출*/
PROC SQL; DROP TABLE SAPTMP.T_NAME; 
&CONNECT.;
EXECUTE(
CREATE TABLE T_NAME AS (
--
))BY X1; DISCONNECT FROM X1;
QUIT;

/**********************************************************************************************/
/**********************************************************************************************/
/**상위 10개 자료 확인**/
PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT TOP 10*
    FROM NHISBDA.COVID_PT_OPEN_SPC  /**확진자 DB**/
);DISCONNECT FROM X1;
QUIT; 
/**********************************************************************************************/
PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT TOP 10*
    FROM NHIS_220051.EXP_DLY_DT  /**임신주수 & 임신부 전수**/
);DISCONNECT FROM X1;
QUIT; 
/**********************************************************************************************/
PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT count(INDI_DSCM_NO) as cnt, count(distInct indi_dscm_no) as unique_cnt ,min(MTNT_SCD_DT) AS MIN_DT,
	MAX(MTNT_SCD_DT) AS MAX_DT 
    FROM NHIS_220051.EXP_DLY_DT  /**임신주수 & 임신부 전수**/
);DISCONNECT FROM X1;
QUIT; 
/**********************************************************************************************/

/*국민행복카드 DB 분만예정일자 대상자*/
PROC SQL; 
&CONNECT.;
CREATE TABLE TARGET AS SELECT * FROM CONNECTION TO X1(
    SELECT * FROM NHIS_220051.EXP_DLY_DT  /**임신주수 & 임신부 전수**/
);DISCONNECT FROM X1;
QUIT; 

data aa.target ; set target; run;

/*국민행복카드에서 대상자수 파악*/
PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT count(INDI_DSCM_NO) as cnt, count(distInct indi_dscm_no) as unique_cnt ,min(MTNT_SCD_DT) AS MIN_DT,
	MAX(MTNT_SCD_DT) AS MAX_DT 
    FROM NHIS_220051.EXP_DLY_DT  /**임신주수 & 임신부 전수**/
);DISCONNECT FROM X1;
QUIT; 

PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT TOP 10*
    FROM NHISBDA.HHDV_MOTHER_CHILD_LINK  /**확진자 DB**/
);DISCONNECT FROM X1;
QUIT; 

/*엄마-아이 연계 DB 대상자 수 파악 2002~2022.05*/
PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT count(CHILD_ID) as CHD_cnt, count(distInct CHILD_ID) as unique_CHD_cnt,
           count(MOTHER_ID) as MTH_cnt, count(distInct MOTHER_ID) as unique_MTH_cnt,
           min(DLV_DT) AS MIN_DLV_DT, MAX(DLV_DT) AS MAX_DLV_DT 
    FROM NHISBDA.HHDV_MOTHER_CHILD_LINK  /**임신주수 & 임신부 전수**/
);DISCONNECT FROM X1;
QUIT; 

/*2020~2022년까지 분만 이력 있는 산모만 추리기*/
PROC SQL; 
&CONNECT.;
CREATE TABLE MC AS SELECT * FROM CONNECTION TO X1(
SELECT * FROM NHISBDA.HHDV_MOTHER_CHILD_LINK   /**임신주수 & 임신부 전수**/
WHERE SUBSTR(DLV_DT,1,4) IN (2020,2021,2022)   /*2020~2022.05*/
);DISCONNECT FROM X1;
QUIT; 

DATA AA.MC     ; SET MC  ; DLV_YYYY=SUBSTR(DLV_DT,1,4); DLV_MONTH=SUBSTR(DLV_DT,5,2); RUN;
DATA SAPTMP.MC; SET AA.MC; DLV_YYYY=SUBSTR(DLV_DT,1,4); DLV_MONTH=SUBSTR(DLV_DT,5,2); RUN;

PROC FREQ DATA=AA.MC; TABLES DLV_YYYY*DLV_MONTH/LIST; RUN;

PROC SQL; DROP TABLE SAPTMP.WORK; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT count(CHILD_ID) as CHD_cnt, count(distInct CHILD_ID) as unique_CHD_cnt,
           count(MOTHER_ID) as MTH_cnt, count(distInct MOTHER_ID) as unique_MTH_cnt,
           min(DLV_DT) AS MIN_DLV_DT, MAX(DLV_DT) AS MAX_DLV_DT 
    FROM MC  /**임신주수 & 임신부 전수**/
);DISCONNECT FROM X1;
QUIT; 

/**********************************************************************************************/
/*엄마-아이 대상자와 국민행복카드 임신주수 있는 대상자 연계*/
PROC SQL; DROP TABLE SAPTMP.JOIN1;
&CONNECT.;
EXECUTE(
CREATE TABLE JOIN1 AS

(SELECT A.*,B.MTNT_SCD_DT
FROM MC A left join NHIS_220051.EXP_DLY_DT B  
ON A.MOTHER_ID=B.INDI_DSCM_NO AND A.DLV_YYYY BETWEEN '2020' AND '2022'
)
)BY X1; DISCONNECT FROM X1;QUIT;

PROC SQL; DROP TABLE SAPTMP.WORK; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT count(MOTHER_ID) as MTH_cnt, count(distInct MOTHER_ID) as unique_MTH_cnt 
    FROM JOIN1  /**임신주수 & 임신부 전수**/
);DISCONNECT FROM X1;
QUIT; 

/*분만예정일 없는 경우 제외*/
/*분만일-분만예정일 날짜 차이 많이나는 경우 제외(코챈스 기준 적용)-150~+50*/
DATA AA.Z; 
RETAIN CHILD_ID SEX_TYPE MOTHER_ID DLV_DT MTNT_SCD_DT DIFF_DATE;
SET SAPTMP.JOIN1;  
if MTNT_SCD_DT^="";
DATE1=MDY(SUBSTR(DLV_DT,5,2),SUBSTR(DLV_DT,7,2),SUBSTR(DLV_DT,1,4));               /*분만일(분만코드로 입원한 날짜)*/
DATE2=MDY(SUBSTR(MTNT_SCD_DT,5,2),SUBSTR(MTNT_SCD_DT,7,2),SUBSTR(MTNT_SCD_DT,1,4));/*분만예정일*/
DIFF_DATE=DATE1-DATE2;
IF DIFF_DATE>=-150 & DIFF_DATE <=50; /*-150~+50*/
FORMAT DATE1 DATE10. DATE2 DATE10.;
DROP DATE1 DATE2;
RUN;

PROC FREQ DATA=AA.Z; TABLES DIFF_DATE; RUN;

proc sort data=aa.z; by CHILD_ID MOTHER_ID  descending diff_date; RUN;
/*한 엄마가 분만예정일 여러번 붙은 경우*/
PROC SQL; CREATE TABLE ZZ
AS SELECT MOTHER_ID AS ID, COUNT(MOTHER_ID) AS CNT FROM AA.Z GROUP BY MOTHER_ID; QUIT;
PROC FREQ DATA=ZZ; TABLES CNT; RUN;

/*산모 1명만 붙은 경우: 분만예정일 변수*/
DATA Z_CNT1; SET ZZ; IF CNT=1; RUN;
/*산모 2명 이상 붙은 경우: 분만예정일 변수*/
DATA Z_CNT2; SET ZZ; IF CNT>=2; RUN;

proc sort data=z_cnt2; by CHILD_ID MOTHER_ID DATE_DIFF; RUN;
proc sort data=aa.z; by MOTHER_ID MTNT_SCD_DT; RUN;

/*연구기간동안 한 산모당 분만예정일 1번 연계되는 경우 */
PROC SQL;CREATE TABLE Z_DUP1 AS SELECT * FROM AA.Z WHERE MOTHER_ID IN(SELECT ID FROM Z_CNT1); QUIT;
/*연구기간동안 한 산모당 분만예정일 2번 이상 연계되는 경우 */
PROC SQL;CREATE TABLE Z_DUP2 AS SELECT * FROM AA.Z WHERE MOTHER_ID IN(SELECT ID FROM Z_CNT2); QUIT;

/*엄마-아이쌍 킷값 중복인 대상자 찾기*/
DATA Z_DUP2; SET Z_DUP2;
KEY=COMPRESS(LEFT(CHILD_ID))||("-")||COMPRESS(LEFT(MOTHER_ID)); RUN;

PROC SQL;CREATE TABLE MC_DUPKEY AS SELECT KEY, COUNT(KEY) AS CNT FROM Z_DUP2 GROUP BY KEY; QUIT;
DATA MC_DUPKEY; SET MC_DUPKEY; IF CNT>=2; RUN;

/*엄마-아이쌍 키 값에서 중복있는 경우*/
PROC SQL; CREATE TABLE Z_DUP2_REV1 AS SELECT * FROM Z_DUP2 WHERE KEY IN (SELECT KEY FROM MC_DUPKEY); QUIT;
/*엄마-아이쌍 키 값에서 중복없는 경우*/
PROC SQL; CREATE TABLE Z_DUP2_REV2 AS SELECT * FROM Z_DUP2 WHERE KEY NOT IN (SELECT KEY FROM MC_DUPKEY); QUIT;

/*(1)엄마-아이쌍 중복 제거 (2) 날짜 우선순위로 분만예정일 뒷날인애 먼저 살리기*/
PROC SORT DATA=Z_DUP2_REV1 NODUPKEY; BY KEY; RUN;
PROC SORT DATA=Z_DUP2_REV2 NODUPKEY; BY KEY; RUN;
/*변수정리*/
DATA Z_DUP2_REV1; SET Z_DUP2_REV1; DROP KEY; RUN;
DATA Z_DUP2_REV2; SET Z_DUP2_REV2; DROP KEY; RUN;

DATA Z_DUP2_F; SET Z_DUP2_REV1 Z_DUP2_REV2; RUN;

PROC FREQ DATA=Z_DUP2_F; TABLES DLV_YYYY*DLV_MONTH/LIST; RUN;

DATA TARGET; SET Z_DUP1 Z_DUP2_F; 
DATE1=MDY(SUBSTR(DLV_DT,5,2),SUBSTR(DLV_DT,7,2),SUBSTR(DLV_DT,1,4));               /*분만일(분만코드로 입원한 날짜)*/
DATE2=MDY(SUBSTR(MTNT_SCD_DT,5,2),SUBSTR(MTNT_SCD_DT,7,2),SUBSTR(MTNT_SCD_DT,1,4));/*분만예정일*/

preg_date=date2-280; /*임신 시작일(추정):분만예정일- 280*/
GA       =date1-preg_date; /*Gestational age*/
GA_wk    =GA/7;
preg_ddate=put(preg_date,yymmddn8.);
FORMAT DATE1 DATE10. DATE2 DATE10. preg_date date10.;
DROP DATE1 DATE2;
drop diff_date;
RUN;

proc freq data=target; tables ga; run;

proc sql; create table z as select mother_id as id, count(mother_id) as cnt from target group by mother_id; quit;
proc freq data=z;tables cnt; run;

/*분만-출산DB& 임신주수 연계한 최종 자료 -> AA, SAPTMP에 옮김*/
data saptmp.TARGET; set AA.TARGET;
rename sex_type=CHD_sex;
PREG_YEAR=SUBSTR(PREG_DDATE,1,4);  run;


/**********************************************************************************************/
/**********************************************************************************************/
/*JOIN*/
/*HANA DB내에서 자료 merge할 때 앞에 위치 안붙여줘도 알아서 SAPTMP 자료씀 근데, SAPTMP에 자료있는지 확인 필요*/
PROC SQL; 
&CONNECT.;
EXECUTE(
CREATE TABLE JK_TARGET AS
(SELECT * FROM TARGET A left join NHISBDA.HHDV_DSES_YY B  
ON A.MOTHER_ID=B.INDI_DSCM_NO WHERE A.PREG_YEAR=B.STD_YYYY
)
)BY X1; DISCONNECT FROM X1;QUIT;

/*데이터 대상자별 건수(중복 허용), 고유 대상자*/
PROC SQL;
CREATE TABLE ZZ AS SELECT COUNT(INDI_DSCM_NO) AS CNT, 
count(distInct indi_dscm_no) as unique_cnt FROM SAPTMP.JK_TARGET; RUN;
/**********************************************************************************************/
/*자격 연계후에 거주지 지역 변수 만들고 거주지 지역 MISSING 있는지 검토*/
/*1st data cleaning*/
DATA AA.JK_TARGET; set saptmp.JK_TARGET;

/*거주지 변수 만들기 시도/시군구*/
MTH_SIDO=SUBSTR(RVSN_ADDR_CD,1,2);
MTH_SGG =SUBSTR(RVSN_ADDR_CD,1,5);
/*출생아 성별 무효 제외*/
/*엄마 성별 무효 제외*/
/*연령 결측 없음: 확인함*/
if CHD_SEX IN (1,2);
IF SEX_TYPE=2;
IF MTH_SIDO^="";
IF FOREIGNER_Y="Y" THEN FOREIGNER_YN=1; ELSE FOREIGNER_YN=0;  /*엄마 외국인 유무*/
DROP INDI_DSCM_NO;
RUN;

/*자격 연계 후 몇몇 변수들 빈도 검토, 결측 있나 없나*/
PROC FREQ DATA=AA.JK_TARGET; TABLES MTH_sido; RUN;
PROC FREQ DATA=AA.JK_TARGET; TABLES FOREIGNER_YN; RUN;
PROC FREQ DATA=AA.JK_TARGET; TABLES YEND_STD_AGE; RUN;
PROC FREQ DATA=AA.JK_TARGET; TABLES PREG_YEAR; RUN;
PROC FREQ DATA=AA.JK_TARGET; TABLES DLV_YYYY; RUN;

/*AA 테이블에서 데이터 클리닝 및 변수 추가함*/
/*기존 자격& 대상자 연계한 자료 (JK_TARGET) SAPTMP에서 삭제후 덮어씌우기 */
DATA AA.JK_TARGET; SET AA.JK_TARGET; DROP INDI_DSCM_NO; RUN;
DATA SAPTMP.JK_TARGET; SET AA.JK_TARGET; RUN;

/**********************************************************************************************/
/**코로나 확진자 (감염 대상자) DB 조회: 상위 10개 자료 확인**/
PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT TOP 10*
    FROM NHISBDA.COVID_PT_OPEN_SPC  /**확진자 DB**/
);DISCONNECT FROM X1;
QUIT; 

/**코로나 확진자 빈도 **/
PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT count(INDI_DSCM_NO) as cnt, count(distInct indi_dscm_no) as unique_cnt ,min(CDX_DT) AS MIN_CDXDT,
	MAX(CDX_DT) AS MAX_CDXDT 
    FROM NHISBDA.COVID_PT_OPEN_SPC   /**확진자 DB**/
);DISCONNECT FROM X1;
QUIT; 
/*우리 연구(이대)에서는 COVID-19 처음 감염자만 고려, 재감염은 고려X (김충종 교수님 의견)*/
/*임신 기간 전에 COVID-19 감염인경우도 제외 이건 뒤에서 자료 정리할 때!!!!!!*/
/**확진자의 첫 확진일자 추출**/
PROC SQL; 
&CONNECT.;
EXECUTE(
CREATE TABLE COVID_FIRST AS (
SELECT DISTINCT INDI_DSCM_NO, CDX_DT 
FROM (SELECT *, ROW_NUMBER() OVER (PARTITION BY INDI_DSCM_NO ORDER BY INDI_DSCM_NO, CDX_DT) AS RN
           FROM NHISBDA.COVID_PT_OPEN_SPC
           WHERE INDI_DSCM_NO IS NOT NULL
               AND CDX_DT<='20220331')
WHERE RN=1
)
)BY X1; DISCONNECT FROM X1;
QUIT;
DATA AA.COVID_FIRST; SET SAPTMP.COVID_FIRST; RUN;

/*JOIN*/
/*임신부 대상자에 코로나 처음 확진자 DB 연계*/
PROC SQL; 
&CONNECT.;
EXECUTE(
CREATE TABLE TARGET_COVID AS
(SELECT * FROM JK_TARGET A left join COVID_FIRST B
ON A.MOTHER_ID=B.INDI_DSCM_NO
)
)BY X1; DISCONNECT FROM X1;QUIT;

DATA AA.TARGET_COVID; SET SAPTMP.TARGET_COVID;  RUN;
/*연계 자료 변수 가공*/
DATA AA.TARGET_COVID; SET SAPTMP.TARGET_COVID; 

/*코로나 감염 유무(히스토리) 임신 전, 임신 중, 분만 후 든 모두 고려되어있음*/
/*이 중에 임신 중 COVID-19 인 경우만 우리 연구 임신부 COVID 감염 대상자로 정의*/
IF CDX_DT^="" THEN COVID_H=1; ELSE COVID_H=0; 

/*코로나 감염 시기 년/년월*/
CDX_YEAR=SUBSTR(CDX_DT,1,4);
CDX_YYYYMM=SUBSTR(CDX_DT,1,6);
/*코로나 변이 시기*/
IF      COVID_H=1 & (SUBSTR(CDX_DT,1,4)=2020 | SUBSTR(CDX_DT,1,6) IN (202101,202102,202103,202104,202105,202106)) THEN COVID_PERIOD="Pre delta"; 
else if COVID_H=1 & SUBSTR(CDX_DT,1,6) IN (202107, 202108,202109,202110,202111,202112) THEN COVID_PERIOD="Delta";
else if COVID_H=1 & substr(CDX_DT,1,4)=2022 THEN COVID_PERIOD="Omicron" ; else covid_period="NA";

/*임신 전/ 임신중/ 분만후 날짜 계산*/
DATE1     =MDY(SUBSTR(DLV_DT,5,2),SUBSTR(DLV_DT,7,2),SUBSTR(DLV_DT,1,4));   
COVID_DATE=MDY(SUBSTR(CDX_DT,5,2),SUBSTR(CDX_DT,7,2),SUBSTR(CDX_DT,1,4));
DATE2     =MDY(SUBSTR(PREG_DDATE,5,2),SUBSTR(PREG_DDATE,7,2),SUBSTR(PREG_DDATE,1,4));

/*기간에 따른 COVID-19 감염 유무*/
IF COVID_H=1 & COVID_DATE>=DATE2 & COVID_DATE<= DATE1 THEN COVID19=1; ELSE COVID19=0;
IF COVID_H=1 & COVID_DATE<DATE2 THEN COVID_BEFORE_PREG=1; ELSE COVID_BEFORE_PREG=0;
IF COVID_H=1 & COVID_DATE>DATE1 THEN COVID_AFTER_PREG=1; ELSE COVID_AFTER_PREG=0;
RUN;

PROC FREQ DATA=AA.TARGET_COVID; TABLES COVID19; RUN;
PROC FREQ DATA=AA.TARGET_COVID; TABLES COVID_BEFORE_PREG; RUN;
PROC FREQ DATA=AA.TARGET_COVID; TABLES COVID_AFTER_PREG; RUN;

DATA SAPTMP.TARGET_COVID; SET AA.TARGET_COVID; RUN;

PROC FREQ DATA=AA.TARGET_COVID; TABLES CDX_YEAR; RUN;
PROC FREQ DATA=AA.TARGET_COVID; TABLES CDX_YYYYMM; RUN;
PROC FREQ DATA=AA.TARGET_COVID; TABLES COVID_PERIOD; RUN;

DATA ZZ1; SET AA.TARGET_COVID; IF COVID_H=1 & COVID19=1; RUN;
DATA ZZ2; SET AA.TARGET_COVID; IF COVID_H=1 & COVID_BEFORE_PREG=1;RUN;
DATA ZZ3; SET AA.TARGET_COVID; IF COVID_H=1 & COVID_AFTER_PREG=1;RUN;

PROC FREQ DATA=ZZ1; TABLES CDX_YYYYMM/LIST; RUN;
PROC FREQ DATA=ZZ2; TABLES CDX_YYYYMM/LIST; RUN;
PROC FREQ DATA=ZZ3; TABLES CDX_YYYYMM/LIST; RUN;
/**********************************************************************************************/
/*코로나 백신 DB*/
PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT TOP 10*
    FROM NHISBDA.COVID_VAC_OPEN_20220415  /**백신접종 DB**/
);DISCONNECT FROM X1;
QUIT; 
/*코로나 백신 DB 코로나 백신 DB 대상자 빈도*/
PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT count(INDI_DSCM_NO) as cnt, count(distInct indi_dscm_no) as unique_cnt ,min(INOC_DT) AS MIN_DT,
	MAX(INOC_DT) AS MAX_DT 
    FROM NHISBDA.COVID_VAC_OPEN_20220415   /**확진자 DB**/
);DISCONNECT FROM X1;
QUIT; 

PROC SQL; 
&CONNECT.;
EXECUTE(
CREATE TABLE ZZ AS (
SELECT * FROM NHISBDA.COVID_VAC_OPEN_20220415 WHERE INDI_DSCM_NO IN (SELECT MOTHER_ID FROM TARGET_COVID)
)
)BY X1; DISCONNECT FROM X1;
QUIT;

PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT count(INDI_DSCM_NO) as cnt, count(distInct indi_dscm_no) as unique_cnt ,min(INOC_DT) AS MIN_DT,
	MAX(INOC_DT) AS MAX_DT 
    FROM ZZ   /**확진자 DB**/
);DISCONNECT FROM X1;
QUIT; 

/*백신 종류*/
proc freq data=saptmp.ZZ; tables VACN_NM; run;

/*백신 차수*/
proc freq data=saptmp.ZZ; tables inoc_dfno; RUN;

DATA AA.ZZ; SET SAPTMP.ZZ; RUN;

/*한 사람당 백신 몇번 접종 했는지 검토*/
PROC SQL; 
&CONNECT.;
CREATE TABLE VAC_CNT AS SELECT * FROM CONNECTION TO X1(
    SELECT INDI_DSCM_NO AS ID, count(INDI_DSCM_NO) as cnt FROM ZZ GROUP BY INDI_DSCM_NO   /**확진자 DB**/
);DISCONNECT FROM X1;
QUIT;

data aa.Preg_COVID_VAC; set saptmp.ZZ; run;
data aa.VAC_CNT; set vac_cnt; run;
data saptmp.VAC_CNT; set vac_cnt;run;

/*백신 접종한 임신부 대상자 전체 아이디 테이블에 자료 옆으로 불이기 (long format -> wide format)
차수별 접종 시기와 어떤 백신 접종 했는지 붙이기*/

/*고유 대상자 먼저 식별*/
PROC SORT DATA=AA.PREG_COVID_VAC NODUPKEY OUT=UNIQ_PREG_VAC ; BY INDI_DSCM_NO; RUN;
DATA UNIQ_PREG_VAC; SET UNIQ_PREG_VAC; KEEP INDI_DSCM_NO; RUN;

/*차수별 대상자 보기, 5차는 한명 입력되어있는데 제외하기*/
/*1차 백신접종*/
DATA PREG_VAC1; 
RENAME INOC_DT=INOC_DT1  VACN_NM=VACN_NM1;
SET AA.PREG_COVID_VAC; IF INOC_DFNO=1; DROP INOC_DFNO; RUN;
/*2차 백신접종*/
DATA PREG_VAC2; 
RENAME INOC_DT=INOC_DT2  VACN_NM=VACN_NM2;
SET AA.PREG_COVID_VAC; IF INOC_DFNO=2; DROP INOC_DFNO; RUN;
/*3차 백신접종*/
DATA PREG_VAC3; 
RENAME INOC_DT=INOC_DT3  VACN_NM=VACN_NM3;
SET AA.PREG_COVID_VAC; IF INOC_DFNO=3; DROP INOC_DFNO; RUN;
/*4차 백신접종*/
DATA PREG_VAC4; 
RENAME INOC_DT=INOC_DT4  VACN_NM=VACN_NM4;
SET AA.PREG_COVID_VAC; IF INOC_DFNO=4; DROP INOC_DFNO; RUN;

/*접종차수별 자료 merge*/
PROC SQL; CREATE TABLE PREG_VAC_M AS SELECT * FROM UNIQ_PREG_VAC  AS A LEFT JOIN PREG_VAC1 AS B ON A.INDI_DSCM_NO=B.INDI_DSCM_NO; QUIT;
PROC SQL; CREATE TABLE PREG_VAC_M AS SELECT * FROM PREG_VAC_M        AS A LEFT JOIN PREG_VAC2 AS B ON A.INDI_DSCM_NO=B.INDI_DSCM_NO; QUIT;
PROC SQL; CREATE TABLE PREG_VAC_M AS SELECT * FROM PREG_VAC_M        AS A LEFT JOIN PREG_VAC3 AS B ON A.INDI_DSCM_NO=B.INDI_DSCM_NO; QUIT;
PROC SQL; CREATE TABLE PREG_VAC_M AS SELECT * FROM PREG_VAC_M        AS A LEFT JOIN PREG_VAC4 AS B ON A.INDI_DSCM_NO=B.INDI_DSCM_NO; QUIT;

DATA PREG_VAC_M; set preg_vac_m;
IF vacn_nm1^="" THEN Index1=1; else index1=0;
IF vacn_nm2^="" THEN Index2=2; else index2=0;
IF vacn_nm3^="" THEN Index3=3; else index3=0;
IF vacn_nm4^="" THEN Index4=4; else index4=0;

/*몇차까지 맞았는지?*/
INOC_CUM  =MAX(index1,index2, index3,index4);
/*접종 차수 1~4차 중 어느 시기든지 한번만있으면 1*/
/*1차에 접종 안했는데 2,3차에 존재하는 경우 있음 (해외입국?)-> 전체적으로 다맞음*/
IF INOC_CUM>=1 THEN INOC=1; ELSE INOC=0;
drop index1-index4; run;
proc freq data=preg_vac_m; tables inoc; run;
proc freq data=preg_vac_m; tables inoc_cum; run;
proc freq data=preg_vac_m; tables vacn_nm1; run;

PROC SQL; CREATE TABLE Z AS SELECT INDI_DSCM_NO AS ID, COUNT(INDI_DSCM_NO) AS CNT
FROM PREG_VAC_M GROUP BY INDI_DSCM_NO; QUIT;
/*백신 접종 DB 살펴보면 한 사람이 동일한 차수에 중복 입력되는 겨우 있음
접종일/접종 백신이 다른 경우 존재
9명 정도 -> 이건 불분명해서 제외하기 */
DATA ZZ; SET Z; IF CNT>=2; RUN;

PROC SQL; CREATE TABLE PREG_VAC_Final as select * from preg_vac_m where indi_dscm_no not in (select ID from zz) ; quit;

/*임신부 대상자의 백신 접종 db 정리 (long format-> wide format)*/
/*이 자료랑 위에서 정리한 대상자 자료랑 merge해서 임신 전/임신 중 백신접종 유무 검토 필요*/
data aa.preg_vac_final; set preg_vac_final; run;
data saptmp.PREG_VAC_FINAL; set aa.preg_vac_final;run;

PROC SQL; 
&CONNECT.;
EXECUTE(
CREATE TABLE TARGET_COVID_VAC AS
(SELECT * FROM TARGET_COVID A left join PREG_VAC_FINAL B  
ON A.MOTHER_ID=B.INDI_DSCM_NO 
)
)BY X1; DISCONNECT FROM X1;QUIT;

DATA AA.TARGET_COVID_VAC; SET SAPTMP.TARGET_COVID_VAC; RUN;

/*PREG_VAC_FINAL은 타겟 대상자 중(40만명) 백신 접종 맞은 사람(30만명)만 데려와서 정리한 거*/
/*백신 접종 결측인 경우는 안맞은 대상자니 0 넣어주기*/
DATA AA.TARGET_COVID_VAC; SET AA.TARGET_COVID_VAC; 
IF INDI_DSCM_NO="." THEN INOC_CUM=0; ELSE INOC_CUM=INOC_CUM;
IF INDI_DSCM_NO="." THEN INOC         =0; ELSE INOC=INOC; RUN;

/*임신전, 임신 중, 분만 후 백신 접종 검토 살펴보기*/
/*1차 접종만 예시로*/
/*DATE2: 임신시작일 간주
   DATE1: 분만일 간주*/
DATA  AA.TARGET_COVID_VAC; SET AA.TARGET_COVID_VAC;

INOC_DATE1=MDY(SUBSTR(INOC_DT1,5,2),SUBSTR(INOC_DT1,7,2),SUBSTR(INOC_DT1,1,4));
INOC_DATE2=MDY(SUBSTR(INOC_DT2,5,2),SUBSTR(INOC_DT2,7,2),SUBSTR(INOC_DT2,1,4));
INOC_DATE3=MDY(SUBSTR(INOC_DT3,5,2),SUBSTR(INOC_DT3,7,2),SUBSTR(INOC_DT3,1,4));
INOC_DATE4=MDY(SUBSTR(INOC_DT4,5,2),SUBSTR(INOC_DT4,7,2),SUBSTR(INOC_DT4,1,4));

/*임신 전 백신 접종 유무*/
IF INOC_DT4^=""         & INOC_DATE4<DATE2 THEN PRE_PREG_VAC=1 ; 
ELSE IF INOC_DT3^="" & INOC_DATE3<DATE2 THEN PRE_PREG_VAC=1;
ELSE IF INOC_DT2^="" & INOC_DATE2<DATE2 THEN PRE_PREG_VAC=1;
ELSE IF INOC_DT1^="" & INOC_DATE1<DATE2 THEN PRE_PREG_VAC=1; 
ELSE PRE_PREG_VAC=0;

/*임신 중  백신 접종 유무*/
IF INOC_DT4^=""         & (INOC_DATE4>=DATE2 & INOC_DATE4<=DATE1) THEN PREG_VAC=1 ; 
ELSE IF INOC_DT3^="" & (INOC_DATE3>=DATE2 & INOC_DATE3<=DATE1)  THEN PREG_VAC=1;
ELSE IF INOC_DT2^="" & (INOC_DATE2>=DATE2 & INOC_DATE2<=DATE1)  THEN PREG_VAC=1;
ELSE IF INOC_DT1^="" & (INOC_DATE1>=DATE2 & INOC_DATE1<=DATE1)  THEN PREG_VAC=1; 
ELSE PREG_VAC=0;

/*분만 후  백신 접종 유무*/
IF INOC_DT4^=""         & INOC_DATE4>DATE1 THEN POST_PREG_VAC=1 ; 
ELSE IF INOC_DT3^="" & INOC_DATE3>DATE1 THEN POST_PREG_VAC=1;
ELSE IF INOC_DT2^="" & INOC_DATE2>DATE1 THEN POST_PREG_VAC=1;
ELSE IF INOC_DT1^="" & INOC_DATE1>DATE1 THEN POST_PREG_VAC=1; 
ELSE POST_PREG_VAC=0;

/*임신 전 백신 접종 차수*/
IF  INOC_DT4^=""          & INOC_DATE4<DATE2 THEN PRE_PREG_VAC_NM=4;
ELSE IF INOC_DT3 ^="" & INOC_DATE3<DATE2 THEN PRE_PREG_VAC_NM=3;
ELSE IF INOC_DT2 ^="" & INOC_DATE2<DATE2 THEN PRE_PREG_VAC_NM=2;
ELSE IF INOC_DT1 ^="" & INOC_DATE1<DATE2 THEN PRE_PREG_VAC_NM=1;
ELSE  PRE_PREG_VAC_NM=0;

/*임신 중 백신 접종 차수*/
IF  INOC_DT4^=""          & (INOC_DATE4>=DATE2 & INOC_DATE4<=DATE1) THEN PREG_VAC_NM=4;
ELSE IF INOC_DT3 ^="" & (INOC_DATE3>=DATE2 & INOC_DATE3<=DATE1) THEN PREG_VAC_NM=3;
ELSE IF INOC_DT2 ^="" & (INOC_DATE2>=DATE2 & INOC_DATE2<=DATE1) THEN PREG_VAC_NM=2;
ELSE IF INOC_DT1 ^="" & (INOC_DATE1>=DATE2 & INOC_DATE1<=DATE1) THEN PREG_VAC_NM=1;
ELSE  PREG_VAC_NM=0;

/*분만 후 백신 접종 차수*/
IF  INOC_DT4^=""           & INOC_DATE4>DATE1 THEN POST_PREG_VAC_NM=4;
ELSE IF INOC_DT3 ^=""  & INOC_DATE3>DATE1 THEN POST_PREG_VAC_NM=3;
ELSE IF INOC_DT2 ^=""  & INOC_DATE2>DATE1 THEN POST_PREG_VAC_NM=2;
ELSE IF INOC_DT1 ^=""  & INOC_DATE1>DATE1 THEN POST_PREG_VAC_NM=1;
ELSE POST_PREG_VAC_NM=0;

/*코로나 감염전 백신 접종 유무*/
IF INOC_DT4^=""         & INOC_DATE4<COVID_DATE THEN PRE_COVID_VAC=1 ; 
ELSE IF INOC_DT3^="" & INOC_DATE3<COVID_DATE THEN PRE_COVID_VAC=1;
ELSE IF INOC_DT2^="" & INOC_DATE2<COVID_DATE THEN PRE_COVID_VAC=1;
ELSE IF INOC_DT1^="" & INOC_DATE1<COVID_DATE THEN PRE_COVID_VAC=1; 
ELSE PRE_COVID_VAC=0;

/*코로나 감염 이후 백신 접종 유무*/
IF INOC_DT4^=""         & INOC_DATE4>=COVID_DATE THEN POST_COVID_VAC=1 ; 
ELSE IF INOC_DT3^="" & INOC_DATE3>=COVID_DATE THEN POST_COVID_VAC=1;
ELSE IF INOC_DT2^="" & INOC_DATE2>=COVID_DATE THEN POST_COVID_VAC=1;
ELSE IF INOC_DT1^="" & INOC_DATE1>=COVID_DATE THEN POST_COVID_VAC=1; 
ELSE POST_COVID_VAC=0;

/*코로나 감염전 백신 접종 차수*/
IF  INOC_DT4^=""          & INOC_DATE4<COVID_DATE THEN PRE_COVID_VAC_NM=4;
ELSE IF INOC_DT3 ^="" & INOC_DATE3<COVID_DATE THEN PRE_COVID_VAC_NM=3;
ELSE IF INOC_DT2 ^="" & INOC_DATE2<COVID_DATE THEN PRE_COVID_VAC_NM=2;
ELSE IF INOC_DT1 ^="" & INOC_DATE1<COVID_DATE THEN PRE_COVID_VAC_NM=1;
ELSE  PRE_COVID_VAC_NM=0;

/*코로나 감염이후 백신 접종 차수*/
IF  INOC_DT4^=""          & INOC_DATE4>=COVID_DATE THEN POST_COVID_VAC_NM=4;
ELSE IF INOC_DT3 ^="" & INOC_DATE3>=COVID_DATE THEN POST_COVID_VAC_NM=3;
ELSE IF INOC_DT2 ^="" & INOC_DATE2>=COVID_DATE THEN POST_COVID_VAC_NM=2;
ELSE IF INOC_DT1 ^="" & INOC_DATE1>=COVID_DATE THEN POST_COVID_VAC_NM=1;
ELSE  POST_COVID_VAC_NM=0;

/*임신  이전 FULLY VACCINATED*/
/*임신 이전에 백신 2차 까지 맞았거나, 임신 이전 이면서 1차 백신 얀센인 경우*/
IF PRE_PREG_VAC_NM>=2 OR (VACN_NM1="04" & PRE_PREG_VAC=1) THEN FULLY_PRE_PREG_VAC=1; 
ELSE FULLY_PRE_PREG_VAC=0;

/*코로나 감염 이전 FULLY VACCINATED :임신관련없이, 임신부 대상자에서 본거 */
/*코로나 감염 이전에 백신 2차 까지 맞았거나, 코로나 감염 이전 이면서 1차 백신 얀센인 경우*/
IF PRE_COVID_VAC_NM>=2 OR (VACN_NM1="04" & PRE_COVID_VAC=1) THEN FULLY_PRE_COVID_VAC=1; 
ELSE FULLY_PRE_COVID_VAC=0;

/*임신 중 코로나 감염 (case군)이면서 임신 전 fully-vaccinated 인경우*/
IF COVID19=1 & FULLY_PRE_PREG_VAC=1 THEN FULLY_VAC_COVID_PRE_PREG=1; ELSE FULLY_VAC_COVID_PRE_PREG=0;

/*임신 중 코로나 감염 (case군)이면서 감염 전 fully-vaccinated 인경우*/
IF COVID19=1 & FULLY_PRE_COVID_VAC=1 THEN FULLY_VAC_COVID=1; ELSE FULLY_VAC_COVID=0;

DROP INOC_DATE1-INOC_DATE4; RUN;

/*대상자+코로나 감염+백신접종 연계 자료 SAPTMP에 저장*/
DATA SAPTMP.TARGET_COVID_VAC; SET AA.TARGET_COVID_VAC; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES FULLY_VAC_COVID; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES FULLY_VAC_COVID_PRE_PREG; RUN;

PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES INOC_CUM; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES INOC; RUN;

PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES PRE_PREG_VAC_NM; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES PRE_PREG_VAC; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES PREG_VAC; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES POST_PREG_VAC; RUN;

PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES PRE_PREG_VAC/LIST; RUN;

/*접종 차수별, 임신 시기별 백신 접종 검토*/
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES PRE_PREG_VAC_NM*PRE_PREG_VAC/LIST; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES PREG_VAC_NM*PREG_VAC/LIST; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES POST_PREG_VAC_NM*POST_PREG_VAC/LIST; RUN;

/*접종 차수별, 코로나 전 백신 접종 검토*/
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES PRE_COVID_VAC_NM*PRE_COVID_VAC/LIST; RUN;

/*접종 차수별, 코로나 이후 백신 접종 검토*/
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES POST_COVID_VAC_NM*POST_COVID_VAC/LIST; RUN;

/*접종 차수별, 임신 시기별 백신 종류 검토*/
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES VACN_NM1*PRE_PREG_VAC/LIST; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES VACN_NM2*PRE_PREG_VAC/LIST; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES VACN_NM3*PRE_PREG_VAC/LIST; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES VACN_NM4*PRE_PREG_VAC/LIST; RUN;

PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES VACN_NM1*PREG_VAC/LIST; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES VACN_NM2*PREG_VAC/LIST; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES VACN_NM3*PREG_VAC/LIST; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES VACN_NM4*PREG_VAC/LIST; RUN;

PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES VACN_NM1*POST_PREG_VAC/LIST; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES VACN_NM2*POST_PREG_VAC/LIST; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES VACN_NM3*POST_PREG_VAC/LIST; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES VACN_NM4*POST_PREG_VAC/LIST; RUN;

/*임신이전, 코로나 감염 이전에 fully-vaccinated 빈도*/
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES FULLY_PRE_PREG_VAC; RUN;
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES FULLY_PRE_COVID_VAC; RUN;

/*임신 중 코로나 감염 (case군)이면서 fully-vaccinated 인경우 빈도*/
PROC FREQ DATA=AA.TARGET_COVID_VAC; TABLES FULLY_VAC_COVID; RUN;

PROC CONTENTS DATA=AA.TARGET_COVID_VAC; RUN;

/**********************************************************************************************/
/**********************************************************************************************/
/*기저 질환은 진료 DB (T20)에서 찾기*/
PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT TOP 10*
    FROM NHISBASE.HBMV_TBGJME20  /**T20 DB**/
);DISCONNECT FROM X1;
QUIT; 

proc freq data=aa.TARGET_COVID_VAC; tables covid19; run;

DATA SAPTMP.TARGET_COVID_VAC; SET AA.TARGET_COVID_VAC; RUN;

/*기저질환 연결*/
/*1) 확진일 전 1년간 주상병+제1 부상병 중 임신일 이전까지 
해당질환으로 진료받은 기록(입원 또는 외래)이 한 해에 2회 이상인 경우
임신성 당뇨, 임신성  고혈압 질환들은 임신 기간내 존재하는지 검토하기*/

/*
T20이 전체 대상자니까 이중에 우리 연구 대상자만 추출
한 환자 같은날 동일 질환으로 여러번 카운트 된 경우 중복 제거
우선순위 주상병> 제1 부상병, 2회이상이면 KEEP
*/

/*여기에 기저질환 미리 타이핑해서 제외조건 걸고 돌리기
심평원 심사년월
주상병, 부상병 질환 ICD 조건
필요한 가용 변수 (분석에 쓸 것만)
 */

/**********************************************************************************************/
PROC SQL; 
&CONNECT.;
CREATE TABLE MTH_T20 AS SELECT * FROM CONNECTION TO X1(
    SELECT *
    FROM NHISBASE.HBMV_TBGJME20  /**T20 DB**/
	WHERE  INDI_DSCM_NO IN (SELECT MOTHER_ID FROM TARGET_COVID_VAC)
	AND HIRA_EXM_YYYYMM<='202204'
    AND PAY_YN='1'
	AND FORM_CD IN ('02','03','07','08','09','10','11','15')
	/*2018 (시점) 2020.1~2022.3*/
	AND MDCARE_STRT_YYYYMM IN ('201801','201802','201803','201804','201805','201806','201807','201808','201809','201810','201811','201812',
'201901','201902','201903','201904','201905','201906','201907','201908','201909','201910','201911','201912',
'202001','202002','202003','202004','202005','202006','202007','202008','202009','202010','202011','202012',
'202101','202102','202103','202104','202105','202106','202107','202108','202109','202110','202111','202112')
	AND INDI_DSCM_NO<>0 
	AND INDI_DSCM_NO IS NOT NULL
	AND INDI_DSCM_NO<90000000
	/*주상병 */
	 AND           (LEFT(SICK_SYM1,3) BETWEEN 'J42' AND 'J44'      /*Asthma*/
	               OR LEFT(SICK_SYM1,3)='J45'                                             /*COPD*/
	               OR LEFT(SICK_SYM1,3) BETWEEN 'A15' AND 'A19'    /*Tuberculosus*/
	               OR LEFT(SICK_SYM1,3)='I50'                                            /*Heart failure*/
                   OR LEFT(SICK_SYM1,3)='I05'                                            /*Rheumatic mitral valve DO*/
                   OR LEFT(SICK_SYM1,3)='I34'                                            /*Nonrheumatic mitral valve DO*/
				   OR LEFT(SICK_SYM1,3)='Q23'                                           /*Congenital malformations of aortic and mitral valve*/
				   OR LEFT(SICK_SYM1,3)='I35'                                            /*Nonrheumatic aortic valve DO*/
				   OR LEFT(SICK_SYM1,3)='I06'                                            /*Rheumatic aortic valve disease*/
				   OR LEFT(SICK_SYM1,3)='I08'                                            /*Mitral, aortic valve*/
				   OR LEFT(SICK_SYM1,3) BETWEEN 'Q20' AND 'Q21'  /*VSD 등 복합*/
				   OR LEFT(SICK_SYM1,3)='J84'                                            /*Intestinal lung disease (간질성 폐질환)*/
				   OR LEFT(SICK_SYM1,3)='I26'                                            /*Pulmonary embolism (폐색전증)*/
				   OR LEFT(SICK_SYM1,3)='J47'                                            /*Bronchiectasis (기관지 확장증)*/
				   OR LEFT(SICK_SYM1,3) BETWEEN 'N18' AND 'N19'  /*Chronic kidney disease*/
				   OR LEFT(SICK_SYM1,3)='M32'                                        /*SLE (Systemic lupus erythematosus)*/
				   OR LEFT(SICK_SYM1,3)='M05'                                        /*RA (Rheumatoia Arthritis)*/
				   OR LEFT(SICK_SYM1,1)='C'                                             /*Cancer*/

				   /*임신중 기저질환 보려는 것 (임당, 임신성 고혈압 등) */
				   OR LEFT(SICK_SYM1,4)='O240'
				   OR LEFT(SICK_SYM1,4)='O241'
				   OR LEFT(SICK_SYM1,4)='O243'
				   OR LEFT(SICK_SYM1,4)='O244'
				   OR LEFT(SICK_SYM1,4)='O249'
                   OR LEFT(SICK_SYM1,3) BETWEEN 'E10' AND 'E14'              /*DM*/
                   OR LEFT(SICK_SYM1,3)='O10'
                   OR LEFT(SICK_SYM1,3)='O16'
				   OR LEFT(SICK_SYM1,3) BETWEEN 'I10' AND 'I15'              /*Chronic hypertension*/
				   OR LEFT(SICK_SYM1,3) BETWEEN 'Q20' AND 'Q28'              /*Congenitial heart disease*/

				   /*제 2 부상병*/
				   OR LEFT(SICK_SYM2,3) BETWEEN 'J42' AND 'J44'              /*Asthma*/
	               OR LEFT(SICK_SYM2,3)='J45'                                             /*COPD*/
	               OR LEFT(SICK_SYM2,3) BETWEEN 'A15' AND 'A19'    /*Tuberculosus*/
	               OR LEFT(SICK_SYM2,3)='I50'                                            /*Heart failure*/
                   OR LEFT(SICK_SYM2,3)='I05'                                            /*Rheumatic mitral valve DO*/
                   OR LEFT(SICK_SYM2,3)='I34'                                            /*Nonrheumatic mitral valve DO*/
				   OR LEFT(SICK_SYM2,3)='Q23'                                           /*Congenital malformations of aortic and mitral valve*/
				   OR LEFT(SICK_SYM2,3)='I35'                                            /*Nonrheumatic aortic valve DO*/
				   OR LEFT(SICK_SYM2,3)='I06'                                            /*Rheumatic aortic valve disease*/
				   OR LEFT(SICK_SYM2,3)='I08'                                            /*Mitral, aortic valve*/
				   OR LEFT(SICK_SYM2,3) BETWEEN 'Q20' AND 'Q21'  /*VSD 등 복합*/
				   OR LEFT(SICK_SYM2,3)='J84'                                            /*Intestinal lung disease (간질성 폐질환)*/
				   OR LEFT(SICK_SYM2,3)='I26'                                            /*Pulmonary embolism (폐색전증)*/
				   OR LEFT(SICK_SYM2,3)='J47'                                            /*Bronchiectasis (기관지 확장증)*/
				   OR LEFT(SICK_SYM2,3) BETWEEN 'N18' AND 'N19'  /*Chronic kidney disease*/
				   OR LEFT(SICK_SYM2,3)='M32'                                        /*SLE (Systemic lupus erythematosus)*/
				   OR LEFT(SICK_SYM2,3)='M05'                                        /*RA (Rheumatoia Arthritis)*/
				   OR LEFT(SICK_SYM2,1)='C'                                             /*Cancer*/

				   /*임신중 기저질환 보려는 것 (임당, 임신성 고혈압 등) */
				   OR LEFT(SICK_SYM2,4)='O240'
				   OR LEFT(SICK_SYM2,4)='O241'
				   OR LEFT(SICK_SYM2,4)='O243'
				   OR LEFT(SICK_SYM2,4)='O244'
				   OR LEFT(SICK_SYM2,4)='O249'
                   OR LEFT(SICK_SYM2,3) BETWEEN 'E10' AND 'E14'              /*DM*/
                   OR LEFT(SICK_SYM2,3)='O10'
                   OR LEFT(SICK_SYM2,3)='O16'
				   OR LEFT(SICK_SYM2,3) BETWEEN 'I10' AND 'I15'              /*Chronic hypertension*/
				   OR LEFT(SICK_SYM2,3) BETWEEN 'Q20' AND 'Q28'              /*Congenitial heart disease*/

)
);DISCONNECT FROM X1;
QUIT; 

DATA AA.MTH_Baseline_dis; 
SET MTH_T20; 
KEEP CMN_KEY INDI_DSCM_NO MDCARE_STRT_YYMM MDCARE_STRT_DT SICK_SYM1 SICK_SYM2;
RUN;
DATA SAPTMP.MTH_BASELINE_DIS; SET AA.MTH_Baseline_dis;
KEEP CMN_KEY INDI_DSCM_NO MDCARE_STRT_YYMM MDCARE_STRT_DT SICK_SYM1 SICK_SYM2;
RUN;

/*기저질환 자료  대상자, 요양개시일자별 SORTING*/
PROC SORT DATA= AA.MTH_BASELINE_DIS; BY INDI_DSCM_NO MDCARE_STRT_DT; RUN;

/*임신부 고유 대상자 뽑고, 연구기간내 처음 임신/두번임신/...N번임신 시점 구분*/
/*정리한 자료랑 기저질환 자료 MERGE해서 임신기간동안 기저질환 유무 체크하고, 
  기간 내에 2번이상 이면 해당질환자로 카운트*/

/*엄마-임신일자 키 만들어서 검토*/
data AA.TARGET_COVID_VAC; set AA.TARGET_COVID_VAC; 
DUPKEY=COMPRESS(LEFT(MOTHER_ID))||("-")||COMPRESS(LEFT(PREG_DDATE));
run;

/*엄마-임신일 중복제외*/
PROC SORT DATA=AA.TARGET_COVID_VAC NODUPKEY OUT=ZZ2; BY DUPKEY; RUN;
PROC SQL; CREATE TABLE AA.MTH_CNT AS SELECT MOTHER_ID, COUNT(MOTHER_ID) AS CNT FROM ZZ2 GROUP BY MOTHER_ID;QUIT;

/*쌍둥이  중복건수 제외하고  연구기간 동안 식별되는 임신 횟수 (1-3)*/
PROC FREQ DATA= AA.MTH_CNT; TABLES CNT; RUN;

/*엄마-임신일 키 중복 제거한 자료 엄마 아이디 임신일 정렬*/
PROC SORT DATA=ZZ2; BY MOTHER_ID PREG_DDATE; RUN;

/*한 엄마가 연구 기간동안 1번 임신한 경우1, 2번이면 2, 3번이면 3*/
DATA ZZZ; SET ZZ2; 
BY MOTHER_ID;
RETAIN OBS_NUM 0;
IF FIRST.MOTHER_ID THEN OBS_NUM=1; 
ELSE OBS_NUM=OBS_NUM+1;
KEEP DUPKEY OBS_NUM;
RUN;

PROC FREQ DATA=ZZZ; TABLES OBS_NUM; RUN;

PROC  SQL; CREATE TABLE Z AS SELECT * FROM AA.TARGET_COVID_VAC AS A LEFT JOIN ZZZ AS B ON A.DUPKEY =B.DUPKEY; QUIT;

/*첫번째-세번째 임신 시점에서 기저질환자 찾기*/
DATA MTH_DUP1; SET Z; IF OBS_NUM=1; KEEP MOTHER_ID DLV_DT PREG_DDATE DATE1 DATE2 DUPKEY OBS_NUM;  RUN;
DATA MTH_DUP2; SET Z; IF OBS_NUM=2; KEEP MOTHER_ID DLV_DT PREG_DDATE DATE1 DATE2 DUPKEY OBS_NUM;  RUN;
DATA MTH_DUP3; SET Z; IF OBS_NUM=3; KEEP MOTHER_ID DLV_DT PREG_DDATE DATE1 DATE2 DUPKEY OBS_NUM;  RUN;

DATA AA.MTH_DUP1; SET MTH_DUP1; RUN;
DATA AA.MTH_DUP2; SET MTH_DUP2; RUN;
DATA AA.MTH_DUP3; SET MTH_DUP3; RUN;

/*각각 자료 연계 임신 시점별, 기저질환 연결  (INNER JOIN)*/
PROC SQL; CREATE  TABLE AA.MTH_BD1 AS SELECT * FROM AA.MTH_BASELINE_DIS AS A INNER JOIN MTH_DUP1 AS B  ON A.INDI_DSCM_NO=B.MOTHER_ID; QUIT;
PROC SQL; CREATE  TABLE AA.MTH_BD2 AS SELECT * FROM AA.MTH_BASELINE_DIS AS A INNER JOIN MTH_DUP2 AS B  ON A.INDI_DSCM_NO=B.MOTHER_ID; QUIT;
PROC SQL; CREATE  TABLE AA.MTH_BD3 AS SELECT * FROM AA.MTH_BASELINE_DIS AS A INNER JOIN MTH_DUP3 AS B  ON A.INDI_DSCM_NO=B.MOTHER_ID; QUIT;

DATA TG_COVID; SET AA.TARGET_COVID_VAC; 
IF COVID19=1 THEN COVID_DATE=MDY(SUBSTR(CDX_DT,5,2),SUBSTR(CDX_DT,7,2),SUBSTR(CDX_DT,1,4));
KEEP DUPKEY CDX_DT COVID19 COVID_DATE ; RUN;

PROC SQL; CREATE TABLE AA.MTH_BD1 AS SELECT * FROM AA.MTH_BD1 AS A LEFT JOIN TG_COVID AS B ON A.DUPKEY=B.DUPKEY; QUIT;
PROC SQL; CREATE TABLE AA.MTH_BD2 AS SELECT * FROM AA.MTH_BD2 AS A LEFT JOIN TG_COVID AS B ON A.DUPKEY=B.DUPKEY; QUIT;
PROC SQL; CREATE TABLE AA.MTH_BD3 AS SELECT * FROM AA.MTH_BD3 AS A LEFT JOIN TG_COVID AS B ON A.DUPKEY=B.DUPKEY; QUIT;

/*각 해당 기저질환이 2건 이상 있으면 최종적으로 기저질환 존재 1, 아니면 0*/
/*(1) 임신 전 1년 기저질환
    (2.1) 임신 기간 동안 & 코로나 감염 이전 기저질환 
    (2.2) 코로나 감염이  아니라면 임신 중 기간 */

DATA AA.MTH_BD1; SET AA.MTH_BD1;
/*요양 개시일자 */
MDATE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
/*기저질환 날짜 계산*/
IF 0<DATE2-MDATE <=365 THEN BD_INDEX=1 ; ELSE BD_INDEX=0;                                                                  /*임신일-요양개시일자 차이*/    
IF         COVID19=0 & (MDATE>=DATE2 & MDATE<DATE1) THEN BD_INDEX2=1;                                             /*코로나 미감염이면 임신 중 기저질환 유무 */
ELSE IF COVID19=1 & (MDATE>=DATE2 & MDATE<COVID_DATE) THEN BD_INDEX2=1; ELSE BD_INDEX2=0; /*코로나 감염이면 임신~코로나 감염 이전까지*/
RUN;

DATA AA.MTH_BD2; SET AA.MTH_BD2;
/*요양 개시일자 */
MDATE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
/*기저질환 날짜 계산*/
IF 0<DATE2-MDATE <=365 THEN BD_INDEX=1 ; ELSE BD_INDEX=0;                                                                  /*임신일-요양개시일자 차이*/    
IF         COVID19=0 & (MDATE>=DATE2 & MDATE<DATE1) THEN BD_INDEX2=1;                                             /*코로나 미감염이면 임신 중 기저질환 유무 */
ELSE IF COVID19=1 & (MDATE>=DATE2 & MDATE<COVID_DATE) THEN BD_INDEX2=1; ELSE BD_INDEX2=0; /*코로나 감염이면 임신~코로나 감염 이전까지*/
RUN;

DATA AA.MTH_BD3; SET AA.MTH_BD3;
/*요양 개시일자 */
MDATE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
/*기저질환 날짜 계산*/
IF 0<DATE2-MDATE <=365 THEN BD_INDEX=1 ; ELSE BD_INDEX=0;                                                                  /*임신일-요양개시일자 차이*/    
IF         COVID19=0 & (MDATE>=DATE2 & MDATE<DATE1) THEN BD_INDEX2=1;                                             /*코로나 미감염이면 임신 중 기저질환 유무 */
ELSE IF COVID19=1 & (MDATE>=DATE2 & MDATE<COVID_DATE) THEN BD_INDEX2=1; ELSE BD_INDEX2=0; /*코로나 감염이면 임신~코로나 감염 이전까지*/
RUN;

DATA MTH_DUP1_ID; SET MTH_DUP1; KEEP DUPKEY; RUN;
DATA MTH_DUP2_ID; SET MTH_DUP2; KEEP DUPKEY; RUN;
DATA MTH_DUP3_ID; SET MTH_DUP3; KEEP DUPKEY; RUN;

DATA AA.MTH_DUP1_ID; SET MTH_DUP1_ID;  RUN;
DATA AA.MTH_DUP2_ID; SET MTH_DUP2_ID; RUN;
DATA AA.MTH_DUP3_ID; SET MTH_DUP3_ID;  RUN;

/*각 주/부상병에 해당 질환 있는지 찾아서 두 건 이상이면 1, 아니면 0 생성 매크로*/
%MACRO BD(DATA,OUTPUT,VAR,K,D1,D2);
DATA OUT; SET AA.&DATA.; 
IF (SUBSTR(SICK_SYM1,1,&K.) >=&D1. & SUBSTR(SICK_SYM1,1,&K.)<=&D2. )OR
    (SUBSTR(SICK_SYM2,1,&K.) >=&D1. & SUBSTR(SICK_SYM2,1,&K.)<=&D2.);
DUPKEY=COMPRESS(LEFT(MOTHER_ID))||("-")||COMPRESS(LEFT(PREG_DDATE));
RUN;

PROC SQL; CREATE TABLE &OUTPUT. AS SELECT DUPKEY, SUM(BD_INDEX) AS BD_SUM FROM OUT  GROUP BY DUPKEY; QUIT;
DATA &OUTPUT.; SET &OUTPUT.; IF BD_SUM>=2 THEN &VAR.=1; ELSE &VAR.=0; DROP BD_SUM; RUN;
%MEND; 

/*연구기간 내 첫번째 임신한 경우 기저질환 매크로*/
%BD(MTH_BD1,G1_BD1   ,BD1  ,3,"J42","J44");
%BD(MTH_BD1,G1_BD2   ,BD2  ,3,"J45","J45");
%BD(MTH_BD1,G1_BD3   ,BD3  ,3,"A15","A19");
%BD(MTH_BD1,G1_BD4   ,BD4  ,3,"I50","I50");
%BD(MTH_BD1,G1_BD5   ,BD5  ,3,"I05","I05");
%BD(MTH_BD1,G1_BD6   ,BD6  ,3,"I34","I34");
%BD(MTH_BD1,G1_BD7   ,BD7  ,3,"Q23","Q23");
%BD(MTH_BD1,G1_BD8   ,BD8  ,3,"I35","I35");
%BD(MTH_BD1,G1_BD9   ,BD9  ,3,"I06","I06");
%BD(MTH_BD1,G1_BD10,BD10,3,"I08","I08");
%BD(MTH_BD1,G1_BD11,BD11,3,"Q20","Q21");
%BD(MTH_BD1,G1_BD12,BD12,3,"J84","J84");
%BD(MTH_BD1,G1_BD13,BD13,3,"I26","I26");
%BD(MTH_BD1,G1_BD14,BD14,3,"J47","J47");
%BD(MTH_BD1,G1_BD15,BD15,3,"N18","N19");
%BD(MTH_BD1,G1_BD16,BD16,3,"M32","M32");
%BD(MTH_BD1,G1_BD17,BD17,3,"M05","M05");
%BD(MTH_BD1,G1_BD18,BD18,1,"C","C");

/*연구기간 내 두번째 임신한 경우 기저질환 매크로*/
%BD(MTH_BD2,G2_BD1   ,BD1  ,3,"J42","J44");
%BD(MTH_BD2,G2_BD2   ,BD2  ,3,"J45","J45");
%BD(MTH_BD2,G2_BD3   ,BD3  ,3,"A15","A19");
%BD(MTH_BD2,G2_BD4   ,BD4  ,3,"I50","I50");
%BD(MTH_BD2,G2_BD5   ,BD5  ,3,"I05","I05");
%BD(MTH_BD2,G2_BD6   ,BD6  ,3,"I34","I34");
%BD(MTH_BD2,G2_BD7   ,BD7  ,3,"Q23","Q23");
%BD(MTH_BD2,G2_BD8   ,BD8  ,3,"I35","I35");
%BD(MTH_BD2,G2_BD9   ,BD9  ,3,"I06","I06");
%BD(MTH_BD2,G2_BD10,BD10,3,"I08","I08");
%BD(MTH_BD2,G2_BD11,BD11,3,"Q20","Q21");
%BD(MTH_BD2,G2_BD12,BD12,3,"J84","J84");
%BD(MTH_BD2,G2_BD13,BD13,3,"I26","I26");
%BD(MTH_BD2,G2_BD14,BD14,3,"J47","J47");
%BD(MTH_BD2,G2_BD15,BD15,3,"N18","N19");
%BD(MTH_BD2,G2_BD16,BD16,3,"M32","M32");
%BD(MTH_BD2,G2_BD17,BD17,3,"M05","M05");
%BD(MTH_BD2,G2_BD18,BD18,1,"C","C");

/*연구기간 내 두번째 임신한 경우 기저질환 매크로*/
%BD(MTH_BD3,G3_BD1   ,BD1  ,3,"J42","J44");
%BD(MTH_BD3,G3_BD2   ,BD2  ,3,"J45","J45");
%BD(MTH_BD3,G3_BD3   ,BD3  ,3,"A15","A19");
%BD(MTH_BD3,G3_BD4   ,BD4  ,3,"I50","I50");
%BD(MTH_BD3,G3_BD5   ,BD5  ,3,"I05","I05");
%BD(MTH_BD3,G3_BD6   ,BD6  ,3,"I34","I34");
%BD(MTH_BD3,G3_BD7   ,BD7  ,3,"Q23","Q23");
%BD(MTH_BD3,G3_BD8   ,BD8  ,3,"I35","I35");
%BD(MTH_BD3,G3_BD9   ,BD9  ,3,"I06","I06");
%BD(MTH_BD3,G3_BD10,BD10,3,"I08","I08");
%BD(MTH_BD3,G3_BD11,BD11,3,"Q20","Q21");
%BD(MTH_BD3,G3_BD12,BD12,3,"J84","J84");
%BD(MTH_BD3,G3_BD13,BD13,3,"I26","I26");
%BD(MTH_BD3,G3_BD14,BD14,3,"J47","J47");
%BD(MTH_BD3,G3_BD15,BD15,3,"N18","N19");
%BD(MTH_BD3,G3_BD16,BD16,3,"M32","M32");
%BD(MTH_BD3,G3_BD17,BD17,3,"M05","M05");
%BD(MTH_BD3,G3_BD18,BD18,1,"C","C");

/**********************************************************************************************/
/**********************************************************************************************/
/*임신중 기저질환 찾기, DM, Chronic hypertension, Congenital heeart disease*/

DATA PREG_OUT1_D1; SET AA.MTH_BD1; 
IF (SUBSTR(SICK_SYM1,1,3) >="E10" & SUBSTR(SICK_SYM1,1,3)<="E14") OR (SUBSTR(SICK_SYM1,1,4) in ("O240","O241","O243","O244","O249") ) OR
    (SUBSTR(SICK_SYM2,1,3) >="E10" & SUBSTR(SICK_SYM2,1,3)<="E14") OR (SUBSTR(SICK_SYM2,1,4) in ("O240","O241","O243","O244","O249") ) ;
DUPKEY=COMPRESS(LEFT(MOTHER_ID))||("-")||COMPRESS(LEFT(PREG_DDATE)); RUN;

DATA PREG_OUT1_D2; SET AA.MTH_BD2; 
IF (SUBSTR(SICK_SYM1,1,3) >="E10" & SUBSTR(SICK_SYM1,1,3)<="E14") OR (SUBSTR(SICK_SYM1,1,4) in ("O240","O241","O243","O244","O249") ) OR
    (SUBSTR(SICK_SYM2,1,3) >="E10" & SUBSTR(SICK_SYM2,1,3)<="E14") OR (SUBSTR(SICK_SYM2,1,4) in ("O240","O241","O243","O244","O249") ) ;
DUPKEY=COMPRESS(LEFT(MOTHER_ID))||("-")||COMPRESS(LEFT(PREG_DDATE)); RUN;

DATA PREG_OUT1_D3; SET AA.MTH_BD3; 
IF (SUBSTR(SICK_SYM1,1,3) >="E10" & SUBSTR(SICK_SYM1,1,3)<="E14") OR (SUBSTR(SICK_SYM1,1,4) in ("O240","O241","O243","O244","O249") ) OR
    (SUBSTR(SICK_SYM2,1,3) >="E10" & SUBSTR(SICK_SYM2,1,3)<="E14") OR (SUBSTR(SICK_SYM2,1,4) in ("O240","O241","O243","O244","O249") ) ;
DUPKEY=COMPRESS(LEFT(MOTHER_ID))||("-")||COMPRESS(LEFT(PREG_DDATE)); RUN;

DATA PREG_OUT2_D1; SET AA.MTH_BD1; 
IF (SUBSTR(SICK_SYM1,1,3) in ("O10","O16","I10",'I11',"I12","I13","I14","I15"))  OR
     (SUBSTR(SICK_SYM2,1,3) in ("O10","O16","I10",'I11',"I12","I13","I14","I15"))  ;
DUPKEY=COMPRESS(LEFT(MOTHER_ID))||("-")||COMPRESS(LEFT(PREG_DDATE)); RUN;

DATA PREG_OUT2_D2; SET AA.MTH_BD2; 
IF (SUBSTR(SICK_SYM1,1,3) in ("O10","O16","I10",'I11',"I12","I13","I14","I15"))  OR
     (SUBSTR(SICK_SYM2,1,3) in ("O10","O16","I10",'I11',"I12","I13","I14","I15"))  ;
DUPKEY=COMPRESS(LEFT(MOTHER_ID))||("-")||COMPRESS(LEFT(PREG_DDATE)); RUN;

DATA PREG_OUT2_D3; SET AA.MTH_BD3; 
IF (SUBSTR(SICK_SYM1,1,3) in ("O10","O16","I10",'I11',"I12","I13","I14","I15"))  OR
     (SUBSTR(SICK_SYM2,1,3) in ("O10","O16","I10",'I11',"I12","I13","I14","I15"))  ;
DUPKEY=COMPRESS(LEFT(MOTHER_ID))||("-")||COMPRESS(LEFT(PREG_DDATE)); RUN;

DATA PREG_OUT3_D1; SET AA.MTH_BD1; 
IF (SUBSTR(SICK_SYM1,1,3) >="Q20" & SUBSTR(SICK_SYM1,1,3)<="Q28")  OR
     (SUBSTR(SICK_SYM2,1,3) >="Q20" & SUBSTR(SICK_SYM1,1,3)<="Q28")  ;
DUPKEY=COMPRESS(LEFT(MOTHER_ID))||("-")||COMPRESS(LEFT(PREG_DDATE)); RUN;

DATA PREG_OUT3_D2; SET AA.MTH_BD2; 
IF (SUBSTR(SICK_SYM1,1,3) >="Q20" & SUBSTR(SICK_SYM1,1,3)<="Q28")  OR
     (SUBSTR(SICK_SYM2,1,3) >="Q20" & SUBSTR(SICK_SYM1,1,3)<="Q28")  ;
DUPKEY=COMPRESS(LEFT(MOTHER_ID))||("-")||COMPRESS(LEFT(PREG_DDATE)); RUN;

DATA PREG_OUT3_D3; SET AA.MTH_BD3; 
IF (SUBSTR(SICK_SYM1,1,3) >="Q20" & SUBSTR(SICK_SYM1,1,3)<="Q28")  OR
     (SUBSTR(SICK_SYM2,1,3) >="Q20" & SUBSTR(SICK_SYM1,1,3)<="Q28")  ;
DUPKEY=COMPRESS(LEFT(MOTHER_ID))||("-")||COMPRESS(LEFT(PREG_DDATE)); RUN;

/*임신중 DM 여부*/
PROC SQL; CREATE TABLE  PREG_OCNT1_D1 AS SELECT DUPKEY, SUM(BD_INDEX2) AS BD_SUM FROM PREG_OUT1_D1  GROUP BY DUPKEY; QUIT;
PROC SQL; CREATE TABLE  PREG_OCNT1_D2 AS SELECT DUPKEY, SUM(BD_INDEX2) AS BD_SUM FROM PREG_OUT1_D2  GROUP BY DUPKEY; QUIT;
PROC SQL; CREATE TABLE  PREG_OCNT1_D3 AS SELECT DUPKEY, SUM(BD_INDEX2) AS BD_SUM FROM PREG_OUT1_D3  GROUP BY DUPKEY; QUIT;

/*임신중 Chronic hypertenstion 여부*/
PROC SQL; CREATE TABLE  PREG_OCNT2_D1 AS SELECT DUPKEY, SUM(BD_INDEX2) AS BD_SUM FROM PREG_OUT2_D1  GROUP BY DUPKEY; QUIT;
PROC SQL; CREATE TABLE  PREG_OCNT2_D2 AS SELECT DUPKEY, SUM(BD_INDEX2) AS BD_SUM FROM PREG_OUT2_D2  GROUP BY DUPKEY; QUIT;
PROC SQL; CREATE TABLE  PREG_OCNT2_D3 AS SELECT DUPKEY, SUM(BD_INDEX2) AS BD_SUM FROM PREG_OUT2_D3  GROUP BY DUPKEY; QUIT;

/*임신중 Congenital heart disease 여부*/
PROC SQL; CREATE TABLE  PREG_OCNT3_D1 AS SELECT DUPKEY, SUM(BD_INDEX2) AS BD_SUM FROM PREG_OUT3_D1  GROUP BY DUPKEY; QUIT;
PROC SQL; CREATE TABLE  PREG_OCNT3_D2 AS SELECT DUPKEY, SUM(BD_INDEX2) AS BD_SUM FROM PREG_OUT3_D2  GROUP BY DUPKEY; QUIT;
PROC SQL; CREATE TABLE  PREG_OCNT3_D3 AS SELECT DUPKEY, SUM(BD_INDEX2) AS BD_SUM FROM PREG_OUT3_D3  GROUP BY DUPKEY; QUIT;

/*임신중 DM 여부 2건 이상 식별되면 기저질환 */
DATA PREG_OCNT1_D1; SET PREG_OCNT1_D1; IF BD_SUM>=2 THEN BD19=1; ELSE BD19=0; DROP BD_SUM; RUN;
DATA PREG_OCNT1_D2; SET PREG_OCNT1_D2; IF BD_SUM>=2 THEN BD19=1; ELSE BD19=0; DROP BD_SUM; RUN;
DATA PREG_OCNT1_D3; SET PREG_OCNT1_D3; IF BD_SUM>=2 THEN BD19=1; ELSE BD19=0; DROP BD_SUM; RUN;

/*임신중 Chronic hypertenstion 여부 2건 이상 식별되면 기저질환*/
DATA PREG_OCNT2_D1; SET PREG_OCNT2_D1; IF BD_SUM>=2 THEN BD20=1; ELSE BD20=0; DROP BD_SUM; RUN;
DATA PREG_OCNT2_D2; SET PREG_OCNT2_D2; IF BD_SUM>=2 THEN BD20=1; ELSE BD20=0; DROP BD_SUM; RUN;
DATA PREG_OCNT2_D3; SET PREG_OCNT2_D3; IF BD_SUM>=2 THEN BD20=1; ELSE BD20=0; DROP BD_SUM; RUN;

/*임신중 Congenital heart disease 여부 2건 이상 식별되면 기저질환*/
DATA PREG_OCNT3_D1; SET PREG_OCNT3_D1; IF BD_SUM>=2 THEN BD21=1; ELSE BD21=0; DROP BD_SUM; RUN;
DATA PREG_OCNT3_D2; SET PREG_OCNT3_D2; IF BD_SUM>=2 THEN BD21=1; ELSE BD21=0; DROP BD_SUM; RUN;
DATA PREG_OCNT3_D3; SET PREG_OCNT3_D3; IF BD_SUM>=2 THEN BD21=1; ELSE BD21=0; DROP BD_SUM; RUN;

/**********************************************************************************************/
/**********************************************************************************************/

/*각 첫번째-세번째 임신한 경우 기저질환자 뽑아서 정리 한거 MERGE*/
DATA G1_BD_ALL; MERGE MTH_DUP1_ID G1_BD1-G1_BD18 PREG_OCNT1_D1 PREG_OCNT2_D1 PREG_OCNT3_D1; BY DUPKEY; RUN;
DATA G2_BD_ALL; MERGE MTH_DUP2_ID G2_BD1-G2_BD18 PREG_OCNT1_D2 PREG_OCNT2_D2 PREG_OCNT3_D2; BY DUPKEY; RUN;
DATA G3_BD_ALL; MERGE MTH_DUP3_ID G3_BD1-G3_BD18 PREG_OCNT1_D3 PREG_OCNT2_D3 PREG_OCNT3_D3; BY DUPKEY; RUN;

DATA G1_BD_ALL; SET G1_BD_ALL;
IF BD1="." THEN BD1=0;       IF BD2="." THEN BD2=0;     IF BD3="." THEN BD3=0; 
IF BD4="." THEN BD4=0;       IF BD5="." THEN BD5=0;     IF BD6="." THEN BD6=0; 
IF BD7="." THEN BD7=0;       IF BD8="." THEN BD8=0;     IF BD9="." THEN BD9=0; 
IF BD10="." THEN BD10=0;  IF BD11="." THEN BD11=0; IF BD12="." THEN BD12=0; 
IF BD13="." THEN BD13=0;  IF BD14="." THEN BD14=0; IF BD15="." THEN BD15=0; 
IF BD16="." THEN BD16=0;  IF BD17="." THEN BD17=0;  IF BD18="." THEN BD18=0; 
IF BD19="." THEN BD19=0;  IF BD20="." THEN BD20=0;  IF BD21="." THEN BD21=0; 
RUN;

DATA G2_BD_ALL; SET G2_BD_ALL;
IF BD1="." THEN BD1=0;       IF BD2="." THEN BD2=0;     IF BD3="." THEN BD3=0; 
IF BD4="." THEN BD4=0;       IF BD5="." THEN BD5=0;     IF BD6="." THEN BD6=0; 
IF BD7="." THEN BD7=0;       IF BD8="." THEN BD8=0;     IF BD9="." THEN BD9=0; 
IF BD10="." THEN BD10=0;  IF BD11="." THEN BD11=0; IF BD12="." THEN BD12=0; 
IF BD13="." THEN BD13=0;  IF BD14="." THEN BD14=0; IF BD15="." THEN BD15=0; 
IF BD16="." THEN BD16=0;  IF BD17="." THEN BD17=0;  IF BD18="." THEN BD18=0; 
IF BD19="." THEN BD19=0;  IF BD20="." THEN BD20=0;  IF BD21="." THEN BD21=0; 
RUN;

DATA G3_BD_ALL; SET G3_BD_ALL;
IF BD1="." THEN BD1=0;       IF BD2="." THEN BD2=0;     IF BD3="." THEN BD3=0; 
IF BD4="." THEN BD4=0;       IF BD5="." THEN BD5=0;     IF BD6="." THEN BD6=0; 
IF BD7="." THEN BD7=0;       IF BD8="." THEN BD8=0;     IF BD9="." THEN BD9=0; 
IF BD10="." THEN BD10=0;  IF BD11="." THEN BD11=0; IF BD12="." THEN BD12=0; 
IF BD13="." THEN BD13=0;  IF BD14="." THEN BD14=0; IF BD15="." THEN BD15=0; 
IF BD16="." THEN BD16=0;  IF BD17="." THEN BD17=0;  IF BD18="." THEN BD18=0; 
IF BD19="." THEN BD19=0;  IF BD20="." THEN BD20=0;  IF BD21="." THEN BD21=0; 
RUN;

/*각 임신 시점별 기저질환 산출한  부분 merge 하기*/
DATA AA.MTH_BD; SET G1_BD_ALL G2_BD_ALL G3_BD_ALL; RUN;
DATA SAPTMP.MTH_BD; SET AA.MTH_BD; RUN;

/*기저질환 정리*/
PROC FREQ DATA=AA.MTH_BD; TABLES BD1-BD21; RUN;

/*그냥 붙이면 쌍둥이 대상자는 DUPKEY가 여러개니까 산모-임신일 고유한 값만 만들어서  대상자 연계*/
PROC SORT DATA=AA.MTH_BD NODUPKEY OUT=AA.MTH_BD_UNIQ; BY DUPKEY; RUN;

/*대상자, 자격, 코로나 감염, 백신 여부, 기저질환 연결*/
PROC SQL; CREATE TABLE AA.TARGET_COVID_VAC_BD AS SELECT * FROM AA.TARGET_COVID_VAC AS A LEFT JOIN 
AA.MTH_BD_UNIQ AS B ON A.DUPKEY=B.DUPKEY; QUIT;

DATA SAPTMP.TARGET_COVID_VAC_BD; SET AA.TARGET_COVID_VAC_BD; RUN;

/**********************************************************************************************/
/**********************************************************************************************/
/*단태아/다태아 유무 식별목적*/
/*2020.01 ~ 2022.03 CMN_KEY 해당하는 경우*/
PROC SQL; 
&CONNECT.;
CREATE TABLE SAPTMP.MC_CMN_KEY AS SELECT * FROM CONNECTION TO X1(
    SELECT *
    FROM NHISBASE.HBMV_TBGJME20  /**T20 DB**/
	WHERE  CMN_KEY IN (SELECT CMN_KEY FROM TARGET_COVID_VAC_BD)
	AND HIRA_EXM_YYYYMM<='202204'
    AND PAY_YN='1'
	AND FORM_CD IN ('02','03','07','08','09','10','11','15')
	/*2018 (시점) 2020.1~2022.3*/
	AND MDCARE_STRT_YYYYMM IN 
('202001','202002','202003','202004','202005','202006','202007','202008','202009','202010','202011','202012',
'202101','202102','202103','202104','202105','202106','202107','202108','202109','202110','202111','202112',
'202201','202202','202203')
	AND INDI_DSCM_NO<>0 
	AND INDI_DSCM_NO IS NOT NULL
	AND INDI_DSCM_NO<90000000
	/*주상병 */
);DISCONNECT FROM X1;
QUIT; 

DATA AA.MC_CMN_KEY; SET SAPTMP.MC_CMN_KEY; 
IF (SUBSTR(SICK_SYM1,1,3) >="O80" & SUBSTR(SICK_SYM1,1,3)<="O83") OR 
    (SUBSTR(SICK_SYM1,1,4)  >="Z380" & SUBSTR(SICK_SYM1,1,4)<="Z382")OR
    (SUBSTR(SICK_SYM2,1,3) >="O80" & SUBSTR(SICK_SYM2,1,3)<="O83") OR 
    (SUBSTR(SICK_SYM2,1,4)  >="Z380" & SUBSTR(SICK_SYM2,1,4)<="Z382")OR
	(SUBSTR(SICK_SYM3,1,3) >="O80" & SUBSTR(SICK_SYM3,1,3)<="O83") OR 
    (SUBSTR(SICK_SYM3,1,4)  >="Z380" & SUBSTR(SICK_SYM3,1,4)<="Z382")OR
    (SUBSTR(SICK_SYM4,1,3) >="O80" & SUBSTR(SICK_SYM4,1,3)<="O83") OR 
    (SUBSTR(SICK_SYM4,1,4)  >="Z380" & SUBSTR(SICK_SYM4,1,4)<="Z382")OR
	(SUBSTR(SICK_SYM5,1,3) >="O80" & SUBSTR(SICK_SYM5,1,3)<="O83") OR 
    (SUBSTR(SICK_SYM5,1,4)  >="Z380" & SUBSTR(SICK_SYM5,1,4)<="Z382") THEN multi=1;
	else if
    (SUBSTR(SICK_SYM1,1,3) >="O84" & SUBSTR(SICK_SYM1,1,3)<="O84") OR 
    (SUBSTR(SICK_SYM1,1,4)  >="Z383" & SUBSTR(SICK_SYM1,1,4)<="Z388")OR
    (SUBSTR(SICK_SYM2,1,3) >="O84" & SUBSTR(SICK_SYM2,1,3)<="O84") OR 
    (SUBSTR(SICK_SYM2,1,4)  >="Z383" & SUBSTR(SICK_SYM2,1,4)<="Z388")OR
	(SUBSTR(SICK_SYM3,1,3) >="O84" & SUBSTR(SICK_SYM3,1,3)<="O84") OR 
    (SUBSTR(SICK_SYM3,1,4)  >="Z383" & SUBSTR(SICK_SYM3,1,4)<="Z388")OR
    (SUBSTR(SICK_SYM4,1,3) >="O84" & SUBSTR(SICK_SYM4,1,3)<="O84") OR 
    (SUBSTR(SICK_SYM4,1,4)  >="Z383" & SUBSTR(SICK_SYM4,1,4)<="Z388")OR
	(SUBSTR(SICK_SYM5,1,3) >="O84" & SUBSTR(SICK_SYM5,1,3)<="O84") OR 
    (SUBSTR(SICK_SYM5,1,4)  >="Z383" & SUBSTR(SICK_SYM5,1,4)<="Z388") THEN multi=2;
	else multi=9;
KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1-SICK_SYM5 FORM_CD multi;
RUN;

/*ICD-10 코드로 단태아/다태아 유무 식별 (T20에서)*/
proc freq data=aa.mc_cmn_key; tables multi; run;
/*자료 연계하려고 연계키/ 단태아/다태아 여부만*/
DATA mc_cmn_key2; set AA.mc_cmn_key; keep cmn_key  multi; run;

PROC SQL; CREATE TABLE AA.TARGET_COVID_VAC_BD AS SELECT * FROM AA.TARGET_COVID_VAC_BD 
AS A LEFT JOIN MC_CMN_KEY2 AS B ON A.CMN_KEY=B.CMN_KEY;QUIT;

DATA SAPTMP.TARGET_COVID_VAC_BD; SET AA.TARGET_COVID_VAC_BD; RUN;
/**********************************************************************************************/
/**********************************************************************************************/
/*결과변수 정리*/
/*분만 후 7일 이내에 결과변수가 존재하는 경우*/
/*엄마 질환 /출생아 질환 따로보기 */

PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT TOP 1*
    FROM TARGET_COVID_VAC_BD /**임신주수 & 임신부 전수**/
);DISCONNECT FROM X1;
QUIT; 


/*Outcome 상병 혼재 되어있으니 엄마 상병 코드 쭉 보고*/
PROC SQL; 
&CONNECT.;
CREATE TABLE MTH_OUT AS SELECT * FROM CONNECTION TO X1(
    SELECT *
    FROM NHISBASE.HBMV_TBGJME20  /**T20 DB**/

	/*엄마 질환*/
	WHERE  INDI_DSCM_NO IN (SELECT MOTHER_ID FROM TARGET_COVID_VAC)
	AND HIRA_EXM_YYYYMM<='202204'
    AND PAY_YN='1'
	AND FORM_CD IN ('02','03','08')
	/*2020.1 ~2022.3*/
	AND MDCARE_STRT_YYYYMM IN 
('202001','202002','202003','202004','202005','202006','202007','202008','202009','202010','202011','202012',
'202101','202102','202103','202104','202105','202106','202107','202108','202109','202110','202111','202112',
'202201','202202','202203')
	AND INDI_DSCM_NO<>0 
	AND INDI_DSCM_NO IS NOT NULL
	AND INDI_DSCM_NO<90000000
	/*주상병 */
	 AND           
	              /*대분류1 (임신성 고혈압 질환)*/
	               /*Gestational hypertension*/
                   (LEFT(SICK_SYM1,3)='O13'                                             
			      /*Preelampsia*/
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O140' AND 'O141'
	               OR LEFT(SICK_SYM1,4)='O149'                                        
			      /*Eclampsia*/
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O150' AND 'O152'
	               OR LEFT(SICK_SYM1,4)='O159'     
	               /*HELLP Syndrome*/
	               OR LEFT(SICK_SYM1,4)='O142'        
	               /*Superimposed preelampsia*/
	               OR LEFT(SICK_SYM1,3)='O11'        
				   /*대분류2 DM 기저질환으로 */

				   /*대분류3 (산전출혈, APH)*/
	               /*Threatened abortion*/
	               OR LEFT(SICK_SYM1,4)='O200'
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O208' AND 'O209'
	               /*Placenta previa*/
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O440' AND 'O441'
	               /*Plancental abruption*/
	               OR LEFT(SICK_SYM1,4)='O450'
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O458' AND 'O459'
			       /*Antepartum hemorrhage, unspecified*/
	               OR LEFT(SICK_SYM1,4)='O460'
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O468' AND 'O469'
			       /*Varginal bleeding*/
				   OR LEFT(SICK_SYM1,4) BETWEEN 'N938' AND 'N939'

				   /*대분류4 PPH/
	               /*Postpartum hemorrhage*/
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O720' AND 'O721'

				   /*대분류5 FDIU/
	               /*Abortion (pregnancy loss <20 weeks)*/
				   OR LEFT(SICK_SYM1,4)='O021'
				   OR LEFT(SICK_SYM1,3)='O03'
				  /*Maternal care for intrauterine death*/
				   OR LEFT(SICK_SYM1,4)='O364'
				  /*stillbirth (pregnancy loss >= weeks)*/
				   OR LEFT(SICK_SYM1,4)='O364'
				   /*stillbirth (1) singleton*/
				  OR LEFT(SICK_SYM1,3)='P95'
                  OR LEFT(SICK_SYM1,5)='Z3719'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'Z3710' AND 'Z3713'

			       /*stillbirth (2)  twin*/
                  OR LEFT(SICK_SYM1,5)='Z3739'
                  OR LEFT(SICK_SYM1,5)='Z3749'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'Z3730' AND 'Z3733'
                  OR LEFT(SICK_SYM1,5) BETWEEN 'Z3740' AND 'Z3743'

			       /*stillbirth (3)  multiple*/
                  OR LEFT(SICK_SYM1,5)='Z3769'
                  OR LEFT(SICK_SYM1,5)='Z3779'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'Z3760' AND 'Z3763'
                  OR LEFT(SICK_SYM1,5) BETWEEN 'Z3770' AND 'Z3773'

				   /*대분류6 PTB/
	               /*PTB*/
                  OR LEFT(SICK_SYM1,5)='P0729'
				  OR LEFT(SICK_SYM1,5)='P0739'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'P0720' AND 'P0725'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'P0730' AND 'P0732'

                  /*대분류7 Low birth weight/
	               /*LBW*/
                  OR LEFT(SICK_SYM1,5)='P0719'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'P0700' AND 'P0702'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'P0709' AND 'P0710'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'P0711' AND 'P0714'

                  /*대분류8 SGA (Small for gestational age) infantst*/
                 /*Light for gestational age */
				  OR LEFT(SICK_SYM1,4) BETWEEN 'P050' AND 'P051'
	               /*Maternal care for poor fetal growth*/
                  OR LEFT(SICK_SYM1,4)='O365'

				  /*대분류9 Neonatal respiratory disorders*/
                  /*RDS (respiratory distress syndrome) */
                  OR LEFT(SICK_SYM1,4)='P220'
				  /*BPD (bronchopulmonary dysplasia) */
				  OR LEFT(SICK_SYM1,5)='P2719'
                  OR LEFT(SICK_SYM1,5) BETWEEN 'P2711' AND 'P2712'
                 /*TTN (transient tachypnea of newborn)*/
				  OR LEFT(SICK_SYM1,4)='P221'
				  /*대분류10 IVH (intraventricular hemorrhage)*/
                  /*IVH */
				  OR LEFT(SICK_SYM1,4)='P523'
                  OR LEFT(SICK_SYM1,4) BETWEEN 'P520' AND 'P521'
                  OR LEFT(SICK_SYM1,5) BETWEEN 'P5220' AND 'P5221'
				   /*대분류11 HIE (hypoxic-ischemic encephalopathy)*/
				  /*HIE (hypoxic-ischemic encephalopathy)*/
				  OR LEFT(SICK_SYM1,4)='P916'
				   /*대분류12 Neonatal sepsis*/
				  /*sepsis of newborn*/
                  OR LEFT(SICK_SYM1,4) BETWEEN 'P360' AND 'P365'
				  OR LEFT(SICK_SYM1,4) BETWEEN 'P368' AND 'P369'
				  /*대분류13 neonatal interstinal perforation*/
				  /*NEC (necrotizing enterocolitis)*/
				  OR LEFT(SICK_SYM1,3)='P77'
				  /*perinatal intestinal perforation*/
				  OR LEFT(SICK_SYM1,4)='P780'
				  /*대분류14 ROP (resinopathy of prematurity)*/
				  OR LEFT(SICK_SYM1,5)='H3510'
				  OR LEFT(SICK_SYM1,5)='H3519'
                  OR LEFT(SICK_SYM1,5) BETWEEN 'H3511' AND 'H3514'
                  /*대분류15 PDA (patent ductus arteriosus)*/
				  OR LEFT(SICK_SYM1,4)='P293'
				  OR LEFT(SICK_SYM1,4)='Q250'

				  /*부상병*/				
	              /*대분류1 (임신성 고혈압 질환)*/			
	               /*Gestational hypertension*/			
                   OR LEFT(SICK_SYM2,3)='O13'                                             				
			      /*Preelampsia*/	
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O140' AND 'O141'
	               OR LEFT(SICK_SYM2,4)='O149'                                        			
			      /*Eclampsia*/	
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O150' AND 'O152'
	               OR LEFT(SICK_SYM2,4)='O159'     			
	               /*HELLP Syndrome*/			
	               OR LEFT(SICK_SYM2,4)='O142'        			
	               /*Superimposed preelampsia*/			
	               OR LEFT(SICK_SYM2,3)='O11'        			
				   /*대분류2 DM 기저질환으로 */
				
				   /*대분류3 (산전출혈, APH)*/
	               /*Threatened abortion*/			
	               OR LEFT(SICK_SYM2,4)='O200'			
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O208' AND 'O209'
	               /*Placenta previa*/			
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O440' AND 'O441'
	               /*Plancental abruption*/			
	               OR LEFT(SICK_SYM2,4)='O450'			
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O458' AND 'O459'
			       /*Antepartum hemorrhage, unspecified*/	
	               OR LEFT(SICK_SYM2,4)='O460'			
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O468' AND 'O469'
			       /*Varginal bleeding*/	
				   OR LEFT(SICK_SYM2,4) BETWEEN 'N938' AND 'N939'
				
				   /*대분류4 PPH/
	               /*Postpartum hemorrhage*/			
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O720' AND 'O721'
				
				   /*대분류5 FDIU/
	               /*Abortion (pregnancy loss <20 weeks)*/			
				   OR LEFT(SICK_SYM2,4)='O021'
				   OR LEFT(SICK_SYM2,3)='O03'
				  /*Maternal care for intrauterine death*/
				   OR LEFT(SICK_SYM2,4)='O364'
				  /*stillbirth (pregnancy loss >= weeks)*/
				   OR LEFT(SICK_SYM2,4)='O364'
				   /*stillbirth (1) singleton*/
				  OR LEFT(SICK_SYM2,3)='P95'
                  OR LEFT(SICK_SYM2,5)='Z3719'				
				  OR LEFT(SICK_SYM2,5) BETWEEN 'Z3710' AND 'Z3713'
				
			       /*stillbirth (2)  twin*/	
                  OR LEFT(SICK_SYM2,5)='Z3739'				
                  OR LEFT(SICK_SYM2,5)='Z3749'				
				  OR LEFT(SICK_SYM2,5) BETWEEN 'Z3730' AND 'Z3733'
                  OR LEFT(SICK_SYM2,5) BETWEEN 'Z3740' AND 'Z3743'				
				
			       /*stillbirth (3)  multiple*/	
                  OR LEFT(SICK_SYM2,5)='Z3769'				
                  OR LEFT(SICK_SYM2,5)='Z3779'				
				  OR LEFT(SICK_SYM2,5) BETWEEN 'Z3760' AND 'Z3763'
                  OR LEFT(SICK_SYM2,5) BETWEEN 'Z3770' AND 'Z3773'				
				
				   /*대분류6 PTB/
	               /*PTB*/			
                  OR LEFT(SICK_SYM2,5)='P0729'				
				  OR LEFT(SICK_SYM2,5)='P0739'
				  OR LEFT(SICK_SYM2,5) BETWEEN 'P0720' AND 'P0725'
				  OR LEFT(SICK_SYM2,5) BETWEEN 'P0730' AND 'P0732'
				
                  /*대분류7 Low birth weight/				
	               /*LBW*/			
                  OR LEFT(SICK_SYM2,5)='P0719'				
				  OR LEFT(SICK_SYM2,5) BETWEEN 'P0700' AND 'P0702'
				  OR LEFT(SICK_SYM2,5) BETWEEN 'P0709' AND 'P0710'
				  OR LEFT(SICK_SYM2,5) BETWEEN 'P0711' AND 'P0714'
				
                  /*대분류8 SGA (Small for gestational age) infantst*/				
                 /*Light for gestational age */				
				  OR LEFT(SICK_SYM2,4) BETWEEN 'P050' AND 'P051'
	               /*Maternal care for poor fetal growth*/			
                  OR LEFT(SICK_SYM2,4)='O365'				
				
				  /*대분류9 Neonatal respiratory disorders*/
                  /*RDS (respiratory distress syndrome) */				
                  OR LEFT(SICK_SYM2,4)='P220'				
				  /*BPD (bronchopulmonary dysplasia) */
				  OR LEFT(SICK_SYM2,5)='P2719'
                  OR LEFT(SICK_SYM2,5) BETWEEN 'P2711' AND 'P2712'				
                 /*TTN (transient tachypnea of newborn)*/				
				  OR LEFT(SICK_SYM2,4)='P221'
				  /*대분류10 IVH (intraventricular hemorrhage)*/
                  /*IVH */				
				  OR LEFT(SICK_SYM2,4)='P523'
                  OR LEFT(SICK_SYM2,4) BETWEEN 'P520' AND 'P521'				
                  OR LEFT(SICK_SYM2,5) BETWEEN 'P5220' AND 'P5221'				
				   /*대분류11 HIE (hypoxic-ischemic encephalopathy)*/
				  /*HIE (hypoxic-ischemic encephalopathy)*/
				  OR LEFT(SICK_SYM2,4)='P916'
				   /*대분류12 Neonatal sepsis*/
				  /*sepsis of newborn*/
                  OR LEFT(SICK_SYM2,4) BETWEEN 'P360' AND 'P365'				
				  OR LEFT(SICK_SYM2,4) BETWEEN 'P368' AND 'P369'
				  /*대분류13 neonatal interstinal perforation*/
				  /*NEC (necrotizing enterocolitis)*/
				  OR LEFT(SICK_SYM2,3)='P77'
				  /*perinatal intestinal perforation*/
				  OR LEFT(SICK_SYM2,4)='P780'
				  /*대분류14 ROP (resinopathy of prematurity)*/
				  OR LEFT(SICK_SYM2,5)='H3510'
				  OR LEFT(SICK_SYM2,5)='H3519'
                  OR LEFT(SICK_SYM2,5) BETWEEN 'H3511' AND 'H3514'				
                  /*대분류15 PDA (patent ductus arteriosus)*/				
				  OR LEFT(SICK_SYM2,4)='P293'
				  OR LEFT(SICK_SYM2,4)='Q250'
)
);DISCONNECT FROM X1;
QUIT; 


/**********************************************************************************************/
/**********************************************************************************************/
/*Outcome 상병 혼재 되어있으니 아이 상병 코드 쭉 보고*/
PROC SQL; 
&CONNECT.;
CREATE TABLE CHD_OUT AS SELECT * FROM CONNECTION TO X1(
    SELECT *
    FROM NHISBASE.HBMV_TBGJME20  /**T20 DB**/

	/*엄마 질환*/
	WHERE  INDI_DSCM_NO IN (SELECT CHILD_ID FROM TARGET_COVID_VAC)
	AND HIRA_EXM_YYYYMM<='202204'
    AND PAY_YN='1'
	AND FORM_CD IN ('02','03','08')
	/*2020.1 ~2022.3*/
	AND MDCARE_STRT_YYYYMM IN 
('202001','202002','202003','202004','202005','202006','202007','202008','202009','202010','202011','202012',
'202101','202102','202103','202104','202105','202106','202107','202108','202109','202110','202111','202112',
'202201','202202','202203')
	AND INDI_DSCM_NO<>0 
	AND INDI_DSCM_NO IS NOT NULL
	AND INDI_DSCM_NO<90000000
	/*주상병 */
	 AND           
	              /*대분류1 (임신성 고혈압 질환)*/
	               /*Gestational hypertension*/
                   (LEFT(SICK_SYM1,3)='O13'                                             
			      /*Preelampsia*/
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O140' AND 'O141'
	               OR LEFT(SICK_SYM1,4)='O149'                                        
			      /*Eclampsia*/
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O150' AND 'O152'
	               OR LEFT(SICK_SYM1,4)='O159'     
	               /*HELLP Syndrome*/
	               OR LEFT(SICK_SYM1,4)='O142'        
	               /*Superimposed preelampsia*/
	               OR LEFT(SICK_SYM1,3)='O11'        
				   /*대분류2 DM 기저질환으로 */

				   /*대분류3 (산전출혈, APH)*/
	               /*Threatened abortion*/
	               OR LEFT(SICK_SYM1,4)='O200'
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O208' AND 'O209'
	               /*Placenta previa*/
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O440' AND 'O441'
	               /*Plancental abruption*/
	               OR LEFT(SICK_SYM1,4)='O450'
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O458' AND 'O459'
			       /*Antepartum hemorrhage, unspecified*/
	               OR LEFT(SICK_SYM1,4)='O460'
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O468' AND 'O469'
			       /*Varginal bleeding*/
				   OR LEFT(SICK_SYM1,4) BETWEEN 'N938' AND 'N939'

				   /*대분류4 PPH/
	               /*Postpartum hemorrhage*/
				   OR LEFT(SICK_SYM1,4) BETWEEN 'O720' AND 'O721'

				   /*대분류5 FDIU/
	               /*Abortion (pregnancy loss <20 weeks)*/
				   OR LEFT(SICK_SYM1,4)='O021'
				   OR LEFT(SICK_SYM1,3)='O03'
				  /*Maternal care for intrauterine death*/
				   OR LEFT(SICK_SYM1,4)='O364'
				  /*stillbirth (pregnancy loss >= weeks)*/
				   OR LEFT(SICK_SYM1,4)='O364'
				   /*stillbirth (1) singleton*/
				  OR LEFT(SICK_SYM1,3)='P95'
                  OR LEFT(SICK_SYM1,5)='Z3719'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'Z3710' AND 'Z3713'

			       /*stillbirth (2)  twin*/
                  OR LEFT(SICK_SYM1,5)='Z3739'
                  OR LEFT(SICK_SYM1,5)='Z3749'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'Z3730' AND 'Z3733'
                  OR LEFT(SICK_SYM1,5) BETWEEN 'Z3740' AND 'Z3743'

			       /*stillbirth (3)  multiple*/
                  OR LEFT(SICK_SYM1,5)='Z3769'
                  OR LEFT(SICK_SYM1,5)='Z3779'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'Z3760' AND 'Z3763'
                  OR LEFT(SICK_SYM1,5) BETWEEN 'Z3770' AND 'Z3773'

				   /*대분류6 PTB/
	               /*PTB*/
                  OR LEFT(SICK_SYM1,5)='P0729'
				  OR LEFT(SICK_SYM1,5)='P0739'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'P0720' AND 'P0725'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'P0730' AND 'P0732'

                  /*대분류7 Low birth weight/
	               /*LBW*/
                  OR LEFT(SICK_SYM1,5)='P0719'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'P0700' AND 'P0702'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'P0709' AND 'P0710'
				  OR LEFT(SICK_SYM1,5) BETWEEN 'P0711' AND 'P0714'

                  /*대분류8 SGA (Small for gestational age) infantst*/
                 /*Light for gestational age */
				  OR LEFT(SICK_SYM1,4) BETWEEN 'P050' AND 'P051'
	               /*Maternal care for poor fetal growth*/
                  OR LEFT(SICK_SYM1,4)='O365'

				  /*대분류9 Neonatal respiratory disorders*/
                  /*RDS (respiratory distress syndrome) */
                  OR LEFT(SICK_SYM1,4)='P220'
				  /*BPD (bronchopulmonary dysplasia) */
				  OR LEFT(SICK_SYM1,5)='P2719'
                  OR LEFT(SICK_SYM1,5) BETWEEN 'P2711' AND 'P2712'
                 /*TTN (transient tachypnea of newborn)*/
				  OR LEFT(SICK_SYM1,4)='P221'
				  /*대분류10 IVH (intraventricular hemorrhage)*/
                  /*IVH */
				  OR LEFT(SICK_SYM1,4)='P523'
                  OR LEFT(SICK_SYM1,4) BETWEEN 'P520' AND 'P521'
                  OR LEFT(SICK_SYM1,5) BETWEEN 'P5220' AND 'P5221'
				   /*대분류11 HIE (hypoxic-ischemic encephalopathy)*/
				  /*HIE (hypoxic-ischemic encephalopathy)*/
				  OR LEFT(SICK_SYM1,4)='P916'
				   /*대분류12 Neonatal sepsis*/
				  /*sepsis of newborn*/
                  OR LEFT(SICK_SYM1,4) BETWEEN 'P360' AND 'P365'
				  OR LEFT(SICK_SYM1,4) BETWEEN 'P368' AND 'P369'
				  /*대분류13 neonatal interstinal perforation*/
				  /*NEC (necrotizing enterocolitis)*/
				  OR LEFT(SICK_SYM1,3)='P77'
				  /*perinatal intestinal perforation*/
				  OR LEFT(SICK_SYM1,4)='P780'
				  /*대분류14 ROP (resinopathy of prematurity)*/
				  OR LEFT(SICK_SYM1,5)='H3510'
				  OR LEFT(SICK_SYM1,5)='H3519'
                  OR LEFT(SICK_SYM1,5) BETWEEN 'H3511' AND 'H3514'
                  /*대분류15 PDA (patent ductus arteriosus)*/
				  OR LEFT(SICK_SYM1,4)='P293'
				  OR LEFT(SICK_SYM1,4)='Q250'

				  /*부상병*/				
	              /*대분류1 (임신성 고혈압 질환)*/			
	               /*Gestational hypertension*/			
                   OR LEFT(SICK_SYM2,3)='O13'                                             				
			      /*Preelampsia*/	
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O140' AND 'O141'
	               OR LEFT(SICK_SYM2,4)='O149'                                        			
			      /*Eclampsia*/	
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O150' AND 'O152'
	               OR LEFT(SICK_SYM2,4)='O159'     			
	               /*HELLP Syndrome*/			
	               OR LEFT(SICK_SYM2,4)='O142'        			
	               /*Superimposed preelampsia*/			
	               OR LEFT(SICK_SYM2,3)='O11'        			
				   /*대분류2 DM 기저질환으로 */
				
				   /*대분류3 (산전출혈, APH)*/
	               /*Threatened abortion*/			
	               OR LEFT(SICK_SYM2,4)='O200'			
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O208' AND 'O209'
	               /*Placenta previa*/			
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O440' AND 'O441'
	               /*Plancental abruption*/			
	               OR LEFT(SICK_SYM2,4)='O450'			
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O458' AND 'O459'
			       /*Antepartum hemorrhage, unspecified*/	
	               OR LEFT(SICK_SYM2,4)='O460'			
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O468' AND 'O469'
			       /*Varginal bleeding*/	
				   OR LEFT(SICK_SYM2,4) BETWEEN 'N938' AND 'N939'
				
				   /*대분류4 PPH/
	               /*Postpartum hemorrhage*/			
				   OR LEFT(SICK_SYM2,4) BETWEEN 'O720' AND 'O721'
				
				   /*대분류5 FDIU/
	               /*Abortion (pregnancy loss <20 weeks)*/			
				   OR LEFT(SICK_SYM2,4)='O021'
				   OR LEFT(SICK_SYM2,3)='O03'
				  /*Maternal care for intrauterine death*/
				   OR LEFT(SICK_SYM2,4)='O364'
				  /*stillbirth (pregnancy loss >= weeks)*/
				   OR LEFT(SICK_SYM2,4)='O364'
				   /*stillbirth (1) singleton*/
				  OR LEFT(SICK_SYM2,3)='P95'
                  OR LEFT(SICK_SYM2,5)='Z3719'				
				  OR LEFT(SICK_SYM2,5) BETWEEN 'Z3710' AND 'Z3713'
				
			       /*stillbirth (2)  twin*/	
                  OR LEFT(SICK_SYM2,5)='Z3739'				
                  OR LEFT(SICK_SYM2,5)='Z3749'				
				  OR LEFT(SICK_SYM2,5) BETWEEN 'Z3730' AND 'Z3733'
                  OR LEFT(SICK_SYM2,5) BETWEEN 'Z3740' AND 'Z3743'				
				
			       /*stillbirth (3)  multiple*/	
                  OR LEFT(SICK_SYM2,5)='Z3769'				
                  OR LEFT(SICK_SYM2,5)='Z3779'				
				  OR LEFT(SICK_SYM2,5) BETWEEN 'Z3760' AND 'Z3763'
                  OR LEFT(SICK_SYM2,5) BETWEEN 'Z3770' AND 'Z3773'				
				
				   /*대분류6 PTB/
	               /*PTB*/			
                  OR LEFT(SICK_SYM2,5)='P0729'				
				  OR LEFT(SICK_SYM2,5)='P0739'
				  OR LEFT(SICK_SYM2,5) BETWEEN 'P0720' AND 'P0725'
				  OR LEFT(SICK_SYM2,5) BETWEEN 'P0730' AND 'P0732'
				
                  /*대분류7 Low birth weight/				
	               /*LBW*/			
                  OR LEFT(SICK_SYM2,5)='P0719'				
				  OR LEFT(SICK_SYM2,5) BETWEEN 'P0700' AND 'P0702'
				  OR LEFT(SICK_SYM2,5) BETWEEN 'P0709' AND 'P0710'
				  OR LEFT(SICK_SYM2,5) BETWEEN 'P0711' AND 'P0714'
				
                  /*대분류8 SGA (Small for gestational age) infantst*/				
                 /*Light for gestational age */				
				  OR LEFT(SICK_SYM2,4) BETWEEN 'P050' AND 'P051'
	               /*Maternal care for poor fetal growth*/			
                  OR LEFT(SICK_SYM2,4)='O365'				
				
				  /*대분류9 Neonatal respiratory disorders*/
                  /*RDS (respiratory distress syndrome) */				
                  OR LEFT(SICK_SYM2,4)='P220'				
				  /*BPD (bronchopulmonary dysplasia) */
				  OR LEFT(SICK_SYM2,5)='P2719'
                  OR LEFT(SICK_SYM2,5) BETWEEN 'P2711' AND 'P2712'				
                 /*TTN (transient tachypnea of newborn)*/				
				  OR LEFT(SICK_SYM2,4)='P221'
				  /*대분류10 IVH (intraventricular hemorrhage)*/
                  /*IVH */				
				  OR LEFT(SICK_SYM2,4)='P523'
                  OR LEFT(SICK_SYM2,4) BETWEEN 'P520' AND 'P521'				
                  OR LEFT(SICK_SYM2,5) BETWEEN 'P5220' AND 'P5221'				
				   /*대분류11 HIE (hypoxic-ischemic encephalopathy)*/
				  /*HIE (hypoxic-ischemic encephalopathy)*/
				  OR LEFT(SICK_SYM2,4)='P916'
				   /*대분류12 Neonatal sepsis*/
				  /*sepsis of newborn*/
                  OR LEFT(SICK_SYM2,4) BETWEEN 'P360' AND 'P365'				
				  OR LEFT(SICK_SYM2,4) BETWEEN 'P368' AND 'P369'
				  /*대분류13 neonatal interstinal perforation*/
				  /*NEC (necrotizing enterocolitis)*/
				  OR LEFT(SICK_SYM2,3)='P77'
				  /*perinatal intestinal perforation*/
				  OR LEFT(SICK_SYM2,4)='P780'
				  /*대분류14 ROP (resinopathy of prematurity)*/
				  OR LEFT(SICK_SYM2,5)='H3510'
				  OR LEFT(SICK_SYM2,5)='H3519'
                  OR LEFT(SICK_SYM2,5) BETWEEN 'H3511' AND 'H3514'				
                  /*대분류15 PDA (patent ductus arteriosus)*/				
				  OR LEFT(SICK_SYM2,4)='P293'
				  OR LEFT(SICK_SYM2,4)='Q250'
)
);DISCONNECT FROM X1;
QUIT; 

/*엄마/아이 Outcome 자료*/
DATA AA.CHD_OUT; SET CHD_OUT ; KEEP INDI_DSCM_NO CMN_KEY SICK_SYM1 SICK_SYM2 FORM_CD  MDCARE_STRT_YYYYMM MDCARE_STRT_DT; RUN;
DATA AA.MTH_OUT; SET MTH_OUT; KEEP INDI_DSCM_NO CMN_KEY SICK_SYM1 SICK_SYM2 FORM_CD  MDCARE_STRT_YYYYMM MDCARE_STRT_DT; RUN;

DATA TG ; SET AA.TARGET_COVID_VAC;
IF COVID19=1 THEN COVID_DATE=MDY(SUBSTR(CDX_DT,5,2),SUBSTR(CDX_DT,7,2),SUBSTR(CDX_DT,1,4));
KEEP DUPKEY CDX_DT COVID19 COVID_DATE; RUN;

PROC SORT DATA=TG NODUPKEY OUT=TG2; BY DUPKEY; RUN;

PROC SQL; CREATE TABLE MTH_DUP1 AS SELECT * FROM MTH_DUP1 AS A LEFT JOIN TG2 AS B ON A.DUPKEY=B.DUPKEY; QUIT;
PROC SQL; CREATE TABLE MTH_DUP2 AS SELECT * FROM MTH_DUP2 AS A LEFT JOIN TG2 AS B ON A.DUPKEY=B.DUPKEY; QUIT;
PROC SQL; CREATE TABLE MTH_DUP3 AS SELECT * FROM MTH_DUP3 AS A LEFT JOIN TG2 AS B ON A.DUPKEY=B.DUPKEY; QUIT;

DATA AA.MTH_DUP1; SET MTH_DUP1; RUN;
DATA AA.MTH_DUP2; SET MTH_DUP2; RUN;
DATA AA.MTH_DUP3; SET MTH_DUP3; RUN;

/* 임신 첫번째~3번째 (시점별), 산모 결과변수 연결  (INNER JOIN)*/
PROC SQL; CREATE  TABLE AA.MTH_OUT1 AS SELECT * FROM AA.MTH_OUT AS A INNER JOIN AA.MTH_DUP1 AS B  ON A.INDI_DSCM_NO=B.MOTHER_ID; QUIT;
PROC SQL; CREATE  TABLE AA.MTH_OUT2 AS SELECT * FROM AA.MTH_OUT AS A INNER JOIN AA.MTH_DUP2 AS B  ON A.INDI_DSCM_NO=B.MOTHER_ID; QUIT;
PROC SQL; CREATE  TABLE AA.MTH_OUT3 AS SELECT * FROM AA.MTH_OUT AS A INNER JOIN AA.MTH_DUP3 AS B  ON A.INDI_DSCM_NO=B.MOTHER_ID; QUIT;

/* 아이 결과변수 연결  (INNER JOIN)*/
PROC SQL; CREATE  TABLE AA.CHD_OUT1 AS SELECT * FROM AA.CHD_OUT AS A INNER JOIN AA.TARGET_COVID_VAC_BD AS B  ON A.INDI_DSCM_NO=B.CHILD_ID; QUIT;

/**********************************************************************************************/
/**********************************************************************************************/
/*첫번째 임신경험시 HDP*/
DATA AA.M1_OUT1; SET AA.MTH_OUT1; IF SUBSTR(SICK_SYM1,1,3) IN ("O13") OR SUBSTR(SICK_SYM2,1,3) IN ("O13"); RUN;
DATA AA.M1_OUT2; SET AA.MTH_OUT1; IF  SUBSTR(SICK_SYM1,1,4) IN ("O140","O141","O149") OR  SUBSTR(SICK_SYM2,1,4) IN  ("O140","O141","O149"); RUN;
DATA AA.M1_OUT3; SET AA.MTH_OUT1; IF  SUBSTR(SICK_SYM1,1,4) IN ("O150","O151","O152","O159") OR  SUBSTR(SICK_SYM2,1,4) IN  ("O150","O151","O152","O159"); RUN;
DATA AA.M1_OUT4; SET AA.MTH_OUT1; IF  SUBSTR(SICK_SYM1,1,4) IN ("O142") OR  SUBSTR(SICK_SYM2,1,4) IN  ("O142"); RUN;
DATA AA.M1_OUT5; SET AA.MTH_OUT1; IF SUBSTR(SICK_SYM1,1,3) IN ("O15") OR SUBSTR(SICK_SYM2,1,3) IN ("O15"); RUN;

/*첫번째 임신경험시 APH, 산전출혈*/
DATA AA.M1_OUT6; SET AA.MTH_OUT1; if substr(SICK_SYM1,1,4) IN ("O200","O208","O209") OR SUBSTR(SICK_SYM2,1,4) IN ("O200","O208","O209"); RUN;
DATA AA.M1_OUT7; SET AA.MTH_OUT1; if substr(SICK_SYM1,1,4) IN ("O440","O441") OR SUBSTR(SICK_SYM2,1,4) IN ("O440","O441"); RUN;
DATA AA.M1_OUT8; SET AA.MTH_OUT1; if substr(SICK_SYM1,1,4) IN ("O450","O458","O459") OR SUBSTR(SICK_SYM2,1,4) IN ("O450","O458","O459"); RUN;
DATA AA.M1_OUT9; SET AA.MTH_OUT1; if substr(SICK_SYM1,1,4) IN ("O460","O468","O469") OR SUBSTR(SICK_SYM2,1,4) IN ("O460","O468","O469"); RUN;
DATA AA.M1_OUT10; SET AA.MTH_OUT1; if substr(SICK_SYM1,1,4) IN ("N938","N939") OR SUBSTR(SICK_SYM2,1,4) IN ("N938","N939"); RUN;

/*첫번째 임신경험시 PPH, 산전출혈*/
DATA AA.M1_OUT11; SET AA.MTH_OUT1; if substr(SICK_SYM1,1,4) IN ("O720","O721") OR SUBSTR(SICK_SYM2,1,4) IN ("O720","O721"); RUN;

/*첫번째 임신경험시 유산*/
DATA AA.M1_OUT12; SET AA.MTH_OUT1; if substr(SICK_SYM1,1,3) IN ("O03") OR substr(SICK_SYM1,1,4) IN ("O021")
OR substr(SICK_SYM2,1,3) IN ("O03") OR substr(SICK_SYM2,1,4) IN ("O021"); RUN;
DATA AA.M1_OUT13; SET AA.MTH_OUT1; if substr(SICK_SYM1,1,4) IN ("O364") OR SUBSTR(SICK_SYM2,1,4) IN ("O364"); RUN;

/*첫번째 임신경험시 사산*/
DATA AA.M1_OUT14; SET AA.MTH_OUT1; if substr(SICK_SYM1,1,3) IN ("P95") OR substr(SICK_SYM1,1,5) IN ("Z3710","Z3711","Z3712","Z3713","Z3719")
OR substr(SICK_SYM2,1,3) IN ("P95") OR substr(SICK_SYM2,1,5) IN ("Z3710","Z3711","Z3712","Z3713","Z3719"); RUN;
DATA AA.M1_OUT15; SET AA.MTH_OUT1; if  substr(SICK_SYM1,1,5) IN ("Z3730","Z3731","Z3732","Z3733","Z3739","Z3740","Z3741","Z3742","Z3743","Z3749")
OR   substr(SICK_SYM2,1,5) IN ("Z3730","Z3731","Z3732","Z3733","Z3739","Z3740","Z3741","Z3742","Z3743","Z3749"); RUN;
DATA AA.M1_OUT16; SET AA.MTH_OUT1; if  substr(SICK_SYM1,1,5) IN ("Z3760","Z3761","Z3762","Z3763","Z3769","Z3770","Z3771","Z3772","Z3773","Z3779")
OR   substr(SICK_SYM2,1,5) IN ("Z3760","Z3761","Z3762","Z3763","Z3769","Z3770","Z3771","Z3772","Z3773","Z3779"); RUN;

/*첫번째 임신경험시 SGA*/
DATA AA.M1_OUT21; SET AA.MTH_OUT1; IF SUBSTR(SICK_SYM1,1,4) IN ("O365") OR SUBSTR(SICK_SYM2,1,4) IN ("O365"); RUN;

/*두번째 임신경험시 HDP*/
DATA AA.M2_OUT1; SET AA.MTH_OUT2; IF SUBSTR(SICK_SYM1,1,3) IN ("O13") OR SUBSTR(SICK_SYM2,1,3) IN ("O13"); RUN;
DATA AA.M2_OUT2; SET AA.MTH_OUT2; IF  SUBSTR(SICK_SYM1,1,4) IN ("O140","O141","O149") OR  SUBSTR(SICK_SYM2,1,4) IN  ("O140","O141","O149"); RUN;
DATA AA.M2_OUT3; SET AA.MTH_OUT2; IF  SUBSTR(SICK_SYM1,1,4) IN ("O150","O151","O152","O159") OR  SUBSTR(SICK_SYM2,1,4) IN  ("O150","O151","O152","O159"); RUN;
DATA AA.M2_OUT4; SET AA.MTH_OUT2; IF  SUBSTR(SICK_SYM1,1,4) IN ("O142") OR  SUBSTR(SICK_SYM2,1,4) IN  ("O142"); RUN;
DATA AA.M2_OUT5; SET AA.MTH_OUT2; IF SUBSTR(SICK_SYM1,1,3) IN ("O15") OR SUBSTR(SICK_SYM2,1,3) IN ("O15"); RUN;

/*두번째 임신경험시 APH, 산전출혈*/
DATA AA.M2_OUT6; SET AA.MTH_OUT2; if substr(SICK_SYM1,1,4) IN ("O200","O208","O209") OR SUBSTR(SICK_SYM2,1,4) IN ("O200","O208","O209"); RUN;
DATA AA.M2_OUT7; SET AA.MTH_OUT2; if substr(SICK_SYM1,1,4) IN ("O440","O441") OR SUBSTR(SICK_SYM2,1,4) IN ("O440","O441"); RUN;
DATA AA.M2_OUT8; SET AA.MTH_OUT2; if substr(SICK_SYM1,1,4) IN ("O450","O458","O459") OR SUBSTR(SICK_SYM2,1,4) IN ("O450","O458","O459"); RUN;
DATA AA.M2_OUT9; SET AA.MTH_OUT2; if substr(SICK_SYM1,1,4) IN ("O460","O468","O469") OR SUBSTR(SICK_SYM2,1,4) IN ("O460","O468","O469"); RUN;
DATA AA.M2_OUT10; SET AA.MTH_OUT2; if substr(SICK_SYM1,1,4) IN ("N938","N939") OR SUBSTR(SICK_SYM2,1,4) IN ("N938","N939"); RUN;

/*두번째  임신경험시 PPH, 산전출혈*/
DATA AA.M2_OUT11; SET AA.MTH_OUT2; if substr(SICK_SYM1,1,4) IN ("O720","O721") OR SUBSTR(SICK_SYM2,1,4) IN ("O720","O721"); RUN;

/*두번째 임신경험시 유산*/
DATA AA.M2_OUT12; SET AA.MTH_OUT2; if substr(SICK_SYM1,1,3) IN ("O03") OR substr(SICK_SYM1,1,4) IN ("O021")
OR substr(SICK_SYM2,1,3) IN ("O03") OR substr(SICK_SYM2,1,4) IN ("O021"); RUN;
DATA AA.M2_OUT13; SET AA.MTH_OUT2; if substr(SICK_SYM1,1,4) IN ("O364") OR SUBSTR(SICK_SYM2,1,4) IN ("O364"); RUN;

/*두번째 임신경험시 사산*/
DATA AA.M2_OUT14; SET AA.MTH_OUT2; if substr(SICK_SYM1,1,3) IN ("P95") OR substr(SICK_SYM1,1,5) IN ("Z3710","Z3711","Z3712","Z3713","Z3719")
OR substr(SICK_SYM2,1,3) IN ("P95") OR substr(SICK_SYM2,1,5) IN ("Z3710","Z3711","Z3712","Z3713","Z3719"); RUN;
DATA AA.M2_OUT15; SET AA.MTH_OUT2; if  substr(SICK_SYM1,1,5) IN ("Z3730","Z3731","Z3732","Z3733","Z3739","Z3740","Z3741","Z3742","Z3743","Z3749")
OR   substr(SICK_SYM2,1,5) IN ("Z3730","Z3731","Z3732","Z3733","Z3739","Z3740","Z3741","Z3742","Z3743","Z3749"); RUN;
DATA AA.M2_OUT16; SET AA.MTH_OUT2; if  substr(SICK_SYM1,1,5) IN ("Z3760","Z3761","Z3762","Z3763","Z3769","Z3770","Z3771","Z3772","Z3773","Z3779")
OR   substr(SICK_SYM2,1,5) IN ("Z3760","Z3761","Z3762","Z3763","Z3769","Z3770","Z3771","Z3772","Z3773","Z3779"); RUN;

/*두번째 임신경험시 SGA*/
DATA AA.M2_OUT21; SET AA.MTH_OUT2; IF SUBSTR(SICK_SYM1,1,4) IN ("O365") OR SUBSTR(SICK_SYM2,1,4) IN ("O365"); RUN;

/*세번째 임신경험시 HDP*/
DATA AA.M3_OUT1; SET AA.MTH_OUT3; IF SUBSTR(SICK_SYM1,1,3) IN ("O13") OR SUBSTR(SICK_SYM2,1,3) IN ("O13"); RUN;
DATA AA.M3_OUT2; SET AA.MTH_OUT3; IF  SUBSTR(SICK_SYM1,1,4) IN ("O140","O141","O149") OR  SUBSTR(SICK_SYM2,1,4) IN  ("O140","O141","O149"); RUN;
DATA AA.M3_OUT3; SET AA.MTH_OUT3; IF  SUBSTR(SICK_SYM1,1,4) IN ("O150","O151","O152","O159") OR  SUBSTR(SICK_SYM2,1,4) IN  ("O150","O151","O152","O159"); RUN;
DATA AA.M3_OUT4; SET AA.MTH_OUT3; IF  SUBSTR(SICK_SYM1,1,4) IN ("O142") OR  SUBSTR(SICK_SYM2,1,4) IN  ("O142"); RUN;
DATA AA.M3_OUT5; SET AA.MTH_OUT3; IF SUBSTR(SICK_SYM1,1,3) IN ("O15") OR SUBSTR(SICK_SYM2,1,3) IN ("O15"); RUN;

/*세번째 임신경험시 APH, 산전출혈*/
DATA AA.M3_OUT6; SET AA.MTH_OUT3; if substr(SICK_SYM1,1,4) IN ("O200","O208","O209") OR SUBSTR(SICK_SYM2,1,4) IN ("O200","O208","O209"); RUN;
DATA AA.M3_OUT7; SET AA.MTH_OUT3; if substr(SICK_SYM1,1,4) IN ("O440","O441") OR SUBSTR(SICK_SYM2,1,4) IN ("O440","O441"); RUN;
DATA AA.M3_OUT8; SET AA.MTH_OUT3; if substr(SICK_SYM1,1,4) IN ("O450","O458","O459") OR SUBSTR(SICK_SYM2,1,4) IN ("O450","O458","O459"); RUN;
DATA AA.M3_OUT9; SET AA.MTH_OUT3; if substr(SICK_SYM1,1,4) IN ("O460","O468","O469") OR SUBSTR(SICK_SYM2,1,4) IN ("O460","O468","O469"); RUN;
DATA AA.M3_OUT10; SET AA.MTH_OUT3; if substr(SICK_SYM1,1,4) IN ("N938","N939") OR SUBSTR(SICK_SYM2,1,4) IN ("N938","N939"); RUN;

/*세번째 임신경험시 PPH, 산전출혈*/
DATA AA.M3_OUT11; SET AA.MTH_OUT3; if substr(SICK_SYM1,1,4) IN ("O720","O721") OR SUBSTR(SICK_SYM2,1,4) IN ("O720","O721"); RUN;

/*세번째 임신경험시 유산*/
DATA AA.M3_OUT12; SET AA.MTH_OUT3; if substr(SICK_SYM1,1,3) IN ("O03") OR substr(SICK_SYM1,1,4) IN ("O021")
OR substr(SICK_SYM2,1,3) IN ("O03") OR substr(SICK_SYM2,1,4) IN ("O021"); RUN;
DATA AA.M3_OUT13; SET AA.MTH_OUT3; if substr(SICK_SYM1,1,4) IN ("O364") OR SUBSTR(SICK_SYM2,1,4) IN ("O364"); RUN;

/*세번째  임신경험시 사산*/
DATA AA.M3_OUT14; SET AA.MTH_OUT3; if substr(SICK_SYM1,1,3) IN ("P95") OR substr(SICK_SYM1,1,5) IN ("Z3710","Z3711","Z3712","Z3713","Z3719")
OR substr(SICK_SYM2,1,3) IN ("P95") OR substr(SICK_SYM2,1,5) IN ("Z3710","Z3711","Z3712","Z3713","Z3719"); RUN;
DATA AA.M3_OUT15; SET AA.MTH_OUT3; if  substr(SICK_SYM1,1,5) IN ("Z3730","Z3731","Z3732","Z3733","Z3739","Z3740","Z3741","Z3742","Z3743","Z3749")
OR   substr(SICK_SYM2,1,5) IN ("Z3730","Z3731","Z3732","Z3733","Z3739","Z3740","Z3741","Z3742","Z3743","Z3749"); RUN;
DATA AA.M3_OUT16; SET AA.MTH_OUT3; if  substr(SICK_SYM1,1,5) IN ("Z3760","Z3761","Z3762","Z3763","Z3769","Z3770","Z3771","Z3772","Z3773","Z3779")
OR   substr(SICK_SYM2,1,5) IN ("Z3760","Z3761","Z3762","Z3763","Z3769","Z3770","Z3771","Z3772","Z3773","Z3779"); RUN;

/*세번째 임신경험시 SGA*/
DATA AA.M3_OUT21; SET AA.MTH_OUT3; IF SUBSTR(SICK_SYM1,1,4) IN ("O365") OR SUBSTR(SICK_SYM2,1,4) IN ("O365"); RUN;

/*아이 질환*/
DATA AA.C_OUT14; SET AA.CHD_OUT1; if substr(SICK_SYM1,1,3) IN ("P95") OR substr(SICK_SYM1,1,5) IN ("Z3710","Z3711","Z3712","Z3713","Z3719")
OR substr(SICK_SYM2,1,3) IN ("P95") OR substr(SICK_SYM2,1,5) IN ("Z3710","Z3711","Z3712","Z3713","Z3719"); RUN;
DATA AA.C_OUT15; SET AA.CHD_OUT1; if  substr(SICK_SYM1,1,5) IN ("Z3730","Z3731","Z3732","Z3733","Z3739","Z3740","Z3741","Z3742","Z3743","Z3749")
OR   substr(SICK_SYM2,1,5) IN ("Z3730","Z3731","Z3732","Z3733","Z3739","Z3740","Z3741","Z3742","Z3743","Z3749"); RUN;
DATA AA.C_OUT16; SET AA.CHD_OUT1; if  substr(SICK_SYM1,1,5) IN ("Z3760","Z3761","Z3762","Z3763","Z3769","Z3770","Z3771","Z3772","Z3773","Z3779")
OR   substr(SICK_SYM2,1,5) IN ("Z3760","Z3761","Z3762","Z3763","Z3769","Z3770","Z3771","Z3772","Z3773","Z3779"); RUN;

DATA AA.C_OUT20; SET AA.CHD_OUT1; if substr(SICK_SYM1,1,4) IN ("P050","P051") OR SUBSTR(SICK_SYM2,1,4) IN ("P050","P051"); RUN;
DATA AA.C_OUT22; SET AA.CHD_OUT1; if substr(SICK_SYM1,1,4) IN ("P220") OR SUBSTR(SICK_SYM2,1,4) IN ("P220"); RUN;
DATA AA.C_OUT23; SET AA.CHD_OUT1; if substr(SICK_SYM1,1,5) IN ("P2711","P2712","P2719") OR substr(SICK_SYM2,1,5) IN ("P2711","P2712","P2719") ; RUN;
DATA AA.C_OUT24; SET AA.CHD_OUT1; if substr(SICK_SYM1,1,4) IN ("P221") OR SUBSTR(SICK_SYM2,1,4) IN ("P221"); RUN;
DATA AA.C_OUT25; SET AA.CHD_OUT1; if substr(SICK_SYM1,1,4) IN ("P520","P521") OR substr(SICK_SYM1,1,5) IN ("P5220","P5221","P5223")
OR substr(SICK_SYM2,1,4) IN ("P520","P521") OR substr(SICK_SYM2,1,5) IN ("P5220","P5221","P5223"); RUN;
DATA AA.C_OUT26; SET AA.CHD_OUT1; if substr(SICK_SYM1,1,4) IN ("P916") OR SUBSTR(SICK_SYM2,1,4) IN ("P916"); RUN;
DATA AA.C_OUT27; SET AA.CHD_OUT1; if substr(SICK_SYM1,1,4) IN ("P360","P361","P362","P363","P364","P365","P368","P369") 
OR substr(SICK_SYM2,1,4) IN ("P360","P361","P362","P363","P364","P365","P368","P369") ; RUN;
DATA AA.C_OUT28; SET AA.CHD_OUT1; if substr(SICK_SYM1,1,3) IN ("P77") OR SUBSTR(SICK_SYM2,1,4) IN ("P77"); RUN;
DATA AA.C_OUT29; SET AA.CHD_OUT1; if substr(SICK_SYM1,1,4) IN ("P780") OR SUBSTR(SICK_SYM2,1,4) IN ("P780"); RUN;
DATA AA.C_OUT30; SET AA.CHD_OUT1; if substr(SICK_SYM1,1,5) IN ("H3510","H3511","H3512","H3513","H3514","H3519") 
OR substr(SICK_SYM2,1,5) IN ("H3510","H3511","H3512","H3513","H3514","H3519") ; RUN;
DATA AA.C_OUT31; SET AA.CHD_OUT1; if substr(SICK_SYM1,1,4) IN ("P293","Q250") OR SUBSTR(SICK_SYM2,1,4) IN ("P293","Q250"); RUN;

/**********************************************************************************************/
/**********************************************************************************************/
%MACRO HDP(DATA,OUT);
DATA &DATA.; SET AA.&DATA.;
/*요양 개시일*/
MDATE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
/*코로나 미감염 , 20주 이상 분만후 일주일 이내*/
IF COVID19=0 & (DATE2+140 <= MDATE<=DATE1+7) THEN OUT=1; ELSE IF
/*코로나 감염, 20주 이상, 분만 후 일주일 이내, 코로나 이후*/
COVID19=1 &  (DATE2+140 <= MDATE<=DATE1+7) & (MDATE>=COVID_DATE) THEN OUT=1; ELSE OUT=0;
KEEP DUPKEY OUT;
RUN;

PROC SQL; CREATE TABLE &DATA. AS SELECT DUPKEY, SUM(OUT) AS OUTSUM FROM &DATA. GROUP BY DUPKEY; QUIT;
DATA &DATA.; SET &DATA.;  IF OUTSUM>=1 THEN &OUT.=1; ELSE &OUT.=0; KEEP DUPKEY &OUT.; RUN;
%MEND;

%HDP (M1_OUT1,OUT1);%HDP (M1_OUT2,OUT2);%HDP (M1_OUT3,OUT3);%HDP (M1_OUT4,OUT4);%HDP (M1_OUT5,OUT5);
%HDP (M2_OUT1,OUT1);%HDP (M2_OUT2,OUT2);%HDP (M2_OUT3,OUT3);%HDP (M2_OUT4,OUT4);%HDP (M2_OUT5,OUT5);
%HDP (M3_OUT1,OUT1);%HDP (M3_OUT2,OUT2);%HDP (M3_OUT3,OUT3);%HDP (M3_OUT4,OUT4);%HDP (M3_OUT5,OUT5);

DATA M1_HDP; MERGE MTH_DUP1_ID M1_OUT1-M1_OUT5; BY DUPKEY; RUN;
DATA M2_HDP;MERGE MTH_DUP2_ID M2_OUT1-M2_OUT5; BY DUPKEY; RUN;
DATA M3_HDP; MERGE MTH_DUP3_ID M3_OUT1-M3_OUT5; BY DUPKEY; RUN;

DATA  M1_HDP; SET  M1_HDP;
IF OUT1="." THEN OUT1=0;       IF OUT2="." THEN OUT2=0;     IF OUT3="." THEN OUT3=0; 
IF OUT4="." THEN OUT4=0;       IF OUT5="." THEN OUT5=0;     
RUN;
DATA  M2_HDP; SET  M2_HDP;
IF OUT1="." THEN OUT1=0;       IF OUT2="." THEN OUT2=0;     IF OUT3="." THEN OUT3=0; 
IF OUT4="." THEN OUT4=0;       IF OUT5="." THEN OUT5=0;     
RUN;
DATA  M3_HDP; SET  M3_HDP;
IF OUT1="." THEN OUT1=0;       IF OUT2="." THEN OUT2=0;     IF OUT3="." THEN OUT3=0; 
IF OUT4="." THEN OUT4=0;       IF OUT5="." THEN OUT5=0;     
RUN;

DATA MTH_HDP; SET M1_HDP M2_HDP M3_HDP; RUN;

PROC FREQ DATA=MTH_HDP; TABLES OUT1-OUT5; RUN;

/*그냥 붙이면 쌍둥이 대상자는 DUPKEY가 여러개니까 산모-임신일 고유한 값만 만들어서  대상자 연계*/
PROC SORT DATA=MTH_HDP NODUPKEY OUT=Q; BY DUPKEY; RUN;

/*대상자, 자격, 코로나 감염, 백신 여부, 기저질환 연결*/
proc sql; create table qq as select * from aa.TARGET_COVID_VAC as a left join Q as b on a.dupkey=b.dupkey;quit;

proc freq data=qq; tables covid19*out1/list; run;
proc freq data=qq; tables covid19*out2/list; run;

DATA QQ; SET QQ; IF GA_WK <37 THEN PTB=1; ELSE PTB=0; RUN;
PROC FREQ DATA=QQ; TABLES DLV_YYYY*PTB/LIST; RUN;
PROC FREQ DATA=QQ; tables covid19*ptb/list; run;

/*각 임신 시점별 기저질환 산출한  부분 merge 하기*/
/*DATA AA.MTH_BD; SET G1_BD_ALL G2_BD_ALL G3_BD_ALL; RUN;*/

%MACRO APH(DATA,OUT);
DATA &DATA.; SET AA.&DATA.;
/*요양 개시일*/
MDATE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
/*코로나 미감염 , 분만일 하루전*/
IF COVID19=0 & (DATE1-2<MDATE<=DATE1-1) THEN OUT=1; ELSE IF
/*코로나 감염, 분만일 하루전*/
COVID19=1 & (DATE1-2<MDATE<=DATE1-1) & (MDATE>=COVID_DATE) THEN OUT=1; ELSE OUT=0;
KEEP DUPKEY OUT;
RUN;
PROC SQL; CREATE TABLE &DATA. AS SELECT DUPKEY, SUM(OUT) AS OUTSUM FROM &DATA. GROUP BY DUPKEY; QUIT;
DATA &DATA.; SET &DATA.;  IF OUTSUM>=1 THEN &OUT.=1; ELSE &OUT.=0; KEEP DUPKEY &OUT.; RUN;
%MEND;

%MACRO APH2(DATA,OUT);
DATA &DATA.; SET AA.&DATA.;
/*요양 개시일*/
MDATE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
/*코로나 미감염 , 분만일~1일전*/
IF COVID19=0 & (DATE1-1<=MDATE<=DATE1) THEN OUT=1; ELSE IF
/*코로나 감염, 분만일~1일전*/
COVID19=1 & (DATE1-1<=MDATE<=DATE1) & (MDATE>=COVID_DATE) THEN OUT=1; ELSE OUT=0;
KEEP DUPKEY OUT;
RUN;
PROC SQL; CREATE TABLE &DATA. AS SELECT DUPKEY, SUM(OUT) AS OUTSUM FROM &DATA. GROUP BY DUPKEY; QUIT;
DATA &DATA.; SET &DATA.;  IF OUTSUM>=1 THEN &OUT.=1; ELSE &OUT.=0; KEEP DUPKEY &OUT.; RUN;
%MEND;

%APH (M1_OUT6,OUT6);%APH (M1_OUT7,OUT7);%APH2 (M1_OUT8,OUT8);%APH (M1_OUT9,OUT9);%APH (M1_OUT10,OUT10);
%APH (M2_OUT6,OUT6);%APH (M2_OUT7,OUT7);%APH2 (M2_OUT8,OUT8);%APH (M2_OUT9,OUT9);%APH (M2_OUT10,OUT10);
%APH (M3_OUT6,OUT6);%APH (M3_OUT7,OUT7);%APH2 (M3_OUT8,OUT8);%APH (M3_OUT9,OUT9);%APH (M3_OUT10,OUT10);

DATA M1_APH; MERGE MTH_DUP1_ID M1_OUT6-M1_OUT10; BY DUPKEY; RUN;
DATA M2_APH;MERGE MTH_DUP2_ID M2_OUT6-M2_OUT10; BY DUPKEY; RUN;
DATA M3_APH; MERGE MTH_DUP3_ID M3_OUT6-M3_OUT10; BY DUPKEY; RUN;

DATA  M1_APH; SET  M1_APH;
IF OUT6="." THEN OUT6=0;       IF OUT7="." THEN OUT7=0;     IF OUT8="." THEN OUT8=0; 
IF OUT9="." THEN OUT9=0;       IF OUT10="." THEN OUT10=0;     
RUN;
DATA  M2_APH; SET  M2_APH;
IF OUT6="." THEN OUT6=0;       IF OUT7="." THEN OUT7=0;     IF OUT8="." THEN OUT8=0; 
IF OUT9="." THEN OUT9=0;       IF OUT10="." THEN OUT10=0;     
RUN;
DATA  M3_APH; SET  M3_APH;
IF OUT6="." THEN OUT6=0;       IF OUT7="." THEN OUT7=0;     IF OUT8="." THEN OUT8=0; 
IF OUT9="." THEN OUT9=0;       IF OUT10="." THEN OUT10=0;     
RUN;

DATA MTH_APH; SET M1_APH M2_APH M3_APH; RUN;

PROC FREQ DATA=MTH_APH; TABLES OUT6-OUT10; RUN;


%MACRO DLV_SEVEN(DATA,OUT);
DATA &DATA.; SET AA.&DATA.;
/*요양 개시일*/
MDATE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
/*코로나 미감염 , 분만 후 일주일 이내*/
IF COVID19=0 & (DATE1<=MDATE<=DATE1+7) THEN OUT=1; ELSE IF
/*코로나 감염, 분만 후 일주일 이내, 코로나 이후*/
COVID19=1 & (DATE1<=MDATE<=DATE1+7) & (MDATE>=COVID_DATE) THEN OUT=1; ELSE OUT=0;
KEEP DUPKEY OUT;
RUN;
PROC SQL; CREATE TABLE &DATA. AS SELECT DUPKEY, SUM(OUT) AS OUTSUM FROM &DATA. GROUP BY DUPKEY; QUIT;
DATA &DATA.; SET &DATA.;  IF OUTSUM>=1 THEN &OUT.=1; ELSE &OUT.=0; KEEP DUPKEY &OUT.; RUN;
%MEND;

/*아이는 아이 개인식별번호 기준으로*/
%MACRO DLV_SEVEN2(DATA,OUT);
DATA &DATA.; SET AA.&DATA.;
/*요양 개시일*/
MDATE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
/*코로나 미감염 , 분만 후 일주일 이내*/
IF COVID19=0 & (DATE1<=MDATE<=DATE1+7) THEN OUT=1; ELSE IF
/*코로나 감염, 분만 후 일주일 이내, 코로나 이후*/
COVID19=1 & (DATE1<=MDATE<=DATE1+7) & (MDATE>=COVID_DATE) THEN OUT=1; ELSE OUT=0;
KEEP CHILD_ID OUT;
RUN;
PROC SQL; CREATE TABLE &DATA. AS SELECT CHILD_ID, SUM(OUT) AS OUTSUM FROM &DATA. GROUP BY CHILD_ID; QUIT;
DATA &DATA.; SET &DATA.;  IF OUTSUM>=1 THEN &OUT.=1; ELSE &OUT.=0; KEEP CHILD_ID &OUT.; RUN;
%MEND;

%DLV_SEVEN (M1_OUT11,OUT11);%DLV_SEVEN (M2_OUT11,OUT11);%DLV_SEVEN (M3_OUT11,OUT11);
%DLV_SEVEN (M1_OUT21,OUT21);%DLV_SEVEN (M2_OUT21,OUT21);%DLV_SEVEN (M3_OUT21,OUT21);

DATA M1_PPH; MERGE MTH_DUP1_ID M1_OUT11; BY DUPKEY; RUN;
DATA M2_PPH; MERGE MTH_DUP2_ID M2_OUT11; BY DUPKEY; RUN;
DATA M3_PPH; MERGE MTH_DUP3_ID M3_OUT11; BY DUPKEY; RUN;

DATA  M1_PPH; SET  M1_PPH; IF OUT11="." THEN OUT11=0; RUN;
DATA  M2_PPH; SET  M2_PPH; IF OUT11="." THEN OUT11=0; RUN;
DATA  M3_PPH; SET  M3_PPH; IF OUT11="." THEN OUT11=0; RUN;

DATA MTH_PPH; SET M1_PPH M2_PPH M3_PPH; RUN;

DATA M1_SGA; MERGE MTH_DUP1_ID M1_OUT21; BY DUPKEY; RUN;
DATA M2_SGA; MERGE MTH_DUP2_ID M2_OUT21; BY DUPKEY; RUN;
DATA M3_SGA; MERGE MTH_DUP3_ID M3_OUT21; BY DUPKEY; RUN;

DATA  M1_SGA; SET  M1_SGA; IF OUT21="." THEN OUT21=0; RUN;
DATA  M2_SGA; SET  M2_SGA; IF OUT21="." THEN OUT21=0; RUN;
DATA  M3_SGA; SET  M3_SGA; IF OUT21="." THEN OUT21=0; RUN;

DATA MTH_SGA; SET M1_SGA M2_SGA M3_SGA; RUN;

PROC FREQ DATA=MTH_SGA; TABLES OUT21; RUN;

%DLV_SEVEN2 (C_OUT20,OUT20); %DLV_SEVEN2 (C_OUT22,OUT22); %DLV_SEVEN2 (C_OUT23,OUT23); 
%DLV_SEVEN2 (C_OUT24,OUT24); %DLV_SEVEN2 (C_OUT25,OUT25); %DLV_SEVEN2 (C_OUT26,OUT26);
%DLV_SEVEN2 (C_OUT27,OUT27); %DLV_SEVEN2 (C_OUT28,OUT28); %DLV_SEVEN2 (C_OUT29,OUT29); 
%DLV_SEVEN2 (C_OUT30,OUT30); %DLV_SEVEN2 (C_OUT31,OUT31);

DATA AA.CHD_ID; SET AA.TARGET_COVID_VAC;  KEEP CHILD_ID; RUN;
PROC SORT DATA=AA.CHD_ID; BY CHILD_ID; RUN;

DATA CHD_OUTCOME; MERGE AA.CHD_ID C_OUT20 C_OUT22-C_OUT31 ;BY CHILD_ID; RUN;

DATA  CHD_OUTCOME; SET  CHD_OUTCOME;
IF OUT20="." THEN OUT20=0; 
IF OUT22="." THEN OUT22=0;   IF OUT23="." THEN OUT23=0;  IF OUT24="." THEN OUT24=0; 
IF OUT25="." THEN OUT25=0;   IF OUT26="." THEN OUT26=0;  IF OUT27="." THEN OUT27=0; 
IF OUT28="." THEN OUT28=0;   IF OUT29="." THEN OUT29=0;  IF OUT30="." THEN OUT30=0;  IF OUT31="." THEN OUT31=0; 
RUN;

PROC FREQ DATA=CHD_OUTCOME; TABLES OUT20 OUT22-OUT31; RUN;


%MACRO FDIU(DATA,OUT);
DATA &DATA.; SET AA.&DATA.;
/*요양 개시일*/
MDATE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
/*코로나 미감염 , 임신기간*/
IF COVID19=0 & (DATE2<=MDATE<=DATE1) THEN OUT=1; ELSE IF
/*코로나 감염, 코로나 이후*/
COVID19=1 & (COVID_DATE<=MDATE<=DATE1)  THEN OUT=1; ELSE OUT=0;
KEEP DUPKEY OUT;
RUN;
PROC SQL; CREATE TABLE &DATA. AS SELECT DUPKEY, SUM(OUT) AS OUTSUM FROM &DATA. GROUP BY DUPKEY; QUIT;
DATA &DATA.; SET &DATA.;  IF OUTSUM>=1 THEN &OUT.=1; ELSE &OUT.=0; KEEP DUPKEY &OUT.; RUN;
%MEND;

%FDIU (M1_OUT12,OUT12);%FDIU (M1_OUT13,OUT13);%FDIU (M1_OUT14,OUT14);%FDIU (M1_OUT15,OUT15);%FDIU (M1_OUT16,OUT16);
%FDIU (M2_OUT12,OUT12);%FDIU (M2_OUT13,OUT13);%FDIU (M2_OUT14,OUT14);%FDIU (M2_OUT15,OUT15);%FDIU (M2_OUT16,OUT16);
%FDIU (M3_OUT12,OUT12);%FDIU (M3_OUT13,OUT13);%FDIU (M3_OUT14,OUT14);%FDIU (M3_OUT15,OUT15);%FDIU (M3_OUT16,OUT16);

DATA M1_FDIU; MERGE MTH_DUP1_ID M1_OUT12-M1_OUT16; BY DUPKEY; RUN;
DATA M2_FDIU; MERGE MTH_DUP2_ID M2_OUT12-M2_OUT16; BY DUPKEY; RUN;
DATA M3_FDIU; MERGE MTH_DUP3_ID M3_OUT12-M3_OUT16; BY DUPKEY; RUN;

DATA  M1_FDIU; SET  M1_FDIU;
IF OUT12="." THEN OUT12=0;       IF OUT13="." THEN OUT13=0;     IF OUT14="." THEN OUT14=0; 
IF OUT15="." THEN OUT15=0;       IF OUT16="." THEN OUT16=0;     
RUN;
DATA  M2_FDIU; SET  M2_FDIU;
IF OUT12="." THEN OUT12=0;       IF OUT13="." THEN OUT13=0;     IF OUT14="." THEN OUT14=0; 
IF OUT15="." THEN OUT15=0;       IF OUT16="." THEN OUT16=0;     
RUN;
DATA  M3_FDIU; SET  M3_FDIU;
IF OUT12="." THEN OUT12=0;       IF OUT13="." THEN OUT13=0;     IF OUT14="." THEN OUT14=0; 
IF OUT15="." THEN OUT15=0;       IF OUT16="." THEN OUT16=0;        
RUN;

DATA MTH_FDIU; SET M1_FDIU M2_FDIU M3_FDIU; RUN;

PROC FREQ DATA=MTH_FDIU; TABLES OUT12-OUT16; RUN;

DATA AA.MTH_HDP; SET MTH_HDP; RUN;
DATA AA.MTH_APH; SET MTH_APH; RUN;
DATA AA.MTH_PPH; SET MTH_PPH; RUN;
DATA AA.MTH_FDIU; SET MTH_FDIU; RUN;
DATA AA.MTH_SGA; SET MTH_SGA; RUN;

PROC SORT DATA=AA.MTH_HDP NODUPKEY OUT=A1; BY DUPKEY; RUN;
PROC SORT DATA=AA.MTH_APH NODUPKEY OUT=A2; BY DUPKEY; RUN;
PROC SORT DATA=AA.MTH_PPH NODUPKEY OUT=A3; BY DUPKEY; RUN;
PROC SORT DATA=AA.MTH_FDIU NODUPKEY OUT=A4; BY DUPKEY; RUN;
PROC SORT DATA=AA.MTH_SGA NODUPKEY OUT=A5; BY DUPKEY; RUN;

DATA A2; SET A2; DROP OUT11; RUN;
DATA AA.CHD_OUTCOME; SET CHD_OUTCOME; RUN;

DATA MTH_OUTCOME; MERGE A1-A5; BY DUPKEY; RUN;
PROC SQL; CREATE TABLE AA.TARGET_COVID_VAC_BD_OUT AS SELECT * FROM AA.TARGET_COVID_VAC_BD AS A LEFT JOIN 
MTH_OUTCOME AS B ON A.DUPKEY = B.DUPKEY; QUIT;

PROC SQL; CREATE TABLE AA.TARGET_COVID_VAC_BD_OUT AS SELECT * FROM AA.TARGET_COVID_VAC_BD_OUT AS A LEFT JOIN 
AA.CHD_OUTCOME AS B ON A.CHILD_ID = B.CHILD_ID; QUIT;

DATA AA.TARGET_COVID_VAC_BD_OUT; SET AA.TARGET_COVID_VAC_BD_OUT;
IF GA_WK<37 THEN Out17=1; ELSE Out17=0; RUN;

DATA SAPTMP.TARGET_COVID_VAC_BD_OUT; SET AA.TARGET_COVID_VAC_BD_OUT; RUN;

/**********************************************************************************************/
/**********************************************************************************************/
/*2021년 영유아 검진 안해서.. 상병코드로 먼저 찾기*/
PROC SQL; 
&CONNECT.;
CREATE TABLE CHD_LBW AS SELECT * FROM CONNECTION TO X1(
    SELECT *
    FROM NHISBASE.HBMV_TBGJME20  /**T20 DB**/

	/*엄마 질환*/
	WHERE  INDI_DSCM_NO IN (SELECT CHILD_ID FROM TARGET_COVID_VAC)
	AND HIRA_EXM_YYYYMM<='202204'
    AND PAY_YN='1'
	AND FORM_CD IN ('02','03','08')
	/*2020.1 ~2022.3*/
	AND MDCARE_STRT_YYYYMM IN 
('202001','202002','202003','202004','202005','202006','202007','202008','202009','202010','202011','202012',
'202101','202102','202103','202104','202105','202106','202107','202108','202109','202110','202111','202112',
'202201','202202','202203')
	AND INDI_DSCM_NO<>0 
	AND INDI_DSCM_NO IS NOT NULL
	AND INDI_DSCM_NO<90000000
	/*주상병 */
	 AND           
	               /*LBW*/
                   (LEFT(SICK_SYM1,5) BETWEEN 'P070' AND 'P071'	
                  OR LEFT(SICK_SYM2,5) BETWEEN 'P070' AND 'P071'
                  OR LEFT(SICK_SYM3,5) BETWEEN 'P070' AND 'P071'	
                  OR LEFT(SICK_SYM4,5) BETWEEN 'P070' AND 'P071'		
                  OR LEFT(SICK_SYM5,5) BETWEEN 'P070' AND 'P071'	
			                                      			
)
);DISCONNECT FROM X1;
QUIT; 


/*임신 출산 DB 이용해서 parity찾기*/
PROC SQL; 
&CONNECT.;
CREATE TABLE MC_ALL AS SELECT * FROM CONNECTION TO X1(
SELECT * FROM NHISBDA.HHDV_MOTHER_CHILD_LINK   /**임신주수 & 임신부 전수**/
);DISCONNECT FROM X1; QUIT; 

DATA AA.MC_ALL; SET MC_ALL;
KEY=COMPRESS(LEFT(MOTHER_ID))||("-")||COMPRESS(LEFT(DLV_DT));
KEEP CHILD_ID MOTHER_ID DLV_DT KEY; RUN;

PROC SORT DATA= AA.MC_ALL NODUPKEY OUT=AA.MC_UNIQ ; BY KEY; RUN;
PROC SORT DATA= AA.MC_UNIQ ; BY MOTHER_ID DLV_DT; RUN;

DATA AA.PARITY; SET AA.MC_UNIQ; 
BY MOTHER_ID;
RETAIN PARITY 0;
IF FIRST.MOTHER_ID THEN PARITY=1; 
ELSE PARITY=PARITY+1;
KEEP CHILD_ID PARITY KEY;
RUN;

proc sql; create table AA.PARITY2 AS SELECT * FROM AA.MC_ALL AS A LEFT JOIN AA.PARITY AS B 
ON A.KEY=B.KEY; QUIT;

PROC FREQ DATA=AA.PARITY2; TABLES PARITY; RUN;
PROC SQL; CREATE TABLE AA.PARITY3 AS SELECT CHILD_ID, PARITY FROM AA.PARITY2 WHERE CHILD_ID IN 
(SELECT CHILD_ID FROM AA.TARGET_COVID_VAC_BD_OUT); QUIT;


/*parity 연계*/
PROC SQL; CREATE TABLE AA.TARGET_COVID_VAC_BD_OUT AS SELECT * FROM 
AA.TARGET_COVID_VAC_BD_OUT AS A LEFT JOIN AA.PARITY3 AS B ON A.CHILD_ID =B.CHILD_ID; QUIT;
PROC FREQ DATA=AA.TARGET_COVID_VAC_BD_OUT; TABLES PARITY; RUN;

DATA SAPTMP.TARGET_COVID_VAC_BD_OUT; SET AA.TARGET_COVID_VAC_BD_OUT; RUN;

/**********************************************************************************************/
/*검진 변수 살펴보기*/
PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT TOP 10*
    FROM NHISBDA.HMDT_G1E_RST_2019
);DISCONNECT FROM X1; QUIT; 
/**********************************************************************************************/
/*검진 대상자수, 검사날짜 검토*/
PROC SQL; 
&CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
    SELECT count(INDI_DSCM_NO) as cnt, count(distInct indi_dscm_no) as unique_cnt ,min(HC_DT) AS MIN_DT,
	MAX(HC_DT) AS MAX_DT 
    FROM NHISBDA.HMDT_G1E_RST_2020
);DISCONNECT FROM X1; QUIT; 
/**********************************************************************************************/
/*2018년 검진*/
PROC SQL; 
&CONNECT.;
CREATE TABLE GJ_MTH_2018 AS SELECT * FROM CONNECTION TO X1(
    SELECT * FROM NHISBDA.HMDT_G1E_RST_2018  
	WHERE  INDI_DSCM_NO IN (SELECT MOTHER_ID FROM TARGET_COVID_VAC)
);DISCONNECT FROM X1; QUIT; 
/*2019년 검진*/
PROC SQL; 
&CONNECT.;
CREATE TABLE GJ_MTH_2019 AS SELECT * FROM CONNECTION TO X1(
    SELECT * FROM NHISBDA.HMDT_G1E_RST_2019  
	WHERE  INDI_DSCM_NO IN (SELECT MOTHER_ID FROM TARGET_COVID_VAC)
);DISCONNECT FROM X1; QUIT; 
/*2020년 검진*/
PROC SQL; 
&CONNECT.;
CREATE TABLE GJ_MTH_2020 AS SELECT * FROM CONNECTION TO X1(
    SELECT * FROM NHISBDA.HMDT_G1E_RST_2020 
	WHERE  INDI_DSCM_NO IN (SELECT MOTHER_ID FROM TARGET_COVID_VAC)
);DISCONNECT FROM X1; QUIT; 

/*변수정리*/
DATA GJ_MTH_2018; 
Retain gj_year;
SET GJ_MTH_2018;
RENAME HME_DT=GJ_DT G1E_HGHT=MTH_Height G1E_WGHT=MTH_weight G1E_WSTC=MTH_waist G1E_BMI=MTH_BMI 
G1E_BP_SYS=MTH_SBP G1E_BP_DIA=MTH_DBP G1E_HGB=MTH_HGB G1E_FBS=MTH_FBS G1E_CRTN=MTH_Creatinine G1E_GFR=MTH_GFR;
GJ_year=2018;
gj_date=mdy(substr(HME_DT,5,2),substr(HME_DT,7,2),substr(HME_DT,1,4));
KEEP INDI_DSCM_NO HME_DT G1E_HGHT G1E_WGHT G1E_WSTC G1E_BMI G1E_BP_SYS G1E_BP_DIA G1E_HGB G1E_FBS G1E_CRTN G1E_GFR gj_year gj_date;
RUN;

DATA GJ_MTH_2019; 
Retain gj_year;
SET GJ_MTH_2019;
RENAME HC_DT=GJ_DT G1E_HGHT=MTH_Height G1E_WGHT=MTH_weight G1E_WSTC=MTH_waist G1E_BMI=MTH_BMI 
G1E_BP_SYS=MTH_SBP G1E_BP_DIA=MTH_DBP G1E_HGB=MTH_HGB G1E_FBS=MTH_FBS G1E_CRTN=MTH_Creatinine G1E_GFR=MTH_GFR;
GJ_year=2019;
gj_date=mdy(substr(HC_DT,5,2),substr(HC_DT,7,2),substr(HC_DT,1,4));
KEEP INDI_DSCM_NO HC_DT G1E_HGHT G1E_WGHT G1E_WSTC G1E_BMI G1E_BP_SYS G1E_BP_DIA G1E_HGB G1E_FBS G1E_CRTN G1E_GFR gj_year gj_date;
RUN;

DATA GJ_MTH_2020; 
Retain gj_year;
SET GJ_MTH_2020;
RENAME HC_DT=GJ_DT G1E_HGHT=MTH_Height G1E_WGHT=MTH_weight G1E_WSTC=MTH_waist G1E_BMI=MTH_BMI 
G1E_BP_SYS=MTH_SBP G1E_BP_DIA=MTH_DBP G1E_HGB=MTH_HGB G1E_FBS=MTH_FBS G1E_CRTN=MTH_Creatinine G1E_GFR=MTH_GFR;
GJ_year=2020;
gj_date=mdy(substr(HC_DT,5,2),substr(HC_DT,7,2),substr(HC_DT,1,4));
KEEP INDI_DSCM_NO HC_DT G1E_HGHT G1E_WGHT G1E_WSTC G1E_BMI G1E_BP_SYS G1E_BP_DIA G1E_HGB G1E_FBS G1E_CRTN G1E_GFR gj_year gj_date;
RUN;

proc sql; create table z1 as select indi_dscm_no, count(indi_dscm_no) as cnt from gj_mth_2018 group by indi_dscm_no; quit;
proc sql; create table z2 as select indi_dscm_no, count(indi_dscm_no) as cnt from gj_mth_2019 group by indi_dscm_no; quit;
proc sql; create table z3 as select indi_dscm_no, count(indi_dscm_no) as cnt from gj_mth_2020 group by indi_dscm_no; quit;

proc freq data=z1; tables cnt; run;
proc freq data=z2; tables cnt; run;
proc freq data=z3; tables cnt; run;

/*2018년 검진자료 대상자 중복 제거 ,71건*/
proc sort data=GJ_MTH_2018 NODUPKEY ; BY INDI_DSCM_NO; RUN;

DATA GJ_MTH; SET GJ_MTH_2018-GJ_MTH_2020;  RUN;
DATA AA.GJ_MTH; SET GJ_MTH; RUN;

PROC SORT DATA=GJ_MTH; BY INDI_DSCM_NO gj_date; RUN;
PROC FREQ DATA=GJ_MTH; TABLES gj_year; RUN;

data d1; set aa.mth_dup1; keep mother_id preg_ddate date2 DUPKEY; run;
data d2; set aa.mth_dup2; keep mother_id preg_ddate date2 DUPKEY; run;
data d3; set aa.mth_dup3; keep mother_id preg_ddate date2 DUPKEY; run;

/*대상자 임신 시기별 merge 후, 검진일<임신일이면 자료 유지*/
/*그 중 날짜 젤 최신인 대상자 남기기*/
%macro gj_obj(data,out);
proc sql; create table &out. as select * from aa.gj_mth as a left join &data. as b on a.INDI_DSCM_NO =b.mother_id; quit;

data &out.; set &out.;
if gj_date-DATE2<=0 then keep=1 ; else keep=0; 
if mother_id^=".";
if keep=1; run;

proc sort data=&out.; by indi_dscm_no descending gj_date; run;
proc sort data=&out. nodupkey ; by indi_dscm_no; run;
%mend;

%gj_obj(d1,gj_m1);
%gj_obj(d2,gj_m2);
%gj_obj(d3,gj_m3);

data gj_m; set gj_m1-gj_m3; 
drop INDI_DSCM_NO MOTHER_ID PREG_DDATE DATE2 keep; 
run;

proc sql; create table final_data as select * from aa.target_covid_vac_bd_out as a left join gj_m as b on a.dupkey=b.dupkey; quit;

data AA.final_data; set final_data; run;
DATA SAPTMP.FINAL_DATA; SET FINAL_DATA; RUN;

proc contents data=aa.final_data; run;
