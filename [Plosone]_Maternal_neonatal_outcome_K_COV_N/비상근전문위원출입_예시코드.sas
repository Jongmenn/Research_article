
/*
readbuff: LIBNAME 옵션과 데이터셋 옵션은 기본 데이터베이스가 확장 페치를
             지원할 때 기본 블록 크기(64K)를 사용하여 데이터를 읽습니다. 기본 행 수는 64K를 행 너비로 나누어 결정. 
             SAS 9.4 및 이전 릴리스의 SAS Viya에서 이 옵션은 기본적으로 단일 행 페치를 수행 
insertbuff: oracle 연결시 입력하는 값 
*/

/*(1) SAS HANA DB 접속 (libname 설정) NHISBASE*/
libname SAPEDW saphana readbuff=32767 server=kalmia1 port=30015 user=아이디 SCHEMA='NHISBASE' password=비밀번호;

/* (2) SAS HANA DB 접속 (libname 설정) NHISBDA */
libname SAPEDW_1 saphana readbuff=32767 server kalmia1 port=30015 user=아이디 SCHEMA='NHISBDA' password=비밀번호;

/*SAPTMP*/
libname SAPTMP saphana TABLE_TYPE=COLUMN readbuff=32767 insertbuff=32767 server=kalmia1 port=30015 user=아이디 password=비밀번호
connection=global PRESERVE_TAB_NAMES=YES;


/******************************************************************************************************************/
/*NHIS-COVID-19 연구 예시 코드 미리작성 (연구실) */
/*예전 메뉴얼 참조하여 SAS내 SQL문으로 작성해보기 */
/*표본 코호트 1.0 자료 이용*/
/*추후 코드 수정수정수정해서 이용 */
/******************************************************************************************************************/
LIBNAME GJ     'D:\EUMC\데이터관리\표본코호트_DB(NHIS_NSC)\GJ';
LIBNAME JK     'D:\EUMC\데이터관리\표본코호트_DB(NHIS_NSC)\JK';
LIBNAME T120 'D:\EUMC\데이터관리\표본코호트_DB(NHIS_NSC)\T120';
LIBNAME T130 'D:\EUMC\데이터관리\표본코호트_DB(NHIS_NSC)\T130';
LIBNAME T140 'D:\EUMC\데이터관리\표본코호트_DB(NHIS_NSC)\T140';
LIBNAME T160 'D:\EUMC\데이터관리\표본코호트_DB(NHIS_NSC)\T160';

/*예시 코드 임시로 저장할 임시 디렉토리*/
libname A 'D:\EUMC\코로나19공단연구\data\example'; 
/******************************************************************************************************************/
/*상병코드 및 수가코드 추출시 주의사항 코드변경*/
/*TRIM(MAIN_SICK) 공백제거해서 이용 (불필요, 공단에서 작업 완료)*/
/*LEFT(MAIN_SICK) 왼쪽 정렬해서 이용 (불필요, 공단에서 작업완료)*/

PROC SQL;
CREATE TABLE ZZ AS SELECT PERSON_ID, KEY_SEQ, YKIHO_ID, RECU_FR_DT, FORM_CD, MAIN_SICK, SUB_SICK, RECN, VSCN, FST_IN_PAT_DT FROM
T120.T120 WHERE SUBSTR(MAIN_SICK,1,4)="A101"; QUIT;

/*조인(JOIN) 구문의 경우, ON 절에는 A 테이블과 B 테이블의 조인키 변수와 B테이블 필터조건을 기술하고 WHERE 절에는 A테이블 필터 조건을 기술함 */
/*예시니까 1개년도 자료만 일단 보기 2002*/
/*조인할 변수들 다 붙일거면 * 이용하기*/
/*데이터 속성에 따라 다를 수 있음*/

PROC SQL;
CREATE TABLE A.TEST01 AS SELECT A.PERSON_ID, A.KEY_SEQ, A.FORM_CD, A.MAIN_SICK, A.SUB_SICK, A.RECN, A.VSCN, A.FST_IN_PAT_DT,
B.STND_Y, B.SEX, B.AGE_GROUP, B.DTH_YM, B.DTH_CODE1, B.SIDO, B.SGG FROM T120.Nhid_gy20_t1_2002 AS A INNER JOIN JK.Nhid_jk_2002 AS B
ON A.PERSON_ID=B.PERSON_ID 
AND B.SIDO="11" /*ON 절에 뒤에오는 B 테이블에 대한 조건 필터*/
WHERE A.FORM_CD="02" AND SUBSTR(MAIN_SICK,1,3)="I20";  /*A테이블에 대한 조건 필터, 여러개인 경우 AND로 연결*/
QUIT;


/******************************************************************************************************************/
/*※ 데이터 저장이 필요한 경우 샤프론 서버를 이용함-> 
   즉, 최종 자료 또는 어느정도 데이터 사이즈를 줄인 자료 형태면 샤프론에 내려서 이용 */

/*간접접속(①) 시 라이브러리 할당 쿼리 (예시) */
%LET 사번=/*본인사번*/;
LIBNAME SAPEDW saphana readbuff=32767 server=pepper1 port=30015 user=Nhis_&사번 SCHEMA='NHISBASE' password=!Nhis_&사번.1;
LIBNAME SAPTMP saphana  TABLE_TYPE=COLUMN readbuff=32767 insertbuff=32767 server=pepper1 port=30015 user=Nhis_&사번 password=!Nhis_&사번.1 connection=global PRESERVE_TAB_NAMES=YES;
LIBNAME SAPBDA_R saphana readbuff=32767 server=pepper1 port=30015 user=NHIS_&사번 SCHEMA='NHISBDA' password=!Nhis_&사번.1; 
LIBNAME AA ‘/*샤프론 서버 경로*/’;

/******************************************************************************************************************/
/******************************************************************************************************************/
/*□ HANA DB 개인영역(NHIS_사번)에 자료 생성하는 예시*/
%MACRO BRT_Y; %DO Y=2002 %TO 2014; 
%MACRO BRT_M; %DO YM=&Y.01 %TO &Y.12;

PROC SQL;connect to saphana as x1(server=pepper1 port=30015 user=개인계정 password=비밀번호 readbuff=32767 insertbuff=32767);
execute(
CREATE TABLE TMP_TG_MAIN_&YM. AS ( 
        SELECT 추출컬럼
          FROM NHISBASE.추출테이블명  /* 실제 hana db schema 사용 ( 기본 :NHISBASE )*/
         WHERE MDCARE_STRT_YYYYMM = %nrbquote('&YM.')  
        ) by x1;
disconnect from x1;
QUIT;  
%END; %MEND BRT_M; %BRT_M; %END; %MEND BRT_Y; %BRT_Y; 

/*□ 샤프론 서버로 자료 생성하는 예시*/
%MACRO BRT_Y; %DO Y=2002 %TO 2014; %MACRO BRT_M; %DO YM=&Y.01 %TO &Y.12;
PROC SQL;
  connect to saphana as x1(server=pepper1 port=30015 user=개인계정 password=비밀번호 readbuff=32767 insertbuff=32767);
create table LDH.TMP_TG_MAIN_&YM. AS
SELECT * FROM CONNECTION TO X1(
        SELECT 추출컬럼
          FROM NHISBASE.테이블명 /* 실제 hana db schema 사용 ( 기본 :NHISBASE )*/
         WHERE MDCARE_STRT_YYYYMM = %nrbquote('&YM.')
           AND 필터조건 );
disconnect from x1;
QUIT;  
%END; %MEND BRT_M; %BRT_M; %END; %MEND BRT_Y; %BRT_Y;
/******************************************************************************************************************/
/******************************************************************************************************************/
/*%nrbquote() -> %, &도 글자로 인식시킴*/
/*%bquote() 모든 것을 글자로 인식*/

/*연월별 출력할 경우 */
/*□ HANA DB 개인영역(NHIS_사번)에 자료 생성하는 예시 적용*/
%MACRO BRT_Y; %DO Y=2002 %TO 2002; 
%MACRO BRT_M; %DO YM=&Y.01 %TO &Y.12;

PROC SQL;
CREATE TABLE A.TEST_&YM. AS 
        SELECT PERSON_ID, KEY_SEQ, YKIHO_ID, RECU_FR_DT, FORM_CD, MAIN_SICK, SUB_SICK, RECN, VSCN, FST_IN_PAT_DT
        FROM T120.Nhid_gy20_t1_2002
        WHERE %nrbquote(SUBSTR(RECU_FR_DT,1,6)) = %nrbquote('&YM.')  AND
		          SUBSTR(MAIN_SICK,1,3)="I20";
QUIT;  
%END; %MEND BRT_M; 
%BRT_M; 
%END; %MEND BRT_Y; 
%BRT_Y; 


/******************************************************************************************************************/
/*연도별 출력할 경우 */
%MACRO BRT_Y; %DO Y=2004 %TO 2013; 
PROC SQL;
CREATE TABLE A.TEST_&Y. AS 
        SELECT PERSON_ID, KEY_SEQ, YKIHO_ID, RECU_FR_DT, FORM_CD, MAIN_SICK, SUB_SICK, RECN, VSCN, FST_IN_PAT_DT
        FROM T120.T120
		/*연구실에선 문자열 인식이 좀 잘안되서 이렇게입력, 비상근 공단 DB는 문자열 매크로 붙여서 쓰기*/
        WHERE SUBSTR(RECU_FR_DT,1,4) = "&Y." AND
		          SUBSTR(MAIN_SICK,1,3)="I20";
QUIT;  
%END; %MEND BRT_Y; 

%BRT_Y; 
/******************************************************************************************************************/
/******************************************************************************************************************/
/*자격 연도별 데이터 가져올 경우*/
/*BFC_INYR 데이터에서 한 사람당 한 개의 자격만 추출하고자하면 우선순위(PRTY)가 ‘1’인 값을 조건으로 주면 됨 (이건 변수가 따로 있는듯)*/
/*오라클에선 빈 문자열("")은 NULL로 인식하기 때문에 컬럼값이 빈 문자열이면 NULL과 동일한 것처럼 조건 처리해야됨*/
PROC SQL; CREATE TABLE A.TEST_JK AS SELECT * FROM JK.JK 
WHERE STND_Y="2013" 
AND PERSON_ID <>0               /*부정을 의미(<>, != , ^=)*/
AND PERSON_ID IS NOT NULL    /*NULL값이면 제외*/
AND PERSON_ID <900000000  /*이 값은 유효하지 않은 수치 대략 준듯 */
/*AND PRTY ="1" */                /*우선순위 1인 사람으로 한명만 뽑고 싶을 때 */
; QUIT;



/******************************************************************************************************************/
/******************************************************************************************************************/
/*사망 테이블은 1개 테이블로 구성되어 있으며, 연도 구분 없이 과거부터 현재까지 전체 사망자의 사망일자 정보가 있음 */
/*필수조건 및 주의사항  이중에 택일 해서 연구목적에 맞게 사용권장
(1) 행망 사망 일자(DTH_HM_DT)
(2) 자격 사망 일자(DTH_BFC_DT)
(3) 최종 진료일 이후에 자격사망일로 보정한 사망 보정 변수(DTH_ASSMD_DT)  */

proc sql;
create table CALL SP_TABLE_DESC("a.test"); quit;

proc sql;
create table CALL SP_TABLE_DESC("NHISBASE","HBAT_TBJGBA20");

/*테이블 속성 보여주기 */
PROC CONTENTS DATA=A.TEST; RUN;

/******************************************************************************************************************/
/*슬비샘 예시코드 기술*/
%LET CONNECT=CONNET TO SAPHANA AS X1(SERVER=KALMIA1 PORT=30015 USER=NHIS_220051 PASSWORD=!Nhis_2200511 READBUFF=32767 
INSERTBUFF=32767);

/*SAS에서 HANA DB 접속하는 두가지 형태*/

PROC SQL;
&CONNECT.;
CREATE TABLE T_NAME AS SELECT * FROM CONNECTION TO X1(SELECT ~~ ); DISCONNECT FROM X1; QUIT;

PROC SQL; DROP TABLE SAPTMP.T_NAME;
&CONNECT.; EXECUTE(CREATE TABLE T_NAME AS (SELECT ~~)) BY X1; DISCONNECT FROM X1; QUIT;

/******************************************************************************************************************/
/*상위 10개 자료 확인*/
PROC SQL;
&CONNECT.; 
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(SELECT TOP 10* FROM NHISBDA.COVID_PT_OPEN_SPC);
DISCONNECT FROM X1; QUIT;

/*SQL Server*/
PROC SQL;
CREATE TABLE WORK AS SELECT TOP 10 * FROM A.TEST;  QUIT;

/*Oracle*/
PROC SQL;
CREATE TABLE WORK AS SELECT  * FROM A.TEST WHERE ROWNUM<=10;  QUIT;

/*MySQL*/
PROC SQL;
CREATE TABLE WORK AS SELECT * FROM A.TEST order by PERSON_ID DESC LIMIT 10;  QUIT;

/*SAS SQL 문*/
PROC SQL OUTOBS=10; 
CREATE TABLE WORK AS SELECT * FROM A.TEST order by 10;  QUIT;
/******************************************************************************************************************/
PROC SQL;
&CONNECT.; 
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
SELECT COUNT(*) AS CNT, COUNT(DISTINCT INDI_DSCM_NO)  AS UNIQ_CNT, MAX(CDX_DT), MAX(DTH_DT) FROM 
NHISBDA.COVID_PT_OPEN_SPC); DISCONNECT FROM X1; QUIT;

PROC SQL;
CREATE TABLE WORK AS SELECT COUNT(PERSON_ID) AS CNT, COUNT(DISTINCT PERSON_ID) AS UNIQUE_CNT,  MIN(RECU_FR_DT) AS MIN_RECU_DT, 
MAX(RECU_FR_DT) AS MAX_RECU_DT
FROM A. TEST; QUIT;

/******************************************************************************************************************/
/*[ORACLE]*/
/*ROWNUM 조회된 순서대로 순번을 매김, ORDER BY 를 같이 사용할 경우 정렬된 sub-quary에 rownum 매길수 있음*/
/*ROW_NUMBER() ORDER BY된 결과에 순번을 매길 경우*/
/*그룹별(PARTITION)로 순번을 따로 부여*/
PROC SQL; DROP TABLE SAPTMP.T1;
&CONNECT.;
EXECUTE(
CREATE TABLE T1 AS(
SELECT DISTINCT CDX_NO, INDI_DSCM_NO, CDX_DT FROM (SELECT *, ROW_NUMBER() OVER (PARTITION BY INDI_DSCM_NO ORDER BY INDI_DSCM_NO, CDX_DT)
AS RN FROM NHISBDA.COVID_PT_OPEN_SPC WHERE INDI_DSCM_NO IS NOT NULL AND CDX_DT<="202203031")
WHERE RN=1)
) BY X1; DISCONNECT FROM X1;
QUIT;

PROC SQL;
&CONNECT.;
CREATE TABLE WORK AS SELCET * FROM CONNECRTION TO X1( SELECT TOP 10* FROM T1); DISCONNECT FROM X1;QUIT;

/******************************************************************************************************************/
/*코딩북 확인*/
PROC SQL; &CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
CALL SP_TABLE_DESC("NHISBDA",'HHDT_DSES_YY') /*'스키마','테이블 명'*/
); DISCONNECT FROM X1;
QUIT;
