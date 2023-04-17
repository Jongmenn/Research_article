
/*
readbuff: LIBNAME �ɼǰ� �����ͼ� �ɼ��� �⺻ �����ͺ��̽��� Ȯ�� ��ġ��
             ������ �� �⺻ ��� ũ��(64K)�� ����Ͽ� �����͸� �н��ϴ�. �⺻ �� ���� 64K�� �� �ʺ�� ������ ����. 
             SAS 9.4 �� ���� �������� SAS Viya���� �� �ɼ��� �⺻������ ���� �� ��ġ�� ���� 
insertbuff: oracle ����� �Է��ϴ� �� 
*/

/*(1) SAS HANA DB ���� (libname ����) NHISBASE*/
libname SAPEDW saphana readbuff=32767 server=kalmia1 port=30015 user=���̵� SCHEMA='NHISBASE' password=��й�ȣ;

/* (2) SAS HANA DB ���� (libname ����) NHISBDA */
libname SAPEDW_1 saphana readbuff=32767 server kalmia1 port=30015 user=���̵� SCHEMA='NHISBDA' password=��й�ȣ;

/*SAPTMP*/
libname SAPTMP saphana TABLE_TYPE=COLUMN readbuff=32767 insertbuff=32767 server=kalmia1 port=30015 user=���̵� password=��й�ȣ
connection=global PRESERVE_TAB_NAMES=YES;


/******************************************************************************************************************/
/*NHIS-COVID-19 ���� ���� �ڵ� �̸��ۼ� (������) */
/*���� �޴��� �����Ͽ� SAS�� SQL������ �ۼ��غ��� */
/*ǥ�� ��ȣƮ 1.0 �ڷ� �̿�*/
/*���� �ڵ� �������������ؼ� �̿� */
/******************************************************************************************************************/
LIBNAME GJ     'D:\EUMC\�����Ͱ���\ǥ����ȣƮ_DB(NHIS_NSC)\GJ';
LIBNAME JK     'D:\EUMC\�����Ͱ���\ǥ����ȣƮ_DB(NHIS_NSC)\JK';
LIBNAME T120 'D:\EUMC\�����Ͱ���\ǥ����ȣƮ_DB(NHIS_NSC)\T120';
LIBNAME T130 'D:\EUMC\�����Ͱ���\ǥ����ȣƮ_DB(NHIS_NSC)\T130';
LIBNAME T140 'D:\EUMC\�����Ͱ���\ǥ����ȣƮ_DB(NHIS_NSC)\T140';
LIBNAME T160 'D:\EUMC\�����Ͱ���\ǥ����ȣƮ_DB(NHIS_NSC)\T160';

/*���� �ڵ� �ӽ÷� ������ �ӽ� ���丮*/
libname A 'D:\EUMC\�ڷγ�19���ܿ���\data\example'; 
/******************************************************************************************************************/
/*���ڵ� �� �����ڵ� ����� ���ǻ��� �ڵ庯��*/
/*TRIM(MAIN_SICK) ���������ؼ� �̿� (���ʿ�, ���ܿ��� �۾� �Ϸ�)*/
/*LEFT(MAIN_SICK) ���� �����ؼ� �̿� (���ʿ�, ���ܿ��� �۾��Ϸ�)*/

PROC SQL;
CREATE TABLE ZZ AS SELECT PERSON_ID, KEY_SEQ, YKIHO_ID, RECU_FR_DT, FORM_CD, MAIN_SICK, SUB_SICK, RECN, VSCN, FST_IN_PAT_DT FROM
T120.T120 WHERE SUBSTR(MAIN_SICK,1,4)="A101"; QUIT;

/*����(JOIN) ������ ���, ON ������ A ���̺�� B ���̺��� ����Ű ������ B���̺� ���������� ����ϰ� WHERE ������ A���̺� ���� ������ ����� */
/*���ôϱ� 1���⵵ �ڷḸ �ϴ� ���� 2002*/
/*������ ������ �� ���ϰŸ� * �̿��ϱ�*/
/*������ �Ӽ��� ���� �ٸ� �� ����*/

PROC SQL;
CREATE TABLE A.TEST01 AS SELECT A.PERSON_ID, A.KEY_SEQ, A.FORM_CD, A.MAIN_SICK, A.SUB_SICK, A.RECN, A.VSCN, A.FST_IN_PAT_DT,
B.STND_Y, B.SEX, B.AGE_GROUP, B.DTH_YM, B.DTH_CODE1, B.SIDO, B.SGG FROM T120.Nhid_gy20_t1_2002 AS A INNER JOIN JK.Nhid_jk_2002 AS B
ON A.PERSON_ID=B.PERSON_ID 
AND B.SIDO="11" /*ON ���� �ڿ����� B ���̺� ���� ���� ����*/
WHERE A.FORM_CD="02" AND SUBSTR(MAIN_SICK,1,3)="I20";  /*A���̺� ���� ���� ����, �������� ��� AND�� ����*/
QUIT;


/******************************************************************************************************************/
/*�� ������ ������ �ʿ��� ��� ������ ������ �̿���-> 
   ��, ���� �ڷ� �Ǵ� ������� ������ ����� ���� �ڷ� ���¸� �����п� ������ �̿� */

/*��������(��) �� ���̺귯�� �Ҵ� ���� (����) */
%LET ���=/*���λ��*/;
LIBNAME SAPEDW saphana readbuff=32767 server=pepper1 port=30015 user=Nhis_&��� SCHEMA='NHISBASE' password=!Nhis_&���.1;
LIBNAME SAPTMP saphana  TABLE_TYPE=COLUMN readbuff=32767 insertbuff=32767 server=pepper1 port=30015 user=Nhis_&��� password=!Nhis_&���.1 connection=global PRESERVE_TAB_NAMES=YES;
LIBNAME SAPBDA_R saphana readbuff=32767 server=pepper1 port=30015 user=NHIS_&��� SCHEMA='NHISBDA' password=!Nhis_&���.1; 
LIBNAME AA ��/*������ ���� ���*/��;

/******************************************************************************************************************/
/******************************************************************************************************************/
/*�� HANA DB ���ο���(NHIS_���)�� �ڷ� �����ϴ� ����*/
%MACRO BRT_Y; %DO Y=2002 %TO 2014; 
%MACRO BRT_M; %DO YM=&Y.01 %TO &Y.12;

PROC SQL;connect to saphana as x1(server=pepper1 port=30015 user=���ΰ��� password=��й�ȣ readbuff=32767 insertbuff=32767);
execute(
CREATE TABLE TMP_TG_MAIN_&YM. AS ( 
        SELECT �����÷�
          FROM NHISBASE.�������̺��  /* ���� hana db schema ��� ( �⺻ :NHISBASE )*/
         WHERE MDCARE_STRT_YYYYMM = %nrbquote('&YM.')  
        ) by x1;
disconnect from x1;
QUIT;  
%END; %MEND BRT_M; %BRT_M; %END; %MEND BRT_Y; %BRT_Y; 

/*�� ������ ������ �ڷ� �����ϴ� ����*/
%MACRO BRT_Y; %DO Y=2002 %TO 2014; %MACRO BRT_M; %DO YM=&Y.01 %TO &Y.12;
PROC SQL;
  connect to saphana as x1(server=pepper1 port=30015 user=���ΰ��� password=��й�ȣ readbuff=32767 insertbuff=32767);
create table LDH.TMP_TG_MAIN_&YM. AS
SELECT * FROM CONNECTION TO X1(
        SELECT �����÷�
          FROM NHISBASE.���̺�� /* ���� hana db schema ��� ( �⺻ :NHISBASE )*/
         WHERE MDCARE_STRT_YYYYMM = %nrbquote('&YM.')
           AND �������� );
disconnect from x1;
QUIT;  
%END; %MEND BRT_M; %BRT_M; %END; %MEND BRT_Y; %BRT_Y;
/******************************************************************************************************************/
/******************************************************************************************************************/
/*%nrbquote() -> %, &�� ���ڷ� �νĽ�Ŵ*/
/*%bquote() ��� ���� ���ڷ� �ν�*/

/*������ ����� ��� */
/*�� HANA DB ���ο���(NHIS_���)�� �ڷ� �����ϴ� ���� ����*/
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
/*������ ����� ��� */
%MACRO BRT_Y; %DO Y=2004 %TO 2013; 
PROC SQL;
CREATE TABLE A.TEST_&Y. AS 
        SELECT PERSON_ID, KEY_SEQ, YKIHO_ID, RECU_FR_DT, FORM_CD, MAIN_SICK, SUB_SICK, RECN, VSCN, FST_IN_PAT_DT
        FROM T120.T120
		/*�����ǿ��� ���ڿ� �ν��� �� �߾ȵǼ� �̷����Է�, ���� ���� DB�� ���ڿ� ��ũ�� �ٿ��� ����*/
        WHERE SUBSTR(RECU_FR_DT,1,4) = "&Y." AND
		          SUBSTR(MAIN_SICK,1,3)="I20";
QUIT;  
%END; %MEND BRT_Y; 

%BRT_Y; 
/******************************************************************************************************************/
/******************************************************************************************************************/
/*�ڰ� ������ ������ ������ ���*/
/*BFC_INYR �����Ϳ��� �� ����� �� ���� �ڰݸ� �����ϰ����ϸ� �켱����(PRTY)�� ��1���� ���� �������� �ָ� �� (�̰� ������ ���� �ִµ�)*/
/*����Ŭ���� �� ���ڿ�("")�� NULL�� �ν��ϱ� ������ �÷����� �� ���ڿ��̸� NULL�� ������ ��ó�� ���� ó���ؾߵ�*/
PROC SQL; CREATE TABLE A.TEST_JK AS SELECT * FROM JK.JK 
WHERE STND_Y="2013" 
AND PERSON_ID <>0               /*������ �ǹ�(<>, != , ^=)*/
AND PERSON_ID IS NOT NULL    /*NULL���̸� ����*/
AND PERSON_ID <900000000  /*�� ���� ��ȿ���� ���� ��ġ �뷫 �ص� */
/*AND PRTY ="1" */                /*�켱���� 1�� ������� �Ѹ� �̰� ���� �� */
; QUIT;



/******************************************************************************************************************/
/******************************************************************************************************************/
/*��� ���̺��� 1�� ���̺�� �����Ǿ� ������, ���� ���� ���� ���ź��� ������� ��ü ������� ������� ������ ���� */
/*�ʼ����� �� ���ǻ���  ���߿� ���� �ؼ� ���������� �°� ������
(1) ��� ��� ����(DTH_HM_DT)
(2) �ڰ� ��� ����(DTH_BFC_DT)
(3) ���� ������ ���Ŀ� �ڰݻ���Ϸ� ������ ��� ���� ����(DTH_ASSMD_DT)  */

proc sql;
create table CALL SP_TABLE_DESC("a.test"); quit;

proc sql;
create table CALL SP_TABLE_DESC("NHISBASE","HBAT_TBJGBA20");

/*���̺� �Ӽ� �����ֱ� */
PROC CONTENTS DATA=A.TEST; RUN;

/******************************************************************************************************************/
/*����� �����ڵ� ���*/
%LET CONNECT=CONNET TO SAPHANA AS X1(SERVER=KALMIA1 PORT=30015 USER=NHIS_220051 PASSWORD=!Nhis_2200511 READBUFF=32767 
INSERTBUFF=32767);

/*SAS���� HANA DB �����ϴ� �ΰ��� ����*/

PROC SQL;
&CONNECT.;
CREATE TABLE T_NAME AS SELECT * FROM CONNECTION TO X1(SELECT ~~ ); DISCONNECT FROM X1; QUIT;

PROC SQL; DROP TABLE SAPTMP.T_NAME;
&CONNECT.; EXECUTE(CREATE TABLE T_NAME AS (SELECT ~~)) BY X1; DISCONNECT FROM X1; QUIT;

/******************************************************************************************************************/
/*���� 10�� �ڷ� Ȯ��*/
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

/*SAS SQL ��*/
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
/*ROWNUM ��ȸ�� ������� ������ �ű�, ORDER BY �� ���� ����� ��� ���ĵ� sub-quary�� rownum �ű�� ����*/
/*ROW_NUMBER() ORDER BY�� ����� ������ �ű� ���*/
/*�׷캰(PARTITION)�� ������ ���� �ο�*/
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
/*�ڵ��� Ȯ��*/
PROC SQL; &CONNECT.;
CREATE TABLE WORK AS SELECT * FROM CONNECTION TO X1(
CALL SP_TABLE_DESC("NHISBDA",'HHDT_DSES_YY') /*'��Ű��','���̺� ��'*/
); DISCONNECT FROM X1;
QUIT;
