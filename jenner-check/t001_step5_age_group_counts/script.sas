/* jenner-check bundle: t001_step5_age_group_counts
   Source: [EI]PM2.5_AOM/AOM_AR_191210.sas (STEP5 macro, lines 13-56)
   The macro body is byte-identical to the repo's original; only the
   %STEP5 call arguments below select a small mock cohort table instead
   of the researcher's NHIS claims extract (A.AOM_W1 on a Linux libname
   that doesn't exist outside the researcher's institution).
*/

/* mock cohort: one row per outpatient/inpatient claim record, shaped like
   the NHIS claims table the original %STEP5 calls read from (RVSN_ADDR_CD,
   AGE, SEX_TYPE, START_DATE, INDI_DSCM_NO) */
data a.aom_w1;
  length RVSN_ADDR_CD $5 START_DATE 8;
  input INDI_DSCM_NO RVSN_ADDR_CD $ AGE SEX_TYPE START_DATE :yymmdd10.;
  format START_DATE yymmdd10.;
  datalines;
1001 11010 0 1 2015-01-05
1002 11010 1 2 2015-01-05
1003 11020 2 1 2015-01-05
1004 26010 3 2 2015-01-06
1005 26020 0 1 2015-01-06
1006 27010 4 2 2015-01-06
1007 27020 1 1 2015-01-07
1008 28010 2 2 2015-01-07
1009 29010 3 1 2015-01-07
1010 30010 0 2 2015-01-08
1011 31010 6 1 2015-01-08
1012 11030 2 1 2015-01-08
1013 26030 1 2 2015-01-09
1014 27030 3 1 2015-01-09
1015 28030 0 2 2015-01-09
;
run;

/* --- STEP5 macro body, unmodified from AOM_AR_191210.sas --- */
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

/* call the macro on Seoul (SIDO 11) exactly as the original repo calls it
   for each region code, using our mock cohort in place of A.AOM_W1 */
%STEP5 (AOM_W11, AOM_W1, S1_W1_COUNT, 11);

proc print data=a.s1_w1_count;
  title "STEP5 output: daily age/sex group counts, SIDO=11";
run;
