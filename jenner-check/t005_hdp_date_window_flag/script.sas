/* jenner-check bundle: t005_hdp_date_window_flag
   Source: [Plosone]_Maternal_neonatal_outcome_K_COV_N/NHIS_COVID19_SAS_20220726.sas
   (HDP macro)
   The macro body is byte-identical to the original repo file; the HDP
   call below points at a small mock maternal-claims table instead of the
   researcher's linked mother-infant NHIS extract (AA.&DATA. on a Linux/HANA
   source that only exists inside the institution environment).
*/

/* mock claims rows shaped like the linked maternal-claims table the
   original HDP macro reads: one visit per row, with MDCARE_STRT_DT (claim
   start date, YYYYMMDD string), COVID19 status, and the delivery-window
   bounds DATE1 (delivery date) / DATE2 (conception date) / COVID_DATE */
data aa.mother_claims;
  length MDCARE_STRT_DT $8;
  input DUPKEY $ MDCARE_STRT_DT $ COVID19 DATE1 :yymmdd10. DATE2 :yymmdd10. COVID_DATE :yymmdd10.;
  format DATE1 DATE2 COVID_DATE yymmdd10.;
  datalines;
D001 20210310 0 2021-03-15 2020-08-01 2020-02-01
D001 20200815 0 2021-03-15 2020-08-01 2020-02-01
D002 20210605 1 2021-06-10 2020-11-01 2020-02-01
D002 20201105 1 2021-06-10 2020-11-01 2020-02-01
D003 20210120 0 2021-01-25 2020-06-15 2020-02-01
D004 20210812 1 2021-08-20 2021-01-10 2020-02-01
D004 20210115 1 2021-08-20 2021-01-10 2020-02-01
D005 20191201 0 2021-05-01 2020-10-01 2020-02-01
;
run;

/* --- HDP macro body, unmodified from NHIS_COVID19_SAS_20220726.sas --- */
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

%HDP(MOTHER_CLAIMS, HDP_FLAG);

proc print data=mother_claims;
  title "HDP macro output: per-mother hypertensive-disorders-of-pregnancy window flag";
run;
