/* jenner-check bundle: t002_icd_flag_ihd
   Source: [ER]PM2.5_Elderly_IHDP/M2.5_Elderly_IHD_SAS.sas (IHD macro, lines 17-24)
   The macro body is byte-identical to the repo's original; the %IHD call
   below points at a small mock claims table instead of the researcher's
   NHIS inpatient extract (RAW.T20_2008 on a Linux libname that only exists
   inside the institution's analysis environment).
*/

/* mock claims rows shaped like the NHIS T20 inpatient/outpatient table the
   original %IHD macro reads: FORM_CD (02/03 = inpatient claim forms),
   SICK_SYM1/SICK_SYM2 (primary/secondary ICD-10 diagnosis codes), plus the
   billing columns the macro drops afterward */
data raw.t20_2008;
  length FORM_CD $2 SICK_SYM1 $5 SICK_SYM2 $5 MCARE_SUBJ_CD $2 HSPTZ_PATH_TYPE $2
         DISP_SUBJ_TYPE $2;
  input INDI_DSCM_NO FORM_CD $ SICK_SYM1 $ SICK_SYM2 $ ED_RC_TOT_AMT EDC_SBA EDC_INSUR_BRDN_AMT
        MCARE_SUBJ_CD $ HSPTZ_PATH_TYPE $ DISP_SUBJ_TYPE $ SICK_SYM4 $ SICK_SYM5 $;
  datalines;
2001 02 I219 J189 850000 100000 750000 01 01 01 . .
2002 03 I250 I208 620000 80000  540000 01 02 01 . .
2003 02 J440 J189 210000 30000  180000 02 01 02 . .
2004 03 I209 M545 990000 150000 840000 01 01 01 . .
2005 02 E119 I255 430000 60000  370000 02 02 02 . .
2006 04 I219 I209 500000 70000  430000 01 01 01 . .
2007 02 K219 I210 310000 40000  270000 01 02 01 . .
2008 03 I215 I220 780000 110000 670000 02 01 02 . .
;
run;

/* --- IHD macro body, unmodified from M2.5_Elderly_IHD_SAS.sas --- */
%Macro IHD(TABLE1,TABLE2);
data A.&table1 ; set RAW.&table2;
IF FORM_CD IN("02","03") AND "I20" <= SUBSTR(SICK_SYM1,1,3)<="I25" THEN K1=2; ELSE K1=0; /*입원 이면서 주상병 STROKE 코드, K1은 주상병에 존재하면 2*/
IF FORM_CD IN("02","03") AND "I20" <= SUBSTR(SICK_SYM2,1,3)<="I25" THEN K2=1; ELSE K2=0; /*입원 이면서 부상병 STROKE 코드, K2은 부상병에 존재하면 1*/
ICD_RANK=K1+K2; IF ICD_RANK>0; /*ICD_RANK 주+부상병 중요도 순위 나타냄*/
DROP MCARE_SUBJ_CD HSPTZ_PATH_TYPE ED_RC_TOT_AMT EDC_SBA EDC_INSUR_BRDN_AMT DISP_SUBJ_TYPE SICK_SYM4 SICK_SYM5;
RUN;
%MEND IHD;

%IHD(TOTAL_IHD_08, T20_2008);

proc print data=a.total_ihd_08;
  title "IHD macro output: inpatient claims flagged for ischemic heart disease (ICD-10 I20-I25)";
run;
