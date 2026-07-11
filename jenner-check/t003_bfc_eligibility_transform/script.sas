/* jenner-check bundle: t003_bfc_eligibility_transform
   Source: [JCEM]T2DM_Lean_patientT2DM_Lean_patient_Mortality_CVD/T2DM_Script_20240208.sas
   (BFC macro, lines 27-37)
   The macro body is byte-identical to the repo's original; the %BFC call
   below points at a small mock eligibility table instead of the
   researcher's NHIS "자격" (insurance eligibility) extract (RAW.BFC_2010
   on a Linux libname that only exists inside the institution's environment).
*/

/* mock NHIS eligibility ("자격") rows shaped like RAW.BFC_20&YY.: one row
   per insured person per year, with the columns %BFC renames, derives, or
   drops */
data raw.bfc_2010;
  length RVSN_ADDR_CD $5;
  input INDI_DSCM_NO STD_YYYY SEX_TYPE BYEAR GAIBJA_TYPE RVSN_ADDR_CD $
        CALC_CTRB_VTILE_FD CALC_CTRB_FD CNT_ID_HHHI_FD CMPR_DSB_GRADE MAIN_DSB_TYPE INDTP_CD;
  datalines;
3001 2010 1 1975 1 11010 6 55000 3 0 0 5
3002 2010 2 1968 1 26030 8 72000 2 0 0 3
3003 2010 1 1990 2 27010 3 30000 4 1 2 7
3004 2010 2 1955 1 11020 9 88000 1 0 0 5
3005 2010 1 1982 1 28010 5 41000 3 0 0 6
;
run;

/* --- BFC macro body, unmodified from T2DM_Script_20240208.sas --- */
%MACRO BFC(YY);
DATA A.BFC_20&YY. ;
RETAIN PKEY STD_YYYY INDI_DSCM_NO SEX_TYPE AGE GAIBJA_TYPE SIDO RVSN_ADDR_CD CALC_CTRB_VTILE_FD;
SET RAW.BFC_20&YY.;
RENAME SEX_TYPE=SEX GAIBJA_TYPE=GAIBJA RVSN_ADDR_CD=SGG CALC_CTRB_VTILE_FD=INCOME_G;
AGE=STD_YYYY-BYEAR;
SIDO=SUBSTR(RVSN_ADDR_CD,1,2);
PKEY=COMPRESS(STD_YYYY)||("-")||COMPRESS(INDI_DSCM_NO);
DROP CALC_CTRB_FD CNT_ID_HHHI_FD CMPR_DSB_GRADE MAIN_DSB_TYPE INDTP_CD;
RUN;
%MEND;

%BFC(10);

proc print data=a.bfc_2010;
  title "BFC macro output: eligibility records with derived AGE/SIDO/PKEY";
run;
