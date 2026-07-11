/* cap input rows for the captured run */
options obs=100;

/* original script connects to a SAP HANA warehouse and writes to a
   Linux-side libname A; redirected here to a local, bundle-relative
   working directory so the script runs standalone */
libname a "./input";
