/* cap input rows for the captured run */
options obs=100;

/* original script connects to a SAP HANA warehouse via a CONNECT macro
   variable and writes intermediate tables under a Linux-side libname AA;
   redirected here to a local, bundle-relative working directory */
libname aa "./input";
