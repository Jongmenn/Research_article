/* cap input rows for the captured run */
options obs=100;

/* the original script uses libname a pointing at a Linux data-source path
   (/userdata06/room206/data_source/...); redirected here to a local,
   bundle-relative working directory so the script runs standalone */
libname a "./input";
