/* cap input rows for the captured run */
options obs=100;

/* original script uses RAW/A libnames pointing at Linux data-source paths;
   redirected here to a local, bundle-relative working directory */
libname raw "./input";
libname a "./input";
