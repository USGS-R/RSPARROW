/* Program name: Import_csv_data1.sas
   Written by: Greg Schwarz, U.S. Geological Survey
   Date 9/19/2018

   Purpose: Program reads in a csv file containing the data1 information. Also input
   is a data dictionary csv file which are used to create labels for the SAS data1 
   output file. 
*/

%let infile_data1_pathfile_csv = ~\coasttest.csv ;
%let infile_data1dict_pathfile_csv = ~\datadicttest.csv ;

%let outfile_data1_pathname_sas = ~ ;
%let outfile_data1_filename_sas = coastwlabel ;

/* ------------------------------------------------------------------------ */

%macro import_data ;

  libname outdir "&outfile_data1_pathname_sas" ;

  proc import datafile = "&infile_data1_pathfile_csv" out = outdir.&outfile_data1_filename_sas dbms = csv replace ;
    getnames = yes ;
    guessingrows = MAX ; 
  run ;

  proc import datafile = "&infile_data1dict_pathfile_csv" out = data1dict dbms = csv replace ;
    getnames = yes ;
    guessingrows = MAX ; 
  run ;

  proc datasets lib = outdir nolist ;
    modify &outfile_data1_filename_sas ;
      label
      %if %sysfunc(exist(data1dict)) %then %do ;
        %let in_id = %sysfunc(open(data1dict)) ;
        %let varnum_name = %sysfunc(varnum(&in_id,data1UserNames)) ;
        %let varnum_label = %sysfunc(varnum(&in_id,explanation)) ;
        %if &varnum_name > 0 and &varnum_label > 0 %then %do ;
          %do i = 1 %to %sysfunc(attrn(&in_id,nobs)) ;
            %let rc = %sysfunc(fetch(&in_id)) ;
            %sysfunc(getvarc(&in_id,&varnum_name)) = "%sysfunc(getvarc(&in_id,&varnum_label))"
          %end ;
        %end ;
        %let rc = %sysfunc(close(&in_id)) ;;
      %end ;
  quit ;

%mend import_data ;

%import_data ;
