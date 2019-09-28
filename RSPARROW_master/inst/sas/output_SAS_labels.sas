/*   Written by: Greg Schwarz, U.S. Geological Survey
     Date 9/19/2018
*/

/* Specify the directory containing the SAS data1 file (leave blank if file is in WORK directory) */
%let SAS_data1_path = ~ ;

/* Specify the name of the SAS data1 file */
%let SAS_data1_file = coastwlabel ;

/* Specify the output csv file (including path) containing the data dictionary variables */
%let datadict_pathfile_csv = ~\dataDictionaryFromSAS.csv ;

%macro output_labels ;

  %if %length(&sas_data1_path) > 0 %then %do ;
    libname indir "&SAS_data1_path" ;
  %end ;
  
  proc contents data = 
    %if %length(&sas_data1_path) > 0 %then indir.&sas_data1_file ;
    %else &sas_data1_file ;
    noprint out = data_info (keep = name label rename = (name = data1UserNames label = explanation)) ;
  run ;

  proc export data=data_info outfile="&datadict_pathfile_csv" dbms=csv replace ;
  run ;

%mend output_labels ;

%output_labels ;
