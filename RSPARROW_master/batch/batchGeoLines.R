#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))
if (length(res)!=0){
  load(gsub("batchGeoLines.R","batch.RData",res))
  
  if (RSPARROW_errorOption=="yes"){
    #errorhandle
    backupOptions<-list(error = options()$error,
                        show.error.locations = options()$show.error.locations,
                        keep.source = options()$keep.source)
    options(error = quote({
      #print custom message to console
      message("\nRSPARROW SYSTEM ERROR OCCURRED");
      #instruct the user to reset their options
      message('To reset user options in R use options(backupOptions)'); 
      #First dump error stack to file; not accessible by the R session.
      dump.frames("errorDump", to.file=TRUE, include.GlobalEnv=TRUE); 
      #sink to file
      sink(file=paste0(path_results,"error.log"));  
      #print custom error message to file
      cat("RSPARROW SYSTEM ERROR OCCURRED\n");
      #instruct the user to reset their options
      cat('To reset user options in R use options(backupOptions)\n \n'); 
      #Dump again to get error message and write it to error log; 
      #accessible by the R session.
      dump.frames(); 
      #Print simple error message to file
      cat(attr(last.dump,"error.message")); 
      cat('\nTraceback:');
      cat('\n'); #line space
      # Print full traceback of function calls. 
      #The '2' omits the outermost two function calls in the traceback.
      traceback(2); 
      shell.exec(paste0(path_results,"error.log"));
      sink() #end sink
    }),
    #show line numbers in traceback (shown as 'from #4')
    #line numbers count from the function call (i.e. `nestedFunc<-function(){` is line 1)
    show.error.locations = TRUE,keep.source = TRUE)
  }#end Error
  
  
  
  suppressWarnings(suppressMessages(library(rgdal)))
  suppressWarnings(suppressMessages(library(sp)))
  suppressWarnings(suppressMessages(library(sf)))

  #load and convert shape file



  GeoLines <- sf::st_read(paste0(path_gis,"/",LineShapeGeo,".shp"), quiet = TRUE)
  GeoLines<-st_transform(GeoLines,CRS(CRStext))

  #save file
  objfile <- paste0(path_gis,.Platform$file.sep,"GeoLines")
  save(GeoLines,file=objfile) 
  
  if (RSPARROW_errorOption=="yes"){
    options(backupOptions)  
  }
}