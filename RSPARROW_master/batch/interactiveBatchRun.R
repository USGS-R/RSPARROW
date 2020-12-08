#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))

#get paths
load(gsub("interactiveBatchRun.R","interactiveBatch.RData",res))

#load RSPARROW
runRsparrow<-"no"
devtools::load_all(path_main,recompile = FALSE)

#get batch plot data
load(batchFilename)  

unPackList(lists = list(file.output.list = file.output.list,
                        scenario.input.list = scenario.input.list,
                        mapping.input.list = mapping.input.list),
           parentObj = list(NA,NA,NA)) 


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



#run interactive batch plot

#compile metrics
allMetrics<-as.character(unlist(inputShiny[which(regexpr("Check",names(inputShiny))>0 & names(inputShiny)!="outCheck")]))


if (inputShiny$mapType=="Source Change Scenarios"){
  output_map_type<-tolower(as.character(inputShiny$outCheck))
}else{
  output_map_type<-inputShiny$mapType
}


if (inputShiny$mapType=="Stream" | inputShiny$mapType=="Catchment"){
  mapScenarios<-FALSE
  scenarioFlag<-NA
  
  predictMaps(inputShiny,allMetrics,output_map_type, TRUE,
              file.output.list,
              data_names,mapping.input.list,
              subdata,
              #scenarios
              mapScenarios,
              scenario_map_list,
              predictScenarios.list,
              scenarioFlag,
              batch_mode)
  
  
  
  
}else if (inputShiny$mapType=="Site Attributes"){
  
  for (a in inputShiny$dataCheck){
    filename<- paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"SiteAttributes",.Platform$file.sep,run_id,"_SiteAttributes_",a,".pdf")
    if (inputShiny$enablePlotly=="static"){
      pdf(filename)
    }else{
      filename<-gsub(".pdf",".html",filename)
    }
    
    p<-mapSiteAttributes(#Rshiny
      inputShiny,a, path_gis, sitedata, LineShapeGeo,data_names,TRUE,
      #regular
      mapColumn,mapdata,GeoLines,mapping.input.list,
      strTitle,unitAttr,batch_mode)
    
    if (inputShiny$enablePlotly=="static"){
      replayPlot(p)
    
      }else{
        reportPath<-paste0(path_master,"shinySavePlot.Rmd")
        #edit title of report
        reportTitle<-run_id
        #read Rmd file as text
        x <- readLines(reportPath)
        #find where title is designated
        editthis<-x[which(regexpr("title:",gsub(" ","",x))>0)]
        #replace with current reportTitle
        y <- gsub( editthis, paste0("title: '",reportTitle,"'"), x )
        #overwrite the file
        cat(y, file=reportPath, sep="\n") 
        #ptm <- proc.time()
        rmarkdown::render(
          reportPath, params = list(
            p = p
          ),
          output_file = filename, quiet = TRUE
        )
        #saveWidget(p,filename,selfcontained = FALSE)
      }
    if (inputShiny$enablePlotly=="static"){
      dev.off()
    } 
    if (a==inputShiny$dataCheck[length(inputShiny$dataCheck)]){
      shell.exec(filename)  
    }
  }#for each attr
  
  #output siteAttr shapefile
  
  if (inputShiny$shapeFile=="yes"){
    load(paste0(path_results,"estimate",.Platform$file.sep,run_id,"_Mdiagnostics.list"))
    xlat<-Mdiagnostics.list$xlat
    xlon<-Mdiagnostics.list$xlon
    map_siteAttributes.list<-as.character(inputShiny$attrCheck)
    CRStext<-mapping.input.list$CRStext
    siteAttrshape<-data.frame(xlat,xlon)
    for (s in 1:length(map_siteAttributes.list)){
      if (length(names(sitedata)[which(names(sitedata)==map_siteAttributes.list[s])])!=0){
        siteAttr<-eval(parse(text= paste0("data.frame(",map_siteAttributes.list[s],"=sitedata$",map_siteAttributes.list[s],")")))
        siteAttrshape<-data.frame(siteAttrshape,siteAttr)
        names(siteAttrshape)[length(siteAttrshape)]<-map_siteAttributes.list[s]
      }
    }
    
    
    
    siteAttrshape<-sp::SpatialPointsDataFrame(siteAttrshape[,c("xlon","xlat")],siteAttrshape[,which(!names(siteAttrshape) %in% c("xlat","xlon"))],proj4string=sp::CRS(CRStext))
    
    if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep))){
      dir.create(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep),showWarnings = FALSE)
    }
    if (!dir.exists(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep))){
      dir.create(paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep),showWarnings = FALSE)
    }
    
    maptools::writeSpatialShape(siteAttrshape,paste0(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,"siteAttrshape"))
    cat(rgdal::showWKT(sp::proj4string(siteAttrshape)),file=paste0(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,"siteAttrshape.prj")) 
  }
  
}else if (inputShiny$mapType=="Source Change Scenarios"){
  
  
  
  predictScenarios(#Rshiny
    inputShiny,allMetrics, output_map_type,TRUE,
    #regular
    estimate.input.list,estimate.list,
    predict.list,scenario.input.list,
    data_names,JacobResults,if_predict,
    #bootcorrection,
    DataMatrix.list,SelParmValues,subdata,
    #predictStreamMapScenarios
    file.output.list,
    #scenarios out
    add_vars,
    mapping.input.list,
    batch_mode,
    RSPARROW_errorOption)
  
  
  
}


if (RSPARROW_errorOption=="yes"){
  options(backupOptions)  
}