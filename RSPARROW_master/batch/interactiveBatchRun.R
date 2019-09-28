#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))

#get paths
load(gsub("interactiveBatchRun.R","interactiveBatch.RData",res))

#load RSPARROW
runRsparrow<-"no"
if_install_packages<-"no"
devtools::load_all(path_main,recompile = FALSE)

#get batch plot data
load(batchFilename)  

unPackList(lists = list(file.output.list = file.output.list,
                        scenario.input.list = scenario.input.list),
           parentObj = list(NA,NA)) 


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
    filename<- paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"SiteAttributes",.Platform$file.sep,run_id,"_SiteAttributes_",a,".pdf",sep="")
    pdf(filename)
    
    mapSiteAttributes(#Rshiny
      inputShiny,a, path_gis, sitedata, LineShapeGeo,data_names,TRUE,
      #regular
      mapColumn,mapdata,GeoLines,mapping.input.list,
      strTitle,unitAttr,batch_mode)
    
    dev.off()
    if (a==inputShiny$dataCheck[length(inputShiny$dataCheck)]){
      shell.exec(filename)  
    }
  }#for each attr
  
  #output siteAttr shapefile
  
  if (inputShiny$shapeFile=="yes"){
    xlat<-Mdiagnostics.list$xlat
    xlon<-Mdiagnostics.list$xlon
    map_siteAttributes.list<-as.character(inputShiny$attrCheck)
    CRStext<-mapping.input.list$CRStext
    siteAttrshape<-data.frame(xlat,xlon)
    for (s in 1:length(map_siteAttributes.list)){
      if (length(names(sitedata)[which(names(sitedata)==map_siteAttributes.list[s])])!=0){
        siteAttr<-eval(parse(text= paste("data.frame(",map_siteAttributes.list[s],"=sitedata$",map_siteAttributes.list[s],")",sep="")))
        siteAttrshape<-data.frame(siteAttrshape,siteAttr)
        names(siteAttrshape)[length(siteAttrshape)]<-map_siteAttributes.list[s]
      }
    }
    
    
    
    siteAttrshape<-sp::SpatialPointsDataFrame(siteAttrshape[,c("xlon","xlat")],siteAttrshape[,which(!names(siteAttrshape) %in% c("xlat","xlon"))],proj4string=sp::CRS(CRStext))
    
    if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""))){
      dir.create(paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,sep=""),showWarnings = FALSE)
    }
    if (!dir.exists(paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,sep=""))){
      dir.create(paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,sep=""),showWarnings = FALSE)
    }
    
    maptools::writeSpatialShape(siteAttrshape,paste(path_results,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,"siteAttrshape",sep=""))
    cat(rgdal::showWKT(sp::proj4string(siteAttrshape)),file=paste(path_results,.Platform$file.sep,"maps",.Platform$file.sep,"Interactive",.Platform$file.sep,"ESRI_ShapeFiles",.Platform$file.sep,"siteAttributes",.Platform$file.sep,"siteAttrshape.prj",sep="")) 
  }
  
}else if (inputShiny$mapType=="Source Change Scenarios"){
  
  
  
  predictScenarios(#Rshiny
    inputShiny,allMetrics, output_map_type,TRUE,
    #regular
    estimate.input.list,
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