#'@title checkBinaryMaps
#'@description Checks if binary mapping objects exist and loads binary files \\cr \\cr
#'Executed By: \\itemize\{\\item checkDrainageareaErrors.R
#'             \\item diagnosticPlotsNLLS.R
#'             \\item diagnosticPlotsValidate.R
#'             \\item mapSiteAttributes.R
#'             \\item predictMaps.R\} \\cr
#'@param mapSetting setting in control file that user sets to use to load the binary map 
#'       (`lineShapeName`, `polyShapeName`, or `LineShapeGeo`)
#'@param path_gis path to users gis data
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode
#'@return `fileLoaded`  logical TRUE/FALSE indicating whether or not file is loaded



checkBinaryMaps<-function(mapSetting,path_gis,batch_mode){
  
  
  #get name of setting
  settingName<-deparse(substitute(mapSetting))
  
  #set name of output object
  if (settingName=="lineShapeName"){
    outObj<-"lineShape"
  }else if (settingName=="LineShapeGeo"){
    outObj<-"GeoLines"
  }else{
    outObj<-"polyShape"
  }
  objfile <- paste0(path_gis,.Platform$file.sep,outObj)   
  
  
  
  if(!is.na(mapSetting) & file.exists(objfile)) { 
    load(objfile)
    assign(outObj,get(outObj),env = parent.frame())
    
    if (!exists("outObj")){
      message(paste0(settingName," <- ",mapSetting," NOT FOUND MAPPING CANNOT COMPLETE.\nSet if_create_binary_maps<-'yes' to create binary files."))
      if (batch_mode=="yes"){#if batch output message to log
        cat(settingName," <- ",mapSetting," NOT FOUND MAPPING CANNOT COMPLETE.\nSet if_create_binary_maps<-'yes' to create binary files.",sep="")
      }
      fileLoaded<-FALSE
    }else{
      fileLoaded<-TRUE
    }
  }else{
    fileLoaded<-FALSE
  }
  
  
  return(fileLoaded)
  
}#end function
