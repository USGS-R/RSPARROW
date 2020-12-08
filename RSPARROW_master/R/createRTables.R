#'@title createRTables
#'@description Creates user input tables to be converted into hottables in Rshiny. \\cr \\cr
#'Executed By: shinyMap2.R \\cr
#'Executes Routines: named.list.R \\cr
#'@param selectSources character vector of SOURCE parameters defined as 
#'       as.character(JacobResults$Parmnames[which(JacobResults$btype=="SOURCE")]) if `JacobResults` object is available
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@return `out`  list containing all user input tables for the Rshiny app.



createRTables<-function(selectSources,data_names, mapping.input.list){
  #set up Rtables
  #for different reach selection for different sources
  if (length(selectSources)==1){
    if (selectSources!=""){
      selectSources<-c("",selectSources)
    }else{
      selectSources<-"" 
    }
  }else{
    selectSources<-c("",selectSources)
  }
  #set up table with all options
  allSourcesDFno<-data.frame(Source = factor(as.character(""),levels = selectSources),
                             PercentChange = as.character(""),
                             ChangeCoefficient = factor(as.character("no"),levels = c("yes","no")),
                             SelectionVariable = factor(as.character(""),levels = c("",as.character(data_names$sparrowNames))),
                             Min = as.character(""),
                             Max = as.character(""),
                             Equals = as.character(""),
                             Like = as.character(""),
                             LanduseConversion = factor(as.character("None"),levels = c("None",selectSources),exclude = ""),
                             Separator = factor(as.character(""),levels = c("","OR","AND")),
                             stringsAsFactors = FALSE )
  
  #for all sources same reach selection
  sourceRed<-data.frame(Source = factor(as.character(""),levels = selectSources),
                        PercentChange = as.character(""),
                        ChangeCoefficient = factor(as.character("no"),levels = c("yes","no")),
                        LanduseConversion = factor(as.character("None"),levels = c("None",selectSources),exclude = ""),
                        stringsAsFactors = FALSE )
  #set 2nd table for allSources get same reach selection
  allSourcesDF<-allSourcesDFno[,which(!names(allSourcesDFno) %in% names(sourceRed))]
  
  
  #cosmetic mapping table
  cosmetic<-mapping.input.list[which(regexpr("prediction",names(mapping.input.list))>0 |
                                       regexpr("siteAttr",names(mapping.input.list))>0 | 
                                       regexpr("scenario",names(mapping.input.list))>0 |
                                       names(mapping.input.list)=="lineWidth")]
  cosmetic<-cosmetic[which(names(cosmetic)!="map_siteAttributes.list")]
  cosmeticDF<-data.frame(type = character(0),
                         setting = character(0),
                         settingValue = character(0))
  #get current control script setting values for cosmetic mapping settings
  for (c in names(cosmetic)){
    setting<-eval(parse(text = paste0("cosmetic$",c)))
    sub<-data.frame(type = ifelse(regexpr("prediction",c)>0 | c=="lineWidth","prediction",
                                  ifelse(regexpr("site",c)>0,"siteAttr",
                                         "scenario")))
    sub$setting<-c
    sub$settingValue<-ifelse(length(setting)!=1,
                             paste0("c(",paste("'",setting,"'",collapse=","),")"),
                             setting)
    
    cosmeticDF<-rbind(cosmeticDF,sub)
  }
  
  #cosmetic tables for each namespace
  cosmeticPred<-cosmeticDF[which(cosmeticDF$type=="prediction"),(names(cosmeticDF)!="type")]
  cosmeticSite<-cosmeticDF[which(cosmeticDF$type=="siteAttr"),(names(cosmeticDF)!="type")]
  cosmeticScen<-cosmeticDF[which(cosmeticDF$type %in% c("prediction","scenario") & cosmeticDF$setting!="predictionMapColors"),(names(cosmeticDF)!="type")]
  
  
  
  #output all tables
  out<-named.list(allSourcesDFno,sourceRed,allSourcesDF,cosmeticPred,cosmeticSite,cosmeticScen)
  
  return(out)
}
