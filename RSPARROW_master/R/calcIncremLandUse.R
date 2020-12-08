#'@title calcIncremLandUse
#'@description Calculates the land-use percentages for the incremental drainage areas of the 
#'            monitoring sites for use in decile diagnostics \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: sumIncremAttributes.R \\cr
#'@param subdata data.frame input data (subdata)
#'@param class_landuse character vector of class_landuses from sparrow_control.R
#'@param idseq staidseq or vstaidseq, integer vector site IDs assigned contiguously to 
#'       upstream incremental reaches
#'@param minimum_reaches_separating_sites number indicating the minimum number of reaches 
#'       separating sites
#'@return `df`  data.frame incremental land-use percentages calculated by class_landuse



calcIncremLandUse <- function(subdata,class_landuse,idseq,minimum_reaches_separating_sites) { 
  
  
  # Note that the setup code required for diagnostics for non-contiguous classification 
  # variables that require summation of attributes for the station incremental areas, such as land use.
  # Contiguous variables, such as HUC-4, do not require this step.
  
  # setup classvar2 classes in 'df' object for plotting diagnostics for non-contiguous variables:
  if(!is.na( class_landuse[1])){
    classvar2 <- character(length(class_landuse))
    for (i in 1:length(class_landuse)) {
      classvar2[i] <- paste0(class_landuse[i],"_pct")
    }
  }
  
  # incremental site area 
  waterid <- subdata$waterid
  tnode <- subdata$tnode
  fnode <- subdata$fnode
  demiarea <- subdata$demiarea
  df <- data.frame(waterid,demiarea,idseq)
  siteiarea <- sumIncremAttributes(idseq,demiarea,"siteincarea")  # sum incremental area by unique siteIDs
  df <- siteiarea
  
  # Code executes 'sumIncremAttributes' function for each land-use type:
  # Forest percentage example
  #siteiarea <- sumIncremAttributes(idseq,subdata$forest,"forest_pct")
  #df <- merge(df,siteiarea,by="idseq",all.y=FALSE,all.x=TRUE)
  #df$forest_pct <- df$forest_pct / df$siteincarea * 100
  if(!is.na( class_landuse[1])){
    for (i in 1:length(class_landuse)){
      nclass <- paste0("subdata$",class_landuse[i])
      nclasspct <- paste0("df$",classvar2[i])
      xname <- paste0("siteiarea <- sumIncremAttributes(idseq,",nclass,",",shQuote(classvar2[i]),")")
      eval((parse(text=xname)))
      
      df <- merge(df,siteiarea,by="idseq",all.y=FALSE,all.x=TRUE)
      
      xname <- paste0("df$",classvar2[i]," <- df$",classvar2[i]," / df$siteincarea * 100")
      eval((parse(text=xname)))
    }}
  
  # substitute 0.0 for NAs for user-selected parameters (assumes variables already present in 'df')
  setNAdf <- function(names){
    for (i in 1:length(names)) {
      dname <- paste0("df$",names[i],"<-ifelse(is.na(df$",names[i],"),0.0,df$",names[i],")") 
      eval(parse(text=dname)) 
    }
  }
  if(!is.na( class_landuse[1])){
    setNAdf(classvar2)
  }
  
  return(df)   
  
}#end function



