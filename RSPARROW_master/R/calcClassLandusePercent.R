#'@title calcClassLandusePercent
#'@description Calculates the land use percentages for the total drainage area for each reach, 
#'            based on the user selected land uses given by the class_landuse control setting, followed by a 
#'            subsetting of the reaches by the class_landuse_percent control setting. \\cr \\cr
#'Executed By: predictSummaryOutCSV.R \\cr
#'Executes Routines: \\itemize\{\\item accumulateIncrArea.R
#'             \\item unPackList.R\} \\cr
#'@param subdata data.frame input data (subdata)
#'@param class.input.list list of control settings related to classification variables
#'@return `percentClassLanduse`  data.frame with calculated percentages by land use



calcClassLandusePercent<-function(subdata,class.input.list){
  
  
  unPackList(lists = list(class.input.list = class.input.list),
             parentObj = list(NA)) 
  
  #get data
  data<-subdata
  
  #get subset of data that matches user input class_landuse_percent
  for (i in class_landuse){
    #accumulate incremental landuse
    areaIncr<-accumulateIncrArea(data,i,"totalIncrArea")
    
    #get as percent of demtarea
    percentDemt<-merge(data[c("waterid","demtarea")],areaIncr, by ="waterid")
    percentDemt$percentLanduse<-percentDemt$totalIncrArea/percentDemt$demtarea*100
    
    #subset by user selected class_landuse_percent
    percentSub<-data.frame(waterid = percentDemt[which(percentDemt$percentLanduse>=class_landuse_percent[which(class_landuse==i)]),]$waterid)
    if (nrow(percentSub)!=0){
      percentSub$landuse<-i}
    
    #save
    if (i==class_landuse[1]){
      if (nrow(percentSub)!=0){
        percentClassLanduse<-percentSub
      }else{
        percentClassLanduse<-as.data.frame(matrix(c(1,"1"),ncol=2,nrow=0))
        names(percentClassLanduse)<-c("waterid","landuse")
      } 
    }else{
      if (nrow(percentSub)!=0){
        percentClassLanduse<-rbind(percentClassLanduse,percentSub)}
    }
  }#end for each class_landuse
  
  
  return(percentClassLanduse)
}#end function
