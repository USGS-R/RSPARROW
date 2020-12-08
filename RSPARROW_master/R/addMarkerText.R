#'@title addMarkerText
#'@description Adds user selected `add_plotlyVars` to data to be mapped and creates the string
#'             to execute in plotly to add to the hover text. \\cr \\cr
#'Executed By: \\itemize\{\\item checkDrainageareaErrors.Rmd
#'                        \\item checkDrainageareaErrorsChild.Rmd
#'                        \\item diagnosticContiguousChild.Rmd
#'                        \\item diagnosticCorrChild.Rmd
#'                        \\item diagnosticMaps.R
#'                        \\item diagnosticPlotsNLLS.Rmd
#'                        \\item mapSiteAttributes.R
#'                        \\item predictMaps.R
#'                        \\item predictMaps.Rmd} \\cr
#'Executes Routines: named.list.R \\cr
#'@param markerText character string starting plotly markerText (example: 
#'                  `markerText<-"~paste('</br> ',master_map_list[k],' :',
#'                  round(mapdataname,predictionClassRounding)"`)
#'@param add_plotlyVars character vector indicating user selected variables to add to plot hover
#'                      text
#'@param mapData data.frame of data to be plotted
#'@param sourceData data.frame with additional variables to be added to plot data for hover text
#'@return `marker.list` a named list containing `markerText` the character string to execute to
#'                      generate plotly hover text and `mapData` the data to be plotted


addMarkerText<-function(markerText,add_plotlyVars,mapData, sourceData){
  
  if (!is.na(add_plotlyVars[1])){
    add_plotlyVars<-as.character(ifelse(add_plotlyVars=="waterid","waterid_for_RSPARROW_mapping",add_plotlyVars))
    #add attributes to markerText
    for (m in add_plotlyVars){  
      if (m %in% names(sourceData)){
        markerText<-paste0(markerText,",'</br> ",m," : ',",m)
        if (!m %in% names(mapData)){
        markerAttrs<-eval(parse(text= paste0("data.frame(",m,"=sourceData$",m,")"))) 
        mapData<-cbind(mapData,markerAttrs)
        }
      }
    }
  }
  #wrap up text strings
  markerText<-paste0(markerText,")")
  
  marker.list<-named.list(markerText, mapData)
  return(marker.list)
}


