#'@title mapSiteAttributes
#'@description function to execute site attribute mapping \\cr \\cr
#'Executed By: \\itemize\{\\item interactiveBatchRun.R
#'             \\item diagnosticPlotsNLLS.R
#'             \\item goShinyPlot.R\} \\cr
#'Executes Routines: \\itemize\{\\item checkBinaryMaps.R
#'             \\item mapBreaks.R
#'             \\item named.list.R
#'             \\item replaceNAs.R
#'             \\item unPackList.R\} \\cr
#'@param input top level interactive user input in Shiny app
#'@param attr character string shiny user input of attribute to map in `mapSiteAttributes.R`
#'@param path_gis path to users gis data
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0), ]`
#'@param LineShapeGeo character string control settting indicating which binary map file to 
#'       load for the Geolines background layer
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param Rshiny TRUE/FALSE indicating whether routine is being run from the Shiny app
#'@param mapColumn character string indicating column of data to be mapped
#'@param mapdata input data.frame with lat, long, and column to be mapped
#'@param GeoLines Optional geospatial shape file for overlay of lines on output maps
#'@param strTitle character string for plot title
#'@param unitAttr character string indicating the unit of the attribute being mapped by 
#'       `mapSiteAttributes.R`
#'@param batch_mode yes/no character string indicating whether RSPARROW is being run in batch 
#'       mode



mapSiteAttributes<-function(#Rshiny
  input,attr, path_gis, sitedata, LineShapeGeo,data_names,Rshiny,
  #regular
  mapColumn,mapdata,GeoLines,mapping.input.list,
  strTitle,unitAttr,batch_mode){
  
  
  if (((input$var!="" |!is.na(attr)) & Rshiny==TRUE)|Rshiny==FALSE){
    
    if (Rshiny==TRUE){
      
      #get geoLines
      existGeoLines<-checkBinaryMaps(LineShapeGeo,path_gis,batch_mode)
      if (existGeoLines==TRUE){
        load(paste(path_gis,.Platform$file.sep,"GeoLines",sep=""))
      }
    }
    
    # create global variable from list names (mapping.input.list)
    unPackList(lists = list(mapping.input.list = mapping.input.list),
               parentObj = list(NA))
    
    if (Rshiny==TRUE){
      siteAttr_mapPointSize<-as.numeric(input$siteAttr_mapPointSize)
      siteAttrTitleSize<-as.numeric(input$siteAttrTitleSize)
      siteAttrLegendSize<-as.numeric(input$siteAttrLegendSize)
      siteAttrColors<-eval(parse(text=as.character(input$siteAttrColors)))
      siteAttrClassRounding<-as.numeric(input$siteAttrClassRounding)
      siteAttr_mapPointStyle<-as.numeric(input$siteAttr_mapPointStyle)
      siteAttrMapBackground<-gsub("\"","",gsub("'","",as.character(input$siteAttrMapBackground)))
      
      if (input$batch!="Batch"){
        
        
        mapColumn<-as.character(input$var)
      }else{
        
        mapColumn<-as.character(attr)
        
      }
      siteAttr<-eval(parse(text= paste("data.frame(",mapColumn,"=sitedata$",mapColumn,")",sep="")))
      titleAttr<-data_names[which(data_names$sparrowNames==mapColumn),]
      unitAttr<-titleAttr$varunits
      titleAttr<-as.character(titleAttr$explanation)
      strTitle<-titleAttr
      
      xlat<-sitedata$lat
      xlon<-sitedata$lon
      
      mapdata<-data.frame(xlat,xlon,siteAttr)
      
    }else{ #regular
      #get data
      mapdata <- mapdata
    }
    
    mapdata$mapColumn<-eval(parse(text=paste("mapdata$",mapColumn,sep="")))
    
    #replace NAs
    mapColumn<-mapdata$mapColumn
    replaceNAs(listColumns = named.list("mapColumn"))
    mapdata$mapColumn<-mapColumn
    
    #set breakpoints
    cls<-unique(mapBreaks(mapdata$mapColumn,siteAttrColors)$brks)
    cls<-round(cls[2:length(cls)],siteAttrClassRounding)
    
    #size and color
    sze<-rep(0.45,length(cls))*siteAttr_mapPointSize
    color<-siteAttrColors[1:length(cls)]
    cbckgrd <- siteAttrMapBackground
    
    # Symbol type filled circle
    pnch <- siteAttr_mapPointStyle
    
    par(mfrow=c(1,1), pch=1)    # 1 plots on one page
    
    plot(GeoLines,lwd=0.1,xlim=lon_limit,ylim=lat_limit,col=1,bg=cbckgrd)
    title(strTitle, cex.main =siteAttrTitleSize)
    
    map1 <- mapdata[(mapdata$mapColumn <= cls[1]), ]
    Lat<- map1$xlat
    Lon<- map1$xlon
    plotloc <- data.frame(Lat,Lon)
    points(plotloc$Lon, plotloc$Lat, pch=pnch[1], col=color[1], cex=sze[1])  
    
    strLegend<-paste(round(min(mapdata$mapColumn),siteAttrClassRounding)," to ",cls[1],sep="")
    
    for (k in 1:(length(cls)-1)) {
      map1 <- mapdata[(mapdata$mapColumn > cls[k] & mapdata$mapColumn <= cls[k+1]), ]
      Lat<- map1$xlat
      Lon<- map1$xlon
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch, col=color[k+1], cex=sze[k+1]) 
      
      if (k!=(length(cls)-1)){
        strlegend<-paste(cls[k]," to ",cls[k+1],sep="")
      }else{
        strlegend<-paste(cls[k]," to ",round(max(mapdata$mapColumn),siteAttrClassRounding),sep="")
      }
      strLegend<-c(strLegend,strlegend)
    }
    
    map1 <- mapdata[(mapdata$mapColumn > cls[(length(cls)-1)]), ]
    Lat<- map1$xlat
    Lon<- map1$xlon
    plotloc <- data.frame(Lat,Lon)
    points(plotloc$Lon, plotloc$Lat, pch=pnch, col=color[length(cls)], cex=sze[length(cls)])  
    
    
    legend("bottomleft",strLegend,
           bg=siteAttrMapBackground, bty="o",pch = siteAttr_mapPointStyle, 
           pt.cex = sze, col=color,title = unitAttr,cex=siteAttrLegendSize)
    
  }#if attr selected in shiny
  
  
}#end function
