#'@title diagnosticMaps
#'@description Creates diagnostic maps of residuals and site attributes and saves them to 
#'            ~/estimate/(run_id)_diagnostic_plots.pdf. \\cr \\cr
#'Executed By: \\itemize\{\\item diagnosticPlotsNLLS.R
#'             \\item diagnosticPlotsValidate.R\} \\cr
#'Executes Routines: unPackList.R \\cr
#'@param mapColumn character string indicating column of data to be mapped
#'@param mapdata input data.frame with lat, long, and column to be mapped
#'@param GeoLines Optional geospatial shape file for overlay of lines on output maps
#'@param strTitle character string for plot title



diagnosticMaps<-function(mapColumn,mapdata,GeoLines,
                         map.list,strTitle,mapping.input.list){
  
  
  # Setup variable lists 
  # create global variable from list names (mapping.input.list)
  unPackList(lists = list(mapping.input.list = mapping.input.list),
             parentObj = list(NA)) 
  
  #get data
  mapdata <- mapdata
  
  #set up according to maptype
  if (regexpr("Resid",mapColumn,ignore.case = TRUE)>0 ){
    #set breakpoints
    if (is.na(residual_map_breakpoints) | length(residual_map_breakpoints)!=7){
      cls <- c(-2.5,-0.75,-0.25,0,0.25,0.75,2.5)  # Residual breakpoints
    }else{
      cls<-residual_map_breakpoints
    }
    
    #set threshold and data column
    threshold<-0
    
  }else{# Ratio breakpoints
    if (is.na(ratio_map_breakpoints) | length(ratio_map_breakpoints)!=7){
      cls <-  c(0.3,0.5,0.8,1,1.25,2,3.3)    # Residual breakpoints
    }else{
      cls<-ratio_map_breakpoints
    }
    
    
    #set threshold and data column
    threshold<-1
    
  }#end setup mapType    
  
  
  #point size and color  
  sze <- residualPointSize_breakpoints*residualPointSize_factor  # Symbol sizes
  color <- residualColors  
  cbckgrd <- residualMapBackground
  
  # Symbol types:
  pnch <- residualPointStyle
  
  if ("threshold" %in% map.list){   
    par(mfrow=c(1,1), pch=16)    # 1 plots on one page
    #subset data
    above <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn,"<",threshold,"),]",sep="")))  # over predictions (neg residuals)
    below <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn,">",threshold,"),]",sep="")))
    nabove <- eval(parse(text=paste("length(above$",mapColumn,")",sep="")))
    nbelow <- eval(parse(text=paste("length(below$",mapColumn,")",sep="")))
    
    #for below threshold
    plot(GeoLines,lwd=0.1,xlim=lon_limit,ylim=lat_limit,col=1,bg=cbckgrd)
    title(bquote(paste(.(strTitle)," - Over Predictions - n=",.(nabove))),cex.main=residualTitleSize)
    
    map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn,"<= cls[1]), ]",sep="")))
    Lat<- map1$xlat
    Lon<- map1$xlon
    plotloc <- data.frame(Lat,Lon)
    points(plotloc$Lon, plotloc$Lat, pch=pnch[1], col=color[1], cex=sze[1])  
    
    strLegend<-paste("< ",cls[1],sep="")
    
    for (k in 1:3) {
      map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[k] & mapdata$",mapColumn,"<= cls[k+1]), ]",sep="")))
      Lat<- map1$xlat
      Lon<- map1$xlon
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch[k+1], col=color[k+1], cex=sze[k+1])
      
      strlegend<-paste(cls[k]," to ",cls[k+1],sep="")
      strLegend<-c(strLegend,strlegend)
    }
    
    legend("bottomleft",strLegend,
           bg=cbckgrd, bty="o",pch = residualPointStyle[1:4], pt.cex = residualPointSize_breakpoints[1:4]*residualPointSize_factor, col=color[1:4]
           ,cex=residualLegendSize)
    
    
    #for above threshold
    plot(GeoLines,lwd=0.1,xlim=lon_limit,ylim=lat_limit,col=1,bg=cbckgrd)
    title(bquote(paste(.(strTitle)," - Under Predictions - n=",.(nbelow))),cex.main=residualTitleSize)
    
    map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[7]), ]",sep="")))
    Lat<- map1$xlat
    Lon<- map1$xlon
    plotloc <- data.frame(Lat,Lon)
    points(plotloc$Lon, plotloc$Lat, pch=pnch[8], col=color[8], cex=sze[8])  
    
    strLegend<-vector('character')
    
    for (k in 4:7) {
      map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[k] & mapdata$",mapColumn," <= cls[k+1]), ]",sep="")))
      Lat<- map1$xlat
      Lon<- map1$xlon
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch[k+1], col=color[k+1], cex=sze[k+1]) 
      
      if (k!=7){
        strlegend<-paste(cls[k]," to ",cls[k+1],sep="")
      }else{
        strlegend<-paste("> ",cls[k],sep="")
      }
      strLegend<-c(strLegend,strlegend)
    }
    
    legend("bottomleft",strLegend,
           bg=cbckgrd, bty="o",pch = residualPointStyle[5:8], pt.cex = residualPointSize_breakpoints[5:8]*residualPointSize_factor, 
           col=color[5:8],cex=residualLegendSize)
  }#end if threshold map    
  
  if ("all" %in% map.list){
    #for all cls
    par(mfrow=c(1,1), pch=16)    # 1 plots on one page
    
    plot(GeoLines,lwd=0.1,xlim=lon_limit,ylim=lat_limit,col=1,bg=cbckgrd)
    title(strTitle,cex.main=residualTitleSize)
    
    map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," <= cls[1]), ]",sep="")))
    Lat<- map1$xlat
    Lon<- map1$xlon
    plotloc <- data.frame(Lat,Lon)
    points(plotloc$Lon, plotloc$Lat, pch=pnch[1], col=color[1], cex=sze[1])  
    
    strLegend<-paste("< ",cls[1],sep="")
    
    for (k in 1:7) {
      map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[k] & mapdata$",mapColumn," <= cls[k+1]), ]",sep="")))
      Lat<- map1$xlat
      Lon<- map1$xlon
      plotloc <- data.frame(Lat,Lon)
      points(plotloc$Lon, plotloc$Lat, pch=pnch[k+1], col=color[k+1], cex=sze[k+1]) 
      
      if (k!=7){
        strlegend<-paste(cls[k]," to ",cls[k+1],sep="")
      }else{
        strlegend<-paste("> ",cls[k],sep="")
      }
      strLegend<-c(strLegend,strlegend)
    }
    
    map1 <- eval(parse(text=paste("mapdata[(mapdata$",mapColumn," > cls[7]), ]",sep="")))
    Lat<- map1$xlat
    Lon<- map1$xlon
    plotloc <- data.frame(Lat,Lon)
    points(plotloc$Lon, plotloc$Lat, pch=pnch[8], col=color[8], cex=sze[8])  
    
    
    legend("bottomleft",strLegend,
           bg=cbckgrd, bty="o",pch = residualPointStyle, 
           pt.cex = sze, col=color,cex=residualLegendSize)
    
  }#end if all  
  
  
}#end function
