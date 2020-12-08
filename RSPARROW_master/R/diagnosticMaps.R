#'@title diagnosticMaps
#'@description Creates diagnostic maps of residuals and site attributes and saves them to 
#'            ~/estimate/(run_id)_diagnostic_plots.pdf. \\cr \\cr
#'Executed By: \\itemize\{\\item diagnosticPlotsNLLS.R
#'             \\item diagnosticPlotsValidate.R\} \\cr
#'Executes Routines: unPackList.R \\cr
#'@param mapColumn character string indicating column of data to be mapped
#'@param mapdata input data.frame with lat, long, and column to be mapped
#'@param GeoLines Optional geospatial shape file for overlay of lines on output maps
#'@param map.list character string indicating whether over/under predictions 
#'                ("threshold") are to be mapped or all predictions ("all"). Value 
#'                c("threshold","all")
#'@param strTitle character string for plot title
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, 
#'                          CRStext, convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)


diagnosticMaps<-function(mapColumn,mapdata,GeoLines,
                         map.list,strTitle,mapping.input.list,sitedata){
  
  
  # Setup variable lists 
  # create global variable from list names (mapping.input.list)
  unPackList(lists = list(mapping.input.list = mapping.input.list),
             parentObj = list(NA)) 
  
  #get data
  mapdata <- mapdata
  mapColumnName<-mapColumn 
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
  
  #create function to make a vector of classes for legend
  makeAESvector<-function(mapdata, values, breaks,include = "all"){
    #first break
    if (include %in% c("all","first")){
      colData<-ifelse(mapdata$mapColumn<=breaks[1],values[1],NA)
    }else{
      colData<-rep(NA,nrow(mapdata))
    }
    for (k in 1:(length(breaks)-1)) {
      colData<-ifelse(mapdata$mapColumn > breaks[k] & mapdata$mapColumn <= breaks[k+1],
                      values[k+1],
                      colData)
    }
    #last break
    if (include %in% c("all","last")){
      colData<-ifelse(mapdata$mapColumn>breaks[(length(breaks)-1)],values[length(breaks)],colData)
    }
    return(colData)
  } 
  
  #point size and color  
  if (enable_plotlyMaps=="no"){
  sze <- residualPointSize_breakpoints*residualPointSize_factor  # Symbol sizes
  }else{
    sze <- residualPointSize_breakpoints*residualPointSize_factor*10  # Symbol sizes
  }
  
  color <- residualColors  
  uniqueColsleaf<-colorNumeric(color, 1:length(color))
  cbckgrd <- residualMapBackground
  
  
  if (enable_plotlyMaps=="no"){
    pnch <- residualPointStyle
    par(mfrow=c(1,1))    # 1 plots on one page
    
    p <- ggplot() +
      geom_sf(data = GeoLines, size = 0.1, fill = cbckgrd, colour ="black") +
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_blank())
    
  }else{#plotly
    pnch<-sapply(residualPointStyle, function(x) as.character(pchPlotlyCross[pchPlotlyCross$pch==x,]$plotly))

    

    #ititialize text strings for plotly
    markerText<-paste("~paste('</br> Lat: ',Lat,
    '</br> Lon: ',Lon,
    '</br>",mapColumnName,": ',
    round(",mapColumn,",siteAttrClassRounding)")

    plotLocStr<-paste0("plotloc <- data.frame(Lat,Lon, ",mapColumn," = map1$",mapColumn)
    
    markerText<-addMarkerText(markerText,add_plotlyVars,mapdata, sitedata)$markerText
    mapdata<-addMarkerText(markerText,add_plotlyVars, mapdata,sitedata)$mapData
    
    if (!is.na(add_plotlyVars[1])){
      add_plotlyVars<-as.character(ifelse(add_plotlyVars=="waterid","waterid_for_RSPARROW_mapping",add_plotlyVars))
      
      #add attributes to markerText
      for (m in add_plotlyVars){
        plotLocStr<-paste0(plotLocStr,",",m," = map1$",m)
      }
    }
    
    #wrap up text strings
    plotLocStr<-paste0(plotLocStr,")")
  
    #plotly plot
    p<-plot_ly() %>%
      layout(
        showlegend =TRUE,
        xaxis = list(range = lon_limit,
                     showticklabels= TRUE,
                     title = "Longitude"),
        yaxis = list(range = lat_limit,
                     showticklabels = TRUE,
                     title = "Latitude")) %>%
      add_sf(data = GeoLines,  mode = "lines", type = "scatter",
             stroke = I("black"),color = I(cbckgrd),
             name = LineShapeGeo)
  }
  
  
  if ("threshold-above" %in% map.list){   
   # par(mfrow=c(1,1), pch=16)    # 1 plots on one page
    #subset data
    above <- eval(parse(text=paste0("mapdata[(mapdata$",mapColumn,"<",threshold,"),]")))  # over predictions (neg residuals)
    nabove <- eval(parse(text=paste0("length(above$",mapColumn,")")))

    #for below threshold
    strTitle2<-paste(strTitle," - Over Predictions - n=",nabove)
    if (enable_plotlyMaps=="yes"){
    p <- p %>% layout(title = strTitle2)  
    } 
    
    
    map1 <- eval(parse(text=paste0("mapdata[(mapdata$",mapColumn,"<= cls[1]), ]")))
    Lat<- map1$xlat
    Lon<- map1$xlon
    
 
    if (enable_plotlyMaps=="yes"){#plotly
      
      eval(parse(text = plotLocStr))
      #update markerList for marker styling
      markerList<-paste0("list(symbol = pnch[1], size = sze[1],")
      if (regexpr("open",pnch)>0){
        markerList1<-paste0(markerList,"color = uniqueColsleaf(1))")
      }else{
        markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(1)),color = uniqueColsleaf(1))")
      }
      p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter",
                           mode = "markers",#color = I(color[1]),
                           #marker = list(symbol = pnchPlotly[1],size = sze[1]),
                           marker = eval(parse(text = markerList1)),
                           name = paste0("< ",cls[1]),
                           hoverinfo = 'text',
                           text = eval(parse(text = markerText)))
    }
    
    
    strLegend<-paste0("< ",cls[1])
    
    for (k in 1:3) {
      map1 <- eval(parse(text=paste0("mapdata[(mapdata$",mapColumn," > cls[k] & mapdata$",mapColumn,"<= cls[k+1]), ]")))
      Lat<- map1$xlat
      Lon<- map1$xlon

      strlegend<-paste0(cls[k]," to ",cls[k+1])
      strLegend<-c(strLegend,strlegend)
      
      if (enable_plotlyMaps=="yes"){#plotly
        
        eval(parse(text = plotLocStr))
        #update markerList for marker styling
        markerList<-paste0("list(symbol = pnch[k+1], size = sze[k+1],")
        if (regexpr("open",pnch)>0){
          markerList1<-paste0(markerList,"color = uniqueColsleaf(k+1))")
        }else{
          markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(k+1)),color = uniqueColsleaf(k+1))")
        }
        p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter", 
                             mode = "markers",#color = I(color[k+1]),
                             #marker = list(symbol = pnchPlotly[k+1],size = sze[k+1]),
                             marker = eval(parse(text = markerList1)),
                             name = strlegend,
                             hoverinfo = 'text',
                             text = eval(parse(text = markerText)))
      }
      
      
      
    }
    
    if (enable_plotlyMaps=="no"){
      
      mapdata$mapColumn<-eval(parse(text = paste0("mapdata$",mapColumn)))
      map1<-mapdata[mapdata$mapColumn<=cls[4],]
      
      #create vector of classes for legend
      map1$cls<-makeAESvector(map1,values = seq(1,4,1), breaks = cls[1:4], include = "first")
      
      #make sf object
      map1<-st_as_sf(map1,coords = c("xlon", "xlat"), crs = CRStext)

      p<-p +
        geom_sf(data = map1,
                aes(colour = as.factor(cls), size = as.factor(cls), shape = as.factor(cls)), 
                show.legend = TRUE) +
        coord_sf(xlim = lon_limit, ylim = lat_limit, crs = CRStext) +
        scale_colour_manual(values = residualColors[1:4],
                            labels = strLegend[1:4],
                            name = "Over Predictions") +
        scale_shape_manual(values = pnch[1:4],
                           labels = strLegend,
                           name = "Over Predictions") +
        scale_size_manual(values = sze[1:4]*residualPointSize_factor,
                          labels = strLegend[1:4],
                          name = "Over Predictions") +
        ggtitle(paste0(mapColumnName,"\n",strTitle2)) +
        theme(plot.title = element_text(hjust = 0.5,size =residualTitleSize, face = 'bold'),
              legend.position='bottom',
              legend.justification = 'left',
              legend.text = element_text(size = 24*residualLegendSize),
              legend.title = element_text(size = 26*residualLegendSize,face ='bold'),
              legend.background = element_rect(fill=residualMapBackground),
              legend.key.size = unit(residualLegendSize, 'cm'),
              legend.key = element_rect(fill = residualMapBackground)) +
        guides(col = guide_legend(ncol=1), size = "legend", shape = "legend")

      
    }
      return(p)
    
  }else if ("threshold-below" %in% map.list){ 
    below <- eval(parse(text=paste0("mapdata[(mapdata$",mapColumn,">",threshold,"),]")))
    nbelow <- eval(parse(text=paste0("length(below$",mapColumn,")")))

    if (enable_plotlyMaps=="yes"){
#plotly
 #plotly plot
      p<-plot_ly() %>%
        layout(
          showlegend =TRUE,
          xaxis = list(range = lon_limit,
                       showticklabels= TRUE,
                       title = "Longitude"),
          yaxis = list(range = lat_limit,
                       showticklabels = TRUE,
                       title = "Latitude")) %>%
        add_sf(data = GeoLines,  mode = "lines", type = "scatter",
               stroke = I("black"),color = I(cbckgrd),
               name = LineShapeGeo)
    }   

    #for above threshold
    strTitle2<-paste(strTitle," - Under Predictions - n=",nbelow)
    if (enable_plotlyMaps=="yes"){
      p <- p %>% layout(title = strTitle2)  
    }
 
    strLegend<-vector('character')

    for (k in 4:7) {
      map1 <- eval(parse(text=paste0("mapdata[(mapdata$",mapColumn," > cls[k] & mapdata$",mapColumn," <= cls[k+1]), ]")))
      Lat<- map1$xlat
      Lon<- map1$xlon

      if (k!=7){
        strlegend<-paste0(cls[k]," to ",cls[k+1])
      }else{
        strlegend<-paste0("> ",cls[k])
      }
      strLegend<-c(strLegend,strlegend)
      
      if (enable_plotlyMaps=="yes"){
       #plotly
        eval(parse(text = plotLocStr))
        #update markerList for marker styling
        markerList<-paste0("list(symbol = pnch[k+1], size = sze[k+1],")
        if (regexpr("open",pnch)>0){
          markerList1<-paste0(markerList,"color = uniqueColsleaf(k+1))")
        }else{
          markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(k+1)),color = uniqueColsleaf(k+1))")
        }
        p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter", 
                             mode = "markers",#color = I(color[k+1]),
                             #marker = list(symbol = pnchPlotly[k+1],size = sze[k+1]),
                             marker = eval(parse(text = markerList1)),
                             name = strlegend,
                             hoverinfo = 'text',
                             text = eval(parse(text = markerText)))
      }
      
      
      
    }
    

    map1 <- eval(parse(text=paste0("mapdata[(mapdata$",mapColumn," > cls[7]), ]")))
    Lat<- map1$xlat
    Lon<- map1$xlon

    if (enable_plotlyMaps=="yes"){
#plotly
      
      eval(parse(text = plotLocStr))
      #update markerList for marker styling
      markerList<-paste0("list(symbol = pnch[8], size = sze[8],")
      if (regexpr("open",pnch)>0){
        markerList1<-paste0(markerList,"color = uniqueColsleaf(8))")
      }else{
        markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(8)),color = uniqueColsleaf(8))")
      }
      p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter",
                           mode = "markers",#color = I(color[8]),
                           #marker = list(symbol = pnchPlotly[8],size = sze[8]),
                           marker = eval(parse(text = markerList1)),
                           name = paste0("> ",cls[7]),
                           hoverinfo = 'text',
                           text = eval(parse(text = markerText)))
    }

    if (enable_plotlyMaps=="no"){
      mapdata$mapColumn<-eval(parse(text = paste0("mapdata$",mapColumn)))
      map1<-mapdata[mapdata$mapColumn>cls[4],]

      #create vector of classes for legend
      map1$cls<-makeAESvector(map1,values = seq(4,8,1), breaks = cls[4:8], include = "last")

      map1<-st_as_sf(map1,coords = c("xlon", "xlat"), crs = CRStext)

      p<-p +
        geom_sf(data = map1,
                aes(colour = as.factor(cls), size = as.factor(cls), shape = as.factor(cls)), 
                show.legend = TRUE) +
        coord_sf(xlim = lon_limit, ylim = lat_limit, crs = CRStext) +
        scale_colour_manual(values = residualColors[5:8],
                            labels = strLegend[1:4],
                            name = "Under Predictions") +
        scale_shape_manual(values = pnch[5:8],
                           labels = strLegend[1:4],
                           name = "Under Predictions") +
        scale_size_manual(values = sze[5:8]*residualPointSize_factor,
                          labels = strLegend[1:4],
                          name = "Under Predictions") +
        ggtitle(paste0(mapColumnName,"\n",strTitle2)) +
        theme(plot.title = element_text(hjust = 0.5,size =residualTitleSize, face = 'bold'),
              legend.position='bottom',
              legend.justification = 'left',
              legend.text = element_text(size = 24*residualLegendSize),
              legend.title = element_text(size = 26*residualLegendSize,face ='bold'),
              legend.background = element_rect(fill=residualMapBackground),
              legend.key.size = unit(residualLegendSize, 'cm'),
              legend.key = element_rect(fill = residualMapBackground)) +
        guides(col = guide_legend(ncol=1), size = "legend", shape = "legend")
      
      
    }
      return(p)
    

  }#end if threshold map    
  
  if ("all" %in% map.list){
    #for all cls
    
    if (enable_plotlyMaps=="yes"){
#plotly

      #plotly plot
      p<-plot_ly() %>%
        layout(
          showlegend =TRUE,
          xaxis = list(range = lon_limit,
                       showticklabels= TRUE,
                       title = "Longitude"),
          yaxis = list(range = lat_limit,
                       showticklabels = TRUE,
                       title = "Latitude")) %>%
        add_sf(data = GeoLines,  mode = "lines", type = "scatter",
               stroke = I("black"),color = I(cbckgrd),
               name = LineShapeGeo)
    }   
    

    strTitle2<-strTitle
    if (enable_plotlyMaps=="yes"){
      p <- p %>% layout(title = strTitle2)  
    }
    
    
    map1 <- eval(parse(text=paste0("mapdata[(mapdata$",mapColumn," <= cls[1]), ]")))
    Lat<- map1$xlat
    Lon<- map1$xlon

    strLegend<-paste0("< ",cls[1])
    
    if (enable_plotlyMaps=="yes"){#plotly
      eval(parse(text = plotLocStr))
      #update markerList for marker styling
      markerList<-paste0("list(symbol = pnch[1], size = sze[1],")
      if (regexpr("open",pnch)>0){
        markerList1<-paste0(markerList,"color = uniqueColsleaf(1))")
      }else{
        markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(1)),color = uniqueColsleaf(1))")
      }
      
      p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter", 
                           mode = "markers",
                           marker = eval(parse(text = markerList1)),
                           name = strLegend,
                           hoverinfo = 'text',
                           text = eval(parse(text = markerText)))
    }
    
    for (k in 1:7) {
      map1 <- eval(parse(text=paste0("mapdata[(mapdata$",mapColumn," > cls[k] & mapdata$",mapColumn," <= cls[k+1]), ]")))
      Lat<- map1$xlat
      Lon<- map1$xlon

      if (k!=7){
        strlegend<-paste0(cls[k]," to ",cls[k+1])
      }else{
        strlegend<-paste0("> ",cls[k])
      }
      strLegend<-c(strLegend,strlegend)
    
    
    if (enable_plotlyMaps=="yes"){
#plotly
      eval(parse(text = plotLocStr))
      #update markerList for marker styling
      markerList<-paste0("list(symbol = pnch[k+1], size = sze[k+1],")
      if (regexpr("open",pnch)>0){
        markerList1<-paste0(markerList,"color = uniqueColsleaf(k+1))")
      }else{
        markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(k+1)),color = uniqueColsleaf(k+1))")
      }
      p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter", 
                           mode = "markers",
                           marker = eval(parse(text = markerList1)),
                           name = strlegend,
                           hoverinfo = 'text',
                           text = eval(parse(text = markerText)))
    }
    }
    map1 <- eval(parse(text=paste0("mapdata[(mapdata$",mapColumn," > cls[7]), ]")))
    Lat<- map1$xlat
    Lon<- map1$xlon

    if (enable_plotlyMaps=="no"){

      mapdata$mapColumn<-eval(parse(text = paste0("mapdata$",mapColumn)))
      map1<-mapdata
      
      #create vector of classes for legend
      map1$cls<-makeAESvector(map1,values = seq(1,8,1), breaks = cls, include = "all")

      map1<-st_as_sf(map1,coords = c("xlon", "xlat"), crs = CRStext)
      # save(list = c("mapdata","CRStext","residualColors","strLegend",
      #               "strTitle2","mapColumnName","residualTitleSize","residualLegendSize",
      #               "residualMapBackground","pnch","sze",
      #               "residualPointSize_factor","GeoLines","mapColumn","cls"),file = "D:/mapdata")
      p<-p +
        geom_sf(data = map1,
                aes(colour = as.factor(cls), size = as.factor(cls), shape = as.factor(cls)), 
                show.legend = TRUE) +
        coord_sf(xlim = lon_limit, ylim = lat_limit, crs = CRStext) +
        scale_colour_manual(values = residualColors[1:8],
                            labels = strLegend[1:8],
                            name = "Over Predictions") +
        scale_shape_manual(values = pnch[1:8],
                           labels = strLegend,
                           name = "Over Predictions") +
        scale_size_manual(values = sze[1:8]*residualPointSize_factor,
                          labels = strLegend[1:8],
                          name = "Over Predictions") +
        ggtitle(paste0(mapColumnName,"\n",strTitle2)) +
        theme(plot.title = element_text(hjust = 0.5,size =residualTitleSize, face = 'bold'),
              legend.position='bottom',
              legend.justification = 'left',
              legend.text = element_text(size = 24*residualLegendSize),
              legend.title = element_blank(),
              legend.background = element_rect(fill=residualMapBackground),
              legend.key.size = unit(residualLegendSize, 'cm'),
              legend.key = element_rect(fill = residualMapBackground)) +
        guides(col = guide_legend(ncol=1), size = "legend", shape = "legend")
      

    }
      return(p)
    
    
    
    
  }#end if all  
  
  
}#end function
