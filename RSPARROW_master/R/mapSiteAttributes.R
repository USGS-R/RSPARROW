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
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)
#'@param LineShapeGeo character string control settting indicating which binary map file to 
#'       load for the Geolines background layer
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@param Rshiny TRUE/FALSE indicating whether routine is being run from the Shiny app
#'@param mapColumn character string indicating column of data to be mapped
#'@param mapdata input data.frame with lat, long, and column to be mapped
#'@param GeoLines Optional geospatial shape file for overlay of lines on output maps
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
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
  
  # create global variable from list names (mapping.input.list)
  unPackList(lists = list(mapping.input.list = mapping.input.list),
             parentObj = list(NA)) 
  
  if (((input$var!="" |!is.na(attr)) & Rshiny)| !Rshiny){
      
    
      if (Rshiny){
      #get geoLines
      existGeoLines<-checkBinaryMaps(LineShapeGeo,path_gis,batch_mode)
      if (existGeoLines){
        load(paste0(path_gis,.Platform$file.sep,"GeoLines"))
      }   
        
      #set shiny variables
      enable_plotlyMaps<-as.character(input$enablePlotly)
      add_plotlyVars<-as.character(input$plotlyDrop)
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
      
      mapColumnName<-mapColumn 
      siteAttr<-eval(parse(text= paste0("data.frame(",mapColumn,"=sitedata$",mapColumn,")")))
      titleAttr<-data_names[which(data_names$sparrowNames==mapColumn),]
      unitAttr<-titleAttr$varunits
      titleAttr<-as.character(titleAttr$explanation)
      strTitle<-titleAttr
      
      xlat<-sitedata$lat
      xlon<-sitedata$lon
      
      mapdata<-data.frame(xlat,xlon,siteAttr)
      
    }else{ #not shiny
      #get data
      mapdata <- mapdata
      mapColumnName<-mapColumn 
    }
    
    #set map data
    mapdata$mapColumn<-eval(parse(text=paste0("mapdata$",mapColumn)))
    
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
    uniqueColsleaf<-colorNumeric(color, 1:length(color))
    

    #plot setup
    if (enable_plotlyMaps=="no" | enable_plotlyMaps=="static"){
      pnch <- siteAttr_mapPointStyle
    }else{#plotly/leaflet
      #set marker styling
      pnch<-as.character(pchPlotlyCross[pchPlotlyCross$pch==siteAttr_mapPointStyle,]$plotly)
      markerList<-paste0("list(symbol = pnch, size = sze[1]*10,")
      
      #ititialize text strings for plotly
      markerText<-"~paste('</br> Lat: ',Lat,
      '</br> Lon: ',Lon,
      '</br>',mapColumnName,' :',round(mapColumn,siteAttrClassRounding)"

      plotLocStr<-paste0("plotloc <- data.frame(Lat,Lon, mapColumn = map1$mapColumn")
      
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
      
      if (enable_plotlyMaps=="yes" | enable_plotlyMaps=="plotly"){
        #plotly plot
        p<-plot_ly() %>%
          layout(
            showlegend =TRUE,
            xaxis = list(range = lon_limit,
                         showticklabels= TRUE,
                         title = "Longitude"),
            yaxis = list(range = lat_limit,
                         showticklabels = TRUE,
                         title = "Latitude"),
            title = paste0(mapColumnName,"\n",unitAttr))
      }
    }#else plotly or leaflet
    
    #plotgeolines
      if (enable_plotlyMaps=="no" | enable_plotlyMaps=="static"){
        p <- ggplot() +
          geom_sf(data = GeoLines, size = 0.1, fill = siteAttrMapBackground, colour ="black") +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_blank())
      }else if (enable_plotlyMaps=="yes" | enable_plotlyMaps=="plotly"){
        p<-p %>%
          add_sf(data = GeoLines,  mode = "lines", type = "scatter",
                 stroke = I("black"),color = I(cbckgrd),
                 name = LineShapeGeo)
}

    
    map1 <- mapdata[(mapdata$mapColumn <= cls[1]), ]
    Lat<- map1$xlat
    Lon<- map1$xlon
    strLegend<-paste0(round(min(mapdata$mapColumn),siteAttrClassRounding)," to ",cls[1])
    
    if (enable_plotlyMaps=="yes" | enable_plotlyMaps=="plotly"){  
      eval(parse(text = plotLocStr))
      #update markerList for marker styling
      if (regexpr("open",pnch)>0){
        markerList1<-paste0(markerList,"color = uniqueColsleaf(1))")
      }else{
        markerList1<-paste0(markerList,"line = list(color = uniqueColsleaf(1)),color = uniqueColsleaf(1))")
      }
      
      #add first class
      p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter",
                           mode = "markers",
                           marker = eval(parse(text = markerList1)),
                           name = paste0(round(min(mapdata$mapColumn),siteAttrClassRounding)," to ",cls[1]),
                           hoverinfo = 'text',
                           text = eval(parse(text = markerText)))
    } 
      #middle classes
      for (k in 1:(length(cls)-1)) {
        map1 <- mapdata[(mapdata$mapColumn > cls[k] & mapdata$mapColumn <= cls[k+1]), ]
        Lat<- map1$xlat
        Lon<- map1$xlon
        
        if (k!=(length(cls)-1)){
          strlegend<-paste0(cls[k]," to ",cls[k+1])
        }else{
          strlegend<-paste0(cls[k]," to ",round(max(mapdata$mapColumn),siteAttrClassRounding))
        }
        strLegend<-c(strLegend,strlegend)
        
        if (enable_plotlyMaps=="yes" | enable_plotlyMaps=="plotly"){#plotly
          eval(parse(text = plotLocStr))
          #update markerList for marker styling
          if (regexpr("open",pnch)>0){
            markerList2<-paste0(markerList,"color = uniqueColsleaf(k+1))")
          }else{
            markerList2<-paste0(markerList,"line = list(color = uniqueColsleaf(k+1)),color = uniqueColsleaf(k+1))")
          }
            p <- p %>% add_trace(data = plotloc, x=~Lon, y = ~Lat, type = "scatter", 
                                 mode = "markers",#color = I(color[k+1]),
                                 marker = eval(parse(text = markerList2)),
                                 name = strlegend,
                                 hoverinfo = 'text',
                                 text = eval(parse(text = markerText)))
        }#end plotly
        
      } #for each middle class k 
      
      mapdata$Lat<- mapdata$xlat
      mapdata$Lon<- mapdata$xlon
      mapdata$mapColor<-ifelse(mapdata$mapColumn<=cls[1],col2hex(color)[1],NA)
      for (k in 1:(length(cls)-1)) {
        mapdata$mapColor<-ifelse(mapdata$mapColumn > cls[k] & mapdata$mapColumn <= cls[k+1],
                                 col2hex(color)[k+1],
                                 mapdata$mapColor)
      }
      mapdata$mapColor<-ifelse(is.na(mapdata$mapColor),col2hex(color)[length(color)],mapdata$mapColor)
      mapdata$mapColor<-factor(mapdata$mapColor, levels = col2hex(color))
      if (enable_plotlyMaps=="no" | enable_plotlyMaps=="static"){
        mapdata<-st_as_sf(mapdata,coords = c("xlon", "xlat"), crs = CRStext)
        
         p<-p +
          geom_sf(data = mapdata,
                  aes(colour = factor(mapColor, levels = col2hex(color))), 
                  size = siteAttr_mapPointSize, 
                  shape = siteAttr_mapPointStyle,
                     show.legend = TRUE) +
          coord_sf(xlim = lon_limit, ylim = lat_limit, crs = CRStext) +
           scale_colour_manual(values = color,
                               labels = strLegend,
                               name = unitAttr) +
           ggtitle(paste0(mapColumnName,"\n",unitAttr)) +
           theme(plot.title = element_text(hjust = 0.5,size =siteAttrTitleSize, face = 'bold'),
                 legend.position='bottom',
                 legend.justification = 'left',
                 legend.text = element_text(size = 24*siteAttrLegendSize),
                 legend.title = element_text(size = 26*siteAttrLegendSize,face ='bold'),
                 legend.background = element_rect(fill=siteAttrMapBackground),
                 legend.key.size = unit(siteAttrLegendSize, 'cm')) +
           guides(col = guide_legend(ncol=1))
      }else if (enable_plotlyMaps=="leaflet"){
        markerText<-gsub("~","",markerText)
        
        markerTextHTML<-paste0("~lapply(",markerText,",HTML)")
        
        mapdata<-st_as_sf(mapdata,coords = c("xlon", "xlat"), crs = 4326)
        
        p <- mapview(mapdata,
                     fill = F, homebutton = F, popup = NULL, legend = F, viewer.suppress = F) %>% 
          .@map %>% 
          clearMarkers() %>% 
          clearShapes() %>% 
          addCircleMarkers(
            data = mapdata,
            fillOpacity = 1,
            opacity = 1,
            weight = 1,
            radius = unique(sze),
            color = ~col2hex(mapColor),
            fill=TRUE,
            fillColor = ~col2hex(mapColor),
            label = eval(parse(text = markerTextHTML))
            #group = mapColor,
          ) %>% 
          addLegend("bottomleft", labels = strLegend, colors = col2hex(color),
                    title = paste0(mapColumnName,"\n",unitAttr), opacity = 1)
      }
      return(p)
  }#if attribute
}#end function