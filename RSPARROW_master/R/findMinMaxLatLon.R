#'@title findMinMaxLatLon
#'@description Identifies the minimum and maximum values of the calibration station latitudes 
#'            and longitudes, which is output to the Console, and used where the control settings 'lat_limit' 
#'            and 'lon_limit' are equal to 'NA' \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: named.list.R \\cr
#'@param sitedata Sites selected for calibration using `subdata[(subdata$depvar > 0
#'                & subdata$calsites==1), ]`. The object contains the dataDictionary 
#'                ‘sparrowNames’ variables, with records sorted in hydrological 
#'                (upstream to downstream) order (see the documentation Chapter 
#'                sub-section 5.1.2 for details)
#'@param mapping.input.list Named list of sparrow_control settings for mapping: lat_limit, 
#'                          lon_limit, master_map_list, lineShapeName, lineWaterid, 
#'                          polyShapeName, ployWaterid, LineShapeGeo, LineShapeGeo, CRStext, 
#'                          convertShapeToBinary.list, map_siteAttributes.list, 
#'                          residual_map_breakpoints, site_mapPointScale, 
#'                          if_verify_demtarea_maps
#'@return `sitegeolimits` named list of minimum and maximum values of the calibration station 
#'            latitudes and longitudes



findMinMaxLatLon <- function(sitedata,mapping.input.list) {
  
  
  lat <- ifelse(sitedata$lat == 0,NA,sitedata$lat)
  lon <- ifelse(sitedata$lon == 0,NA,sitedata$lon)
  latmax <- max(lat,na.rm=TRUE)
  latmin <- min(lat,na.rm=TRUE)
  lonmax <- max(lon,na.rm=TRUE)
  lonmin <- min(lon,na.rm=TRUE)
  sitegeolimits <- named.list(latmax,latmin,lonmax,lonmin)
  
  if (is.na(lat_limit)){
    mapping.input.list$lat_limit<-c(latmin-2,latmax+2)
    assign("mapping.input.list",mapping.input.list,envir = .GlobalEnv)
    assign("lat_limit",c(latmin-2,latmax+2),envir = .GlobalEnv)
  }
  if (is.na(lon_limit)){
    mapping.input.list$lon_limit<-c(lonmin-2,lonmax+2)
    assign("mapping.input.list",mapping.input.list,envir = .GlobalEnv)
    assign("lon_limit",c(lonmin-2,lonmax+2),envir = .GlobalEnv)
  }
  
  
  return(sitegeolimits)
  
}#end function

