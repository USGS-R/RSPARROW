#'@title fixDupLatLons
#'@description Adds a small random increment of decimal degrees to the latitude and longitude 
#'            for monitoring sites with duplicate values. \\cr \\cr
#'Executed By: diagnosticSpatialAutoCorr.R \\cr
#'@param latLon numeric vector of latitude or longitude values
#'@return `x` latitude or longitude vector with small random increment added to duplicate 
#'            latitudes and longitudes



fixDupLatLons <- function(latLon){
  
  
  xd <- duplicated(latLon)

  # Generate random values for each latLon (won't use them all)
  random_vals_to_add <- sapply(seq_along(latLon), function(x) {
    runif(1,min=0.000001,max=0.000009)
  })
  # Only add random values if there are duplicates
  if(any(xd)) {
    latLon[xd] <- latLon[xd] + random_vals_to_add[xd]
  }
  
  return(latLon)
  
}#end function

