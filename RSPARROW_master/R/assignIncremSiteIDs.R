#'@title assignIncremSiteIDs
#'@description Assigns the nearest downstream site ID (staidseq variable based on staid 
#'            variable) to reaches in the incremental areas between monitoring sites by climbing network structure 
#'            (reaches sorted by HYDSEQ) \\cr \\cr
#'Executed By: \\itemize\{\\item selectCalibrationSites.R
#'             \\item selectValidationSites.R
#'             \\item setNLLSWeights.R\} \\cr
#'Executes Routines: sites_incr.for \\cr
#'@param minnum user selected minimum number of reaches between sites
#'@param staid reach site IDs (non-zero for reaches with selected monitoring sites)
#'@param waterid reach ID number
#'@param tnode reach to (downstream) node
#'@param fnode reach from (upstream) node
#'@return `rchstaid`  integer vector site IDs assigned contiguously to upstream incremental 
#'            reaches



assignIncremSiteIDs <- function(minnum,staid,waterid,tnode,fnode) { 
  
  
  numrchs <- length(tnode)
  nnode <- max(tnode,fnode)
  rchstaid <- matrix(0,nrow=numrchs,ncol=1)
  
  return_data <- .Fortran('sites_incr',
                          numrchs=as.integer(numrchs),
                          nnode=as.integer(nnode),
                          minnum=as.integer(minnum),
                          fnode=as.integer(fnode),
                          tnode=as.integer(tnode),
                          staid=as.integer(staid),
                          waterid=as.integer(waterid),
                          rchstaid=as.integer(rchstaid),PACKAGE='sites_incr')
  rchstaid <- return_data$rchstaid
  
  
  return(rchstaid)   
  
}#end function
