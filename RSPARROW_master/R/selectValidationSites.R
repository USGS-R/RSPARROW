#'@title selectValidationSites
#'@description Identifies the validation sites according to user-specified control settings 
#'            (section 6 of the control script).  \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'Executes Routines: \\itemize\{\\item assignIncremSiteIDs.R
#'             \\item named.list.R
#'             \\item unPackList.R\} \\cr
#'@param iseed User specified initial seed for the bootstraps from sparrow_control
#'@param pvalidate numeric control setting indicating a percentage of calibration sites to 
#'       select for validation or if equal to 0 indicates that the user defined valsites variable should be 
#'       used to select sites for validation. For more details see documentation Section 4.4.6
#'@param subdata data.frame input data (subdata)
#'@param minimum_reaches_separating_sites number indicating the minimum number of reaches 
#'       separating sites
#'@param data_names data.frame of variable metadata from data_Dictionary.csv file
#'@return `Vsites.list` named list of sites for validation



selectValidationSites <- function(iseed,pvalidate,subdata,minimum_reaches_separating_sites,data_names) {
  
  
  ################################################
  # create global variables for calculations
  unPackList(lists = list(datalstreq = data_names$sparrowName),
             parentObj = list(subdata = subdata)) 
  
  numrchs <- length(waterid)
  minnum <- minimum_reaches_separating_sites
  
  set.seed(iseed)
  nsamples <- round( (1-pvalidate) * max(staid))  # number of calibration sites
  rpick <- sample(rep(1:max(staid)),nsamples)
  tstaid <- staid
  staid <- numeric(numrchs)
  vstaid <- numeric(numrchs)
  vdepvar <- numeric(numrchs)
  vcalsites <- numeric(numrchs)
  nMon <- 0
  vic <- 0
  
  if(pvalidate > 0) { 
    message("   Random selection of validation sites applied.")
    
    # Default method using random selection of validation sites
    for (i in 1:length(depvar)) {
      if (tstaid[i] > 0) {
        iset <- 0
        for (j in 1:nsamples) {
          if (tstaid[i] == rpick[j]) {   
            if(depvar[i] > 0 & calsites[i]==1) {          # accepted site with non-zero load
              nMon <- nMon+1
              iset <- 1
              staid[i] <- nMon
            }
          } 
        }
        if (iset == 0) {   # site not picked, use for validation site
          staid[i] <- 0
          if(depvar[i] > 0 & calsites[i]==1) {
            vic <- vic+1
            vdepvar[i] <- depvar[i]    # store validation dependent variable value
            vstaid[i] <- vic
            vcalsites[i]<- calsites[i]
            depvar[i] <- 0
          }
        }
      }
    }
    
  } else {
    
    message("   User-defined validation site selection executed.")
    
    # User-define validation sites (pvalidate==0)
    for (i in 1:length(depvar)) {
      if(valsites[i]==1) {
        if(depvar[i] > 0) {
          staid[i] <- 0
          vic <- vic+1
          vdepvar[i] <- depvar[i]    # store validation dependent variable value
          vstaid[i] <- vic
          depvar[i] <- 0
        } else {
          staid[i] <- 0   # eliminate site from tagging as possible calibration site
        }
      } 
      if(calsites[i]==1 & valsites[i] != 1) {
        if(depvar[i] > 0) {
          nMon <- nMon+1
          staid[i] <- nMon
        } else {
          staid[i] <- 0
        }
      }
    }
    
  }  # end validation site selection method
  
  
  # obtain final 'staidseq' based on final site selection
  sid <- staid  
  staidseq <- assignIncremSiteIDs(minnum,sid,waterid,tnode,fnode)   # call function
  
  # obtain final 'vstaidseq' based on final site selection
  sid <- vstaid  
  vstaidseq <- assignIncremSiteIDs(minnum,sid,waterid,tnode,fnode)   # call function
  vdepvar <- ifelse(vdepvar > 0 & vstaidseq != sid,0,vdepvar) # eliminate sites with < min. number of separation reaches
  ncheck <- sum(ifelse(vdepvar == 0,0,1))
  
  # obtain final station count and loads, if any sites eliminated because < min. number separation reaches
  if(ncheck < vic) {
    # Assign validation sequence numbers
    vic <- 0
    for (i in 1:numrchs) {
      if (is.na(vstaid[i]) | vdepvar[i] == 0) {
        vstaid[i] <- 0
      } else {
        vic<-vic+1
        vstaid[i] <- vic
      }
    }
    # obtain final 'vstaidseq' (with updated vic sequence number) based on final validation site numbering
    sid <- vstaid 
    minnum <- minimum_reaches_separating_sites
    vstaidseq <- assignIncremSiteIDs(minnum,sid,waterid,tnode,fnode)   # call function
  }
  
  Vsites.list <- named.list(waterid,staid,vstaid,depvar,vdepvar,nMon,vic,staidseq,vstaidseq)
  
  
  return(Vsites.list)
  
}#end function


