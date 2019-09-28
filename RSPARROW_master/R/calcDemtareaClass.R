#'@title calcDemtareaClass
#'@description Obtains monitoring station classification (deciles) based on total drainage 
#'            area for each site \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'@param demtarea total drainage area for each site
#'@return `demtarea.class`  a numeric vector indicating decile class labeled with the total 



calcDemtareaClass <- function(demtarea) {
  
  vvar <- demtarea
  iprob<-10
  chk <- unique(quantile(vvar, probs=0:iprob/iprob))
  chk1 <- 11 - length(chk)
  if(chk1 == 0) {
    darea <- quantile(vvar, probs=0:iprob/iprob)
    qvars <- as.integer(cut(vvar, quantile(vvar, probs=0:iprob/iprob), include.lowest=TRUE))
    demtarea.class <- numeric(length(qvars))
    for (k in 1:10) {
      for (i in 1:length(qvars)){
        if(qvars[i] == k) {
          demtarea.class[i] <- round(darea[k+1],digits=0)
        }
      }
    }
  }
  
  
  return(demtarea.class)
  
}#end function
