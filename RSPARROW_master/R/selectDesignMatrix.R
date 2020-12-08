#'@title selectDesignMatrix
#'@description Creates the 'dlvdsgn' matrix for use in model estimation and prediction, based 
#'            on the user-selected model parameters.  \\cr \\cr
#'Executed By: startModelRun.R \\cr
#'@param SelParmValues selected parameters from parameters.csv using condition 
#'       `ifelse((parmMax > 0 | (parmType=="DELIVF" & parmMax>=0)) & (parmMin<parmMax) & ((parmType=="SOURCE" & 
#'       parmMin>=0) | parmType!="SOURCE")`
#'@param betavalues data.frame of model parameters from parameters.csv
#'@param dmatrixin imported object from design_matrix.csv
#'@return `dlvdsgn` design matrix imported from design_matrix.csv for selected parameters



selectDesignMatrix <- function(SelParmValues,betavalues,dmatrixin){
  
  
  # prep for design matrix
  pselect <- SelParmValues$pselect
  srcselect <- ifelse(betavalues$parmType == "SOURCE" & pselect == 1,1,0)
  dlvselect <- ifelse(betavalues$parmType == "DELIVF" & pselect == 1,1,0)
  
  asrc <- sum(ifelse(betavalues$parmType == "SOURCE",1,0))   # total number of source variables
  adel <- sum(ifelse(betavalues$parmType == "DELIVF",1,0))   # total number of delivery variables
  ndeliv <- sum(dlvselect)
  nsrc <- sum(srcselect)
  sdmatrix <- logical(length=asrc)
  dmatrix <- logical(length=adel)
  sdmatrix <- ifelse(pselect[betavalues$parmType == "SOURCE"] == 1,TRUE,FALSE)
  dmatrix <- ifelse(pselect[betavalues$parmType == "DELIVF"] == 1,TRUE,FALSE)
  
  d2matrixin <- matrix(unlist(as.data.frame(dmatrixin)), ncol=adel, nrow=asrc)
  dlvdsgnO <- numeric(adel*asrc)
  k <- 0
  for (i in 1:asrc) {
    for (j in 1:adel) {
      k <- k+1
      dlvdsgnO[k] <- d2matrixin[i,j]
    }
  }
  
  # obtain matrix consistent with variable selections if delivery variables selected
  # Reduce size of dlvdsgnO to be consistent with variable selections
  
  dlvdsgn1 <- matrix(unlist(dlvdsgnO))
  if (ndeliv > 0) {
    dlvdsgn <- matrix(1, ncol=ndeliv, nrow=nsrc)
    isrc = 0
    jdel = 0
    jtot = 0
    for (i in 1:asrc){
      if(sdmatrix[i]){
        isrc=isrc+1
        for (j in 1:adel){
          jtot=jtot+1
          if(dmatrix[j]){
            jdel=jdel+1
            dlvdsgn[isrc,jdel] = dlvdsgn1[jtot]
          }
        }
        jdel=0
      } else {jtot=jtot+adel}
    }
  } else {
    dlvdsgn <- dlvdsgn1
  }
  
  
  return(dlvdsgn)
  
}#end function



