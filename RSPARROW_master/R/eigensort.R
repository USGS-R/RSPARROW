#'@title eigensort
#'@description Sorts eigenvectors and eigenvalues in decreasing order. \\cr \\cr
#'Executed By: estimateNLLSmetrics.R \\cr
#'@param eigs object containing eigenvalues and vectors from the base R `eigen()` function
#'@return `eigenout`  list object with eigenvalues and vectors sorted in decreasing order



eigensort <- function(eigs) {
  
  
  temp <- sort.int(eigs$values, index.return=TRUE, decreasing=TRUE)
  values <- temp$x
  n <- nrow(eigs$vectors)
  vectors <- matrix(0,nrow=n,ncol=n)
  for (i in 1:ncol(eigs$vectors)) {
    vectors[,i] <- eigs$vectors[,temp$ix[i]]
  }
  eigenout <- list("values"=values,"vectors"=vectors)
  
  
  return(eigenout)
  
}#end function
