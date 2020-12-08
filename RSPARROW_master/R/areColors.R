#'@title areColors
#'@description Tests color strings and indicate whether invalid (not supported by R) color 
#'            designations have been found \\cr \\cr
#'Executed By: getSpecialSett.R \\cr
#'@param strColors character vector of supported R color strings
#'@return `testCol`  logical TRUE/FALSE indicating whether or not invalid color strings were 
#'            found



areColors <- function(strColors) {
  #https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
  testCol<- sapply(strColors, function(X) {
    tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE)
  })
  #end sourced from https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
  testCol<-unique(testCol)
  if (length(testCol)==1){
    if (suppressWarnings(testCol)){
      testCol<-TRUE
    }else{
      testCol<-FALSE
    }
  }else{
    testCol<-FALSE
  }
  return(testCol)
}
