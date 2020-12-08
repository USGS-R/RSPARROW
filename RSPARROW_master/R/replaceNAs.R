#'@title replaceNAs
#'@description Replaces all NAs with 0's. Output is saved to parent.frame(). \\cr \\cr
#'Executed By: \\itemize\{\\item applyUserModify.R
#'             \\item mapSiteAttributes.R\} \\cr
#'@param listColumns a named list of variables in which to replace NAs.



replaceNAs<-function(listColumns){
  columnNames<-names(listColumns)
  for (i in 1:length(columnNames)) { 
    dname <- paste0(columnNames[i],"<-ifelse(is.na(",columnNames[i],"),0,",columnNames[i],")")
    eval(parse(text=dname),envir = parent.frame()) 
  }
}


