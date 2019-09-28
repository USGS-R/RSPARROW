#'@title allowRemoveRow
#'@description Function to edit context menu for rhandsontable hottables in shiny ap, so that 
#'            the first row cannot be deleted. \\cr \\cr
#'Executed By: handsOnMod.R \\cr
#'@param hot shiny rhandsontable hot table used for user input 
#'@param allowRemove TRUE/FALSE indicates whether the user should have the option to remove 
#'       rows from a rhandsontable hottable in shiny.  If only 1 row in the table, then FALSE



allowRemoveRow <- function(hot, allowRemove) {
  
  
  if (is.null(hot$x$contextMenu$items))
    opts = list()
  else
    opts = hot$x$contextMenu$items
  
  add_opts = function(new, old, val = list()) {
    new_ = lapply(new, function(x) {
      if (grepl("^hsep", x) && !is.null(val))
        return(list(name = "---------"))
      else
        return(val)
    })
    names(new_) = new
    if (length(old) > 0) {
      modifyList(old, new_)
    } else {
      new_
    }
  }
  remove_opts = function(new) {
    add_opts(new, opts, val = NULL)
  }
  
  if (allowRemove){
    opts =  add_opts(c("hsep1", "row_above", "row_below", "remove_row"), opts)
  }else{
    opts =  remove_opts(c("remove_row"))
  }
  
  hot$x$contextMenu = list(items = opts)
  
  hot
}
