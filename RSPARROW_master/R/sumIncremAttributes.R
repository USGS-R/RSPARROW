#'@title sumIncremAttributes
#'@description Sums the specified attribute over the incremental area of the calibration 
#'            sites, based on the unique station sequence number for each reach (staidseq).   [NOTE THAT THE 
#'            FUNCTION ARGUMENTS COULD USE EDITING TO USE THE UNIQUE NAMES RATHER THAN GENERIC NAMES) \\cr \\cr
#'Executed By: \\itemize\{\\item calcIncremLandUse.R
#'             \\item correlationMatrix.R\} \\cr
#'@param idseq staidseq or vstaidseq, integer vector site IDs assigned contiguously to 
#'       upstream incremental reaches
#'@param attrib specified attributes with length equal to number of reaches
#'@param attrib_name character string naming `attrib`
#'@return `siteiarea` dataframe with summed attribute



sumIncremAttributes <- function(idseq,attrib,attrib_name) { 
  
  xx <- data.frame(idseq,attrib)
  count<-ddply(xx,.(idseq), summarize, nirchs=length(idseq))      # get count for unique staids
  count <- count[-1,]  # delete first row
  
  siteiarea<-ddply(xx,.(idseq),summarize,tiarea=sum(attrib))    # sum attribute for unique staids
  siteiarea <- siteiarea[-1,]  # delete first row with "0" idseq
  colnames(siteiarea) <- c("idseq",attrib_name)
  
  
  
  return(siteiarea)
  
}#end function

############### EOF
