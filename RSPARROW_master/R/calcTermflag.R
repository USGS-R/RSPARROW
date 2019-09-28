#'@title calcTermflag
#'@description Calculates the system variable termflag, based on reach navigation variables \\cr \\cr
#'Executed By: createVerifyReachAttr.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item unPackList.R\} \\cr
#'@param data1 input data (data1)
#'@return `outdata`  data.frame with waterid and termflag



calcTermflag<-function(data1){
  
  # transfer required variables to global environment from data1
  unPackList(lists = list(datalstCheck = as.character(getVarList()$varList)),
             parentObj = list(data1 = data1))
  
  #create sequence variable
  SEQ<-data.frame(seqvar = seq(1,nrow(data1),1))
  #add seqvar to tnode and fnode
  tnode<-as.data.frame(cbind(SEQ,tnode))
  fnode<-as.data.frame(cbind(SEQ,fnode))
  
  #sort data
  tnode<-tnode[order(tnode$tnode),]
  fnode<-fnode[order(fnode$fnode),]
  
  #save rownumbers
  fnode$Row<-seq(1,nrow(fnode),1)
  tnode$Row<-seq(1,nrow(tnode),1)
  
  tnode<-data.frame(tnode = unique(tnode$tnode))
  
  #save as termflag in data1
  ifterm<-na.omit(tnode[which(!tnode$tnode %in% fnode$fnode),])
  data1$termflag<-ifelse(data1$tnode %in% ifterm,1,0)
  
  outdata<-data1[,which(names(data1) %in% c("waterid","termflag"))]
  
  return(outdata)
  
  
  
}#end function
