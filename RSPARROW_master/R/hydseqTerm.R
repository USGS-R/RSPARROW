#'@title hydseqTerm
#'@description flags all reaches upstream of the designated target reaches for execution of 
#'            source reduction scenarios \\cr \\cr
#'Executed By: predictScenariosPrep.R \\cr
#'Executes Routines: \\itemize\{\\item getVarList.R
#'             \\item unPackList.R\} \\cr
#'@param subdata data.frame input data (subdata)
#'@param select_targetReachWatersheds Indicate the watershed locations where the scenarios 
#'       will be applied. For more details see documenation Section 4.4.9.1
#'@return `data1` input data file with RSPARROW calculated hydseq values for the reaches 
#'            upstream of the designated target reach for source reduction scenarios



hydseqTerm<-function(subdata, select_targetReachWatersheds){
  
  
  #get data
  data1<-subdata
  
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
  
  #create list of all flowlines immediately downstram of given fnode
  dnstream_list_index<-with(fnode,aggregate(Row~fnode,FUN = function(x) min(x)))
  dnstream_list_index2<-with(fnode,aggregate(Row~fnode,FUN = function(x) max(x)))
  dnstream_list_index<-merge(dnstream_list_index,dnstream_list_index2, by ="fnode")
  names(dnstream_list_index)[2:3]<-c("start","end")
  dnstream_list<-data.frame(seqvar = fnode$seqvar)
  
  #obtain maximum fnode value, and unique fnode list
  maxfnode<-max(fnode$fnode)
  fnode<-data.frame(fnode = unique(fnode$fnode))
  
  #Create a list of all flowlines immediately upstream of a given fnode. 
  #Create the companion index that references the list by fnode.
  upstream_list_index<-merge(fnode,tnode,by.x="fnode",by.y="tnode")
  upstream_list_index$Row<-seq(1,nrow(upstream_list_index),1)
  upstream_list_index2<-with(upstream_list_index,aggregate(Row~fnode,FUN = function(x) min(x)))
  upstream_list_index3<-with(upstream_list_index,aggregate(Row~fnode,FUN = function(x) max(x)))
  upstream_list_index4<-merge(upstream_list_index2,upstream_list_index3, by ="fnode")
  names(upstream_list_index4)[2:3]<-c("start","end")
  
  #get order for upstream_list
  Start<-upstream_list_index4[,which(names(upstream_list_index4) %in% c("fnode","start"))]
  End<-upstream_list_index4[which(!upstream_list_index4$end %in% upstream_list_index4$start),which(names(upstream_list_index4) %in% c("fnode","end"))]
  names(End)[2]<-"start"
  lineOrder<-rbind(Start,End)
  
  lineOrder<-lineOrder[order(lineOrder$start),]
  #get non-continuous list values
  for (l in which(!seq(1:max(lineOrder$start)) %in% lineOrder$start)){
    MAX<-min(lineOrder[which(lineOrder$start>l),]$start)
    MIN<-max(lineOrder[which(lineOrder$start<l),]$start)
    Fnode<-lineOrder[which(lineOrder$start==MAX),]$fnode
    sub<-data.frame(fnode = rep(Fnode,length(seq(MIN,MAX,1))),start = seq(MIN,MAX,1))
    sub<-sub[which(!sub$start %in% lineOrder$start),]
    lineOrder<-rbind(lineOrder,sub)
  }
  
  
  lineOrder$seqvar<-upstream_list_index[match(lineOrder$start,upstream_list_index$Row),]$seqvar
  lineOrder<-lineOrder[order(lineOrder$fnode,lineOrder$seqvar),]
  
  #define upstream_list
  upstream_list<-data.frame(seqvar = unique(lineOrder$seqvar))
  
  #save upstream_list_index
  upstream_list_index<-upstream_list_index4
  
  #Determine terminal flowlines, which serve as the initial group for 
  #computing hydro sequence
  selectterm<-data1[which(data1$waterid_for_RSPARROW_mapping %in% select_targetReachWatersheds),]
  for (f in selectterm$fnode){
    select_tnodes<-data1[which(data1$tnode==f),]
    if (exists("ifterm")){
      ifterm<-rbind(ifterm,select_tnodes)
    }else{
      ifterm<-select_tnodes
    }
  }
  ifterm<-data.frame(tnode=ifterm$tnode)
  ifterm<-merge(tnode,ifterm,by="tnode")
  # ifterm<-merge(tnode,fnode,by.x="tnode",by.y="fnode",all=TRUE)
  #ifterm<-ifterm[which(!ifterm$tnode %in% fnode$fnode),]
  ifterm<-data.frame(seqvar = ifterm[order(ifterm$tnode,ifterm$Row),c("seqvar")])
  
  
  #Module creates a sequence of upstream indices based on the 
  #values in the two columns of the list index for the
  #requested row. 
  getuindx<-function(inrow,upstream_list_index){
    list_start<-upstream_list_index[inrow,]
    if (nrow(na.omit(list_start))==0){
      out<-NA
    }else{
      out<-seq(list_start[,1],list_start[,2],1)
    }
    return(out)
  }
  
  #Module creates a sequence of downstream indices based on the 
  #values in the two columns of the list index for the
  #requested row. 
  getdindx<-function(inrow,dnstream_list_index){
    list_start<-dnstream_list_index[inrow,]
    return(seq(list_start[,1],list_start[,2],1))
  }
  
  #Module returns the sequential ids for all flowlines
  #immediately upstream of all ids in the stack group
  #that also meet the condition that every flowline
  #downstream of stack flowlines fromnode has an 
  #assigned hydrosequence value and have not previously
  #been processed.
  upstream<-function(group,hydseqvar,ifproc,fnode,dnstream_list,upstream_list,upstream_list_index,dnstream_list_index){
    for (i in 1:length(group)){
      #get upstream and dnstream indexes
      ulist_subset<-getuindx(fnode[group[i]],upstream_list_index)
      dlist_subset<-dnstream_list[getdindx(fnode[group[i]],dnstream_list_index)]
      #create upgroup vector, empty
      if (i==1){
        upgroup<-vector("numeric")
      }
      
      #If all reaches downstream of fnode have hydseg then get list of
      #reaches upstream of fnode to be added to upstream group
      if (length(na.omit(ulist_subset))!=0 & !any(is.na(hydseqvar[dlist_subset])) & is.na(ifproc[fnode[group[i]]])){
        #mark as processed
        ifproc[fnode[group[i]]] <- 1
        #save in upgroup
        upgroup<-c(upgroup,upstream_list[ulist_subset])
      }#end if (!is.na(ulist_subset) & ...
    }#for i     
    if (exists("upgroup")){
      #save ifproc changes to parent.frame
      assign("ifproc",ifproc,envir = parent.frame())  
      return(upgroup)
    }else{
      return(NA)
    }#end exists upgroup
  }#end upstream
  
  #Load terminal flowlines - used to initial the stack 
  stack<-ifterm$seqvar
  
  #Load fnode - used with list indexes to retrieve lists 
  #of flowlines immedeately upstream or downstream of
  #a fnode 
  fnode<-data1$fnode
  
  #Load the list index, categorized by fnode, to find
  #in the upstream_list the list of flowline sequence 
  #numbers immediately upstream of the fnode.
  upstrm_fnode<-upstream_list_index$fnode
  upstrm_list_index<-upstream_list_index[c("start","end")]
  
  
  #Load the list of flowline sequence numbers upstream
  #of a given fnode, structured by fnode via the index
  #upstream_list_index
  upstream_list<-upstream_list$seqvar
  
  #Load the list index, categorized by fnode, to find
  #in the dnstream_list the list of flowline sequence 
  #numbers immediately dnstream of the fnode.
  dnstrm_fnode<-dnstream_list_index$fnode
  dnstrm_list_index<-dnstream_list_index[c("start","end")]
  
  #Load the list of flowline sequence numbers downstream
  #of a given fnode, structured by fnode via the index
  #dnstream_list_index
  dnstream_list<-dnstream_list$seqvar
  
  nreaches = length(fnode)
  #Initialize the hydrosequence variable
  hydseqvar<-rep(NA,nreaches)
  
  #Imbed the indexes in fnode matrices 
  upstream_list_index<-as.data.frame(matrix(nrow=maxfnode,ncol=2,NA))
  upstream_list_index[upstrm_fnode,]<- upstrm_list_index
  dnstream_list_index<-as.data.frame(matrix(nrow=maxfnode,ncol=2,NA))
  dnstream_list_index[dnstrm_fnode,]<- dnstrm_list_index
  
  #Initialze a fnode-referenced vector to track if 
  #the flowlines upstream of a given fnode have 
  #already been processed by assignment of hydrosequence
  #values
  ifproc<-rep(NA,maxfnode)
  
  #Initialize the hydrosequence counter
  h0 = 0 
  
  #Loop through the network in increments defined by 
  #the stack of flowlines conditionally selected to 
  #receive sequential hydrosequence numbers. Assign 
  #hydrosequence numbers to the existing stack and 
  ##then repopulate the stack based on qualifying 
  #flowlines immediately upstream of the existing 
  #stack. The loop terminates when there are no more
  #upstream flowlines in the network.
  
  while (length(stack)!=0){
    #Determine the upper range of hydrosequence
    #numbers to assign to the existing stack
    h1 = h0 + length(stack)
    
    #Assign hydrosequnce numbers to the flowlines
    #in the existing stack, as referenced by the 
    #flowline sequence id.
    hydseqvar[stack] = seq((h0 + 1),h1,1)
    
    #Repopulate the stack
    stack<-upstream(stack,hydseqvar,ifproc,fnode,dnstream_list,upstream_list,upstream_list_index,dnstream_list_index)
    
    #Increment the hydrosequence counter 
    h0<-h1
    
  }#end while
  
  #Update the values of the hydrosequence variable
  #in the network data set 
  
  #downstream ordering
  data1$hydseq <- hydseqvar*-1 
  data1[which(data1$waterid_for_RSPARROW_mapping %in% select_targetReachWatersheds),]$hydseq<-9999
  
  
  
  return(data1)
  
  
  
}#end function
