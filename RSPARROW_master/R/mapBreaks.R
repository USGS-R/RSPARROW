#'@title mapBreaks
#'@description Creates mapping color breakpoints.  \\cr \\cr
#'Executed By: \\itemize\{\\item mapSiteAttributes.R
#'             \\item predictMaps.R\} \\cr
#'Executes Routines: named.list.R \\cr
#'@param vvar mapping variable as vector
#'@param colors character vector of colors used in mapping with the number of colors 
#'       indicating the number of breaks in the legend
#'@return `outBrks` named list of iprob and brks for mapping



mapBreaks<-function(vvar,  colors){
  
  # link MAPCOLORS for variable to shape object (https://gist.github.com/mbacou/5880859)
  
  iprob<-length(colors)
  rp<-numeric(0)
  set_unique_breaks <- function(x,ip,rp) {
    
    chk1 <- quantile(x, probs=0:ip/ip)
    chk <- unique(quantile(x, probs=0:ip/ip)) # define quartiles
    
    constChk<-plyr::count(chk1)
    constChk<-constChk[which(constChk$freq!=1),]
    
    if (nrow(constChk)!=0){#if constants exist in quantiles
      removeConst<-constChk[which(constChk$freq==max(constChk$freq)),]$x
      x<-x[which(!x %in% removeConst)]
      ip<-ip-length(removeConst)
      rp<-c(rp,removeConst)
      Recall(x,ip,rp)
    }else{ 
      if (length(chk1) == length(chk)){
        ip<-ip+length(rp)
        chk1<-as.vector(chk1)
        chk1<-sort(c(rp,chk1))
        
        return(named.list(ip,chk1))
      } 
      
      
      ip<-ip-1
      Recall(x,ip,rp)
    }# run the function again
    
    
  }
  iprob <- set_unique_breaks(vvar,iprob,rp)$ip
  
  brks<-set_unique_breaks(vvar,iprob,rp)$chk1
  
  outBrks<-named.list(brks,iprob)
  
  return(outBrks)
  
}
