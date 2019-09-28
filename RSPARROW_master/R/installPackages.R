#'@title installPackages
#'@description  \\cr \\cr
#'Executed By:  \\cr
#'@param if_install_packages yes/no indicating whether to install required R packages to run 
#'       RSPARROW. For more details see documentaion Section 4.4.12
#'@param path_master character string path to RSPARROW_master directory.  Internally reset to 
#'       'RSPARROW_master/R/' subdirectory



installPackages <- function(if_install_packages,path_master){
  
  if(substr(path_master,nchar(path_master),nchar(path_master))==.Platform$file.sep){
    path_master<-substr(path_master,1,nchar(path_master)-1)
    assign("path_master",path_master,.GlobalEnv)
  }
  
  if (if_install_packages=="yes"){
    
    pkgs<-c(    "rstan",
                "devtools", 
                "numDeriv", 
                "nlmrt",    
                "stringr", 
                "gplots", 
                "ggplot2",
                "plyr",
                "OpenMx",
                "rgdal",                
                "maptools",
                "sp",
                "spdep",   
                "data.table",
                "data.tree",
                "rstudioapi",
                "roxygen2",
                "svGUI",
                "svDialogs",
                "shiny",
                "shinyWidgets",
                "rhandsontable",
                "gear", 
                "car",
                "dplyr",
                "inline",
                "evaluate",
                "formatR",
                "highr",
                "markdown",
                "knitr",
                "rmarkdown"
    )
    
    #get path to Rscript.exe
    
    while(length(pkgs!=0)){
      #find installed packages
      installed<-list.dirs(.libPaths()[1],recursive=FALSE,full.names = FALSE)
      
      #subset package install list
      pkgs<-pkgs[which(!pkgs %in% installed)]
      
      #installation loop
      for (p in pkgs){
        if (p!="rstan"){
          if (!p %in% installed){
            print(paste0("try ",p))
          }
          #find package dependencies
          deps<-as.character(unlist((tools::package_dependencies(p))))
          for (d in deps){
            if (!d %in% installed){
              #batch install dependencies
              package<-d
              save(list = c("package"),
                   file=paste(path_master,.Platform$file.sep,"batch",.Platform$file.sep,"batch.RData",sep=""))
              system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_master,.Platform$file.sep,"batch",.Platform$file.sep,"pkgInstall.R",sep="")),sep=""), wait = TRUE, invisible = FALSE)
              
              #reset installed
              installed<-list.dirs(.libPaths()[1],recursive=FALSE,full.names = FALSE)
              
            }
          }
          if (!p %in% installed){
            
            #batch installcore package
            package<-p
            save(list = c("package"),
                 file=paste(path_master,.Platform$file.sep,"batch",.Platform$file.sep,"batch.RData",sep=""))
            system(paste(Sys.which("Rscript.exe")," ",file.path(paste(path_master,.Platform$file.sep,"batch",.Platform$file.sep,"pkgInstall.R",sep="")),sep=""), wait = TRUE, invisible = FALSE)
            
            #reset installed
            installed<-list.dirs(.libPaths()[1],recursive=FALSE,full.names = FALSE)
            
            if (p %in% installed){
              print(paste0(p, " complete")) 
            }
          }
        }else{#rstan
          if (!p %in% installed){
            install.packages("rstan")
          }
        }
      }
    }
    message("RSPARROW dependent package installation COMPLETE")
  }#if_install_pacakges
}#end function
