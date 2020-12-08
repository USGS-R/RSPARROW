C sum_atts.for
C
C Purpose: For a user-specified reach attribute, obtain the sum of the upstream
C          reach values of the attribute
C
C Notes:
C  1. This is a utility function that is not called by any RSPARROW functions.
C  2. Reaches must be sorted by HYDSEQ prior to subroutine call. 
C  3. Input variables:
C     'numrchs' = number of reaches
C     'nnode' = total number of reach IDs
C     'data2' = matrix(0,numrchs,ncol=4)
C         [,1] = 'fnode' - from (upstream) node for reach
C         [,2] = 'tnode' - to (downstream) node for reach
C         [,3] = 'frac' - reach fractional diversion quantity
C         [,4] = 'iftran' - reach transport indicator
C     'incatt' = reach value to be summed for all upstream reaches
C     'sumatt' = reach summed value (returned to R environment)
C
C Example of R statements to use subroutine to sum incremental reach area:
C    numrchs <- length(tnode)
C    nnode <- max(tnode,fnode)
C    data2 <- matrix(0,nrow=numrchs,ncol=4)
C    data2[,1] <- fnode
C    data2[,2] <- tnode
C    data2[,3] <- frac
C    data2[,4] <- iftran
C    sumatts <- function(incatt) { 
C       sumatt <- matrix(0,nrow=numrchs,ncol=1)
C       fsumatt <- matrix(0,nrow=numrchs,ncol=1)
C       if (!is.loaded('sum_atts')) {
C         dyn.load('sum_atts.dll')                             
C       }
C       return_data <- .Fortran('sum_atts',
C        numrchs=as.integer(numrchs),
C        nnode=as.integer(nnode),
C        data2=as.double(data2),
C        incatt=as.double(incatt),
C        sumatt=as.double(sumatt)) 
C        fsumatt <- return_data$sumatt
C        dyn.unload('sum_atts.dll')
C       return(fsumatt) 
C   }  # end sumatts function
C    incatt <- demiarea            # incremental reach area
C    fsumiarea <- sumatts(incatt)  # total reach area

      subroutine sum_atts(nreach,nnode,data2,incatt,sumatt)
        !GCC$ ATTRIBUTES DLLEXPORT::sum_atts
        integer, intent(in) :: nreach
        integer, intent(in) :: nnode
        integer :: fnode,tnode,iftran
        double precision, intent(in) :: data2(nreach,4)
        double precision, intent(in) :: incatt(nreach)
        double precision :: frac(nreach),node(nnode)
        double precision, intent(inout) :: sumatt(nreach)

        do i=1,nnode
         node(i) = 0
        end do
        do i=1,nreach
         fnode = int(data2(i,1))
         tnode = int(data2(i,2))
         frac = data2(i,3)
         iftran = int(data2(i,4))
         sumatt(i) = (incatt(i) + frac(i) * node(fnode)) 
         node(tnode) = node(tnode) + iftran * sumatt(i)
        end do
        end subroutine sum_atts
