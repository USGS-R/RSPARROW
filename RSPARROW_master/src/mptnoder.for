C mptnoder.for

C Purpose: Load stream network accumulation routine that returns a share of the prediction.  
C          Computes the monitoring-adjusted source loads, which are calculated 
C          as shares of the nonadjusted total loads. 

C Notes:   See predict.R calculation of 'mpload_' shares for background. 

      subroutine mptnoder(ifadj,share,nreach,nnode,
     cdata2,incddsrc,carryf,pred)
        !GCC$ ATTRIBUTES DLLEXPORT::mptnoder
        integer, intent(in) :: ifadj
        integer, intent(in) :: nreach
        integer, intent(in) :: nnode
        integer :: fnode,tnode,iftran,i_obs
        double precision, intent(in) :: data2(nreach,4),share(nreach)
        double precision, intent(in) :: incddsrc(nreach),carryf(nreach)
        double precision :: depvar,node(nnode)
        double precision, intent(inout) :: pred(nreach)

        i_obs = 1
        do i=1,nnode
         node(i) = 0
        end do
        do i=1,nreach
         fnode = int(data2(i,1))
         tnode = int(data2(i,2))
         depvar = data2(i,3)
         iftran = int(data2(i,4))
         rchld = (incddsrc(i) + carryf(i) * node(fnode)) 
            if(depvar.gt.0) then
              if(ifadj.eq.1) then
                 rchld=depvar*share(i)
              endif
              i_obs=i_obs+1
            end if

         pred(i) = rchld
         node(tnode) = node(tnode) + iftran * rchld
        end do
        end subroutine mptnoder
