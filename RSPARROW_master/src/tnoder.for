C tnoder.for

C Purpose: Load stream network accumulation routine that returns residuals for use in parameter estimation. 

      subroutine tnoder(ifadj,nreach,nnode,data2,incddsrc,carryf,ee)
        !GCC$ ATTRIBUTES DLLEXPORT::tnoder
        integer, intent(in) :: ifadj
        integer, intent(in) :: nreach
        integer, intent(in) :: nnode
        integer :: fnode,tnode,iftran,i_obs
        double precision, intent(in) :: data2(nreach,4)
        double precision, intent(in) :: incddsrc(nreach),carryf(nreach)
        double precision :: depvar,node(nnode)
        double precision, intent(inout) :: ee(nreach)

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
              if(rchld .le. 0) rchld = 1.0
              ee(i_obs) = log(depvar) - log(rchld)
              if(ifadj.eq.1) rchld=depvar
              i_obs=i_obs+1
            end if
         node(tnode) = node(tnode) + iftran * rchld
        end do
        end subroutine tnoder
