C deliv_fraction.for

C Purpose: Computes the delivery fraction metric in the prediction functions (e.g., predict.R).
C Notes:  The input reaches are sorted in reverse hydrologic order

      subroutine deliv_fraction(nreach,waterid,nnode,data2,
     &incdecay,totdecay,sumatt)
        !GCC$ ATTRIBUTES DLLEXPORT::deliv_fraction
        integer, intent(in) :: nreach
        integer, intent(in) :: nnode,waterid(nreach)
        integer :: fnode,tnode,termflag
        double precision, intent(in) :: data2(nreach,5)
        double precision, intent(in) :: incdecay(nreach)
        double precision, intent(in) :: totdecay(nreach)
        double precision :: frac,node(nnode),xiftran
        double precision, intent(inout) :: sumatt(nreach)

C        open(50,file='outtest.txt')
        do i=1,nnode
         node(i) = 0.0
        end do
        do i=nreach,1,-1
         fnode = int(data2(i,1))
         tnode = int(data2(i,2))
         termflag = int(data2(i,5))
         frac = data2(i,3)
         xiftran = data2(i,4)

         if(termflag.ne.0) then 
            sumatt(i) = 1.0
         else
            sumatt(i) =  xiftran * node(tnode) 
         endif
         node(fnode) = node(fnode) + sumatt(i) * frac * totdecay(i)
         sumatt(i) = sumatt(i) * incdecay(i)

C         write(50,8888) i,waterid(i),fnode,tnode,sumatt(i),
C     &incdecay(i),totdecay(i),frac,xiftran
C 8888 format(1x,4i6,5f12.4)

        end do

C        close(50)

        end subroutine deliv_fraction
