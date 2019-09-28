C     
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
