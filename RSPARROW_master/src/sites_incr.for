C SITES_INCR.FOR
C  Notes:
C   -reaches sorted by HYDSEQ
C   -input: 'staid' - reach site IDs (non-zero for reaches with selected monitoring sites)
C   -input: 'minnum' - user selected minimum number of reaches between sites
C   -returns: 'rchstaid' - site IDs assigned to upstream incremental reaches contiguously
   
      subroutine sites_incr(nreach,nnode,minnum,fnode,tnode,staid,
     &mrbid,nstaid)
        !GCC$ ATTRIBUTES DLLEXPORT::sites_incr
        integer, intent(in) :: nreach,nnode,minnum
        integer, intent(in) :: fnode(nreach),tnode(nreach)
        integer, intent(in) :: staid(nreach),mrbid(nreach)
        integer, intent(inout) :: nstaid(nreach)
        integer :: nmont(nnode),nrch(nnode)

        do i=1,nnode
         nmont(i) = 0
         nrch(i) = minnum  ! initialize with minimum number reaches
        end do

        do i=nreach,1,-1
          if(staid(i).gt.0.and.nrch(tnode(i)).ge.minnum) then
             nmont(fnode(i)) = staid(i)  ! assign new staid to upstream node
             nstaid(i) = staid(i)
             nrch(fnode(i)) = 1                 ! initialize reach counter
          else
             nmont(fnode(i)) = nmont(tnode(i)) ! pass ID upstream
             nstaid(i) = nmont(tnode(i))
             nrch(fnode(i)) = nrch(tnode(i)) + 1  ! count reaches btwn sites
          endif
        end do
        end subroutine sites_incr
