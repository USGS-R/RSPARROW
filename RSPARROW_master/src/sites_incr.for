C sites_incr.for

C Purpose: Assigns the site ID to the upstream reaches in the incremental drainage.  

C Notes:
C  1. Called from the 'assignIncremSiteIDs.R' function.
C  2. Reaches sorted by HYDSEQ
C  3. Input variables:
C     'nreach' - stream reach ID
C     'nnode' - total number of reach IDs
C     'minnum' - user selected minimum number of reaches between sites
C     'fnode' - from (upstream) node
C     'tnode' - to (downstream) node
C     'waterid' - generic water identification number
C     'staid' - reach site IDs (non-zero for reaches with selected monitoring sites)
C  4. Return variable: 'rchstaid' - site IDs assigned to upstream incremental reaches contiguously

      subroutine sites_incr(nreach,nnode,minnum,fnode,tnode,staid,
     &waterid,nstaid)
        !GCC$ ATTRIBUTES DLLEXPORT::sites_incr
        integer, intent(in) :: nreach,nnode,minnum
        integer, intent(in) :: fnode(nreach),tnode(nreach)
        integer, intent(in) :: staid(nreach),waterid(nreach)
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
