C PROGRAM NAME:  hydseqR.for
C=============================================================

C PURPOSE:  Program creates two SPARROW pre-processing variables:
C           HYDSEQ and DEMTAREA, the hydrologic sequence code and
C           the total drainage area by reach.  User may allow
C           program to automatically identify headwater reaches.
C 
C=============================================================

C HISTORY:
C         Programmer               Date              Comments
C
C R.B. Alexander                 06/27/00
C                                05/20/02      Added code to handle 
C                                                nonzero IPTR
C                                06/21/02      Change output to 
C                                              file 20 so that
C                                              to- and from-node
C                                              order identical for 
C                                              all three write statements
C                                12/20/02      Final cleanup for release
C                                01/14/03      Increase array sizes,
C                                               increased output formats
C                                               for reaches and total area
C                                01/28/03      Added option to identify the
C                                               nearest downstream monitoring
C                                               station for each reach; output
C                                               headwater flag to hydseq file
C                                08/12/04      Replace IMSL sort with 
C                                               subroutine QSORTI
C                                07/28/16      Revised as subroutine for R
C                                               Deleted optional code for
C                                               summing monitoring station area
C=============================================================

C NOTES:
C
C 1.  The program employs stacks and a binary-search algorithm to 
C     climb upstream.  Stack structure was modified from Eckhouse 
C     (1975) "Minicomputer Systems:  Organization and Programming," 
C     Prentice-Hall, NJ, p. 132.  The binary-search sub-program was
C     written by Kenneth J. Lanfear, U.S. Geological Survey, Water
C     Resources Division.
C 
C 2.  One input file is required. Each file is in free-format,
C      meaning each record is a list of numbers separated by
C      any number of spaces.  No sorting is required on the file.
C
C 3. The reach file is topologically correct (full connectivity)
C    and contains a from-node# and to-node# for every reach in
C    the domain.  Flow direction is FROM-TO.
C    
C 4. The maximum limits of the program are in PARAMETER
C    statements below:
C      MAXARC:  Max # of arcs in reach network
C      MAXNEST: Max # of arcs in an UN-nested drainage structure
C
C 5. A maximum of 1000000 nodes may be pushed onto the stack.
C 
C 6. Checks made on downstream TNODE for adjacent reaches to allow for
C       maximum of four tributary reaches.
C 
C=============================================================

       SUBROUTINE HYDSEQR(MAXARC,MAXREACH,R,F,T,A,FR,XDATA)

c RETURN XDATA as vector:
C  ,1) = reach id
C  ,2) = hydrologic sequence code
C  ,3) = HEADFLAG - headwater reach flag
C  ,4) = total drainage area

        !GCC$ ATTRIBUTES DLLEXPORT::HYDSEQR

        integer, intent(in) :: MAXARC
        integer, intent(in) :: maxreach
        integer, intent(in) :: R(MAXARC),F(MAXARC),T(MAXARC)
        double precision, intent(in) :: A(MAXARC),FR(MAXARC)
        double precision, intent(inout) :: xdata(maxreach)

C        CHARACTER(LEN=*), intent(in) :: PATH
C        CHARACTER(LEN=*), intent(inout) :: OPATH

C MAXARC==number of reaches
C WATERID==R
C FNODE==F
C TNODE==T
C DEMIAREA==A
C FRAC==FR

C max no. arcs avaiable for push/pull stack
        integer :: STACKA(1000000)
        integer :: idata(maxarc,4)
        integer :: IPTR,ARCH,DRCH,HRCH,CRCH,SNODE
        integer :: INDEX,IHIGH,ILOW
        integer :: ISEQ,ISEL,IH
        integer :: HD,ICT1,ICT2,index2,index3,index4
        integer :: WREACH(MAXARC),TEMP(MAXARC)
        integer :: REACH(MAXARC),FNODE(MAXARC),TNODE(MAXARC)
        integer :: STNODE(MAXARC),SFNODE(MAXARC)
        integer :: HEAD(MAXARC),FPERM(MAXARC),TPERM(MAXARC)
        integer :: CHKRCH(MAXARC),IUP(MAXARC),IDOWN(MAXARC)
        double precision :: AREA(MAXARC),FRAC(MAXARC)
        double precision :: CAREA(MAXARC),AREARCH(MAXARC)
        double precision :: rdata(maxarc,2)

C=============================================================

C variable dictionary

C Index variables:
C INDEX,ITERM: arbitrary index names to arrays.
C IHIGH,ILOW : range of record number(s) for reach file which 
C    indicate the reach(es) immediately upstream to the current reach.
C ICT1..5: counters for output file records.
C IPTR : pointer in stack.
C NRA : actual number of reaches used in program.
C MAXARC : maximum number of reaches program will accept.

C Data arrays:

C FNODE:   from-node number of terminal node from reach file.
C REACH : cover number from ARC-INFO cover.AAT for a given reach (ERF1##)
C RFRAC: diversion fraction applied to sources and instream load in the 
C		intervening drainage area between WQ stations.
C RTOT: reach time of travel from stream velocity and reach length in days.
C SNODE : search to_node
C STACK : stack array stores reach numbers during reach climb.
C TNODE:  to-node number of terminal node from reach file
C 
C=============================================================

C Identify headwater reaches automatically
        ISEL = 1

C initialize counter and other variables
         ICT1 = 0
         ICT2 = 0
         ISEQ = 0

C read reach file into memory 

      DO 250 I=1,MAXARC
        STNODE(I) = 0
        SFNODE(I) = 0
        HEAD(I) = 0
        AREA(I) = 0
        WREACH(I) = -1
 250  CONTINUE

	DO 300  I = 1,MAXARC
         if(R(I).eq.0) goto 300
         WREACH(R(I)) = 0
         REACH(R(I))=R(I)
         FNODE(R(I))=F(I)
         TNODE(R(I))=T(I)
         AREA(R(I))=A(I)
         FRAC(R(I))=FR(I)

         STNODE(R(I))=T(I)
         TPERM(R(I))=R(I)
         SFNODE(R(I))=F(I)
         FPERM(R(I))=R(I)
 300  CONTINUE


C sort arrays
      CALL QSORTI (TPERM,MAXARC,STNODE) ! original array STNODE not sorted
      DO 305 I=1,MAXARC   ! temporary storage of array values
       TEMP(I)=STNODE(TPERM(I))
 305  CONTINUE
      DO 306 I=1,MAXARC   ! place values in ascending order in original array
       STNODE(I)=TEMP(I)
 306  CONTINUE

      CALL QSORTI (FPERM,MAXARC,SFNODE) ! original array STNODE not sorted
      DO 307 I=1,MAXARC   ! temporary storage of array values
       TEMP(I)=SFNODE(FPERM(I))
 307  CONTINUE
      DO 308 I=1,MAXARC   ! place values in ascending order in original array
       SFNODE(I)=TEMP(I)
 308  CONTINUE

C========================================
C  Identify headwater reaches (IH=number)

         IH=0
         DO 45 I=1,MAXARC
             IF(SFNODE(I).EQ.0) GOTO 45
           DO 50 K=1,MAXARC
             IF(SFNODE(I).EQ.STNODE(K)) GOTO 45
 50        CONTINUE
             HEAD(FPERM(I))=1
             IH=IH+1
 45      CONTINUE


C =============================================================

c output files
C	OPEN(20,FILE='hydseq.dat')

C begin loop through headwater reaches tracing downstream...creating HYDSEQ

        IPTR = 0

        DO 1000 IREACH = 1,MAXARC
           IF(HEAD(IREACH).NE.1) GOTO 1000

C  write attributes for headwater reach

        HRCH = REACH(IREACH)
        ISEQ = ISEQ+1

C        WRITE(20,5151)ISEQ,IREACH,FNODE(IREACH),TNODE(IREACH),
C     &     AREA(IREACH),FRAC(IREACH),HEAD(IREACH)

         index2 = maxarc + ireach
         index3 = maxarc*2 + ireach
         xdata(ireach) = float(ireach)
         xdata(index2) = float(iseq)
         xdata(index3) = float(head(ireach))

         idata(iseq,1) = ireach
         idata(iseq,2) = iseq
         idata(iseq,3) = fnode(ireach)
         idata(iseq,4) = tnode(ireach)
         rdata(iseq,1) = area(ireach)
         rdata(iseq,2) = frac(ireach)

        ICT2=ICT2+1
        WREACH(IREACH) = WREACH(IREACH) + 1
        SNODE = TNODE(IREACH)
        CRCH = HRCH
C5151    FORMAT(4(I6,1x),F12.3,1X,E12.5,1X,I3)

C========================================

C  Find adjacent tributary reach

 66   CONTINUE
        CALL BSRCH(SNODE,MAXARC,STNODE,ILOW,IHIGH)

C  Determine if all tributary reaches have been processed
C      (as many as 4 tributaries may exist)

        IRCK = 0
        DO 750 INDEX = ILOW,IHIGH
          IF(WREACH(TPERM(INDEX)).EQ.1) IRCK=IRCK+1
 750    CONTINUE

        IRCH = IHIGH-ILOW+1

c===============================================
        IF(IRCK.LT.IRCH) THEN   

C      Adjacent reach attributes not written or this is a 
C          terminal reach (ILOW=IHIGH=0).
C      Get new reach from the stack or headwater array
          
          IF(IPTR.EQ.0) GOTO 1000  ! stack empty get headwater rch
          CALL POP(INDEX,IPTR,STACKA)
          ARCH = FPERM(INDEX)

          IF(WREACH(ARCH).EQ.0) THEN
            ISEQ = ISEQ+1
C            WRITE(20,5151)ISEQ,FPERM(INDEX),FNODE(FPERM(INDEX)),
C     &         TNODE(FPERM(INDEX)),AREA(FPERM(INDEX)),
C     &         FRAC(FPERM(INDEX)),HEAD(FPERM(INDEX))  

           index2 = maxarc + FPERM(INDEX)
           index3 = maxarc*2 + FPERM(INDEX)
           xdata(FPERM(INDEX)) = float(FPERM(INDEX))
           xdata(index2) = float(iseq)
           xdata(index3) = float(HEAD(FPERM(INDEX)))

           idata(iseq,1) = FPERM(INDEX)
           idata(iseq,2) = iseq
           idata(iseq,3) = fnode(FPERM(INDEX))
           idata(iseq,4) = tnode(FPERM(INDEX))
           rdata(iseq,1) = area(FPERM(INDEX))
           rdata(iseq,2) = frac(FPERM(INDEX))

            ICT2=ICT2+1
            WREACH(ARCH) = WREACH(ARCH) + 1
          ENDIF

          SNODE = TNODE(FPERM(INDEX))
          CRCH = ARCH
          GOTO 66
         
C==================================================================
C  All tributaries are processed/written - continue downstream

C  Find downstream reach (2 outcomes:  single, divergent)
C
       ELSE

C  downstream rch is single or divergent reach ===============
C    if divergent, write ilow, push ihigh

          SNODE = TNODE(TPERM(ILOW))
          CALL BSRCH(SNODE,MAXARC,SFNODE,ILOW,IHIGH) 
                                   !locate downstream reach FNODE

          IF(ILOW.EQ.0) THEN
C     no downstream reach - check the stack
           IF(IPTR.EQ.0) GOTO 1000  ! stack empty get headwater reach
           CALL POP(INDEX,IPTR,STACKA)
           ARCH = FPERM(INDEX)

           IF(WREACH(ARCH).EQ.0) THEN
            ISEQ = ISEQ+1
C            WRITE(20,5151)ISEQ,FPERM(INDEX),FNODE(FPERM(INDEX)),
C     &          TNODE(FPERM(INDEX)),AREA(FPERM(INDEX)),
C     &          FRAC(FPERM(INDEX)),HEAD(FPERM(INDEX))

           index2 = maxarc + FPERM(INDEX)
           index3 = maxarc*2 + FPERM(INDEX)
           xdata(FPERM(INDEX)) = float(FPERM(INDEX))
           xdata(index2) = float(iseq)
           xdata(index3) = float(HEAD(FPERM(INDEX)))

           idata(iseq,1) = FPERM(INDEX)
           idata(iseq,2) = iseq
           idata(iseq,3) = fnode(FPERM(INDEX))
           idata(iseq,4) = tnode(FPERM(INDEX))
           rdata(iseq,1) = area(FPERM(INDEX))
           rdata(iseq,2) = frac(FPERM(INDEX))

            ICT2=ICT2+1
            WREACH(ARCH) = WREACH(ARCH) + 1
           ENDIF

           SNODE = TNODE(FPERM(INDEX))
           CRCH = ARCH
           GOTO 66
          ENDIF
C===================================

          DRCH = FPERM(ILOW)

          IF(WREACH(DRCH).EQ.0) THEN
             ISEQ = ISEQ+1
C             WRITE(20,5151)ISEQ,DRCH,FNODE(FPERM(ILOW)),
C     &          TNODE(FPERM(ILOW)),
C     &          AREA(FPERM(ILOW)),FRAC(FPERM(ILOW)),HEAD(DRCH)

            index2 = maxarc + DRCH
            index3 = maxarc*2 + DRCH
            xdata(DRCH) = float(DRCH)
            xdata(index2) = float(iseq)
            xdata(index3) = float(HEAD(DRCH))

           idata(iseq,1) = DRCH
           idata(iseq,2) = iseq
           idata(iseq,3) = FNODE(FPERM(ILOW))
           idata(iseq,4) = TNODE(FPERM(ILOW))
           rdata(iseq,1) = AREA(FPERM(ILOW))
           rdata(iseq,2) = FRAC(FPERM(ILOW))

             ICT2=ICT2+1
             WREACH(DRCH) = WREACH(DRCH) + 1
          ENDIF

C  divergent reach -- push ihigh reach, track ilow
          IF(ILOW.NE.IHIGH) THEN
            CALL PUSH(IHIGH,IPTR,STACKA) 
          ENDIF

C  assign downstream T-node as search node

          SNODE = TNODE(FPERM(ILOW))
          CRCH = DRCH
          GOTO 66

       ENDIF   ! end of check of IRCK vs IRCH


1000	CONTINUE
	
C        CLOSE(20)
C========================================

C  Number of reaches unprocessed

C        OPEN(21,FILE='nohydseq.dat')

        ICT1=0
        DO 22 IRCH = 1,MAXARC
           IF(WREACH(IRCH).EQ.0) THEN
              ICT1=ICT1+1
C              WRITE(21,2222) IRCH
C 2222         FORMAT(I5)
           ENDIF

 22     CONTINUE

C        CLOSE(21)

C  Records written to HYDSEQ.DAT = ICT2
C  Number of reaches not processed = ICT1

C=========================================================================
C=========================================================================

C Accumulating drainage area... (open subsequently created file)

C      OPEN(10,FILE='hydseq.dat')

      NR = 0
      NZERO=0

      DO 150 I = 1,MAXARC
 150    CHKRCH(I) = -1

      DO 155 I = 1,ISEQ

C        READ(10,*,END=156) IHYDSEQ,REACH(I),IUP(I),IDOWN(I),
C     &     AREA(I),FRAC(I),HEAD(I)

        REACH(I) = idata(i,1)
        IHYDSEQ = idata(i,2)
        IUP(I) = idata(i,3)
        IDOWN(I) = idata(i,4)
        AREA(I) = rdata(i,1)
        FRAC(I) = rdata(i,2)

        IF(IHYDSEQ.LE.0) GOTO 155
        IF(AREA(I).EQ.0) NZERO=NZERO+1
        NR = NR + 1
        CHKRCH(REACH(I)) = 0
  155 CONTINUE
  156 CONTINUE
C      CLOSE(10)

C--------------------------------------
C ACCUMULATE INCREMENTAL DRAINAGE AREA 
C--------------------------------------

      DO 580 I = 1,MAXARC
        CAREA(I) = 0
        AREARCH(I) = 0
 580  CONTINUE

      DO 1111 I = 1,NR

         CAREA(IDOWN(I)) = CAREA(IDOWN(I)) + 
     &      ( FRAC(I) * CAREA(IUP(I)) + AREA(I) )

         AREARCH(REACH(I)) = FRAC(I) * CAREA(IUP(I)) 
     &     + AREA(I)

 1111  CONTINUE

      IOUT=0
C      OPEN(20,FILE='tarea.dat')
      DO 2002 I = 1,MAXARC
       IF(CHKRCH(I).EQ.0) THEN
          IOUT=IOUT+1
C          WRITE(20,2020) I,AREARCH(I)
C2020      FORMAT(I6,1X,E16.8)

          index4 = maxarc*3 + I
          xdata(index4) = AREARCH(I)

       ENDIF
2002  CONTINUE
C      CLOSE(20)

C  Records written to TAREA.DAT = IOUT
C  Reaches with zero incremental area = NZERO

        end subroutine HYDSEQR

C =============================================================

       SUBROUTINE PUSH (AVALUE,IPTR,STACKA)
        !GCC$ ATTRIBUTES DLLEXPORT::PUSH

        integer, intent(inout) :: AVALUE,IPTR
        integer, intent(inout) :: STACKA(1000000)

	    IPTR = IPTR + 1
          IF(IPTR.GT.50000) THEN
            WRITE(*,1000) 
 1000       FORMAT(1X,'*** STACK MELTDOWN *** IPTR>1000000')
            WRITE(*,*) '   '
            WRITE(*,*) AVALUE
            STOP
          ENDIF
	   STACKA(IPTR) = AVALUE
        end subroutine PUSH

C =============================================================

       SUBROUTINE POP (AVALUE,IPTR,STACKA)
        !GCC$ ATTRIBUTES DLLEXPORT::POP

        integer, intent(inout) :: AVALUE,IPTR
        integer, intent(inout) :: STACKA(1000000)

         AVALUE = STACKA(IPTR)
         IPTR = IPTR - 1
        end subroutine POP

C =============================================================

	SUBROUTINE BSRCH (NUM,N,IARRAY,J1,J2)
        !GCC$ ATTRIBUTES DLLEXPORT::BSRCH

C Author: Kenneth J. Lanfear
C	  U.S. Geological Survey, Water Resources Division
C
C Finds the range of indices, J1 to J2, for which the
C ordered array, IARRAY, equals NUM.
C
        integer, intent(in) :: NUM,N,IARRAY(1)
        integer, intent(inout) :: J1,J2
        integer :: ITOP,IBOT,ITEST,IHIGH,ILOW


C Variable dictionary
C
C NUM : Value we are trying to match in IARRAY.
C N : Length of IARRAY.
C IARRAY : Vector to be searched.
C J1,J2 : Beginning and end of matching range.
C
C.... Find pointer to the first element that exceeds NUM

      IBOT = 0
      ITOP = N + 1
      DO 100 IXXX = 1, N
         IF ((ITOP-IBOT).EQ.1) GO TO 110
         ITEST = (ITOP + IBOT) / 2
         IF (IARRAY(ITEST).GT.NUM) THEN
               ITOP = ITEST
            ELSE
               IBOT = ITEST
            ENDIF
  100    CONTINUE
  110 CONTINUE
      IHIGH = ITOP
C
C.... Find pointer to the first element that is less than NUM
      IBOT = 0
      DO 200 IXXX = 1, N
         IF ((ITOP-IBOT).EQ.1) GO TO 210
         ITEST = (ITOP + IBOT) / 2
         IF (IARRAY(ITEST).LT.NUM) THEN
               IBOT = ITEST
            ELSE
               ITOP = ITEST
            ENDIF
  200    CONTINUE
  210 CONTINUE
      ILOW = IBOT
C
C.... Find the pointers to the equal range.
      IF ((IHIGH-ILOW).EQ.1) THEN
C....       There is no equal range
            J1 = 0
            J2 = 0
         ELSE
C....       Find the range
            J1 = ILOW + 1
            J2 = IHIGH - 1
         ENDIF
C
C.... Finished
        end subroutine BSRCH 




      SUBROUTINE QSORTI (ORD,N,A)
C
C==============SORTS THE ARRAY A(I),I=1,2,...,N BY PUTTING THE
C   ASCENDING ORDER VECTOR IN ORD.  THAT IS ASCENDING ORDERED A
C   IS A(ORD(I)),I=1,2,...,N; DESCENDING ORDER A IS A(ORD(N-I+1)),
C   I=1,2,...,N .  THIS SORT RUNS IN TIME PROPORTIONAL TO N LOG N .
C
C
C     ACM QUICKSORT - ALGORITHM #402 - IMPLEMENTED IN FORTRAN 66 BY
C                                 WILLIAM H. VERITY, WHV@PSUVM.PSU.EDU
C                                 CENTER FOR ACADEMIC COMPUTING
C                                 THE PENNSYLVANIA STATE UNIVERSITY
C                                 UNIVERSITY PARK, PA.  16802
C
        !GCC$ ATTRIBUTES DLLEXPORT::QSORTI

        integer, intent(in) :: N
        integer, intent(inout) :: ORD(N),A(N)
        integer :: X,XX,Z,ZZ,Y
        integer :: POPLST(2,600000)
        integer :: NDEEP,U1,L1,L,U,P,Q

C     TO SORT DIFFERENT INPUT TYPES, CHANGE THE FOLLOWING
C     SPECIFICATION STATEMENTS; FOR EXAMPLE, FOR FORTRAN CHARACTER
C     USE THE FOLLOWING:  CHARACTER *(*) A(N)

      NDEEP=0
      U1=N
      L1=1
      DO 1  I=1,N
    1 ORD(I)=I
    2 IF (U1.LE.L1) RETURN
C
    3 L=L1
      U=U1
C
C PART
C
    4 P=L
      Q=U
C     FOR CHARACTER SORTS, THE FOLLOWING 3 STATEMENTS WOULD BECOME
C     X = ORD(P)
C     Z = ORD(Q)
C     IF (A(X) .LE. A(Z)) GO TO 2
C
C     WHERE "CLE" IS A LOGICAL FUNCTION WHICH RETURNS "TRUE" IF THE
C     FIRST ARGUMENT IS LESS THAN OR EQUAL TO THE SECOND, BASED ON "LEN"
C     CHARACTERS.
C
      X=A(ORD(P))
      Z=A(ORD(Q))
      IF (X.LE.Z) GO TO 5
      Y=X
      X=Z
      Z=Y
      YP=ORD(P)
      ORD(P)=ORD(Q)
      ORD(Q)=YP
    5 IF (U-L.LE.1) GO TO 15
      XX=X
      IX=P
      ZZ=Z
      IZ=Q
C
C LEFT
C
    6 P=P+1
      IF (P.GE.Q) GO TO 7
      X=A(ORD(P))
      IF (X.GE.XX) GO TO 8
      GO TO 6
    7 P=Q-1
      GO TO 13
C
C RIGHT
C
    8 Q=Q-1
      IF (Q.LE.P) GO TO 9
      Z=A(ORD(Q))
      IF (Z.LE.ZZ) GO TO 10
      GO TO 8
    9 Q=P
      P=P-1
      Z=X
      X=A(ORD(P))
C
C DIST
C
   10 IF (X.LE.Z) GO TO 11
      Y=X
      X=Z
      Z=Y
      IP=ORD(P)
      ORD(P)=ORD(Q)
      ORD(Q)=IP
   11 IF (X.LE.XX) GO TO 12
      XX=X
      IX=P
   12 IF (Z.GE.ZZ) GO TO 6
      ZZ=Z
      IZ=Q
      GO TO 6
C
C OUT
C
   13 CONTINUE
      IF (.NOT.(P.NE.IX.AND.X.NE.XX)) GO TO 14
      IP=ORD(P)
      ORD(P)=ORD(IX)
      ORD(IX)=IP
   14 CONTINUE
      IF (.NOT.(Q.NE.IZ.AND.Z.NE.ZZ)) GO TO 15
      IQ=ORD(Q)
      ORD(Q)=ORD(IZ)
      ORD(IZ)=IQ
   15 CONTINUE
      IF (U-Q.LE.P-L) GO TO 16
      L1=L
      U1=P-1
      L=Q+1
      GO TO 17
   16 U1=U
      L1=Q+1
      U=P-1
   17 CONTINUE
      IF (U1.LE.L1) GO TO 18
C
C START RECURSIVE CALL
C
      NDEEP=NDEEP+1
      POPLST(1,NDEEP)=U
      POPLST(2,NDEEP)=L
      GO TO 3
   18 IF (U.GT.L) GO TO 4
C
C POP BACK UP IN THE RECURSION LIST
C
      IF (NDEEP.EQ.0) GO TO 2
      U=POPLST(1,NDEEP)
      L=POPLST(2,NDEEP)
      NDEEP=NDEEP-1
      GO TO 18
C
C END SORT
C END QSORT
C
        end subroutine QSORTI 
