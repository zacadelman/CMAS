      program TRANS_FAC

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  CEMPD
c  UNC Chapel Hill
c  Program: adjust_moves_pm.f
c  Author: Mohammad Omary
c  Description: adjust the runpm and startpm with temperature
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none 


cccccccccccc
c  Includes
cccccccccccc
      include 'PARMS3.EXT'
      include 'FDESC3.EXT'
      include 'IODECL3.EXT'

c...........   EXTERNAL FUNCTIONS and their descriptions:

        CHARACTER(2)    CRLF
        CHARACTER(16)   PROMPTMFILE
        INTEGER         FINDC
        INTEGER         INDEX1
        INTEGER         LOCATC
        INTEGER         GETFLINE
        INTEGER         PROMPTFFILE
        INTEGER         STR2INT
        INTEGER         ENVINT
        REAL            STR2REAL
        LOGICAL         BLKORCMT
        LOGICAL         SETENVVAR

        EXTERNAL   CRLF, FINDC, GETFLINE, PROMPTFFILE, STR2INT, STR2REAL,
     &             BLKORCMT, ENVINT, INDEX1,SETENVVAR 

c............   Allocatable arrays

c      logical::debug = .true.
      REAL, ALLOCATABLE  :: XPORTFRAC  ( :,: )
      REAL, ALLOCATABLE  :: EMISIN  ( :,:,: )
      REAL, ALLOCATABLE  :: EMISOUT ( :,:,: )

      INTEGER LOGDEV

      CHARACTER(12), ALLOCATABLE :: VNAMES_SAVE (:)

      INTEGER NVARS_SAVE, NVARS_SAVE2

C  COUNTERS
      INTEGER T, N, C, R, L, I, J

      CHARACTER(16)   :: FDUST_IN       ! netCDF file for fdust before applying tfactors (input)
      CHARACTER(16)   :: FDUST_OUT      ! netCDF fdust after applyinf tfactors (input)
      CHARACTER(16)   :: TFACTORS       ! netCDF file for transport factors (input)
      CHARACTER(16)   :: PNAME
      
      DATA PNAME /'XPORTFRACTIONS'/

c****************************************************
c..... Begin body of program ADJUST_PM

        LOGDEV = INIT3() 
	
         FDUST_IN   = 'FDUST_IN'
         FDUST_OUT  = 'FDUST_OUT'
         TFACTORS   = 'TRANS_FAC'

c.....open met data file 
        IF ( .NOT. OPEN3( TFACTORS, FSREAD3, PNAME) )THEN
          WRITE(*,*), "Error opening input file: ", TFACTORS
          CALL exit(-1)
c        END IF
        ELSE IF ( .NOT. DESC3( TFACTORS ) ) THEN
          WRITE(*,*), "Error getting description: ", TFACTORS
          CALL exit(-1)
        END IF

        ALLOCATE (XPORTFRAC (NCOLS3D, NROWS3D ) )

c.....open unadjusted pm data file 

        IF ( .NOT. OPEN3(FDUST_IN , FSREAD3, PNAME) )THEN
 	  WRITE(*,*), "Error opening input file: ", FDUST_IN
 	  CALL exit(-1)
        END IF
        IF ( .NOT. DESC3( FDUST_IN ) ) THEN
 	  WRITE(*,*), "Error getting description: ", FDUST_IN
 	  CALL exit(-1)
        END IF

        ALLOCATE (EMISIN( NCOLS3D, NROWS3D, NVARS3D ) )
        ALLOCATE (EMISOUT( NCOLS3D, NROWS3D, NVARS3D ) )

        ALLOCATE (VNAMES_SAVE( NVARS3D ) )

c..... Save this list of variables
        DO N=1, NVARS3D
          VNAMES_SAVE (N) = VNAME3D (N)
        END DO

ccccccccccccccccccccccccccccccc
c  Open the up the output file
ccccccccccccccccccccccccccccccc
        IF ( .NOT. OPEN3(FDUST_OUT, FSUNKN3, PNAME) ) THEN
           WRITE(*,*), "Error opening the output file"
          CALL exit(-1)
        END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Read the transport factors
c  Read the unadjusted emissions, adjust and wtite to outpufile
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DO T = 1, MXREC3D

      EMISIN    = 0
      EMISOUT   = 0
      XPORTFRAC = 0   

c.... Get the transport factors
        IF (.NOT. READ3 (TFACTORS, 'xportfrac',ALLAYS3,
     &       SDATE3D, STIME3D, XPORTFRAC )) THEN
          WRITE(*,*),'Error reading ... ', TFACTORS
          CALL exit(-1)
        END IF


c.... Get the unadjusted emissions
        if (.NOT. READ3 (FDUST_IN, ALLVAR3, ALLAYS3,
     &       SDATE3D, STIME3D, EMISIN )) THEN
        CALL M3EXIT('test', SDATE3D, STIME3D, 'Error Reading ' // FDUST_IN // ' ' // ALLVAR3, -1)
        END IF

c   Compute the adjustemnt factors, and adjusted emissions

        DO C = 1, NCOLS3D
          DO R = 1, NROWS3D
             IF (XPORTFRAC (C,R) .GT. 1 ) THEN
               XPORTFRAC (C,R) = 1.0
             END IF 
               DO N = 1,NVARS3D 
                   EMISOUT(C,R,N) = EMISIN(C,R,N)*XPORTFRAC(C,R)    ! interpolate the adjustment factor
               END DO  ! N
          END DO ! R
        END DO ! C

c       Now write this time step to the new file
        if ( .NOT. WRITE3( FDUST_OUT, ALLVAR3,
     &       SDATE3D, STIME3D, EMISOUT ) ) THEN
          WRITE(*,*), "Error writing to the output file"
        END IF

        CALL NEXTIME( SDATE3D, STIME3D, TSTEP3D )
      END DO

C******************  FORMAT  STATEMENTS   ******************************

         END PROGRAM TRANS_FAC
