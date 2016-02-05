!------------------------------------------------------------------
!
!                 CMORPH 0.25 degree 3-hourly Information
!
!                    Last Update: February 13 2007
!
! The 0.25 degree 3-hourly CMORPH can be found on the ftp.cpc.ncep.noaa.gov
! server in the precip/global_CMORPH/3-hourly_025deg directory
!
!
! The data are compressed using the standard Unix compress function (files have a
! suffix of ".Z"). Each file is composed of 16 direct access binary
! ("big_endian") records that are defined as follows:
!
! Record 1: contains the merged microwave precipitation only for 00 UTC
! Record 2: contains the "CMORPH" precipitation estimates for 00 UTC
!
! Record 3: contains the merged microwave precipitation only for 03 UTC
! Record 4: contains the "CMORPH" precipitation estimates for 03 UTC
! .
! .
! .
! Record 15: contains the merged microwave precipitation only for 21 UTC
! Record 16: contains the "CMORPH" precipitation estimates for 21 UTC
!
! All units are "mm/hr".  Missing data are denoted by values of "-9999."
! Each record contains a 1440 x 480 REAL*4 array of data which is oriented
! from 0.125E EASTward and from 59.875N SOUTHward, with a grid increment
! of 0.25 degrees of latitude and longitude.  Thus, the grid locations
! are the centers on a 0.25 degree lat/lon grid box.  Note that these 
! estimates represent spatial averages, so the data are grid-centered, 
! rather than lattice-centered.
!
! For example (1,1) is 0.125E, 59.875N 
!             (2,2) is 0.375E, 59.625N, etc.
!
! There is an associated GrADS ctl file with this data set:
! CMORPH+MWCOMB_025deg-3hr.ctl 
!
! 
! Below is FORTRAN code (run on an SGI) that will read the data sets:
!
!------------------------------------------------------------------

      PROGRAM cmorph_025deg_tigge
      
      USE kinds
      
      IMPLICIT NONE

      INTEGER (KIND=int_kind), PARAMETER :: &
        IRMAX=1440, &        ! Meridional length of input data array
        JRMAX=480, &         ! Latitudinal length of input data array
        PMAX=2, &
        real_size = 4
      REAL (KIND=real_kind), PARAMETER :: &
        lon1loc = 0.125, &   ! Latitude location of first grid cell (degrees)
        lat1loc = 59.875, &  ! Longitude location of first grid cell (degrees)
        resol = 0.25         ! Latitude/longitude grid resolution (degrees)
      INTEGER (KIND=int_kind) :: &
        irec, &              ! Record counter for input data
        outrec, &            ! Record counter for output data
        io, &                ! IOSTAT for configuration file
        starthr, &           ! Start time for temporal subsetting (00Z, 06Z, 12Z, 18Z)
        endhr, &             ! End time for temporal subsetting (00Z, 06Z, 12Z, 18Z)
        lon1_ix, lon2_ix, &  ! Indices of longitude subset boundaries
        lat1_ix, lat2_ix, &  ! Indices of latitude subset boundaries
        IRSUB, JRSUB, &      ! Length of lat/lon subset dimensions
        inlength, ihr, &
        i, j, ireg, jreg
      REAL (KIND=real_kind) :: &
        lon, lat, lon1, lon2, lat1, lat2
      REAL (KIND=real_kind), DIMENSION(JRMAX) :: &
        lat_grid             ! Latitude grid centers (degrees)
      REAL (KIND=real_kind), DIMENSION(IRMAX) :: &
        lon_grid             ! Longitude grid centers (degrees)
      REAL*4, DIMENSION(IRMAX, JRMAX, PMAX) :: &
        ssmipropREG          ! Input data array
      REAL (KIND=real_kind), ALLOCATABLE, DIMENSION(:,:) :: &
        cmorphOUT            ! Output data array
      CHARACTER (LEN=120) :: &
        argv, &              ! Input argument array
        infile, &            ! Input data file (path + file name)
        outfile, &           ! Output data file (path + file name)
        config               ! Configuration file (path + file name)
      CHARACTER (LEN=101) :: &
        crec
      CHARACTER (LEN=4) :: &
        id

!------------------------------------------------------------------

      irec = 0
      outrec = 0

!------------------------------------------------------------------
!      Input arguments:
!      1. Input data file (path + file name)
!      2. Output data file (path + file name)
!      3. configuration file (path + file name)

!      call getarg(1, argv)
!      infile = argv
!      call getarg(2, argv)
!      outfile = argv
!      call getarg(3, argv)
!      config = argv

      infile = '/glade/data02/dsswork/tcram/tigge/cmorph/data/025deg/20110616_3hr-025deg_cpc+comb'
      outfile = '/glade/data02/dsswork/tcram/tigge/cmorph/data/025deg/cmorph_20110616_out'
      config = '/glade/data02/dsswork/tcram/tigge/cmorph/config/cmorph_config'

!------------------------------------------------------------------

      starthr = 0
      endhr   = 18
      lon1    = lon1loc
      lon2    = 359.875
      lat1    = lat1loc
      lat2    = -59.875

!------------------------------------------------------------------
!	open the configuration file and read in lat/lon boundaries

      open (unit=10, file=config, form='formatted')
      do i = 1,10
        read(10, '(A100)', iostat=io) crec
        if (io < 0) exit
        inlength=len_trim(crec)-4
        select case (crec(1:4))
          case ("LATS")
            read (crec,*) id,lat1,lat2
            print *,"Latitude Values: ",lat1,lat2
          case ("LONS")
            read (crec,*) id,lon1,lon2
            print *,"Longitude Values: ",lon1,lon2
          case ("HOUR")
            read (crec,*) id,starthr,endhr
            print *,"Start and end hours: ",starthr,endhr
        end select
      end do

!------------------------------------------------------------------
! Define the CMORPH latitude and longitude grids (0.25 deg resolution)
! and determine the array indices of the lat/lon grid subset boundary

      lon1_ix = 1
      lon2_ix = IRMAX
      lat1_ix = 1
      lat2_ix = JRMAX
      
      do i = 1, IRMAX
        lon = lon1loc + (i-1)*resol
        lon_grid(i) = lon
!        if (lon1 .eq. lon) then
!          lon1_ix = i
!        else if (lon1 .ge. lon) then
!          lon1_ix = i+1
!        endif
!        if (lon2 .ge. lon) lon2_ix = i
      enddo
      do j = 1, JRMAX
        lat = lat1loc - (j-1)*resol
        lat_grid(j) = lat
!        if (lat1 .eq. lat) then 
!          lat1_ix = j
!        else if (lat1 .le. lat) then
!          lat1_ix = j+1
!        endif
!        if (lat2 .le. lat) lat2_ix = j
      enddo

      lon1_ix = MINLOC(lon_grid, DIM = 1, MASK = lon_grid .GE. lon1)
      lon2_ix = MAXLOC(lon_grid, DIM = 1, MASK = lon_grid .LE. lon2)
      lat1_ix = MAXLOC(lat_grid, DIM = 1, MASK = lat_grid .LE. lat1)
      lat2_ix = MINLOC(lat_grid, DIM = 1, MASK = lat_grid .GE. lat2)
      
      IRSUB = lon2_ix - lon1_ix + 1
      JRSUB = lat2_ix - lat1_ix + 1
      
      write(6,*) 'IRSUB, JRSUB = ',IRSUB,JRSUB
      write(6,*) 'lon1_ix, lon2_ix = ',lon1_ix, lon2_ix
      write(6,*) 'lat1_ix, lat2_ix = ',lat1_ix, lat2_ix
      write(6,*) ''
      write(6,*) 'latitude subset = ',lat_grid(lat1_ix:lat2_ix)
      write(6,*) ''
      write(6,*) 'longitude subset = ',lon_grid(lon1_ix:lon2_ix)
      
      ALLOCATE (cmorphOUT(IRSUB, JRSUB)) 
      
!------------------------------------------------------------------
!    Open input regional blended sensor rainfall file for 3
!      hour CPC micro rainfall file

      write(6,'('' Opening input file '',a120)') infile

!    Open input data file.  Notify compiler that we're reading from
!    big endian records.
      open (unit=15, file=infile, access='direct', &
            recl=IRMAX*JRMAX*PMAX*real_size, form='unformatted', &
            convert='big_endian')

!    Output data file.
      open (unit=95, file=outfile, access = 'direct', &
            form='unformatted', recl=IRSUB*JRSUB*real_size)

!    Loop over 8 3 hourly periods for one day
      do ihr = 0, 21, 3
        irec = irec + 1

        write(6,'(/,''reading 3-hourly data at hour '',i4)') ihr
        read (15, rec = irec) ssmipropREG

!     Check data
       write (6,'(/,'' cpc combined MW '')')
       do jreg = 271, 280
        write(6,'(15(f12.1))') (ssmipropREG(ireg,jreg,1),ireg=1161,1175)
       enddo
       write (6,'(/,'' CMORPH '')')
       do jreg = 271, 280
        write(6,'(15(f12.1))') (ssmipropREG(ireg,jreg,2),ireg=1161,1175)
       enddo

! Retain data only at synoptic times (00Z, 06Z, 12Z, 18Z)
        if ((mod(ihr, 6) .ne. 0) .or. &
            (ihr .lt. starthr) .or. &
            (ihr .gt. endhr)) cycle

! Write subset data        
        do i = lon1_ix, lon2_ix
        do j = lat1_ix, lat2_ix
          cmorphOUT(i-lon1_ix+1,j-lat1_ix+1) = ssmipropREG(i,j,2)
        enddo
        enddo
        
        write(6,'(/,''writing data for hour '',i4)') ihr
        outrec = outrec + 1
        write(95, rec=outrec) cmorphOUT

      enddo  ! end ihr loop
      
      close (15)
      close (95)
      
      write(6,*) 'End program'

      END PROGRAM cmorph_025deg_tigge
