c-----7---------------------------------------------------------------72
c 		  CMORPH ~ 8km, 1/2 hourly data description.  
c
c 			Last Update: June 22, 2010
c
c The 8-km 30 minute CMORPH data sets can be found on the ftp.cpc.ncep.noaa.gov
c server in the precip/global_CMORPH/30min_8km directory
c
c The data are compressed using the standard Unix compress function (files have a
c suffix of ".Z")
c 
c Each file contains 6 records.  The 1st 3 records pertain to the top half
c of the hour (00-29 minus after the hour) and the last 3 records are for the
c bottom half of the hour.  Within each group:
c
c   -  the 1st record contains the CMORPH precipitation estimates 
c  
c   -  the 2nd record contains the time (in half hour units) since the most
c      recent microwave pass.  Note that since we do both a forward &
c      backward interpolation in time, the nearest time may be prior to
c      the file time stamp or after it.  
c      
c   -  the 3rd record contains an ID that tells the satellite from which the last
c      microwave observation was made which can be interpretted by the following
c      table (as of the time of the last update of this documentation):
c      
c        13 = DMSP-13 (SSM/I instrument)
c        14 = DMSP-14 (  "       "      )
c        15 = DMSP-15 (  "       "      )
c        16 = DMSP-16 (SSMIS instrument, coming soon)
c        17 = DMSP-17 ( " " )
c        18 = DMSP-18 ( " " ) 
c       115 = NOAA-15 (AMSU-B    "      )
c       116 = NOAA-16 (  "       "      )
c       117 = NOAA-17 (  "       "      )
c       118 = NOAA-18 (MHS )
c       119 = NOAA-19 ( " " )
c       151 = METOP-A ( " " )
c       201 = TRMM    (TMI       "      )
c       211 = AQUA    (AMSR-E    "      )
c 
c
c Each direct access record is a 4948 x 1649 CHARACTER*1 (use FORTRAN ichar
c command to retrieve interger value for all parameters) array with  grid
c increments of 0.072756669 degrees of longitude and 0.072771377 of latitude,
c which is apporoximately 8 km at the equator.  The arrays are oriented from
c North to South, beginning from latitude 59.963614N and from West to EAST from
c longitude  0.036378335E.
c
c Missing data are denoted by values of 255.
c
c Note that the precipitation estimates have been scaled, and when multiplied by
c "0.2", the data units are "mm/hour".
c
c For GrADS users, a descriptor ("ctl") file: CMORPH_8km-30-minute.ctl has been
c provided.  However, since the data are in CHARACTER*1 words (bytes) the parameters
c after each variable (-1,40,1,-1 in our example "ctl file") are system dependent.  
c Our example is for an SGI system.
c
c-----7---------------------------------------------------------------72

c The following program reads 8km CMORPH, averages into 0.25 deg lat/lon regions.
 
      program cmorph_8km
c    Program CMORPH_8-km-30min_read.f
c    Average already made 8 km , half hourly CMORPH into 025 degree and write out to files

      parameter (I8KMMAX=4948,J8KMMAX=1649,XMISSING=330,AMISSING=-9999,
     & ZERO=0,ONE=1,TEN=10,HOURMX=24,inunit=10,inunit2=15,ioutunit=70,
     & MTHMAX=12, IRMAX=1440, JRMAX=480, PIXCNTTHRESH=9, XLATMAX=60.0,
     & XLAT1STPIXLOC=59.963614, XLATINCR= 0.072771377, XLON1STPIX=
     & 0.036378335, XLONINCR=0.072756669, MISSING=-9999)

      character*1 ssmigrid(I8KMMAX,J8KMMAX),
     &  satid(I8KMMAX,J8KMMAX), timestamp(I8KMMAX,J8KMMAX)
    
      character*120 in1ssmi, out025d
      integer       imndy(MTHMAX), CMISSING

      real*4 ssmipropREG(IRMAX,JRMAX), ssmipropREGCNT(IRMAX,JRMAX)
  
      CMISSING = 255
      xres     = 0.25

      write (*,*) ' enter start & end day, i.e. 20050802 20050802'
      read (*,*) iyrmndy1, iyrmndy2
      write (*,*) ' start & end day entered ', iyrmndy1, iyrmndy2

      do iyrmndy = iyrmndy1, iyrmndy2

      do ihr = 0, 0 
 
c    Open 8 km half hourly CMORPH
 
      in1ssmi='/glade/data02/dsswork/tcram/tigge/cmorph/data/8km/'
     &//'advt-8km-interp-prim-sat-spat-2lag-2.5+5dovlp8kmIR-YYYY0M0D0H'
      write (in1ssmi(102:109),'(i8)') iyrmndy
      if (ihr.lt.10) then
       write (in1ssmi(111:111),'(i1)') ihr
      else
       write (in1ssmi(110:111),'(i2)') ihr
      endif
      open (unit=10, file=in1ssmi, access = 'direct',
     &            form='unformatted', recl=4948*1649*3)
      write(6,'(/,'' input comb 8-km advect ssmi '',a120)') in1ssmi

      out025d='/glade/data02/dsswork/tcram/tigge/cmorph/data/025deg/'
     &//'CMORPH_025deg_YYYY0M0D0H'
      write (out025d(68:75),'(i8)') iyrmndy
      if (ihr.lt.10) then
       write (out025d(77:77),'(i1)') ihr
      else
       write (out025d(76:77),'(i2)') ihr
      endif

      open (unit=95, file=out025d, access = 'direct',
     &            form='unformatted', recl=1440*480*4)

      write(6,'(/,'' output comb 025 deg advect ssmi '',a120)') out025d

c    Loop through  1. top of the hour global IR merge and
c                  2. mid hour global IR merged images

      do ihlfhr = 1, 2

c    Init half hourly 0.25 deg region arrays
 
       do jreg = 1, JRMAX
        do ireg = 1, IRMAX
         ssmipropREG(ireg,jreg)      =ZERO
         ssmipropREGCNT(ireg,jreg)   =ZERO
        enddo
       enddo

       write (6,'('' ihr, ihlfhr '',2i8)') ihr, ihlfhr

       if (ihlfhr.eq.1) then

        read (10, rec = 1) ssmigrid, timestamp, satid

c    Simply read in forward and backward advected SSMI scans

       else

        read (10, rec = 2) ssmigrid, timestamp, satid

       endif

       write (6,'('' real array ihalfhr = '',i8)') ihlfhr
       do j = 700, 720
        write (6,'(21i4)') (ichar(ssmigrid(i,j)),i=3501,3521)
       enddo


c    Accumulate microwave anal derived precip into
 
       do j = 1, J8KMMAX
        xlat1 = XLAT1STPIXLOC - (j-1)*XLATINCR
        jreg  = ((XLATMAX - xlat1)/xres + 1)
        do i = 1, I8KMMAX
         if (ssmigrid(i,j).ne.char(CMISSING)) then
c    Determine region of 8-km pixel
          xlon1 = XLON1STPIX + (i-1)*XLONINCR
          ireg  = (xlon1/xres + 1)

          if (ireg.lt.1.or.ireg.gt.IRMAX.or.
     &                    jreg.lt.1.or.jreg.gt.JRMAX) then
           write (6,'(''region out of bounds'',4i8)') i, ireg, j, jreg 
          else
 
           if (ssmigrid(i,j).ne.char(CMISSING)) then
            ssmipropREG(ireg,jreg)=ssmipropREG(ireg,jreg)+
     &                            ichar(ssmigrid(i,j))*0.2
            ssmipropREGCNT(ireg,jreg) =
     &                       ssmipropREGCNT(ireg,jreg) + 1
           endif
          endif
 
         endif
        enddo
       enddo
 
c    Average microwave derived precip into regional estimates and write to disk
 
       do jreg = 1, JRMAX
        do ireg = 1, IRMAX
         if (ssmipropREGCNT(ireg,jreg).ge.1) then
          ssmipropREG(ireg,jreg)=ssmipropREG(ireg,jreg)/
     &                           ssmipropREGCNT(ireg,jreg)
         else
          ssmipropREG(ireg,jreg) = MISSING
         endif
        enddo
       enddo

       write (6,*) ''
       write (6,'('' ssmipropREG @ ihalfhr = '',i8)') ihlfhr
       do j = 271, 280 
        write (6,'(21f12.6)') (ssmipropREG(i,j),i=1161, 1175)
       enddo

c    Write combined 0.5 hour SSMI precip array to disk

       if (ihlfhr.eq.1) then
        write (95, rec = 1) ssmipropREG
       else
        write (95, rec = 2) ssmipropREG
        close (95)
       endif
 
      enddo
 
      enddo
 
      enddo

      stop

      end
