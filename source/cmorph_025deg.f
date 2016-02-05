c-----7---------------------------------------------------------------72
c
c                 CMORPH 0.25 degree 3-hourly Information
c
c                    Last Update: February 13 2007
c
c The 0.25 degree 3-hourly CMORPH can be found on the ftp.cpc.ncep.noaa.gov
c server in the precip/global_CMORPH/3-hourly_025deg directory
c
c
c The data are compressed using the standard Unix compress function (files have a
c suffix of ".Z"). Each file is composed of 16 direct access binary
c ("big_endian") records that are defined as follows:
c
c Record 1: contains the merged microwave precipitation only for 00 UTC
c Record 2: contains the "CMORPH" precipitation estimates for 00 UTC
c
c Record 3: contains the merged microwave precipitation only for 03 UTC
c Record 4: contains the "CMORPH" precipitation estimates for 03 UTC
c .
c .
c .
c Record 15: contains the merged microwave precipitation only for 21 UTC
c Record 16: contains the "CMORPH" precipitation estimates for 21 UTC
c
c All units are "mm/hr".  Missing data are denoted by values of "-9999."
c Each record contains a 1440 x 480 REAL*4 array of data which is oriented
c from 0.125E EASTward and from 59.875N SOUTHward, with a grid increment
c of 0.25 degrees of latitude and longitude.  Thus, the grid locations
c are the centers on a 0.25 degree lat/lon grid box.  Note that these 
c estimates represent spatial averages, so the data are grid-centered, 
c rather than lattice-centered.
c
c For example (1,1) is 0.125E, 59.875N 
c             (2,2) is 0.375E, 59.625N, etc.
c
c There is an associated GrADS ctl file with this data set:
c CMORPH+MWCOMB_025deg-3hr.ctl 
c
c 
c Below is FORTRAN code (run on an SGI) that will read the data sets:
c
c-----7---------------------------------------------------------------72

      program cmorph_025deg

      parameter (IRMAX=1440, JRMAX=480, PMAX=2)

      real*4 ssmipropREG(IRMAX,JRMAX,PMAX)
      character*115 in1ssmipropREG

      iyr = 2011
      iyrmndy = 20110901
      irec = 0

c    Open input regional blended sensor rainfall file for 3
c      hour CPC micro rainfall file

      in1ssmipropREG = '/glade/data02/dsswork/tcram/tigge/cmorph/data/'
     &//'025deg/YYYY/YYYY0M0D_3hr-025deg_cpc+comb'

      write (in1ssmipropREG(54:57),'(i4)') iyr
      write (in1ssmipropREG(59:66),'(i8)') iyrmndy
      write(6,'('' file '',a88)') in1ssmipropREG

      open (unit=15, file=in1ssmipropREG, access='direct',
     &              recl=1440*480*2*4, form='unformatted',
     &              convert='big_endian')

c    Loop over 8 3 hourly periods for one day
      do ihr = 0, 21, 3

       irec = irec + 1

       write(6,'(//,''proc 3 hr starting '',i8,i4)') iyrmndy, ihr

       read (15, rec = irec) ssmipropREG

       write (6,'(/,'' cpc combined MW '')')
       do jreg = 271, 280
        write(6,'(15(f12.1))') (ssmipropREG(ireg,jreg,1),ireg=1161,1175)
       enddo

       write (6,'(/,'' CMORPH '')')
       do jreg = 271, 280
        write(6,'(15(f12.1))') (ssmipropREG(ireg,jreg,2),ireg=1161,1175)
       enddo

c    End 3 hour loop

      enddo

      close (15)

      stop

      end
