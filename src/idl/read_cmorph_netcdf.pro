datadir = '/glade/data02/dsswork/tcram/tigge/cmorph/data/025deg/netcdf/'
ncfile = datadir + 'cmorph.20110901.nc'

print, 'ncfile = ' + ncfile
cdfid = NCDF_OPEN(ncfile)

glob = NCDF_INQUIRE(cdfid)

; Get size of each dimension
PRINT, 'Dimensions', glob.ndims

FOR i = 0, glob.ndims-1 DO BEGIN
   NCDF_DIMINQ, cdfid, i, name, size
   IF i EQ glob.recdim THEN $
      PRINT, '   ', name, size, ' (Unlimited dim)' $
   ELSE $
      PRINT, '   ', name, size
ENDFOR

; Get variable info
PRINT
PRINT, 'Variables'

var_names = STRARR(glob.nvars)

FOR i = 0, glob.nvars-1 DO BEGIN
   
   ; Get information about the variable
   info = NCDF_VARINQ(cdfid, i)
   fmtstr = '(A," (", A, ") Dimension Ids = [ ", 10(I0, " "), $)'
   PRINT, FORMAT = fmtstr, info.name, info.datatype, info.dim[*]
   PRINT, ']'

   var_names[i] = info.name
   
   ; Get attributes associated with the variable
   FOR j = 0, info.natts-1 DO BEGIN
      attname = NCDF_ATTNAME(cdfid, i, j)
      NCDF_ATTGET, cdfid, i, attname, attvalue
      PRINT, '     Attribute ', attname, ' = ', STRING(attvalue)
   ENDFOR
ENDFOR

; Get data
NCDF_VARGET, cdfid, 'lon', lon 
NCDF_VARGET, cdfid, 'lat', lat
NCDF_VARGET, cdfid, 'time', time 
NCDF_VARGET, cdfid, 'mmw_precip', mmw_precip 
NCDF_VARGET, cdfid, 'cmorph_precip', cmorph_precip

END
