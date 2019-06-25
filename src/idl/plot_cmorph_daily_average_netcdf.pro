; Plots daily averages of CMORPH estimated precipitation

year  = '2012'
month = '02'
day   = '16'
date  = year + month + day

rootdir = '/glade/data02/dsswork/tcram/tigge/cmorph'
datadir = '/glade/data02/dsszone/ds502.0/netcdf/' + year
figdir  = rootdir + '/fig/025deg/'
ncfile    = datadir + '/cmorph.3hr-025deg.' + date + '.nc'

;-----------------------------------------------------------------------
SET_PLOT, 'ps'
DEVICE, /INCHES, $
        XSIZE = 9.0, XOFFSET = 2.5, $
        YSIZE = 4.0, YOFFSET = 10.0, $
        /LANDSCAPE, $
        /COLOR, $
        FILENAME = figdir + 'cmorph_025deg_' + date + '_avg.ps'
        
;!P.MULTI = [0, 1, 2]

position = [0.1, 0.1, 0.9, 0.9]

xsize = !D.X_SIZE
ysize = !D.Y_SIZE
!P.FONT = 0

;-----------------------------------------------------------------------
; Set up color table

n_colors = N_ELEMENTS(colors)

;IF n_colors EQ 0 THEN BEGIN
;  red   = [0L, 100L, 125L,   0L,  50L,   0L,   0L,  50L, 255L, 255L, 255L, 255L]
;  green = [0L,  50L,  75L,   0L,  50L, 125L, 120L, 200L, 255L, 175L,  50L, 255L]
;  blue  = [0L, 150L, 175L, 175L, 255L, 100L,   0L,  50L,   0L,   0L,   0L, 255L]

;  n_colors = N_ELEMENTS(red)
;  colors = LINDGEN(n_colors)
;  TVLCT, red, green, blue
;ENDIF

LOADCT, 39
n_colors = 14
colors = LINDGEN(n_colors) * LONG(255./FLOAT(n_colors-2))

c_colors = [255, colors[1:*]]

IF N_ELEMENTS(background) EQ 0 THEN background = n_colors-1

;-----------------------------------------------------------------------

xrange = [-180., 180.]
yrange = [-60., 60.]

;-----------------------------------------------------------------------

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
NCDF_VARGET, cdfid, 'lon', lon_grid 
NCDF_VARGET, cdfid, 'lat', lat_grid
NCDF_VARGET, cdfid, 'time', time 
NCDF_VARGET, cdfid, 'mmw_precip', mmw_precip 
NCDF_VARGET, cdfid, 'cmorph_precip', cmorph_precip

; Get missing data value
NCDF_ATTGET, cdfid, 4, 'missing_value', missing

s_lon = SIZE(lon_grid)
s_lat = SIZE(lat_grid)
s_time = SIZE(time)

n_lon = s_lon[1]
n_lat = s_lat[1]
n_time = s_time[1]

main_title = 'CMORPH daily average precipitation: '

months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
month_str = months[FIX(month)-1]

;-----------------------------------------------------------------------
mmwREG    = FLTARR(n_lon, n_lat)
mmwCNT    = FLTARR(n_lon, n_lat)
mmwAVG    = FLTARR(n_lon, n_lat)
cmorphREG = FLTARR(n_lon, n_lat)
cmorphCNT = FLTARR(n_lon, n_lat)
cmorphAVG = FLTARR(n_lon, n_lat)

FOR ihr = 0, n_time-1 DO BEGIN
  
  FOR i = 0, n_lon-1 DO BEGIN
  FOR j = 0, n_lat-1 DO BEGIN
    IF (mmw_precip[i,j,ihr] NE missing) THEN BEGIN
      mmwREG[i,j] = mmwREG[i,j] + mmw_precip[i,j,ihr]
      mmwCNT[i,j] = mmwCNT[i,j] + 1
    ENDIF
    IF (cmorph_precip[i,j,ihr] NE missing) THEN BEGIN
      cmorphREG[i,j] = cmorphREG[i,j] + cmorph_precip[i,j,ihr]
      cmorphCNT[i,j] = cmorphCNT[i,j] + 1
    ENDIF
  ENDFOR
  ENDFOR
  
; Compute daily average, convert units to mm/day
FOR i = 0, n_lon-1 DO BEGIN
FOR j = 0, n_lat-1 DO BEGIN
  mmwAVG[i,j] = 24. * mmwREG[i,j] / mmwCNT[i,j]
  cmorphAVG[i,j] = 24. * cmorphREG[i,j] / cmorphCNT[i,j]
ENDFOR
ENDFOR

ENDFOR
;-----------------------------------------------------------------------

levels = [0.0, 1.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 50.0, 75.0, 100.0, 125.0, 150.0, 10000.0]
labels = STRCOMPRESS(STRING(levels, FORMAT='(G8.4)'), /REMOVE_ALL)

labels = STRCOMPRESS(STRING(levels, FORMAT='(I)'), /REMOVE_ALL)
n_labels = N_ELEMENTS(labels)
labels[n_labels-1] = ''

charthick = 1

IF N_ELEMENTS(lon_center) NE 1 THEN lon_center = 0.
IF N_ELEMENTS(lat_center) NE 1 THEN lat_center = 0.

IF N_ELEMENTS(xrange) NE 2 THEN xrange = [-180., 180.]
IF N_ELEMENTS(yrange) NE 2 THEN yrange = [ -90., 90.]
s = SIZE(xrange)

limit = [ MIN(yrange), MIN(xrange), MAX(yrange), MAX(xrange) ]

field = REVERSE(cmorphAVG, 2)
lat_grid2 = REVERSE(lat_grid)

; Re-arrange lon_grid and the meridional dimension of the plot field

field2 = FLTARR(n_lon, n_lat)
where_east = WHERE(lon_grid LE 180., count_east)
where_west = WHERE(lon_grid GT 180., count_west)

field2[0:count_west-1, *]     = field[where_west, *]
field2[count_west:n_lon-1, *] = field[where_east, *]

lon_grid2 = [lon_grid[where_west]-360., lon_grid[where_east]]

min_value = 1.0

title = main_title + day + ' ' + month_str + ' ' + year

MAP_SET, lat_center, lon_center, $
         NAME = proj_name, $
         POSITION = position, $
         ISOTROPIC = isotropic, $
         HORIZON = horizon, $
         LIMIT = limit, $
         NOBORDER = noborder, $
         TITLE = title, $
         NOERASE = noerase, $
         CONTINENTS = continents

CONTOUR, field2, $
         lon_grid2, lat_grid2, $
         LEVELS = levels, $
         /CELL_FILL, $
         /CLOSED, $
         C_COLORS = c_colors, $
         COLOR = 0, $
         /OVERPLOT, $
         MIN_VALUE = min_value, $
         MAX_VALUE = max_value

MAP_GRID, COLOR = colors[0]
MAP_CONTINENTS, COLOR = colors[0]

; Plot colorbar
  bordercolor = 0
  startx = position[0] + 0.05
  starty = position[1] - 0.05
  xwidth = position[2] - position[0] - 0.1
  yheight = 0.03
  cb_title_offset = 0.1

  cb_colors = c_colors[1:n_colors-2]
  makecolorbar, startx, starty, cb_colors, $
                xwidth, yheight, labels[1:*], $
                BORDERCOLOR = bordercolor, $
                CHARTHICK = charthick

  colorbar_title = 'mm/day'

  XYOUTS, 0.5, starty - cb_title_offset, colorbar_title, /NORMAL, ALIGN = 0.5, $
          CHARTHICK = charthick, COLOR = 0L, CHARSIZE = charsize

DEVICE, /CLOSE

END
