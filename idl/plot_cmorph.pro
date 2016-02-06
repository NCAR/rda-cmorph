IRMAX = 1440
JRMAX = 480

ssmipropREG = FLTARR(IRMAX, JRMAX)

year  = '2011'
month = '06'
day   = '17'
hour  = '00'
time    = year + month + day + hour

rootdir = '/glade/data02/dsswork/tcram/tigge/cmorph'
datadir = rootdir + '/data/025deg/'
figdir  = rootdir + '/fig/025deg/'
file    = 'CMORPH_025deg_' + time

record_length = IRMAX*JRMAX*4
OPENR, lun, datadir + file, record_length, /GET_LUN

;-----------------------------------------------------------------------
SET_PLOT, 'ps'
DEVICE, /INCHES, $
        XSIZE = 7.5, XOFFSET = 0.5, $
        YSIZE = 9.0, YOFFSET = 0.5, $
        /COLOR, $
        FILENAME = figdir + 'cmorph_025deg+' + time + '.ps'
        
!P.MULTI = [0, 1, 2]

;-----------------------------------------------------------------------
; Set up color table

n_colors = N_ELEMENTS(colors)

IF n_colors EQ 0 THEN BEGIN
  red   = [0L, 100L,   0L,  50L,   0L,   0L,  50L, 255L, 255L, 255L, 255L]
  green = [0L,  50L,   0L,  50L, 125L, 120L, 200L, 255L, 175L,  50L, 255L]
  blue  = [0L, 150L, 175L, 255L, 100L,   0L,  50L,   0L,   0L,   0L, 255L]

  n_colors = N_ELEMENTS(red)
  colors = LINDGEN(n_colors)
  TVLCT, red, green, blue
ENDIF

c_colors = [n_colors-1, colors[1:*]]

IF N_ELEMENTS(background) EQ 0 THEN background = n_colors-1

;-----------------------------------------------------------------------

n_x = IRMAX
n_y = JRMAX
delta_lon = 0.25
delta_lat = 0.25

lon_start = 0.125   ; East longitude
lat_start = 59.875  ; North latitude

lon_grid = lon_start + FINDGEN(n_x) * delta_lon
lat_grid = lat_start - FINDGEN(n_y) * delta_lat

xrange = [-180., 180.]
yrange = [-60., 60.]

main_title = 'CMORPH precipitation estimate (mm/h): '

;-----------------------------------------------------------------------

FOR ihlfhr = 0, 1 DO BEGIN
  READU, lun, ssmipropREG
  field = ssmipropREG
  
;IF N_ELEMENTS(levels) EQ 0 THEN BEGIN
;   floor = 1.0
;   ceil  = FLOAT( CEIL(MAX(field)) )
;   dl = (ceil - floor) / (n_colors-2)
;   levels = FINDGEN(n_colors-1)*dl + floor
   levels = [0.0, 1.0, 2.0, 3.0, 5.0, 7.5, 10.0, 15.0, 20.0, 25.0, 30.0]
   labels = STRCOMPRESS(STRING(levels, FORMAT='(G8.4)'), /REMOVE_ALL)
;ENDIF

;IF N_ELEMENTS(labels) EQ 0 THEN $
   labels = STRCOMPRESS(STRING(levels, FORMAT='(I)'), /REMOVE_ALL)

FOR i = 1, N_ELEMENTS(labels)-1, 2 DO labels[i] = ' '

charthick = 1

IF N_ELEMENTS(lon_center) NE 1 THEN lon_center = 0.
IF N_ELEMENTS(lat_center) NE 1 THEN lat_center = 0.

IF N_ELEMENTS(xrange) NE 2 THEN xrange = [-180., 180.]
IF N_ELEMENTS(yrange) NE 2 THEN yrange = [ -90., 90.]
s = SIZE(xrange)

limit = [ MIN(yrange), MIN(xrange), MAX(yrange), MAX(xrange) ]

field = REVERSE(field, 2)
lat_grid2 = REVERSE(lat_grid)

; Re-arrange lon_grid and the meridional dimension of the plot field

field2 = FLTARR(IRMAX, JRMAX)
where_east = WHERE(lon_grid LE 180., count_east)
where_west = WHERE(lon_grid GT 180., count_west)

field2[0:count_west-1, *]     = field[where_west, *]
field2[count_west:IRMAX-1, *] = field[where_east, *]

lon_grid2 = [lon_grid[where_west]-360., lon_grid[where_east]]

min_value = 0.0

title = main_title + hour + STRCOMPRESS(STRING(ihlfhr*30, FORMAT='(I2.2)'), /REMOVE_ALL) + $
        'Z ' + day + '/' + month + '/' + year

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

ENDFOR

;-----------------------------------------------------------------------

CLOSE, lun
FREE_LUN, lun
DEVICE, /CLOSE

END
