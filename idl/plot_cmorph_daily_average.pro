; Plots daily averages of CMORPH estimated precipitation

IRMAX = 1440
JRMAX = 480

ssmigrid    = FLTARR(IRMAX, JRMAX)
ssmipropREG = FLTARR(IRMAX, JRMAX)
ssmipropCNT = FLTARR(IRMAX, JRMAX)
ssmipropAVG = FLTARR(IRMAX, JRMAX)

year  = '2011'
month = '06'
day   = '17'
date  = year + month + day

missing = -9999.0

rootdir = '/glade/data02/dsswork/tcram/tigge/cmorph'
datadir = rootdir + '/data/025deg/'
figdir  = rootdir + '/fig/025deg/'
file    = 'CMORPH_025deg_' + date

record_length = IRMAX*JRMAX*4

;-----------------------------------------------------------------------
SET_PLOT, 'ps'
DEVICE, /INCHES, $
        XSIZE = 7.5, XOFFSET = 0.5, $
        YSIZE = 4.0, YOFFSET = 3.0, $
        /COLOR, $
        FILENAME = figdir + 'cmorph_025deg_' + date + '_avg.ps'
        
;!P.MULTI = [0, 1, 2]

position = [0.1, 0.1, 0.9, 0.9]

xsize = !D.X_SIZE
ysize = !D.Y_SIZE

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

main_title = 'CMORPH daily average precipitation estimate (mm/day): '

;-----------------------------------------------------------------------

FOR ihr = 0, 23 DO BEGIN
  hour = STRCOMPRESS(STRING(ihr, FORMAT='(I2.2)'), /REMOVE_ALL)
  OPENR, lun, datadir + file + hour, record_length, /GET_LUN

FOR ihlfhr = 0, 1 DO BEGIN
  READU, lun, ssmigrid
  
  FOR i = 0, IRMAX-1 DO BEGIN
  FOR j = 0, JRMAX-1 DO BEGIN
    IF (ssmigrid(i,j) NE missing) THEN BEGIN
      ssmipropREG[i,j] = ssmipropREG[i,j] + ssmigrid[i,j]
      ssmipropCNT[i,j] = ssmipropCNT[i,j] + 1
    ENDIF
  ENDFOR
  ENDFOR
  
ENDFOR

FOR i = 0, IRMAX-1 DO BEGIN
FOR j = 0, JRMAX-1 DO BEGIN
  ssmipropAVG[i,j] = 24. * ssmipropREG[i,j] / ssmipropCNT[i,j]
ENDFOR
ENDFOR

;-----------------------------------------------------------------------

CLOSE, lun
FREE_LUN, lun

ENDFOR

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

field = REVERSE(ssmipropAVG, 2)
lat_grid2 = REVERSE(lat_grid)

; Re-arrange lon_grid and the meridional dimension of the plot field

field2 = FLTARR(IRMAX, JRMAX)
where_east = WHERE(lon_grid LE 180., count_east)
where_west = WHERE(lon_grid GT 180., count_west)

field2[0:count_west-1, *]     = field[where_west, *]
field2[count_west:IRMAX-1, *] = field[where_east, *]

lon_grid2 = [lon_grid[where_west]-360., lon_grid[where_east]]

min_value = 1.0

title = main_title + day + '/' + month + '/' + year

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
  starty = position[1] - 0.175
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
