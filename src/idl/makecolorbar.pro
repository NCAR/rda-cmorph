;================================================================

PRO makecolorbar, startx, starty, colors, $
                   xwidth, yheight, labels, $
		    	   BORDERWIDTH = borderwidth, $
                   BORDERCOLOR = bordercolor, $
	           	   LABELCOLOR = labelcolor, $
	           	   LABEL_OFFSET = label_offset, $
		    	   CHARSIZE = charsize, $
		    	   CHARTHICK = charthick

;================================================================
; A procedure to plot a colorbar alongside a color figure.  Alignment
; is horizontal if xwidth > yheight; otherwise colorbar is vertically
; aligned.
;
; INPUT
;    startx = FLOAT = Normalized x-coordinate position of lower 
;                     left corner of colorbar
;
;    starty = FLOAT = Normalized y-coordinate position of lower
;                     left corner of colorbar
;
;    colors = LARR(n_colors) = Vector of color values
;
;    xwidth = FLOAT = Width in x-direction of colorbar, in
;                     normalized coordinates
;
;    yheight = FLOAT = Height in y-direction of colorbar, in
;                      normalized coordinates
;
;    labels = STRARR(n_colors+1) = String vector of colorbar labels. 
;             Plots character string at each color increment and also
;             at the ends of the colorbar; define accordingly.
;
; KEYWORD INPUT
;    BORDERWIDTH = FLOAT = Width of colorbar borders, in normalized 
;                  coordinates
;
;    BORDERCOLOR = LONG = Color of colorbar border
;
;    LABELCOLOR = LONG = Color of colorbar labels
;
;    LABEL_OFFSET = FLOAT = Normalized position of colorbar tick labels
;                   relative to edge of colorbar.  Default = 0.05.
;
;    CHARSIZE = FLOAT = Character size of colorbar labels
;================================================================

  IF N_ELEMENTS(bordercolor) EQ 0 THEN $
     bordercolor = 255L + 255L * 256L + 256L * 256L * 255L

  IF N_ELEMENTS(labelcolor) EQ 0 THEN labelcolor = bordercolor  
  IF NOT KEYWORD_SET(charsize) THEN charsize = 1.0
  
  IF N_ELEMENTS(label_offset) EQ 0 THEN label_offset = 0.05
  
  IF xwidth GT yheight THEN BEGIN
     align = 1
     IF NOT KEYWORD_SET(borderwidth) THEN borderwidth = yheight / 15.0
  ENDIF ELSE BEGIN
     align = 0
     IF NOT KEYWORD_SET(borderwidth) THEN borderwidth = xwidth / 10.0
  ENDELSE
  
  n_colors = N_ELEMENTS(colors)
  n_labels = N_ELEMENTS(labels)
  xlen = xwidth  / FLOAT(n_labels-1)
  ylen = yheight / FLOAT(n_labels-1)

  xcoords = [ startx - borderwidth, $
              startx + xwidth + borderwidth, $
              startx + xwidth + borderwidth, $
              startx - borderwidth ]
  
  ycoords = [ starty - borderwidth, $
              starty - borderwidth, $
              starty + yheight + borderwidth, $
              starty + yheight + borderwidth ]

  POLYFILL, xcoords, ycoords, $
            COLOR = bordercolor, $
            /NORMAL

  IF align EQ 1 THEN BEGIN
     xcoords = [ startx, startx + xlen, startx + xlen, startx ]
     ycoords = [ starty, starty, starty + yheight, starty + yheight ]
  
     FOR i = 0, n_colors-1 DO BEGIN
       POLYFILL, xcoords + (i * xlen), ycoords, $
                 COLOR = colors[i MOD N_ELEMENTS(colors)], $
                 /NORMAL

       XYOUTS, startx + (i * xlen), $
               starty - borderwidth - label_offset, $
               labels[i], $
	           CHARSIZE = charsize, $
               COLOR = labelcolor, $
               CHARTHICK = charthick, $
               /NORMAL, ALIGN = 0.5
               
       IF (labels[i] NE ' ') AND (labels[i] NE '') THEN $
         PLOTS, [startx + (i * xlen), startx + (i * xlen)], $
                [starty - borderwidth, starty - borderwidth - 0.01], $
                /NORMAL, THICK = 2
     ENDFOR
       XYOUTS, startx + (n_colors * xlen), $
               starty - borderwidth - label_offset, $
               labels[n_labels-1], $
	           CHARSIZE = charsize, $
               COLOR = labelcolor, $
               CHARTHICK = charthick, $
               /NORMAL, ALIGN = 0.5
       
       IF (labels[n_labels-1] NE ' ') AND (labels[n_labels-1] NE '') THEN $
         PLOTS, [startx + (n_colors * xlen), startx + (n_colors * xlen)], $
                [starty - borderwidth, starty - borderwidth - 0.01], $
                /NORMAL, THICK = 2

  ENDIF ELSE BEGIN
     xcoords = [ startx, startx + xwidth, startx + xwidth, startx ]
     ycoords = [ starty, starty, starty + ylen, starty + ylen ]
     
       FOR i = 0, n_colors-1 DO BEGIN
         POLYFILL, xcoords, ycoords + (i * ylen), $
                   COLOR = colors[i MOD N_ELEMENTS(colors)], $
                   /NORMAL

         XYOUTS, startx + xwidth + borderwidth + label_offset, $
                 starty + (i * ylen), $
                 labels[i], $
	             CHARSIZE = charsize, $
                 COLOR = labelcolor, $
                 CHARTHICK = charthick, $
                 /NORMAL, ALIGN = 0.5

       IF (labels[i] NE ' ') AND (labels[i] NE '') THEN $
         PLOTS, [startx + xwidth + borderwidth, $
                 startx + xwidth + borderwidth + 0.01], $
                [starty + (i * ylen), starty + (i * ylen)], $
                /NORMAL, THICK = 2
       ENDFOR
         XYOUTS, startx + xwidth + borderwidth + label_offset, $
                 starty + (n_colors * ylen), $
                 labels[n_labels-1], $
	             CHARSIZE = charsize, $
                 COLOR = labelcolor, $
                 CHARTHICK = charthick, $
                 /NORMAL, ALIGN = 0.5

       IF (labels[n_labels-1] NE ' ') AND (labels[n_labels-1] NE '') THEN $
         PLOTS, [startx + xwidth + borderwidth, $
                 startx + xwidth + borderwidth + 0.01], $
                [starty + (n_colors * ylen), starty + (n_colors * ylen)], $
                /NORMAL, THICK = 2

  ENDELSE
  
  RETURN

END

