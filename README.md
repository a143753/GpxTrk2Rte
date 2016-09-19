# GpxTrk2Rte

## Track format
gpx
 +- metadata
   +- name
   +- desc
   +- author
   +- keywords
   +- time
   +- bounds
 +- trk
   +- name
   +- desc
   +- trkseg
     +- trkpt [lat, lon]
       +- ele
       +- time
     +- trkpt
     :

## RTE format
gpx
 +- rte
   +- name
   +- number
   +- extensions
     +- kashmr3d:line_color
     +- kashmr3d:line_size
     +- kashmr3d:line_style
   +- rtept [lat, lon]
     +- ele
     +- time
     +- name
     +- extensions
       +- kashimr3d:icon
       
     