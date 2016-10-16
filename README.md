# GpxTrk2Rte

## Track format
- gpx
    - metadata
		- name
        - desc
		- author
		- keywords
		- time
		- bounds
    - trk
		- name
		- desc
		- trkseg
			- trkpt [lat, lon]
				- ele
				- time
			- trkpt
				- ele
				- time
			- ..

## RTE format
- gpx
	- rte
		- name
		- number
		- extensions
			- kashmr3d:line\_color
			- kashmr3d:line\_size
			- kashmr3d:line\_style
		- rtept [lat, lon]
			- ele
			- time
			- name
			- extensions
				- kashimr3d:icon
		- rtept ..

## Time format

ISO8601 standard format. (UTC)

 ex) 2016-09-17T13:46:50Z
