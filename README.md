# HawaiiDownscaling
Process NetCDF files into raster (TIF) files for Dynamical Downscaling, Hawaii.

Input files do not have lat/lon assigned as dimensions (instead they use "west_east" and "south_north").
These codes are used to assign lat/lon values to each time step and generate raster (TIF) files from the NetCDF.

Input starts as 4 separate islands, 2 variables (rainfall and temperature), and 3 time periods (present day 1990-2009, and 
future under RCP 4.5 and RCP 8.5 CMIP5 scenarios). Outputs are single files for statewide monthly present day & future rainfall/temp, 
resmapled to 250 m and masked to the coastline.

Derived seasonal variables are calculated: percent change, absolute change, and future means. A bias correction is applied using the Delta Change Approach, where the future change is applied to observation-based present day means. 

