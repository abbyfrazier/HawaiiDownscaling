#A single code to process downscaling...streamlined (adjust variables at top, should run for all islands & both variables)
#Temperature & Rainfall, BY ISLAND
#STEP 1: extract monthly rasters from NetCDF file
#STEP 2: resample to 250 m, Mask by Coastline (from Rainfall Atlas layer)

rm(list=ls())

library('raster')
library('ncdf4')
library('rgdal')
library('sp')
library('maptools') #for old shapefile read command
library(rasterVis) #for plots

isl_list<-c("ka","oa","ma","ha")
isl2_list<-c("kauai","oahu","maui","hawaii")
varb_list<-c("rf","temp")
varb2_list<-c("RAIN","T2")
varb3_list<-c("Rainfall","Temperature")
tm_list<-c("present","future","future")
tm2_list<-c("pres","rcp45","rcp85")
tm3_list<-c("present","rcp45","rcp85")

#Things to update before running code - specify island, variable (rain or temp), time (future rcp, present day)
#These now update automatically in the Loops (x,y,z)
#isl<-"ma"              #island abbreviation: ka, oa, ma, ha
#isl2<-"maui"          #full name of island: kauai, oahu, maui, hawaii
#varb<-"temp"           #temp or rf
#varb2<-"T2"            #T2 or RAIN
#varb3<-"Temperature"   #Temperature or Rainfall
#tm<-"future"          #present or future
#tm2<-"rcp85"            #pres or rcp45 or rcp85
#tm3<-"rcp85"         #present or rcp45 or rcp85

for (x in 1:4){  #loop through islands 1-4
  isl<-isl_list[x]
  isl2<-isl2_list[x]

  for (y in 1:2){  #loop through variables (rain & temp)
    varb<-varb_list[y]
    varb2<-varb2_list[y]
    varb3<-varb3_list[y]
    
    for (z in 1:3){  #loop through times (present, rcp45, rcp85)
      tm<-tm_list[z]
      tm2<-tm2_list[z]
      tm3<-tm3_list[z]
      
      
      #SET NUMBER OF COLUMNS AND ROWS FOR OUTPUT RASTERS: (Kauai: 82,60. Oahu: 97,70. Maui: 197,110. Big Island: 180,194.)  
      #and Open NetCDF file from Chunxi:  (file name / folder different for each island)
      if(isl2=="kauai"){
        setwd("H:/Downscaling/Dynamical/CMIP5/FromZhang_10_2016/")  #for Kauai & Oahu******
        nc<-nc_open(paste(isl2,"_",tm3,"_",varb2,".nc",sep=""), write=TRUE)
        isl.rc<-nc$var$XLAT$size  #get row and column sizes from NetCDF file
        isl.col<-as.numeric(isl.rc[1])
        isl.row<-as.numeric(isl.rc[2])
      }
      if(isl2=="oahu"){
        setwd("H:/Downscaling/Dynamical/CMIP5/FromZhang_10_2016/")  #for Kauai & Oahu******
        nc<-nc_open(paste(isl2,"_",tm3,"_",varb2,".nc",sep=""), write=TRUE)
        isl.rc<-nc$var$XLAT$size  #get row and column sizes from NetCDF file
        isl.col<-as.numeric(isl.rc[1])
        isl.row<-as.numeric(isl.rc[2])
      }
      if(isl2=="maui"){
        if(y==1){varb2="rain"} #maui & big island have lowercase RAIN dimension to read NetCDF...T2 is same
        setwd("H:/Downscaling/Dynamical/CMIP5/FromZhang_01_2018/")  #for Maui & Hawaii island (file name rearranged):*******
        nc<-nc_open(paste(isl2,"_",varb2,"_",tm3,".nc",sep=""), write=TRUE)
        isl.rc<-nc$var$XLAT$size  #get row and column sizes from NetCDF file
        isl.col<-as.numeric(isl.rc[1])
        isl.row<-as.numeric(isl.rc[2])
      }
      if(isl2=="hawaii"){
        if(y==1){varb2="rain"} #maui & big island have lowercase RAIN dimension to read NetCDF...T2 is same
        setwd("H:/Downscaling/Dynamical/CMIP5/FromZhang_01_2018/")  #for Maui & Hawaii island (file name rearranged):*******
        nc<-nc_open(paste(isl2,"_",varb2,"_",tm3,".nc",sep=""), write=TRUE)
        isl.rc<-nc$var$XLAT$size  #get row and column sizes from NetCDF file
        isl.col<-as.numeric(isl.rc[1])
        isl.row<-as.numeric(isl.rc[2])
      }
      
      
      #grab T2/RAIN for all time bands:
      array1<-ncvar_get(nc,varb2)  # T2 FOR TEMPERATURE, RAIN FOR RAINFALL
      dim(array1)
      
      #set directories
      allmonthrast.out<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_RawMonthlyTIF_byIsland/",isl2,"/",tm3,"/",sep="")
      allmonthrast.out250<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_MonthlyTIF_byIsland_250m/",isl2,"/",tm3,"/",sep="")

      #Values in each dimension of NetCDF file
      vLon = ncvar_get(nc, "XLON")
      vLat = ncvar_get(nc, "XLAT")
      
      #convert Lat/Lon grid into one long line of coords (data frame with 2 columns - lat & lon)
      #convert to matrix
      vLon_m = matrix(vLon)
      vLat_m = matrix(vLat)
      #convert to vector
      vLon_v = as.vector(t(vLon_m))
      vLat_v = as.vector(t(vLat_m))
      #combine into dataframe with coordinates:
      coords = data.frame(vLon_v,vLat_v)
      
      #go through all 240 time bands and add as columns in table to be plotted as rasters:
      for (i in 1:240){
        yrmo<-array1[,,i]
        yrmo_m = matrix(yrmo) #convert to matrix
        yrmo_v = as.vector(t(yrmo_m))  #convert to single line vector, transposing each row
        coords[,i+2]<-yrmo_v  #add new column to dataframe
        colnames(coords)[i+2]<-paste(isl,"_",varb,"_band_",i,sep="")  #update column name
      }
      
      #save data table just in case:  
      setwd(allmonthrast.out)
      write.table(coords,file=paste("AllRasters_Coords_",isl,"_",varb,"_",tm3,".txt",sep=""),sep="\t",row.names=FALSE)
      
      coords2=coords #make a copy
      
      #Loop through each time step and convert to raster, save as TIF
      #make new dataframe...convert to RASTER!  http://chris35wills.github.io/gridding_data/
      
      
      #open Rainfall Atlas grid to mask (island extent, not statewide file):
      setwd(paste("H:\\GIS_Layers\\RainfallAtlas\\RF_inches\\",isl2,"RFGrids_inches",sep=""))
      isl4=isl
      if(isl=="ma"){isl4="mc"} #maui rainfall atlas grids are called "maui county" or "mc"     
      if(isl=="ha"){isl4="bi"} #big island rainfall atlas grids are called "bi" not  "ha"
      rfa_mask<-raster(paste("rf_in_",isl4,"_ann",sep=""))
      P4S.latlon <- CRS(projection(rfa_mask))
      #open coast file:
      setwd("H:\\GIS_Layers\\coast_n83.shp")
      coast<-readShapePoly("coast_geo",proj4string=P4S.latlon)  #NEEDS TO BE UPDATED
      
      
      #GET OUTPUT FILE NAMES READY:
      if(tm=="present"){  
        styr=1990
        endyr=2009
      }
      if(tm=="future"){  
        styr=2080
        endyr=2099
      }
      yr_list=seq(from = styr, to = endyr)
      mo_list=c("_01","_02","_03","_04","_05","_06","_07","_08","_09","_10","_11","_12")
      yrmo_list=expand.grid(yr_list,mo_list)
      yrmo_list$C = paste(yrmo_list$Var1, yrmo_list$Var2, sep='')
      yrmo_list <- yrmo_list[order(yrmo_list$C),]  #default order is 1990_01, 1991_01, etc. -- Need it sorted by 1990_01, 1990_02...
      yrmo_names<-yrmo_list$C
      
      
      
      #Loop through each month and output as TIF in raw spatial resolution
      #  Output a 2nd raster resampled to 250m, masked to coastline
      for (i in 1:240){
        pts<-data.frame(coords2[,1:2],coords2[,i+2])
        colnames(pts)<-c("x","y","z")
        # create a SpatialPointsDataFrame
        coordinates(pts) = ~x+y 
        # create an empty raster object to the extent of the points
        rast <- raster(ncols=isl.col, nrows=isl.row,ext=extent(pts)) 
        # rasterize your irregular points 
        rasOut<-rasterize(pts, rast, pts$z, fun = mean) # we use a mean function here to regularly grid the irregular input points
        #rasOut
        #spplot(rasOut)+layer(sp.polygons(coast))  #plot
        
        #write raw monthly output as a geotiff
        setwd(allmonthrast.out)  #set directory for output DynDS_RawMonthlyTIF_byIsland
        fout=paste(isl,"_",varb,"_",tm2,"_",yrmo_names[i],".tif",sep="")
        writeRaster(rasOut, fout, format="GTiff",overwrite=TRUE)
        
        #resample to 250 m, mask to coast based on RF Atlas layer, and save as new TIF:
        r.in<-rasOut  #previous output becomes input to resample
        r.out<-resample(r.in,rfa_mask,method="bilinear")  #resample to 250 m (same resolution as RF Atlas raster)
        r.out2<-mask(r.out,rfa_mask)  #mask to coastline of rainfall atlas layer
        f.out=paste(isl,"_",varb,"_",tm2,"_",yrmo_names[i],"_250m.tif",sep="")
        setwd(allmonthrast.out250) #output directory DynDS_MonthlyTIF_byIsland_250m
        writeRaster(r.out2, f.out, format="GTiff",overwrite=TRUE)
      }


    } #end time loop
  }   #end variable loop
}     #end island loop

#THEN CREATE STATEWIDE FILES (MOSAIC)
#THEN CREATE ANNUAL MEANS FOR EACH YEAR, 
#THEN FINAL CONVERSIONS COMPARED TO PRESENT DAY (FUTURE CHANGE % AND INCHES, ETC.)


