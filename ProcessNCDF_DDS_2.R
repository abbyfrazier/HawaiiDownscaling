#Process Dynamical Downscaling NetCDF files into seasonal rasters
#for STEPS 1 & 2 --> see "ProcessNCDF_DDS_1.R" code
#STEP 3: Mosaic all islands into statewide files
#STEP 4: Create annual and seasonal means for each year

#open each month for each island, mosaic together into single statewide file for that month
rm(list=ls())

library('raster')
library('ncdf4')
library('rgdal')
library('sp')
library('maptools') #for old shapefile read command
library(rasterVis) #for plots

isl_list<-c("ka","oa","ma","ha")
isl2_list<-c("kauai","oahu","maui","hawaii")
varb_list<-c("RF","Temp")
varb2_list<-c("RAIN","T2")
varb3_list<-c("Rainfall","Temperature")
tm_list<-c("present","future","future")
tm2_list<-c("pres","rcp45","rcp85")
tm3_list<-c("present","rcp45","rcp85")



for (y in 1:2){  #loop through variables (rain & temp)
  varb<-varb_list[y]
  varb2<-varb2_list[y]
  varb3<-varb3_list[y]
  
  for (z in 1:3){  #loop through times (present, rcp45, rcp85)
    tm<-tm_list[z]
    tm2<-tm2_list[z]
    tm3<-tm3_list[z]
    

    #GET monthly FILE NAMES READY:
    if(tm2=="pres"){  
      styr=1990
      endyr=2009
    } else {
      styr=2080
      endyr=2099
    }
    yr_list=seq(from = styr, to = endyr)
    mo_list=c("_01","_02","_03","_04","_05","_06","_07","_08","_09","_10","_11","_12")
    yrmo_list=expand.grid(yr_list,mo_list)
    yrmo_list$C = paste(yrmo_list$Var1, yrmo_list$Var2, sep='')
    yrmo_list <- yrmo_list[order(yrmo_list$C),]  #default order is 1990_01, 1991_01, etc. -- Need it sorted by 1990_01, 1990_02...
    yrmo_names<-yrmo_list$C
    
    #open Rainfall Atlas statewide grid to set extent/size:
    setwd("H:\\GIS_Layers\\RainfallAtlas\\RF_inches\\State")
    rfa_mask<-raster("staterf_inann")
    P4S.latlon <- CRS(projection(rfa_mask))
    
    for (j in 1:240){ #loop through all months
      #create blank raster to merge into:
      rmerg <- raster(ncols=ncol(rfa_mask), nrows=nrow(rfa_mask),ext=extent(rfa_mask)) 
      
      #open each month for each island
      for (i in 1:4){
        isl<-isl_list[i]
        isl2<-isl2_list[i]
        #set directories
        allmonthrast.out250<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_MonthlyTIF_byIsland_250m/",isl2,"/",tm3,"/",sep="")
        f.in=paste(isl,"_",varb,"_",tm2,"_",yrmo_names[j],"_250m.tif",sep="")
        setwd(allmonthrast.out250) #directory DynDS_MonthlyTIF_byIsland_250m
        r1<-raster(f.in) #open island-month raster (yrmo_names[j])
        rmerg<-merge(rmerg,r1) #merge new raster onto blank statewide-extent raster to create new monthly statewide raster
      }
      
      #save new statewide raster for this month:
      f.out=paste("State_",varb,"_",tm2,"_",yrmo_names[j],"_250m.tif",sep="")
      MonthlyStateOut<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_MonthlyTIF_State_250m/",tm3,"/",sep="")
      setwd(MonthlyStateOut) #output directory DynDS_MonthlyTIF_State_250m
      writeRaster(rmerg, f.out, format="GTiff",overwrite=TRUE)
    
    } #now onto next month

  }
}


#*****************************************************************************************************************************
#*****************************************************************************************************************************
#*****************************************************************************************************************************
#STEP 4: Create annual and seasonal means for each year
rm(list=ls())

isl_list<-c("ka","oa","ma","ha")
isl2_list<-c("kauai","oahu","maui","hawaii")
varb_list<-c("RF","Temp")
varb2_list<-c("RAIN","T2")
varb3_list<-c("Rainfall","Temperature")
tm_list<-c("present","future","future")
tm2_list<-c("pres","rcp45","rcp85")
tm3_list<-c("present","rcp45","rcp85")

#open Rainfall Atlas statewide grid to set extent/size:
setwd("H:\\GIS_Layers\\RainfallAtlas\\RF_inches\\State")
rfa_mask<-raster("staterf_inann")
P4S.latlon <- CRS(projection(rfa_mask))


for (z in 1:3){  #loop through times (present, rcp45, rcp85)
  tm<-tm_list[z]
  tm2<-tm2_list[z]
  tm3<-tm3_list[z]

  #Year List and Month Lists:
  if(tm2=="pres"){  
    styr=1990
    endyr=2009
  } else {
    styr=2080
    endyr=2099
  }
  yr_list=seq(from = styr, to = endyr)
  mo_list=c("_01","_02","_03","_04","_05","_06","_07","_08","_09","_10","_11","_12")
  mo_list_dry=c("_05","_06","_07","_08","_09","_10")  #just dry season months
  mo_list_wet=c("_11","_12","_01","_02","_03","_04")  #6 months nov-april
  


  #Temperature - just calculate annual
  y=2 #1 = rain, 2 = temp
  varb<-varb_list[y]
  varb2<-varb2_list[y]
  varb3<-varb3_list[y]

  
  for (p in 1:20){  #number of years
    yr<-yr_list[p]
    #create empty stack
    s<-stack()
    
    for (q in 1:12){ #loop through 12 months
      mo<-mo_list[q]
      
      MonthlyState<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_MonthlyTIF_State_250m/",tm3,"/",sep="")
      setwd(MonthlyState) #set directory DynDS_MonthlyTIF_State_250m
      
      f.in=paste("State_",varb,"_",tm2,"_",yr,mo,"_250m.tif",sep="")
      r1<-raster(f.in) #open island-month raster 
      s<-stack(s,r1) #add all months in this year to raster stack
    }
    
    s_mean<-mean(s,na.rm=TRUE) #Calculate MEAN of all TEMPERATURE rasters (SUM RAINFALL RASTERS...)

    #save new statewide raster for this YEAR:
    f.out=paste("State_Ann_",varb,"_",tm2,"_",yr,"_250m.tif",sep="") #only need YEAR in the name
    AnnSeasStateOut<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_AnnSeasTIF_State_250m/",tm3,"/",sep="")
    setwd(AnnSeasStateOut) #output directory DynDS_AnnSeasTIF_State_250m
    writeRaster(s_mean, f.out, format="GTiff",overwrite=TRUE)
        
  } #on to the next year
  
  
  
  #Rainfall - calculate annual, wet season (nov-apr), and dry season (may-oct) SUMS
  #NEED TO MASK again
  #ANNUAL:***********
  y=1 #1 = rain, 2 = temp
  varb<-varb_list[y]
  varb2<-varb2_list[y]
  varb3<-varb3_list[y]

  
  for (p in 1:20){  #number of years
    yr<-yr_list[p]
    #create empty stack
    s<-stack()
    
    for (q in 1:12){ #loop through 12 months
      mo<-mo_list[q]
      
      MonthlyState<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_MonthlyTIF_State_250m/",tm3,"/",sep="")
      setwd(MonthlyState) #set directory DynDS_MonthlyTIF_State_250m
      
      f.in=paste("State_",varb,"_",tm2,"_",yr,mo,"_250m.tif",sep="")
      r1<-raster(f.in) #open island-month raster 
      s<-stack(s,r1) #add all months in this year to raster stack
    }
    
    s_sum_ann<-sum(s,na.rm=TRUE) #Calculate SUM of all RAINFALL RASTERS...
    s_sum_ann2<-mask(s_sum_ann,rfa_mask)  #mask to coastline of rainfall atlas layer
    
    #save new statewide raster for this YEAR:
    f.out=paste("State_Ann_",varb,"_",tm2,"_",yr,"_250m.tif",sep="") #only need YEAR in the name
    AnnSeasStateOut<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_AnnSeasTIF_State_250m/",tm3,"/",sep="")
    setwd(AnnSeasStateOut) #output directory DynDS_AnnSeasTIF_State_250m
    writeRaster(s_sum_ann2, f.out, format="GTiff",overwrite=TRUE)
    
  } #on to the next year
  
  
  
  
  #Rainfall - calculate annual, wet season (nov-apr), and dry season (may-oct) SUMS
  #DRY SEASON May-October:***********
  
  for (p in 1:20){  #number of years
    yr<-yr_list[p]
    #create empty stack
    s<-stack()
    
    for (q in 1:6){ #loop through 6 months
      mo<-mo_list_dry[q]
      
      MonthlyState<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_MonthlyTIF_State_250m/",tm3,"/",sep="")
      setwd(MonthlyState) #set directory DynDS_MonthlyTIF_State_250m
      
      f.in=paste("State_",varb,"_",tm2,"_",yr,mo,"_250m.tif",sep="")
      r1<-raster(f.in) #open island-month raster 
      s<-stack(s,r1) #add all months in this year to raster stack
    }
    
    s_sum_dry<-sum(s,na.rm=TRUE) #Calculate SUM of all RAINFALL RASTERS...
    s_sum_dry2<-mask(s_sum_dry,rfa_mask)  #mask to coastline of rainfall atlas layer
    
    #save new statewide raster for this YEAR:
    f.out=paste("State_Dry_",varb,"_",tm2,"_",yr,"_250m.tif",sep="") #only need YEAR in the name
    AnnSeasStateOut<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_AnnSeasTIF_State_250m/",tm3,"/",sep="")
    setwd(AnnSeasStateOut) #output directory DynDS_AnnSeasTIF_State_250m
    writeRaster(s_sum_dry2, f.out, format="GTiff",overwrite=TRUE)
    
  } #on to the next year
  
  
  
  

  #Rainfall - calculate annual, wet season (nov-apr), and dry season (may-oct) SUMS
  #WET SEASON Nov (prev yr) to Apr (next year):***********
  
  for (p in 1:19){  #number of years (one fewer year than the rest since is spans 2 calendar years)
    yr<-yr_list[p]
    yr1<-yr_list[p+1]
    #create empty stack
    s<-stack()
    
    for (q in 1:6){ #loop through 6 months
      mo<-mo_list[q]
      
      MonthlyState<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_MonthlyTIF_State_250m/",tm3,"/",sep="")
      setwd(MonthlyState) #set directory DynDS_MonthlyTIF_State_250m
      
      if(q==1){
        f.in=paste("State_",varb,"_",tm2,"_",yr,mo,"_250m.tif",sep="") #for nov & dec, grab previous year
      } else if(q==2) {
        f.in=paste("State_",varb,"_",tm2,"_",yr,mo,"_250m.tif",sep="") #for nov & dec, grab previous year
      } else {
        f.in=paste("State_",varb,"_",tm2,"_",yr1,mo,"_250m.tif",sep="") #for jan - apr grab next year (p+1)
      }
      r1<-raster(f.in) #open island-month raster 
      s<-stack(s,r1) #add all months in this year to raster stack
    }
    
    s_sum_wet<-sum(s,na.rm=TRUE) #Calculate SUM of all RAINFALL RASTERS...
    s_sum_wet2<-mask(s_sum_wet,rfa_mask)  #mask to coastline of rainfall atlas layer
    
    #save new statewide raster for this YEAR:
    f.out=paste("State_Wet_",varb,"_",tm2,"_",yr,"_250m.tif",sep="") #only need YEAR in the name
    AnnSeasStateOut<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_AnnSeasTIF_State_250m/",tm3,"/",sep="")
    setwd(AnnSeasStateOut) #output directory DynDS_AnnSeasTIF_State_250m
    writeRaster(s_sum_wet2, f.out, format="GTiff",overwrite=TRUE)
    
  } #on to the next year

} #on to the next time


