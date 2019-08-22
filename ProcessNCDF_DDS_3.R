#Process Dynamical Downscaling NetCDF files into seasonal rasters
#for STEPS 1 & 2 --> see "ProcessNCDF_DDS_1.R" code
#for STEPS 3 & 4 --> see "ProcessNCDF_DDS_2.R" code

#STEP 5: Create Present & Future Means
#STEP 6: Calculate Future change (% and absolute (inches/mm) change relative to present day)

rm(list=ls())

library('raster')
library('ncdf4')
library('rgdal')
library('sp')
library('maptools') #for old shapefile read command
library(rasterVis) #for plots

#--> check that directories are updated


#Step 5: MEANS: present day and future
#average the annual & seasonal files from each year

seas_list<-c("Ann","Wet","Dry")
seas2_list<-c("Annual","WetSeason","DrySeason")
numyrs_seas<-c(20,19,20)  #wet season has only 19 years
varb_list<-c("RF","Temp")
varb2_list<-c("RAIN","T2")
varb3_list<-c("Rainfall","Temperature")
tm_list<-c("present","future","future")
tm2_list<-c("pres","rcp45","rcp85")
tm3_list<-c("present","rcp45","rcp85")
tm4_list<-c("2009","2100","2100")
unit_list<-c("mm","in","degK","degC","degF")  #original units: mm and degK

#open Rainfall Atlas statewide grid to set extent/size:
setwd("H:\\GIS_Layers\\RainfallAtlas\\RF_inches\\State")
rfa_mask<-raster("staterf_inann")
P4S.latlon <- CRS(projection(rfa_mask))


for (y in 1:2){  #loop through variables (rain & temp)
  varb<-varb_list[y]
  varb2<-varb2_list[y]
  varb3<-varb3_list[y]

  for (ss in 1:3){
    #loop through seasons (Ann, Wet, Dry - rainfall only...temp only has ANN)
    seas<-seas_list[ss]
    seas2<-seas2_list[ss]
    numyrs<-numyrs_seas[ss] 
    
  
    for (z in 1:3){  #loop through times (present, rcp45, rcp85)
      tm<-tm_list[z]
      tm2<-tm2_list[z]
      tm3<-tm3_list[z]
      tm4<-tm4_list[z]
      
      
      #Year Lists:
      if(tm2=="pres"){  
        styr=1990
        endyr=2009
      } else {
        styr=2080
        endyr=2099
      }
      yr_list=seq(from = styr, to = endyr)
    
      #create empty stack
        s<-stack()
        
      for (p in 1:numyrs){  #number of years to average - wet season only has 19, Ann & Dry have 20
        yr<-yr_list[p]
        
        AnnSeasDir<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_5AnnSeasTIF_State_250m/",tm3,"/",sep="")
        setwd(AnnSeasDir) #set directory DynDS_AnnSeasTIF_State_250m
          
        f.in=paste("State_",seas,"_",varb,"_",tm2,"_",yr,"_250m.tif",sep="")
        r1<-raster(f.in) #open island annual raster 
        s<-stack(s,r1) #add all years to raster stack
        
      } #on to the next year
    
      s_avg_ann<-mean(s,na.rm=TRUE) #Calculate MEAN of all RASTERS...units are same as orig (mm for RF, degK for Temp)
      s_avg_ann2<-mask(s_avg_ann,rfa_mask)  #mask to coastline of rainfall atlas layer
      
      #unit conversions & save outputs:
      
      if(varb=="RF"){
        s_avg_ann3<-s_avg_ann2/25.4  #unit conversion is mm to inches
        #set file names for outputs based on units, write rasters:
        f.out.2=paste("DynDS_HI_",varb,"_",tm3,"_mean_mm_",seas,"_",tm4,".tif",sep="") #these outputs are in mm
        f.out.3=paste("DynDS_HI_",varb,"_",tm3,"_mean_in_",seas,"_",tm4,".tif",sep="") #these outputs are in inches
        #Year Lists:
        if(tm2=="pres"){  
          setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_4PresentDay_SeasMeans_",varb,"_250m/",sep="")) #directory DynDS Present Day Seas Means
          writeRaster(s_avg_ann2, f.out.2, format="GTiff",overwrite=TRUE)
          writeRaster(s_avg_ann3, f.out.3, format="GTiff",overwrite=TRUE)
        } else { #future
          setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_3FutureMeans_AbsChange_raw/DynDS_FutureSeasMeans_",varb,"_mm_250m/",sep="")) #directory future seas mean in mm/degK
          writeRaster(s_avg_ann2, f.out.2, format="GTiff",overwrite=TRUE)
          setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_3FutureMeans_AbsChange_raw/DynDS_FutureSeasMeans_",varb,"_in_250m/",sep="")) #directory future seas mean in mm/degK
          writeRaster(s_avg_ann3, f.out.3, format="GTiff",overwrite=TRUE)
        }

      } 
      if(varb=="Temp"){ #else if temperature:
        s_avg_ann3<-s_avg_ann2 - 273.15  # unit conversion is Kelvin to deg Celsius
        s_avg_ann4<-(s_avg_ann2*1.8)-459.67  # unit conversion is degK to Farenheit
        #set file names for outputs based on units, write rasters:
        f.out.2=paste("DynDS_HI_",varb,"_",tm3,"_mean_degK_",seas,"_",tm4,".tif",sep="") #these outputs are in degK
        f.out.3=paste("DynDS_HI_",varb,"_",tm3,"_mean_degC_",seas,"_",tm4,".tif",sep="") #these outputs are in degC
        f.out.4=paste("DynDS_HI_",varb,"_",tm3,"_mean_degF_",seas,"_",tm4,".tif",sep="") #these outputs are in degF
        if(tm2=="pres"){  
          setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_4PresentDay_SeasMeans_",varb,"_250m/",sep="")) #directory DynDS Present Day Seas Means
          writeRaster(s_avg_ann2, f.out.2, format="GTiff",overwrite=TRUE)
          writeRaster(s_avg_ann3, f.out.3, format="GTiff",overwrite=TRUE)
          writeRaster(s_avg_ann4, f.out.4, format="GTiff",overwrite=TRUE)
        } else { #future
          setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_3FutureMeans_raw/DynDS_FutureSeasMeans_",varb,"_degK_250m/",sep="")) #directory future seas mean in mm/degK
          writeRaster(s_avg_ann2, f.out.2, format="GTiff",overwrite=TRUE)
          setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_3FutureMeans_raw/DynDS_FutureSeasMeans_",varb,"_degC_250m/",sep="")) #directory future seas mean in mm/degK
          writeRaster(s_avg_ann3, f.out.3, format="GTiff",overwrite=TRUE)
          setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_3FutureMeans_raw/DynDS_FutureSeasMeans_",varb,"_degF_250m/",sep="")) #directory future seas mean in mm/degK
          writeRaster(s_avg_ann4, f.out.4, format="GTiff",overwrite=TRUE)
        }
        
      }
      
    } #on to next time
    
    if(varb=="Temp"){break}
  } #on to next season - unless it's temperature, then we exit this loop

} #on to next variable




#*****************************************************************************************************************************
#*****************************************************************************************************************************
#*****************************************************************************************************************************
#STEP 6. Calculate Future Change (relative to present day)

#Absolute Change (inches and mm / degK, degC, degF) and Percent change (divide):

for (y in 1:2){  #loop through variables (rain & temp)
  varb<-varb_list[y]
  varb2<-varb2_list[y]
  varb3<-varb3_list[y]
  
  for (ss in 1:3){
    #loop through seasons (Ann, Wet, Dry - rainfall only...temp only has ANN)
    seas<-seas_list[ss]
    seas2<-seas2_list[ss]

    for (z in 2:3){  #loop through times (NO PRES, rcp45, rcp85)
      tm<-tm_list[z]
      tm2<-tm2_list[z]
      tm3<-tm3_list[z]
      tm4<-tm4_list[z]
      
      #OPEN SEAS MEANS: --check that directories are updated
      
      #set file names:
      if(varb=="RF"){
        unit1<-unit_list[1] #mm
        unit2<-unit_list[2] #in
      }
      if(varb=="Temp"){
        unit1<-unit_list[4] #degC -- don't need BOTH degC and degK for abs change - same units
        unit2<-unit_list[5] #degF
      }
      #set file names & open rasters:
      #Present day means:
      setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_4PresentDay_SeasMeans_",varb,"_250m/",sep="")) #directory DynDS Present Day Seas Means
      f.mean.pres1=paste("DynDS_HI_",varb,"_present_mean_",unit1,"_",seas,"_2009.tif",sep="") #present day seas mean in mm/degK
      r.mean.pres1<-raster(f.mean.pres1)
      f.mean.pres2=paste("DynDS_HI_",varb,"_present_mean_",unit2,"_",seas,"_2009.tif",sep="") #present day seas mean in inches/degC
      r.mean.pres2<-raster(f.mean.pres2)
      
      #Future Means (separated into folders by units)
      if(varb=="RF"){setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_3FutureMeans_AbsChange_raw/DynDS_FutureSeasMeans_",varb,"_",unit1,"_250m/",sep=""))} #directory future seas mean in mm/degK
      if(varb=="Temp"){setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_3FutureMeans_raw/DynDS_FutureSeasMeans_",varb,"_",unit1,"_250m/",sep=""))} #directory future seas mean in mm/degK
      f.mean.fut1=paste("DynDS_HI_",varb,"_",tm3,"_mean_",unit1,"_",seas,"_",tm4,".tif",sep="") #future seas mean in mm/degK
      r.mean.fut1<-raster(f.mean.fut1)
      
      if(varb=="RF"){setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_3FutureMeans_AbsChange_raw/DynDS_FutureSeasMeans_",varb,"_",unit2,"_250m/",sep=""))} #directory future seas mean in inches/degC
      if(varb=="Temp"){setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_3FutureMeans_raw/DynDS_FutureSeasMeans_",varb,"_",unit2,"_250m/",sep=""))} #directory future seas mean in inches/degC
      f.mean.fut2=paste("DynDS_HI_",varb,"_",tm3,"_mean_",unit2,"_",seas,"_",tm4,".tif",sep="") #future seas mean in inches/degC
      r.mean.fut2<-raster(f.mean.fut2)
      
      
      #CALCULATE THE MAIN OUTPUTS: % and Abs change
      
      #SUBTRACT (Future minus Present) TO GET ABS MEAN, inches & mm (or degC, degF)
      abschange.1 <- r.mean.fut1 - r.mean.pres1
      abschange.2 <- r.mean.fut2 - r.mean.pres2

      #DIVIDE (Future Change / Present Mean) TO GET % CHANGE (RAINFALL ONLY)
      if(varb=="RF"){pctchange <- (abschange.1 / r.mean.pres1)*100}  #only need to do it once, mm and inches come out same when you divide.
      
      ##################
      #SAVE outputs: - check that directories are updated
      setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_3FutureMeans_AbsChange_raw/DynDS_FutureSeasChange_",varb,"_",unit1,"_250m/",sep="")) #directory abs change mm/degC
      f.out.chg.1=paste("DynDS_HI_",varb,"_",unit1,"_chng_",tm2,"_",seas,"_2100.tif",sep="") #these outputs are in mm/degC
      writeRaster(abschange.1, f.out.chg.1, format="GTiff",overwrite=TRUE)
      
      setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_3FutureMeans_AbsChange_raw/DynDS_FutureSeasChange_",varb,"_",unit2,"_250m/",sep="")) #directory abs change in/degF
      f.out.chg.2=paste("DynDS_HI_",varb,"_",unit2,"_chng_",tm2,"_",seas,"_2100.tif",sep="") #these outputs are in in/degF
      writeRaster(abschange.2, f.out.chg.2, format="GTiff",overwrite=TRUE)
      
      if(varb=="RF"){ #percent change
        setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_1FutureSeasChange_",varb,"_Pct_250m/",sep="")) #directory % change
        f.out.chg.pct=paste("DynDS_HI_",varb,"_pct_chng_",tm2,"_",seas,"_2100.tif",sep="") #these outputs are in percent
        writeRaster(pctchange, f.out.chg.pct, format="GTiff",overwrite=TRUE)
      }
      
    } #next time
    
    if(varb=="Temp"){break}
  } #on to next season - unless it's temperature, then we exit this loop
  
  
} #next variable



