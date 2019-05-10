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
        
        AnnSeasDir<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_AnnSeasTIF_State_250m/",tm3,"/",sep="")
        setwd(AnnSeasDir) #set directory DynDS_AnnSeasTIF_State_250m
          
        f.in=paste("State_",seas,"_",varb,"_",tm2,"_",yr,"_250m.tif",sep="")
        r1<-raster(f.in) #open island annual raster 
        s<-stack(s,r1) #add all years to raster stack
        
      } #on to the next year
    
      s_avg_ann<-mean(s,na.rm=TRUE) #Calculate MEAN of all RASTERS...
      s_avg_ann2<-mask(s_avg_ann,rfa_mask)  #mask to coastline of rainfall atlas layer
      s_avg_ann2_in<-s_avg_ann2/25.4
        
      #save new statewide mean raster:
      f.out.mm=paste("DynDS_HI_",varb,"_",tm3,"_mean_mm_",seas,"_",tm4,".tif",sep="") #these outputs are in mm
      f.out.in=paste("DynDS_HI_",varb,"_",tm3,"_mean_in_",seas,"_",tm4,".tif",sep="") #these outputs are in inches
      
      AnnSeasStateMean<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_SeasMeans_State_",varb,"_250m/",sep="")
      setwd(AnnSeasStateMean) #output directory DynDS_SeasMeans_State_RF (or Temp)
      writeRaster(s_avg_ann2, f.out.mm, format="GTiff",overwrite=TRUE)
      writeRaster(s_avg_ann2_in, f.out.in, format="GTiff",overwrite=TRUE)
      
    } #on to next time
    
    if(varb=="Temp"){break}
  } #on to next season - unless it's temperature, then we exit this loop

} #on to next variable




#*****************************************************************************************************************************
#*****************************************************************************************************************************
#*****************************************************************************************************************************
#STEP 6. Calculate Future Change (relative to present day)

#Absolute Change (inches and mm) and Percent change (divide):

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
      
      #OPEN SEAS MEANS:
      AnnSeasStateMean<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_SeasMeans_State_",varb,"_250m/",sep="")
      setwd(AnnSeasStateMean) #directory DynDS_SeasMeans_State_RF (or Temp)
      #set file names:
      f.mean.presmm=paste("DynDS_HI_",varb,"_present_mean_mm_",seas,"_2009.tif",sep="") #present day seas mean in mm
      f.mean.presin=paste("DynDS_HI_",varb,"_present_mean_in_",seas,"_2009.tif",sep="") #present day seas mean in inches
      f.mean.futmm=paste("DynDS_HI_",varb,"_",tm3,"_mean_mm_",seas,"_",tm4,".tif",sep="") #future seas mean in mm
      f.mean.futin=paste("DynDS_HI_",varb,"_",tm3,"_mean_in_",seas,"_",tm4,".tif",sep="") #future seas mean in inches
      #Open rasters:
      r.mean.presmm<-raster(f.mean.presmm)
      r.mean.presin<-raster(f.mean.presin)
      r.mean.futmm<-raster(f.mean.futmm)
      r.mean.futin<-raster(f.mean.futin)
      
      #CALCULATE THE MAIN OUTPUTS
      #SUBTRACT (Future minus Present) TO GET ABS MEAN, inches & mm
      abschange.mm <- r.mean.futmm - r.mean.presmm
      abschange.in <- r.mean.futin - r.mean.presin
      
      #DIVIDE (Future Change / Present Mean) TO GET % CHANGE (RAINFALL ONLY)
      if(varb=="RF"){pctchange <- (abschange.mm / r.mean.presmm)*100}  #only need to do it once, mm and inches come out same when you divide.
      
      ##################
      #SAVE outputs:
      setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_FutureSeasChange_",varb,"_in_250m/",sep="")) #directory abs change inches
      f.out.chg.in=paste("DynDS_HI_",varb,"_in_chng_",tm2,"_",seas,"_2100.tif",sep="") #these outputs are in inches
      writeRaster(abschange.in, f.out.chg.in, format="GTiff",overwrite=TRUE)
      
      setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_FutureSeasChange_",varb,"_mm_250m/",sep="")) #directory abs change mm
      f.out.chg.mm=paste("DynDS_HI_",varb,"_mm_chng_",tm2,"_",seas,"_2100.tif",sep="") #these outputs are in mm
      writeRaster(abschange.mm, f.out.chg.mm, format="GTiff",overwrite=TRUE)
      
      if(varb=="RF"){
        setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_FutureSeasChange_",varb,"_Pct_250m/",sep="")) #directory % change
        f.out.chg.pct=paste("DynDS_HI_",varb,"_pct_chng_",tm2,"_",seas,"_2100.tif",sep="") #these outputs are in percent
        writeRaster(pctchange, f.out.chg.pct, format="GTiff",overwrite=TRUE)
      }
      
    } #next time
    
    if(varb=="Temp"){break}
  } #on to next season - unless it's temperature, then we exit this loop
  
  
} #next variable







