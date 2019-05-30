#Process Dynamical Downscaling NetCDF files into seasonal rasters
#for STEPS 1 & 2 --> see "ProcessNCDF_DDS_1.R" code
#for STEPS 3 & 4 --> see "ProcessNCDF_DDS_2.R" code

#STEP 5: Create Present & Future Means
#STEP 6: Calculate Future change (% and absolute (inches/mm) change relative to present day)
#STEP 7: calculate new future means and future absolute changes from observation-based present-day maps (Rainfall Atlas)

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
        
        AnnSeasDir<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_AnnSeasTIF_State_250m/",tm3,"/",sep="")
        setwd(AnnSeasDir) #set directory DynDS_AnnSeasTIF_State_250m
          
        f.in=paste("State_",seas,"_",varb,"_",tm2,"_",yr,"_250m.tif",sep="")
        r1<-raster(f.in) #open island annual raster 
        s<-stack(s,r1) #add all years to raster stack
        
      } #on to the next year
    
      s_avg_ann<-mean(s,na.rm=TRUE) #Calculate MEAN of all RASTERS...units are same as orig (mm for RF, degK for Temp)
      s_avg_ann2<-mask(s_avg_ann,rfa_mask)  #mask to coastline of rainfall atlas layer
      
      #unit conversions & save outputs:
      AnnSeasStateMean<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_SeasMeans_State_",varb,"_250m/",sep="")
      setwd(AnnSeasStateMean) #output directory DynDS_SeasMeans_State_RF (or Temp)
      if(varb=="RF"){
        s_avg_ann3<-s_avg_ann2/25.4  #unit conversion is mm to inches
        #set file names for outputs based on units, write rasters:
        f.out.2=paste("DynDS_HI_",varb,"_",tm3,"_mean_mm_",seas,"_",tm4,".tif",sep="") #these outputs are in mm
        f.out.3=paste("DynDS_HI_",varb,"_",tm3,"_mean_in_",seas,"_",tm4,".tif",sep="") #these outputs are in inches
        writeRaster(s_avg_ann2, f.out.2, format="GTiff",overwrite=TRUE)
        writeRaster(s_avg_ann3, f.out.3, format="GTiff",overwrite=TRUE)
      } 
      if(varb=="Temp"){ #else if temperature:
        s_avg_ann3<-s_avg_ann2 - 273.15  # unit conversion is Kelvin to deg Celsius
        s_avg_ann4<-(s_avg_ann2*1.8)-459.67  # unit conversion is degK to Farenheit
        #set file names for outputs based on units, write rasters:
        f.out.2=paste("DynDS_HI_",varb,"_",tm3,"_mean_degK_",seas,"_",tm4,".tif",sep="") #these outputs are in degK
        f.out.3=paste("DynDS_HI_",varb,"_",tm3,"_mean_degC_",seas,"_",tm4,".tif",sep="") #these outputs are in degC
        f.out.4=paste("DynDS_HI_",varb,"_",tm3,"_mean_degF_",seas,"_",tm4,".tif",sep="") #these outputs are in degF
        writeRaster(s_avg_ann2, f.out.2, format="GTiff",overwrite=TRUE)
        writeRaster(s_avg_ann3, f.out.3, format="GTiff",overwrite=TRUE)
        writeRaster(s_avg_ann4, f.out.4, format="GTiff",overwrite=TRUE)
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
      
      #OPEN SEAS MEANS:
      AnnSeasStateMean<-paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_SeasMeans_State_",varb,"_250m/",sep="")
      setwd(AnnSeasStateMean) #directory DynDS_SeasMeans_State_RF (or Temp)
      #set file names:
      if(varb=="RF"){
        unit1<-unit_list[1] #mm
        unit2<-unit_list[2] #in
      }
      if(varb=="Temp"){
        unit1<-unit_list[4] #degC -- don't need BOTH degC and degK for abs change - same units
        unit2<-unit_list[5] #degF
      }
      f.mean.pres1=paste("DynDS_HI_",varb,"_present_mean_",unit1,"_",seas,"_2009.tif",sep="") #present day seas mean in mm/degK
      f.mean.pres2=paste("DynDS_HI_",varb,"_present_mean_",unit2,"_",seas,"_2009.tif",sep="") #present day seas mean in inches/degC
      f.mean.fut1=paste("DynDS_HI_",varb,"_",tm3,"_mean_",unit1,"_",seas,"_",tm4,".tif",sep="") #future seas mean in mm/degK
      f.mean.fut2=paste("DynDS_HI_",varb,"_",tm3,"_mean_",unit2,"_",seas,"_",tm4,".tif",sep="") #future seas mean in inches/degC
      #Open rasters:
      r.mean.pres1<-raster(f.mean.pres1)
      r.mean.pres2<-raster(f.mean.pres2)
      r.mean.fut1<-raster(f.mean.fut1)
      r.mean.fut2<-raster(f.mean.fut2)
        
      
      #CALCULATE THE MAIN OUTPUTS
      #SUBTRACT (Future minus Present) TO GET ABS MEAN, inches & mm (or degC, degF)
      abschange.1 <- r.mean.fut1 - r.mean.pres1
      abschange.2 <- r.mean.fut2 - r.mean.pres2

      #DIVIDE (Future Change / Present Mean) TO GET % CHANGE (RAINFALL ONLY)
      if(varb=="RF"){pctchange <- (abschange.1 / r.mean.pres1)*100}  #only need to do it once, mm and inches come out same when you divide.
      
      ##################
      #SAVE outputs:
      setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_FutureSeasChange_",varb,"_",unit1,"_250m/",sep="")) #directory abs change mm/degC
      f.out.chg.1=paste("DynDS_HI_",varb,"_",unit1,"_chng_",tm2,"_",seas,"_2100.tif",sep="") #these outputs are in mm/degC
      writeRaster(abschange.1, f.out.chg.1, format="GTiff",overwrite=TRUE)
      
      setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_FutureSeasChange_",varb,"_",unit2,"_250m/",sep="")) #directory abs change in/degF
      f.out.chg.2=paste("DynDS_HI_",varb,"_",unit2,"_chng_",tm2,"_",seas,"_2100.tif",sep="") #these outputs are in in/degF
      writeRaster(abschange.2, f.out.chg.2, format="GTiff",overwrite=TRUE)
      
      if(varb=="RF"){ #percent change
        setwd(paste("H:/Downscaling/Dynamical/CMIP5/DynDS_",varb3,"TIF/DynDS_FutureSeasChange_",varb,"_Pct_250m/",sep="")) #directory % change
        f.out.chg.pct=paste("DynDS_HI_",varb,"_pct_chng_",tm2,"_",seas,"_2100.tif",sep="") #these outputs are in percent
        writeRaster(pctchange, f.out.chg.pct, format="GTiff",overwrite=TRUE)
      }
      
    } #next time
    
    if(varb=="Temp"){break}
  } #on to next season - unless it's temperature, then we exit this loop
  
  
} #next variable



##############################################################################################
#added 5/29/2019 - changed folder structure to reflect the two different sets of absolute maps (based on SDS and DDS present day means)

#STEP 7: calculate future means and future absolute change from observation-based present-day maps
#   For rainfall, calculate future mean using the Rainfall Atlas of Hawaii mean annual rainfall map (Giambelluca et al. 2013) as base 
#   For Temperature, use the Climate of Hawaii mean annual temp. map (Giambelluca et al. 2014) as base 
#   This will allow you to compare absolute values from both products.

#Open present day means from Stat DS (which are based on observations)
#Open % change from Dyn DS 
#Multiply % change to present-day mean (from observations) in inches to get future Dyn DS change in inches, then add to SDS present day to get future mean (do for all units)
#For temperature no %: instead use abs change plus SDS present day mean to get new future mean

#Not written efficiently (no loops), one line per each map

library('raster')

#OPEN PRESENT-DAY MEANS (BASED ON OBSERVATIONS) for each season, all units
setwd("H:\\Downscaling_TIF_processing\\StatDS_RainfallTIF\\StatDS_SeasMeans_State_RF_250m")
pres.obsmn.ann.in<-raster("StatDS_HI_RF_present_mean_in_ann_2007.tif")
pres.obsmn.ann.mm<-raster("StatDS_HI_RF_present_mean_mm_ann_2007.tif")
pres.obsmn.wet.in<-raster("StatDS_HI_RF_present_mean_in_wet_2007.tif")
pres.obsmn.wet.mm<-raster("StatDS_HI_RF_present_mean_mm_wet_2007.tif")
pres.obsmn.dry.in<-raster("StatDS_HI_RF_present_mean_in_dry_2007.tif")
pres.obsmn.dry.mm<-raster("StatDS_HI_RF_present_mean_mm_dry_2007.tif")
setwd("H:\\Downscaling_TIF_processing\\StatDS_TemperatureTIF\\StatDS_SeasMeans_State_Temp_250m")
pres.obsmn.ann.degc<-raster("StatDS_HI_Temp_present_mean_degC_Ann_2009.tif")
pres.obsmn.ann.degf<-raster("StatDS_HI_Temp_present_mean_degF_Ann_2009.tif")


#Temp DegC & DegK:
#Ann:
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_TemperatureTIF\\DynDS_1FutureSeasChange_Temp_250m\\DynDS_FutureSeasChange_Temp_degC_250m")
rcp45.chg.ann.degc<-raster("DynDS_HI_Temp_degC_chng_rcp45_Ann_2100.tif")
rcp85.chg.ann.degc<-raster("DynDS_HI_Temp_degC_chng_rcp85_Ann_2100.tif")
#Add to present mean, create new Future Mean
newfut.rcp45<-(rcp45.chg.ann.degc + pres.obsmn.ann.degc)
newfut.rcp85<-(rcp85.chg.ann.degc + pres.obsmn.ann.degc)
#convert degC to degK
newfut.rcp45.degK<-newfut.rcp45+273.15
newfut.rcp85.degK<-newfut.rcp85+273.15
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_TemperatureTIF\\DynDS_2FutureMeans_SDS_comp\\DynDS_FutureSeasMeans_Temp_degC_250m_SDS")
writeRaster(newfut.rcp45, "DynDS_HI_Temp_rcp45_mean_degC_Ann_2100_obs.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.rcp85, "DynDS_HI_Temp_rcp85_mean_degC_Ann_2100_obs.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.rcp45.degK, "DynDS_HI_Temp_rcp45_mean_degK_Ann_2100_obs.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.rcp85.degK, "DynDS_HI_Temp_rcp85_mean_degK_Ann_2100_obs.tif", format="GTiff",overwrite=TRUE)


#Temp DegF:
#Ann:
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_TemperatureTIF\\DynDS_1FutureSeasChange_Temp_250m\\DynDS_FutureSeasChange_Temp_degF_250m")
rcp45.chg.ann.degf<-raster("DynDS_HI_Temp_degF_chng_rcp45_Ann_2100.tif")
rcp85.chg.ann.degf<-raster("DynDS_HI_Temp_degF_chng_rcp85_Ann_2100.tif")
#Add to present mean, create new Future Mean
newfut.rcp45<-(rcp45.chg.ann.degf + pres.obsmn.ann.degf)
newfut.rcp85<-(rcp85.chg.ann.degf + pres.obsmn.ann.degf)
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_TemperatureTIF\\DynDS_2FutureMeans_SDS_comp\\DynDS_FutureSeasMeans_Temp_degF_250m_SDS")
writeRaster(newfut.rcp45, "DynDS_HI_Temp_rcp45_mean_degF_Ann_2100_obs.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.rcp85, "DynDS_HI_Temp_rcp85_mean_degF_Ann_2100_obs.tif", format="GTiff",overwrite=TRUE)



#RF inches:
#Ann:
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_1FutureSeasChange_RF_Pct_250m")
rcp45.pctchg.ann<-raster("DynDS_HI_RF_pct_chng_rcp45_Ann_2100.tif")
rcp85.pctchg.ann<-raster("DynDS_HI_RF_pct_chng_rcp85_Ann_2100.tif")
#Use pct change to calculate future change relative to SDS mean, Add that to SDS present mean, create new DDS Future Mean (comparable to SDS futures)
newfut.chg.rcp45<-(rcp45.pctchg.ann / 100) * pres.obsmn.ann.in
newfut.chg.rcp85<-(rcp85.pctchg.ann / 100) * pres.obsmn.ann.in
newfut.mean.rcp45<-newfut.chg.rcp45 + pres.obsmn.ann.in
newfut.mean.rcp85<-newfut.chg.rcp85 + pres.obsmn.ann.in
#Save new outputs
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_SDS_comp\\DynDS_FutureSeasChange_RF_in_250m_SDS")
writeRaster(newfut.chg.rcp45, "DynDS_HI_RF_rcp45_chng_in_Ann_2100_SDS.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.chg.rcp85, "DynDS_HI_RF_rcp85_chng_in_Ann_2100_SDS.tif", format="GTiff",overwrite=TRUE)
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_SDS_comp\\DynDS_FutureSeasMeans_RF_in_250m_SDS")
writeRaster(newfut.mean.rcp45, "DynDS_HI_RF_rcp45_mean_in_Ann_2100_SDS.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.mean.rcp85, "DynDS_HI_RF_rcp85_mean_in_Ann_2100_SDS.tif", format="GTiff",overwrite=TRUE)


#Wet Season:
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_1FutureSeasChange_RF_Pct_250m")
rcp45.pctchg.wet<-raster("DynDS_HI_RF_pct_chng_rcp45_wet_2100.tif")
rcp85.pctchg.wet<-raster("DynDS_HI_RF_pct_chng_rcp85_wet_2100.tif")
#Use pct change to calculate future change relative to SDS mean, Add that to SDS present mean, create new DDS Future Mean (comparable to SDS futures)
newfut.chg.rcp45<-(rcp45.pctchg.wet / 100) * pres.obsmn.wet.in
newfut.chg.rcp85<-(rcp85.pctchg.wet / 100) * pres.obsmn.wet.in
newfut.mean.rcp45<-newfut.chg.rcp45 + pres.obsmn.wet.in
newfut.mean.rcp85<-newfut.chg.rcp85 + pres.obsmn.wet.in
#Save new outputs
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_SDS_comp\\DynDS_FutureSeasChange_RF_in_250m_SDS")
writeRaster(newfut.chg.rcp45, "DynDS_HI_RF_rcp45_chng_in_Wet_2100_SDS.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.chg.rcp85, "DynDS_HI_RF_rcp85_chng_in_Wet_2100_SDS.tif", format="GTiff",overwrite=TRUE)
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_SDS_comp\\DynDS_FutureSeasMeans_RF_in_250m_SDS")
writeRaster(newfut.mean.rcp45, "DynDS_HI_RF_rcp45_mean_in_Wet_2100_SDS.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.mean.rcp85, "DynDS_HI_RF_rcp85_mean_in_Wet_2100_SDS.tif", format="GTiff",overwrite=TRUE)


#Dry Season:
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_1FutureSeasChange_RF_Pct_250m")
rcp45.pctchg.dry<-raster("DynDS_HI_RF_pct_chng_rcp45_dry_2100.tif")
rcp85.pctchg.dry<-raster("DynDS_HI_RF_pct_chng_rcp85_dry_2100.tif")
#Use pct change to calculate future change relative to SDS mean, Add that to SDS present mean, create new DDS Future Mean (comparable to SDS futures)
newfut.chg.rcp45<-(rcp45.pctchg.dry / 100) * pres.obsmn.dry.in
newfut.chg.rcp85<-(rcp85.pctchg.dry / 100) * pres.obsmn.dry.in
newfut.mean.rcp45<-newfut.chg.rcp45 + pres.obsmn.dry.in
newfut.mean.rcp85<-newfut.chg.rcp85 + pres.obsmn.dry.in
#Save new outputs
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_SDS_comp\\DynDS_FutureSeasChange_RF_in_250m_SDS")
writeRaster(newfut.chg.rcp45, "DynDS_HI_RF_rcp45_chng_in_Dry_2100_SDS.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.chg.rcp85, "DynDS_HI_RF_rcp85_chng_in_Dry_2100_SDS.tif", format="GTiff",overwrite=TRUE)
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_SDS_comp\\DynDS_FutureSeasMeans_RF_in_250m_SDS")
writeRaster(newfut.mean.rcp45, "DynDS_HI_RF_rcp45_mean_in_Dry_2100_SDS.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.mean.rcp85, "DynDS_HI_RF_rcp85_mean_in_Dry_2100_SDS.tif", format="GTiff",overwrite=TRUE)



#RF mm:
#Ann:
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_1FutureSeasChange_RF_Pct_250m")
rcp45.pctchg.ann<-raster("DynDS_HI_RF_pct_chng_rcp45_Ann_2100.tif")
rcp85.pctchg.ann<-raster("DynDS_HI_RF_pct_chng_rcp85_Ann_2100.tif")
#Use pct change to calculate future change relative to SDS mean, Add that to SDS present mean, create new DDS Future Mean (comparable to SDS futures)
newfut.chg.rcp45<-(rcp45.pctchg.ann / 100) * pres.obsmn.ann.mm
newfut.chg.rcp85<-(rcp85.pctchg.ann / 100) * pres.obsmn.ann.mm
newfut.mean.rcp45<-newfut.chg.rcp45 + pres.obsmn.ann.mm
newfut.mean.rcp85<-newfut.chg.rcp85 + pres.obsmn.ann.mm
#Save new outputs
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_SDS_comp\\DynDS_FutureSeasChange_RF_mm_250m_SDS")
writeRaster(newfut.chg.rcp45, "DynDS_HI_RF_rcp45_chng_mm_Ann_2100_SDS.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.chg.rcp85, "DynDS_HI_RF_rcp85_chng_mm_Ann_2100_SDS.tif", format="GTiff",overwrite=TRUE)
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_SDS_comp\\DynDS_FutureSeasMeans_RF_mm_250m_SDS")
writeRaster(newfut.mean.rcp45, "DynDS_HI_RF_rcp45_mean_mm_Ann_2100_SDS.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.mean.rcp85, "DynDS_HI_RF_rcp85_mean_mm_Ann_2100_SDS.tif", format="GTiff",overwrite=TRUE)


#Wet Season:
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_1FutureSeasChange_RF_Pct_250m")
rcp45.pctchg.wet<-raster("DynDS_HI_RF_pct_chng_rcp45_wet_2100.tif")
rcp85.pctchg.wet<-raster("DynDS_HI_RF_pct_chng_rcp85_wet_2100.tif")
#Use pct change to calculate future change relative to SDS mean, Add that to SDS present mean, create new DDS Future Mean (comparable to SDS futures)
newfut.chg.rcp45<-(rcp45.pctchg.wet / 100) * pres.obsmn.wet.mm
newfut.chg.rcp85<-(rcp85.pctchg.wet / 100) * pres.obsmn.wet.mm
newfut.mean.rcp45<-newfut.chg.rcp45 + pres.obsmn.wet.mm
newfut.mean.rcp85<-newfut.chg.rcp85 + pres.obsmn.wet.mm
#Save new outputs
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_SDS_comp\\DynDS_FutureSeasChange_RF_mm_250m_SDS")
writeRaster(newfut.chg.rcp45, "DynDS_HI_RF_rcp45_chng_mm_Wet_2100_SDS.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.chg.rcp85, "DynDS_HI_RF_rcp85_chng_mm_Wet_2100_SDS.tif", format="GTiff",overwrite=TRUE)
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_SDS_comp\\DynDS_FutureSeasMeans_RF_mm_250m_SDS")
writeRaster(newfut.mean.rcp45, "DynDS_HI_RF_rcp45_mean_mm_Wet_2100_SDS.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.mean.rcp85, "DynDS_HI_RF_rcp85_mean_mm_Wet_2100_SDS.tif", format="GTiff",overwrite=TRUE)


#Dry Season:
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_1FutureSeasChange_RF_Pct_250m")
rcp45.pctchg.dry<-raster("DynDS_HI_RF_pct_chng_rcp45_dry_2100.tif")
rcp85.pctchg.dry<-raster("DynDS_HI_RF_pct_chng_rcp85_dry_2100.tif")
#Use pct change to calculate future change relative to SDS mean, Add that to SDS present mean, create new DDS Future Mean (comparable to SDS futures)
newfut.chg.rcp45<-(rcp45.pctchg.dry / 100) * pres.obsmn.dry.mm
newfut.chg.rcp85<-(rcp85.pctchg.dry / 100) * pres.obsmn.dry.mm
newfut.mean.rcp45<-newfut.chg.rcp45 + pres.obsmn.dry.mm
newfut.mean.rcp85<-newfut.chg.rcp85 + pres.obsmn.dry.mm
#Save new outputs
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_SDS_comp\\DynDS_FutureSeasChange_RF_mm_250m_SDS")
writeRaster(newfut.chg.rcp45, "DynDS_HI_RF_rcp45_chng_mm_Dry_2100_SDS.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.chg.rcp85, "DynDS_HI_RF_rcp85_chng_mm_Dry_2100_SDS.tif", format="GTiff",overwrite=TRUE)
setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_SDS_comp\\DynDS_FutureSeasMeans_RF_mm_250m_SDS")
writeRaster(newfut.mean.rcp45, "DynDS_HI_RF_rcp45_mean_mm_Dry_2100_SDS.tif", format="GTiff",overwrite=TRUE)
writeRaster(newfut.mean.rcp85, "DynDS_HI_RF_rcp85_mean_mm_Dry_2100_SDS.tif", format="GTiff",overwrite=TRUE)
