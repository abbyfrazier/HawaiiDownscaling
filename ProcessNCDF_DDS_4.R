#8/21/2019 -- do temperature too?

#Process Dynamical Downscaling NetCDF files into seasonal rasters
#for STEPS 1 & 2 --> see "ProcessNCDF_DDS_1.R" code
#for STEPS 3 & 4 --> see "ProcessNCDF_DDS_2.R" code
#for STEPS 5 & 6 --> see "ProcessNCDF_DDS_3.R" code

#STEP 7: BIAS CORRECTION OF DYNAMICAL RESULTS (8/21/2019)
#Using Delta Change Approach (see Teutschbein & Seibert 2012 or Maraun 2016 for equations/details)
#  Use Regional Climate Model (RCM)-simulated future change (anomalies) for a perturbation of observed data 
#    (rather than to use the RCM-simulations of future conditions directly)
#  Superimpose RCM anomalies (between control & scenario runs) on observational time series 
#  Multiplicative correction for Precip (relative changes): Use Percent Change
#  Additive correction for Temperature: Use Absolute Change

#MAKE SURE OBSERVATIONS HAVE SAME REFERENCE PERIOD AS USED IN RCM (1990-2009)


rm(list=ls())

library('raster')
#library('ncdf4')
#library('rgdal')
#library('sp')
#library('maptools') #for old shapefile read command
#library(rasterVis) #for plots




##############################################################################################

#STEP 7: BIAS CORRECTION: calculate future means and future absolute change from observation-based present-day maps (same matching base period)
# ??  For Temperature, use the Climate of Hawaii mean annual temp. map (Giambelluca et al. 2014) as base?? 
#   This will allow you to compare absolute values from both SD and DD products.

#FIRST calculate Observational present-day means from 1990-2009 (calculated from month-year maps)

#Then for BIAS CORRECTION: Open scenario future % change from Dyn DS 
#Multiply % change by present-day mean (from observations) in inches to get future Dyn DS change in inches, 
#   then add to OBS present day to get future mean (do for all units)
#For temperature no %: instead use abs change plus OBS present day mean to get new future mean


#RAINFALL:

library('raster')

#CREATE 1990-2009 MEANS FROM MONTH-YEAR MAPS:****************************

#open Rainfall Atlas statewide grid to set extent/size:
setwd("H:\\GIS_Layers\\RainfallAtlas\\RF_inches\\State")
rfa_mask<-raster("staterf_inann")
P4S.latlon <- CRS(projection(rfa_mask))

#create 1990-2009 means based on observations (month-year maps, Frazier et al. 2016)
#ANNUAL:
setwd("C:/AbbyF/GIS_Layers/MonthYearRainfallMaps/Month_Rasters_State_in/Annual")
s=stack()

for (ii in 1990:2009){  #loop through yearly rasters
  r<-raster(paste("stann_",ii,"_in",sep=""))
  s<-stack(s,r)
}
obsmean.in.ann<-mean(s,na.rm=TRUE)
obsmean.mm.ann<-obsmean.in.ann*25.4
obsmean.in.ann<-mask(obsmean.in.ann,rfa_mask)  #mask to coastline of rainfall atlas layer
obsmean.mm.ann<-mask(obsmean.mm.ann,rfa_mask)  #mask to coastline of rainfall atlas layer

setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_4PresentDay_SeasMeans_RF_250m")
writeRaster(obsmean.in.ann, "Obs_HI_RF_present_mean_in_Ann_2009.tif", format="GTiff",overwrite=TRUE)
writeRaster(obsmean.mm.ann, "Obs_HI_RF_present_mean_mm_Ann_2009.tif", format="GTiff",overwrite=TRUE)


#create 6-month SEASONAL MEANS (1990-2009, based on observations, month-year maps Frazier et al. 2016)
#1st: create YEARLY SEASONAL FILES for entire time period, afterwards: average the 1990-2009 period

seas2_list<-c("Dry","Wet")
seas3_list<-c("May_Oct","Nov_Apr")

drymos<-c("may","jun","jul","aug","sep","oct")
wetmos<-c("nov","dec","jan","feb","mar","apr")

for (ii in 1920:2012){  #loop through years
  
  for (jj in 1:2) { #loop through seasons
    setwd("C:/AbbyF/GIS_Layers/MonthYearRainfallMaps/Month_Rasters_State_mm_ALL")
    s=stack()
    
    seas2<-seas2_list[jj] #dry or wet
    seas3<-seas3_list[jj] #dry or wet
    
    if(seas2=="Dry"){ 
      moslist<-drymos
      yrlist<-c(ii,ii,ii,ii,ii,ii)
    }
    if(seas2=="Wet"){ 
      moslist<-wetmos
      yrlist<-c(ii,ii,ii+1,ii+1,ii+1,ii+1) #wet season grabs months from next year
      if(ii==2012){break} #wet season 2012 can't do 2013 half, exit loop
    }
    
    for (kk in 1:6) { #add 6 months to season
      mo<-moslist[kk]
      yr<-yrlist[kk]
      r<-raster(paste("st",mo,yr,"_mm",sep=""))
      s<-stack(s,r)
    }
    #sum months to get 6-month seasonal value:
    obs.seasyr.mm<-sum(s,na.rm=TRUE)
    obs.seasyr.in<-obs.seasyr.mm/25.4
    obs.seasyr.mm<-mask(obs.seasyr.mm,rfa_mask)  #mask to coastline of rainfall atlas layer
    obs.seasyr.in<-mask(obs.seasyr.in,rfa_mask)  #mask to coastline of rainfall atlas layer
    
    #write output, inches & mm
    setwd("C:/AbbyF/GIS_Layers/MonthYearRainfallMaps/Seas_Rasters_State_in")
    writeRaster(obs.seasyr.in, paste("StateRF_",seas3,"_",ii,"_in.tif",sep=""), format="GTiff",overwrite=TRUE)
    setwd("C:/AbbyF/GIS_Layers/MonthYearRainfallMaps/Seas_Rasters_State_mm")
    writeRaster(obs.seasyr.mm, paste("StateRF_",seas3,"_",ii,"_mm.tif",sep=""), format="GTiff",overwrite=TRUE)
    
  }
}


#AVERAGE YEARLY SEAS FILES TO create 1990-2009 Seas MEANS based on observations (month-year maps, Frazier et al. 2016)
#WET & DRY SEAS:
setwd("C:/AbbyF/GIS_Layers/MonthYearRainfallMaps/Seas_Rasters_State_in")
s=stack()
s2=stack()

for (ii in 1990:2009){  #loop through yearly rasters
  r<-raster(paste("StateRF_May_Oct_",ii,"_in.tif",sep=""))
  s<-stack(s,r)
}
for (ii in 1990:2008){  #loop through yearly rasters - one less year for wet season (2008 = nov,dec 2008 through april 2009)
  r2<-raster(paste("StateRF_Nov_Apr_",ii,"_in.tif",sep=""))
  s2<-stack(s2,r2)
}

obsmean.in.dry<-mean(s,na.rm=TRUE)
obsmean.in.wet<-mean(s2,na.rm=TRUE)
obsmean.in.dry<-mask(obsmean.in.dry,rfa_mask)  #mask to coastline of rainfall atlas layer
obsmean.in.wet<-mask(obsmean.in.wet,rfa_mask)  #mask to coastline of rainfall atlas layer

obsmean.mm.dry<-obsmean.in.dry*25.4
obsmean.mm.wet<-obsmean.in.wet*25.4
obsmean.mm.dry<-mask(obsmean.mm.dry,rfa_mask)  #mask to coastline of rainfall atlas layer
obsmean.mm.wet<-mask(obsmean.mm.wet,rfa_mask)  #mask to coastline of rainfall atlas layer

setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_4PresentDay_SeasMeans_RF_250m")
writeRaster(obsmean.in.dry, "Obs_HI_RF_present_mean_in_Dry_2009.tif", format="GTiff",overwrite=TRUE)
writeRaster(obsmean.mm.dry, "Obs_HI_RF_present_mean_mm_Dry_2009.tif", format="GTiff",overwrite=TRUE)
writeRaster(obsmean.in.wet, "Obs_HI_RF_present_mean_in_Wet_2009.tif", format="GTiff",overwrite=TRUE)
writeRaster(obsmean.mm.wet, "Obs_HI_RF_present_mean_mm_Wet_2009.tif", format="GTiff",overwrite=TRUE)



#BIAS CORRECTION ****************************************************************************************************
#NOW RECREATE FUTURE MEANS AND FUTURE ABSOLUTE CHANGE BASED ON THESE OBS-BASED PRESENT DAY MEANS (1990-2009)
#loop through seasons, rcps, units:

tm_list<-c("rcp45","rcp85")
seas_list<-c("Ann","Dry","Wet")
rfunits_list<-c("mm","in")

for (x in 1:3){  #loop through seasons
  seas<-seas_list[x]
  
  for (y in 1:2){  #loop through rcps 
    tm<-tm_list[y]
    
    cent<-"late"
    cent3<-"2100"
    
    for (zz in 1:2) { #loop through units (mm and in)
      rfunits<-rfunits_list[zz]
      #SET PRESENT MEAN FOR THIS SEASON/UNITS (based on observations):
      setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_4PresentDay_SeasMeans_RF_250m")
      pres.OBSmn<-raster(paste("Obs_HI_RF_present_mean_",rfunits,"_",seas,"_2009.tif",sep=""))
      
      #open percent change rasters:
      setwd("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_1FutureSeasChange_RF_Pct_250m")
      pctchg.dds<-raster(paste("DynDS_HI_RF_pct_chng_",tm,"_",seas,"_",cent3,".tif",sep=""))
      
      #Use pct change to calculate future change relative to DDS mean, Add that to DDS present mean, create new SDS Future Mean (comparable to DDS-derived futures)
      newfut.chg<-(pctchg.dds / 100) * pres.OBSmn
      newfut.mean<-newfut.chg + pres.OBSmn
      
      #Save new outputs (Final Bias-corrected outputs):
      setwd(paste("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_OBS\\DynDS_FutureSeasChange_RF_",rfunits,"_250m_obs",sep=""))
      writeRaster(newfut.chg, paste("DynDS_HI_RF_",tm,"_chng_",rfunits,"_",seas,"_",cent3,"_obs.tif",sep=""), format="GTiff",overwrite=TRUE)
      setwd(paste("H:\\Downscaling\\Dynamical\\CMIP5\\DynDS_RainfallTIF\\DynDS_2FutureMeans_AbsChange_OBS\\DynDS_FutureSeasMeans_RF_",rfunits,"_250m_obs",sep=""))
      writeRaster(newfut.mean, paste("DynDS_HI_RF_",tm,"_mean_",rfunits,"_",seas,"_",cent3,"_obs.tif",sep=""), format="GTiff",overwrite=TRUE)
      
    }
  }
}



