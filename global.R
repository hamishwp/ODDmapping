library(dplyr)
library(magrittr)
library(leaflet)
# source("../IIDIPUS/RCode/GetPopDemo.R")
library(raster)
library(shinyWidgets)
library(shinythemes)
library(leaflet.extras)
# devtools::install_github("rstudio/fontawesome")
library(fontawesome)
library(ggplot2)
library(leafpop)
library(ggmap)
library(stringr)
library(osmdata)
library(sp)
library(sf)
library(osrm)
library(geosphere)
# devtools::install_github("rCarto/osrm")

idirectory<-"../IIDIPUS/"
# source(paste0(idirectory,"RCode/ODDobj.R"))

setClass("ODD", 
         slots = c(dir="character",
                   hazard="character",
                   cIndies="data.frame",
                   fIndies="list",
                   IDPs="data.frame", # includes measurement dates
                   gmax="list",
                   alerts="data.frame",
                   I0="numeric",
                   hazdates="Date",
                   eventid="numeric",
                   predictDisp="data.frame"),
         contains = "SpatialPixelsDataFrame")

as.raster.ODD<-function(ODD){
  
  Object<-SpatialPixelsDataFrame(points = ODD@coords,
                                 data = ODD@data,
                                 proj4string = ODD@proj4string,
                                 grid = ODD@grid)
                                
  return(raster(Object))

}

PrepPred<-function(predictDisp){
  
  predictDisp%<>%dplyr::select(-qualifier)
  predictDisp[,c("gmax","predictor")]%<>%as.integer
  colnames(predictDisp)<-c("Country","Obs. Disp.","Pred. Disp.")
  
  return(predictDisp)
  
}

population<-raster(x=paste0(idirectory,
      # "Demography_Data/Population/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif"))
      "Demography_Data/Population/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_15_min.tif"))
# FBpop<-raster(x="./data/population_fji_2018-10-01.tif")
qpal <- colorQuantile("Greys", values(population), n = 6, na.color = "transparent")
leg_pal<-rev(unique(qpal(values(population)))) #[-c(2,8)]
leg_val<-rev(signif(quantile(population,probs=(0:6/6)),1)) # [-c(2,8)]

# ufiles<-list.files(path=paste0(idirectory,"IIDIPUS_Results/ODDobjects_ODDRIN"),pattern="",recursive = T,ignore.case = T)
# Extract DispData, attach centroid value from ODDobject, attach object filename for quick extraction
# DispData<-as.data.frame(read.csv(paste0(idirectory,"IIDIPUS_Input/DispData_EQ.csv")))
# DispData$Longitude<-DispData$Latitude<-DispData$filename<-NA
# for (ev in unique(DispData$eventid)){
#   ftmp<-ufiles[endsWith(ufiles,suffix = paste0("_",as.character(ev)))]
#   if(length(ftmp)<1) next
#   ODDy<-readRDS(paste0(idirectory,"IIDIPUS_Results/ODDobjects_ODDRIN/",ftmp))
#   tmp<-ODDy@coords[which.max(ODDy$hazMean1),]
#   DispData$Longitude[DispData$eventid==ev]<-tmp[1]
#   DispData$Latitude[DispData$eventid==ev]<-tmp[2]
#   DispData$filename[DispData$eventid==ev]<-paste0(idirectory,"IIDIPUS_Results/ODDobjects_ODDRIN/",ftmp)
# }

# Plot only these objects
# 





# Run IIDIPUS and save out ODD objects for ODDRIN to read, including predictDisp and Disp
# Use beta=0, linpred_optim and full_optim versions to predict displacement



# source ODDRIN
# Create a code to evaluate central point of earthquakes by mean of ODDRIN bbox
# Attach this to DispData, including maximum hazard magnitude & alertscore
# Use ODD@hazard for tag
# Create zoom-in plot of ODD object data:
  # plot hazard intensities for all hazards
  # plot Damage percentages & variance
  # plot Displacement values
  # plot building damage points
# Include all cIndy values on the RHS
# Develop polygon counting tool (mean for hazard & damage, sum for displacements)
# Give warning that tsunami displacements not included


# Plot both PHL earthquakes in 2019 on one map (ggmap using modified bbox) then show overlapping regions
# IRN & IRQ border split
# Find an EQ with multiple aftershocks, all dispersed around
# Plot income distributions and their mean
# Discuss how to predict long term displacement using stats method (EM-DAT) and IPM (FB & shelter counts) 
# How to tackle economic impact/cost predictive method
# Easily applicable to tropical cyclones, floods and storms
# Highlight the shit out of multiple or continuing hazard displacement capacity
# Talk about which vulnerabilities seem to play an important role!




# load("./Historical_IIDIPUS") # subCM parameter
# # anynan<-apply(PopCounts, 1, function(x) any(is.na(x)))
# # subCM<-CM%>%filter(eventid%in%eventy[!anynan])
# 
# dfGDACS<-readRDS(paste0(idirectory,"Disaster_Data/GDACS/GDACS_all_events_red-ora-grn_DF.Rdata"))
# dfGDACS$latitude<-dfGDACS$lat ; dfGDACS$lat<-NULL
# dfGDACS$longitude<-dfGDACS$long ; dfGDACS$long<-NULL
# dfGDACS%<>%dplyr::select(alert,alertscore,latitude,longitude,sdate,hazard_type,eventid)
#   # filter(sdate>(Sys.Date()-180) & alertscore>0.5)
#   # filter(alertscore>0.5)
# 
# polyframe<-readRDS(paste0(idirectory,"Disaster_Data/GDACS_polyframe.Rdata")) %>% 
#   filter(id==1)%>%dplyr::select(-id)
# 
# dfGDACS%<>%filter(eventid%in%unique(polyframe$eventid))
# 
# helix<-readRDS(file = paste0(idirectory,"Helix/helix_save_14-08-20.Rdata")) %>% 
#   filter(hazard_type=="Earthquake") %>% 
#   dplyr::select(hazard_type,event_id,iso3,country,type,figure,term,date,event_name,created_by, start_date)
# 
# load(paste0(idirectory,"IIDIPUS_Results/Historical_Analysis/Results_EQ_RD1_v3.RData"))
# anynan<-apply(PopCounts, 1, function(x) any(is.na(x)))
# subCM<-CM%>%filter(eventid%in%eventy[!anynan])
# subCM%<>%dplyr::select(-c(day,mxdate,hazard_date,hday))
# subCM$hazard_alert<-round(subCM$hazard_alert,2)
# severity<-subCM%>%group_by(eventid)%>%summarise(haz=max(hazard_severity),gmax=max(gmax),IDP=max(IDP),
#                                                 iso3=unique(iso3)[1],alert=unique(hazard_alert)[1],
#                                                 sdate=min(sdate))
# 
# source(paste0(idirectory,"RCode/GetINFORM.R"))
# source(paste0(idirectory,"RCode/GetSocioEconomic.R"))
# source(paste0(idirectory,"RCode/Functions.R"))
# 
# Ephys<-GetINFORMdata("HA.NAT.EQ",AsYear(Sys.Date()),normalise = F)
# print("WARNING: INFORM data based on current year, should be interpolated")
# Infra<-GetINFORMdata("CC.INF",AsYear(Sys.Date()),normalise = F)
# VU<-GetINFORMdata("VU",AsYear(Sys.Date()),normalise = F)
# INFORM<-GetINFORMdata("INFORM",AsYear(Sys.Date()),normalise = F)
# # INCOME<-GetWB("NY.ADJ.NNTY.PC.CD",Idate=2015)
# severity<-Reduce(function(x,y) merge(x = x, y = y, by = "iso3"), list(severity,Ephys,VU,Infra,INFORM))
# 
# allnan<-apply(PopCounts, 1, function(x) all(is.na(x)))
# anynan<-apply(PopCounts, 1, function(x) any(is.na(x)))
# severity%<>%arrange(desc(eventid))%>%filter(eventid%in%eventy)%>%cbind(PopCounts[!anynan,],MaxGDP[!anynan,4],MaxPopDens[!anynan,4])
# colnames(severity)[(dim(severity)[2]-1):dim(severity)[2]]<-c("MaxGDP_6M","MaxPopDens_6M")
# severity[["MaxPopDens_6M"]][is.infinite(severity[["MaxPopDens_6M"]])]<-0
# severity[["MaxGDP_6M"]][is.infinite(severity[["MaxGDP_6M"]])]<-0
# # severity%<>%filter(day<20)
# for (chr in paste0(cIntMap[2:7],"-PopCounts")){
#   severity[[chr]]<-ceiling(severity[[chr]]/5)
# }
# 
# rm(allnan,anynan,INFORM,VU,Infra,Ephys,ZoneArea,PopCounts,MaxGDP,MaxPopDens)
# 
# # severity%<>%filter(abs(hday)<15 & abs(day)<20)%>%select(-c(hday,day))
# if(hazard=="EQ") {
#   severity%<>%dplyr::select(-c("9-PopCounts"))
#   cIntMap<-cIntMap[-lenIM]
#   cIntMap_m<-as.character((IntMap-0.5)) ; cIntMap_m<-cIntMap_m[-lenIM]  
#   IntMap<-IntMap[-lenIM]
# }
# # GIDD<-GetGIDD(idirectory)

Icons<-awesomeIconList(
  # awesomeIcons("gripfire",library = "fa")
  ST = makeAwesomeIcon("ios-thunderstorm",library = "ion",markerColor = "purple",iconColor = "white"),
  EQ = makeAwesomeIcon("ios-pulse-strong",library = "ion",markerColor = "green",iconColor = "white"),
  DR = makeAwesomeIcon("thermometer-full",library = "fa",markerColor = "orange",iconColor = "white"),
  FL = makeAwesomeIcon("waterdrop",library = "ion",markerColor = "blue",iconColor = "white"),
  TC = makeAwesomeIcon("social-chrome",library = "ion",markerColor = "blue",iconColor = "white")
)

# p<-ggplot(longData, aes(x = Var1, y = Var2)) +
#   geom_raster(aes(fill=value)) +
#   scale_fill_gradient(low="grey90", high="red",name = "GDP (PPP) [$]") +
#   labs(x="Longitude", y="Latitude", title="Philippines Earthquake 15 December 2019") +
#   theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
#                      axis.text.y=element_text(size=9),
#                      plot.title=element_text(size=11))

