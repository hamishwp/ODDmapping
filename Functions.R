library(openrouteservice)
key<-"5b3ce3597851110001cf62480cfe1584c30047dab9b42da98a6eba3b"

convert2sf<-function(coordz){
  coordz%<>%unname()
  if(length(coordz)==2)  return(sf::st_as_sf(coords = c("lon","lat"), x=data.frame(lon=coordz[1],lat=coordz[2]),
               crs = crs("+proj=longlat +datum=WGS84 +ellps=WGS84")))
  return(sf::st_as_sf(coords = c("lon","lat"), x=data.frame(lon=coordz[,1],lat=coordz[,2]),
                      crs = crs("+proj=longlat +datum=WGS84 +ellps=WGS84")))
}

GetTimeDistance<-function(begin_longitudes,begin_latitudes,end_longitudes,end_latitudes,distance_unit="km"){
  
  # Perform checks first
  if(length(begin_longitudes)!=length(begin_latitudes)) stop("Start location longitude and latitudes are not the same length")
  if(length(end_longitudes)!=length(end_latitudes))  stop("End location longitude and latitudes are not the same length") 
  
  # Bind the values into one list to be used by OpenRouteService
  coordinates<-rbind(c(end_longitudes,end_latitudes),
                     cbind(begin_longitudes,begin_latitudes))
  # Convert from matrix form to list for OpenRouteService
  coordinates<-unname(split(coordinates, c(row(coordinates))))
  if(length(end_latitudes)>1) indies<-as.list(0:(length(end_latitudes)-1)) else indies<-list(0)
  
  # Access & use the OpenRouteService API service
  output<-ors_matrix(api_key = key,locations=coordinates,
                     destinations=indies,
                     metrics = c("duration", "distance"),
                     units = distance_unit)
  # Remove end location time duration values & set time duration to be in minutes
  output$durations<-output$durations[-c(1:length(end_latitudes)),]/60
  # Remove end location distance values 
  output$distances<-output$distances[-c(1:length(end_latitudes)),]
  
  return(output[c("durations","distances")])
  
}

sortCharacter<-function(x){
  tolower(stri_replace_all_fixed(x,pattern =" ",replacement = ""))
}