library(dplyr)
library(geosphere)
library(slider)
library(sf)
library(sfheaders)
## Functions 
count_return <- function(long, lat, dist) {
  clonglat <- cbind(long,lat)
  count <- rowSums(distm(clonglat[1,],clonglat, 
                         fun = distHaversine)<= dist) # Average max quantile distance for 8 collars in varying canopy densities was 34m
  return(count)
}

# Data Pre-wrangle
# # Selecting raw data from the Vence_api data folder
file <- read.csv(row.names(file.info(list.files(paste0(getwd(),"/CollarData"),full.names = T))%>% arrange(desc(mtime)))[1]) 
sampdata <-file[
  order( file[,4], file[,3] ),
] %>% select(-X,-uuid);sampdata$date<- lubridate::ymd_hms(sampdata$date)
sampdata <- sampdata[!is.na(sampdata$date),]
# Selecting collar list to filter data from the api pull
Metafiles <-file.info(list.files(paste0(getwd(),"/TargetCollars"),full.names = T))%>% arrange(desc(mtime))

Meta<- read.csv(row.names(Metafiles)[1]) %>% select("collar" = DeviceEUI,EarTag,HerdName)
NMC_uncleaned <- sampdata %>% left_join(Meta, by = "collar") %>% tidyr::drop_na(HerdName)
NMC_uncleaned <- NMC_uncleaned%>% filter(grepl('GpsLocationExtIndication', messagetype))
NMC_uncleaned <- NMC_uncleaned %>% group_by(collar)%>% arrange(collar,date) %>% ungroup()
NMC_uncleaned <- NMC_uncleaned %>% mutate(longitude = as.numeric(longitude),latitude = as.numeric(latitude) )
NMC_uncleaned <- NMC_uncleaned[!is.na(NMC_uncleaned$latitude),]
rm(file,sampdata, Metafiles)

## Step 1:Removing Distance Errors
# Add Distance for each collar group, rbind, mutate the time between gps points, assess the rate of movement as DistTime (m/min)
NMC_listed <- split(NMC_uncleaned,NMC_uncleaned$collar)
if (length(Filter(function(x) any(nrow(x) < 2),NMC_listed))>0) {print(paste0("Collar ",names(Filter(function(x) any(nrow(x) < 2),NMC_listed))[[1]]," only has one point and cant be found"));NMC_listed <- Filter(function(x) any(nrow(x) > 1),NMC_listed)}
for (i in 1:length(NMC_listed)) {
  NMC_listed[[i]]$Dist <- c(NA)
  for (j in 1:(nrow(NMC_listed[[i]])-1)) {
    NMC_listed[[i]]$Dist[j+1]<- distm(c(NMC_listed[[i]][j,5][[1]],
                                        NMC_listed[[i]][j,4][[1]]),
                                      c(NMC_listed[[i]][(j+1),5][[1]],
                                        NMC_listed[[i]][(j+1),4][[1]]),
                                      fun = distHaversine)
  }
}
NMC_uncleaned <- data.table::rbindlist(NMC_listed)

# Add a rate of movement
NMC_uncleaned <- NMC_uncleaned %>% group_by(collar)%>% arrange(collar,date) %>%
  mutate(time_diff = difftime(date,lag(date), units='mins'))

NMC_uncleaned <- NMC_uncleaned %>% group_by(collar) %>% mutate(DistTime=Dist/as.numeric(time_diff))
NMC_uncleaned <-NMC_uncleaned[!NMC_uncleaned$DistTime >=84,] # 84 m/min: movement speed of a cow
NMC_listed <- split(NMC_uncleaned,NMC_uncleaned$collar)
for (i in 1:length(NMC_listed)) {
  NMC_listed[[i]]$Dist <- c(NA)
  for (j in 1:(nrow(NMC_listed[[i]])-1)) {
    NMC_listed[[i]]$Dist[j+1]<- distm(c(NMC_listed[[i]][j,5][[1]],
                                        NMC_listed[[i]][j,4][[1]]),
                                      c(NMC_listed[[i]][(j+1),5][[1]],
                                        NMC_listed[[i]][(j+1),4][[1]]),
                                      fun = distHaversine)
  }
}
NMC_uncleaned <- data.table::rbindlist(NMC_listed)

NMC_Cleaned <-NMC_uncleaned %>%
  group_by(collar)%>%
  filter(date<=max(date) & date >= (max(date)- lubridate::days(30))) %>% 
  arrange(desc(date),.by_group = TRUE)%>%
  mutate(ErrorGPS1 = slide2_dbl(.x = longitude,.y = latitude, ~count_return(.x,.y,34),.after = 25),
         ErrorGPS2 = slide2_dbl(.x = longitude,.y = latitude, ~count_return(.x,.y,34),.before = 25))
rm(NMC_listed,NMC_uncleaned)

 ## GPS count visualizations
# for (i in unique(NMC_uncleanedErrored$collar)) {
# temp <- ggplot2::ggplot(data=NMC_uncleanedErrored[NMC_uncleanedErrored$collar == i,], ggplot2::aes(x=date, y=ErrorGPS1, color = )) +
#     ggplot2::geom_line()+
#     ggplot2::geom_point()+
#   ggplot2::ggtitle(paste(i," 1"))
# temp2 <- ggplot2::ggplot(data=NMC_uncleanedErrored[NMC_uncleanedErrored$collar == i,], ggplot2::aes(x=date, y=ErrorGPS2, color = )) +
#   ggplot2::geom_line()+
#   ggplot2::geom_point()+
#   ggplot2::ggtitle(paste(i," 2"))
#   print(temp);print(temp2)
# }

## Visual observation of center in relation to all points  
# for (i in 1:length(unique(NoMovement$collar))) {
#   x <- unique(NoMovement$collar)
#   df <- NoMovement%>%filter(collar==x[i])
#   m<- ggplot2::ggplot(df, ggplot2::aes(x = longitude, y = latitude)) +
#     ggplot2::geom_point() +
#     ggplot2::geom_point(data = df, ggplot2::aes(x = mean(longitude),y = mean(latitude)),color ="red")+
#     ggplot2::coord_map(xlim = c(min(df$longitude), max(df$longitude)),ylim = c(min(df$latitude), max(df$latitude))) + #limits added as there are some points really far away
#     ggplot2::theme_classic()+ ggplot2::labs(title = x[i])
#   print(m)
# }

# Step 2: writes a shapefile of the clustered gps center point
# May need to  adjust epsg
NoMovementCenter <- NMC_Cleaned[NMC_Cleaned$ErrorGPS1 >=26 |NMC_Cleaned$ErrorGPS2 >=26,] %>% group_by(collar)%>% summarise(longitude=mean(longitude), latitude=mean(latitude))
st_write(st_as_sf(NoMovementCenter, coords = c("longitude", "latitude"), crs = 4326),paste0(getwd(),"/Product/MissingNMCollars","_",lubridate::date(min(NMC_Cleaned$date)),"_",lubridate::date(max(NMC_Cleaned$date)),".shp"))
st_write(st_as_sf(NoMovementCenter, coords = c("longitude", "latitude"), crs = 4326),paste0(getwd(),"/Product/MissingNMCollars","_",lubridate::date(min(NMC_Cleaned$date)),"_",lubridate::date(max(NMC_Cleaned$date)),".kml"),driver = "KML")

