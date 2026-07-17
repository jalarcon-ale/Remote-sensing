library(readxl)
library(geosphere)

setwd("C:/Users/ajag1/Documents/Postgraduate/Remote sensing/assignment")


MetOpA_Uccle <- read_excel("METOPA_2007_2013_Ukkel.xlsx")
MetOpA_De_Bilt <- read_excel("METOPA_2007_2013_DE_BILT.xlsx")

##########################################################
###Calculating distances between stations and pixels' coordinates  

###Pixels' coordinates
Uccle_pixel_lon = MetOpA_Uccle$Longitude
Uccle_pixel_lat = MetOpA_Uccle$Latitude
#pixels=paste(pixel_lat,pixel_lon, sep = ', ')
Bilt_pixel_lon = MetOpA_De_Bilt$Longitude
Bilt_pixel_lat = MetOpA_De_Bilt$Latitude


###Station's coordinates
Uccle_lon = 4.35
Uccle_lat = 50.8
Uccle = c(Uccle_lon,Uccle_lat)
De_Bilt_lon = 5.18 #Rounded up for consistency with Uccle's coordinates
De_Bilt_lat = 52.1
De_Bilt = c(De_Bilt_lon,De_Bilt_lat)


###Adding a column with distance to station
MetOpA_Uccle$dist_km <- distHaversine(
  matrix(Uccle, nrow = nrow(MetOpA_Uccle), ncol = 2, byrow = TRUE),
  cbind(Uccle_pixel_lon, Uccle_pixel_lat),
  r = 6371000
) / 1000

max(MetOpA_Uccle$dist_km)


MetOpA_De_Bilt$dist_km <- distHaversine(
  matrix(De_Bilt, nrow = nrow(MetOpA_De_Bilt), ncol = 2, byrow = TRUE),
  cbind(Bilt_pixel_lon, Bilt_pixel_lat),
  r = 6371000
) / 1000

max(MetOpA_De_Bilt$dist_km)

##########################################
###Summing up stratospheric with tropospheric O3
###In this way, a column for total ozone is created 

MetOpA_Uccle$Total_O3 = MetOpA_Uccle$TROPOZONE+MetOpA_Uccle$STRATOZONE

MetOpA_De_Bilt$Total_O3 = MetOpA_De_Bilt$TROPOZONE+MetOpA_De_Bilt$STRATOZONE
  

###In case some pixels lie beyond the circle  
###Filtering out pixels within a 100 km radius 
MetOpA_De_Bilt_filtered = MetOpA_De_Bilt[MetOpA_De_Bilt$dist_km <= 100,]
max(MetOpA_De_Bilt_filtered$dist_km)
