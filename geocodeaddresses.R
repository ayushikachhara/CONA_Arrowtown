library(ggmap)
library(googledrive)
### defining path of working directory###
path <- "S:/kachharaa/CONA/Arrowtown2019/hauhau/" ##folder where the home folders will be found
setwd(path)
addresses <- read.csv("HouseLocations.csv", stringsAsFactors = F)
addresses <- read.csv("S:/kachharaa/NO2 spatial modelling/modelforeachsite/observationaldata/UO_NO2.csv",
                      stringsAsFactors = F)

mykey = "AIzaSyACi3pNvPQTxZWx5u0nTtke598dPqdgySg"
register_google(key = mykey)

# ggmap(map)
addresses$lon <- NA
addresses$lat <- NA


i = 1
for(i in 1:nrow(addresses)) {
  if(addresses$location[i] == "") {
    addresses$lon[i] <- NA
    addresses$lat[i] <- NA
    addresses$geoAddress[i] <- NA
    print(paste(addresses$HouseID[i], "is blank"))
  }
  result <- geocode(addresses$location[i], output = "latlona", source = "google", force = T)
  addresses$lon[i] <- as.numeric(result[1])
  addresses$lat[i] <- as.numeric(result[2])
  addresses$geoAddress[i] <- ifelse(ncol(result) == 2, NA, as.character(result[3]))
}

map <- get_googlemap(center = "NZ",
                     zoom = 15,scale = 2, color = "bw", maptype = "roadmap")
ggmap(map) +
  geom_point(data = addresses,aes(lon,lat), size = 4, color = "indianred") +
  ggtitle("Study homes - Arrowtown 2019")

addresses <- addresses %>% filter(!site_ID %in% c(440,742,257))

latlon_CRS <- "+proj=longlat +datum=WGS84"
NZTM_CRS <- "+init=epsg:2193"

addresses2 <- addresses[complete.cases(addresses$lat),]
coordinates(addresses2) <- ~lon+lat
proj4string(addresses2) <- CRS(latlon_CRS)

addresses2 <- spTransform(addresses2, CRS(NZTM_CRS))

addresses2 <- as.data.frame(addresses2)
addresses2 <- addresses2[,c(2,5,6)]
colnames(addresses2)[10:11] <- c("NZTM_E_ayushi","NZTM_N_ayushi")
addresses2 <- addresses2[,c(1,10:11)]
addresses <- merge(addresses, addresses2, by = "site_ID", all = T)

write.csv(addresses, 
          "S:/kachharaa/NO2 spatial modelling/modelforeachsite/observationaldata/UO_NO2_geocoded_locations.csv", 
          row.names = F)
