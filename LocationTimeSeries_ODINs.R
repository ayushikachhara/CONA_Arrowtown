library(data.table)


# path <- "S:/kachharaa/CONA/Arrowtown2019/ODINs/"
# setwd(path)

## creating location timeseries till date ####
odin.loc.info <- fread("S:/kachharaa/CONA/Arrowtown2019/ODINs/odin_locations.txt", stringsAsFactors = F)
odin.loc.info$startdate <- dmy_hm(odin.loc.info$startdate, tz = "Pacific/Auckland")
odin.loc.info$enddate <- dmy_hm(odin.loc.info$enddate, tz = "Pacific/Auckland")

### currently valid locations get todays timestap attached ####
odin.loc.info$enddate[which(is.na(odin.loc.info$enddate))] <- Sys.time()  + 86400

### creating 1 minute time series of locations#####
location.list <-list()
for(i in 1:nrow(odin.loc.info)) {
  cur.loc <- odin.loc.info[i,]
  
  locations.ts <- data.table(date = seq(cur.loc$startdate, cur.loc$enddate, by = "1 min"))
  locations.ts$serialn <- cur.loc$serialn
  locations.ts$serialn_concat <- cur.loc$serialn_concat
  locations.ts$lat <- cur.loc$lat
  locations.ts$lon <- cur.loc$lon
  
  location.list[[i]] <- locations.ts 
  
}

allLocations <- rbindlist(location.list)
