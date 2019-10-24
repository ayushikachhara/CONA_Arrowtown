### import libraries ##
library(data.table)
library(ggplot2)
library(gtable) ## arranging plots
library(zoo)
library(dplyr)
library(gridExtra)
library(grid)
library(plotly)
library(RColorBrewer)
library(statsr)
library(dplyr)
library(pracma)
library(openair)
library(lubridate)
library(tidyr)
library(tidyverse)
library(xlsx)
library(mapplots)
library(mapview)
library(leaflet)
library(rgdal)
library(sp)
library(raster)
library(rgeos) ## gDistance function is from this packages
library(gstat)
library(ggmap)
library(gganimate)
library(gifski)
library(curl)
library(googledrive)


## setwd ####
path <- "S:/kachharaa/CONA/Arrowtown2019/hauhau/phase4/"
setwd(path)

latlon_CRS <- "+proj=longlat +datum=WGS84"
NZTM_CRS <- "+init=epsg:2193"

## import hh data - phase 3 ###
allhh <- read.csv("S:/kachharaa/CONA/Arrowtown2019/hauhau/phase4/allHH_2019-09-12.csv", 
                  stringsAsFactors = F)

# ## import hh data - phase 2 ###
# allhh <- read.csv("S:/kachharaa/CONA/Arrowtown2019/hauhau/phase2/allHH_2019-09-06.csv", 
#                   stringsAsFactors = F)



allhh$date <- as.POSIXct(allhh$date, format ="%Y-%m-%d %H:%M:%S")


allhouseids <- unique(allhh$HouseID)
allhh.list <- list()
for(i in 1:length(allhouseids)){
  cur.hh <- allhh %>% filter(HouseID == allhouseids[i])
  ## correct for temperature glitches ###
  cur.hh$tempCorr <- hampel(cur.hh$Temperature, 60)$`y` 
  cur.hh$date <- round_date(cur.hh$date, "10 minute")
  cur.hh10min <- timeAverage(cur.hh, avg.time = '10 min', type = c("HouseID","HH_ID","IB_ID"))
  allhh.list[[i]] <- cur.hh10min
  print(i)
}
allhh <- rbindlist(allhh.list)


## import odin data - phase 3 ###

ODINs.master <- read.csv("S:/kachharaa/CONA/Arrowtown2019/ODINs/allODIN10min_20190923.csv", 
                         stringsAsFactors = F)

ODINs.master$date <- as.POSIXct(ODINs.master$date, format ="%Y-%m-%d %H:%M:%S")

# i = 1
for(i in 1:length(allhouseids)) {
  cur.hh <- allhh %>% filter(HouseID == allhouseids[i])
  start.date <- min(cur.hh$date)
  end.date <- max(cur.hh$date)
  
  ## subset dataset for current house ###
  cur.odins <- ODINs.master %>% filter(date %within% interval(start.date,end.date))
  
  ### report on coverage ####
  coverage.report <- cur.odins %>% 
    group_by(serialn, lat,lon) %>%
    filter(!is.na(PM2.5)) %>%
    summarise(count = n(),
              ideal.count = as.numeric(end.date - start.date)*144)
  
  coverage.report <- coverage.report %>% filter(!is.na(lat))
  cur.hh.pos <- cbind.data.frame(HouseID = cur.hh$HouseID[1],
                                 NZTM_E = cur.hh$NZTM_E[1],
                                 NZTM_N = cur.hh$NZTM_N[1])
  
  ## convert to a spatial object - both House and ODIN dataset####
  coordinates(cur.hh.pos) <- ~NZTM_E +NZTM_N
  proj4string(cur.hh.pos) <- NZTM_CRS
  
  coordinates(coverage.report) <- ~lon + lat
  proj4string(coverage.report) <- latlon_CRS
  coverage.report <- spTransform(coverage.report, CRS(NZTM_CRS))
  
  ## find distance to all ODINs for current house ###
  dist.all <- gDistance(cur.hh.pos, coverage.report, byid = T)
  dist.all <- melt(dist.all)
  colnames(dist.all) <-c("ODINserialn_rowno","HouseID","distance")
  dist.all$HouseID <- allhouseids[i]
  dist.all$serialn <- coverage.report$serialn[dist.all$ODINserialn_rowno]
  dist.all$count <- coverage.report$count[dist.all$ODINserialn_rowno]
  
  ## order based on distance and count ###
  ranked.dist.all <- dist.all[order(dist.all$distance, dist.all$count),][c(1:4),]
  ranked.dist.all$rank <- c("Primary","Secondary","Tertiary", "Quaternary")
  
  ## primary ODIN data ###
  p.odin <- ODINs.master %>% filter(serialn == ranked.dist.all$serialn[1])
  
  p.odin <- p.odin[,c("date","serialn","lat","lon",
                      "PM1", "PM2.5",
                      "PM10","Temperature","RH")]
  
  colnames(p.odin)[2:9] <- paste0("p.odin.", colnames(p.odin)[2:9])
  
  cur.hh <- merge(cur.hh, p.odin, by = "date", all.x = T)
  cur.hh$p.dist <- ranked.dist.all$distance[1]
  
  ## secondary ODIN data ###
  s.odin <- ODINs.master %>% filter(serialn == ranked.dist.all$serialn[2])
  
  s.odin <- s.odin[,c("date","serialn","lat","lon",
                      "PM1", "PM2.5",
                      "PM10","Temperature","RH")]
  
  colnames(s.odin)[2:9] <- paste0("s.odin.", colnames(s.odin)[2:9])
  
  cur.hh <- merge(cur.hh, s.odin, by = "date", all.x = T)
  cur.hh$s.dist <- ranked.dist.all$distance[2]
  
  
  ## tertiary ODIN data ###
  t.odin <- ODINs.master %>% filter(serialn == ranked.dist.all$serialn[3])
  
  t.odin <- t.odin[,c("date","serialn","lat","lon",
                      "PM1", "PM2.5",
                      "PM10","Temperature","RH")]
  
  colnames(t.odin)[2:9] <- paste0("t.odin.", colnames(t.odin)[2:9])
  
  cur.hh <- merge(cur.hh, t.odin, by = "date", all.x = T)
  cur.hh$t.dist <- ranked.dist.all$distance[3]
  
  ## quaternary ODIN data ###
  q.odin <- ODINs.master %>% filter(serialn == ranked.dist.all$serialn[4])
  
  q.odin <- q.odin[,c("date","serialn","lat","lon",
                      "PM1", "PM2.5",
                      "PM10","Temperature","RH")]
  
  colnames(q.odin)[2:9] <- paste0("q.odin.", colnames(q.odin)[2:9])
  
  cur.hh <- merge(cur.hh, q.odin, by = "date", all.x = T)
  cur.hh$q.dist <- ranked.dist.all$distance[4]
  
  allhh.list[[i]] <- cur.hh
  print(paste(allhouseids[i], "has", nrow(cur.hh), "rows of data"))
  
  cur.hh$week <- isoweek(cur.hh$date)
  cur.hh$weekno <- cur.hh$week - (min(cur.hh$week)) +1
  curr.hh <- cur.hh %>% filter(date >= as.POSIXct("2019-06-01 09:00:00",
                                                  format = "%Y-%m-%d %H:%M:%S"))
  
}
## for each hh 'active period' - determine - 3 nearest odins and their performance during that period
allhh <- rbindlist(allhh.list)

write.csv(allhh, "hh-odin-P4_20190923.csv", row.names = F)

