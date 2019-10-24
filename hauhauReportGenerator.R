######### check for available packages #######################

unavailable <- setdiff(c("data.table",'tidyr',
                         'tidyverse','dplyr',
                         'statsr','openair',
                         'zoo','lubridate',
                         'ggplot2','ggalt','gghighlight',
                         'grid','gridExtra', 'plotly',
                         'RColorBrewer', 'mapplots',
                         'mapview','leaflet',
                         'rgdal','sp',
                         'raster','webshot'), rownames(installed.packages()))
if(length(unavailable>0)){
  install.packages(unavailable)

} else {
  "The code below can run on your currently installed packages"
}

## dataframe manipulation packages ####
library(data.table)
library(tidyr)
library(tidyverse)
library(dplyr)
library(statsr)

## air quality package ####
library(openair)

## datetime manipulation packages ####
library(zoo)
library(lubridate)

## plotting/ interactive plotting packages ####
library(ggplot2)
library(ggalt)
library(gghighlight)
library(grid)
library(gridExtra)
library(plotly)
library(RColorBrewer)

## spatial data analysis/ mapping packages ####
library(mapplots)
library( mapview)
library(leaflet)
library(rgdal)
library(sp)
library(raster)
library(webshot)

### defining path of working directory###
path <- "S:/kachharaa/CONA/Arrowtown2019/hauhau/phase4" ##folder where the home folders will be found
setwd(path)

cur.phase = "P04"

## read in instrument register ####
indoor.info <- read.csv("S:/kachharaa/CONA/Arrowtown2019/hauhau/instrument_register.csv", stringsAsFactors = F)
indoor.info$deploydate <- dmy(indoor.info$deploydate)
indoor.info$downloaddate <- dmy(indoor.info$downloaddate)

indoor.info <- indoor.info %>% filter(Phase == cur.phase)## analyse only for current phase ###

listfolders <- list.dirs(path, recursive = F, full.names = F) ## get folder names, recursive = F removes current path name from the list###

### calculate number of points per midday to midday ###
overnight <- seq(ymd_hms("2019-05-29 12:00:00"),
                 ymd_hms("2019-11-04 12:00:00"), by = "day")


allhh.list <- list () ## initiate an empty list to store hauhau data 
k = 1 ## initial a list counter ###
for(i in 1:length(listfolders)) {
  cur.folder <- listfolders[i] ## housename from current folder name ####
  
  indoor.info.sub <- indoor.info %>% filter(HouseID == cur.folder)
  
  ## if hauhau was faulty or not installed ####
  if (indoor.info.sub$status_hh == "faulty" | 
      indoor.info.sub$status_hh == "notinstalled" | 
      indoor.info.sub$status_hh == "nodata" |
      indoor.info.sub$status_hh == "noconsent") {
    
    print(paste(i,"Hauhau from",cur.folder, "was/has",indoor.info.sub$status_hh))
    next
  } 
  
  ## read the hh file from current house folder ##
  cur.path <- paste0(path,"/",cur.folder,"/")
  cur.file <-  list.files(path = cur.path, pattern = '.txt')
  cur.hh <-  fread(paste0(cur.path,cur.file))
  
  cur.hh$HouseID <- cur.folder ## assign houseID
  cur.hh <- merge(cur.hh, indoor.info.sub[,c(1:9)], by = "HouseID", all.x = T)
  
  cur.hh$TimeStamp <- ymd_hms(cur.hh$TimeStamp) ## creating a 'datetime object'
  cur.hh <- cur.hh %>% filter(TimeStamp %within% interval(unique(deploydate),unique(downloaddate)))
  cur.hh$DateOnly <- as.Date(cur.hh$TimeStamp)
  colnames(cur.hh)[2] <- "date"
  cur.hh$date <- round_date(cur.hh$date, unit = "minute") ## round to nearest minute
  
  cur.hh$hour <- hour(cur.hh$date)
  
  cur.hh$daytime <- ifelse(cur.hh$hour>=6 & cur.hh$hour<12, "Morning",
                           ifelse(cur.hh$hour>=12 & cur.hh$hour<18, "Afternoon",
                                  ifelse(cur.hh$hour>=18, "Evening","Night")))
  
  cur.hh$middaybreaks<- cut(cur.hh$date, overnight, label = FALSE)
  
  data.qual.permidday <- as.data.table(table(cur.hh$middaybreaks))
  data.qual.permidday <- data.qual.permidday %>% filter(N>700) ## removing days with less than half day of data
  cur.hh <- cur.hh[which(cur.hh$middaybreaks %in% as.numeric(data.qual.permidday$V1)),]
  cur.hh$start.date <- min(cur.hh$DateOnly, na.rm = T)
  cur.hh$end.date <- max(cur.hh$DateOnly, na.rm = T)
  allhh.list[[k]] <- cur.hh
  k = k+1
  # print(paste("House number",cur.folder, ": filtering finished"))
  
}
print("Dataset from each home has been filtered and formatted.")
print(paste((i-(k-1)),"out of" ,i, "homes will not be included in the analysis"))

allhh <- rbindlist(allhh.list) ## create a master 1 minute file
write.csv(allhh, paste0("allHH_", Sys.Date(),".csv"), row.names = F)
print ("Filtering raw data completed, summarising now..")


midday.summaries <- allhh %>% group_by(HouseID,middaybreaks) %>% 
    summarise(nightof = unique(DateOnly)[1],
              count = n(),
              NZTM_E = mean(NZTM_E, na.rm = T),
              NZTM_N = mean(NZTM_N, na.rm = T),
              PM2.5 = mean(PM2.5, na.rm = T),
              PM10 = mean(PM10, na.rm = T),
              CO2 = mean(CO2, na.rm = T),
              Temperature = mean(Temperature, na.rm = T),
              Humidity = mean(Humidity, na.rm = T))

write.csv(midday.summaries, paste0("HH_DailySummaryReport_", Sys.Date(),".csv"), row.names = F)

## calculate midday summaries ###
house.summaries.midday <- midday.summaries %>% group_by(HouseID) %>% 
  summarise(no.days = n(),
            NZTM_E = mean(NZTM_E, na.rm = T),
            NZTM_N = mean(NZTM_N, na.rm = T),
            PM2.5 = mean(PM2.5, na.rm = T),
            PM10 = mean(PM10, na.rm = T),
            CO2 = mean(CO2, na.rm = T),
            Temperature = mean(Temperature, na.rm = T),
            Humidity = mean(Humidity, na.rm = T)) %>%
  mutate(status = ifelse(no.days<4, "Repeat", "Done"))

### subset for observation period only ####
obs.period <- allhh %>% group_by(HouseID) %>%
  summarise(start.date = unique(start.date),
            end.date = unique(end.date))

house.summaries.midday <- merge(house.summaries.midday, obs.period, by = "HouseID", all.x = T)
write.csv(house.summaries.midday, paste0("HH_HouseSummaryReport_", Sys.Date(),".csv"), row.names = F)

print ("Summary per day and per home completed, plotting now..")
## calculate subsequent range of PM in Arrowtown for each home ###

### plots per home #####

for(i in 1:length(unique(allhh$HouseID))){
  cur.house <- unique(allhh$HouseID)[i]
  cur.hh <- allhh %>% filter(HouseID == cur.house)
  # ### PDF path - per home ####
  PDFfile <- paste0(path,"/",cur.house,"_","SummaryPlots_",Sys.Date(),".pdf")
  pdf(file=PDFfile, paper = "USr", width = 28)
  timePlot(cur.hh, c("PM2.5","PM10"))
  timePlot(cur.hh, "CO2", type = "HouseID")
  timePlot(cur.hh, "Temperature", type = "HouseID")
  calendarPlot(cur.hh, "PM2.5", main = "PM2.5")
  
  p1 <- ggplot(cur.hh) + 
    geom_boxplot(aes(hour, PM2.5, group = hour)) + 
    ggtitle("PM2.5 dirunal") +
    scale_x_continuous(breaks = seq(0,23,1)) + theme_bw()
  print(p1)
  
  p2 <- ggplot(cur.hh) + 
    geom_boxplot(aes(y = PM2.5, x = daytime)) +
    ggtitle("PM2.5: 6 hour zones")  + theme_bw()
  print(p2)
  
  p3 <- ggplot(cur.hh) + 
    geom_boxplot(aes(y = PM2.5, x = DateOnly, group = DateOnly)) + 
    scale_x_date(date_breaks = "2 days", date_labels = "%d-%b") +
    ggtitle("PM2.5: Daily")  + theme_bw()
  print(p3)
  
  dev.off()
  
}
print("Plot update 1 out of 3: Individual house plots complete")

## all homes plots ####
PDFfile <- paste0(path,"/AllHomes_SummaryPlots_",Sys.Date(),".pdf")
pdf(file=PDFfile, paper = "USr", width = 28)
p4 <- ggplot(allhh, aes(y = PM2.5, x = reorder(HouseID,PM2.5, FUN = median))) +
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(limits = c(0,100)) +
  ggtitle("Range of indoor PM2.5 per home") +
  theme_bw()
print(p4)
timePlot(allhh, "PM2.5", type = "HouseID")
timePlot(allhh, "PM10", type = "HouseID")
timePlot(allhh, "Temperature", type = "HouseID")
timePlot(allhh, "CO2", type = "HouseID")

dev.off()

print("Plot update 2 out of 3: All house combined plots complete")

done.homes <- house.summaries.midday %>% filter(status == "Done")

# ### PDF path - range plots ####
PDFfile <- paste0(path,"/RangePlots2",Sys.Date(),".pdf")
pdf(file=PDFfile, paper = "USr", width = 28)

for(i in 1:nrow(done.homes)) {
  cur.house <- done.homes$HouseID[i]
  cur.period <- interval(done.homes$start.date[i],done.homes$end.date[i])
  
  all.hh.subset <- allhh %>%filter((DateOnly %within% cur.period) & (HouseID %in% done.homes$HouseID))
  
  ## calculate average PM per house in 6 hourly zones:
  range.summaries <- all.hh.subset %>% group_by(daytime, HouseID) %>%
    summarise(PM.mean = mean(PM2.5, na.rm = T)) %>%
    mutate(PM.min = min(PM.mean),
           PM.max = max(PM.mean))
  
  range.summaries$daytime <- factor(range.summaries$daytime, 
                                    levels=c("Night", "Morning", "Afternoon", "Evening"))

  
  p2 <- ggplot(range.summaries) +
    geom_point(aes(daytime, PM.mean,color = HouseID), size = 12,color = "firebrick2", fill = "firebrick2") +
    gghighlight(HouseID == cur.house, label_key = HouseID,use_direct_label = FALSE) +
    labs(title = paste0(cur.house," and all homes: between ", format(done.homes$start.date[i], format = "%d %b"), " - ",
                        format(done.homes$end.date[i], format = "%d %b"), " 2019"),
         x= "Time of the day", y = expression("Indoor PM"[2.5])) +
    scale_y_continuous(limits = c(0,45), breaks = seq(0,45,5))+
    theme_bw() +
    theme(plot.margin = margin(1,1,1,1,"cm"),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 14),
          legend.position = "NULL")
  print(p2)
  # print(i)
}

dev.off()    
print("Plot update 3 out of 3: Range Plots for feedback complete")
print ("Plots output completed, mapping now (please note this may take a while)..")


## create maps ####

hhmap <- done.homes

latlon_CRS <- "+proj=longlat +datum=WGS84"
NZTM_CRS <- "+init=epsg:2193"

coordinates(hhmap) <- ~NZTM_E + NZTM_N
proj4string(hhmap) <- CRS(NZTM_CRS)

pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))

# mapviewOptions(basemaps ="OpenStreetMap.DE") ## set a basemap for all mapview objects
m1 <- mapview(hhmap,zcol = "PM2.5", col.regions = pal(10), at = seq(5,15,2),cex = 16, alpha = 1)
m2 <- mapview(hhmap,zcol = "PM10", col.regions = pal(10), cex = 16, alpha = 1)
m3 <- mapview(hhmap,zcol = "Temperature", col.regions = pal(10), cex = 16, alpha = 1)
m4 <- mapview(hhmap,zcol = "CO2", col.regions = pal(10), cex = 16, alpha = 1)

# m5 <- sync(m1,m2,m3,m4)
# m5# m1+m2+m3+m4

mapshot(m1, file = paste0(path,"/",cur.phase,"_IndoorPM2.5_",Sys.Date(),".jpeg"))
mapshot(m2, file = paste0(path,"/",cur.phase,"_IndoorPM10_",Sys.Date(),".jpeg"))
mapshot(m3, file = paste0(path,"/",cur.phase,"_IndoorTemp_",Sys.Date(),".jpeg"))
mapshot(m4, file = paste0(path,"/",cur.phase,"_IndoorHumidity_",Sys.Date(),".jpeg"))



print(paste("Script run has completed for", cur.phase, "."))

print(paste("All Outputs can be found at", getwd()))
