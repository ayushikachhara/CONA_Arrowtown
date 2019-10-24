## hh- odin visualisations ####
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
library(ggforce)

## setwd ####

# setwd("S:/kachharaa/CONA/Arrowtown2019/hauhau/phase3")
# hh.odin <- read.csv("hh-odin-P3.csv", stringsAsFactors = F)

# setwd("S:/kachharaa/CONA/Arrowtown2019/hauhau/phase2/")
# hh.odin <- read.csv("hh-odin-P2_20190906.csv", stringsAsFactors = F)

# setwd("S:/kachharaa/CONA/Arrowtown2019/hauhau/phase1")
# hh.odin <- read.csv("hh-odin-P1.csv", stringsAsFactors = F)

# setwd("S:/kachharaa/CONA/Arrowtown2019/hauhau/phase4/")
# hh.odin <- read.csv("hh-odin-P4_20190923.csv", stringsAsFactors = F)


# colnames(hh.odin)
hh.odin$date <- ymd_hms(hh.odin$date)
hh.odin$hour <- hour(hh.odin$date)

allhouseids <- unique(hh.odin$HouseID)
allhouseids <- allhouseids[which(!is.na(allhouseids))] ## removing NAs

i = 1

## creating csv file outputs for school students ####
for(i in 1:length(allhouseids)) {
  tryCatch({ 
    cur.hh <- hh.odin %>% filter(HouseID == allhouseids[i])
    cur.hh$com.odin.PM2.5 <- rowMeans(cur.hh[,c("p.odin.PM2.5","s.odin.PM2.5",
                                                "t.odin.PM2.5","q.odin.PM2.5")], na.rm = T)
    cur.hh$com.odin.PM10 <- rowMeans(cur.hh[,c("p.odin.PM10","s.odin.PM10",
                                               "t.odin.PM10","q.odin.PM10")], na.rm = T)
    cur.hh$com.odin.Temperature <- rowMeans(cur.hh[,c("p.odin.Temperature","s.odin.Temperature",
                                                      "t.odin.Temperature","q.odin.Temperature")], na.rm = T)
    
    final.hh <- cur.hh %>% dplyr::select(date,HouseID,NZTM_E,NZTM_N, tempCorr, Humidity, 
                                         CO2, PM2.5, PM10, com.odin.Temperature, com.odin.PM2.5,
                                         com.odin.PM10)
    colnames(final.hh ) <- c('date','HouseID','NZTM_E','NZTM_N', 'Indoor.Temperature', 'Indoor.Humidity', 
                             'Indoor.CO2', 'Indoor.PM2.5','Indoor.PM10', 'Outdoor.Temperature',
                             ' Outdoor.PM2.5', 'Outdoor.PM10')
    write.csv(final.hh,
              paste0('S:/kachharaa/CONA/Arrowtown2019/hauhau/individual house summaries/',
                     allhouseids[i],"_P04_datafile.csv"), row.names = F)
    
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  print(i)
  
}


# i = 15
#### raw feedback charts version ####
for(i in 1:length(allhouseids)) {
  tryCatch({
    cur.hh <- hh.odin %>% filter(HouseID == allhouseids[i])
    cur.hh$com.odin.PM2.5 <- rowMeans(cur.hh[,c("p.odin.PM2.5","s.odin.PM2.5",
                                                "t.odin.PM2.5","q.odin.PM2.5")], na.rm = T)
    cur.hh$com.odin.PM10 <- rowMeans(cur.hh[,c("p.odin.PM10","s.odin.PM10",
                                               "t.odin.PM10","q.odin.PM10")], na.rm = T)
    cur.hh$com.odin.Temperature <- rowMeans(cur.hh[,c("p.odin.Temperature","s.odin.Temperature",
                                                      "t.odin.Temperature","q.odin.Temperature")], na.rm = T)
    
    cur.hh$hourlypm2.5.out <- rollapply(cur.hh$com.odin.PM10, 6, align = "right", 
                                        FUN = function(x)mean(x,na.rm = T), na.pad = T)
    
    cur.hh$hourlypm10.out <- rollapply(cur.hh$com.odin.PM2.5, 6, align = "right", 
                                        FUN = function(x)mean(x,na.rm = T), na.pad = T)
    
    cur.hh$hourlytemperature.out <- rollapply(cur.hh$com.odin.Temperature, 6, align = "right", 
                                        FUN = function(x)mean(x,na.rm = T), na.pad = T)
    
    start.date <- min(cur.hh$date[which(!is.na(cur.hh$PM2.5))])
    end.date <- max(cur.hh$date[which(!is.na(cur.hh$PM2.5))])
    cur.hh$week <- isoweek(cur.hh$date)
    cur.hh$weekno <- cur.hh$week - (min(cur.hh$week)) +1
    cur.hh$hour <- hour(cur.hh$date)
    cur.hh$Light <- ifelse(cur.hh$hour %in% c(8:16), "Daytime", "Nighttime")
    cur.hh$Dateonly <- as.Date(cur.hh$date)
    cur.hh <- cur.hh %>% filter(date >= as.POSIXct("2019-06-01 12:00:00",
                                                   format = "%Y-%m-%d %H:%M:%S"))
    cur.hh <- cur.hh %>% group_by(weekno) %>%
      mutate(recordsperweek = n())
    
    cur.hh <- cur.hh %>% filter(recordsperweek>=288)
    
    ## rectangle for daytime/nighttime indicators ###
    rectangle <- cur.hh %>% group_by(Dateonly,Light) %>%
      summarise(start = min(date),
                end = max(date),
                weekno = unique(weekno))
    rectangle <- rectangle %>% filter(Light == "Daytime")
    
    
    # # ### PDF path - per home ####
    PDFfile <- paste0("S:/kachharaa/CONA/Arrowtown2019/hauhau/individual house summaries/NIWA Air Quality_Your Data Charts_",
                      allhouseids[i],"_","_P04_20190923.pdf")
    pdf(file=PDFfile, paper = "special", width = 8.2, height = 11.6)
    
    
    p1 <- ggplot(cur.hh) +
      geom_rect(data = rectangle,
                aes(xmin = start, xmax = end,
                    min = -Inf, ymax = Inf, fill = Light), alpha = 1) +
      geom_line(aes(date,hourlypm2.5.out, color = "Outdoor"), size = 1, alpha = 1) + 
      geom_line(aes(date, PM2.5, color = "Indoor"), size =1.5) + 
      scale_color_manual(values = c("firebrick","turquoise")) +
      scale_fill_manual(values = "white") +
      guides(fill =F) +
      scale_x_datetime(date_labels = "%d-%b %H:%M", name = "Date and Time")+
      scale_y_continuous(limits = c(0,200), name = "PM2.5") +
      theme_grey() +
      facet_wrap_paginate(.~weekno,  scales = "free_x", nrow = length(unique(cur.hh$weekno)),page = 2) +
      ggtitle(paste("PM2.5")) + 
      theme(legend.position = "top",
            legend.title = element_blank(),
            legend.key.width = unit(1.5, "cm"),
            legend.spacing = unit(1, "cm"),
            title = element_text(size = 14),
            text = element_text(size = 14),
            plot.margin = margin(1,1,1,1, "cm"),
            plot.title = element_text(hjust = 0.5),
            legend.box = "Horizontal",
            line = element_blank())
    print(p1)
    
    
    p2 <-  ggplot(cur.hh) +
      geom_rect(data = rectangle,
                aes(xmin = start, xmax = end,
                    min = -Inf, ymax = Inf, fill = Light), alpha = 1) +
      # geom_line(aes(date,hourlypm10.out, color = "Outdoor"), size = 1, alpha = 1) + 
      geom_line(aes(date, CO2, color = "Indoor"), size =1.5) + 
      scale_color_manual(values = c("firebrick")) +
      scale_fill_manual(values = "white") +
      guides(fill =F) +
      scale_x_datetime(date_labels = "%d-%b %H:%M", name = "Date and Time")+
      scale_y_continuous(name = "CO2") +
      theme_grey() +
      facet_wrap(.~weekno,  scales = "free_x", nrow = length(unique(cur.hh$weekno))) +
      ggtitle(paste("CO2")) + 
      theme(legend.position = "top",
            legend.title = element_blank(),
            legend.key.width = unit(1.5, "cm"),
            legend.spacing = unit(1, "cm"),
            title = element_text(size = 14),
            text = element_text(size = 14),
            plot.margin = margin(1,1,1,1, "cm"),
            plot.title = element_text(hjust = 0.5),
            legend.box = "Horizontal",
            line = element_blank())
    print(p2)
    
    p3 <-  ggplot(cur.hh) +
      geom_rect(data = rectangle,
                aes(xmin = start, xmax = end,
                    min = -Inf, ymax = Inf, fill = Light), alpha = 1) +
      geom_line(aes(date,hourlytemperature.out, color = "Outdoor"), size = 1, alpha = 1) + 
      geom_line(aes(date,tempCorr, color = "Indoor"), size =1.5) + 
      scale_color_manual(values = c("firebrick","turquoise")) +
      scale_fill_manual(values = "white") +
      guides(fill =F) +
      scale_x_datetime(date_labels = "%d-%b %H:%M", name = "Date and Time")+
      scale_y_continuous(limits = c(-5,35), name = "Temperature") +
      theme_grey() +
      facet_wrap(~weekno,  scales = "free_x", nrow = length(unique(cur.hh$weekno)),dir = "v") +
      ggtitle(paste("Temperature")) + 
      theme(legend.position = "top",
            legend.title = element_blank(),
            legend.key.width = unit(1.5, "cm"),
            legend.spacing = unit(1, "cm"),
            title = element_text(size = 14),
            text = element_text(size = 14),
            plot.margin = margin(1,1,1,1, "cm"),
            plot.title = element_text(hjust = 0.5),
            legend.box = "Horizontal",
            line = element_blank())
    print(p3)
    dev.off()
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  print(i)
  
}

