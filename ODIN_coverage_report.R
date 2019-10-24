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
library(gstat)
library(ggmap)
library(gganimate)
library(gifski)
library(curl)
library(googledrive)

path <- "~/CONA/Arrowtown2019/ODINs/Raw/"
google_dir <- drive_get("ODIN Reports 2019")
setwd(path)


url <- "ftp:"
h = new_handle(dirlistonly=TRUE)
con = curl(url, "r", h) ### start connection 
tbl = read.table(con, stringsAsFactors=F, fill=TRUE)
close(con) ## end connection 

urls <- paste0(url, tbl[,1]) ## get all items on the ftp

fls = basename(urls) ## separate basename from filename ###
existing.files <- list.files(path ="~/CONA/Arrowtown2019/ODINs/Raw/")### list all files in directory


## autodownload from the web if there is a new file ###
tgz.imports <- NULL
counter = 1

for(i in 1:nrow(tbl)) {
  cur.url = urls[i]
  cur.file <- fls[i]
  download_tf <- c(grep("AVG", cur.url), grep(".png", cur.url), grep("idw", cur.url)) ## condition to check if to download or not
  
  ## if the file already exists OR if it is not the correct one, don't download ####
  if(length(download_tf)>0) { # | cur.file %in% existing.files) {
    print(cur.url)
   
  } else {
    untar(curl_download(url = cur.url,
                      destfile = paste0("~/CONA/Arrowtown2019/ODINs/Raw/",fls[i])))
    ## create a vector of all filesnames
    tgz.imports[counter] <- paste0("~/CONA/Arrowtown2019/ODINs/Raw/",fls[i])
    print(i)
    counter = counter +1
  }
 
}


## autounzip all files in the folder
## function to read and filter only required columns #### 

## function to read and filter only required columns #### 
readandfilter <- function(x) {
  cur.odin <- fread(paste0(path,x))
  cur.odin <- cur.odin[,c("PM1","PM2.5","PM10","PMc","GAS1","Tgas1",
                          "GAS2","Temperature","RH","date","serialn")]
  return(cur.odin)
}

## read and merge all the data 
listODINs <- list.files(path = path, pattern = ".txt")
ODINs.master <- rbindlist(lapply(listODINs, readandfilter))
## correct for datetime
ODINs.master$date <- ymd_hms(ODINs.master$date, tz = "Pacific/Auckland")
ODINs.master$date <- round_date(ODINs.master$date, "minute")

ODINs.master <- ODINs.master %>% filter(year(date)<2020) ## removing date-errors


##merge all locations ####
source("~/CONA/Arrowtown2019/ODINs/LocationTimeSeries_ODINs.R")

#####
allserialn <- unique(ODINs.master$serialn)
allODIN10min <- list()
i = 1
for(i in 1:length(allserialn)) {
  cur.odin <- ODINs.master %>% filter(serialn == allserialn[i])
  cur.odin.locs <- allLocations %>% filter(serialn == allserialn[i])
  cur.odin <- merge(cur.odin, cur.odin.locs, by = c("date", 'serialn'),all.x = T)
  cur.odin10min <- timeAverage(cur.odin, "10 min")
  cur.odin10min$date <- round_date(cur.odin10min$date, "10 minutes")
  cur.odin10min$dateonly <- as.Date(cur.odin10min$date)
  cur.odin10min$weekno <- isoweek(cur.odin10min$date)
  cur.odin10min$hour <- hour(cur.odin10min$date)
  cur.odin10min$serialn <- allserialn[i]
  allODIN10min[[i]] <- cur.odin10min
  print(i)
}

ODINs.master10min <- rbindlist(allODIN10min)


## calculate 24-hour midnight averages per ODIN ####

dailyaverages <- ODINs.master10min %>% group_by(dateonly, serialn, lat, lon) %>%
  filter(!is.na(PM2.5)) %>%
  summarise(PM2.5 = mean(PM2.5, na.rm = T),
            PM10 = mean(PM10, na.rm = T),
            Temperature = mean(Temperature, na.rm = T),
            count = n(),
            coverage = n()/144*100)
dailyaverages_honest <- dailyaverages %>% filter(coverage >=95)



write.csv(ODINs.master10min, 
          paste0("~/CONA/Arrowtown2019/ODINs/allODIN10min.csv"),
          row.names = F)
write.csv(dailyaverages_honest, "~/CONA/Arrowtown2019/ODINs/DailyAverages_Honest.csv", row.names = F)
write.csv(dailyaverages, "~/CONA/Arrowtown2019/ODINs/DailyAverages.csv", row.names = F)

### clear memory ####
do.call(file.remove, list(list.files(path, full.names = TRUE)))

# build daily dashboards ####
PDFfile <- paste0("~/CONA/Arrowtown2019/ODINs/SummaryDashboards",".pdf")
pdf(file=PDFfile, paper = "USr", width = 26, height = 30)

## number of odins live per day by hour of day ####
livesummary <- ODINs.master10min %>% group_by(dateonly,hour) %>%
  filter(!is.na(PM2.5)) %>%
  summarise(countODIN = length(unique(serialn)))
# livesummary$time <- format(livesummary$date, format = "%H:%M")

p5 <- ggplot(livesummary) +
  geom_tile(aes(dateonly,hour, fill = countODIN), color = "black") +
  scale_fill_continuous(low = "white", high = "darkred", breaks = seq(0,23,3))+
  scale_y_reverse(breaks = seq(0,24,1)) +
  ggtitle("Number of ODINs live per day by hour of the day") + 
  scale_x_date(date_breaks = "4 day", date_labels = "%d-%m") + 
  theme_bw() + 
  theme(plot.margin = margin(1,1,1,1,"cm"))
print(p5)

## per ODIN coverage report####
ODINcover.summary <- ODINs.master10min %>% group_by(dateonly, serialn) %>%
  filter(!is.na(PM2.5)) %>%
  summarise(countpoints = n())


p6 <- ggplot(ODINcover.summary) +
  geom_tile(aes(dateonly,serialn, fill = countpoints), color = "black") +
  ggtitle("per ODIN activity daily") + 
  scale_fill_continuous(low = "white", high = "darkred", 
                        breaks = seq(0,max(ODINcover.summary$countpoints),20))+
  scale_x_date(date_breaks = "4 day", date_labels = "%d-%m") + 
  theme_light()
print(p6)
### per day reports by hour ####

## per ODIN coverage report####
daily.cover <- ODINs.master10min %>% group_by(dateonly,hour,serialn,lat,lon) %>%
  filter(!is.na(PM2.5)) %>%
  summarise(countpoints = n(),
            PM2.5 = mean(PM2.5, na.rm = T))

all.dates <- sort(unique(daily.cover$dateonly),decreasing = TRUE)


mykey = ""
register_google(key = mykey)
centre_lat <- mean(odin.loc.info$lat,na.rm = TRUE)
centre_lon <- mean(odin.loc.info$lon,na.rm = TRUE)

map <- get_googlemap(center =c(centre_lon,centre_lat), 
                     zoom = 15, scale = 2, color = "bw", maptype = "roadmap")
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 80))
i = 1
alldaily <- list()
for(i in 1:length(all.dates)) {
  cur.date <- all.dates[i]
  cur.date.data <- ODINs.master10min %>% filter(dateonly == cur.date)
  coverage <- daily.cover %>% filter(dateonly == cur.date)
  
  cur.locations <- allLocations %>% 
    filter(as.Date(date) == cur.date) %>% 
    group_by(serialn) %>%
    summarise(lat = lat[1],
              lon = lon[1])
  
  coverage <- merge(cur.locations, coverage, by = c("serialn", "lat","lon"), all.x = T)
  
  cover.map <- coverage %>% group_by(lat,lon,serialn) %>%
    summarise(totalpoints = sum(countpoints, na.rm = T),
              PM2.5 = mean(PM2.5, na.rm = T)) %>%
    mutate(PM2.5corr = ifelse(totalpoints %in% c(137:145),"more than 95% coverage", 
                              ifelse(totalpoints == 0, "No coverage", "Partial Coverage")))
  cover.map$date <- cur.date
  
  alldaily[[i]] <- cover.map
  ### time series ####
  
  p1 <- ggplot(cur.date.data) +
    geom_line(aes(date, PM2.5, color = serialn)) + labs(x = "time of the day", title = "PM2.5 timeseries")+
    scale_x_datetime(date_breaks = "4 hours", date_labels = "%H:%M") +
    guides(color=guide_legend(ncol=3)) +
    theme_bw()

  #### coverage by hour of the day ####
  p2 <- ggplot(coverage) +
    geom_tile(aes(reorder(serialn, countpoints, fun = min), hour, fill = countpoints), color = "black") +
    ggtitle("Coverage per ODIN by hour") +
    scale_fill_continuous(low = "white", high = "darkred", limits = c(0,6))+
    scale_y_reverse(breaks = seq(0,24,2), limits = c(23,0)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 15, size = 6))

  ## Coverage map for the day per ODIN ####
  p3 <- ggmap(map) +
    geom_point(data = (cover.map%>% filter(!is.na(PM2.5))),
               aes(lon, lat, color = totalpoints),size = 6) +
    geom_point(data = (cover.map %>% filter(PM2.5corr == "No coverage")),
               color = "black", size =6) +
    scale_colour_gradientn(colours = rev(myPalette(100)), limits=c(0, 150)) +
    ggtitle("Coverage Map [black dots = no coverage]") +
    # geom_text(data = cover.map,aes(lon, lat, label = serialn), nudge_y = -0.0005, size = 2) +
    theme_bw()

  ### PM2.5 map based on coverage ####
  p4 <- ggmap(map)   +
    geom_point(data = (cover.map%>% filter(!is.na(PM2.5))),
               aes(lon, lat, color = PM2.5),size = 6) +
    geom_point(data = (cover.map %>% filter(PM2.5corr == "No coverage")),
               aes(lon, lat), color = "black", size =6, alpha = 0.1) +
    sc +
    ggtitle("PM2.5 map") +
    # geom_text(data = cover.map,aes(lon, lat, label = serialn), nudge_y = -0.0005, size = 2) +
    theme_bw()


  lay1 <- rbind(c(1,1,1, NA),
                c(1,1,1, NA),
                c(2,2,3,3),
                c(2,2,3,3),
                c(2,2,3,3),
                c(4,4,4,4),
                c(4,4,4,4))

  grid.arrange(p1,p3,p4, p2,
               layout_matrix =lay1,
               bottom = textGrob(paste("ODIN Report for",
                                       format(cur.date, "%d-%b-%y NZST")),
                                 gp=gpar(fontsize=16)))
  
}
dev.off()
drive_rm("SummaryDashboards.pdf")
drive_rm("DailyAverages.csv")
drive_rm("DailyAverages_Honest.csv")
drive_rm("allODIN10min.csv")
drive_rm("DailyAverages.gif")
drive_upload(media = "~/CONA/Arrowtown2019/ODINs/SummaryDashboards.pdf",
             path = google_dir)

drive_upload(media = "~/CONA/Arrowtown2019/ODINs/DailyAverages.csv",
             path = google_dir)


drive_upload(media = "~/CONA/Arrowtown2019/ODINs/DailyAverages_Honest.csv",
             path = google_dir)


drive_upload(media = "~/CONA/Arrowtown2019/ODINs/allODIN10min.csv",
             path = google_dir)

alldailydf <- rbindlist(alldaily)

p7 <- ggmap(map)  + transition_states(states = date)  +
  labs(title = paste("Date:", "{closest_state}")) +
  geom_point(data = (alldailydf %>% filter(!is.na(PM2.5))),
             aes(lon, lat, color = PM2.5),size = 6) +
  geom_point(data = (alldailydf %>% filter(PM2.5corr == "No coverage")),
             aes(lon, lat), color = "black", size =6, alpha = 0.1) +
  sc +
  # geom_text(data = alldailydf,aes(lon, lat, label = serialn), nudge_y = -0.0005, size = 2) +
  theme_bw()

anim_save("~/CONA/Arrowtown2019/ODINs/DailyAverages.gif", p7)


drive_upload(media = "~/CONA/Arrowtown2019/ODINs/DailyAverages.gif",
             path = google_dir)


##### weekly datasets and maps #####

weekly.df <- ODINs.master10min %>% 
  group_by(weekno, serialn, lat, lon) %>% filter(!is.na(PM2.5)) %>%
  summarise(PM2.5 = mean(PM2.5, na.rm = T),
            PM10 = mean(PM10, na.rm = T),
            count = n(),
            countdays = length(unique(dateonly)),
            Start = min(dateonly),
            End = max(dateonly))

allweeks <- unique(weekly.df$weekno)
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0,80))

allweeks.list <- list()


latestweek <- allweeks[length(allweeks)-1]
today <- weekdays(Sys.Date())

perform_weeklyoperations <- FALSE
if(today == "Monday") perform_weeklyoperations <- TRUE

for(i in 1:length(allweeks)) {
  curweek <- allweeks[i]
  cur.week.data <- weekly.df %>% filter(weekno == curweek)
  
  start = format(min(cur.week.data$Start), "%d %b")
  end = format(max(cur.week.data$End), "%d %b")
  ### PM2.5 map based on coverage ####
  p8 <- ggmap(map)   +
    geom_point(data = (cur.week.data%>% filter(count >(0.75*144*countdays))),
               aes(lon, lat, color = PM2.5),size = 6) +
    geom_point(data = (cur.week.data%>% filter(count <(0.75*144*countdays))),
               aes(lon, lat), color = "black", size =6, alpha = 0.1) +
    sc +
    ggtitle(paste( start, "to", end, "2019")) +
    
    theme_bw()
  
  if(perform_weeklyoperations) {
    ggsave(paste0("~/CONA/Arrowtown2019/ODINs/MondayOutputs/WeeklyMap",
                  start,"to",end, ".jpeg"),
           plot = p8, device = "jpeg")
    write.xlsx2(cur.week.data, 
                paste0("~/CONA/Arrowtown2019/ODINs/MondayOutputs/ODINweekly_",start,"to",end,".xlsx"))
    drive_upload(media = paste0("~/CONA/Arrowtown2019/ODINs/MondayOutputs/WeeklyMap",
                                start,"to",end, ".jpeg"),
                 path = google_dir)
    }
    

  print(i)
}



