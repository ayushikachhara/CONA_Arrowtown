####### HOBART iButton Analysis script #####

library(librarian)

shelf(openair,
      data.table,
      dplyr,
      lubridate,
      tidyr,
      zoo,
      ggplot2,
      plotly,
      leaflet,
      shiny,
      mapview,
      maptools,
      rgdal,
      rgeos,
      sp,
      raster,
      RColorBrewer,
      pracma,
      sfsmisc,
      ggpmisc,
      lib = tempdir())

######### define path and read data ####
path <- "Q:/AirQual/Research/CONA/Hobart/2018/Wood smoke study/woodsmoke study/2018/pm house date/"
setwd(path)

odin10min <- read.csv("ODIN10min_AK.csv", stringsAsFactors = F)
odin10min$date <- dmy_hm(odin10min$date)
odin10min$time <- as.POSIXct(format(odin10min$date, format = "%H:%M"), format = "%H:%M")
odin10min$dateonly <- as.Date(odin10min$date)

smog10min <- read.csv("smog10min_AK.csv", stringsAsFactors = F)
smog10min$date <- dmy_hm(smog10min$date)
smog10min$time <- as.POSIXct(format(smog10min$date, format = "%H:%M"), format = "%H:%M")
smog10min$dateonly <- as.Date(smog10min$date)

smogodin <- rbind.data.frame(smog10min,odin10min)
smogodin$HouseID <- as.character(smogodin$HouseID)

### import startup times ####
startups <-read.csv("Q:/AirQual/Research/CONA/Hobart/2018/Wood smoke study/woodsmoke study/2018/pm house date/iButtons/startupresults.csv",
                    stringsAsFactors = F)

startups$HouseID <- as.character(startups$HouseID)
startups$date <- dmy_hm(startups$startup_datetime)
startups$startup_datetime <- dmy_hm(startups$startup_datetime)

wbhouses <- unique(startups$HouseID)
smogodin <- smogodin[which(smogodin$HouseID %in% wbhouses),]
all.data <- merge(smogodin, startups, by = c("HouseID","Visitno","InterventionType","date"), all = T)

all.data$identifier <- paste("HouseID: ",all.data$HouseID,"Intervention: ", all.data$InterventionType)
list1 <- split(all.data, all.data$identifier)
i =3

#### plotting ######
PDFPath = paste0("StartUpsandPM_23052019.pdf")
pdf(file=PDFPath, paper = "USr", width = 17)
for(i in 1:length(list1)) {
  tryCatch({
    h8 <- list1[[i]]
    h8 <- h8[complete.cases(h8$pm2.5.in),]
    
    p1 <- ggplot(h8) +
      geom_line(aes(date, pm2.5.in, color = "Indoor PM")) +
      geom_point(aes(startup_datetime, pm2.5.in, color = "WB start identified"),size = 3) + 
      ggtitle(h8$identifier) + 
      theme_bw()
    
    print(p1)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
            
}
dev.off()



write.csv(all.data,"PMandStartUps.csv")
