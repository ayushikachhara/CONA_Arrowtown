### hobart merge data ####
library(librarian)

shelf(data.table,
      ggplot2,
      zoo,
      dplyr,
      gridExtra,
      grid,
      plotly,
      RColorBrewer,
      statsr,
      dplyr,
      pracma,
      openair,
      lubridate,
      tidyr,
      tidyverse)




## read all SMOG data ###
path <- "Q:/AirQual/Research/CONA/Hobart/2018/Wood smoke study/woodsmoke study/2018/pm house date/"
setwd(path)
smog.all <- read.csv("smog_all_AK.csv", stringsAsFactors = F)
smog.all <- as.data.table(smog.all)
smog.all$date <- ymd_hms(smog.all$date)
smog.all$date <- round_date(smog.all$date, "1 minute")
smog.all <- smog.all[,-c(1,4,12,20,21)]

visits <- read.csv("VisitandInterventionType.csv", stringsAsFactors = F)
visits <- as.data.table(visits)
visits$StartDate <- dmy(visits$StartDate) + days(1)
visits$EndDate <- dmy(visits$EndDate) - days(1)


all.data <- inner_join(smog.all, visits, by = "houseID")

all.data$inInterval <- ifelse(all.data$date >= all.data$StartDate & 
                              all.data$date <=all.data$EndDate, 1,0)

all.data <- all.data[which(all.data$inInterval ==1),]

all.data$HouseID <- as.character(all.data$houseID)

smog.all.5min <- timeAverage(all.data, avg.time = "10 min", 
                             type = c("HouseID", "InterventionType"))

# smog.all.5min2 <- merge(smog.all.5min, visits, c("houseID", "Visitno"), all.x = T)
write.csv(smog.all.5min, "smog10min_AK.csv", row.names = F)


smog.list <- split(smog.all.5min, smog.all.5min$houseID)
i = 9

# PDFfile <- paste0(path,"SMOG2018_indoorpeakdetection_HEPA.pdf")
# pdf(file=PDFfile, paper = "USr", width = 28)
for( i in 1:length(smog.list)) {
  smog <- smog.list[[i]]
  smog <- smog[which(smog$Visitno == 2),]
  peaks <- peakdet(smog$pm2.5corr.in, 30, smog$date)
  if(!is.null(peaks$maxtab)) {
    plot(smog$pm2.5corr.in~smog$date, type = "l",
         main = paste("ID: House", unique(smog$houseID), "(HEPA weeks)"))
    abline(h = 30)
    points(peaks$maxtab, col = "red")
    # points(peaks$mintab, col = "blue")
    print(i)
  } else {
    print("No indoor peaks detected")
  }
  
}

# dev.off()




