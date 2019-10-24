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
      tidyr)

## read all SMOG data ###
path <- "Q:/AirQual/Research/CONA/Hobart/2018/Wood smoke study/woodsmoke study/2018/pm house date/"
setwd(path)
smog <- read.csv(paste0(path,"Combined_SMOG_data_2018_01_10.csv"), stringsAsFactors = F)
# corrections <- read.csv("smog_corrections2018.csv")

#### epa site: South Hobart#####
# epa1 <- read.csv("./epa/sHobart_2018.csv", stringsAsFactors = F)
# epa1$DateTime <- parse_date_time(epa1$DateTime, "dmY HM")

smog$VISIT_DATE_DD_MM_YYYY <-  parse_date_time(smog$VISIT_DATE_DD_MM_YYYY, "dmY")
smog$START_TIME1 <-  parse_date_time(smog$START_TIME1, "Ymd HMS")
smog$END_TIME1 <-  parse_date_time(smog$END_TIME1, "Ymd HMS")
smog$START_TIME2 <-  parse_date_time(smog$START_TIME2, "Ymd HMS")
smog$END_TIME2 <-  parse_date_time(smog$END_TIME2, "Ymd HMS")
smog$DATE_TIME_ORIGINAL <-  parse_date_time(smog$DATE_TIME_ORIGINAL, "Ymd HMS")
smog$DATE_TIME_FIXED <-  parse_date_time(smog$DATE_TIME_ORIGINAL, "Ymd HMS")

smog.short <- smog[,c(1,2,4,5,20,21,22)]
colnames(smog.short) <- c("houseID","visitID","in_out","UNIT_ID","datetime","variableID","value")

smog.in <- smog.short[which(smog.short$in_out == "IN"),]
smog.out <- smog.short[which(smog.short$in_out == "OUT"),]

## indoor ###
smog.wide.in <- reshape(smog.in, idvar = c("houseID","visitID","in_out", "UNIT_ID", "datetime"), 
                        timevar = "variableID", direction = "wide")

# smog.wide.in <- smog.wide.in[,-c(6,10:15)]
colnames(smog.wide.in)[c(6:10)] <- c("pm1","pm2.5","pm10","temperature","humidity")

smog.wide.in$datetime <- round_date(smog.wide.in$datetime, unit = "minute")
smog.wide.in$pm2.5corr.in <- ifelse(smog.wide.in$pm10< smog.wide.in$pm2.5, NA, smog.wide.in$pm2.5)

## outdoor ##
smog.wide.out <- reshape(smog.out, idvar = c("houseID","visitID","in_out","UNIT_ID", "datetime"), 
                         timevar = "variableID", direction = "wide")

# smog.wide.out <- smog.wide.out[,-c(6,10:15)]
colnames(smog.wide.out)[c(6:10)] <- c("pm1","pm2.5","pm10","temperature","humidity")

smog.wide.out$datetime <- round_date(smog.wide.out$datetime, unit = "minute")
smog.wide.out$pm2.5corr.out <- ifelse(smog.wide.out$pm10< smog.wide.out$pm2.5, NA, smog.wide.out$pm2.5)

### applying corrections ####
# smog.wide.in <- merge(smog.wide.in, corrections, by = "UNIT_ID", all.x = T)
# smog.wide.in$pm2.5corr.in <- ifelse(!is.na(smog.wide.in$PM25_EPA.slope),
#                                     ((smog.wide.in$PM25_EPA.slope*smog.wide.in$pm2.5) + smog.wide.in$PM25_EPA.intrcpt),
#                                     smog.wide.in$pm2.5)
# 
# smog.wide.out <- merge(smog.wide.out, corrections, by = "UNIT_ID", all.x = T)
# smog.wide.out$pm2.5corr.out <- ifelse(!is.na(smog.wide.out$PM25_EPA.slope),
#                                       ((smog.wide.out$PM25_EPA.slope*smog.wide.out$pm2.5) + smog.wide.out$PM25_EPA.intrcpt),
#                                       smog.wide.out$pm2.5)


smog.all <- merge(smog.wide.in,smog.wide.out, by = c("houseID","datetime"),
                  suffixes = c('.in','.out'), all = T)


smog.all <- smog.all[,-c(3,4,5,12,13,14)] ## for SMOGs
# smog.all <- smog.all[,-c(3,4,11,12)] ## for ODINs 
# smog.all$pm2.5corr.in<- ifelse(smog.all$pm2.5corr.in <0, NA, smog.all$pm2.5corr.in)
# smog.all$pm2.5corr.out <- ifelse(smog.all$pm2.5corr.out <0, NA, smog.all$pm2.5corr.out)
# smog.all$month <- month(smog.all$datetime)
colnames(smog.all)[2] <- "date"


write.csv(smog.all, "odin_all_AK.csv")



### adding visit info and converting data to 10 min #####

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
write.csv(smog.all.5min, "odin10min_AK.csv", row.names = F)

