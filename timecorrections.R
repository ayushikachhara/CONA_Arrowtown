## time corrections ####
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
      tidyverse,
      lib = tempdir())


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

smogodin_fixed <- timeAverage(smogodin, avg.time = "10 min", type = c("HouseID","InterventionType"))
smogodin_fixed$time <- as.POSIXct(format(smogodin_fixed$date, format = "%H:%M"), format = "%H:%M")
smogodin_fixed$dateonly <- as.Date(smogodin_fixed$date)

h7 <- smogodin[which(smogodin$HouseID == "25"),]
h7$month <- month(h7$date)
plot(h7$pm2.5corr.out~h7$date, type = "l", col = "blue", ylim = c(0,50))


### import south hobart data ####
sh <- read.csv("Q:/AirQual/Research/CONA/Hobart/2018/Wood smoke study/woodsmoke study/2018/pm house date/epa/sHobart_2018.csv",
               stringsAsFactors = F)

sh$DateTime <- dmy_hm(sh$DateTime)
colnames(sh)[1] <- "date"

sh10min <- timeAverage(sh, avg.time = "10 min")

plot(h7$pm2.5corr.out~h7$date, type = "l")
lines(sh10min$PM2.5.EPA.ug.m3~sh10min$date, col = "red")


df1 <- merge(h7,sh10min, by = 'date', all.x = T)


df1$pmlead <- lead(df1$pm2.5corr.out, 60)
p0 <- ggplot(df1) +
  geom_line(aes(date,pm2.5corr.out)) +
  geom_line(aes(date, PM2.5.EPA.ug.m3), color = "blue")

ggplotly(p0)




### house 25 ####

nt <- read.csv("Q:/AirQual/Research/CONA/Hobart/2018/Wood smoke study/woodsmoke study/2018/pm house date/epa/Hobart.csv",
               stringsAsFactors = F)
nt$Date  <- dmy_hm(nt$Date)
colnames(nt)[4] <- "date"

nt10min <- timeAverage(nt, avg.time = "10 min")

plot(h7$pm2.5corr.out~h7$date, type = "l")
lines(nt10min$PM25~nt10min$date, col = "red")

df2 <- merge(h7,nt10min, by = 'date', all.x = T)


df2$pmlead <- lead(df2$pm2.5corr.out, 60)
p1 <- ggplot(df2) +
  geom_line(aes(date,pm2.5corr.out)) +
  geom_line(aes(date, PM25), color = "blue")

ggplotly(p1)
