#### indoor source identification algorithm ####
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

#### functions #####
## peak detection function #
peakdet <- function(v, delta, x = NULL) {
  maxtab <- NULL
  mintab <- NULL
  
  ## if x = NULL, assign a sequence - position indices
  if (is.null(x))
  {
    x <- seq_along(v)
  }
  
  ## checking vector lengths ###
  if (length(v) != length(x))
  {
    stop("Input vectors v and x must have the same length")
  }
  
  if (!is.numeric(delta))
  {
    stop("Input argument delta must be numeric")
  }
  
  if (delta <= 0)
  {
    stop("Input argument delta must be positive")
  }
  
  mn <- Inf
  mx <- -Inf
  
  mnpos <- NA
  mxpos <- NA
  
  lookformax <- TRUE
  
  for(i in seq_along(v))
  {
    this <- v[i]
    
    if (this > mx & !is.na(this))
    {
      mx <- this
      mxpos <- x[i]
    }
    
    if (this < mn & !is.na(this))
    {
      mn <- this
      mnpos <- x[i]
    }
    
    if (lookformax & !is.na(this))
    {
      if ((this < mx - delta) & (!is.na(this)))
      {
        maxtab <- rbind(maxtab, data.frame(pos = mxpos, val = mx))
        
        mn <- this
        mnpos <- x[i]
        
        lookformax <- FALSE
      }
    }
    else
    {
      if ((this > mn + delta) & (!is.na(this)))
      {
        mintab <- rbind(mintab, data.frame(pos = mnpos, val = mn))
        
        mx <- this
        mxpos <- x[i]
        
        lookformax <- TRUE
      }
    }
  }
  
  list(maxtab = maxtab, mintab = mintab)
}

### import data ####
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

## pm2.5 apply corrections ###
smogodin$pm2.5corr.in <- ifelse(smogodin$pm2.5.in <= smogodin$pm10.in, smogodin$pm2.5.in, NA)
smogodin$pm2.5corr.out <- ifelse(smogodin$pm2.5.in <= smogodin$pm10.in, smogodin$pm2.5.out, NA)

unique(smogodin$HouseID)
df <- smogodin[which(smogodin$HouseID == 6),]
df <- df[,c("HouseID","date","pm2.5corr.in", "InterventionType", "Visitno")]
h7out <- smogodin[which(smogodin$HouseID == 7),]
h7out <- h7out[,c(2,14)]

h6 <- merge(df,h7out, by = "date", all.x = T)
h6$pm2.5corr.out.lead <- lead(h6$pm2.5corr.out, 60)

p0 <- ggplot(h6) +
  geom_line(aes(date, pm2.5corr.in))+
  geom_line(aes(date, pm2.5corr.out), color = "blue") 
ggplotly(p0)


## select a home ###
df <- smogodin[which(smogodin$HouseID == 7),]
p0 <- ggplot(df) +
    geom_line(aes(date, pm2.5corr.in)) +
    geom_line(aes(date, pm2.5corr.out), color = "blue")
p0# ggplotly(p0)


## split into Baseline and HEPA ###
baseline <- df[which(df$InterventionType == "Baseline"),]
hepa <- df[which(df$InterventionType == "HEPA"),]

## apply local max and min detection algo with a threshold setting of 10

peaks.baseline <- peakdet(baseline$pm2.5corr.in,10, baseline$date)
peaks.dfmax <- peaks.baseline$maxtab
peaks.dfmax$identifier <- 1:nrow(peaks.dfmax)

peaks.dfmin <- peaks.baseline$mintab
peaks.dfmin$identifier <- 1:nrow(peaks.dfmin)

colnames(peaks.dfmax) = c("date", "value", "identifier")
colnames(peaks.dfmin) = c("date", "value", "identifier")

maxdate <- which.max(baseline$date)
lastrow <- cbind.data.frame(date = baseline$date[maxdate], 
                            value = baseline$pm2.5corr.in[maxdate],
                            identifier = max(peaks.dfmax$identifier))
peaks.dfmin <- rbind.data.frame(peaks.dfmin, lastrow)

peaks.df <- rbind.data.frame(peaks.dfmax, peaks.dfmin)

baseline <- merge(baseline,peaks.df, by = "date", all = T)
baseline <- baseline[,c(1,2,8,14:16,21:22)]

### adding the identifier across to each peakgroup
peakgroups <- unique(peaks.df$identifier)
baseline$peakfilter <- NA

i =1
for(i in 1:length(peakgroups)) {
  cur.peak <- peaks.df[which(peaks.df$identifier == peakgroups[i]),]
  
  ## making a peak group
  baseline$identifier <- ifelse((baseline$date >= cur.peak$date[1] & 
                                   baseline$date<=cur.peak$date[2]), 
                                peakgroups[i], baseline$identifier)
  
  ## apply peak filter
  baseline$peakfilter <- ifelse(baseline$date == cur.peak$date[1], TRUE, baseline$peakfilter)
  baseline$peakfilter <- ifelse((baseline$date == cur.peak$date[1]&
                                   baseline$value > baseline$pm2.5corr.out),
                                TRUE,baseline$peakfilter)
}

## adding switches to peakgroups: TRUE signifies Indoor source in both cases ####
peakdates <- baseline$date[which(!is.na(baseline$peakfilter))] - 600 ### lag by 1 datapoints


allpeakdates <- cbind.data.frame(peakdate = peakdates,
                                 thresh.pm = baseline$pm2.5corr.in[which(baseline$date %in% peakdates)],
                                 identifier = baseline$identifier[which(!is.na(baseline$peakfilter))])

baseline <- merge(baseline, allpeakdates, by = "identifier", all = T)

baseline$indmorethanout <- ifelse((baseline$pm2.5corr.in >= baseline$pm2.5corr.out), 
                                 TRUE,FALSE)

baseline$concmorethanth <- ifelse((baseline$pm2.5corr.in < 1 | baseline$pm2.5corr.in <=baseline$thresh.pm), 
                              FALSE, TRUE)

baseline$pm2.5.pcT <- ifelse((!is.na(baseline$thresh.pm) &
                                 baseline$concmorethanth == TRUE), NA, baseline$pm2.5corr.in)

baseline$pm2.5.icT <- ifelse((baseline$indmorethanout == TRUE),
                             NA, baseline$pm2.5.pcT)


ggplotly(ggplot(baseline) +
  geom_point(aes(date, value)) +
  geom_line(aes(date, pm2.5corr.in, color = "Indoor")) +
    geom_line(aes(date, pm2.5corr.out, color = "Outdoor")) +
    geom_line(aes(date,pm2.5.pcT, color = "Peak to threshold filter")) + 
    geom_line(aes(date, pm2.5.icT, color = "Peak to threshold  + Outdoor filter"))  +
    ggtitle(baseline$HouseID[1]) +
    theme_bw())


baseline <- baseline[order(baseline$date),]

#### RUN NUMBER TWO ####
base2 <- baseline[,c(2:7,14)]
peaks.2 <- peakdet(base2$pm2.5.pcT,10, base2$date)
peaks.dfmax <- peaks.2$maxtab
peaks.dfmax$identifier <- 1:nrow(peaks.dfmax)

peaks.dfmin <- peaks.2$mintab
peaks.dfmin$identifier <- 1:nrow(peaks.dfmin)

colnames(peaks.dfmax) = c("date", "value", "identifier")
colnames(peaks.dfmin) = c("date", "value", "identifier")

maxdate <- which.max(base2$date)
lastrow <- cbind.data.frame(date = base2$date[maxdate], 
                            value = base2$pm2.5.pcT[maxdate],
                            identifier = max(peaks.dfmax$identifier))
peaks.dfmin <- rbind.data.frame(peaks.dfmin, lastrow)

peaks.df <- rbind.data.frame(peaks.dfmax, peaks.dfmin)

base2 <- merge(base2,peaks.df, by = "date", all = T)
# base2 <- base2[,c(1,2,8,14:16,21:22)]

### adding the identifier across to each peakgroup
peakgroups <- unique(peaks.df$identifier)
base2$peakfilter <- NA

i =1
for(i in 1:length(peakgroups)) {
  cur.peak <- peaks.df[which(peaks.df$identifier == peakgroups[i]),]
  
  ## making a peak group
  base2$identifier <- ifelse((base2$date >= cur.peak$date[1] & 
                                   base2$date<=cur.peak$date[2]), 
                                peakgroups[i], base2$identifier)
  
  ## apply peak filter
  base2$peakfilter <- ifelse((base2$date == cur.peak$date[1]&
                                   base2$value > base2$pm2.5corr.out), 
                                TRUE,base2$peakfilter)
}

## adding switches to peakgroups: TRUE signifies Indoor source in both cases ####
peakdates <- base2$date[which(!is.na(base2$peakfilter))] - 600 ### lag by 3 datapoints
# peakdates2 <- peakdates-600
# peakdates3 <- peakdates-1200

allpeakdates <- cbind.data.frame(peakdate = peakdates, 
                                 thresh.pm = base2$pm2.5.pcT[which(base2$date %in% peakdates)],
                                 identifier = base2$identifier[which(!is.na(base2$peakfilter))])
base2 <- merge(base2, allpeakdates, by = "identifier", all = T)

base2$indmorethanout <- ifelse((base2$pm2.5.pcT > base2$pm2.5corr.out), 
                                  TRUE,FALSE)

base2$concmorethanth <- ifelse((base2$pm2.5.pcT < 1 | base2$pm2.5.pcT <=base2$thresh.pm), 
                                  FALSE, TRUE)


# base2$pm2.5.picT <- ifelse((!is.na(base2$peakfilter) |
#                                 base2$indmorethanout == TRUE |
#                                 base2$concmorethanth == TRUE), NA, 
#                               base2$pm2.5.pcT)

base2$pm2.5.pcT2 <- ifelse((!is.na(base2$peakfilter) |
                                base2$concmorethanth == TRUE), NA, base2$pm2.5.pcT)

base2$pm2.5.icT2 <- ifelse((base2$concmorethanth == TRUE | base2$indmorethanout == TRUE), 
                             NA, base2$pm2.5.pcT)


ggplotly(ggplot(base2) +
           geom_point(aes(date, value)) +
           geom_line(aes(date,pm2.5corr.in, color = "Indoor")) +
           geom_line(aes(date, pm2.5.pcT, color = "Peak reaching threshold")) +
           geom_line(aes(date, pm2.5.pcT2, color = "peak reaching threshold or conc greater than outdoor"))  +
           theme_bw())

