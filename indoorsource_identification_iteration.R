## select a home ###
df <- smogodin[which(smogodin$HouseID == 1),]
p0 <- ggplot(df) +
  geom_line(aes(date, pm2.5corr.in))
p0# ggplotly(p0)


## split into Baseline and HEPA ###
baseline <- df[which(df$InterventionType == "Baseline"),]
baseline$pm2.5corr.in.original <- baseline$pm2.5corr.in
baseline$pm2.5corr.in <- na.locf(baseline$pm2.5corr.in, na.rm = F,fromLast = TRUE) # replacing NAs with previous values

## after filling for NAs if there are any NAs, remove them:
baseline <- baseline[complete.cases(baseline$pm2.5corr.in),]
# hepa <- df[which(df$InterventionType == "HEPA"),]

## apply local max and min detection algo with a threshold setting of 10

peaks.baseline <- peakdet(baseline$pm2.5corr.in,5, baseline$date)
peaks.dfmax <- peaks.baseline$maxtab
peaks.dfmax$identifier <- 1:nrow(peaks.dfmax)

peaks.dfmin <- peaks.baseline$mintab
peaks.dfmin$identifier <- 1:nrow(peaks.dfmin)

colnames(peaks.dfmax) = c("date", "value", "identifier")
colnames(peaks.dfmin) = c("date", "value", "identifier")

maxdate <- which.max(baseline$date)
lastrow <- cbind.data.frame(date = baseline$date[maxdate], 
                            value = baseline$pm2.5corr.out[maxdate],
                            identifier = max(peaks.dfmax$identifier))
peaks.dfmin <- rbind.data.frame(peaks.dfmin, lastrow)

peaks.df <- rbind.data.frame(peaks.dfmax, peaks.dfmin)

baseline <- merge(baseline,peaks.df, by = "date", all = T)
baseline <- baseline[,c(1,2,8,14:16,21:23)]

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
  baseline$peakfilter <- ifelse((baseline$date == cur.peak$date[1]),
                                TRUE,baseline$peakfilter)
}

## adding switches to peakgroups: TRUE signifies Indoor source in both cases ####

## getting index  ###
baseline$ind=1:length(baseline$date)

peakdates.index <- which(!is.na(baseline$peakfilter)) 
peakdates.index <- peakdates.index - 1


## threshold set to last non-NA value before the peak
allpeakdates <- cbind.data.frame(peakdate = baseline$date[peakdates.index],
                                 thresh.pm = baseline$pm2.5corr.in[peakdates.index],
                                 # lastnonNA =baseline$ind[which(baseline$date %in% peakdates)],
                                 identifier = baseline$identifier[which(!is.na(baseline$peakfilter))])





baseline <- merge(baseline, allpeakdates, by = "identifier", all = T)

baseline$indmorethanout <- ifelse((baseline$pm2.5corr.in >= baseline$pm2.5corr.out), 
                                  TRUE,FALSE)

baseline$concmorethanth <- ifelse((baseline$pm2.5corr.in < 1 | baseline$pm2.5corr.in <=baseline$thresh.pm), 
                                  FALSE, TRUE)

## applying a filter to remove values after peak till 1 or till it reaches a set threshold
baseline$pm2.5.pcT <- ifelse((!is.na(baseline$thresh.pm) &
                                baseline$concmorethanth == TRUE), NA, baseline$pm2.5corr.in.original)

baseline$pm2.5.icT <- ifelse((baseline$indmorethanout == TRUE),
                             NA, baseline$pm2.5.pcT)


ggplotly(ggplot(baseline) +
           geom_point(aes(date, value)) +
           geom_line(aes(date, pm2.5corr.in.original, color = "Indoor")) +
           # geom_line(aes(date, pm2.5corr.out, color = "Outdoor")) +
           geom_line(aes(date,pm2.5.pcT, color = "Peak to threshold filter")) + 
           # geom_line(aes(date, pm2.5.icT, color = "Peak to threshold  + Outdoor filter"))  +
           ggtitle(baseline$HouseID[1]) +
           theme_bw())

## Iteration 2 ####
base2 <- baseline[,c(2:8,14:17)] ## resetting the dataset
base2 <- base2[order(base2$date),]
base2$pm2.5.pcT.original <- base2$pm2.5.pcT
base2$pm2.5.pcT <- na.locf(base2$pm2.5.pcT, na.rm = F,fromLast = TRUE) # patch NAs created from first iteration
peaks.base2 <- peakdet(base2$pm2.5.pcT,10, base2$date)
peaks.dfmax <- peaks.base2$maxtab
peaks.dfmax$identifier <- 1:nrow(peaks.dfmax)
  
peaks.dfmin <- peaks.base2$mintab
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

### adding the identifier across to each peakgroup
peakgroups <- unique(peaks.df$identifier)
print(paste("Number of peaks:", length(peakgroups)))
base2$peakfilter <- NA

i =1
for(i in 1:length(peakgroups)) {
  cur.peak <- peaks.df[which(peaks.df$identifier == peakgroups[i]),]
  
  ## making a peak group
  base2$identifier <- ifelse((base2$date >= cur.peak$date[1] & 
                                base2$date<=cur.peak$date[2]), 
                             peakgroups[i], base2$identifier)
  
  ## apply peak filter
  base2$peakfilter <- ifelse(base2$date == cur.peak$date[1], TRUE, base2$peakfilter)
  base2$peakfilter <- ifelse((base2$date == cur.peak$date[1]),
                             TRUE,base2$peakfilter)
}

## adding switches to peakgroups: TRUE signifies Indoor source in both cases ####

## getting index  ###
base2$ind=1:length(base2$date)

peakdates.index <- which(!is.na(base2$peakfilter)) 
peakdates.index <- peakdates.index - 1


## threshold set to last non-NA value before the peak
allpeakdates <- cbind.data.frame(peakdate = base2$date[peakdates.index],
                                 thresh.pm = base2$pm2.5.pcT[peakdates.index],
                                 # lastnonNA =baseline$ind[which(baseline$date %in% peakdates)],
                                 identifier = base2$identifier[which(!is.na(base2$peakfilter))])

base2 <- merge(base2, allpeakdates, by = "identifier", all = T)

base2$indmorethanout <- ifelse((base2$pm2.5.pcT >= base2$pm2.5corr.out), 
                               TRUE,FALSE)

base2$concmorethanth <- ifelse((base2$pm2.5.pcT < 1 | base2$pm2.5.pcT <=base2$thresh.pm), 
                               FALSE, TRUE)


base2$pm2.5.pcT2 <- ifelse((!is.na(base2$thresh.pm) &
                              base2$concmorethanth == TRUE), NA, base2$pm2.5.pcT.original)

ggplotly(ggplot(base2) +
           geom_point(aes(date, value)) +
           geom_line(aes(date, pm2.5corr.in, color = "Indoor")) +
           # geom_line(aes(date, pm2.5corr.out, color = "Outdoor")) +
           geom_line(aes(date,pm2.5.pcT.original, color = "Peak to threshold filter")) + 
           # geom_line(aes(date,pm2.5.icT, color = "Peak to threshold + outdoor filter")) +   
           geom_line(aes(date, pm2.5.pcT2, color = "Peak to threshold- 2nd iteration"))  +
           ggtitle(baseline$HouseID[1]) +
           theme_bw())


## Iteration 3 ####

base3 <- base2[,c(2:11,13:14,19)] ## resetting the dataset
base3 <- base3[order(base3$date),]
base3$pm2.5.pcT2.original <- base3$pm2.5.pcT2
base3$pm2.5.pcT2 <- na.locf(base3$pm2.5.pcT2, na.rm = F,fromLast = TRUE) # patch NAs created from first iteration
peaks.base3 <- peakdet(base3$pm2.5.pcT2,5, base3$date)
peaks.dfmax <- peaks.base3$maxtab
peaks.dfmax$identifier <- 1:nrow(peaks.dfmax)

peaks.dfmin <- peaks.base3$mintab
peaks.dfmin$identifier <- 1:nrow(peaks.dfmin)

colnames(peaks.dfmax) = c("date", "value", "identifier")
colnames(peaks.dfmin) = c("date", "value", "identifier")
maxdate <- which.max(base3$date)
lastrow <- cbind.data.frame(date = base3$date[maxdate],
                            value = base3$pm2.5.pcT2[maxdate],
                            identifier = max(peaks.dfmax$identifier))

peaks.dfmin <- rbind.data.frame(peaks.dfmin, lastrow)

peaks.df <- rbind.data.frame(peaks.dfmax, peaks.dfmin)

base3 <- merge(base3,peaks.df, by = "date", all = T)

### adding the identifier across to each peakgroup
peakgroups <- unique(peaks.df$identifier)
print(paste("Number of peaks:", length(peakgroups)))
base3$peakfilter <- NA

i =1
for(i in 1:length(peakgroups)) {
  cur.peak <- peaks.df[which(peaks.df$identifier == peakgroups[i]),]
  
  ## making a peak group
  base3$identifier <- ifelse((base3$date >= cur.peak$date[1] & 
                                base3$date<=cur.peak$date[2]), 
                             peakgroups[i], base3$identifier)
  
  ## apply peak filter
  base3$peakfilter <- ifelse(base3$date == cur.peak$date[1], TRUE, base3$peakfilter)
  base3$peakfilter <- ifelse((base3$date == cur.peak$date[1]),
                             TRUE,base3$peakfilter)
}

## adding switches to peakgroups: TRUE signifies Indoor source in both cases ####

## getting index  ###
base3$ind=1:length(base3$date)

peakdates.index <- which(!is.na(base3$peakfilter)) 
peakdates.index <- peakdates.index - 1


## threshold set to last non-NA value before the peak
allpeakdates <- cbind.data.frame(peakdate = base3$date[peakdates.index],
                                 thresh.pm = base3$pm2.5.pcT2[peakdates.index],
                                 # lastnonNA =baseline$ind[which(baseline$date %in% peakdates)],
                                 identifier = base3$identifier[which(!is.na(base3$peakfilter))])

base3 <- merge(base3, allpeakdates, by = "identifier", all = T)

base3$indmorethanout <- ifelse((base3$pm2.5.pcT2 >= base3$pm2.5corr.out), 
                               TRUE,FALSE)

base3$concmorethanth <- ifelse((base3$pm2.5.pcT2 < 1 | base3$pm2.5.pcT2 <=base3$thresh.pm), 
                               FALSE, TRUE)


base3$pm2.5.pcT3 <- ifelse((!is.na(base3$thresh.pm) &
                              base3$concmorethanth == TRUE), NA, base3$pm2.5.pcT2.original)

ggplotly(ggplot(base3) +
           # geom_point(aes(date, value)) +
           geom_line(aes(date, pm2.5corr.in, color = "Indoor")) +
           # geom_line(aes(date, pm2.5corr.out, color = "Outdoor")) +
           geom_line(aes(date,pm2.5.pcT, color = "Peak to threshold filter")) + 
           geom_line(aes(date,pm2.5.pcT2, color = "Peak to threshold- 2nd iteration")) +   
           geom_line(aes(date, pm2.5.pcT3, color = "Peak to threshold- 3rd iteration"))  +
           ggtitle(base3$HouseID[1]) +
           theme_bw())

## Iteration 4 ####

base4 <- base3[,c(2:10,12,15,21)] ## resetting the dataset
base4 <- base4[order(base4$date),]
base4$pm2.5.pcT3.original <- base4$pm2.5.pcT3
base4$pm2.5.pcT3 <- na.locf(base4$pm2.5.pcT3, na.rm = F,fromLast = TRUE) # patch NAs created from first iteration
peaks.base4 <- peakdet(base4$pm2.5.pcT3,5, base4$date)
peaks.dfmax <- peaks.base4$maxtab
peaks.dfmax$identifier <- 1:nrow(peaks.dfmax)

peaks.dfmin <- peaks.base4$mintab
peaks.dfmin$identifier <- 1:nrow(peaks.dfmin)

colnames(peaks.dfmax) = c("date", "value", "identifier")
colnames(peaks.dfmin) = c("date", "value", "identifier")
maxdate <- which.max(base4$date)
lastrow <- cbind.data.frame(date = base4$date[maxdate],
                            value = base4$pm2.5.pcT3[maxdate],
                            identifier = max(peaks.dfmax$identifier))

peaks.dfmin <- rbind.data.frame(peaks.dfmin, lastrow)

peaks.df <- rbind.data.frame(peaks.dfmax, peaks.dfmin)

base4 <- merge(base4,peaks.df, by = "date", all = T)

### adding the identifier across to each peakgroup
peakgroups <- unique(peaks.df$identifier)
print(paste("Number of peaks:", length(peakgroups)))
base4$peakfilter <- NA

i =1
for(i in 1:length(peakgroups)) {
  cur.peak <- peaks.df[which(peaks.df$identifier == peakgroups[i]),]
  
  ## making a peak group
  base4$identifier <- ifelse((base4$date >= cur.peak$date[1] & 
                                base4$date<=cur.peak$date[2]), 
                             peakgroups[i], base4$identifier)
  
  ## apply peak filter
  base4$peakfilter <- ifelse(base4$date == cur.peak$date[1], TRUE, base4$peakfilter)
  base4$peakfilter <- ifelse((base4$date == cur.peak$date[1]),
                             TRUE,base4$peakfilter)
}

## adding switches to peakgroups: TRUE signifies Indoor source in both cases ####

## getting index  ###
base4$ind=1:length(base4$date)

peakdates.index <- which(!is.na(base4$peakfilter)) 
peakdates.index <- peakdates.index - 1


## threshold set to last non-NA value before the peak
allpeakdates <- cbind.data.frame(peakdate = base4$date[peakdates.index],
                                 thresh.pm = base4$pm2.5.pcT3[peakdates.index],
                                 # lastnonNA =baseline$ind[which(baseline$date %in% peakdates)],
                                 identifier = base4$identifier[which(!is.na(base4$peakfilter))])

base4 <- merge(base4, allpeakdates, by = "identifier", all = T)

base4$indmorethanout <- ifelse((base4$pm2.5.pcT3 >= base4$pm2.5corr.out), 
                               TRUE,FALSE)

base4$concmorethanth <- ifelse((base4$pm2.5.pcT3 < 1 | base4$pm2.5.pcT3 <=base4$thresh.pm), 
                               FALSE, TRUE)


base4$pm2.5.pcT4 <- ifelse((!is.na(base4$thresh.pm) &
                              base4$concmorethanth == TRUE), NA, base4$pm2.5.pcT3.original)

ggplotly(ggplot(base4) +
           geom_point(aes(date, value)) +
           geom_line(aes(date, pm2.5corr.in.original, color = "Indoor")) +
           geom_line(aes(date, pm2.5.pcT.original, color = "Peak to threshold filter")) +
           geom_line(aes(date,pm2.5.pcT2.original, color = "Peak to threshold- 2nd iteration")) + 
           geom_line(aes(date,pm2.5.pcT3.original, color = "Peak to threshold- 3rd iteration")) +   
           geom_line(aes(date, pm2.5.pcT4, color = "Peak to threshold- 4th iteration"))  +
           ggtitle(base4$HouseID[1]) +
           theme_bw())

## Iteration 5 ####

base5 <- base4[,c(2:12,14,20)] ## resetting the dataset
base5 <- base5[order(base5$date),]
base5$pm2.5.pcT4.original <- base5$pm2.5.pcT4
base5$pm2.5.pcT4 <- na.locf(base5$pm2.5.pcT4, na.rm = F,fromLast = TRUE) # patch NAs created from first iteration
peaks.base5 <- peakdet(base5$pm2.5.pcT4,5, base5$date)
peaks.dfmax <- peaks.base5$maxtab
peaks.dfmax$identifier <- 1:nrow(peaks.dfmax)

peaks.dfmin <- peaks.base5$mintab
peaks.dfmin$identifier <- 1:nrow(peaks.dfmin)

colnames(peaks.dfmax) = c("date", "value", "identifier")
colnames(peaks.dfmin) = c("date", "value", "identifier")
maxdate <- which.max(base5$date)
lastrow <- cbind.data.frame(date = base5$date[maxdate],
                            value = base5$pm2.5.pcT4[maxdate],
                            identifier = max(peaks.dfmax$identifier))

peaks.dfmin <- rbind.data.frame(peaks.dfmin, lastrow)

peaks.df <- rbind.data.frame(peaks.dfmax, peaks.dfmin)

base5 <- merge(base5,peaks.df, by = "date", all = T)

### adding the identifier across to each peakgroup
peakgroups <- unique(peaks.df$identifier)
print(paste("Number of peaks:", length(peakgroups)))
base5$peakfilter <- NA

i =1
for(i in 1:length(peakgroups)) {
  cur.peak <- peaks.df[which(peaks.df$identifier == peakgroups[i]),]
  
  ## making a peak group
  base5$identifier <- ifelse((base5$date >= cur.peak$date[1] & 
                                base5$date<=cur.peak$date[2]), 
                             peakgroups[i], base5$identifier)
  
  ## apply peak filter
  base5$peakfilter <- ifelse(base5$date == cur.peak$date[1], TRUE, base5$peakfilter)
  base5$peakfilter <- ifelse((base5$date == cur.peak$date[1]),
                             TRUE,base5$peakfilter)
}

## adding switches to peakgroups: TRUE signifies Indoor source in both cases ####

## getting index  ###
base5$ind=1:length(base5$date)

peakdates.index <- which(!is.na(base5$peakfilter)) 
peakdates.index <- peakdates.index - 1


## threshold set to last non-NA value before the peak
allpeakdates <- cbind.data.frame(peakdate = base5$date[peakdates.index],
                                 thresh.pm = base5$pm2.5.pcT4[peakdates.index],
                                 # lastnonNA =baseline$ind[which(baseline$date %in% peakdates)],
                                 identifier = base5$identifier[which(!is.na(base5$peakfilter))])

base5 <- merge(base5, allpeakdates, by = "identifier", all = T)

base5$indmorethanout <- ifelse((base5$pm2.5.pcT4 >= base5$pm2.5corr.out), 
                               TRUE,FALSE)

base5$concmorethanth <- ifelse((base5$pm2.5.pcT4 < 1 | base5$pm2.5.pcT4 <=base5$thresh.pm), 
                               FALSE, TRUE)


base5$pm2.5.pcT5 <- ifelse((!is.na(base5$thresh.pm) &
                              base5$concmorethanth == TRUE), NA, base5$pm2.5.pcT4.original)

ggplotly(ggplot(base5) +
           geom_point(aes(date, value)) +
           geom_line(aes(date, pm2.5corr.in.original, color = "Indoor")) +
           geom_line(aes(date, pm2.5.pcT.original, color = "Peak to threshold filter")) +
           geom_line(aes(date,pm2.5.pcT2.original, color = "Peak to threshold- 2nd iteration")) + 
           geom_line(aes(date,pm2.5.pcT3.original, color = "Peak to threshold- 3rd iteration")) +   
           geom_line(aes(date, pm2.5.pcT4, color = "Peak to threshold- 4th iteration"))  +
           geom_line(aes(date, pm2.5.pcT5, color = "Peak to threshold- 5th iteration"))  +
           ggtitle(base5$HouseID[1]) +
           theme_bw())


## final summary ####
mean(base4$pm2.5corr.in.original, na.rm = T)
mean(base4$pm2.5.pcT.original, na.rm = T)
mean(base4$pm2.5.pcT2.original, na.rm = T)
mean(base4$pm2.5.pcT3.original, na.rm = T)
mean(base4$pm2.5.pcT4, na.rm = T)
mean(base5$pm2.5.pcT5, na.rm = T)

