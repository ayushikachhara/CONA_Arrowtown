#### load libraries ####
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
      ggpmisc)

#### function: peak detection #####
peakdet <- function(v, delta, x = NULL) {
  maxtab <- NULL
  mintab <- NULL
  
  if (is.null(x))
  {
    x <- seq_along(v)
  }
  
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


#######read data ####

path <- "Q:/AirQual/Research/CONA/Hobart/2018/Wood smoke study/woodsmoke study/2018/pm house date/"
setwd(path)
smog10min <- read.csv("smog10min_AK.csv", stringsAsFactors = F)
smog10min$date <- dmy_hm(smog10min$date)
smog10min$time <- as.POSIXct(format(smog10min$date, format = "%H:%M"), format = "%H:%M")
smog10min$dateonly <- as.Date(smog10min$date)

smog.list <- split(smog10min, smog10min$houseID)
smog.fits.list <- list() 
allfits = NULL
# PDFfile <- paste0(path,"SMOGpeaksdecaysdetection.pdf")
# pdf(file=PDFfile, paper = "USr", width = 28)

for(i in 1:length(smog.list)) {
  
  tryCatch({
    smog <- smog.list[[i]]
    
    ### peaks for plots #####
    meanpm25 <- mean(smog$pm2.5corr.in, na.rm = T)
    peaks <- peakdet(smog$pm2.5corr.in, 30)
    
    ### checking hourly fits ###
    for(k in 1:(nrow(smog)-7)) {
      
      pm2.5 <- smog$pm2.5corr.in[k:(k+6)]
      if(sum(is.na(pm2.5)) >= 2) {
        print("too many NAs")
      } else {
        nos <- 1:length(pm2.5)
        fit <- lm(log(pm2.5)~nos)
        
        
        fit.table <- cbind.data.frame(date= smog$date[k],
                                      rsquared =  summary(fit)$adj.r.squared,
                                      slope = fit$coefficients[2])
        allfits <- rbind(allfits,fit.table)
        
        
      }
      
    }
    allfits <- allfits[which(allfits$slope <=0),]
    smog.fits <- merge(smog, allfits, by = "date", all = T)
    smog.fits$flag <- ifelse((!is.na(smog.fits$slope) & 
                                smog.fits$rsquared >0.6),1, 0)
    
    ## for each decay, define how long it lasted and average slope ####
    r <- rle(smog.fits$flag)
    sequence <- unlist(sapply(r$lengths, seq))
    smog.fits$decay.length <- sequence
    smog.fits$decay.length <- ifelse(smog.fits$flag ==0,NA, smog.fits$decay.length)
    
    r$values[r$lengths <= 6] <- 0
    smog.fits$invrle <- inverse.rle(r)
    r.new <- rle(smog.fits$invrle)
    r.new$group <- 1:length(r.new$values)
    seq2 <- unlist(sapply(r.new$lengths, seq))
    seq3 <- rep(r.new$group, r.new$lengths)
    
    smog.fits$decaylength.corr <- seq2
    smog.fits$decaygroup <- seq3
    
    smog.fits$decaylength.corr[smog.fits$invrle <= 0] <- 0
    smog.fits$decaylength.corr[is.na(smog.fits$invrle)] <- NA
    smog.fits$decaylength.corr[smog.fits$decaylength.corr ==0] <- NA
    smog.fits$decaygroup <- ifelse(is.na(smog.fits$decaylength.corr), NA, smog.fits$decaygroup)
    group.rem <- smog.fits$decaygroup[which(smog.fits$decaylength.corr == 1 &
                                              smog.fits$decay < 25)]
    smog.fits$decaygroup <- ifelse(smog.fits$decaygroup %in% group.rem, NA, smog.fits$decaygroup)
    
    smog.fits$decay <- ifelse(smog.fits$decaygroup>0,smog.fits$pm2.5corr.in,NA)
    smog.fits$decaylength.corr <-ifelse(smog.fits$decaygroup>0,smog.fits$decaylength.corr,NA)
    
    if(i == 1) {
      all.decay.stats <- smog.fits %>% group_by(houseID, decaygroup) %>%
        summarise(starttime = date[1],
                  decay = max(decaylength.corr, na.rm = T),  ## because I look the the following 6 values
                  slope = mean(slope, na.rm = T),
                  meanDust = mean(decay, na.rm =T),
                  dateonly = mean(dateonly))
      all.decay.stats <- all.decay.stats[complete.cases(all.decay.stats),]
      
      ### plotting ####
      plot(smog.fits$pm2.5corr.in, type = "l", ylim = c(0,500),
           main = paste("ID:",all.decay.stats$houseID[1]), col = "grey")
      lines(smog.fits$decay, col = "red", cex = 2)
      points(peaks$maxtab, pch = 19)
      
    } else {
      stat.table <- smog.fits %>% group_by(houseID, decaygroup) %>%
        summarise(starttime = date[1],
                  decay = max(decaylength.corr, na.rm = T),  ## because I look the the following 6 values
                  slope = mean(slope, na.rm = T),
                  meanDust = mean(pm2.5corr.in, na.rm =T),
                  dateonly = mean(dateonly))
      stat.table <- stat.table[complete.cases(stat.table),]
      
      all.decay.stats <- rbind.data.frame(all.decay.stats,stat.table)
      
      ### plotting ####
      plot(smog.fits$pm2.5corr.in, type = "l", ylim = c(0,200),
           main = paste("ID:",stat.table$houseID[1]), col = "grey")
      lines(smog.fits$decay, col = "red", cex = 2)
      # points(peaks$maxtab, pch = 19)
    }
    
    
    # hist(sko.fits$slope[which(!is.na(sko.fits$slope))], 10, col = "royalblue",
    #      main = paste("ID:",sko.fits$ID[1]))
    # 
    smog.fits.list[[i]] <- smog.fits
    print(i)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
dev.off()

allsko <- rbind_list(sko.fits.list)
allsko <- allsko[,-c(2,13,14,15)]

all.decay.stats <- all.decay.stats %>% group_by(ID) %>%
  mutate(decaygroup = 1:n())
all.decay.stats$decay_minutes <- all.decay.stats$decay*10



all.home.stats <- all.decay.stats %>% group_by(ID) %>%
  summarise(no.decays = max(decaygroup),
            average.slope = mean(slope, na.rm = T),
            slope.std.dev = sd(slope,na.rm = T),
            average.duration = mean(decay_minutes, na.rm = T),
            duration.std.dev = sd(decay_minutes, na.rm = T))

# write.csv(all.home.stats, "DecaySummary_perHouse.csv")

write.csv(all.decay.stats, "allDecayStats.csv")
write.csv(allsko, "allsmo_decays.included.csv")

# for( id in 1:length(unique(all.decay.stats$ID))) {
#   decay <- all.decay.stats$decay[which(all.decay.stats$ID == unique(all.decay.stats$ID)[id])]
#   hist(decay, main = id)
# }

all.decay.stats <- as.data.frame(all.decay.stats)

plot(all.decay.stats$slope~all.decay.stats$decay, ylim = c(-1,0))
hist((all.decay.stats$decay*10)/60, 30)


all.decay.stats <- all.decay.stats %>% group_by(ID) %>%
  mutate(max.decay = max(decay))

all.decays.maxes <- all.decay.stats[which(all.decay.stats$decay == all.decay.stats$max.decay),]

ggplot(all.decay.stats) +
  geom_point(aes((decay*10)/60, slope, color = ID), size = 2) +
  scale_x_continuous(name = "Number of hours (decay period)") +
  scale_y_continuous(name = "Average Slope of the decay") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.margin =unit(c(1,1,1,1), "cm"))




######## from peak detection script ####
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

# PDFfile <- paste0(path,"SMOG2018_diurnal_interventionType.pdf")
# pdf(file=PDFfile, paper = "USr", width = 28)

for(i in 1:length(smog.list)) {
  smog <- smog.list[[i]]
  smog$Time <- as.POSIXct(format(smog$date, format = "%H:%M"), format = "%H:%M")
  
  
  p1 <-  ggplot(smog) +
    geom_point(aes(Time, pm2.5corr.in, color = InterventionType)) +
    # geom_point(aes(Time, pm2.5corr.out, color = "Outdoor PM2.5")) +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
    ggtitle(paste("House ID:", smog$houseID[1])) +
    theme_bw()
  print(p1)
  
  p2 <-  ggplot(smog) +
    geom_smooth(aes(Time, pm2.5corr.in, color = InterventionType), se = F) +
   # geom_point(aes(Time, pm2.5corr.out, color = "Outdoor PM2.5")) +
    scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
    ggtitle(paste("House ID:", smog$houseID[1])) +
    theme_bw()
  print(p2)
  
  
}

# dev.off()
smog10min$nonNA_io <- ifelse((!is.na(smog10min$pm2.5.in) &
                           !is.na(smog10min$pm2.5.out)), TRUE, FALSE)


smog.summary <- smog10min %>% group_by(houseID, InterventionType,nonNA_io) %>%
  summarise(meanin = mean(pm2.5.in, na.rm = T),
            meanout = mean(pm2.5.out, na.rm = T),
            count = n())
smog.summary <- smog.summary[which(smog.summary$nonNA_io == TRUE),]
