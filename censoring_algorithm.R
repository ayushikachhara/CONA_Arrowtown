## peak detection function #####
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

### minimisation of sum of squares function ####
fn <- function(par) {
  x <- par[1]
  y <- par[2]
  
  vectorc <- ind + x*((y*out) - ind)
  return(sqrt(sum((vectorb - vectorc)^2, na.rm = T)))
}

### new censoring algorithm ###

## import libraries ###
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

path <- "Q:/AirQual/Research/CONA/Hobart/2018/Wood smoke study/woodsmoke study/2018/pm house date/"
setwd(path)

smog10min <- read.csv("smog10min_AK.csv", stringsAsFactors = F)
smog10min$date <- dmy_hm(smog10min$date)
smog10min$time <- as.POSIXct(format(smog10min$date, format = "%H:%M"), format = "%H:%M")
smog10min$dateonly <- as.Date(smog10min$date)
# 
# smogodin <- rbind.data.frame(smog10min,odin10min)
# smogodin$HouseID <- as.character(smogodin$HouseID)

df <- smog10min[which(smog10min$HouseID == 1 &
                         smog10min$InterventionType == "Baseline"),]

df$pm2.5corr.in <- ifelse(df$pm2.5.in <= df$pm10.in, df$pm2.5.in, NA)
df$pm2.5corr.out <- ifelse(df$pm2.5.in <= df$pm10.in, df$pm2.5.out, NA)

plot(df$pm2.5corr.out, type = "l")
lines(df$pm2.5corr.in, col = "blue")



##### running first guess model #####
corr.factor = 0.9
AIF = 0.02
aif = 1
smog <- df
smog <- smog[complete.cases(smog$pm2.5corr.out),] ### model only data with outdoor data available
## picking a clean dataset for modelling
smog$pm2.5.inClean <-ifelse(smog$pm2.5corr.in>50, NA, smog$pm2.5corr.in)

NonNAindex <- which(!is.na(smog$pm2.5.inClean))
firstNonNA <- min(NonNAindex)
## start the modelling with one indoor value in the beginning, pre-defined AIF and corr.factor ###
smog$modelled <- rep(NA, nrow(smog))
smog$modelled[firstNonNA] <- smog$pm2.5.inClean[firstNonNA]
### column with initial concentrations loaded ###
for (j in (firstNonNA+1):nrow(smog)){
  base.value <- smog$modelled[(j-1)]
  out.value <- smog$pm2.5corr.out[(j)] ### CHAMGE IT ALL OVER
  # smog$modelled[j] <- corr.factor*(base.value + AIF[aif]*(out.value - base.value))
  smog$modelled[j] <- base.value + AIF[aif]*((corr.factor*out.value) - base.value)

} 



##### optimise model based on complete dataset ####
vectorb <- smog$pm2.5.inClean
ind <- lag(smog$pm2.5.inClean) ### previous value in the model for initial conc. ###
out <- smog$pm2.5corr.out### outdoor odin values ####

### perform optimisation #####
results1 <- optim(c(0.02, 0.9), fn)

### re-modelling using optimised values #####
smog$optim.model <- rep(NA,nrow(smog))
smog$optim.model[firstNonNA] <-smog$pm2.5.inClean[firstNonNA]

### column with new factor loaded ###
for (j in (firstNonNA+1):nrow(smog)){
  base.value <- smog$optim.model[(j-1)]
  out.value <- smog$pm2.5corr.out[(j)]
  # smog$optim.model[j] <- results$par[2]*(base.value + results$par[1]*(out.value - base.value))
  smog$optim.model[j] <-base.value + results1$par[1]*((results1$par[2]*out.value) - base.value)
}

fit.all <- summary(lm(optim.model~pm2.5.inClean, data = smog))$adj.r.squared
fit1 <- data.frame(modelfit = fit.all,
                   initial.AIF = 0.02,
                   optim.AIF = results1$par[1],
                   optim.corr.factor = results1$par[2],
                   pos = NA,
                   no.removed = 0)


### finding peak values ####
peaks <- peakdet(smog$pm2.5corr.in, 10, smog$date)
peaks.df <- peaks$maxtab
peaks.dfmin <- peaks$mintab

colnames(peaks.df) = c("date", "peakvalue")
smog <- merge(smog,peaks.df, by = "date", all = T)

smog$pos <- 1:nrow(smog)

peak.pos <- smog$pos[which(complete.cases(smog$peakvalue))]

plot(smog$pm2.5corr.in~smog$date, type = "l")
points(smog$peakvalue~smog$date)
points(peaks.dfmin, col = "red")
#### fit dataset initialise ####
allfits <- cbind.data.frame(peakpos = NA,
                            number.removed = 0,
                            aif = fit1$optim.AIF,
                            corr.factor = fit1$optim.corr.factor,
                            fit = fit.all)

i =1
for(i in 1:length(peak.pos)) {
  k = peak.pos[i] ### initial position
  counter = 0 #### initial counter
  threshold <- ifelse(i == length(peak.pos), 1718,peak.pos[(i+1)])
  
  while((k+counter) < threshold){
    df <- smog ### set initial dataset
    remove.pos <- c(k:(k+counter)) ### which values are to be removed ####
    df$censored <- ifelse(smog$pos %in% remove.pos, NA, smog$pm2.5.inClean) #### remove and create a "censored" column
    
    vectorb <- df$censored
    ind <- lag(df$censored) ### previous value in the model for initial conc. ###
    out <- df$pm2.5corr.out### outdoor odin values ####
    
    ### perform optimisation #####
    results <- optim(c(0.02, 0.9), fn)
    
    ### re-modelling using optimised values #####
    df$optim.model <- rep(NA,nrow(df))
    df$optim.model[firstNonNA] <-df$pm2.5.inClean[firstNonNA]
    
    ### column with new factor loaded ###
    for (j in (firstNonNA+1):nrow(df)){
      base.value <- df$optim.model[(j-1)]
      out.value <- df$pm2.5corr.out[(j)]
      # df$optim.model[j] <- results$par[2]*(base.value + results$par[1]*(out.value - base.value))
      df$optim.model[j] <-base.value + results$par[1]*((results$par[2]*out.value) - base.value)
    }
    
    fit.params <- cbind.data.frame(peakpos = k,
                                   number.removed = (counter + 1),
                                   aif = results$par[1],
                                   corr.factor = results$par[2],
                                   fit = summary(lm(optim.model~pm2.5.inClean, data = df))$adj.r.squared)
    allfits <- rbind.data.frame(allfits, fit.params)
    counter = counter + 1
  }
  print(k)
}


ggplot(allfits) +
  geom_point(aes(number.removed, fit, color = factor(peakpos))) +
  scale_y_continuous(limits = c(0.76,0.79))


allfits.max <- allfits %>% group_by(peakpos) %>%
  summarise(max.fit = quantile(fit, na.rm = T)[3],
            pos.restore = which(fit>fit.all)[1])

allfits <- merge(allfits, allfits.max, by = c("peakpos"))


allfits2 <- allfits[which(allfits$fit == allfits$max.fit),]

ggplot(allfits2) +
  geom_point(aes(number.removed, fit, color = factor(peakpos)), size = 2) +
  geom_smooth(aes(number.removed, fit), se = F, method = "lm") + 
  theme_bw() +
  scale_y_continuous(limits = c(0.77,0.785))



### creating a vector to remove indoor events based on censoring approach #####
final.rm.vector <- NA
i = 1
for(i in 1:length(allfits2$peakpos)) {
  vec <- c(allfits2$peakpos[i]:(allfits2$peakpos[i] + allfits2$number.removed[i] - 1))
  final.rm.vector <- c(final.rm.vector, vec)
  print(i)
}

final.rm.vector <- final.rm.vector[-1] ## removing NA
smog$censored <- ifelse(smog$pos %in% final.rm.vector, NA, smog$pm2.5.inClean)

vectorb <- smog$censored
ind <- lag(smog$censored) ### previous value in the model for initial conc. ###
out <- smog$pm2.5corr.out### outdoor odin values ####

### perform optimisation #####
results <- optim(c(0.02, 0.9), fn)

### re-modelling using optimised values #####
smog$optim.model <- rep(NA,nrow(smog))
smog$optim.model[firstNonNA] <-smog$pm2.5.inClean[firstNonNA]

### column with new factor loaded ###
for (j in (firstNonNA+1):nrow(smog)){
  base.value <- smog$optim.model[(j-1)]
  out.value <- smog$pm2.5corr.out[(j)]
  # smog$optim.model[j] <- results$par[2]*(base.value + results$par[1]*(out.value - base.value))
  smog$optim.model[j] <-base.value + results$par[1]*((results$par[2]*out.value) - base.value)
}

p1 <- ggplot(smog) +
  geom_line(aes(date, pm2.5corr.in, color = "Actual"), size = 0.5) +
  geom_line(aes(date, censored, color = "Censored"), size = 0.5) +
  geom_line(aes(date, optim.model, color = "Optimized Model"), size = 1) +
  scale_color_manual(labels = c("Actual Indoor","Censored Indoor", "Optimised Model"),
                     values = c("grey", "blue", "red")) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,50,10), name = "PM2.5") +
  theme_bw() +
  ggtitle(paste("HouseID & Intervention:", smog$HouseID[1],smog$InterventionType[1],": Initial AIF is",
                AIF[aif],": \nOptimised AIF is",
                round(unique(results$par[1]),3), "\nCorrection Factor is",
                round(unique(results$par[2]),3)))
print(p1)




