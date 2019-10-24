#### SMOG/ODIN analysis script #####

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

# smogodin_fixed <- timeAverage(smogodin, avg.time = "10 min", type = c("HouseID","InterventionType"))
# smogodin_fixed$time <- as.POSIXct(format(smogodin_fixed$date, format = "%H:%M"), format = "%H:%M")
# smogodin_fixed$dateonly <- as.Date(smogodin_fixed$date)
### per home infiltration model ####

smog.all <- smog10min # smogodin_fixed
smog.list <- split(smog.all, smog.all$HouseID)
smog.list <- list(smog.list$`22`)
indoor.list <- list()
count.censored.list <- list()

### function to calculate sum of square of errors ####
fn <- function(par) {
  x <- par[1]
  y <- par[2]
  
  vectorc <- ind + x*((y*out) - ind)
  return(sqrt(sum((vectorb - vectorc)^2, na.rm = T)))
}
# fn(c(0.2,0.99))

fit.params <- data.frame(HouseID = NA,
                         InterventionType = NA,
                         initial.AIF = NA,
                         optim.AIF = NA,
                         optim.corr.factor = NA)

AIF <- 0.02# seq(0.02,0.2, 0.01) ## Air Infiltration Factor per 10 minutes
aif <- 1
## initiate list count
k <- 1
i = 1
# v = 2

# ### PDF path ###
PDFfile <- paste0(path,"Models_InterventionType_SMOG_newmodel02052019.pdf")
pdf(file=PDFfile, paper = "USr", width = 28)

for (i in 1:length(smog.list)) {
  tryCatch({
    smog.cur <- smog.list[[i]]
    visit.list <- split(smog.cur, smog.cur$InterventionType)
    for(v in 1:length(visit.list)) {
      corr.factor <- 0.9 ## correction factor for differences between two dust sensors
      smog <- visit.list[[v]] ## import current house
      smog <- smog[complete.cases(smog$pm2.5corr.out),] ### model only data with outdoor data available
      ## picking a clean dataset for modelling
      smog$pm2.5.inClean <-ifelse(smog$pm2.5corr.in>50, NA, smog$pm2.5corr.in)
      smog$io.ratio <- smog$pm2.5corr.in/ smog$pm2.5corr.out ## I/O ratios
      
      NonNAindex <- which(!is.na(smog$pm2.5.inClean))
      firstNonNA <- min(NonNAindex)
      ## start the modelling with one indoor value in the beginning, pre-defined AIF and corr.factor ###
      smog$modelled <- rep(NA, nrow(smog))
      smog$modelled[firstNonNA] <- smog$pm2.5.inClean[firstNonNA]
      
      ### column with initial concentrations loaded ###
      for (j in (firstNonNA+1):nrow(smog)){
        base.value <- smog$modelled[(j-1)]
        out.value <- smog$pm2.5corr.out[(j-1)] ### CHAMGE IT ALL OVER
        # smog$modelled[j] <- corr.factor*(base.value + AIF[aif]*(out.value - base.value))
        smog$modelled[j] <- base.value + AIF[aif]*((corr.factor*out.value) - base.value)
      }
      
      ### censor all indoor sources by setting an arbitrary threshold ####
      smog$censored <- ifelse((smog$pm2.5.inClean - smog$modelled) > 20, NA,smog$pm2.5corr.in)
      smog$censored.only <- ifelse((smog$pm2.5.inClean - smog$modelled) > 20, "censored","uncensored")
      smog$fit.error <- (smog$censored - smog$modelled)^2
      count.censored <- as.data.table(table(smog$censored.only))
      colnames(count.censored) <- c("censored","count")
      count.censored <- count.censored[which(count.censored$censored == "censored"),]
      count.censored$aif <- AIF[aif]
      count.censored$HouseID<- unique(smog$HouseID)
      count.censored.list[[k]] <- count.censored
      
      #### optimise the model parameters AIF and corr.factor based on rmse#####
      vectorb <- smog$censored ## censored original data to compare against ###
      ind <- lag(smog$censored) ### previous value in the model for initial conc. ###
      out <- smog$pm2.5corr.out### outdoor odin values ####
      
      ### perform optimisation #####
      results <- optim(c(AIF[aif], 0.9), fn)
      fit1 <- data.frame(HouseID = smog$HouseID[1],
                         InterventionType = smog$InterventionType[1],
                         initial.AIF = AIF[aif],
                         optim.AIF = results$par[1],
                         optim.corr.factor = results$par[2])
      
      fit.params <- rbind(fit.params,fit1)
      
      ### re-modelling using optimised values #####
      smog$optim.model <- rep(NA,nrow(smog))
      smog$optim.model[firstNonNA] <-smog$pm2.5.inClean[firstNonNA]
      
      ### column with new factor loaded ###
      for (j in (firstNonNA+1):nrow(smog)){
        base.value <- smog$optim.model[(j-1)]
        out.value <- smog$pm2.5corr.out[(j-1)]
        # smog$optim.model[j] <- results$par[2]*(base.value + results$par[1]*(out.value - base.value))
        smog$optim.model[j] <-base.value + results$par[1]*((results$par[2]*out.value) - base.value)
      }
      
      smog$original.aif <- rep(AIF[aif], nrow(smog))
      smog$original.corr.factor <- rep(corr.factor, nrow(smog))
      smog$optim.aif <- rep(results$par[1], nrow(smog))
      smog$optim.corr.fac <- rep(results$par[2], nrow(smog))
      
      p1 <- ggplot(smog) +
        geom_line(aes(date, pm2.5.inClean, color = "Actual"), size = 2, alpha = 0.8) +
        geom_line(aes(date, modelled, color = "Modelled"), size = 1.5) +
        geom_line(aes(date, optim.model, color = "Optimized Model"), size = 0.9) +
        scale_color_manual(labels = c("Actual", "Modelled", "Optimized Model"),
                           values = c("grey","black", "red")) +
        scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10), name = "PM2.5") +
        theme_bw() +
        ggtitle(paste("HouseID & Intervention:", smog$HouseID[1],smog$InterventionType[1],": Initial AIF is",
                      AIF[aif],": \nOptimised AIF is",
                      round(unique(fit1$optim.AIF),3), "\nCorrection Factor is",
                      round(unique(fit1$optim.corr.factor)[1],3)))
      print(p1)
      # 
      ## initiate the count for list attributes
      indoor.list[[k]] <- smog
      k <- k+1
    }
    
    print(i)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

dev.off()


fit.params <- fit.params[-1,]

fit.params <- fit.params[which(fit.params$InterventionType == "Baseline"),]
write.csv(fit.params, "SMOGS_parameters_baseline.csv", row.names = F)

ggplot(fit.params) +
  geom_col(aes(factor(HouseID), optim.AIF, fill= InterventionType), 
           position = "dodge") + theme_bw()

count.censored <- rbindlist(count.censored.list)
all.smog.models <- rbindlist(indoor.list)

infiltratedPM <- all.smog.models %>% group_by(HouseID, InterventionType) %>%
  summarise(infiltration.PM = mean(optim.model, na.rm = T),
            aif = mean(optim.aif, na.rm = T))


ggplot(infiltratedPM) +
  geom_point(aes(aif,infiltration.PM, color = InterventionType), size = 3) +
  theme_bw()


####### peak and decay detections #####

### peaks #####
## peak function ####
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

## 

PDFfile <- paste0(path,"SMOG2018_indoorpeakdetection_HEPA.pdf")
pdf(file=PDFfile, paper = "USr", width = 28)
for( i in 1:length(smog.list)) {
  smog <- smog.list[[i]]
  smog <- smog[which(smog$InterventionType == "HEPA"),]
  peaks <- peakdet(smog$pm2.5corr.in, 25, smog$date)
  if(!is.null(peaks$maxtab)) {
    plot(smog$pm2.5corr.in~smog$date, type = "l",
         main = paste("ID: House", unique(smog$HouseID), "(HEPA weeks)"))
    abline(h = 30)
    points(peaks$maxtab, col = "red")
    # points(peaks$mintab, col = "blue")
    print(i)
  } else {
    print("No indoor peaks detected")
  }
  
}

dev.off()






#### plotting indoor and outdoor data per house  per intervention ####

PDFfile <- paste0(path,"SMOG2018_indooroutdoor_Baseline.pdf")
pdf(file=PDFfile, paper = "USr", width = 28)
for( i in 1:length(smog.list)) {
  tryCatch({
  smog <- smog.list[[i]]
  smog <- smog[which(smog$InterventionType == "Baseline"),]
  
  p1 <- ggplot(smog) +
    geom_line(aes(date, pm2.5corr.in, color = "Indoor")) +
    geom_line(aes(date,pm2.5corr.out, color = "Outdoor")) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d-%b") +
    scale_y_continuous(limits = c(0,200), name = "PM2.5") +
    ggtitle(paste("ID: House", unique(smog$HouseID), "(Baseline weeks)"))+
    theme_bw()
  print(p1)
  print(i)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

dev.off()



#####

PDFfile <- paste0(path,"SMOG2018_indooroutdoorRatios.pdf")
pdf(file=PDFfile, paper = "USr", width = 28)
for( i in 1:length(smog.list)) {
  tryCatch({
    smog <- smog.list[[i]]
    smog$iodiff <- (smog$pm2.5corr.out - smog$pm2.5corr.in)/smog$pm2.5corr.out
    smog$iodiff <- ifelse(smog$iodiff <0, NA, smog$iodiff)
    p1 <- ggplot(smog) +
      geom_boxplot(aes(x = InterventionType,y = iodiff, group = InterventionType), outlier.shape = NA) +
      ggtitle(paste("ID: House", unique(smog$HouseID))) +
      scale_y_continuous(limits = c(0,1)) +
      theme_bw()
    print(p1)
    print(i)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

dev.off()


