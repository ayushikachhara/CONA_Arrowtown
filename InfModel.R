### import libraries ##
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
library(rgeos) ## gDistance function is from this packages
library(gstat)
library(ggmap)
library(gganimate)
library(gifski)
library(curl)
library(googledrive)
library(ggforce)


### function to calculate sum of square of errors ####
fn <- function(par) {
  x <- par[1]
  y <- par[2]
  
  vectorc <- ind + x*((y*out) - ind)
  return(sqrt(sum((vectorb - vectorc)^2, na.rm = T)))
}

fit.params <- data.frame(HouseID = NA,
                         Phase = NA,
                         initial.AIF = NA,
                         optim.AIF = NA,
                         optim.corr.factor = NA,
                         countpoints = NA,
                         points.removed = NA,
                         indoor.avgPM2.5 = NA,
                         indoor.avgPM2.5censored = NA,
                         indoor.avg.modelled = NA,
                         outdoor.avgPM2.5 = NA)


AIF <- 0.04# seq(0.02,0.2, 0.01) ## Air Infiltration Factor per 10 minutes
aif <- 1
## initiate list count
k <- 1

corr.factor <- 0.9 ## correction factor for differences between two dust sensors

# # # ### PDF path - per home ####
PDFfile <- paste0("S:/kachharaa/CONA/Arrowtown2019/hauhau/InfiltrationModels_Phase4_201901002.pdf")
pdf(file=PDFfile, paper = "special", width = 8.2, height = 11.6)
setwd("S:/kachharaa/CONA/Arrowtown2019/hauhau/phase4/")
hh.odin <- read.csv("hh-odin-P4_20190923.csv", stringsAsFactors = F)
cur.phase = "P04"

# colnames(hh.odin)
hh.odin$date <- ymd_hms(hh.odin$date)
hh.odin$hour <- hour(hh.odin$date)

allhouseids <- unique(hh.odin$HouseID)
allhouseids <- allhouseids[which(!is.na(allhouseids))] ## removing NAs

modelledhomes <- list() ## initiate an empty list ###
house  = 12

for(house in 1:length(allhouseids)){
  tryCatch({
    cur.house <- allhouseids[house]
    cur.hh <- hh.odin %>% filter(HouseID == cur.house)
    
    ## average of outdoor records (4 nearest ODINs)
    cur.hh$com.odin.PM2.5 <- rowMeans(cur.hh[,c("p.odin.PM2.5","s.odin.PM2.5",
                                                "t.odin.PM2.5","q.odin.PM2.5")], na.rm = T)
    cur.hh$com.odin.PM10 <- rowMeans(cur.hh[,c("p.odin.PM10","s.odin.PM10",
                                               "t.odin.PM10","q.odin.PM10")], na.rm = T)
    cur.hh$com.odin.Temperature <- rowMeans(cur.hh[,c("p.odin.Temperature","s.odin.Temperature",
                                                      "t.odin.Temperature","q.odin.Temperature")], na.rm = T)
    
    
    cur.hh <- cur.hh[complete.cases(cur.hh$com.odin.PM2.5),] ### model only data with outdoor data available
    
    cur.hh$io.ratio <- cur.hh$PM2.5/ cur.hh$com.odin.PM2.5 ## I/O ratios
    ## picking a clean dataset for modelling
    cur.hh$pm2.5.inClean <-ifelse(cur.hh$PM2.5>50, NA, cur.hh$PM2.5)
    
    NonNAindex <- which(!is.na(cur.hh$pm2.5.inClean)) ## all non-NA indices
    firstNonNA <- min(NonNAindex) ## get the first one to start modelling
    
    ## start the modelling with one indoor value in the beginning, pre-defined AIF and corr.factor ###
    cur.hh$modelled <- rep(NA, nrow(cur.hh))
    cur.hh$modelled[firstNonNA] <- cur.hh$pm2.5.inClean[firstNonNA]
    
    ### column with initial concentrations loaded ###
    for (j in (firstNonNA+1):nrow(cur.hh)){
      base.value <- cur.hh$modelled[(j-1)]
      out.value <- cur.hh$com.odin.PM2.5[(j)] ### CHANGE IT ALL OVER
      # cur.hh$modelled[j] <- corr.factor*(base.value + AIF[aif]*(out.value - base.value))
      cur.hh$modelled[j] <- base.value + AIF[aif]*((corr.factor*out.value) - base.value)
    }
    
    ### censor all indoor sources by setting an arbitrary threshold ####
    cur.hh$censored <- ifelse((cur.hh$pm2.5.inClean - cur.hh$modelled) > 20, NA,cur.hh$PM2.5)
    cur.hh$censored.only <- ifelse((cur.hh$pm2.5.inClean - cur.hh$modelled) > 20, "censored","uncensored")
    cur.hh$fit.error <- (cur.hh$censored - cur.hh$modelled)^2
    
    #### optimise the model parameters AIF and corr.factor based on rmse#####
    vectorb <- cur.hh$censored ## censored original data to compare against ###
    ind <- lag(cur.hh$censored) ### previous value in the model for initial conc. ###
    out <- cur.hh$com.odin.PM2.5### outdoor odin values ####
    
    ### perform optimisation #####
    results <- optim(c(AIF[aif], 0.9), fn)
    
    ### re-modelling using optimised values #####
    cur.hh$optim.model <- rep(NA,nrow(cur.hh))
    cur.hh$optim.model[firstNonNA] <-cur.hh$pm2.5.inClean[firstNonNA]
    
    ### column with new factor loaded ###
    for (j in (firstNonNA+1):nrow(cur.hh)){
      base.value <- cur.hh$optim.model[(j-1)]
      out.value <- cur.hh$com.odin.PM2.5[(j)]
      # cur.hh$optim.model[j] <- results$par[2]*(base.value + results$par[1]*(out.value - base.value))
      cur.hh$optim.model[j] <-base.value + results$par[1]*((results$par[2]*out.value) - base.value)
    }
    
    cur.hh$original.aif <- rep(AIF[aif], nrow(cur.hh))
    cur.hh$original.corr.factor <- rep(corr.factor, nrow(cur.hh))
    cur.hh$optim.aif <- rep(results$par[1], nrow(cur.hh))
    cur.hh$optim.corr.fac <- rep(results$par[2], nrow(cur.hh))
    cur.hh$date <- ymd_hms(cur.hh$date)
    cur.hh$week <- isoweek(cur.hh$date)
    cur.hh$weekno <- cur.hh$week - (min(cur.hh$week)) +1
    
    cur.hh <- cur.hh %>% group_by(weekno) %>%
      mutate(recordsperweek = n())
    
    cur.hh <- cur.hh %>% filter(recordsperweek>=288)
    
    fit1 <- data.frame(HouseID = cur.house,
                       Phase = cur.phase,
                       initial.AIF = AIF[aif],
                       optim.AIF = results$par[1],
                       optim.corr.factor = results$par[2],
                       countpoints = sum(!is.na(cur.hh$PM2.5)),
                       points.removed = sum(is.na(cur.hh$censored)),
                       indoor.avgPM2.5 = mean(cur.hh$PM2.5, na.rm = T),
                       indoor.avgPM2.5censored = mean(cur.hh$censored, na.rm = T),
                       indoor.avg.modelled = mean(cur.hh$optim.model, na.rm = T),
                       outdoor.avgPM2.5 = mean(cur.hh$com.odin.PM2.5, na.rm = T))
    
    fit.params <- rbind(fit.params,fit1)
    
    p1 <- ggplot(cur.hh) +
            # geom_line(aes(date,PM2.5, color = "Indoor"), size = 1, linetype = "dashed") +
      geom_line(aes(date,PM2.5, color = "Clean Indoors"), 
                size = 1) +
      geom_line(aes(date, com.odin.PM2.5, color = "Outdoor"), 
                size = 1, alpha = 0.4) +
      geom_line(aes(date, optim.model, color = "Optimised model"), 
                size = 1.2) +
      scale_color_manual(labels = c("Indoor Observed",
                                    "Outdoor contribution to Indoor","Outdoor"),
                         values = c("black", "red","blue")) +
      scale_y_continuous(limits = c(0,300), breaks = seq(0,300,50), name = "PM2.5") +
      theme_bw() #+
      # facet_wrap(.~weekno,  scales = "free_x", nrow = length(unique(cur.hh$weekno))) +
      # ggtitle(paste("HouseID:",cur.house,"\nOptimised AIF is",
      #               round(unique(fit1$optim.AIF),3), "Correction Factor is",
      #               round(unique(fit1$optim.corr.factor)[1],3)))
    print(p1)
    
    ggplot(cur.hh) + 
      geom_point(aes(censored, optim.model)) + theme_bw()
    
    print(house)
    modelledhomes[[house]] <- cur.hh ## output to a list
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
dev.off()

write.csv(fit.params, "S:/kachharaa/CONA/Arrowtown2019/hauhau/InfModel_summaryreport_P1toP4.csv",
          row.names = F)

# allmodelledhomes.df <- rbind_list(modelledhomes)
# write.csv(allmodelledhomes.df, "S:/kachharaa/CONA/Arrowtown2019/hauhau/Inf_modelled_P4.csv",
#           row.names = F)

# ggplot(fit.params,aes(indoor.avgPM2.5/outdoor.avgPM2.5,indoor.avg.modelled)) +
#   geom_point()  + theme_bw() +
#   labs(x = "I/O ratio", y = "Indoor_modelled", 
#        title = "Infiltrated Indoor PM2.5 versus I/O ratio")
# 
# ggplot(fit.params,aes((countpoints - points.removed), optim.AIF)) +
#   geom_point()  + theme_bw() +
#   labs(x = "Number of points", y = "AIF", 
#        title = "Infiltrated Indoor PM2.5 versus I/O ratio")
# 
# ggplot(fit.params %>% filter(countpoints >1000),
#        aes(outdoor.avgPM2.5, 
#            (100*((indoor.avgPM2.5-indoor.avg.modelled)/indoor.avgPM2.5)))) +
#   geom_point()  + theme_bw()+ geom_smooth(se = F, method = "lm") +
#   labs(x = "Outdoor PM", y = "Indoor origin (%)", 
#        title = "")
# 
# 
# fit.params$perc.indoororigin <- 100*(fit.params$indoor.avgPM2.5 - fit.params$indoor.avg.modelled)/ fit.params$indoor.avgPM2.5
# fit.params$perc.outdoororigin <- 100*fit.params$indoor.avg.modelled/fit.params$indoor.avgPM2.5
# 
# fit.params2 <- fit.params %>% filter(perc.indoororigin >0)
# 
# ggplot(fit.params2, aes(outdoor.avgPM2.5,perc.outdoororigin)) +
#   geom_point() +
#   theme_bw() + geom_smooth(method = "lm", se = F) 
  
