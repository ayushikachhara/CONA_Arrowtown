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
head(smog)
smog$VISIT_DATE_DD_MM_YYYY <-  parse_date_time(smog$VISIT_DATE_DD_MM_YYYY, "dmY")
smog$START_TIME1 <-  parse_date_time(smog$START_TIME1, "Ymd HMS")
smog$END_TIME1 <-  parse_date_time(smog$END_TIME1, "Ymd HMS")
smog$START_TIME2 <-  parse_date_time(smog$START_TIME2, "Ymd HMS")
smog$END_TIME2 <-  parse_date_time(smog$END_TIME2, "Ymd HMS")
smog$DATE_TIME_ORIGINAL <-  parse_date_time(smog$DATE_TIME_ORIGINAL, "Ymd HMS")
smog$DATE_TIME_FIXED <-  parse_date_time(smog$DATE_TIME_ORIGINAL, "Ymd HMS")

smog.short <- smog[,c(1,2,4,5,20,21,22)]
colnames(smog.short) <- c("houseID","visitID","in_out","UNIT_ID","datetime","variableID","value")
# smog.short$month <- month(smog.short$datetime)
# smog.short$day <-day(smog.short$datetime)
# ggplot(smog.short[which(smog.short$houseID %in% c(1,2) &
#                           smog.short$day %in% c(10,11) &
#                           smog.short$month == 7 &
#                           smog.short$in_out == "OUT" &
#                           smog.short$variableID == "PM2.5"),]) +
#   geom_line(aes(datetime,value, color = factor(houseID))) +
#   scale_y_continuous(limits = c(0,500))

smog.in <- smog.short[which(smog.short$in_out == "IN"),]
smog.out <- smog.short[which(smog.short$in_out == "OUT"),]

## indoor ###
smog.wide.in <- reshape(smog.in, idvar = c("houseID","visitID","in_out", "UNIT_ID", "datetime"), 
                           timevar = "variableID", direction = "wide")

colnames(smog.wide.in)[c(6:10)] <- c("pm1","pm2.5","pm10","temperature","humidity")


smog.wide.in$datetime <- round_date(smog.wide.in$datetime, unit = "minute")
## outdoor ##
smog.wide.out <- reshape(smog.out, idvar = c("houseID","visitID","in_out","UNIT_ID", "datetime"), 
                        timevar = "variableID", direction = "wide")

colnames(smog.wide.out)[c(6:10)] <- c("pm1","pm2.5","pm10","temperature","humidity")

smog.wide.out$datetime <- round_date(smog.wide.out$datetime, unit = "minute")

### applying corrections ####
# smog.wide.in <- merge(smog.wide.in, corrections, by = "UNIT_ID", all.x = T)
# smog.wide.in$pm2.5corr.in <- ifelse(!is.na(smog.wide.in$PM25_EPA.slope),
#                                  ((smog.wide.in$PM25_EPA.slope*smog.wide.in$pm2.5) + smog.wide.in$PM25_EPA.intrcpt),
#                                  smog.wide.in$pm2.5)

smog.wide.in$pm2.5corr.in <- smog.wide.in$pm2.5

# smog.wide.out <- merge(smog.wide.out, corrections, by = "UNIT_ID", all.x = T)
# smog.wide.out$pm2.5corr.out <- ifelse(!is.na(smog.wide.out$PM25_EPA.slope),
#                                  ((smog.wide.out$PM25_EPA.slope*smog.wide.out$pm2.5) + smog.wide.out$PM25_EPA.intrcpt),
#                                  smog.wide.out$pm2.5)

smog.wide.out$pm2.5corr.out <- smog.wide.out$pm2.5

smog.all <- merge(smog.wide.in,smog.wide.out, by = c("houseID","datetime"),
                  suffixes = c('.in','.out'), all = T)

smog.all <- smog.all[,-c(4,13)]
# smog.all$pm2.5corr.in<- ifelse(smog.all$pm2.5corr.in <0, NA, smog.all$pm2.5corr.in)
# smog.all$pm2.5corr.out <- ifelse(smog.all$pm2.5corr.out <0, NA, smog.all$pm2.5corr.out)
smog.all$month <- month(smog.all$datetime)
colnames(smog.all)[2] <- "date"

smog.all$Visitno <- ifelse(is.na(smog.all$visitID.in),smog.all$visitID.out,smog.all$visitID.in)

smog.all$houseID <- as.factor(smog.all$houseID)
smog.all$Visitno <- as.factor(smog.all$Visitno)

smog10min <- timeAverage(smog.all, avg.time = "10 min", type = c("houseID", "Visitno"))
smog10min <- smog10min[,-c(4,12)]

p0 <- ggplot(smog10min[which(smog10min$month == 7),]) +
  geom_line(aes(date, pm2.5.out, color = factor(houseID))) +
  scale_y_continuous(limits = c(0,500))
p0
# ggplotly(p0)
write.csv(smog.all, "smog_all_AK.csv")
write.csv(smog10min,"smog10min_AK.csv", row.names = F)


## infiltration model, peak detection, etc.

smog10min <- read.csv("smog10min_AK.csv", stringsAsFactors = F)
smog10min$date <- ymd_hms(smog10min$date)
smog10min$time <- as.POSIXct(format(smog10min$date, format = "%H:%M"), format = "%H:%M")
smog10min$dateonly <- as.Date(smog10min$date)


### per home infiltration model ####

smog.all <- smog10min[which(!(smog10min$houseID %in% c(12,16,18))),]
smog.list <- split(smog.all, smog.all$houseID)
indoor.list <- list()
count.censored.list <- list()
### function to calculate sum of square of errors ####
fn <- function(par) {
  x <- par[1]
  y <- par[2]
  
  vectorc <- y*(ind + x*(out - ind))
  return(sqrt(sum((vectorb - vectorc)^2, na.rm = T)))
}
# fn(c(0.2,0.99))

fit.params <- data.frame(houseID = NA,
                         Visitno = NA,
                         initial.AIF = NA,
                         optim.AIF = NA,
                         optim.corr.factor = NA)

AIF <- 0.02# seq(0.02,0.2, 0.01) ## Air Infiltration Factor per 10 minutes
aif <- 1
## initiate list count
k <- 1
i = 2
# v = 1
# ### PDF path ###
PDFfile <- paste0(path,"Models_Visitno.pdf")
pdf(file=PDFfile, paper = "USr", width = 28)

for (i in 1:length(smog.list)) {
  tryCatch({
    smog.cur <- smog.list[[i]]
    visit.list <- split(smog.cur, smog.cur$Visitno)
    for(v in 1:length(visit.list)) {
      corr.factor <- 0.9 ## correction factor for differences between two dust sensors
      smog <- visit.list[[v]] ## import current house
      smog <- smog[complete.cases(smog$pm2.5.out),] ### model only data with outdoor data available
      ## picking a clean dataset for modelling
      smog$pm2.5.inClean <-ifelse(smog$pm2.5.in>100, NA, smog$pm2.5.in)
      smog$io.ratio <- smog$pm2.5.in/ smog$pm2.5.out ## I/O ratios
      
      NonNAindex <- which(!is.na(smog$pm2.5.inClean))
      firstNonNA <- min(NonNAindex)
      ## start the modelling with one indoor value in the beginning, pre-defined AIF and corr.factor ###
      smog$modelled <- rep(NA, nrow(smog))
      smog$modelled[firstNonNA] <- smog$pm2.5.inClean[firstNonNA]
      ### column with initial concentrations loaded ###
      for (j in (firstNonNA+1):nrow(smog)){
        base.value <- smog$modelled[(j-1)]
        out.value <- smog$pm2.5.out[j]
        smog$modelled[j] <- corr.factor*(base.value + AIF[aif]*(out.value - base.value))
      }
      
      ### censor all indoor sources by setting an arbitrary threshold ####
      smog$censored <- ifelse((smog$pm2.5.inClean - smog$modelled) > 20, NA,smog$pm2.5.in)
      smog$censored.only <- ifelse((smog$pm2.5.inClean - smog$modelled) > 20, "censored","uncensored")
      smog$fit.error <- (smog$censored - smog$modelled)^2
      count.censored <- as.data.table(table(smog$censored.only))
      colnames(count.censored) <- c("censored","count")
      count.censored <- count.censored[which(count.censored$censored == "censored"),]
      count.censored$aif <- AIF[aif]
      count.censored$houseID<- unique(smog$houseID)
      count.censored.list[[k]] <- count.censored
      
      #### optimise the model parameters AIF and corr.factor based on rmse#####
      vectorb <- smog$censored ## censored original data to compare against ###
      ind <- lag(smog$censored) ### previous value in the model for initial conc. ###
      out <- smog$pm2.5.out### outdoor odin values ####
      
      ### perform optimisation #####
      results <- optim(c(AIF[aif], 0.9), fn)
      fit1 <- data.frame(houseID = smog$houseID[1],
                         Visitno = smog$Visitno[1],
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
        out.value <- smog$pm2.5.out[j]
        smog$optim.model[j] <- results$par[2]*(base.value + results$par[1]*(out.value - base.value))
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
        ggtitle(paste("HouseID & Intervention:", smog$HouseID[1],smog$Visitno[1],": Initial AIF is",
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


write.csv(fit.params, "firstguessmodel_visitno.csv", row.names = F)

ggplot(fit.params) +
  geom_col(aes(factor(houseID), optim.AIF, fill= Visitno), 
           position = "dodge") + theme_bw()

count.censored <- rbindlist(count.censored.list)
all.smog.models <- rbindlist(indoor.list)

infiltratedPM <- all.smog.models %>% group_by(houseID) %>%
  summarise(infiltration.PM = mean(optim.model, na.rm = T),
            aif = mean(optim.aif, na.rm = T))


