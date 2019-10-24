# setwd("Q:/AirQual/Research/CONA/Hobart/2018/BushfireStudy/")

# df <- read.csv("house 22 IL results.csv", 
#                stringsAsFactors = F)
df <- odin10min[which(odin10min$HouseID == 24),]
df$date <- dmy_hm(df$date)

df$pm2.5corr.in <- ifelse(df$pm2.5.in <= df$pm10.in, df$pm2.5.in, NA)
df$pm2.5corr.out <- ifelse(df$pm2.5.in <= df$pm10.in, df$pm2.5.out, NA)
h22 <- df#smog10min[which(smog10min$HouseID == 22),]
h22 <- h22[which(h22$InterventionType == "HEPA"),]
h22 <- h22[,c(1,2,8,14,15,16)]
# h22$dateOnly <- as.Date(h22$date)
# h22$grouping <- cut(h22$dateOnly, 4, labels = F)
# h22 <- merge(df,h22, by = "date", all = T)

## peak detection #####
# peakdet <- function(v, delta, x = NULL) {
#   maxtab <- NULL
#   mintab <- NULL
#   
#   if (is.null(x))
#   {
#     x <- seq_along(v)
#   }
#   
#   if (length(v) != length(x))
#   {
#     stop("Input vectors v and x must have the same length")
#   }
#   
#   if (!is.numeric(delta))
#   {
#     stop("Input argument delta must be numeric")
#   }
#   
#   if (delta <= 0)
#   {
#     stop("Input argument delta must be positive")
#   }
#   
#   mn <- Inf
#   mx <- -Inf
#   
#   mnpos <- NA
#   mxpos <- NA
#   
#   lookformax <- TRUE
#   
#   for(i in seq_along(v))
#   {
#     this <- v[i]
#     
#     if (this > mx & !is.na(this))
#     {
#       mx <- this
#       mxpos <- x[i]
#     }
#     
#     if (this < mn & !is.na(this))
#     {
#       mn <- this
#       mnpos <- x[i]
#     }
#     
#     if (lookformax & !is.na(this))
#     {
#       if ((this < mx - delta) & (!is.na(this)))
#       {
#         maxtab <- rbind(maxtab, data.frame(pos = mxpos, val = mx))
#         
#         mn <- this
#         mnpos <- x[i]
#         
#         lookformax <- FALSE
#       }
#     }
#     else
#     {
#       if ((this > mn + delta) & (!is.na(this)))
#       {
#         mintab <- rbind(mintab, data.frame(pos = mnpos, val = mn))
#         
#         mx <- this
#         mxpos <- x[i]
#         
#         lookformax <- TRUE
#       }
#     }
#   }
#   
#   list(maxtab = maxtab, mintab = mintab)
# }
# 
# df$date <- ymd_hms(df$date)
# df10min <- timeAverage(df, avg.time = "10 min", type = "InterventionType")
# df <- df10min
# df$jump <- c(NA, abs(diff(df$pm2.5corr.in)))
# df$pm2.5corr.in2 <- ifelse(df$jump >50 | is.na(df$pm2.5corr.in), NA, df$pm2.5corr.in)
# peaks <- peakdet(df$pm2.5corr.in, 10, df$date)
# 
# peaks.df <- peaks$maxtab
# 
# colnames(peaks.df) = c("date", "peakvalue")
# df <- merge(df,peaks.df, by = "date", all = T)
# 
# plot(df$pm2.5corr.in~df$date, type = "l", col = "blue", 
#      main = "All peaks detected")
# points(df$date, df$peakvalue)

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
# v = 2

# ### PDF path ###
# PDFfile <- paste0(path,"Models_InterventionType_SMOG_newmodel02052019.pdf")
# pdf(file=PDFfile, paper = "USr", width = 28)
# dev.off()

smog.cur <- h22
visit.list <- split(smog.cur, smog.cur$grouping)
for(i in 1:length(visit.list)) {
  corr.factor <- 0.9 ## correction factor for differences between two dust sensors
  smog <- smog.cur#visit.list[[i]]
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
    out.value <- smog$pm2.5corr.out[(j)] ### CHAMGE IT ALL OVER
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
    out.value <- smog$pm2.5corr.out[(j)]
    # smog$optim.model[j] <- results$par[2]*(base.value + results$par[1]*(out.value - base.value))
    smog$optim.model[j] <-base.value + results$par[1]*((results$par[2]*out.value) - base.value)
  }
  
  smog$original.aif <- rep(AIF[aif], nrow(smog))
  smog$original.corr.factor <- rep(corr.factor, nrow(smog))
  smog$optim.aif <- rep(results$par[1], nrow(smog))
  smog$optim.corr.fac <- rep(results$par[2], nrow(smog))
  smog$date <- ymd_hms(smog$date)
  
  p1 <- ggplot(smog) +
    geom_line(aes(date, pm2.5corr.in, color = "Actual"), size = 1, alpha = 0.9) +
    geom_line(aes(date, pm2.5corr.out, color = "Modelled"), size = 0.5 , alpha = 0.5) +
    geom_line(aes(date, optim.model, color = "Optimized Model"), size = 1) +
    scale_color_manual(labels = c("Actual Indoor","Actual Outdoor", "Optimised Model"),
                       values = c("grey", "blue", "red")) +
    scale_y_continuous(limits = c(0,30), breaks = seq(0,50,10), name = "PM2.5") +
    theme_bw() +
    ggtitle(paste("HouseID & Intervention:", smog$HouseID[1],smog$InterventionType[1],": Initial AIF is",
                  AIF[aif],": \nOptimised AIF is",
                  round(unique(fit1$optim.AIF),3), "\nCorrection Factor is",
                  round(unique(fit1$optim.corr.factor)[1],3)))
  print(p1)
}



corr.factor <- 0.9 ## correction factor for differences between two dust sensors
smog <- visit.list[[i]]
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
  out.value <- smog$pm2.5corr.out[(j)] ### CHAMGE IT ALL OVER
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
  out.value <- smog$pm2.5corr.out[(j)]
  # smog$optim.model[j] <- results$par[2]*(base.value + results$par[1]*(out.value - base.value))
  smog$optim.model[j] <-base.value + results$par[1]*((results$par[2]*out.value) - base.value)
}

smog$original.aif <- rep(AIF[aif], nrow(smog))
smog$original.corr.factor <- rep(corr.factor, nrow(smog))
smog$optim.aif <- rep(results$par[1], nrow(smog))
smog$optim.corr.fac <- rep(results$par[2], nrow(smog))
smog$date <- ymd_hms(smog$date)

p1 <- ggplot(smog) +
  geom_line(aes(date, pm2.5corr.in, color = "Actual"), size = 1, alpha = 0.8) +
  # geom_line(aes(date, modelled, color = "Modelled"), size = 1) +
  geom_line(aes(date, optim.model, color = "Optimized Model"), size = 1) +
  scale_color_manual(labels = c("Actual", "Optimised Model"),
                     values = c("grey", "red", "blue")) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,50,10), name = "PM2.5") +
  theme_bw() +
  ggtitle(paste("HouseID & Intervention:", smog$HouseID[1],smog$InterventionType[1],": Initial AIF is",
                AIF[aif],": \nOptimised AIF is",
                round(unique(fit1$optim.AIF),3), "\nCorrection Factor is",
                round(unique(fit1$optim.corr.factor)[1],3)))
p1

ggplot(smog) +
  geom_point(aes(optim.model,model.IN)) +
  theme_bw() +
  geom_abline(aes(slope = 1, intercept = 0), color = "red", size = 2)+
  labs(y = "Ian's Model", x = "Ayushi's Model", 
       title = "Ian = 1.09Ayushi + 0.1 || R-squared = 0.9889")

summary(lm(model.IN~optim.model, data = smog))


p2 <- ggplot(df[which(df$Visitno == 1),]) +
  geom_line(aes(date, pm2.5corr.out, color = "Outdoor"), size = 1) +
  geom_line(aes(date, pm2.5corr.in, color = "Indoor"), size = 1) +
  theme_bw() +
  ggtitle(paste("HouseID :", smog$houseID[1],"& Intervention:",df$InterventionType[1]))

ggplotly(p2)
  
baseline <- smog


smog <- df[which(df$InterventionType == "HEPA"),]
intervention <- smog#df[which(df$Visitno == 1),

smog <- intervention
smog <- smog[complete.cases(smog$pm2.5corr.out),] ### model only data with outdoor data available


corr.factor <- 0.257 ## correction factor for differences between two dust sensors
aif <- 0.115
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
  out.value <- smog$pm2.5corr.out[j]
  # smog$modelled[j] <- corr.factor*(base.value + AIF[aif]*(out.value - base.value))
  smog$modelled[j] <- base.value + aif*((corr.factor*out.value) - base.value)
}
smog$optim.model_AllBaseline <- smog$modelled
  

p3 <- ggplot(smog) +
  geom_line(aes(date, pm2.5corr.in, color = "Actual"), size = 1) +
  geom_line(aes(date, optim.model_group1, color = "Modelled1"), size = 1) +
  geom_line(aes(date, optim.model_group2, color = "Modelled2"), size = 1) +
  geom_line(aes(date, optim.model_group3, color = "Modelled3"), size = 1) +
  geom_line(aes(date, optim.model_group4, color = "Modelled4"), size = 1) +
  geom_line(aes(date, optim.model_AllBaseline, color = "Modelled5"), size = 1) +
  scale_color_manual(labels = c("Actual", "Group1",
                                "Group2","Group3",
                                "Group4","AllBaseline"),
                     values = c("grey","blue", "blue2","blue3",
                                "blue4","maroon")) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,20,1), name = "PM2.5") +
  theme_bw() +
  ggtitle(paste("HouseID & Intervention:", smog$HouseID[1],smog$InterventionType[1]))
p3

write.csv(smog, "./House24_modeltrials.csv", row.names = F)
