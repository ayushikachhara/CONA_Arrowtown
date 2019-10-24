fitparams <- read.csv("SMOGS_parameters_baseline.csv", stringsAsFactors = F)
fitparams <- fitparams[,c(1,4,5)]

h24 <- smogodin[which(smogodin$HouseID == "24"),]
smog.HEPA <- h24[which(h24$InterventionType == "HEPA"),]

smog.new <- smog.HEPA #merge(smog.HEPA, fitparams, by = "HouseID", all = T)

smog.list <- split(smog.new, smog.new$HouseID)
i = 1
v = 1
for(i in 1:length(smog.list)) {
  tryCatch({
  smog.cur <- smog.list[[i]]
  visit.list <- split(smog.cur, smog.cur$InterventionType)
  for(v in 1:length(visit.list)) {
    corr.factor <- unique(smog.cur$optim.sensitivity) ## correction factor for differences between two dust sensors
    aif <- unique(smog.cur$optim.AIF)
    
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
      out.value <- smog$pm2.5corr.out[j]
      # smog$modelled[j] <- corr.factor*(base.value + AIF[aif]*(out.value - base.value))
      smog$modelled[j] <- base.value + aif*((corr.factor*out.value) - base.value)
    }
    print(i)
    smog.list[[i]] <- smog
  }
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


smog.all.HEPA <- rbind_list(smog.list)

write.csv(smog.all.HEPA,"Allsmogs_baselineappliedtoHEPA_020519.csv", row.names = F)



smog.baseline <- smog.all[which(smog.all$InterventionType == "Baseline"),]

smog.baseline <- smog.baseline[which(smog.baseline$HouseID %in% fitparams$HouseID),]

smog.new <- merge(smog.baseline, fitparams, by = "HouseID", all = T)

smog.list <- split(smog.new, smog.new$HouseID)
i = 1
v = 1
for(i in 1:length(smog.list)) {
  tryCatch({
    smog.cur <- smog.list[[i]]
    visit.list <- split(smog.cur, smog.cur$InterventionType)
    for(v in 1:length(visit.list)) {
      corr.factor <- unique(smog.cur$optim.sensitivity) ## correction factor for differences between two dust sensors
      aif <- unique(smog.cur$optim.AIF)
      
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
        out.value <- smog$pm2.5corr.out[j]
        # smog$modelled[j] <- corr.factor*(base.value + AIF[aif]*(out.value - base.value))
        smog$modelled[j] <- base.value + aif*((corr.factor*out.value) - base.value)
      }
      print(i)
      smog.list[[i]] <- smog
    }
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


smog.all.baseline <- rbind_list(smog.list)
write.csv(smog.all.baseline,"Allsmogs_baseline_020519.csv", row.names = F)

smog.all.new <- rbind.data.frame(smog.all.baseline, smog.all.HEPA)


smog.summaries <- smog.all.new %>% group_by(HouseID,InterventionType) %>%
  summarise(pm2.5corr.in = mean(pm2.5corr.in,na.rm = T),
            pm2.5.inClean = mean(pm2.5.inClean, na.rm = T),
            modelled =mean(modelled, na.rm = T))


write.csv(smog.summaries, "smogsummaries_020519.csv", row.names = F)
