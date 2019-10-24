library(gghighlight)

smogodin <- smogodin[,c(1,2,8,14,16,17,18)]

smogodin$hour <- hour(smogodin$date)

smogodin$daytime <- ifelse(smogodin$hour>=6 & smogodin$hour<12, "Morning",
                         ifelse(smogodin$hour>=12 & smogodin$hour<18, "Afternoon",
                                ifelse(smogodin$hour>=18, "Evening","Night")))

smogodin$InterventionType[which(smogodin$InterventionType == "HEPA2")] <- "HEPA"
smogodin$InterventionType <- factor(smogodin$InterventionType,
                                           levels=c("Baseline", "HEPA", "Electrostatic "))
smogodin$StartDate <- as.Date(smogodin$StartDate, format = "%d/%m/%Y", tz = tz(smogodin$date))
smogodin$EndDate <- as.Date(smogodin$EndDate, format = "%d/%m/%Y", tz = tz(smogodin$date))

i = 1

allhouseids <- unique(smogodin$HouseID)

for(i in 1:length(allhouseids)) {
  tryCatch({
  cur.house <- allhouseids[i]
  cur.hh <- smogodin %>% filter(HouseID == cur.house)
  
  ## define baseline interval ####
  b.interval <- interval(unique(cur.hh$StartDate[which(cur.hh$InterventionType == "Baseline")]), 
                         unique(cur.hh$EndDate[which(cur.hh$InterventionType == "Baseline")])[1])
  
  ## baseline.averages ###
  base.sub <- smogodin %>% filter(InterventionType == "Baseline" & 
                                    date %within% b.interval)
  base.sub <- base.sub %>% group_by(HouseID) %>%
    mutate(count = n())
  buse.sub <- base.sub %>% filter(count >576)
  
  baseline.df <- base.sub %>% group_by(HouseID, InterventionType, daytime) %>%
    summarise_at(.vars = c('pm2.5corr.in', 'pm2.5corr.out'), mean, na.rm = T)
  
 
  baseline.df$daytime <- factor(baseline.df$daytime, 
                                levels=c("Night", "Morning", "Afternoon", "Evening"))
  p1 <- ggplot(baseline.df) +
    geom_point(aes(daytime, pm2.5corr.in,color = HouseID),
               size = 12,color = "firebrick2", fill = "firebrick2") +
    gghighlight(HouseID == cur.house, 
                label_key = HouseID,use_direct_label = FALSE) +
    labs(x = "Time of the day",
         y = "Indoor PM2.5",
         title = paste("House number",cur.house,":",base.sub$InterventionType[1], "comparisons between",
                        format(cur.hh$StartDate, "%d-%b"), "and",
                       format(cur.hh$EndDate, "%d-%b"), "2018")) +
    theme_bw()
  print(p1)
  print(i)
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

dev.off()  