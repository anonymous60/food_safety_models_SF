


getCrimeSummary <- function(fs, distance = .15) {
  
  b1 <- c()
  c1 <- c()
  
  fs$crimePastYear <- NA_integer_ 
  fs$crimePast6mo <- NA_integer_ 
  fs$crimePast90d <- NA_integer_   
  fs$crimePastWeek <- NA_integer_ 
  fs$arrestsPastYear <- NA_integer_ 
  fs$arrestsPast6mo <- NA_integer_ 
  fs$arrestsPast90d <- NA_integer_   
  fs$arrestsPastWeek <- NA_integer_
  fs$burgsPastYear <- NA_integer_ 
  fs$burgsPast6mo <- NA_integer_ 
  fs$burgsPast90d <- NA_integer_   
  fs$burgsPastWeek <- NA_integer_
  
  for (i in 1:nrow(fs)) {
    # Get the inspection ID to process.
    thisInsp <- fs[i,]
    inspection <- thisInsp$inspection_id  
    inspectDate <- as.POSIXct(thisInsp$inspection_date)

    print("===========")
    print(paste("Loop:", i, "of", nrow(fs), 
                prettyNum(round(i / nrow(fs) * 100, digits = 1)),"%"))
    print("Processing inspection:")
    print(inspection)
    print(inspectDate)    
    print("===========")
    
    # Get the lat/long for the business being inspected.  
    b1 <- data.frame(lng = thisInsp$business_longitude, 
                     lat = thisInsp$business_latitude)
    
    if (!is.na(b1$lng) && !is.na(b1$lat)) {
      
      # Find all relevant crime data leading up to the inspection date.
      cd <- crime4[as.POSIXct(crime4$IncidentDate) <= inspectDate, ]
      
      # Get the incident geo coordinates.    
      c1 <- data.frame(lng = cd$Long, lat = cd$Lat)
      
      # Calculate the distance between the business and crime incidents. 
      m <- distm(b1, c1, fun = distHaversine) / 1609.34
      
      # Only use crimes that are within the specified distance. 
      inRange <- which(m <= distance)
      cd2 <- cd[inRange, ]
  
      # Summarize relevant incidents using a rolling year, six month, 
      #  and week intervals anchored to the incident date.
      cd3 <- cd2 %>%
        group_by(IncidentDate) %>%
        summarise(incidents = sum(Incidents),
                  arrests = sum(Arrests),
                  burgs = sum(Burgs)) %>% 
        mutate(inLastYear = if_else(difftime(inspectDate, IncidentDate, units = "days") < 365, 1, 0),
               inLast6Months = if_else(difftime(inspectDate, IncidentDate, units = "days") < 181, 1, 0),
               inLast90days = if_else(difftime(inspectDate, IncidentDate, units = "days") < 90, 1, 0),               
               inLastWeek = if_else(difftime(inspectDate, IncidentDate, units = "days") < 7, 1, 0)) %>%
        ungroup() %>%
        summarise(incidents1Yr = sum(if_else(inLastYear == 1, incidents, 0L)),
                  incidents6Mo = sum(if_else(inLast6Months == 1, incidents, 0L)),
                  incidents90d = sum(if_else(inLast90days == 1, incidents, 0L)),                  
                  incidents1Wk = sum(if_else(inLastWeek == 1, incidents, 0L)),
                  arrests1Yr = sum(if_else(inLastYear == 1, arrests, 0L)),
                  arrests6Mo = sum(if_else(inLast6Months == 1, arrests, 0L)),
                  arrests90d = sum(if_else(inLast90days == 1, arrests, 0L)),                  
                  arrests1Wk = sum(if_else(inLastWeek == 1, arrests, 0L)),
                  burgs1Yr = sum(if_else(inLastYear == 1, burgs, 0L)),
                  burgs6Mo = sum(if_else(inLast6Months == 1, burgs, 0L)),
                  burgs90d = sum(if_else(inLast90days == 1, burgs, 0L)),                  
                  burgs1Wk = sum(if_else(inLastWeek == 1, burgs, 0L))
                  )           
      
      # Update the food inspection table with the resulting crime attributes. 
      fs[fs$inspection_id == inspection, "crimePastYear"] <- cd3$incidents1Yr 
      fs[fs$inspection_id == inspection, "crimePast6mo"] <- cd3$incidents6Mo 
      fs[fs$inspection_id == inspection, "crimePast90d"] <- cd3$incidents90d       
      fs[fs$inspection_id == inspection, "crimePastWeek"] <- cd3$incidents1Wk 
      
      fs[fs$inspection_id == inspection, "arrestsPastYear"] <- cd3$arrests1Yr 
      fs[fs$inspection_id == inspection, "arrestsPast6mo"] <- cd3$arrests6Mo 
      fs[fs$inspection_id == inspection, "arrestsPast90d"] <- cd3$arrests90d       
      fs[fs$inspection_id == inspection, "arrestsPastWeek"] <- cd3$arrests1Wk  
      
      fs[fs$inspection_id == inspection, "burgsPastYear"] <- cd3$burgs1Yr 
      fs[fs$inspection_id == inspection, "burgsPast6mo"] <- cd3$burgs6Mo 
      fs[fs$inspection_id == inspection, "burgsPast90d"] <- cd3$burgs90d       
      fs[fs$inspection_id == inspection, "burgsPastWeek"] <- cd3$burgs1Wk  
      
    } # end for loop
  } # end if 
  
  print("Done....!")
  print(paste("Processed:", i))
  return(fs)
}


