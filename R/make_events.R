#' A Function to Generate Precipitation Events
#'
#' This function allows you to isolate events from a record of rainfall.
#' @param df Name of data frame with precipitation data.
#' @param rain_col_name Name of column with rainfall data. Defaults to "rain_cm".
#' @param dateTime_col_name Name of the column with timestamps. Defaults to "dateTime".
#' @param mit_hours The time in hours to use as the minimum inter-event time. Defaults to 6.0.
#' @param ID an identifying word that will be used to create identifiers for events in combination with the mit number
#' @keywords rain
#' @export
#' @examples
#' make_events()



# function to generate precipitation events from timestamped precipitation data

# inputs:
# df: data frame of timestamped precipitation data, no NA values
# rain_col_name: name of column that rainfall amounts are stored in
# mit_hours: minimum interevent time criterion, in hours
# ID: character string to identify event ID names in summary table

# returns a list containing:
# 1. events_summary object: data frame with metrics for each event
# 2. precip_events_list: list of events named by eventID

make_events <- function(df=precip_data,
                                 rain_col_name="rain_cm",
                                 dateTime_col_name = "dateTime",
                                 mit_hours=6.0, 
                                 ID="ID"){
  
  # coerce to data frame
  df <- as.data.frame(df)
  # rename dateTime_col_name
  df$dateTime <- as.POSIXct(df[[dateTime_col_name]])
  # indicate whether any precip was recorded
  df$rain_indicator <- NA
  df$rain_indicator <- df[,rain_col_name]>0 
  
  # use rle() to determine number of consecutive rows of rain or no rain
  eventsdf <- as.data.frame(cbind(rle(df$rain_indicator)$lengths, rle(df$rain_indicator)$values))
  colnames(eventsdf) <- c("length", "indicator")
  
  # for each event determine the cumulative number of rows in df until the end of that event
  eventsdf$qLength <- cumsum(eventsdf$length)
  eventsdf$hours <- NA

  # calculate the duration of the first event from row 1 to end of event in hours
  time1 <- as.numeric(difftime(df[eventsdf[1,"qLength"],"dateTime"], df[1,"dateTime"], units="hours"))
  # calculate the duration of all other events from row before event begins to end of event in hours
 # if(exists("eventTimes")) {rm(eventTimes)}
  eventTimes <- sapply(c(2:dim(eventsdf)[1]), function(x) {
    start <- df[(eventsdf[x-1,"qLength"]),"dateTime"]
    end <- df[(eventsdf[x,"qLength"]),"dateTime"]
    return(round(difftime(end,start,units="hours")))
  })
  eventsdf$hours <- c(time1, eventTimes)

  # determine whether no rain events are greater than the given minimum inter-event time
  eventsdf$meetsMIT <- eventsdf$hours>mit_hours & eventsdf$indicator==0
  
  # find the row numbers that start rain events
  miteventstarts <- as.numeric(as.vector(row.names(eventsdf[which(eventsdf$meetsMIT==TRUE),])))
  
  # for each event start, return the rows of df corresponding to that event
  # also contains cumulative amount of rain, cumulative percentage of rainfall

  precip_events_list <- vector("list", length(miteventstarts)-1)
  name_event <- function(x) { # define how to name each event
    return(paste0(ID,"_", mit_hours, "hrs","_", 
                  str_pad(x,nchar(as.character(length(miteventstarts))), pad="0")))
  }

  precip_events_list <- lapply(2:length(miteventstarts)-1, function(x) {
    precip_event <- df[(eventsdf[miteventstarts[x],"qLength"]):eventsdf[miteventstarts[x+1]-1,"qLength"],]
    precip_event$qRain_cm <- cumsum(precip_event[[rain_col_name]])
    precip_event$qRain_percent <- precip_event$qRain_cm/max(precip_event$qRain_cm)
    precip_event$eventID <- name_event(x)
    return(precip_event)
    })
  
  precip_events_data <- rbind_all(precip_events_list)
  
  # define one hour peak metric
  oneHourPeak <- function(x){
    if(difftime(x[nrow(x),"dateTime"], x[1,"dateTime"], units = "mins") < 60){
      return(NA) # divide by 12 for cm/60 mins since it is summed to 5 mins
    } else {
      return(max(sapply(1:nrow(x), function(y){
        begin <- x[y,"dateTime"]
        end <- x[y,"dateTime"]+60*60
        x[which(abs(x$dateTime-end)==min(abs(x$dateTime-end))),"qRain_cm"]-x[y,"qRain_cm"]
      })))
    }
  }
  
  # define intermittency metrics
  intermit2 <- function(x) {
    intra_event_rain <- as.data.frame(cbind(rle(x$rain_indicator)$values, rle(x$rain_indicator)$lengths))
    intra_event_rain$qLength <- cumsum(intra_event_rain$V2)
    intra_event_rain <- intra_event_rain[which(intra_event_rain$V1==0),]
    subdry_length <- intra_event_rain[which.max(intra_event_rain$V2),"V2"]
    subdry_end <- intra_event_rain[which.max(intra_event_rain$V2),"qLength"]
    subdry <- x[(subdry_end-subdry_length):subdry_end,]
    return(round(as.numeric(difftime(max(subdry$dateTime), min(subdry$dateTime), units="mins")),2))
  }
  
  intermit3 <- function(x) {
    intra_event_rain <- as.data.frame(cbind(rle(x$rain_indicator)$values, rle(x$rain_indicator)$lengths))
    intra_event_rain$qLength <- cumsum(intra_event_rain$V2)
    intra_event_rain <- intra_event_rain[which(intra_event_rain$V1==0),]
    subdry_length <- intra_event_rain[which.max(intra_event_rain$V2),"V2"]
    subdry_end <- intra_event_rain[which.max(intra_event_rain$V2),"qLength"]
    subdry <- x[(subdry_end-subdry_length):subdry_end,]
    return(round(as.numeric(difftime(max(subdry$dateTime), min(subdry$dateTime), units="mins"))/
                   as.numeric(difftime(max(x$dateTime), min(x$dateTime), units="mins")),3))
  }
  

  # makes a summary table of event metrics
  events_summary <- paste0(ID, "_",mit_hours, "hrs_summary")
  events_summary <- cbind(
    # names of events
    as.data.frame(unique(precip_events_data$eventID)),
    # total amount of rain in event
    as.data.frame(sapply(precip_events_list, function(x) max(x$qRain_cm))),
    # peak recorded intensity
    as.data.frame(sapply(precip_events_list, function(x) max(x[[rain_col_name]]))),
    # peak one hour intensity
    as.data.frame(sapply(precip_events_list, function(x) oneHourPeak(x))),
    # duration of event in minutes
    as.data.frame(sapply(precip_events_list, function(x) 
      round(difftime(x[nrow(x),"dateTime"], x[1,"dateTime"], units="mins")))),
    # duration of event in hours
    as.data.frame(sapply(precip_events_list, function(x) 
      round(difftime(x[nrow(x),"dateTime"], x[1,"dateTime"], units="hours"),2))),
    # duration of event in days
    as.data.frame(sapply(precip_events_list, function(x) 
      round(difftime(x[nrow(x),"dateTime"], x[1,"dateTime"], units="days"),3))),
    # duration in hours since previous event ended
    as.data.frame(eventsdf[miteventstarts, "hours"][1:length(miteventstarts)-1]),
    # proportion of rows without rain recorded
    as.data.frame(sapply(precip_events_list, function(x) 
      round(nrow(x[which(x$rain_indicator==FALSE),])/nrow(x),2))),
    # duration of longest period without rain in minutes
    as.data.frame(sapply(precip_events_list, function(x) intermit2(x))),
    # proportion of event that longest period without rain takes up
    as.data.frame(sapply(precip_events_list, function(x) intermit3(x)))
    )
  
  names(events_summary) <- c("eventID", "precip_depth", "precip_maxInt","precip_maxHrInt", "precip_durationMins", "precip_durationHrs", "precip_durationDays", "antedecentDryHours", "percentRainless", "intermit2", "intermit3")
  
  # average intensity of each event is depth/duration in cm/min
  events_summary$precip_avgInt <- signif(events_summary$precip_depth/events_summary$precip_durationMins, digits=3)
  
  # start time
  precip_starts <- sapply(precip_events_list, function(x) min(x$dateTime))
  precip_starts <- (as.POSIXct(precip_starts, origin = "1970-01-01", tz="UTC"))
  #precip_starts <- (as.POSIXct(precip_starts, origin = "1970-01-01", tz="UTC")+(4*60*60))
  events_summary$precip_start <- precip_starts
  
  # end time
  precip_ends <- sapply(precip_events_list, function(x) max(x$dateTime))
  precip_ends <- (as.POSIXct(precip_ends, origin = "1970-01-01", tz="UTC"))
  # precip_ends <- (as.POSIXct(precip_starts, origin = "1970-01-01", tz="UTC")+(4*60*60))
  events_summary$precip_end <- precip_ends
  
  # name dfs in list by event name
  names(precip_events_list) <- unique(precip_events_data$eventID)

  # add in antecedent precipitation to event metrics
  precip_events_data <- as.data.frame(precip_events_data)
  
  eventRows <- as.data.frame(cbind(rle(precip_events_data$eventID)$values,cumsum(rle(precip_events_data$eventID)$lengths)))
  names(eventRows) <- c("event", "endrow")
  eventRows$endrow <- as.numeric(as.character(eventRows$endrow))
  eventRows$startRow <- eventRows$endrow + 1
  eventRows$event <- as.character(eventRows$event)
  eventRows$eventID <- c(eventRows$event[2:nrow(eventRows)],NA)
  eventRows <- eventRows[,c("eventID", "startRow")]
  
  api_xdays <- function(days=1){
  
    apiX <- sapply(1:(nrow(eventRows)-1), function(x) {
      end <- precip_events_data[eventRows$startRow[x],"dateTime"]
      begin <- precip_events_data[eventRows$startRow[x],"dateTime"] - (60*60*24*(days))
      beginrow <- which(abs(precip_events_data$dateTime-begin)==min(abs(precip_events_data$dateTime-begin)))
      sum(precip_events_data[beginrow:(eventRows$startRow[x]),rain_col_name])
    })
    
    return(c(apiX,NA))
  }

  antprecip_cols <- lapply(1:14, function(x){
    api_xdays(days = x)
  })
  
  for(i in 1:length(antprecip_cols)){ eventRows[[paste0("api",i,"day")]] <- antprecip_cols[[i]] }
  
  antPrecip <- eventRows[-nrow(eventRows),-2]

  events_summary <- merge(events_summary, antPrecip, all.x = TRUE)

  # outputs to function
  return(list(events_summary, precip_events_list))
}
