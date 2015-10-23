#FindRecentEvents function

FindRecentEvents = function(EventFrame, RecentDays){
  RecentEventFrame = EventFrame[which(EventFrame$Date >= Sys.Date()-(RecentDays - 1)),]
  RecentEventFrame = RecentEventFrame[RecentEventFrame$Status == "Finished", ]
  return(RecentEventFrame)
}


#FindRecentTests function
FindRecentTests = function(RecentEventFrame){
  RecentTestFrame = data.frame(Published.Test = unique(RecentEventFrame$Published.Test))
  RecentTestFrame$Date = as.Date(NA)
  for (i in 1:nrow(RecentTestFrame)){
    RecentTestFrame$Count[i] = sum(RecentEventFrame$Published.Test == RecentTestFrame$Published.Test[i])
    RecentTestFrame$Date[i] = as.Date(min(
      RecentEventFrame$Date[which(
        RecentEventFrame$Published.Test == RecentTestFrame$Published.Test[i])]))
  }
  RecentTestFrame = RecentTestFrame[order(-RecentTestFrame$Count), ]
  return(RecentTestFrame)
}
