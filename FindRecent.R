#FindRecentEvents function

FindRecentEvents = function(EventFrame, RecentDays){
  RecentEventFrame = EventFrame[which(EventFrame$Date >= Sys.Date()-(RecentDays - 1)),]
  return(x)
}



#FindRecentTests function
FindRecentTests = function(RecentEventFrame){
  RecentTestFrame = data.frame(Published.Test = unique(RecentEventFrame$Published.Test))
  for (i in 1:nrow(RecentTestFrame)){
    RecentTestFrame$Count[i] = sum(RecentEventFrame$Published.Test == RecentTestFrame$Published.Test[i])
  }
  return(RecentTestFrame)
}
