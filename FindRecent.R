#FindRecentEvents function

FindRecentEvents = function(EventFrame, RecentDays = NULL, url = NULL, ws = NULL, newScores = T, status = c("Finished"), startcell = "A1"){
  if(!is.null(RecentDays)){
    RecentEventFrame = EventFrame[which(EventFrame$Date >= Sys.Date()-(RecentDays - 1)),]
    RecentEventFrame = RecentEventFrame[RecentEventFrame$Status == "Finished", ]
    return(RecentEventFrame) 
  } else if(is.null(url) | is.null(ws)){
    stop("If RecentDays is not specified, then both url and ws must be.")
  } else {
    PriorEventFrame = gs_read(ss = url, ws = ws) #read in existing events
    gs_edit_cells(ss = url, ws = ws, input = EventFrame, anchor = startcell) #store the complete events
    IDcolumns = colnames(EventFrame)
    if(!newScores){
      IDcolumns = IDcolumns[!(IDcolumns %in% c("Score"))]
    }
    PriorEventFrame$identifier = apply(PriorEventFrame[IDcolumns], MARGIN = 1, FUN = paste0, collapse = "-")
    EventFrame$identifier = apply(EventFrame[IDcolumns], MARGIN = 1, FUN = paste0, collapse = "-")
    RecentEventFrame = EventFrame[!(EventFrame$identifier %in% PriorEventFrame$identifier),1:(ncol(EventFrame)-1)]
    RecentEventFrame = RecentEventFrame[RecentEventFrame$Status %in% status, ]
  }
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
