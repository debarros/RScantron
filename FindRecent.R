#FindRecentEvents function

FindRecentEvents = function(EventFrame, RecentDays = NULL, url.ws = NULL, TAB = NULL, newScores = T, status = c("Finished"), startcell = "A1"){
  
  if(!is.null(RecentDays)){
    RecentEventFrame = EventFrame[which(EventFrame$Date >= Sys.Date()-(RecentDays - 1)),]
    RecentEventFrame = RecentEventFrame[RecentEventFrame$Status == "Finished", ]
    return(RecentEventFrame) 
    
  } else if(!is.null(url.ws)){
    if(length(url.ws) != 2){
      stop("url.ws must be a list of length 2, where the first element is the result of gs_url() and the second is a character value")
    }
    PriorEventFrame = gs_read(ss = url.ws[[1]], ws = url.ws[[2]]) #read in existing events
    gs_edit_cells(ss = url, ws = ws, input = EventFrame, anchor = startcell) #store the complete events
    return(FindRecentEvents.compare(EventFrame, newScores, PriorEventFrame, status))
    
  } else if(!is.null(TAB)){
    if(length(TAB) != 2){
      stop("TAB must be a list of length 2, where the first element is an openxlsx workbook object and the second is a character value with the workbook's file path")
    }
    TAB.wb = TAB[[1]]
    TABpath = TAB[[2]]
    PriorEventFrame = read.xlsx(xlsxFile = TABpath, sheet = "Events") #read in existing events
    writeData(wb = TAB.wb, sheet = "Events", x = EventFrame) #store the complete events
    saveWorkbook(wb = TAB.wb, file = TABpath, overwrite = T)
    return(FindRecentEvents.compare(EventFrame, newScores, PriorEventFrame, status))
    
  } else {
    stop("You must specify at least one of RecentDays, url.ws, or TABpath")
  }
}


FindRecentEvents.compare = function(EventFrame, newScores, PriorEventFrame, status){
  IDcolumns = colnames(EventFrame)
  if(!newScores){
    IDcolumns = IDcolumns[!(IDcolumns %in% c("Score"))]
  }
  PriorEventFrame$identifier = apply(PriorEventFrame[IDcolumns], MARGIN = 1, FUN = paste0, collapse = "-")
  EventFrame$identifier = apply(EventFrame[IDcolumns], MARGIN = 1, FUN = paste0, collapse = "-")
  RecentEventFrame = EventFrame[!(EventFrame$identifier %in% PriorEventFrame$identifier),1:(ncol(EventFrame)-1)]
  RecentEventFrame = RecentEventFrame[RecentEventFrame$Status %in% status, ]
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
