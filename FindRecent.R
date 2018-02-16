# FindRecent.R

# FindRecentEvents function
FindRecentEvents = function(EventFrame, RecentDays = NULL, url.ws = NULL, TAB = NULL, newScores = T, 
                            status = c("Finished"), startcell = "A1", updatePriorEvents = T, messageLevel = 0){
  
  if(!is.null(RecentDays)){
    if(messageLevel > 0){
      print(paste0("Finding events from the last ", RecentDays, " days."))
    }
    RecentEventFrame = EventFrame[which(EventFrame$Date >= Sys.Date() - (RecentDays - 1)),]
    RecentEventFrame = RecentEventFrame[RecentEventFrame$Status == "Finished",]
    return(RecentEventFrame) 
    
  } else if(!is.null(url.ws)){
    if(length(url.ws) != 2){
      stop("url.ws must be a list of length 2, where the first element is the result of gs_url() and the second is a character value")
    }
    PriorEventFrame = gs_read(ss = url.ws[[1]], ws = url.ws[[2]])                    # read in existing events
    gs_edit_cells(ss = url, ws = ws, input = EventFrame, anchor = startcell)         # store the complete events
    return(FindRecentEvents.compare(EventFrame, newScores, PriorEventFrame, status))
    
  } else if(!is.null(TAB)){
    if(length(TAB) != 2){
      stop(paste0("TAB must be a list of length 2, ",
                  "where the first element is an openxlsx workbook object", 
                  "and the second is a character value with the workbook's file path"))
    }
    TAB.wb = TAB[[1]]
    TABpath = TAB[[2]]
    PriorEventFrame = read.xlsx(xlsxFile = TABpath, sheet = "Events")          # read in existing events
    PriorEventFrame$Date = as.Date(PriorEventFrame$Date, origin = "1899-12-30")
    if(updatePriorEvents){                                                     # update prior event table in TAB (if necessary)
      writeData(wb = TAB.wb, sheet = "Events", x = EventFrame)                 # store the complete events
      saveWorkbook(wb = TAB.wb, file = TABpath, overwrite = T)
    }
    return(FindRecentEvents.compare(EventFrame, newScores, PriorEventFrame, status, messageLevel = messageLevel - 1))
    
  } else {
    stop("You must specify at least one of RecentDays, url.ws, or TABpath")
  }
} # /FindRecentEvents function




# FindRecentEvents.compare function
FindRecentEvents.compare = function(EventFrame, newScores, PriorEventFrame, status, messageLevel = 0){
  
  if(messageLevel > 0){
    print("Comparing event frame to remove events that have already been reported")
  }
  
  IDcolumns = colnames(EventFrame)
  if(!newScores){
    IDcolumns = IDcolumns[!(IDcolumns %in% c("Score"))]
  }
  PriorEventFrame$identifier = apply(PriorEventFrame[IDcolumns], MARGIN = 1, FUN = paste0, collapse = "-")
  EventFrame$identifier = apply(EventFrame[IDcolumns], MARGIN = 1, FUN = paste0, collapse = "-")
  RecentEventFrame = EventFrame[!(EventFrame$identifier %in% PriorEventFrame$identifier),1:(ncol(EventFrame)-1)]
  RecentEventFrame = RecentEventFrame[RecentEventFrame$Status %in% status, ]
  
  return(RecentEventFrame) 
  
} # /FindRecentEvents.compare function




# FindRecentTests function
FindRecentTests = function(RecentEventFrame, messageLevel = 0){
  if(nrow(RecentEventFrame) == 0){
    print("There are no recent tests.")
    return(NULL)
  }
  
  if(messageLevel > 0){
    print("Finding recently scanned tests")
  }
  
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
} # /FindRecentTests function
