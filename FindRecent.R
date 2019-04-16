# FindRecent.R

#' @title Find Recent Events
#' @description Identify recent testing events
#' @param EventFrame output from FindEvents function
#' @param RecentDays how many days to look back.
#' @param url.ws list of length 2, where the first element is the result of
#'   gs_url() and the second is a character value.
#' @param TAB list of length 2. The first element is an openxlsx workbook object
#'   created by running loadworkbook on the TAB file.  The second is a character
#'   value with the TAB workbook's file path.
#' @param newScores logical.  Should new scores be considered new testing events.
#' @param status character vector of statuses to include.  Defaults to "Finished".
#' @param startcell
#' @param updatePriorEvents logical - should the table of prior events be
#'   updates with new recent events.
#' @param useNameChanges logical - should changes in student names be considered new scores?
#' @param messageLevel integer of length 1 indicating level of diagnostic
#'   messages to print.  Defaults to 0.
#' @return data.frame with one row for each recent testing event
#' @details At least one of \code{RecentDays}, \code{url.ws}, or \code{TAB} must
#'   be provided.  If \code{TAB} or \code{url.ws} is provided but
#'   \code{RecentDays} is not, the function FindRecentEvents.compare is called.
FindRecentEvents = function(EventFrame, RecentDays = NULL, url.ws = NULL, TAB = NULL, newScores = T, 
                            status = c("Finished"), startcell = "A1", updatePriorEvents = T, 
                            useNameChanges = F, messageLevel = 0){
  
  if(!is.null(RecentDays)){
    if(messageLevel > 0){ print(paste0("Finding events from the last ", RecentDays, " days.")) }
    RecentEventFrame = EventFrame[EventFrame$Date >= Sys.Date() - (RecentDays - 1),]
    RecentEventFrame = RecentEventFrame[RecentEventFrame$Status  %in% status,]
    
  } else if(!is.null(url.ws)){
    if(length(url.ws) != 2){
      stop("url.ws must be a list of length 2, where the first element is the result of gs_url() and the second is a character value")
    }
    PriorEventFrame = gs_read(ss = url.ws[[1]], ws = url.ws[[2]])                    # read in existing events
    gs_edit_cells(ss = url, ws = ws, input = EventFrame, anchor = startcell)         # store the complete events
    RecentEventFrame = FindRecentEvents.compare(EventFrame = EventFrame, newScores = newScores, PriorEventFrame = PriorEventFrame, 
                                                status = status, useNameChanges = F, messageLevel = messageLevel - 1)
    
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
      
      
      
      
      ################################
      
      # This is a bad line.  It ignores the possibility that there are events in the TAB that are not included in EventFrame.
      # This is a poor assumption because it is possible for a student to transfer out and then back in.
      # If the student's entries are deleted, then when he transfers back in, they will appear to be new events.
      # The data to be written should be the union of PriorEventFrame and EventFrame.
      # There should also be a paramater that governs the behavior.
      writeData(wb = TAB.wb, sheet = "Events", x = EventFrame)                 
      
      ###############################
      
      
      saveWorkbook(wb = TAB.wb, file = TABpath, overwrite = T)
    }
    RecentEventFrame = FindRecentEvents.compare(EventFrame = EventFrame, newScores = newScores, PriorEventFrame = PriorEventFrame, 
                                                status = status, useNameChanges = useNameChanges, messageLevel = messageLevel - 1)
    
  } else {
    stop("You must specify at least one of RecentDays, url.ws, or TAB")
  }
  
  return(RecentEventFrame)
} # /FindRecentEvents function




#' @title Find Recent Events By Comparison
#' @description Identify recent testing events using a table of prior testing
#'   events.
#' @param EventFrame output from FindEvents function.
#' @param newScores logical.  Should new scores be considered new testing
#'   events.
#' @param PriorEventFrame data.frame with a table of prior events read from the
#'   TAB or a google sheet.
#' @param status character vector of statuses to include.  Defaults to
#'   "Finished".
#' @param useNameChanges logical - should changes in student names be considered new scores?
#' @param messageLevel integer of length 1 indicating level of diagnostic
#'   messages to print.  Defaults to 0.
#' @return data.frame with one row for each recent testing event
#' @details This function is called from the FindRecentEvents function.
FindRecentEvents.compare = function(EventFrame, newScores, PriorEventFrame, status, useNameChanges, messageLevel = 0){
  
  if(messageLevel > 0){ print("Comparing event frame to remove events that have already been reported") }
  
  IDcolumns = colnames(EventFrame)
  if(!newScores){
    IDcolumns = IDcolumns[!(IDcolumns %in% c("Score"))]
  }
  if(!useNameChanges){
    IDcolumns = IDcolumns[!(IDcolumns %in% c("StNameRep"))]
  }
  
  PriorEventFrame$identifier = apply(PriorEventFrame[IDcolumns], MARGIN = 1, FUN = paste0, collapse = "-")
  EventFrame$identifier = apply(EventFrame[IDcolumns], MARGIN = 1, FUN = paste0, collapse = "-")
  RecentEventFrame = EventFrame[!(EventFrame$identifier %in% PriorEventFrame$identifier),1:(ncol(EventFrame)-1)]
  RecentEventFrame = RecentEventFrame[RecentEventFrame$Status %in% status, ]
  
  return(RecentEventFrame) 
} # /FindRecentEvents.compare function




#' @title Find Recent Tests
#' @description Check the data.frame of recent testing events and compile info
#'   on recent scanned tests.
#' @param RecentEventFrame output from the FindRecentEvents function.
#' @param messageLevel integer of length 1 indicating level of diagnostic
#'   messages to print.  Defaults to 0.
#' @return data.frame with one row for each recently scanned test
FindRecentTests = function(RecentEventFrame, messageLevel = 0){
  if(nrow(RecentEventFrame) == 0){
    print("There are no recent tests.")
    RecentTestFrame = NULL
  } else {
    
    if(messageLevel > 0){ print("Finding recently scanned tests") }
    
    RecentTestFrame = data.frame(Published.Test = unique(RecentEventFrame$Published.Test))
    RecentTestFrame$Date = as.Date(NA)
    for (i in 1:nrow(RecentTestFrame)){
      RecentTestFrame$Count[i] = sum(RecentEventFrame$Published.Test == RecentTestFrame$Published.Test[i])
      RecentTestFrame$Date[i] = as.Date(min(
        RecentEventFrame$Date[RecentEventFrame$Published.Test == RecentTestFrame$Published.Test[i]]))
    } #/for
    RecentTestFrame = RecentTestFrame[order(-RecentTestFrame$Count), ]
  } #/if-else
  
  return(RecentTestFrame)
} # /FindRecentTests function
