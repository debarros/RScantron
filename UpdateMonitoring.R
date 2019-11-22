# UpdateMonitoring.R

# This function does all of the monitoring stuff


#' @title Update Monitoring
#' @description Update the score monitoring spreadsheet
#' @param ScannedTests.url output from the FindMissingTests function
#' @param RecentTestFrame output from the FindTests function
#' @param MakeReportDone the result of running loadWorkbook on the TAB file
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print
#' @return Nothing gets returned by this function
#' @details This function updates the spreadsheet of tests with any new tests found in the TMS
UpdateMonitoring = function(ScannedTests.url, RecentTestFrame, TAB.wb, MakeReportDone = F, sortDown = T, messageLevel = 0){
  
  if(messageLevel > 0){ print("Updating monitoring")}
  
  # Get the current Scanned Tests document 
  if(messageLevel > 0){ print("Getting the scanned test document")}
  ScannedTests = SWSM(gs_read(ss = ScannedTests.url, ws = 1, verbose = F))
  replacementDimensions = c(nrow(ScannedTests), ncol(ScannedTests))
  
  
  # If tests marked as needing reports were done, clear those values and set Send and Monitor to TRUE
  if((nrow(ScannedTests) > 0) & MakeReportDone){
    ScannedTests$SendReport[ScannedTests$MakeReport] = TRUE
    ScannedTests$Monitor[ScannedTests$MakeReport] = TRUE
    ScannedTests$MakeReport = FALSE
  }
  
  # Modify ScannedTests to include the newly scanned tests
  if(messageLevel > 0){ print("Adding newly scanned tests to the list")}
  NewScannedTests = data.frame(
    Test = RecentTestFrame$Published.Test, 
    Folder = read.xlsx(TAB.wb)$Local.folder[match(RecentTestFrame$Published.Test, read.xlsx(TAB.wb)$TestName)])
  if(nrow(NewScannedTests) > 0){
    NewScannedTests$MakeReport = F
    NewScannedTests$SendReport = T
    NewScannedTests$Update = F
    NewScannedTests$Monitor = T
  }
  
  # If ScannedTests has extra columns, put them in NewScannedTests also
  if(ncol(ScannedTests) > 6){
    moreColumns = colnames(ScannedTests)[7:ncol(ScannedTests)]
    for(curColumn in moreColumns){
      NewScannedTests[,curColumn] = ""  
    } # /for each extra column
  } # /if there are extra columns
  
  if(messageLevel > 0){ print("Combining existing scanned tests with new ones")}
  
  AllScannedTests = rbind(NewScannedTests, ScannedTests)
  UniqScanTests = AllScannedTests[!duplicated(AllScannedTests$Test),]
  for(i in 1:nrow(UniqScanTests)){ # For each unique scanned test
    for(j in c("MakeReport","SendReport","Update","Monitor")){ # check whether any instance of it requires any action
      UniqScanTests[i,j] = any(unlist(AllScannedTests[AllScannedTests$Test == UniqScanTests$Test[i],j]))
    }
  }
  
  # Remove from UniqScanTests any records that require no action
  recordsToKeep = apply(X = UniqScanTests[,c("MakeReport","SendReport","Update","Monitor")], 
                        MARGIN = 1, 
                        FUN = any)
  UniqScanTests = UniqScanTests[recordsToKeep,]
  
  # Sort UniqScanTests
  newOrder = order(UniqScanTests$SendReport, UniqScanTests$MakeReport, UniqScanTests$Folder, decreasing = sortDown)
  UniqScanTests = UniqScanTests[newOrder,]
  
  # Update the Scanned Tests document with the modified ScannedTests 
  if(messageLevel > 0){ print("Updating the Scanned Tests document")}
  
  # create a remote copy of the scanned tests sheet.  Start at A1 b/c the header row is also added
  if(messageLevel > 1){ print("Creating a backup worksheet")}
  gs_ws_new(ss = ScannedTests.url, ws_title = "Backup", input = UniqScanTests, anchor = "A1", verbose = F)
  
  # clear the main worksheet if necessary
  if(replacementDimensions[1] > 0){
    Blank = matrix(data = "", nrow = replacementDimensions[1], ncol = replacementDimensions[2]) 
    if(messageLevel > 1){ print("Clearing the scanned test document")}
    SWSM(gs_edit_cells(ss = ScannedTests.url, ws = 1, input = Blank, anchor = "A2")) # Start at A2 to keep header row
  }  
  
  # Load data into the main worksheet, reregister it so the backup becomes visible, and delete the backup
  if(messageLevel > 1){ print("Filling in the new scanned tests list")}
  SWSM(gs_edit_cells(ss = ScannedTests.url, ws = , input = UniqScanTests, anchor = "A1")) 
  
  if(messageLevel > 2){ print("Reregistering the sheet")}
  ScannedTests.url = SWSM(gs_key(x = ScannedTests.url$sheet_key)) 
  
  if(messageLevel > 1){ print("Deleting the backup worksheet")}
  SWSM(gs_ws_delete(ss = ScannedTests.url, ws = "Backup", verbose = F)) 
  
  # store the date and time of the current run
  if(messageLevel > 1){ print("Storing the date and time")}
  SWSM(gs_edit_cells(ss = ScannedTests.url, ws = "LastRun", input = Sys.time(), anchor = "A2") )
  
} # /function
