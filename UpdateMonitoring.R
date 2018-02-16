# Monitor.R

# This function does all of the monitoring stuff

UpdateMonitoring = function(ScannedTests.url, RecentTestFrame, TAB.wb, messageLevel = 0){
  
  if(messageLevel > 0){ print("Updating monitoring")}
  
  # Get the current Scanned Tests document and then clear it out
  if(messageLevel > 0){ print("Getting the scanned test document")}
  ScannedTests = SWSM(gs_read(ss = ScannedTests.url, ws = 1, verbose = F))
  if(nrow(ScannedTests) > 0){
    Blank = matrix(data = "", nrow = nrow(ScannedTests), ncol = ncol(ScannedTests)) 
    if(messageLevel > 0){ print("Clearing the scanned test document")}
    SWSM(gs_edit_cells(ss = ScannedTests.url, ws = 1, input = Blank, anchor = "A2")) # Start at A2 to leave the header row in place
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
  UniqueScannedTests = AllScannedTests[!duplicated(AllScannedTests$Test),]
  for(i in 1:nrow(UniqueScannedTests)){ # For each unique scanned test
    for(j in c("MakeReport","SendReport","Update","Monitor")){ # check whether any instance of it (or or new) requires certain actions
      UniqueScannedTests[i,j] = any(unlist(AllScannedTests[AllScannedTests$Test == UniqueScannedTests$Test[i],j]))
    }
  }
  
  # Remove from UniqueScannedTests any records that require no action
  UniqueScannedTests = UniqueScannedTests[apply(X = UniqueScannedTests[,c("MakeReport","SendReport","Update","Monitor")], MARGIN = 1, FUN = any),]
  
  # Update the Scanned Tests document with the modified ScannedTests 
  if(messageLevel > 0){ print("Update the Scanned Tests document")}
  SWSM(gs_edit_cells(ss = ScannedTests.url, ws = 1, input = UniqueScannedTests, anchor = "A1")) # Start at A1 b/c the header row is also added
  
  # store the date and time of the current run
  SWSM(gs_edit_cells(ss = ScannedTests.url, ws = 2, input = Sys.time(), anchor = "A2") )
  
} # /function