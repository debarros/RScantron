#UpdateTab.R

#' @title Update TAB (Test Address Book)
#' @description Update the Test Address Book file with tests not found in it
#' @param missingTests output from the FindMissingTests function
#' @param TestFrame output from the FindTests function
#' @param TAB.wb the result of running loadWorkbook on the TAB file
#' @param TABpath the file path of the TAB, including the file name
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print
#' @return Nothing gets returned by this function
#' @details This function updates the spreadsheet of tests with any new tests found in the TMS
UpdateTab = function(missingTests, TestFrame, TAB.wb, TABpath, messageLevel = 0){
  if(messageLevel > 0){
    print("Updating the TAB")
  }
  TAB = read.xlsx(xlsxFile = TAB.wb, sheet = "TAB")                                      # Grab the TAB sheet of the TAB
  if(length(missingTests) > 0){                                                          # If there are any missing tests,
    TestFrame.temp = TestFrame                                                           # Make a copy of the TestFrame (for some reason?)
    TestFrame$Local.folder = NA_character_                                               # Add a Local.Folder column
    TestFrame = TestFrame[,c(3,1,2,4)]                                                   # Rearrange the columns
    colnames(TestFrame) = colnames(TAB)                                                  # Make the column names match
    NewTestFrame = rbind.data.frame(TAB, TestFrame)                                      # Merge the two sets of tests
    NewTestFrame = NewTestFrame[!duplicated(NewTestFrame$TestName),]                     # Remove duplicate entries (keeping ones that already have the local folder)
    NewTestFrame = NewTestFrame[order(NewTestFrame$Local.folder, na.last = F),]          # Arrange by folder location and put the ones with missing folders first
    NewTestFrame = DFna.to.empty(NewTestFrame)                                           # Convert NA's to empty cells
    writeData(wb = TAB.wb, sheet = "TAB", x = NewTestFrame)                              # Put the data into the workbook object
    saveWorkbook(wb = TAB.wb, file = TABpath, overwrite = T)                             # Save the workbook to the file system
  } # /if there are missing tests
} # /function
