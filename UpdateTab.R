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
  TAB = read.xlsx(xlsxFile = TAB.wb, sheet = "TAB")
  if(length(missingTests) > 0){ 
    TestFrame.temp = TestFrame
    TestFrame$Local.folder = NA_character_
    TestFrame = TestFrame[,c(3,1,2,4)]
    colnames(TestFrame) = colnames(TAB)
    NewTestFrame = rbind.data.frame(TAB, TestFrame)
    NewTestFrame = NewTestFrame[!duplicated(NewTestFrame$TestName),]
    NewTestFrame = NewTestFrame[order(NewTestFrame$TestName),]
    NewTestFrame = DFna.to.empty(NewTestFrame)                       # Convert NA's to empty cells
    NewTestFrame = NewTestFrame[order(NewTestFrame$Local.folder == "", decreasing = T),]
    writeData(wb = TAB.wb, sheet = "TAB", x = NewTestFrame)
    saveWorkbook(wb = TAB.wb, file = TABpath, overwrite = T)
  } # /if there are missing tests
} # /function
