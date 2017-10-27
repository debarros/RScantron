#UpdateTab.R

UpdateTab = function(missingTests, TestFrame, TAB.wb, TABpath){
  TAB = read.xlsx(xlsxFile = TAB.wb, sheet = "TAB")
  if(length(missingTests) > 0){ 
    TestFrame.temp = TestFrame
    TestFrame$Local.folder = NA_character_
    TestFrame = TestFrame[,c(3,1,2,4)]
    colnames(TestFrame) = colnames(TAB)
    NewTestFrame = rbind.data.frame(TAB, TestFrame)
    NewTestFrame = NewTestFrame[!duplicated(NewTestFrame$TestName),]
    NewTestFrame = NewTestFrame[order(NewTestFrame$TestName),]
    for(i in 1:ncol(NewTestFrame)){
      NewTestFrame[,i] = na.to.empty(NewTestFrame[,i])
    } # /for loop
    writeData(wb = TAB.wb, sheet = "TAB", x = NewTestFrame)
    saveWorkbook(wb = TAB.wb, file = TABpath, overwrite = T)
  } # /if there are missing tests
} # /function


