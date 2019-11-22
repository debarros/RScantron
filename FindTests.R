# FindTests.R

# This is a wrapper function for FindTests_1Folder() to make it run once for each folder in TestFolderFrame
# Return TestFrame, which is a data.frame with one row for each test found

FindTests = function (TestFolderFrame, messageLevel = 0){
  
  if(messageLevel > 0){ print("Finding tests.")}
  
  TestFrame = data.frame(stringsAsFactors = FALSE)
  
  for (i in 1:nrow(TestFolderFrame)){
    TestFolderRow = TestFolderFrame[i,]
    x = FindTests_1Folder(TestFolderRow, messageLevel - 1)
    TestFrame = rbind(TestFrame, x)
  } # /for each row in TestFolderFrame
  
  return(TestFrame)
  
} # /function
