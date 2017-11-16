#FindTests.R

#This is a wrapper function for FindTests_1Folder() to make it run once for each folder in TestFolderFrame

FindTests = function (TestFolderFrame, messageLevel = 0){
  TestFrame = data.frame(stringsAsFactors = FALSE)
  for (i in 1:nrow(TestFolderFrame)){
    TestFolderRow = TestFolderFrame[i,]
    x = FindTests_1Folder(TestFolderRow)
    TestFrame = rbind(TestFrame, x)
  }
  return(TestFrame)
}

