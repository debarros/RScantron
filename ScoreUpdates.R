#Score Updates

ScoreUpdates = function(RecentEventFrame,RecentTestFrame,cap){
  #cap is the maximum number of scan events a test can have and still be an update
  
  x = RecentEventFrame
  y = RecentTestFrame
  
  #Convert the student names to character format
  x$StNameRep = as.character(x$StNameRep)
  
  #Convert the test names to character format
  x$Published.Test = as.character(x$Published.Test)
  y$Published.Test = as.character(y$Published.Test)
  
  #Limit the set of tests to just those that have fewer than cap scans, don't require special scoring, and are not Humanities
  y = y[y$Count < cap,]
  y = y[!grepl(pattern = ">", x = y$Published.Test),]
  y = y[!grepl(pattern = "H[2-3] ", x = y$Published.Test),]
  
  
  #Extract the numerical score
  x$Score2 = substr(x$Score,1,regexpr(pattern = "\\(", text =  x$Score)-1)
  
  #Limit to just the tests and columns of interest
  x =  x[x$Published.Test %in% y$Published.Test,c(2,5,8,1)]
  
  
  
  #Order by Name and Test
  x = x[order(x$StNameRep),]
  x = x[order(x$Published.Test),]
  
  #get rid of the rownames
  rownames(x) = NULL
  rownames(y) = NULL
  
  ReportableTests = RecentTestFrame[!(RecentTestFrame$Published.Test %in% unique(x$Published.Test)),]
  
  #Create the output
  write.csv(x, file = "recentScores.csv")
  write.csv(ReportableTests, file = "reportableTests.csv")
}

#The better way to do this would be to create something that takes each test,
#and creates a character string to match what I would put in the email.
#The character string (complete with spacing and carriage returns) 
#could be written to a file.
#In fact, all of them could be written to the same file, 
#with some sort of vertical delimeter.
#What function writes a text file?
