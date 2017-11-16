#FindResults.R

FindResults_1test = function(tid, ScantronHandle, attempt = 1, messageLevel = 0){
  scores = getURI(
    paste0(
      'https://admin.achievementseries.com/published-test/export/csv-list.csv?',
      'id=',tid,
      '&d=F&t=T&r=F&p=F&_list=ExportList'),
    curl=ScantronHandle)
  q = read.csv(textConnection(scores), stringsAsFactors = FALSE) #treat the page to a csv and make a data.frame
  
  #Recursive call: If the http request got an error page, redo the request
  print(paste0("length(q) is ",length(q), " and this is Attempt ",attempt))
  if(length(q) != 18){
    q = FindResults_1test(tid, ScantronHandle, attempt = attempt+1)
  }
  
  print(paste0("q has ",ncol(q)," columns."))
  
  return(q)
}



FindResults = function (testIDs, ScantronHandle){
  ResultFrame = as.data.frame(matrix(data = character(), nrow = 0, ncol = 18), stringsAsFactors = FALSE)
  for (i in 1:length(testIDs)){
    print(paste0("Test ",i," of ",length(testIDs)))
    tid = testIDs[i]
    x = GetResults_1test(tid,ScantronHandle)
    if(nrow(x)>0){ResultFrame = rbind(ResultFrame,x)}
  }
  ResultFrame$ReportingDate = as.Date(ResultFrame$ReportingDate, "%m/%d/%y")
  ResultFrame$TestAdministrationStartDate = as.Date(ResultFrame$TestAdministrationStartDate, "%m/%d/%y")
  ResultFrame$Score = ResultFrame$PointsAttained / ResultFrame$PointsPossible
  return(ResultFrame)
}


