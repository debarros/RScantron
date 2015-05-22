FindEvents_1Student = function(sid, 
                               ScantronHandle){
  tests = getURI(
    paste0(
      'https://admin.achievementseries.com/student/test-results.csv?id=',
      sid,
      '&_list=TestResults'),
    curl=ScantronHandle)
  
  # This next section is for debugging when an nrow(x) error pops up
  j = textConnection(tests)
  print(str(tests))
  print(str(j))
  write(tests, file = "testpageoutput.html")
  
  #convert the page to a csv
  q = read.csv(j, stringsAsFactors = FALSE)
  q = q[,-c(2,5,7)] #remove unnecessary columns
  
  return(q)
}


FindEvents = function (StudentFrame, 
                       ScantronHandle){
  EventFrame = data.frame(character(),character(),character(),character(),stringsAsFactors = FALSE)
  for (i in 1:nrow(StudentFrame)){
    print(paste0("Student ",i," of ",nrow(StudentFrame)))
    sid = StudentFrame$sid[i]
    x = FindEvents_1Student(sid,ScantronHandle)
    if(nrow(x)>0){
      StNameRep = rep(StudentFrame$StName[i],nrow(x))
      sidRep = rep(sid,nrow(x))
      StNumberRep = rep(StudentFrame$StNumber[i],nrow(x))
      x = cbind(x,StNameRep,sidRep,StNumberRep)
      EventFrame = rbind(EventFrame,x)
    }
  }
  EventFrame$Date = as.Date(EventFrame$Date, "%m/%d/%y")  #fix the date format
  return(EventFrame)
}