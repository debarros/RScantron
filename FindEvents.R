FindEvents_1Student = function(sid, 
                               ScantronHandle){
  tests = getURI(
    paste0(
      'https://admin.achievementseries.com/student/test-results.csv?id=',
      sid,
      '&_list=TestResults'),
    curl=ScantronHandle)
  
  q = read.csv(textConnection(tests), stringsAsFactors = FALSE)
  
  q = q[,-c(2,5,7)] #remove unnecessary columns
  
  return(q)
}


FindEvents = function (StudentFrame, 
                       ScantronHandle){
  EventFrame = data.frame(character(),character(),character(),character(),stringsAsFactors = FALSE)
  for (i in 1:nrow(StudentFrame)){
    sid = StudentFrame$sid[i]
    x = Find1StudentTests(sid,ScantronHandle)
    StNameRep = rep(StudentFrame$StName[i],nrow(x))
    sidRep = rep(sid,nrow(x))
    StNumberRep = rep(StudentFrame$StNumber[i],nrow(x))
    x = cbind(x,StNameRep,sidRep,StNumberRep)
    if(nrow(x)>0){
      EventFrame = rbind(EventFrame,x)
    }
  }
  return(EventFrame)
}