#FindEvents.R


FindEvents = function (StudentFrame, 
                       ScantronHandle){
  EventFrame = data.frame(character(),character(),character(),character(),stringsAsFactors = FALSE)
  for (i in 1:nrow(StudentFrame)){
    print(paste0("Student ",i," of ",nrow(StudentFrame)))
    sid = StudentFrame$sid[i]
    x = FindEvents_1Student(sid,ScantronHandle)
    x = x[,-c(2,5,7)] #remove unnecessary columns
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