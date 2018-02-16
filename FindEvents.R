# FindEvents.R

FindEvents = function (StudentFrame, SchoolYear = NULL, messageLevel = 0, agent = NULL) {
  
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  }
  
  EventFrame = data.frame(character(), character(), character(), character(), stringsAsFactors = FALSE)
  
  for (i in 1:nrow(StudentFrame)) {
    if (messageLevel > 0) {
      print(paste0("Student ", i, " of ", nrow(StudentFrame)))
    }
    
    sid = StudentFrame$sid[i]
    x = FindEvents_1Student(sid = sid, messageLevel = messageLevel - 1, agent = agent)
    x = x[, -c(2, 5, 7)] #remove unnecessary columns
    if (nrow(x) > 0) {
      StNameRep = rep(StudentFrame$StName[i], nrow(x))
      sidRep = rep(sid, nrow(x))
      StNumberRep = rep(StudentFrame$StNumber[i], nrow(x))
      x = cbind(x, StNameRep, sidRep, StNumberRep)
      EventFrame = rbind(EventFrame, x)
    } # /if
  } # /for each student
  
  EventFrame$Date = as.Date(EventFrame$Date, "%m/%d/%y")  #fix the date format
  
  if (!is.null(SchoolYear)) {
    cutoffdate = as.Date(paste0(SchoolYear, "-07-01"))
    EventFrame = EventFrame[EventFrame$Date >= cutoffdate, ]
    rownames(EventFrame) = NULL
  } # /if
  
  return(EventFrame)
  
} # /function
