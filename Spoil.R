# Spoil.R

Spoil_1Event = function(Event, messageLevel = 0, agent = NULL){
  
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  }
  
  # Get the events page for the relevant student
  studentEventsPage <- httr::content(
    httr::GET(
      url = paste0('https://admin.achievementseries.com/student/test-results.ssp?id=',Event$sidRep),
      user_agent(agent)),
    as = "text",
    encoding = "UTF-8")
  
  # Check to make sure it worked
  if (BadReturnCheck(studentEventsPage, messageLevel - 2)) {
    stop("Error!  You are no longer logged in to Scantron.  Log in and then run this command again.")
  }
  
  # Fix weird html characters
  studentEventsPage = FixHtmlChars(x = studentEventsPage, messageLevel = messageLevel - 1)
  
  # Find the spoil link for the relevant event
  matchLocations = gregexpr(pattern = Event$Published.Test, text = studentEventsPage, fixed = T)
  matchLocations = matchLocations[[1]][1]
  spoilLinks = gregexpr(pattern = "/test-instance/spoil.ssp?id=", text = studentEventsPage, fixed = T)
  spoilLinks = spoilLinks[[1]]
  goodSpoilLink = min(spoilLinks[spoilLinks > matchLocations])
  EventID = substr(x = studentEventsPage, start = goodSpoilLink + 28, stop = goodSpoilLink + 43)
  
  # Click the spoil link
  spoilPage1 <- httr::content(
    httr::GET(
      url = paste0('https://admin.achievementseries.com/test-instance/spoil.ssp?id=',EventID),
      user_agent(agent)),
    as = "text",
    encoding = "UTF-8")
  
  spoilPage1 = FixHtmlChars(x = spoilPage1, messageLevel = messageLevel - 1)
  
  # Grab the student ID and test name and print them
  spanTagLocations =  gregexpr(pattern = '<span class="ss2">', text = spoilPage1, fixed = T)[[1]]
  spanTagLocations2 =  gregexpr(pattern = '</span>', text = spoilPage1, fixed = T)[[1]]
  
  pubTestLocation = regexpr(pattern = "Published Test", text = spoilPage1, fixed = T)
  start = min(spanTagLocations[spanTagLocations > pubTestLocation])
  end = min(spanTagLocations2[spanTagLocations2 > start])
  testname = substr(spoilPage1, start + 18, end - 1)
  
  sidLocation = regexpr(pattern = "Student ID", text = spoilPage1, fixed = T)[[1]]
  start = min(spanTagLocations[spanTagLocations > sidLocation])
  end = min(spanTagLocations2[spanTagLocations2 > start])
  sid = substr(spoilPage1, start + 18, end - 1)
  
  if(testname != Event$Published.Test){
    stop("ERROR!  It looks like you need to update your events before trying to spoil stuff.")
  }
  
  if(messageLevel > 0){
    print(paste0("Spoiling ", testname, " for ", sid))  
  }
  
  # Confirm spoil
  pars = list("_hasCancelButton" = "yes",
              "_PageName"        = "1",
              "Confirmation"     = "T",
              "_PageAction"      = "n")
  spoilPage2 <- httr::POST(url = "https://admin.achievementseries.com/test-instance/spoil.ssp", 
                           user_agent(agent), 
                           body = pars, 
                           encode = "multipart")
  
  # Hit OK button
  pars = list("_hasCancelButton" = "yes",
              "_PageName"        = "2",
              "_PageAction"      = "c")
  spoilPage2 <- httr::POST(url = "https://admin.achievementseries.com/test-instance/spoil.ssp", 
                           user_agent(agent), 
                           body = pars, 
                           encode = "multipart")
  
} # /Spoil_1Event








Spoil = function(SpoilFrame, messageLevel = 0, agent = NULL){
  
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  }
  
  SpoilFrame = SpoilFrame[SpoilFrame$Status == "Finished",]
  
  for(thisRow in 1:nrow(SpoilFrame)){
    Spoil_1Event(Event = SpoilFrame[thisRow,], messageLevel = messageLevel - 1, agent = agent)
  }
} # /Spoil










Spoil_1Test = function(EventFrame, PublishedTest, messageLevel = 0, agent = NULL){
  
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  }
  
  SpoilFrame = EventFrame[EventFrame$Published.Test == PublishedTest,]
  
  Spoil(SpoilFrame, messageLevel = messageLevel - 1, agent = agent)
  
} # /Spoil_1Test








Spoil_1Student = function(EventFrame, StudentNumber, messageLevel = 0, agent = NULL){
  
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  }
  
  SpoilFrame = EventFrame[EventFrame$StNumberRep == StudentNumber,]
  
  Spoil(SpoilFrame, messageLevel = messageLevel - 1, agent = agent)
  
} # /Spoil_1Student

