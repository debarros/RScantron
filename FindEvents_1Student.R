#FindEvents_1Student.R

FindEvents_1Student = function(sid,
                               attempt = 1,
                               messageLevel = 0,
                               agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36") {
  tests <- content(httr::GET(
    url = paste0(
      'https://admin.achievementseries.com/student/test-results.csv?id=',
      sid,
      '&_list=TestResults'
    ),
    user_agent(agent)
  ),
  as = "text",
  encoding = "UTF-8")
  # tests <- content(tests, as = "text")
  
  write(tests, file = "~/testpageoutput.html") # This next section is for debugging when an nrow(x) error pops up
  
  if (messageLevel > 0) {
    print(paste0("attempt ", attempt))
  }
  
  #Convert the page to a usable R object
  q = read.csv(textConnection(tests), stringsAsFactors = FALSE) #treat the page as a csv and make a data.frame
  
  #Recursive call: If the http request got an error page, redo the request
  if (ncol(q) != 7) {
    if (messageLevel > 0) {
      print("e1")
    }
    q = FindEvents_1Student(
      sid = sid,
      attempt = attempt + 1,
      messageLevel = messageLevel,
      agent = agent
    )
    if (messageLevel > 0) {
      print("e2")
    }
  }
  
  if (messageLevel > 0) {
    print("h")
  }
  return(q)
} # /function
