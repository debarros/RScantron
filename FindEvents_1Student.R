#FindEvents_1Student.R

#' @title Find Events
#' @description Find testing events in Achievement Series
#' @param sid character of length 1 holding the sid (internal student id code in
#'   Achievement Series) for a student.
#' @param attempt numeric of length 1 representing how many tries (including the
#'   current one) have been made to retrieve this student's testing events.
#' @param messageLevel integer of length 1 indicating level of diagnostic
#'   messages to print.  Defaults to 0.
#' @param agent the browser user agent.  Defaults to NULL, in which case a
#'   specific one is used.
#' @return data.frame with 1 row for each testing associated with the given
#'   student event and 7 columns holding various information about each testing
#'   event.
FindEvents_1Student = function(sid, attempt = 1, messageLevel = 0, agent = NULL) {
  
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  }
  
  tests <- httr::content(
    httr::GET(
      url = paste0('https://admin.achievementseries.com/student/test-results.csv?id=',sid,'&_list=TestResults'),
      user_agent(agent)),
    as = "text",
    encoding = "UTF-8")
  
  # tests <- httr::content(tests, as = "text")
  
  write(tests, file = "~/testpageoutput.html") # This next section is for debugging when an nrow(x) error pops up
  
  if (messageLevel > 0) {
    print(paste0("attempt ", attempt))
  }
  
  # Convert the page to a usable R object
  q = read.csv(textConnection(tests), stringsAsFactors = FALSE) # treat the page as a csv and make a data.frame
  
  # Recursive call: If the http request got an error page, redo the request
  if (ncol(q) != 7) {
    if (messageLevel > 0) {
      print("e1")
    }
    q = FindEvents_1Student(
      sid = sid,
      attempt = attempt + 1,
      messageLevel = messageLevel,
      agent = agent)
    if (messageLevel > 0) {
      print("e2")
    }
  } # /if ncol(q) != 7
  
  if (messageLevel > 0) {
    print("h")
  }
  return(q)
} # /FindEvents_1Student function
