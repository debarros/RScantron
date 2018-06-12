# GetDraftName.R

#' @title Get Draft Name
#' @description Get the name of the test draft given the id of the published test
#' @param tid testID of the desired published test
#' @param agent the browser user agent
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print
#' @return character of length 1 containing the name of the draft test
#' @details This function takes a published test id and returns the name of the draft on which the published test is based
GetDraftName = function(tid, agent = NULL, messageLevel = 0){
  
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  }
  
  
  urlstub = "https://admin.achievementseries.com/published-test/info.ssp?id=" # url stub for getting published test pages
  url = paste0(urlstub, tid)                                                  # build the url for the current test
  TestPage = httr::content(httr::GET(url = url, user_agent(agent)),           # fetch the page
                           as = "text",
                           encoding = "UTF-8")
  
  # Check to make sure it worked
  if (BadReturnCheck(TestPage, messageLevel - 2)) {
    stop("Error!  You are no longer logged in to Scantron.  Log in and then run this command again.")
  }
  
  DraftNameStart = gregexpr("Test Draft Name", TestPage)[[1]][1] + 82         # find where the draft name starts
  SpanLocations = c(gregexpr("</span>", TestPage)[[1]])                       # find all instances of <\span>
  DraftNameEnd = min(SpanLocations[SpanLocations > DraftNameStart]) - 1       # find where the draft name ends
  DraftName = substr(TestPage, DraftNameStart, DraftNameEnd)                  # get the draft name
  DraftName = FixHtmlChars(DraftName)                                         # clean the draft name
  
  return(DraftName)
  
} # /function
