# FindMissingTests.R

#' @title Find Missing Tests
#' @description Identify tests that have recent events, but are not included in
#'   the TAB (Test Address Book).
#' @param RecentTestFrame output from the FindTests function.
#' @param TAB.wb the result of running loadWorkbook on the TAB file.
#' @param TestFrame output from the FindTests function.
#' @param messageLevel integer of length 1 indicating level of diagnostic
#'   messages to print.  Defaults to 0.
#' @return character vector whose elements are the names of tests included in
#'   \code{RecentTestFrame} but which are not in the TAB.
#' @details This functions checks for tests that are not included in the TAB, or
#'   that are listed with a different testID in the TAB than in Scantron.
#'   Diagnostic messages get printed.
FindMissingTests = function(RecentTestFrame, TAB.wb, TestFrame, messageLevel = 0){
  
  if(messageLevel > 0){ print("Finding missing tests.")}
  
  TAB = read.xlsx(TAB.wb)
  
  # Check for tests not included in the tab
  missingTests = RecentTestFrame$Published.Test[!(RecentTestFrame$Published.Test %in% TAB$TestName)]
  if(length(missingTests) > 0){
    if(length(missingTests) == 1){
      message1a = "The following test is missing from the TAB."
      message1b = "You should add it by running UpdateTab."
    } else {
      message1a = "The following tests are missing from the TAB."
      message1b = "You should add them by running UpdateTab."
    }
    print(message1a)
    print(as.character(missingTests))
    print(message1b)
  } # /if
  
  # Check for tests with a different testID in the TAB than in Scantron
  TAB2 = TAB[TAB$TestName %in% TestFrame$TestName,]
  TAB2$ScantronTestID = TestFrame$tid[match(x = TAB2$TestName, table = TestFrame$TestName)]
  badTests = TAB2[TAB2$TestID != TAB2$ScantronTestID, c("TestName", "ScantronTestID")]
  if(nrow(badTests) > 0){
    if(nrow(badTests) == 1){
      message2 = "The following test has a different TestID in the TAB than in Scantron."
    } else {
      message2 = "The following tests have a different TestID in the TAB than in Scantron."
    }
    print(message2)
    print(badTests)
    print("You should figure that out.")
  } # /if
  
  if(length(missingTests) + nrow(badTests) == 0){
    print("No missing tests or conflicting testIDs were found.")
  }
  
  return(missingTests)
  
} # /function
