# BadSectionCheck.R
# page = x

# Given a page pulled from the Scantron website, this function determines whether you tried to get a section that doesn't exist
# 

#' @title Bad Section Check
#' @description Check whether you got what you wanted from the Scantron website using the
#'   page returned by an http call
#' @param page the content of a page pull from the Scantron website
#' @param messageLevel integer of length 1 indicating level of diagnostic
#'   messages to print
#' @return Logical.  If you the section doesn't exist, it returns TRUE.  Otherwise, FALSE.
#' @details This function checks for several different text strings that might
#'   appear in a response from a call to a nonexistant section.
BadSectionCheck = function(page, messageLevel = 0){
  
  if(messageLevel > 0){
    print("Checking to see if it's a valid section.")
  }
  
  badsectCheck = rep(F,4)
#  badsectCheck[1] = grepl(pattern = "<title>The wait operation timed out</title>", x = page, ignore.case = T)
  badsectCheck[2] = grepl(pattern = "does not have permission to view ClassId", x = page, ignore.case = T)
#  badsectCheck[3] = grepl(pattern = "Please review the stack trace", x = page, ignore.case = T)
#  badsectCheck[4] = grepl(pattern = "A network-related or instance-specific error occurred while establishing a connection", x = page, ignore.case = T)
  
  badsectCheck = any(badsectCheck)
  
  return(badsectCheck)
} #/BadSectionCheck function
