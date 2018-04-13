# BadReturnCheck.R
# page = x

# Given a page pulled from the Scantron website, this function determines whether you are logged out
# If you are logged out, it returns TRUE.  Otherwise, FALSE.
BadReturnCheck = function(page, messageLevel = 0){
  
  if(messageLevel > 0){
    print("Checking to see if you are logged in.")
  }
  
  badReturnCheck = grep(pattern = "Admin Site, Enter Your Username/Password", x = page, ignore.case = T)
  if(length(badReturnCheck) == 0){
    badReturnCheck = grep(pattern = "Admin Site, Enter Your Site ID", x = page, ignore.case = T)
  }
  badReturnCheck = badReturnCheck + grep(pattern = "position: absolute", x = page, ignore.case = T)
  badReturnCheck = badReturnCheck + grep(pattern = "header_loggedout_container", x = page, ignore.case = T)
  badReturnCheck = identical(badReturnCheck, as.integer(3))
  return(badReturnCheck)
} #/BadReturnCheck function
