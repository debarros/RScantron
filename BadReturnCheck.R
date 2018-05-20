# BadReturnCheck.R
# page = x

# Given a page pulled from the Scantron website, this function determines whether you are logged out
# If you are logged out, it returns TRUE.  Otherwise, FALSE.
BadReturnCheck = function(page, messageLevel = 0){
  
  if(messageLevel > 0){
    print("Checking to see if you are logged in.")
  }
  
  badReturnCheck[1] = grepl(pattern = "Admin Site, Enter Your Username/Password", x = page, ignore.case = T)
  badReturnCheck[2] = grepl(pattern = "Admin Site, Enter Your Site ID", x = page, ignore.case = T)
  badReturnCheck[3] = grepl(pattern = "header_loggedout_container", x = page, ignore.case = T)
  badReturnCheck[4] = grepl(pattern = "Inactivity - You have been logged out", x = page, ignore.case = T)
  
  badReturnCheck = any(badReturnCheck)
  
  return(badReturnCheck)
} #/BadReturnCheck function
