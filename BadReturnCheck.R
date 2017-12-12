# BadReturnCheck.R
# page = x
BadReturnCheck = function(page, messageLevel = 0){
  badReturnCheck = grep(pattern = "Admin Site, Enter Your Username/Password", x = page, ignore.case = T)
  badReturnCheck = badReturnCheck + grep(pattern = "position: absolute", x = page, ignore.case = T)
  badReturnCheck = badReturnCheck + grep(pattern = "header_loggedout_container", x = page, ignore.case = T)
  badReturnCheck = identical(badReturnCheck, as.integer(3))
  return(badReturnCheck)
}
