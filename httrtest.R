##########
# httrTest.R
#
# This function duplicates the login function written using RCurl
#
# certain arguments are no longer needed, including:
#   - caLocation
#   - getNewCert
#   - ssl.verifypeer
#
# Return value is also omitted: httr keeps a handle pool so handles do not need to be passed around
#
### ### ###

hlogin = function(loginurls,
                  username = character(), 
                  password = character(), 
                  SiteCode = character(), 
                  messageLevel = 0){
  
  print("Starting login process")
  
  #######
  # SETUP
  ### ###
  
  #set the address for the achievement series login page
  loginurl1 = loginurls$step1
  loginurl2 = loginurls$step2
  
  # Prompt User for Input where necessary
  if(length(SiteCode) == 0){SiteCode = rstudioapi::askForPassword(prompt="Site ID: ")}
  if(length(username) == 0){username = rstudioapi::askForPassword(prompt="Staff ID: ")}  
  if(length(password) == 0){password = rstudioapi::askForPassword(prompt="password: ")}
  
  # Set the "agent" (the info that tells web servers what browsers we are using)
  agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36" 
  
  print("Getting Login Page")
  x <- httr::GET(url = loginurl1, user_agent(agent))
  
  #
  # STEP 1: ENTER SITE ID
  #
  print("Entering siteID")
  Tok <- getToken(x)
  Tok
  pars=list("SiteID" = SiteCode,
            "returnUrl" = "/Auth/Login/User",
            "__RequestVerificationToken" = Tok)
  
  x <- httr::POST(url = loginurl1, user_agent(agent), body = pars, encode = "multipart")
  
  #
  # STEP 2: ENTER CREDENTIALS
  #
  print("Entering Username, PW")
  OrgID = getOrgID(x)
  pars=list(
    "Username" = username,
    "Password" = password,
    "returnUrl" = "/Auth/Login/User",
    "__RequestVerificationToken" = Tok,
    "OrganizationId" = OrgID)
  x <- httr::POST(url = loginurl2, user_agent(agent), body = pars, encode = "multipart")
  print("Finishing login function")
  return(x$handle)
}