##########
# login.R
#
# This function duplicates the login function written using RCurl
# Arguments are:
#  username - character, username for the scantron account
#  password - character, password for the scantron account
#  SiteCode - character, the site code for the scantron account
#  messageLevel - integer, indicates depth of console info
#
# certain arguments are no longer needed, including:
#   - caLocation
#   - getNewCert
#   - ssl.verifypeer
#
# Return value is also omitted: httr keeps a handle pool so handles do not need to be passed around
### ### ###

login = function(loginurls,
                 username = character(),
                 password = character(),
                 SiteCode = character(),
                 messageLevel = 0,
                 agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36") {
  #######
  # SETUP
  ### ###
  
  if (messageLevel > 0) {
    print("Starting login process")
  }
  
  #set the address for the achievement series login page
  loginurl1 = loginurls$step1
  loginurl2 = loginurls$step2
  
  # Prompt User for Input where necessary
  if (length(SiteCode) == 0) {
    SiteCode = rstudioapi::askForPassword(prompt = "Site ID: ")
  }
  if (length(username) == 0) {
    username = rstudioapi::askForPassword(prompt = "Staff ID: ")
  }
  if (length(password) == 0) {
    password = rstudioapi::askForPassword(prompt = "password: ")
  }
  
  if (messageLevel > 0) {
    print("Getting Login Page")
  }
  x <- httr::GET(url = loginurl1, user_agent(agent))
  
  #######
  # STEP 1: ENTER SITE ID
  ### ###
  
  if (messageLevel > 0) {
    print("Entering siteID")
  }
  Tok <- getToken(x)
  pars = list(
    "SiteID" = SiteCode,
    "returnUrl" = "/Auth/Login/User",
    "__RequestVerificationToken" = Tok
  )
  
  x <-
    httr::POST(url = loginurl1,
               user_agent(agent),
               body = pars,
               encode = "multipart")
  
  #######
  # STEP 2: ENTER CREDENTIALS
  ### ###
  
  if (messageLevel > 0) {
    print("Entering Username, PW")
  }
  OrgID = getOrgID(x)
  pars = list(
    "Username" = username,
    "Password" = password,
    "returnUrl" = "/Auth/Login/User",
    "__RequestVerificationToken" = Tok,
    "OrganizationId" = OrgID
  )
  x <-
    httr::POST(url = loginurl2,
               user_agent(agent),
               body = pars,
               encode = "multipart")
  
  if (messageLevel > 0) {
    print("Finishing login function")
  }
}
