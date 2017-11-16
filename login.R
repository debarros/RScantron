#login.R

# This function logs into the scantron website
#
# Arguments are:
#  username - character, username for the scantron account
#  password - character, password for the scantron account
#  SiteCode - character, the site code for the scantron account
#  caLocation - character, file path and name for the certification file
#  getNewCert - logical, whether a new certification file should be downloaded
#  ssl.verifypeer - logical, whether curl should verify the identity of the server
#
# Returned value is:
#  ScantronHandle - object of class CURLHandle, holding information about the session with the server

login = function(username = character(), 
                 password = character(), 
                 SiteCode = character(), 
                 caLocation = "cacert.pem",
                 getNewCert = F, 
                 ssl.verifypeer = TRUE,
                 messageLevel = 0){
  
  #Get set up ####
  
  #set the address for the achievement series login page
  loginurl1 = "https://admin.achievementseries.com/Auth/Login/Org"  
  loginurl2 = "https://admin.achievementseries.com/Auth/?returnUrl=%2FAuth%2FLogin%2FUser"
  
  
  # If a new certification file is needed, download it
  if(getNewCert){
    caLocation = ObtainNewCert(caLocation)
  } else if(length(caLocation) == 0){
    caLocation = ObtainNewCert(caLocation)
  } else if(!file.exists(caLocation)){
    caLocation = ObtainNewCert(caLocation)
  }
  
  
  # Prompt User for Input where necessary
  if(length(SiteCode) == 0){SiteCode = readline(prompt="Site ID: ")}
  if(length(username) == 0){username = readline(prompt="Staff ID: ")}  
  if(length(password) == 0){password = readline(prompt="password: ")}
  
  # Set the "agent" (the info that tells web servers what browsers we are using)
  agent="Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36" 
  
  # Set RCurl parameters
  Options <- curlOptions(
    cainfo = caLocation, 
    useragent = agent,
    followlocation = TRUE,
    verbose = FALSE,
    cookiejar = "",
    ssl.verifypeer = ssl.verifypeer
  )
  ScantronHandle = getCurlHandle()
  curlSetOpt(.opts = Options, curl = ScantronHandle)  
  
  
  
  # Login step 0 ####
  # Load the login page
  x = getURI(url = loginurl1, curl = ScantronHandle)
  
  
  
  # Login step 1 ####
  # Enter the siteID
  Token = getToken(x)
  pars=list("SiteID" = SiteCode,
            "returnUrl" = "/Auth/Login/User",
            "__RequestVerificationToken" = Token)
  x = postForm(uri = loginurl1, .params = pars, curl=ScantronHandle, .checkParams = FALSE)
  
  
  
  # Login step 2 #### 
  # enter the username and password
  OrgID = getOrgID(x)
  pars=list(
    "Username" = username,
    "Password" = password,
    "returnUrl" = "/Auth/Login/User",
    "__RequestVerificationToken" = Token,
    "OrganizationId" = OrgID
  )
  x=postForm(uri = loginurl2, .params = pars, curl=ScantronHandle, .checkParams = FALSE)
  
  # Return ####
  return(ScantronHandle)
}