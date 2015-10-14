login = function(username = character(), 
                 password = character(), 
                 SiteCode = character(), 
                 caLocation = character()){

  #set the address for the achievement series login page
  loginurl1 = "https://admin.achievementseries.com/Auth/Login/Org"  
  loginurl2 = "https://admin.achievementseries.com/Auth/?returnUrl=%2FAuth%2FLogin%2FUser"
  
  #if the location of the certificate file is not set, download the file
  if(length(caLocation) == 0){
    download.file(url = "http://curl.haxx.se/ca/cacert.pem", 
                  destfile = "~/cacert.pem.crt", 
                  method = "auto", 
                  quiet = TRUE, 
                  mode = "w")
    caLocation = path.expand("~/cacert.pem.crt")
  }
  
  #Prompt User for Input where necessary
  if(length(SiteCode) == 0){SiteCode = readline(prompt="Site ID: ")}
  if(length(username) == 0){username = readline(prompt="Staff ID: ")}  
  if(length(password) == 0){password = readline(prompt="password: ")}
  
  #Set the "agent" (the info that tells web servers what browsers we are using)
  agent="Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36" 
  
  #Set RCurl parameters
  Options <- curlOptions(
    cainfo = caLocation, 
    useragent = agent,
    followlocation = TRUE,
    verbose = FALSE,
    cookiejar = ""  
  )
  ScantronHandle = getCurlHandle()
  curlSetOpt(.opts = Options, curl = ScantronHandle)  
  
  
  
  #Login step 0: Load the login page
  x = getURI(url = loginurl1, curl = ScantronHandle)
  
  
  
  #Login step 1: Enter the siteID
  Token = getToken(x)
  pars=list("SiteID" = SiteCode,
            "returnUrl" = "/Auth/Login/User",
            "__RequestVerificationToken" = Token)
  x = postForm(uri = loginurl1, .params = pars, curl=ScantronHandle, .checkParams = FALSE)
  
  
  
  #Log In step 2: enter the username and password
  OrgID = getOrgID(x)
  pars=list(
    "Username" = username,
    "Password" = password,
    "returnUrl" = "/Auth/Login/User",
    "__RequestVerificationToken" = Token,
    "OrganizationId" = OrgID
  )
  x=postForm(uri = loginurl2, .params = pars, curl=ScantronHandle, .checkParams = FALSE)
  
  return(ScantronHandle)
}