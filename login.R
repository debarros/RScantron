login = function(username = character(), 
                 password = character(), 
                 SiteCode = character(), 
                 caLocation = character()){

  library(XML)
  library(RCurl)
  library(stringr)
  library(reshape2)
  
  #set the address for the achievement series login page
  loginurl = "https://admin.achievementseries.com/!/login.ssp"  
  
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
  
  #Set user account data and agent
  pars=list(
    "JavaScriptTest" = "yes",
    "username"= username,
    "password"= password,
    "SiteCode" = SiteCode,
    "_PageAction" = "o"
  )
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
  
  #Load the login page
  x = getURI(url = 'https://admin.achievementseries.com/!/login.ssp', 
             curl = ScantronHandle)
  
  #Log In
  x=postForm(loginurl, .params = pars, curl=ScantronHandle, .checkParams = FALSE)
  
  return(ScantronHandle)
}