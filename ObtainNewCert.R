#ObtainNewCert.R

# This function downloads a new certification file
# If the caLocation exists, it uses that location
# Otherwise, it sets the folder path and file name to the default

ObtainNewCert = function(caLocation, messageLevel = 0){
  
  if(messageLevel > 0){
    print("Checking for certification file")
  }
  
  if(length(caLocation) == 0){
    caLocation = "cacert.pem.crt"
    download.file(url = "http://curl.haxx.se/ca/cacert.pem", 
                  destfile = caLocation, 
                  method = "auto", 
                  quiet = TRUE, 
                  mode = "w")
  }
  return(caLocation)
}