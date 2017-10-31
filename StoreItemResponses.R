#Store Item Responses

StoreItemResponses = function(responses, testpath, classname, datafolder = "exports"){
  datapath = paste0(testpath, "/", datafolder)                      # Build the path to the datafolder
  classname = gsub(pattern = "/", replacement = "+", x = classname) # Replace forward slashes with plusses in the class name
  filename = paste0(datapath, "/", classname, "_itemresponses.csv") # Build the filename
  if(!dir.exists(datapath)){dir.create(datapath)}                   # If the datafolder doesn't exist, create it
  cat(responses, file = filename)                                   # Save the file to the datafolder
} # /function
