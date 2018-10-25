#Store Item Responses

#' @title Store Item Responses
#' @description Store item response files downloaded from the TMS
#' @param responses output from GetItemResponses_1section
#' @param testpath file folder path corresponding to the desired test
#' @param classname character of length 1 holding the name of the section (as TEACHER_p#LEVEL)
#' @param datafolder the subfolder in which to store the exports.  Defaults to "exports".
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print
#' @return Nothing gets returned by this function
#' @details This takes an R object holding the table of item responses for given section on a given test and save it as a csv
StoreItemResponses = function(responses, testpath, classname, datafolder = "exports", messageLevel = 0){
  
  if (messageLevel > 0) {
    print(paste0("    storing ", classname, " in ", testpath))
  } # /if messageLevel > 0
  
  if(!file.exists(testpath)){
    stop(paste0("The local folder path for this test (according to the TAB) does not exist. \n", testpath))
  }
  
  datapath = paste0(testpath, "/", datafolder)                      # Build the path to the datafolder
  classname = gsub(pattern = "/", replacement = "+", x = classname) # Replace forward slashes with plusses in the class name
  filename = paste0(datapath, "/", classname, "_itemresponses.csv") # Build the filename
  if(!dir.exists(datapath)){dir.create(datapath)}                   # If the datafolder doesn't exist, create it
  cat(responses, file = filename)                                   # Save the file to the datafolder
  
} # /function
