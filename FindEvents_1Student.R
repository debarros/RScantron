#FindEvents_1Student.R

FindEvents_1Student = function(sid, ScantronHandle, attempt = 1){
  
  print(paste0("Attempt ",attempt))
  tests = getURI(
    paste0(
      'https://admin.achievementseries.com/student/test-results.csv?id=',
      sid,
      '&_list=TestResults'),
    curl=ScantronHandle)
  
  # This next section is for debugging when an nrow(x) error pops up
  write(tests, file = "~/testpageoutput.html")
  
  
  #Convert the page to a usable R object
  q = read.csv(textConnection(tests), stringsAsFactors = FALSE) #treat the page to a csv and make a data.frame
  q = q[,-c(2,5,7)] #remove unnecessary columns
  
  
  #Recursive call: If the http request got an error page, redo the request
  if(dim(q)[2] == 1){
    q = FindEvents_1Student(sid, ScantronHandle, attempt = attempt+1)
  }
  
  return(q)
}