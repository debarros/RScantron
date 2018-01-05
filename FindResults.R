#FindResults.R

FindResults_1test = function(tid,
                             attempt = 1,
                             messageLevel = 0,
                             agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"){
  scores = httr::content(httr::GET(url = paste0('https://admin.achievementseries.com/published-test/export/csv-list.csv?',
                                                'id=',tid,
                                                '&d=F&t=T&r=F&p=F&_list=ExportList'),
                                   user_agent(agent)),
                         as = "text",
                         encoding = "UTF-8")
  
  # Check to make sure it worked
  if(BadReturnCheck(scores, messageLevel - 1)){
    stop("Error!  You are no longer logged in to Scantron.  Log in and then run this command again.")
  }
  
  q = read.csv(textConnection(scores), stringsAsFactors = FALSE) #treat the page to a csv and make a data.frame
  
  #Recursive call: If the http request got an error page, redo the request
  print(paste0("length(q) is ",length(q), " and this is Attempt ",attempt))
  if(length(q) != 18){
    q = FindResults_1test(tid, attempt = attempt+1, messageLevel = messageLevel-1, agent = agent)
  }
  
  print(paste0("q has ",ncol(q)," columns."))
  
  return(q)
} # /function



FindResults = function (testIDs,
                        messageLevel = 0,
                        agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"){
  ResultFrame = as.data.frame(matrix(data = character(), nrow = 0, ncol = 18), stringsAsFactors = FALSE)
  for (i in 1:length(testIDs)){
    print(paste0("Test ",i," of ",length(testIDs)))
    tid = testIDs[i]
    x = FindResults_1test(tid = tid, messageLevel = messageLevel-1, agent = agent)
    if(nrow(x)>0){ResultFrame = rbind(ResultFrame,x)}
  }
  ResultFrame$ReportingDate = as.Date(ResultFrame$ReportingDate, "%m/%d/%y")
  ResultFrame$TestAdministrationStartDate = as.Date(ResultFrame$TestAdministrationStartDate, "%m/%d/%y")
  ResultFrame$Score = ResultFrame$PointsAttained / ResultFrame$PointsPossible
  return(ResultFrame)
} # /function


