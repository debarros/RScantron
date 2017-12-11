#Get Item Responses

GetAndStoreItemResponses = function(RecentTestFrame, TestFrame, TAB.wb, ScantronHandle, messageLevel = 0){
  Coursecode2Testcode = read.xlsx(xlsxFile = TAB.wb, sheet = "Course Codes", startRow = 2)
  Coursecode2Course =  set_colnames(
    x = as.data.frame(
      t(read.xlsx(xlsxFile = TAB.wb, 
                  sheet = "Course Codes", colNames = F, rowNames = F, rows = 1:2)), 
      stringsAsFactors = F), 
    value = c("Course","CourseCode"))
  Sections = read.xlsx(xlsxFile = TAB.wb, sheet = "Sections")
  Sections$Level[is.na(Sections$Level)] = ""
  CustomSectioning = read.xlsx(xlsxFile = TAB.wb, sheet = "CustomSectioning")
  TAB = read.xlsx(TAB.wb)
  
  # Check for testcodes missing from the TAB
  testnames = as.character(RecentTestFrame$Published.Test)
  testcodes = unique(substr(testnames, start = 1, stop = regexpr(pattern = " ", text = testnames) - 1))
  testcodes = testcodes[!(testcodes %in% Coursecode2Testcode$testcode)]
  if(length(testcodes) > 0){
    stop(paste0("The Course Codes tab of the TAB needs rows for the following:"), paste0(testcodes, collapse = ", "))
  }
  
  for(i in 1:nrow(RecentTestFrame)){
    
    if(messageLevel > 0){ print(paste0("row ",i," of ",nrow(RecentTestFrame))) }
    
    # Get the current test name, code, and id
    testname = as.character(RecentTestFrame$Published.Test[i])
    testcode = substr(testname, start = 1, stop = regexpr(pattern = " ", text = testname) - 1)
    testid = TestFrame$tid[TestFrame$TestName == testname]
    testpath = TAB$Local.folder[TAB$TestID == testid][1]
    
    
    currentSections = DetermineCurrentSections(testname, CustomSectioning, Sections, testcode, Coursecode2Testcode, Coursecode2Course)
    
    # Determine the class names and download the item response CSV's
    classnames = paste0(currentSections$TeacherName,"_p", currentSections$Period, currentSections$Level)
    GetAndStoreItemResponses_1test(classIDs = currentSections$ClassID, 
                                   classnames, testid, testpath, ScantronHandle, 
                                   messageLevel = messageLevel - 1)
    
  } # /for each reportable test
} # /GetAndStoreItemResponses



GetAndStoreItemResponses_1test = function(classIDs, classnames, testid, testpath, ScantronHandle, messageLevel = 0){
  for(j in 1:length(classIDs)){
    currentClassID = classIDs[j]     # get the ClassID for the section
    currentClassName = classnames[j] # get the name for the section
    
    # download the item response file
    currentresponses = GetItemResponses_1section(ClassID = currentClassID, TestID = testid, curlhandle = ScantronHandle)
    
    # Check to make sure it worked
    if(BadReturnCheck(currentresponses)){
      stop("Error!  You are no longer logged in to Scantron.  Log in and then run this command again.")
    }
    
    # store it in the exports folder
    StoreItemResponses(responses = currentresponses, testpath = testpath, classname = currentClassName)
  } # /for each section
} # /GetAndStoreItemResponses_1test



GetAndStoreItemResponses_SingleTest = function(testname, TAB.wb, messageLevel = 0){
  Coursecode2Testcode = read.xlsx(xlsxFile = TAB.wb, sheet = "Course Codes", startRow = 2)
  Coursecode2Course =  set_colnames(
    x = as.data.frame(
      t(read.xlsx(xlsxFile = TAB.wb, 
                  sheet = "Course Codes", colNames = F, rowNames = F, rows = 1:2)), 
      stringsAsFactors = F), 
    value = c("Course","CourseCode"))
  Sections = read.xlsx(xlsxFile = TAB.wb, sheet = "Sections")
  Sections$Level[is.na(Sections$Level)] = ""
  CustomSectioning = read.xlsx(xlsxFile = TAB.wb, sheet = "CustomSectioning")
  TAB = read.xlsx(TAB.wb)
  
  testcode = substr(testname, start = 1, stop = regexpr(pattern = " ", text = testname) - 1)
  testid = TAB$TestID[TAB$TestName == testname][1]
  testpath = TAB$Local.folder[TAB$TestID == testid][1]
  currentSections = DetermineCurrentSections(testname, CustomSectioning, Sections, testcode, Coursecode2Testcode, Coursecode2Course)
  classnames = paste0(currentSections$TeacherName,"_p", currentSections$Period, currentSections$Level)
  GetAndStoreItemResponses_1test(currentSections$ClassID, classnames, testid, testpath, ScantronHandle)
} # /GetAndStoreItemResponses_SingleTest



GetItemResponses_1section = function(ClassID, TestID, curlhandle, messageLevel = 0){
  responses = getURI(
    paste0(
      "https://admin.achievementseries.com/report/class/responses.csv?",
      "c=", ClassID, 
      "&t=", TestID, 
      "&v=table&_list=Students"), 
    curl=curlhandle)
  responses = gsub(pattern = "\r\n", replacement = "\n",x = responses) # replace CRLF with LF to avoid blank lines
  return(responses)
} # /GetItemResponses
