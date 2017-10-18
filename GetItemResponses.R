#Get Item Responses

GetAndStoreItemResponses = function(RecentTestFrame, TestFrame, TAB, ScantronHandle, Coursecode2Testcode, Coursecode2Course, Sections, CustomSectioning){
  # Check for testcodes missing from the TAB
  testnames = as.character(RecentTestFrame$Published.Test)
  testcodes = unique(substr(testnames, start = 1, stop = regexpr(pattern = " ", text = testnames) - 1))
  testcodes = testcodes[!(testcodes %in% Coursecode2Testcode$testcode)]
  if(length(testcodes) > 0){
    stop(paste0("The Course Codes tab of the TAB needs rows for the following:"), paste0(testcodes, collapse = ", "))
  }
  
  for(i in 1:nrow(RecentTestFrame)){
    # Get the current test name, code, and id
    testname = as.character(RecentTestFrame$Published.Test[i])
    testcode = substr(testname, start = 1, stop = regexpr(pattern = " ", text = testname) - 1)
    testid = TestFrame$tid[TestFrame$TestName == testname]
    testpath = TAB$Local.folder[TAB$TestID == testid][1]
    
    # determine the courses associated with this test code
    coursecodes = colnames(Coursecode2Testcode)[Coursecode2Testcode[Coursecode2Testcode$testcode == testcode,] == 1]
    courses = Coursecode2Course$Course[Coursecode2Course$CourseCode %in% coursecodes]
    
    # determine the sections of this course
    currentSections = Sections[Sections$ClassName %in% courses,]
    classIDs = currentSections$ClassID
    classnames = paste0(currentSections$TeacherName,"_p", currentSections$Period, currentSections$Level)
    GetAndStoreItemResponses_1test(classIDs, classnames, testid, testpath, ScantronHandle)
    
  } #/for each reportable test
}



GetAndStoreItemResponses_1test = function(classIDs, classnames, testid, testpath, ScantronHandle){
  for(j in 1:length(classIDs)){
    currentClassID = classIDs[j] # get the ClassID for the section
    currentClassName = classnames[j]
    
    # download the item response file
    currentresponses = GetItemResponses_1section(ClassID = currentClassID, TestID = testid, curlhandle = ScantronHandle)
    
    # store it in the exports folder
    StoreItemResponses(responses = currentresponses, testpath = testpath, classname = currentClassName)
  } #/for each section
}


GetAndStoreItemResponses_SingleTest = function(testname, TAB, Coursecode2Testcode, Coursecode2Course){
  testcode = substr(testname, start = 1, stop = regexpr(pattern = " ", text = testname) - 1)
  testid = TAB$TestID[TAB$TestName == testname][1]
  testpath = TAB$Local.folder[TAB$TestID == testid][1]
  
  # determine the courses associated with this test code
  coursecodes = colnames(Coursecode2Testcode)[Coursecode2Testcode[Coursecode2Testcode$testcode == testcode,] == 1]
  courses = Coursecode2Course$Course[Coursecode2Course$CourseCode %in% coursecodes]
  currentSections = Sections[Sections$ClassName %in% courses,]
  classIDs = currentSections$ClassID
  classnames = paste0(currentSections$TeacherName,"_p", currentSections$Period, currentSections$Level)
  GetAndStoreItemResponses_1test(classIDs, classnames, testid, testpath, ScantronHandle)
}




GetItemResponses_1section = function(ClassID, TestID, curlhandle){
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
