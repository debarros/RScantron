# GetItemResponses.R

#' @title Get and Store Item Responses
#' @description Get Item Responses from the Achievement Series website and store them locally as CSVs
#' @param RecentTestFrame output from the FindTests function
#' @param TestFrame output from the FindTests function
#' @param TAB.wb the result of running loadWorkbook on the TAB file
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print.  Defaults to 0.
#' @param agent the browser user agent.  Defaults to NULL.
#' @return This function does not return anything
GetAndStoreItemResponses = function(RecentTestFrame, TestFrame, TAB.wb, startRow = 1, messageLevel = 0, agent = NULL) {
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  }
  
  if(is.null(RecentTestFrame)){
    print("The RecentTestFrame is empty.")
  } else {
    
    
    Coursecode2Testcode = read.xlsx(xlsxFile = TAB.wb, sheet = "Course Codes", startRow = 2)
    Coursecode2Course =  set_colnames(
      x = as.data.frame(t(
        read.xlsx(xlsxFile = TAB.wb,
                  sheet = "Course Codes",
                  colNames = F,
                  rowNames = F,
                  rows = 1:2)),
        stringsAsFactors = F),
      value = c("Course", "CourseCode"))
    Sections = read.xlsx(xlsxFile = TAB.wb, sheet = "Sections")
    Sections$Level[is.na(Sections$Level)] = ""
    CustomSectioning = read.xlsx(xlsxFile = TAB.wb, sheet = "CustomSectioning")
    TAB = read.xlsx(TAB.wb)
    
    # Check for testcodes missing from the TAB
    testnames = as.character(RecentTestFrame$Published.Test)
    testcodes = unique(substr(testnames,
                              start = 1,
                              stop = regexpr(pattern = " ", text = testnames) - 1))
    testcodes = testcodes[!(testcodes %in% Coursecode2Testcode$testcode)]
    if (length(testcodes) > 0) {
      stop(
        paste0("The Course Codes tab of the TAB needs rows for the following:"),
        paste0(testcodes, collapse = ", ")
      )
    }
    
    # thisrow = 1
    for (thisrow in startRow:nrow(RecentTestFrame)) {
      if (messageLevel > 0) {
        print(paste0("row ", thisrow, " of ", nrow(RecentTestFrame)))
      } # /if messageLevel > 0
      
      # Get the current test name, code, and id
      testname = as.character(RecentTestFrame$Published.Test[thisrow])
      testcode = substr(testname,
                        start = 1,
                        stop = regexpr(pattern = " ", text = testname) - 1)
      
      
      # If this line doesn't work or returns NULL, it's probably because you altered TestFrame by stepping through the UpdateTab function
      testid = TestFrame$tid[TestFrame$TestName == testname] 
      if(is.NULL(testid)){
        stop(paste0("No testid was found for ",testname,".  Did you alter the TestFrame by stepping through the UpdateTab function?"))
      }
      
      testpath = TAB$Local.folder[TAB$TestID == testid][1]
      
      currentSections = DetermineCurrentSections(
        testname = testname, CustomSectioning = CustomSectioning, Sections = Sections, testcode = testcode, 
        Coursecode2Testcode = Coursecode2Testcode, Coursecode2Course = Coursecode2Course, messageLevel = messageLevel - 1)
      
      # Determine the class names and download the item response CSV's
      classnames = paste0(currentSections$TeacherName, "_p", currentSections$Period, currentSections$Level)
      GetAndStoreItemResponses_1test(classIDs = currentSections$ClassID, classnames, testid, 
                                     testpath, messageLevel = messageLevel - 1, agent = agent)
      
    } # /for each reportable test
  }
} # /GetAndStoreItemResponses function






#' @title Get and Store Item Responses 1 Test
#' @description Get Item Responses for 1 test from the Achievement Series website and store them locally as CSVs
#' @param classIDs character vector of classIDs for class sections relevant to the current test
#' @param classnames character vector of names of the class sections (as TEACHER_p#LEVEL)
#' @param testid test ID of the desired test
#' @param testpath file folder path corresponding to the desired test
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print.  Defaults to 0.
#' @param agent the browser user agent.  Defaults to NULL.
#' @param removeOld logical, should the old exports be deleted.  Defaults to TRUE.
#' @return This function does not return anything
#' @details This function is intended to be called from other functions
GetAndStoreItemResponses_1test = function(classIDs, classnames, testid, testpath, messageLevel = 0, agent = NULL, removeOld = T) {
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  }
  
  # Clear out the old export files
  if(removeOld){
    oldExports = list.files(paste0(testpath, "/exports"), full.names = T)
    deleteSuccess = file.remove(oldExports)
    if(!all(deleteSuccess)){
      stop("Error!  Some of the old export files could not be deleted.")
    }
  }
  
  if(length(classIDs) == 0){
    stop(paste0("No classes found associated with this test!"))
  }
  
  for (classIDcounter in 1:length(classIDs)) {
    if (messageLevel > 0) {
      print(paste0("  section ", classIDcounter, " of ", length(classIDs)))
    } # /if messageLevel > 0
    currentClassID = classIDs[classIDcounter]     # get the ClassID for the section
    currentClassName = classnames[classIDcounter] # get the name for the section
    
    # download the item response file
    currentresponses = GetItemResponses_1section(ClassID = currentClassID, TestID = testid, agent = agent)
    
    # Check to make sure it worked
    if (BadReturnCheck(currentresponses, messageLevel = messageLevel - 2)) {
      stop("Error!  You are no longer logged in to Scantron.  Log in and then run this command again.")
    }
    if (TimedOutCheck(page = currentresponses, messageLevel = messageLevel - 2)) {
      stop("Error!  The request timed out.  Try again.")
    }
    if (BadSectionCheck(page = currentresponses, messageLevel = messageLevel - 2)) {
      stop(paste0("Error!  The section you tried to pull from might not exist.  Check the TAB.", 
                  "ClassID = ", currentClassID, "; ClassName = ", currentClassName))
    }
    
    
    # store it in the exports folder
    StoreItemResponses(responses = currentresponses, testpath = testpath, classname = currentClassName, messageLevel = messageLevel - 1)
  } # /for each section
} # /GetAndStoreItemResponses_1test






#' @title Get and Store Item Responses Single Test
#' @description Get Item Responses for 1 test from the Achievement Series website and store them locally as CSVs
#' @param testname character of length 1 with the name of the test
#' @param TAB.wb the result of running loadWorkbook on the TAB file
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print.  Defaults to 0.
#' @param agent the browser user agent.  Defaults to NULL.
#' @return This function does not return anything
#' @details This function was written as a wrapper for GetAndStoreItemResponses_1test
GetAndStoreItemResponses_SingleTest = function(testname, TAB.wb, messageLevel = 0, agent = NULL) {
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  }
  
  Coursecode2Testcode = read.xlsx(xlsxFile = TAB.wb, sheet = "Course Codes", startRow = 2)
  
  Coursecode2Course =  set_colnames(
    x = as.data.frame(t(
      read.xlsx(xlsxFile = TAB.wb,
                sheet = "Course Codes",
                colNames = F,
                rowNames = F,
                rows = 1:2)),
      stringsAsFactors = F),
    value = c("Course", "CourseCode"))
  
  Sections = read.xlsx(xlsxFile = TAB.wb, sheet = "Sections")
  Sections$Level[is.na(Sections$Level)] = ""
  CustomSectioning = read.xlsx(xlsxFile = TAB.wb, sheet = "CustomSectioning")
  TAB = read.xlsx(TAB.wb)
  
  testcode = substr(testname, start = 1, stop = regexpr(pattern = " ", text = testname) - 1)
  testid = TAB$TestID[TAB$TestName == testname][1]
  testpath = TAB$Local.folder[TAB$TestID == testid][1]
  currentSections = DetermineCurrentSections(
    testname = testname, CustomSectioning = CustomSectioning, Sections = Sections, testcode = testcode, 
    Coursecode2Testcode = Coursecode2Testcode, Coursecode2Course = Coursecode2Course, messageLevel = messageLevel - 1)
  classnames = paste0(currentSections$TeacherName, "_p", currentSections$Period, currentSections$Level)
  GetAndStoreItemResponses_1test(
    classIDs     = currentSections$ClassID,
    classnames   = classnames,
    testid       = testid,
    testpath     = testpath,
    messageLevel = messageLevel - 1,
    agent        = agent)
} # /GetAndStoreItemResponses_SingleTest






#' @title Get and Store Item Responses 1 section
#' @description Get Item Responses for 1 section for a test from the Achievement Series website and store them locally as CSVs
#' @param ClassID character of length 1 with the classID of the current class section
#' @param TestID test ID of the desired test
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print.  Defaults to 0.
#' @param agent the browser user agent.  Defaults to NULL.
#' @return character of length 1 with the table of item responses.  Formatted to be converted to CSV.
#' @details This function should be called from other functions
GetItemResponses_1section = function(ClassID, TestID, messageLevel = 0, agent = NULL) {
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  }
  
  url = paste0("https://admin.achievementseries.com/report/class/responses.csv?c=", ClassID, "&t=", TestID, "&v=table&_list=Students")
  responses <- httr::content(
    httr::GET(url = url, user_agent(agent)),
    as = "text",
    encoding = "UTF-8")
  # responses = httr::content(x, as = "text")
  responses = gsub(pattern = "\r\n", replacement = "\n", x = responses) # replace CRLF with LF to avoid blank lines
  return(responses)
} # /GetItemResponses_1section
