# R/Scantron Interface
# by Paul de Barros (pj.deBarros@gmail.com)
# Repository located at https://github.com/debarros/RScantron

#------------------#
#### Initialize ####
#------------------#
gc() #garbage collection clears data no longer being used from memory
source("functions.R") #load the functions
source("credentials.R") 


#--------------#
#### Log in ####
#--------------#

# log in to scantron
ScantronHandle = login(username, password, SiteCode, caLocation)

# Sign in to google
gs_auth() # this might launch a browser so you can sign into your account
ScannedTests.url = gs_url(ScannedTests.url.text) #enter the URL of the scanned test document here


#-----------------------------------------#
#### Determine current reporting needs ####
#-----------------------------------------#

StudentFrame = FindStudents(ScantronHandle) # Get the complete list of students
EventFrame = FindEvents(StudentFrame, ScantronHandle, schoolYear()) # Get the complete list of instances in which a student has taken a test

# Compare new event frame to old event frame and subset to the recent events
RecentEventFrame = FindRecentEvents(EventFrame = EventFrame, TAB = list(TAB.wb, TABpath), status = "Finished")

#Get a list of the recently scanned tests, and how many instances per test
RecentTestFrame = FindRecentTests(RecentEventFrame)

# Get the complete list of tests with their test ID's and containing folders
TestFolderFrame = FindFolders(ScantronHandle, "t", SkipTestFolder)
TestFrame = FindTests(TestFolderFrame)


# Check for tests not included in the tab
missingTests = RecentTestFrame$Published.Test[!(RecentTestFrame$Published.Test %in% TAB$TestName)]
print(missingTests)

# If there are any missing tests, add them to the TAB
if(length(missingTests) > 0){ 
  TestFrame.temp = TestFrame
  TestFrame$Local.folder = NA_character_
  TestFrame = TestFrame[,c(3,1,2,4)]
  colnames(TestFrame) = colnames(TAB)
  NewTestFrame = rbind.data.frame(TAB, TestFrame)
  NewTestFrame = NewTestFrame[!duplicated(NewTestFrame$TestName),]
  NewTestFrame = NewTestFrame[order(NewTestFrame$TestName),]
  for(i in 1:ncol(NewTestFrame)){
    NewTestFrame[,i] = na.to.empty(NewTestFrame[,i])
  }
  writeData(wb = TAB.wb, sheet = "TAB", x = NewTestFrame)
  saveWorkbook(wb = TAB.wb, file = "\\\\stuthin2/Data/tests/2017-2018/TAB.xlsx", overwrite = T)
  TestFrame = TestFrame.temp
} # /if there are missing tests

#Reload the TAB
TAB = readWorkbook(xlsxFile = TABpath, sheet = "TAB")

# Download the item response files and save them
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
  
  for(j in 1:nrow(currentSections)){
    currentClassID = currentSections$ClassID[j] # get the ClassID for the section
    currentClassName = paste0(currentSections$TeacherName[j],"_p", currentSections$Period[j])
    
    # download the item response file
    currentresponses = GetItemResponses(ClassID = currentClassID, TestID = testid, curlhandle = ScantronHandle)
    
    # store it in the exports folder
    StoreItemResponses(responses = currentresponses, testpath = testpath, classname = currentClassName)
  } #/for each section
} #/for each reportable test

# Log out of scantron
LogoutPage = logout(ScantronHandle)

# Generate the reports
for(i in 1:nrow(RecentTestFrame)){
  DataLocation = TAB$Local.folder[TAB$TestName == RecentTestFrame$Published.Test[i]]
  generateReport(DataLocation = DataLocation, TMS = "ScantronAS")
}

#--------------------------#
#### Monitoring section ####
#--------------------------#

# Get the current Scanned Tests document and then clear it out
ScannedTests = gs_read(ss = ScannedTests.url, ws = 1)
if(nrow(ScannedTests) > 0){
  Blank = matrix(data = "", nrow = nrow(ScannedTests), ncol = ncol(ScannedTests)) 
  gs_edit_cells(ss = ScannedTests.url, ws = 1, input = Blank, anchor = "A2") # Start at A2 to leave the header row in place
}

# Modify ScannedTests to include the newly scanned tests
NewScannedTests = data.frame(Test = RecentTestFrame$Published.Test, Folder = TAB$Local.folder[match(RecentTestFrame$Published.Test,TAB$TestName)])
if(nrow(NewScannedTests) > 0){
  NewScannedTests$Analyze = T
  NewScannedTests$Update = F
  NewScannedTests$Monitor = T
}


AllScannedTests = rbind(ScannedTests, NewScannedTests)
UniqueScannedTests = AllScannedTests[!duplicated(AllScannedTests$Test),]
for(i in 1:nrow(UniqueScannedTests)){
  for(j in c("Analyze","Update","Monitor")){
    UniqueScannedTests[i,j] = any(unlist(AllScannedTests[AllScannedTests$Test == UniqueScannedTests$Test[i],j]))
  }
}

# Remove from UniqueScannedTests any records that require no action
UniqueScannedTests = UniqueScannedTests[apply(X = UniqueScannedTests[,c("Analyze","Update","Monitor")], MARGIN = 1, FUN = any),]

# Update the Scanned Tests document with the modified ScannedTests 
gs_edit_cells(ss = ScannedTests.url, ws = 1, input = UniqueScannedTests, anchor = "A1") # Start at A1 b/c the header row is also added

# store the date and time of the current run
# gs_edit_cells(ss = ScannedTests.url, ws = 2, input = Sys.time(), anchor = "A2") 