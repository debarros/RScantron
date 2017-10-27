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
RecentEventFrame = FindRecentEvents(EventFrame = EventFrame, TAB = list(TAB.wb, TABpath), status = "Finished", updatePriorEvents = F)
# RecentEventFrame = FindRecentEvents(EventFrame = EventFrame, RecentDays = 5, status = "Finished", updatePriorEvents = F)

#Get a list of the recently scanned tests, and how many instances per test
RecentTestFrame = FindRecentTests(RecentEventFrame)

# Get the complete list of tests with their test ID's and containing folders
TestFolderFrame = FindFolders(ScantronHandle, "t", SkipTestFolder)
TestFrame = FindTests(TestFolderFrame)

# Check for tests not included in the tab
missingTests = RecentTestFrame$Published.Test[!(RecentTestFrame$Published.Test %in% read.xlsx(TAB.wb)$TestName)]
print(missingTests)

# If there are any missing tests, add them to the TAB and reload it
UpdateTab(missingTests, TestFrame, TAB.wb, TABpath)
TAB.wb = loadWorkbook(xlsxFile = TABpath)

# Download the item response files and save them
GetAndStoreItemResponses(RecentTestFrame, TestFrame, TAB.wb, ScantronHandle)
# GetAndStoreItemResponses_SingleTest(testname = "Bio (2017-10-12) Pop Eco Human Impact Unit", TAB.wb)

# Log out of scantron
LogoutPage = logout(ScantronHandle)

# Generate the reports

for(i in 1:nrow(RecentTestFrame)){
  print(i)
  DataLocation = read.xslx(TAB.wb)$Local.folder[read.xslx(TAB.wb)$TestName == RecentTestFrame$Published.Test[i]]
  generateReport(DataLocation = DataLocation, TMS = "ScantronAS")
}

# The following lines can be used to generate the report for one test, given the test name
# DataLocation = TAB$Local.folder[TAB$TestName == "Ge (2017-10-06) Pts Lines Segments Angles +"]
# generateReport(DataLocation = DataLocation, TMS = "ScantronAS")


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
NewScannedTests = data.frame(
  Test = RecentTestFrame$Published.Test, 
  Folder = read.xslx(TAB.wb)$Local.folder[match(RecentTestFrame$Published.Test,read.xslx(TAB.wb)$TestName)])
if(nrow(NewScannedTests) > 0){
  NewScannedTests$MakeReport = F
  NewScannedTests$SendReport = T
  NewScannedTests$Update = F
  NewScannedTests$Monitor = T
}


AllScannedTests = rbind(NewScannedTests, ScannedTests)
UniqueScannedTests = AllScannedTests[!duplicated(AllScannedTests$Test),]
for(i in 1:nrow(UniqueScannedTests)){
  for(j in c("SendReport","Update","Monitor")){
    UniqueScannedTests[i,j] = any(unlist(AllScannedTests[AllScannedTests$Test == UniqueScannedTests$Test[i],j]))
  }
}

# Remove from UniqueScannedTests any records that require no action
UniqueScannedTests = UniqueScannedTests[apply(X = UniqueScannedTests[,c("MakeReport","SendReport","Update","Monitor")], MARGIN = 1, FUN = any),]

# Update the Scanned Tests document with the modified ScannedTests 
gs_edit_cells(ss = ScannedTests.url, ws = 1, input = UniqueScannedTests, anchor = "A1") # Start at A1 b/c the header row is also added

# store the date and time of the current run
gs_edit_cells(ss = ScannedTests.url, ws = 2, input = Sys.time(), anchor = "A2") 

# update prior events
RecentEventFrame = FindRecentEvents(EventFrame = EventFrame, TAB = list(TAB.wb, TABpath), status = "Finished", updatePriorEvents = T)