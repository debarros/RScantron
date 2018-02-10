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

# log in to Scantron website
login(loginurls, username, password, SiteCode, messageLevel = 1, agent = agent)


# Sign in to google
# this might launch a browser so you can sign into your account
SWSM(gs_auth())
ScannedTests.url = SWSM(gs_url(ScannedTests.url.text)) #enter the URL of the scanned test document here

#-----------------------------------------#
#### Determine current reporting needs ####
#-----------------------------------------#

StudentFrame = FindStudents(messageLevel = 1, agent = agent) # Get the complete list of students
EventFrame = FindEvents(StudentFrame, schoolYear(), messageLevel = 1, agent = agent) # Get the complete list of instances in which a student has taken a test

# Compare new event frame to old event frame and subset to the recent events
RecentEventFrame = FindRecentEvents(EventFrame = EventFrame, TAB = list(TAB.wb, TABpath), status = "Finished", updatePriorEvents = F, messageLevel = 1)
# RecentEventFrame = FindRecentEvents(EventFrame = EventFrame, RecentDays = 5, status = "Finished", updatePriorEvents = F)

#Get a list of the recently scanned tests, and how many instances per test
RecentTestFrame = FindRecentTests(RecentEventFrame, messageLevel = 1)

# Get the complete list of tests with their test ID's and containing folders
TestFolderFrame = FindFolders("t", SkipTestFolder, messageLevel = 1, agent = agent)
TestFrame = FindTests(TestFolderFrame, messageLevel = 1)

# Check for tests not included in the tab, or that have altered testID's
missingTests = FindMissingTests(RecentTestFrame, TAB.wb, TestFrame, messageLevel = 1)

# If there are any missing tests, add them to the TAB and reload it
UpdateTab(missingTests, TestFrame, TAB.wb, TABpath, messageLevel = 1)
TAB.wb = loadWorkbook(xlsxFile = TABpath)

# Download the item response files and save them
GetAndStoreItemResponses(RecentTestFrame, TestFrame, TAB.wb, messageLevel = 1, agent = agent)
# GetAndStoreItemResponses_SingleTest(testname = "Sp12 (2018-01-17) Midterm", TAB.wb, messageLevel = 2)

# Log out of scantron
LogoutPage = logout(messageLevel = 1, agent = agent)

# Generate the reports
testsToUse = as.character(RecentTestFrame$Published.Test)
for(i in 1:length(testsToUse)){
  print(paste0(i, " of ", length(testsToUse), " - ", testsToUse[i]))
  DataLocation = read.xlsx(TAB.wb)$Local.folder[read.xlsx(TAB.wb)$TestName == testsToUse[i]]
  generateReport(DataLocation = DataLocation, TMS = "ScantronAS")
}

# The following lines can be used to generate the report for one test, given the test name
# DataLocation = read.xlsx(TAB.wb)$Local.folder[read.xlsx(TAB.wb)$TestName == "Ge (2018-01-08) Midterm"]
# generateReport(DataLocation = DataLocation, TMS = "ScantronAS")


#--------------------------#
#### Monitoring section ####
#--------------------------#

# Update Score Monitoring
UpdateMonitoring(ScannedTests.url, RecentTestFrame, TAB.wb, messageLevel = 1)

# update prior events
RecentEventFrame = FindRecentEvents(EventFrame = EventFrame, TAB = list(TAB.wb, TABpath), status = "Finished", updatePriorEvents = T, messageLevel = 1)
