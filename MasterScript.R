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
login(loginurls, username, password, SiteCode, messageLevel = 3, agent = agent)


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
GetAndStoreItemResponses(RecentTestFrame, TestFrame, TAB.wb, messageLevel = 2, agent = agent)
# GetAndStoreItemResponses_SingleTest(testname = "WHS (2018-03-09) Roots of Democracy", TAB.wb, messageLevel = 2)


# Get a vector of the tests that need reports
testsToUse = as.character(RecentTestFrame$Published.Test)

# Pull the scanned tests document to look for other tests that need to have reports generated
ScannedTests = SWSM(gs_read(ss = ScannedTests.url, ws = 1, verbose = F))
ScannedTests = ScannedTests[ScannedTests$MakeReport,]
if(nrow(ScannedTests) > 0){
  testsToUse = unique(c(testsToUse, ScannedTests$Test))
}

# Generate the reports
i = 1
sort(testsToUse)
print(i)
while(i <= length(testsToUse)){
  print(paste0(i, " of ", length(testsToUse), " - ", testsToUse[i]))
  DataLocation = read.xlsx(TAB.wb)$Local.folder[read.xlsx(TAB.wb)$TestName == testsToUse[i]]
  generateReport(DataLocation = DataLocation, TMS = "ScantronAS")
  i = i + 1
}

# If the while loop throws an error and a row has to be deleted from a csv export, 
# paste the student number in the next line and run it and the one after
# SpoilFrame = RecentEventFrame[RecentEventFrame$Published.Test == testsToUse[i] & RecentEventFrame$StNumberRep == "151610368",]
# Spoil(SpoilFrame,3)


# The following lines can be used to generate the report for one test, given the test name
# DataLocation = read.xlsx(TAB.wb)$Local.folder[read.xlsx(TAB.wb)$TestName == "Ge (2018-02-02) U3 Rigid Motion and Congruence +"]
# generateReport(DataLocation = DataLocation, TMS = "ScantronAS")

# Log out of scantron
LogoutPage = logout(messageLevel = 1, agent = agent)


#--------------------------#
#### Monitoring section ####
#--------------------------#

# The following lines can be used to remove tests from tracking (e.g. if reports couldn't be made)
# droptests = c("Nu (2018-05-04) Life Skills 1", "ALn (2018-04-18) Practice Exam 2 +>")
# RecentTestFrame = RecentTestFrame[!(RecentTestFrame$Published.Test %in% droptests),]
# EventFrame = EventFrame[!(EventFrame$Published.Test %in% droptests),]


# Update Score Monitoring
UpdateMonitoring(ScannedTests.url, RecentTestFrame, TAB.wb, MakeReportDone = T, messageLevel = 1)

# update prior events
RecentEventFrame = FindRecentEvents(EventFrame = EventFrame, TAB = list(TAB.wb, TABpath), status = "Finished", updatePriorEvents = T, messageLevel = 1)


# Are you sure you ran UpdateMonitoring?  Check the history.
