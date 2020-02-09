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
# If you get "Error in curl::curl_fetch_memory(url, handle = handle) : Maximum (10) redirects followed", run the next two lines, then the previous one
# detach("package:httr", unload = TRUE)
# library(httr)

# Sign in to google
# this might launch a browser so you can sign into your account
SWSM(gs_auth())
ScannedTests.url = SWSM(gs_url(ScannedTests.url.text)) #enter the URL of the scanned test document here

#-----------------------------------------#
#### Determine current reporting needs ####
#-----------------------------------------#

# Get the complete list of students and the complete list of instances in which a student has taken a test
StudentFrame = FindStudents(messageLevel = 1, agent = agent) 
EventFrame = FindEvents(StudentFrame, schoolYear(), messageLevel = 2, agent = agent) 

# Compare new event frame to old event frame and subset to the recent events
RecentEventFrame = FindRecentEvents(
  EventFrame = EventFrame, TAB = list(TAB.wb, TABpath), status = "Finished", updatePriorEvents = F, messageLevel = 1)

# This next section is for when stuff gets messed up
# RecentEventFrame = FindRecentEvents(EventFrame = EventFrame, RecentDays = 5, status = "Finished", updatePriorEvents = F)
# RecentEventFrame = RecentEventFrame[RecentEventFrame$Date > as.Date("2019-04-23"),]
# EventFrame2 = EventFrame[EventFrame$Date < as.Date("2019-04-23"),]
# RecentEventFrame = FindRecentEvents(EventFrame = EventFrame2, TAB = list(TAB.wb, TABpath), status = "Finished", updatePriorEvents = T, messageLevel = 1)


#Get a list of the recently scanned tests, and how many instances per test
RecentTestFrame = FindRecentTests(RecentEventFrame, messageLevel = 1)

# Get the complete list of tests with their test ID's and containing folders
TestFolderFrame = FindFolders("t", SkipTestFolder, messageLevel = 1, agent = agent)
TestFrame = FindTests(TestFolderFrame, messageLevel = 1)

# Check for tests not included in the tab, or that have altered testID's
missingTests = FindMissingTests(RecentTestFrame, TAB.wb, TestFrame, messageLevel = 1)

# If there are any missing tests, add them to the TAB and reload it
UpdateTab(missingTests, TestFrame, TAB.wb, TABpath, messageLevel = 1)
# Before you reload the tab, add in the local folder paths
TAB.wb = loadWorkbook(xlsxFile = TABpath)


# Before this next part is run, the scanned tests document should be retrieved.  that way, anything that is marked as needing a new report can have new item response files pulled.
# Download the item response files and save them
GetAndStoreItemResponses(RecentTestFrame, TestFrame, TAB.wb, messageLevel = 2, agent = agent)
# startRow = 4
# GetAndStoreItemResponses(RecentTestFrame, TestFrame, TAB.wb, startRow = startRow, messageLevel = 4, agent = agent)
# GetAndStoreItemResponses_SingleTest(testname = "L1 (2019-11-21) TKAM Part 1", TAB.wb, messageLevel = 4)
# RecentTestFrame[startRow,]


# testnames = c("US (2019-10-28) 2A American Revolution >", "USRR (2019-10-04) U1 Colonial Foundations")
# GetAndStoreItemResponses_SeveralTests(testnames = testnames, TAB.wb, messageLevel = 4, agent = agent)

# Get a vector of the tests that need reports
testsToUse = as.character(RecentTestFrame$Published.Test)
# testsToUse = testnames

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
  generateReport(DataLocation = DataLocation, TMS = "ScantronAS", HaltOnMultiResponse = T, messageLevel = 1)
  i = i + 1
}

# If the while loop throws an error and a row has to be deleted from a csv export, 
# paste the student numbers in the next line and run it and the ones after:
# idsToSpoil = c("171810594")
# SpoilFrame = RecentEventFrame[RecentEventFrame$Published.Test == testsToUse[i] & RecentEventFrame$StNumberRep %in% idsToSpoil,]
# Spoil(SpoilFrame = SpoilFrame, messageLevel = 4)

# This is to spoil all recent scores for a particular test:
# SpoilFrame = RecentEventFrame[RecentEventFrame$Published.Test == testsToUse[i],]

# The following lines can be used to generate the report for one test, given the test name:
# DataLocation = read.xlsx(TAB.wb)$Local.folder[read.xlsx(TAB.wb)$TestName == "Sp1 (2019-01-31) Gustar and Infinitives"]
# generateReport(DataLocation = DataLocation, TMS = "ScantronAS")

# Log out of scantron
LogoutPage = logout(messageLevel = 1, agent = agent)


#--------------------------#
#### Monitoring section ####
#--------------------------#

# The following lines can be used to remove tests from tracking (e.g. if reports couldn't be made)
# droptestnumber = i-1                        # Set the test number
# droptests = c(testsToUse[droptestnumber])  # Grab the test name
# RecentTestFrame = RecentTestFrame[!(RecentTestFrame$Published.Test %in% droptests),] # Remove the test from the recent test frame
# EventFrame = EventFrame[!(EventFrame$Published.Test %in% droptests),]                # Remove the test from the overall test frame
# testsToUse = testsToUse[-droptestnumber]                                             # Remove the test from the testsToUse list

# Update Score Monitoring
UpdateMonitoring(ScannedTests.url, RecentTestFrame, TAB.wb, MakeReportDone = T, sortDown = F, messageLevel = 3)

# update prior events
RecentEventFrame = FindRecentEvents(
  EventFrame = EventFrame, 
  TAB = list(TAB.wb, TABpath), 
  status = "Finished", 
  updatePriorEvents = T, 
  messageLevel = 3)


# Are you sure you ran UpdateMonitoring?  Check the history.

