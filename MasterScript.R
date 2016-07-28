# R/Scantron Interface
# by Paul de Barros (pj.deBarros@gmail.com)
# Repository located at https://github.com/debarros/RScantron

# Initialize ####
gc() #garbage collection clears data no longer being used from memory
source("functions.R") #load the functions

# You can enter the info here, but be careful.
# You should not save this file with your info entered,
# especially if you plan to contribute to a 
# public repository.
username = "sampleUser"
password = "password1"
SiteCode = "12-3456-7890"
caLocation = character()
SkipTestFolder = "skipYests"
SkipDraftFolder = "skipDrafts"
SkipSessionFolder = "skipSessions"

# To avoid committing and sharing credentials, 
# create a file called credentials.R and 
# store the info there.  Git will ignore it,
# so you won't accidentally share it.
source("credentials.R") 

# Log in to the site ####
# Use the first line if you have set the parameters, 
# and the second to enter them at a prompt
ScantronHandle = login(username, password, SiteCode, caLocation)
#ScantronHandle = login()


# Determine current reporting needs ####
# Get the complete list of tests with their test ID's and containing folders
TestFolderFrame = FindFolders(ScantronHandle, "t", SkipTestFolder)
TestFrame = FindTests(TestFolderFrame)
OldTestFrame = readWorkbook(xlsxFile = "C:/Users/pauldeba/Documents/Everything/data drive/weekly tests/2015-2016/export link creator.xlsx", sheet = "tests")
NewTestFrame = rbind.data.frame(OldTestFrame, TestFrame[,c(1,3,2)])
NewTestFrame = NewTestFrame[!duplicated(NewTestFrame$TestName),]
NewTestFrame = NewTestFrame[order(NewTestFrame$TestName),]
write.csv(NewTestFrame, file = "TestFrame.csv", row.names = FALSE)

# Get the complete list of students
StudentFrame = FindStudents(ScantronHandle)

# Get the complete list of instances in which a student has taken a test
EventFrame = FindEvents(StudentFrame, ScantronHandle)

# Get a list of the recently scanned instances
RecentDays = 1
#enter the date modified of recentScores.R.  If more bubble sheets could have been scanned that day, enter the day before.
LastTime = as.Date("2016-06-04")  
RecentDays = as.integer(Sys.Date() - LastTime)
RecentEventFrame = FindRecentEvents(EventFrame, RecentDays)
#View(RecentEventFrame)

#Get a list of the recently scanned tests, and how many instances per test
RecentTestFrame = FindRecentTests(RecentEventFrame)
View(RecentTestFrame)

#Create output of the scores that need quick updates or reports
cap = 10 #only tests with fewer than cap new scores will be included in the quick updates
ScoreUpdates(RecentEventFrame, RecentTestFrame, cap)

# Catalog Draft Tests ####
# Get the complete list of folders with their folder ID's
DraftFolderFrame = FindFolders(ScantronHandle, "d", SkipDraftFolder)
SessionFolderFrame = FindFolders(ScantronHandle, "s", SkipSessionFolder)


# Get the complete list of test drafts with their test ID's and containing folders
DraftFrame = FindDrafts(DraftFolderFrame)


# Catalog class sections ####
#Get the complete list of class sections with their class ID's
ClassFrame = FindClasses(ScantronHandle)
write.csv(ClassFrame,file = "ClassSections.csv", row.names = FALSE)

# Catalog all test scores ###
#Compile all scores ever
testIDs = TestFrame$tid
AllResults = FindResults(testIDs, ScantronHandle)


# Log out of the system ####
LogoutPage = logout(ScantronHandle)

