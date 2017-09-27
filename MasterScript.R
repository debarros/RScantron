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
ScannedTests.url = gs_url("https://docs.google.com/spreadsheets/d/1js6XcxzF4y3uFtc_Uxr8e3UfvvcKjUrQzL8lKV2st1I/edit") #enter the URL of the scanned test document here


#-----------------------------------------#
#### Determine current reporting needs ####
#-----------------------------------------#

# Get the complete list of students
StudentFrame = FindStudents(ScantronHandle)

# Get the complete list of instances in which a student has taken a test
EventFrame = FindEvents(StudentFrame, ScantronHandle)

# Compare new event frame to old event frame and subset to the recent events
#This should be encaspulated as a function with a parameter about whether to include score changes
PriorEventFrame = gs_read(ss = ScannedTests.url, ws = "Events") #read in existing events
gs_edit_cells(ss = ScannedTests.url, ws = "Events", input = EventFrame, anchor = "A1") #store the complete events
IDcolumns = colnames(EventFrame)
IDcolumns = IDcolumns[!(IDcolumns %in% c("Score"))]
PriorEventFrame$identifier = apply(PriorEventFrame[IDcolumns], MARGIN = 1, FUN = paste0, collapse = "-")
EventFrame$identifier = apply(EventFrame[IDcolumns], MARGIN = 1, FUN = paste0, collapse = "-")
RecentEventFrame = EventFrame[!(EventFrame$identifier %in% PriorEventFrame$identifier),1:(ncol(EventFrame)-1)]
RecentEventFrame = RecentEventFrame[RecentEventFrame$Status == "Finished", ]

#Get a list of the recently scanned tests, and how many instances per test
RecentTestFrame = FindRecentTests(RecentEventFrame)

# Read in the TAB (Test Address Book)
TAB = read.xlsx(xlsxFile = "J:/tests/2017-2018/TAB.xlsx")

# Check for tests not included in the tab
missingTests = RecentTestFrame$TestName[!(RecentTestFrame$TestName %in% TAB$TestName)]
print(missingTests)
if(length(missingTests) > 0){ # If there are any missing tests, add them to the TAB
  write.csv(x = RecentTestFrame[!(RecentTestFrame$TestName %in% TAB$TestName), c("TestID", "TestName")], file = "addtoTAB.csv")
}
if(length(missingTests) > 0){ # After adding them to the tab, reload it
  TAB = read.xlsx(xlsxFile = "J:/tests/2017-2018/TAB.xlsx")
}

# Download the item response files and save them
for(i in 1:nrow(RecentTestFrame)){
  # determine the courses associated with this test
  # determine the sections of this course
  for(i in sections){
    # get the ClassID for the section
    # download the item response file
    # store it in the exports folder
  }
}

# Log out of the scantron ####
LogoutPage = logout(ScantronHandle)

# Generate the reports
for(i in 1:nrow(RecentTestFrame)){
  generateReport(DataLocation = TAB$Folder.location[TAB$TestName == RecentTestFrame$TestName[i]])
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
NewScannedTests = data.frame(Test = RecentTestFrame$TestName, Folder = TAB$Folder.location[match(RecentTestFrame$TestName,TAB$TestName)], Analyze = T, Update = F, Monitor = T)
AllScannedTests = rbind(ScannedTests, NewScannedTests)
UniqueScannedTests = AllScannedTests[!duplicated(AllScannedTests$Test),]
for(i in 1:nrow(UniqueScannedTests)){
  for(j in c("Analyze","Update","Monitor")){
    UniqueScannedTests[1,j] = any(unlist(AllScannedTests[AllScannedTests$Test == UniqueScannedTests$Test[i],j]))
  }
}

# Remove from UniqueScannedTests any records that require no action
UniqueScannedTests = UniqueScannedTests[apply(X = UniqueScannedTests[,c("Analyze","Update","Monitor")], MARGIN = 1, FUN = any),]

# Update the Scanned Tests document with the modified ScannedTests 
gs_edit_cells(ss = ScannedTests.url, ws = 1, input = UniqueScannedTests, anchor = "A1") # Start at A1 b/c the header row is also added

# store the date and time of the current run
gs_edit_cells(ss = ScannedTests.url, ws = 2, input = Sys.time(), anchor = "A2") 