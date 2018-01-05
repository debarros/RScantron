


# These functions already existed
# Do we want to run this stuff outside and pass in DraftFrame as an argument?
DraftFolderFrame = FindFolders(ScantronHandle, "d", SkipDraftFolder)  # Get all draft folders, including the entire text of the page
DraftFrame = FindDrafts(DraftFolderFrame)                             # Find all draft tests mentioned on those folders
DraftFrame$Path = paste0(DraftFrame$folder, "/", DraftFrame$TestName) # Construct the full path to the draft


# This function takes a curl handle and a published test id and returns the name of the draft on which the published test is based
GetDraftName = function(ScantronHandle, tid, messageLevel = 0){
  urlstub = "https://admin.achievementseries.com/published-test/info.ssp?id=" # url stub for getting published test pages
  agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  url = paste0(urlstub, tid)                                                  # build the url for the current test
  TestPage = httr::content(httr::GET(url = url,                               # fetch the page
                                     user_agent(agent)),
                           as = "text",
                           encoding = "UTF-8")
  DraftNameStart = gregexpr("Test Draft Name", TestPage)[[1]][1] + 82         # find where the draft name starts
  SpanLocations = c(gregexpr("</span>", TestPage)[[1]])                       # find all instances of <\span>
  DraftNameEnd = min(SpanLocations[SpanLocations > DraftNameStart]) - 1       # find where the draft name ends
  DraftName = substr(TestPage, DraftNameStart, DraftNameEnd)                  # get the draft name
  DraftName = FixHtmlChars(DraftName)                                         # clean the draft name
  return(DraftName)
}



TAB = read.xlsx(TAB.wb)                                       # Get the main sheet of the tab
TAB.limited = TAB[TAB$TestName %in% TestFrame$TestName,]      # limit it to just those tests that are in Scantron
TAB.limited = TAB.limited[!duplicated(TAB.limited$TestName),] # remove any duplicates
TAB.limited$DraftName = NA_character_                         # Add a variable to hold the draft name

# Go through the published tests that appear in both the TAB and in Scantron and get each Draft name
for(i in 1:nrow(TAB.limited)){
  TAB.limited$DraftName[i] = GetDraftName(ScantronHandle, TAB.limited$TestID[i])
}

# Use the DraftFrame to lookup the full path to the draft for each test in the TAB
TAB.limited$DraftPath = DraftFrame$Path[match(x = TAB.limited$DraftName, table = DraftFrame$TestName)]


# Next:
# For each row in TAB.limited, 
#     load the comparison and topic alignment file from the local folder into a workbook object
#     Write the draft path to the place where it goes in the workbook object
#     Write the workbook object back to the xlsx file 


