
# These functions already existed
DraftFolderFrame = FindFolders(ScantronHandle, "d", SkipDraftFolder) # Get all draft folders, including the entire text of the page
DraftFrame = FindDrafts(DraftFolderFrame) # Find all draft tests mentioned on those folders
DraftFrame$Path = paste0(DraftFrame$folder, "/", DraftFrame$TestName) # Construct the full path to the draft


# This function takes a curl handle and a published test id and returns the name of the draft on which the published test is based
GetDraftName = function(ScantronHandle, tid){
  url = paste0("https://admin.achievementseries.com/published-test/info.ssp?id=", tid)
  TestPage = getURI(url = url, curl=ScantronHandle)
  DraftNameStart = gregexpr(pattern = "Test Draft Name", text = TestPage)[[1]][1] + 82
  SpanLocations = c(gregexpr(pattern = "</span>", text = TestPage)[[1]])
  DraftNameEnd = min(SpanLocations[SpanLocations > DraftNameStart]) - 1
  DraftName = substr(x = TestPage, start = DraftNameStart, stop = DraftNameEnd)
  DraftName = gsub("&gt;", ">", DraftName)
  DraftName = gsub("&amp;", "&", DraftName) #switch from html code for & to just the symbol
  return(DraftName)
}



TAB = read.xlsx(TAB.wb) # Get the main sheet of the tab
TAB.limited = TAB[TAB$TestName %in% TestFrame$TestName,] # limit it to just those tests that are in Scantron
TAB.limited = TAB.limited[!duplicated(TAB.limited$TestName),] # remove any duplicates

TAB.limited$DraftName = NA_character_ # Add a variable to hold the draft name

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


