


# These functions already existed
# Do we want to run this stuff outside and pass in DraftFrame as an argument?
DraftFolderFrame = FindFolders("d", SkipDraftFolder)                  # Get all draft folders, including the entire text of the page
DraftFrame = FindDrafts(DraftFolderFrame)                             # Find all draft tests mentioned on those folders
DraftFrame$Path = paste0(DraftFrame$folder, "/", DraftFrame$TestName) # Construct the full path to the draft






TAB = read.xlsx(TAB.wb)                                       # Get the main sheet of the tab
TAB.limited = TAB[TAB$TestName %in% TestFrame$TestName,]      # limit it to just those tests that are in Scantron
TAB.limited = TAB.limited[!duplicated(TAB.limited$TestName),] # remove any duplicates
TAB.limited$DraftName = NA_character_                         # Add a variable to hold the draft name

# Go through the published tests that appear in both the TAB and in Scantron and get each Draft name
for(i in 1:nrow(TAB.limited)){
  TAB.limited$DraftName[i] = GetDraftName(tid = TAB.limited$TestID[i], agent = agent, messageLevel = messageLevel - 1)
}

# Use the DraftFrame to lookup the full path to the draft for each test in the TAB
TAB.limited$DraftPath = DraftFrame$Path[match(x = TAB.limited$DraftName, table = DraftFrame$TestName)]

# Next:
# For each row in TAB.limited, 
#     load the comparison and topic alignment file from the local folder into a workbook object
#     Write the draft path to the place where it goes in the workbook object
#     Write the workbook object back to the xlsx file 


for(thisRow in 1:nrow(TAB.limited)){
  curTestFolder = TAB.limited$Local.folder[thisRow]
  setupfilepath = paste0(curTestFolder, "/comparison and topic alignment.xlsx")
  print(setupfilepath)
  if(file.exists(setupfilepath)){
    wb = loadWorkbook(setupfilepath)
    # modify it
    # save it
  } else {
    warning(paste0("No test setup file found in ", curTestFolder))  
  }
}




