#Cataloging.R

# Catalog Draft Tests ####
# Get the complete list of folders with their folder ID's
DraftFolderFrame = FindFolders(type = "d", SkipFolder = SkipDraftFolder, agent = agent)
SessionFolderFrame = FindFolders(type = "s", SkipFolder = SkipSessionFolder, agent = agent)


# Get the complete list of test drafts with their test ID's and containing folders
DraftFrame = FindDrafts(DraftFolderFrame)

# Get the page showing the content of each draft
# If you have a lot of drafts, include the parameter MaxDrafts = n (where n is some small integer)
DraftFrame = StoreDrafts(DraftFrame)

# Catalog class sections ####
# Get the complete list of class sections with their class ID's
ClassFrame = FindClasses(TeacherLookup = TeacherLookup, CourseLookup = CourseLookup)
ClassFrame = ClassFrame[order(ClassFrame$Dept, ClassFrame$ShortName, ClassFrame$Level, ClassFrame$Primary.Staff, ClassFrame$Period),]
write.csv(ClassFrame,file = "ClassSections.csv", row.names = FALSE)

# Catalog all test scores ###
# Compile all scores ever
testIDs = TestFrame$tid
AllResults = FindResults(testIDs, agent = agent)

