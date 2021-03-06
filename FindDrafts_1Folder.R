#FindDrafts_1Folder.R

#The first argument to this function is DraftFolderRow, a 1-row data frame with three columns: 
#   fname is the name of the current folder.  
#   fid is the folder id.
#   page is the actual page displaying the folder

#This function will return a data.frame listing the names, id's, and containing folder name of every test draft in this folder
#   (obviously, the containing folder will be the same for every test in one call of this function)

#' @title Find Drafts 1 Folder
#' @description Find all test drafts in one folder in Achievement Series
#' @param DraftFolderRow A data.frame consisting of one row from the output from
#'   the function FindFolders (with parameter \code{type} set to "d"),
#'   representing a single folder in Achievement Series.  It has 3 columns:
#'   \code{fname} is the name of the current folder.  \code{fid} is the folder
#'   id. \code{page} is the content of the actual page displaying the folder
#' @param messageLevel integer of length 1 indicating level of diagnostic
#'   messages to print.  Defaults to 0.
#' @return data.frame with 1 row for each test draft in the given folder and
#'   columns holding the test name, folder path, and test ID.
FindDrafts_1Folder = function (DraftFolderRow, messageLevel = 0){
  
  if(messageLevel > 0){
    print(paste0("Finding drafts in ", DraftFolderRow$fname))
  }
  
  # Initialize the TempTests data.frame.  This will hold the list of the names of the tests, along with their folders and id's.
  TempTests = data.frame(character(),character(),character(),stringsAsFactors = FALSE)  
  colnames(TempTests) = c("TestName","folder","tid")
  
  page = htmlParse(DraftFolderRow[1,3])       # this creates a neat looking html string
  links = xpathSApply(page, "//a/@href")      # this finds all of the links in the document
  
  # This next part creates a list of the test ID codes
  # Note that the string we actually want to find is "list.ssp?id="
  # The first problem is that "?" is a special character in regex, and so must be escaped using a "\"
  # The second problem is that "\" is an escape character in R, and so must be escaped using a "\"
  TestLinks = substr(links[grep("list.ssp\\?id=",links)],33,48)    
  
  if(length(TestLinks) == 0){    #if there are no tests in this foler, 
    return(TempTests)            # return the empty data.frame
  } else {  
    
    
    # The object "Location" holds the starting points of the test ID codes, which are all of the same length
    Location = data.frame(integer(0))                    # initialize the location data.frame
    for (i in 1:length(TestLinks)){                      # fill the Location data.frame with starting points of the test ID codes
      Location[i,1] = as.integer(gregexpr(pattern = TestLinks[i],DraftFolderRow[1,3])[[1]][1])
    } # /for
    
    if(nrow(Location)==0){  # If there are no tests here,
      return(TempTests)     # return the empty data.frame
    } else {                # If there are tests, continue to find them
      
      # Append to the Location data.frame the starting position of each test name
      Location = cbind(Location, Location + 79)
      colnames(Location) = c("TestIdStart","TestNameStart")
      
      # Next, find the ending position of every test name and a few other extraneous links
      ends = dcast(
        melt(
          str_locate_all(
            pattern = "</a></span></td>",  # this is the code that always follows test names
            DraftFolderRow[1,3])[[1]]), 
        formula = Var1 ~ Var2)[,2]-1       # the -1 moves from 1st char after the link to the last char of the link
      
      # Append a column to the Location data.frame to hold the end point of each test name
      Location = cbind(Location,TestNameEnd= data.frame(TestNameEnd= rep(as.integer(NA),times = dim(Location)[1])))
      
      # Now, match the starting position of each test name with the next ending position
      for (i in 1:dim(Location)[1]){
        Location[i,3] = min(ends[which(ends > Location[i,2])])
      } # /for
      
      for (i in 1:nrow(Location)){
        TempTests[i,1]=  as.character(substr(DraftFolderRow[1,3], Location[i,2]+1, Location[i,3]))
        TempTests[i,3]=  as.character(substr(DraftFolderRow[1,3], Location[i,1], Location[i,1]+15))
      } # /for
      TempTests[,2] = DraftFolderRow$fname[1]    
      
      TempTests$TestName = FixHtmlChars(TempTests$TestName) # fix weird html characters
      
      return(TempTests)
    } # /if=else
  } # /if-else
} # /function
