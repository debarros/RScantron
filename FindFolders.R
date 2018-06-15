# FindFolder.R

# This function is used to create a list of all of the Folders (Draft, Published, or Scheduled)

# This function takes the following arguments: 
#   type = t for published tests, d for drafts, and s for scheduled sessions
#   SkipFolder = the name of folders that are not to be included (nor are their subfolders)
#   x = the current page, or empty on first call
#   parent = name of the folder we are in right now, or 'Root' if it is the top level folder
#   ThisFolderID = fid of the folder we are in right now, or 'Root' if it is the top level folder
#   messageLevel = the level of messages to be printed
#   agent = the user agent string for browser spoofing
#  
# This function returns a data.frame called TempFolders.
# Each row in TempFolders holds information about one folder
# The 3 columns are:
#   fname = the name of the folder
#   fid = the folder id of the folder
#   page = the full html page of the folder

#' @title Find Folders
#' @description Find folders in Achievement Series
#' @param type one character.  Acceptable values are t (for published tests), d
#'   (for drafts), and s (for scheduled sessions).
#' @param SkipFolder the name of folder that is not to be included (nor are its
#'   subfolders).
#' @param x the content of the current page (or empty character string during
#'   the first call).
#' @param parent name of the current folder (or 'Root' if it is the top level
#'   folder).
#' @param ThisFolderID fid of the current folder (or 'Root' if it is the top
#'   level folder).
#' @param messageLevel integer of length 1 indicating level of diagnostic
#'   messages to print.  Defaults to 0.
#' @param agent the browser user agent.  Defaults to NULL, in which case a
#'   specific one is used.
#' @return data.frame with 1 row for each folder and three columns holding fname
#'   (name of the folder), fid (folder id of the folder), and page (the content
#'   of the webpage for the folder).
FindFolders = function(type = "t",
                       SkipFolder = as.character('NA'),
                       x = character(), 
                       parent = as.character(''), 
                       ThisFolderID = as.character('Root'), 
                       messageLevel = 0,
                       agent = NULL){
  
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"  
  }
  
  ############ Section 1: Setup ##########
  
  z = c(31,46)  # these help find the fid in the links
  if(type == "t"){url = 'https://admin.achievementseries.com/published-tests/list.ssp'}
  if(type == "s"){url = 'https://admin.achievementseries.com/scheduled-tests/list.ssp'}  
  if(type == "d"){url = 'https://admin.achievementseries.com/test-drafts/list.ssp'
  z = z - 4 # the links in the test drafts folders are 4 characters shorter
  }
  
  # If this is the first call of the function, get the page for the top level folder
  if(length(x) == 0){ 
    if(messageLevel > 0){ print("Retrieving the page for the top level folder.")}
    x <- httr::content(
      httr::GET(url = url,user_agent(agent)),
      as = "text",
      encoding = "UTF-8")
    
    # Check to make sure it worked
    if(BadReturnCheck(x, messageLevel - 1)){
      stop("Error!  You are no longer logged in to Scantron.  Log in and then run this command again.")
    }
    
    # Check to make sure it's the top level folder
    # This section is necessary b/c the site will typically just return the page for the last folder viewed
    # This code has not been tested for draft or scheduled session folders
    if(type == "t"){
      optionmatchlist = gregexpr(pattern = "<option.{100}", text = x)      # Find the locations of option tags
      optionstring = regmatches(x = x, m = optionmatchlist)[[1]][3]        # Find the text of option tags and grab the 3rd one
      idmatchlist = regexec(pattern = "fid=(.{16})", text = optionstring)  # Find the location of the id
      idmatches = regmatches(x = optionstring, m = idmatchlist)[[1]]       # Get the id and other crap
      idstring = idmatches[length(idmatches)]                              # Get just the id
      address = paste0(url, '?fid=', idstring, '&ft=O&et=P&_p=1')          # Make the address for the top level folder
      
      x <- httr::content(
        httr::GET(url = address,user_agent(agent)),
        as = "text",
        encoding = "UTF-8")
    } # /if type == "t"
  } # /if top level
  
  # this will hold the folder names and id's
  TempFolders = data.frame(parent, ThisFolderID, x, stringsAsFactors = FALSE)  
  colnames(TempFolders) = c("fname", "fid","page")
  
  
  
  ############ Section 2: Find all the subfolders ##########
  
  page = htmlParse(x)                                       # this creates a neat looking html string
  links = xpathSApply(page, "//a/@href")                    # this finds all of the links in the document
  folderlinks = substr(links[grep("fid", links)],z[1],z[2]) # this creates a list of the folder ID codes
  MinPosition = gregexpr(pattern = "selected", x)[[1]][1]   # the folder links before the word "selected" are irrelevant
  
  # The object "location" holds the starting points of the folder ID codes, which are all of the same length
  location = data.frame(integer(0))                        # initialize the location data.frame
  for (i in 1:length(folderlinks)){                        # fill up the location data.frame with starting points of the folder ID codes
    location[i,1] = as.integer(gregexpr(pattern = folderlinks[i],x)[[1]][1])
  }
  drop = which(location[,1] < MinPosition)                 # find the indices of the locations of fid's that occur before the start of the folder list
  
  if (length(drop) > 0){                                   # if there are any extraneous fid's,
    location = data.frame(location[-drop,])                # get rid of them
  }
  
  if(nrow(location) == 0){ return(TempFolders) }           # If there are no folders here, return the empty data.frame
  
  # The object "bounds" will hold the starting and ending position of the name of each subfolder
  # First, find the starting position of every folder name, using the link text
  bounds = dcast(
    melt(
      str_locate_all(
        pattern = "_p=1",  # this is the character sequence that comes shortly before every folder name
        x)[[1]]), 
    formula = Var1 ~ Var2)[,c(3,1)]+3 #the folder name is 3 chars after the pattern above
  
  # Next, find the ending position of every folder name and a few other extraneous links
  ends = dcast(
    melt(
      str_locate_all(
        pattern = "</a></span>",  # this is the character sequence that always follows folder links
        x)[[1]]), 
    formula = Var1 ~ Var2)[,2]-1  # the -1 moves from 1st char after the link to the last char of the link
  
  
  # Now, match each folder name starting position with the next ending position
  for (i in 1:dim(bounds)[1]){
    bounds[i,2] = min(ends[which(ends > bounds[i,1])])
  }
  
  # Append a column to the bounds data.frame to hold the beginning point of folder ID
  bounds = cbind(bounds,LocationID= data.frame(LocationID= rep(as.integer(NA),times = dim(bounds)[1])))
  
  # For each row in the bounds data.frame, attempt to find the corresponding fid
  for (i in 1:nrow(bounds)){
    if (length(location[which(location[,1] < bounds[i,1]),]) > 0){      # if there are any fid locations before the current fname start point
      bounds[i,3] = max(location[which(location[,1] < bounds[i,1]),]) # use the fid that is closest
    } # /if
  } # /for
  
  # If there are any rows that did not get an fid matched to them, they are not really folders, so remove them
  # Note that, since there was already a check to see if there were no fid's, this will not make bounds empty
  if(length(which(is.na(bounds[,3]))) > 0){
    bounds = bounds[-(which(is.na(bounds[,3]))),]
    rownames(bounds) <- seq(length=nrow(bounds))  # renumber the rows
  }
  
  # Add columns to the bounds data.frame to hold the actual fname and fid for each row
  # Note that we have to use substring instead of substr because the start and endpoints are vectors
  fid = substring(x, bounds[,3], bounds[,3]+15)
  fname = substring(x, bounds[,1], bounds[,2])
  bounds = cbind(bounds, fid, fname)
  colnames(bounds) = c("fnameStart","fnameEnd","fidStart","fid","fname")
  
  # If there are any rows that represent SkipFolder, remove them
  if(length(which(bounds$fname %in% SkipFolder)) > 0){
    bounds = bounds[-(which(bounds$fname %in% SkipFolder)),]
    rownames(bounds) <- seq(length=nrow(bounds))
  }
  
  # If the only subfolders in the current folder are in SkipFolder, bounds will be empty now
  if(nrow(bounds) == 0){ return(TempFolders) }  # If there are no folders remaining in the list, return the empty data.frame
  
  
  ############ Section 3: Recursive Call ##########
  
  # Below is the recursive function call.  
  # For each folder found here, this finds all the subfolders there
  
  # FolderGrab holds the results of one function call, from one iteration of the loop.  This could be many folders.
  # Subfolders collects the results across all iterations of the loop.
  FolderGrab = data.frame(fname=character(),fid=character(),stringsAsFactors = FALSE ) 
  SubFolders = data.frame(fname=character(),fid=character(),stringsAsFactors = FALSE )
  
  for (i in 1:nrow(bounds)){                      # for each folder found here
    NextFolderID = bounds$fid[i]                  # pick one at a time
    address = paste0(url,'?fid=',NextFolderID,
                     '&ft=O&et=P&_p=1')
    if(messageLevel > 0){ 
      print("Retrieving the page for the next folder.")
    } # /if    
    x <- httr::content(
      httr::GET(url = address, user_agent(agent)),
      as = "text",
      encoding = "UTF-8")
    # x <- httr::content(x, as = "text")  # get the folder page
    
    # Check to make sure it worked
    if(BadReturnCheck(x, messageLevel - 1)){
      stop("Error!  You are no longer logged in to Scantron.  Log in and then run this command again.")
    }
    
    TempParent = bounds$fname[i]                           # get the name of the folder page
    FolderGrab = FindFolders(type = type,                  # recursive call
                             SkipFolder = SkipFolder,
                             x = x, 
                             parent = TempParent, 
                             ThisFolderID = NextFolderID,
                             messageLevel = messageLevel - 1,
                             agent = agent)
    
    if(nrow(FolderGrab)>0){                       # if any subfolders were returned from the folder just examined,
      SubFolders = rbind(SubFolders, FolderGrab)  # append them to the subfolders data.frame
    } # /if
    
  } # /for each subfolder
  
  
  ############ Section 4: Wrap Up ##########
  
  # If any subfolders were collected in the loop, append them to TempFolders
  if(nrow(SubFolders) > 0){
    SubFolders$fname = paste0(parent, "/", SubFolders$fname)  # make the folder name have its parent category
    TempFolders = rbind(TempFolders, SubFolders)
  } 
  
  return(TempFolders)                                       # return the list of folders to the calling function
} # /function
