#This function is used to create a list of all of the Folders (Draft, Published, or Scheduled)

FindFolders = function(ScantronHandle, type,
                       SkipFolder = as.character('NA'),
                       x = character(), 
                       parent = as.character(''), 
                       ThisFolderID = as.character('Root'), 
                       messageLevel = 0){
  
  # This function takes 6 arguments: 
  #   ScantronHandle = the info for using cURL to access Scantron
  #   type = t for published tests, d for drafts, and s for scheduled sessions
  #   SkipFolder = the name of folders that are not to be included (nor are their subfolders)
  #   x = the current page, or empty on first call
  #   parent = name of the folder we are in right now, or 'Root' if it is the top level folder
  #   ThisFolderID = fid of the folder we are in right now, or 'Root' if it is the top level folder
  
  
  #This function returns a data.frame called TempFolders.
  #Each row in TempFolders holds information about one folder
  #The 3 columns are:
  #   fname = the name of the folder
  #   fid = the folder id of the folder
  #   page = the full html page of the folder
  
  ############ Section 1: Setup ##########
  
  z = c(31,46)  #these help find the fid in the links
  
  if(type == "t"){url = 'https://admin.achievementseries.com/published-tests/list.ssp'}
  if(type == "s"){url = 'https://admin.achievementseries.com/scheduled-tests/list.ssp'}  
  if(type == "d"){url = 'https://admin.achievementseries.com/test-drafts/list.ssp'
  z = z - 4 #the links in the test drafts folders are 4 characters shorter
  }
  
  #If this is the first call of the function, get the page for the top level folder
  if(length(x) == 0){ 
    if(messageLevel > 0){ print("Retrieving the page for the top level folder.")}
    x = getURI(url, curl=ScantronHandle) 
  } #/if
  
  # Check to make sure it worked
  if(BadReturnCheck(x, messageLevel - 1)){
    stop("Error!  You are no longer logged in to Scantron.  Log in and then run this command again.")
  }
  
  # this will hold the folder names and id's
  TempFolders = data.frame(parent, ThisFolderID, x, stringsAsFactors = FALSE)  
  colnames(TempFolders) = c("fname", "fid","page")
  
  
  ############ Section 2: Find all the subfolders ##########
  
  page = htmlParse(x)                                      # this creates a neat looking html string
  links = xpathSApply(page, "//a/@href")                   # this finds all of the links in the document
  folderlinks = substr(links[grep("fid",links)],z[1],z[2]) # this creates a list of the folder ID codes
  MinPosition = gregexpr(pattern = "selected",x)[[1]][1]   # the folder links before the word "selected" are irrelevant
  
  # The object "location" holds the starting points of the folder ID codes, which are all of the same length
  location = data.frame(integer(0))                        # initialize the location data.frame
  for (i in 1:length(folderlinks)){                        # fill up the location data.frame with starting points of the folder ID codes
    location[i,1] = as.integer(gregexpr(pattern = folderlinks[i],x)[[1]][1])
  }
  drop = which(location[,1]<MinPosition)                   # find the indices of the locations of fid's that occur before the start of the folder list
  
  if (length(drop) > 0){                                   # if there are any extraneous fid's,
    location = data.frame(location[-drop,])                # get rid of them
  }
  
  if(nrow(location) == 0){ return(TempFolders) }             # If there are no folders here, return the empty data.frame
  
  # The object "bounds" will hold the starting and ending position of the name of each subfolder
  # First, find the starting position of every folder name, using the link text
  bounds = dcast(
    melt(
      str_locate_all(
        pattern = "_p=1",  #this is the character sequence that comes shortly before every folder name
        x)[[1]]), 
    formula = Var1 ~ Var2)[,c(3,1)]+3 #the folder name is 3 chars after the pattern above
  
  # Next, find the ending position of every folder name and a few other extraneous links
  ends = dcast(
    melt(
      str_locate_all(
        pattern = "</a></span>",  #this is the character sequence that always follows folder links
        x)[[1]]), 
    formula = Var1 ~ Var2)[,2]-1  #the -1 moves from 1st char after the link to the last char of the link
  
  
  # Now, match each folder name starting position with the next ending position
  for (i in 1:dim(bounds)[1]){ bounds[i,2] = min(ends[which(ends > bounds[i,1])]) }
  
  # Append a column to the bounds data.frame to hold the beginning point of folder ID
  bounds = cbind(bounds,LocationID= data.frame(LocationID= rep(as.integer(NA),times = dim(bounds)[1])))
  
  # For each row in the bounds data.frame, attempt to find the corresponding fid
  for (i in 1:nrow(bounds)){
    if (length(location[which(location[,1] < bounds[i,1]),])>0){      # if there are any fid locations before the current fname start point
      bounds[i,3] = max(location[which(location[,1] < bounds[i,1]),]) # use the fid that is closest
    } # /if
  } # /for
  
  #If there are any rows that did not get an fid matched to them, they are not really folders, so remove them
  # Note that, since there was already a check to see if there were no fid's, this will not make bounds empty
  if(length(which(is.na(bounds[,3])))>0){
    bounds = bounds[-(which(is.na(bounds[,3]))),]
    rownames(bounds) <- seq(length=nrow(bounds))  # renumber the rows
  }
  
  # Add columns to the bounds data.frame to hold the actual fname and fid for each row
  # Note that we have to use substring instead of substr because the start and endpoints are vectors
  fid = substring(x, bounds[,3], bounds[,3]+15)
  fname = substring(x, bounds[,1], bounds[,2])
  bounds = cbind(bounds, fid, fname)
  colnames(bounds) = c("fnameStart","fnameEnd","fidStart","fid","fname")
  
  #If there are any rows that represent SkipFolder, remove them
  if(length(which(bounds$fname %in% SkipFolder))>0){
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
    x = getURI(address, curl=ScantronHandle)      # get the folder page
    
    # Check to make sure it worked
    if(BadReturnCheck(x, messageLevel - 1)){
      stop("Error!  You are no longer logged in to Scantron.  Log in and then run this command again.")
    }
    
    TempParent = bounds$fname[i]                  # get the name of the folder page
    FolderGrab = FindFolders(ScantronHandle, type,# recursive call
                             SkipFolder, x, 
                             TempParent, 
                             NextFolderID)        
    if(nrow(FolderGrab)>0){                       # if any subfolders were returned from the folder just examined,
      SubFolders = rbind(SubFolders, FolderGrab)  # append them to the subfolders data.frame
    } # /if
  } # /for each subfolder
  
  
  ############ Section 4: Wrap Up ##########
  
  # If any subfolders were collected in the loop, append them to TempFolders
  if(nrow(SubFolders)>0){
    SubFolders$fname = paste0(parent,"/",SubFolders$fname)  # make the folder name have its parent category
    TempFolders = rbind(TempFolders, SubFolders)
  } 
  
  return(TempFolders)                                       # return the list of folders to the calling function
} # /function
