# FindDrafts.R


#' @title Find Drafts
#' @description Find all test drafts in Achievement Series
#' @param DraftFolderFrame output from the  function FindFolders (with parameter \code{type} set to "d")
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print
#' @return data.frame with 1 row for each test draft and columns holding the test name, folder path, and test ID
FindDrafts = function (DraftFolderFrame, messageLevel = 0){
  
  if(messageLevel > 0){
    print("Finding test drafts")
  }
  
  DraftFrame = data.frame(stringsAsFactors = FALSE)
  for (i in 1:nrow(DraftFolderFrame)){
    DraftFolderRow = DraftFolderFrame[i,]
    x = FindDrafts_1Folder(DraftFolderRow)
    DraftFrame = rbind(DraftFrame, x)
  } # /for
  
  return(DraftFrame)
  
} # /FindDrafts function
