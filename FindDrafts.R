# FindDrafts.R

FindDrafts = function (DraftFolderFrame, messageLevel = 0){
  DraftFrame = data.frame(stringsAsFactors = FALSE)
  for (i in 1:nrow(DraftFolderFrame)){
    DraftFolderRow = DraftFolderFrame[i,]
    x = FindDrafts_1Folder(DraftFolderRow)
    DraftFrame = rbind(DraftFrame, x)
  } # /for
  return(DraftFrame)
} # /function
