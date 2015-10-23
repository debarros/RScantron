FindDrafts = function (DraftFolderFrame){
  DraftFrame = data.frame(stringsAsFactors = FALSE)
  for (i in 1:nrow(DraftFolderFrame)){
    DraftFolderRow = DraftFolderFrame[i,]
    x = FindDrafts_1Folder(DraftFolderRow)
    DraftFrame = rbind(DraftFrame, x)
  }
  return(DraftFrame)
}

