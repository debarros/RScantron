#StoreDrafts.R
StoreDrafts = function(DraftFrame, MaxDrafts = NA_integer_, messageLevel = 0){
  urlStub = "https://admin.achievementseries.com/test-draft/content/list.ssp?id="
  DraftFrame$page = ""
  if(is.na(MaxDrafts)){
    lastRow = nrow(DraftFrame)
  } else { 
    lastRow = MaxDrafts
  }
  
  for(i in 1:lastRow){
    DraftFrame$page[i] = getURI(paste0(urlStub,DraftFrame$tid[i]), curl=ScantronHandle)
  }
  return(DraftFrame)
}