#StoreDrafts.R
StoreDrafts = function(DraftFrame,
                       MaxDrafts = NA_integer_,
                       messageLevel = 0,
                       agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"){
  urlStub = "https://admin.achievementseries.com/test-draft/content/list.ssp?id="
  DraftFrame$page = ""
  if(is.na(MaxDrafts)){
    lastRow = nrow(DraftFrame)
  } else { 
    lastRow = MaxDrafts
  }
  
  for(i in 1:lastRow){
    DraftFrame$page[i] = httr::content(httr::GET(url = paste0(urlStub,DraftFrame$tid[i]),
                                           user_agent(agent)),
                                 as = "text",
                                 encoding = "UTF-8")
  }
  return(DraftFrame)
}