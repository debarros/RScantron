#StoreDrafts.R

#' @title Store Test Drafts
#' @description Retrieve the web page corresponding to each draft test
#' @param DraftFrame a data.frame with one row for each draft test (the output of the FindDrafts function)
#' @param MaxDrafts maximum number of test drafts to get
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print
#' @return DraftFrame gets returned, but with an added column called "page"
StoreDrafts = function(DraftFrame, MaxDrafts = NA_integer_, messageLevel = 0, agent = NULL){
  
  if(is.null(agent)){
    agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
  }
  
  urlStub = "https://admin.achievementseries.com/test-draft/content/list.ssp?id="
  DraftFrame$page = ""
  if(is.na(MaxDrafts)){
    lastRow = nrow(DraftFrame)
  } else { 
    lastRow = MaxDrafts
  }
  
  for(i in 1:lastRow){
    DraftFrame$page[i] = httr::content(
      httr::GET(url = paste0(urlStub,DraftFrame$tid[i]),user_agent(agent)),
      as = "text",
      encoding = "UTF-8")
  }
  return(DraftFrame)
} # /StoreDrafts function
