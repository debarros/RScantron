#Get Item Responses

GetItemResponses = function(ClassID, TestID, curlhandle){
  responses = getURI(
    paste0(
      "https://admin.achievementseries.com/report/class/responses.csv?",
      "c=", ClassID, 
      "&t=", TestID, 
      "&v=table&_list=Students"), 
    curl=curlhandle)
  responses = gsub(pattern = "\r\n", replacement = "\n",x = responses) # replace CRLF with LF to avoid blank lines
  return(responses)
} # /GetItemResponses
