#Store Item Responses

StoreItemResponses = function(responses, testpath, classname, datafolder = "exports"){
  filename = paste0(testpath, "/", datafolder, "/", classname, "_itemresponses.csv")
  cat(responses, file = filename)
}
