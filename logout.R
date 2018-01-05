# logout.R
# This function ends the session with Scantron
# The only argument it takes is messagelevel
# It returns the content of the logout page

logout = function(messageLevel = 0,
                  agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36") {
  #First, get the home page
  x <-
    httr::content(
      httr::GET(url = 'https://admin.achievementseries.com/home/home.ssp',
                user_agent(agent)),
      as = "text",
      encoding = "UTF-8"
    )
  # x <- httr::content(x, as = "text")
  
  if (BadReturnCheck(x)) {
    stop("You are already logged out.")
  }
  
  #Find the location of the logout link
  y = gregexpr("Logout\\?z=.", x)[[1]][1]
  
  #Assemble the URI for the logout link
  z = paste0("https://admin.achievementseries.com",
             substring(x, y - 6, y + 15))
  
  #Fetch the URI, causing the session to be terminated
  x <- httr::GET(url = z, user_agent(agent))
} # /function
