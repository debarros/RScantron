# logout.R
# This function ends the session with Scantron
# The only argument it takes is ScantronHandle
# It returns the content of the logout page

logout = function(ScantronHandle){
  #First, get the home page
  x = getURI(url = 'https://admin.achievementseries.com/home/home.ssp', 
             curl = ScantronHandle)
  
  #Find the location of the logout link
  y = gregexpr("Logout\\?z=.",x)[[1]][1]
  
  #Assemble the URI for the logout link
  z = paste0("https://admin.achievementseries.com",substring(x,y-6,y+15))
  
  #Fetch the URI, causing the session to be terminated
  getURI(url = z, curl = ScantronHandle)
}