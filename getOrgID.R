#getOrgID.R

# This function takes the following arguments:
#   x - the second login page
#   messageLevel - the extent of messages to print
# It finds and returns the post form value "OrganizationId"
# This is necessary (I think) for getting through the second step of the login process


getOrgID = function (x, messageLevel = 0){
  if(messageLevel > 0){
    print("Getting the OrganizationID")
  }
  OrgIDInputStart = regexpr(pattern = "OrganizationId", text = x)[1]    #Find where the input tag starts
  ValueLocations = gregexpr(pattern = 'value=', text = x)[[1]]          #Find where the "value"s are
  OrgIDStart = ValueLocations[ValueLocations > OrgIDInputStart][1] + 7  #Find the end of the first "value" after the start of the tag
  QuoteLocations = gregexpr(pattern = '\\"', text = x)[[1]]             #Find where the quotation marks are
  OrgIDEnd = QuoteLocations[QuoteLocations > OrgIDStart][1]-1           #Find the quote at the end of the token
  return(substr(x,OrgIDStart,OrgIDEnd))                                 #return the organization ID
} # /function
