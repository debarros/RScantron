# functions.R


#libraries
library(XML)
library(RCurl)
library(stringr)
library(reshape2)


#functions
source("FindDrafts.R")
source("login.R")
source("FindEvents.R")
source("FindFolders.R")
source("FindStudents.R")
source("FindTests.R")
source("FindRecent.R")
source("logout.R")
source("getToken.R")
source("getOrgID.R")
source("FindEvents_1Student.R")
TeacherLookup = read.csv(file = "TeacherLookup.csv", stringsAsFactors = FALSE)
CourseLookup = read.csv(file = "CourseLookup.csv", stringsAsFactors = FALSE)
