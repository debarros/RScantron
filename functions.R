# functions.R


#libraries
library(XML)
library(RCurl)
library(stringr)
library(reshape2)
library(openxlsx)
library(googlesheets)

#functions
source("FindDrafts.R")
source("FindDrafts_1Folder.R")
source("login.R")
source("FindEvents.R")
source("FindFolders.R")
source("FindStudents.R")
source("FindTests.R")
source("FindTests_1Folder.R")
source("FindRecent.R")
source("logout.R")
source("getToken.R")
source("getOrgID.R")
source("FindEvents_1Student.R")
source("FindClasses.R")
source("ScoreUpdates.R")
source("FindResults.R")
source("ObtainNewCert.R")
source("StoreDrafts.R")

#Data
TeacherLookup = read.csv(file = "TeacherLookup.csv", stringsAsFactors = FALSE)
CourseLookup = read.xlsx(xlsxFile = "CourseLookup.xlsx",sheet = 1)
