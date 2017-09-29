# functions.R


#libraries
library(XML)
library(RCurl)
library(stringr)
library(reshape2)
library(openxlsx)
library(googlesheets)
library(rrttReportBuilder)
library(dBtools)

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
TeacherLookup = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "TeacherLookup")
CourseLookup = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "CourseLookup")
