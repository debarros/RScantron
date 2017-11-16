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
library(magrittr)

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
source("GetItemResponses.R")
source("StoreItemResponses.R")
source("UpdateTab.R")
source("DetermineCurrentSections.R")
source("FindMissingTests.R")
source("FixHtmlChars.R")

#Data
TABpath = "\\\\stuthin2/Data/tests/2017-2018/TAB.xlsx"
TeacherLookup = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "TeacherLookup")
CourseLookup = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "CourseLookup")
ScannedTests.url.text = "https://docs.google.com/spreadsheets/d/1js6XcxzF4y3uFtc_Uxr8e3UfvvcKjUrQzL8lKV2st1I/edit"
TAB.wb = loadWorkbook(xlsxFile = TABpath)      # Read in the TAB (Test Address Book)
