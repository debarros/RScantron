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

#Data
TABpath = "\\\\stuthin2/Data/tests/2017-2018/TAB.xlsx"
TeacherLookup = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "TeacherLookup")
CourseLookup = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "CourseLookup")
Coursecode2Testcode = read.xlsx(xlsxFile = TABpath, sheet = "Course Codes", startRow = 2)
Sections = read.xlsx(xlsxFile = TABpath, sheet = "Sections")
Coursecode2Course = set_colnames(
  x = as.data.frame(
    t(read.xlsx(xlsxFile = TABpath, 
                sheet = "Course Codes", colNames = F, rowNames = F, rows = 1:2)), 
    stringsAsFactors = F), 
  value = c("Course","CourseCode"))
ScannedTests.url.text = "https://docs.google.com/spreadsheets/d/1js6XcxzF4y3uFtc_Uxr8e3UfvvcKjUrQzL8lKV2st1I/edit"
TAB = readWorkbook(xlsxFile = TABpath, sheet = "TAB") # Read in the TAB (Test Address Book)
TAB.wb = loadWorkbook(xlsxFile = TABpath)