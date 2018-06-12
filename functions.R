# functions.R


#libraries
library(XML)
library(bitops)
library(RCurl)
library(stringr)
library(reshape2)
library(openxlsx)
library(googlesheets)
library(rrttReportBuilder)
library(dBtools)
library(magrittr)
library(httr)

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
source("BadReturnCheck.R")
source("UpdateMonitoring.R")
source("Spoil.R")
source("GetDraftName.R")

#Data
loginurls = list("step1" = "https://admin.achievementseries.com/Auth/Login/Org", 
                 "step2" = "https://admin.achievementseries.com/Auth/?returnUrl=%2FAuth%2FLogin%2FUser")
TABpath = "\\\\stuthin2/Data/tests/2017-2018/TAB.xlsx"
TeacherLookup = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "TeacherLookup")
CourseLookup = read.xlsx(xlsxFile = "Parameters and Settings.xlsx", sheet = "CourseLookup")
ScannedTests.url.text = "https://docs.google.com/spreadsheets/d/1js6XcxzF4y3uFtc_Uxr8e3UfvvcKjUrQzL8lKV2st1I/edit"
TAB.wb = loadWorkbook(xlsxFile = TABpath)      # Read in the TAB (Test Address Book)
agent = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/37.0.2049.0 Safari/537.36"
