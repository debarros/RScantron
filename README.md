# RScantron
**R code for logging into the Scantron Achievement Series website** 

**Paul de Barros, Schenectady, NY, USA**

## Structure:

* ui.R and server.R should contain as little as possible
* All functions should be stored in separate files to make it easier to implement improvements
* When there is a function that creates a data.frame and  calls a function that creates a row, keep those in the same file.

## Naming conventions:

* functions:
    * PascalCase
    * format should be FindX, where X = what comes out
    * When there is a subfunction that is called using one row at a time from a data.frame, use a 1 in the function name
* data frames:
    * Use PascalCase
    * end name of object with "Frame"
* variables:
    * use bactrianCase
    * if the variable is a row from a data.frame, replace Frame with Row in the name
    * if the variable is a value from a row from a data.frame, use the column name
* Scantron entities
    * Test = published test
    * Draft = draft test
    * Session = scheduled session
    * Student = student
    * Event = the event of a student taking a test
    * Score = student score on a test
    * TestFolder = published test folder
    * DraftFolder = draft test folder
    * SessionFolder = scheduled session folder
    * Class = a class section
    

## New Functions:

### These functions are not yet written, but should be easy
* FindRecentScans # input the date and get all events since that date
* FindDrafts # make a list of test drafts with their id codes
* FindClasses # get a list of classes with their id codes
* FindClassAverages # get every class average for a test, or for all tests
  

### These functions will be a little more complicated to produce
* CreateReport # use RMarkDown to write a report, or just load data in a template
* SendReport # send an email with the report attached
* UpdateScoreMonitoring # load data into the score monitoring spreadsheets
* For now, the CreateReport and UpdateScoreMonitoring functions should simply load data into templates.
  * The CreateReport feature should check to see if the template already exists in the folder.  
  * If it doesn't, it will put a copy there.  If it does, it will just update the one that's there.
  * That way, the custom scoring can be stored individually in the score reports.
  * When the test is set up, the score report can be copied into the folder and the scoring added manually.,
  * Later, the scoring formula format can be formalized, and the formula can be placed as a comment in the published test.
  * Note: the test description can only hold 50 characters.
    * The Session description space can hold up to 100 characters.  Still not enough.
    * The draft description can hold 50 characters.
    * The scan operator instructions for a scheduled test can hold lots of characters, 
    * but there doesn't seem to be a way to access that data.


### These functions will require integration with ExamView
* CopyDraft # save a local copy of an ako draft in some simple format
* MakeLocalDraft # convert some simple format into ExamView form


### These functions rely on the idea of creating a local mirror of all data
* BuildMirror # Create a local copy of all data
* StoreMirror # save that copy
* LoadMirror # load it from memory
* CompareMirror # Compare the current state to a former state 
* The CompareMirror function should generate a To Do list of Score Reports and Score Updates.
* The StoreMirror function should update the score monitoring spreadsheet