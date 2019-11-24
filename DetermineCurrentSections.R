#DetermineCurrentSections.R

#' @title Determine Current Sections
#' @description Given a test code, determine what class sections are associated with it
#' @param testname character of length 1 with the name of the desired test
#' @param CustomSectioning table of custom sections
#' @param Sections table of sections
#' @param testcode testcode for the current test
#' @param Coursecode2Testcode table associating course codes and test codes
#' @param Coursecode2Course table associated course codes and courses
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print.  Defaults to 0.
#' @return data.frame
#' @details This function should be called from other functions
DetermineCurrentSections = function(testname, CustomSectioning, 
                                    Sections, testcode, 
                                    Coursecode2Testcode, Coursecode2Course, 
                                    messageLevel = 0){
  
  if(messageLevel > 0){ print(paste0("Determining sections for test code ", testcode)) }
  
  # Determine the sections for this test code
  if(testname %in% CustomSectioning$TestTitle){                                       # if there are custom sections, just get those
    classIDs = CustomSectioning$ClassID_List[CustomSectioning$TestTitle == testname]
    classIDs = strsplit(x = classIDs, split = ",")[[1]]                               # strsplit returns a list, but we just want the contents of the list, hence the [[1]]
    if(all(classIDs %in% Sections$ClassID)){                                          # Make sure the classIDs specified in the custom sectioning exist
      currentSections = Sections[Sections$ClassID %in% classIDs,]  
    } else {
      badIDs = classIDs[!(classIDs %in% Sections$ClassID)]
      stop(paste0("The following classIDs are associated with ", testname, ", but are not in the Sections table in the TAB: ",VectorSentence(badIDs)))
    }
  } else {                                                                            # if there are no custom sections
    
    courses = DetermineCourses(                                                       # Determine the courses associated with this test code
      testcode = testcode, 
      Coursecode2Testcode = Coursecode2Testcode, 
      Coursecode2Course = Coursecode2Course, 
      messageLevel = messageLevel - 1)
    
    # Get all the sections of those courses
    currentSections = Sections[Sections$ClassName %in% courses,]
    
    if(nrow(currentSections) < 1){
      stop(paste0("Although the testcode ", testcode, "is associated with the course(s) ", VectorSentence(courses), ", 
                  there are no sections for those courses in the section table of the TAB."))
    }
    
  } # /if-else to determine sections
  
  if(messageLevel > 0){ print(paste0("Done determining sections for this test code")) }
  
  return(currentSections)
} # /DetermineCurrentSections function






#' @title Determine Current Courses
#' @description Given a test code, determine what courses are associated with it
#' @param testcode abbreviation at the beginning of a test name
#' @param Coursecode2Testcode table associating course codes and test codes
#' @param Coursecode2Course table associated course codes and courses
#' @param messageLevel integer of length 1 indicating level of diagnostic messages to print.  Defaults to 0.
#' @return data.frame
#' @details This function should be called from other functions
DetermineCourses = function(testcode, 
                            Coursecode2Testcode,
                            Coursecode2Course, 
                            messageLevel = 0){
  coursecodes = colnames(Coursecode2Testcode)[Coursecode2Testcode[Coursecode2Testcode$testcode == testcode,] == 1]
  courses = Coursecode2Course$Course[Coursecode2Course$CourseCode %in% coursecodes]
  if(length(courses) < 1){
    stop(paste0("The testcode ", testcode, " is not associated with any courses."))
  }
  return(courses)
} #/ DetermineCourses function
