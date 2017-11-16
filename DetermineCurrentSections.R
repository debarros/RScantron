#DetermineCurrentSections.R

DetermineCurrentSections = function(testname, CustomSectioning, 
                                    Sections, testcode, 
                                    Coursecode2Testcode, Coursecode2Course, 
                                    messageLevel = 0){
  
  # Determine the sections for this test code
  if(testname %in% CustomSectioning$TestTitle){ # if there are custom sections, just get those
    classIDs = CustomSectioning$ClassID_List[CustomSectioning$TestTitle == testname]
    classIDs = strsplit(x = classIDs, split = ",")[[1]] # strsplit returns a list, but we just want the contents of the list, hence the [[1]]
    if(all(classIDs %in% Sections$ClassID)){
      currentSections = Sections[Sections$ClassID %in% classIDs,]  
    } else {
      badIDs = classIDs[!(classIDs %in% Sections$ClassID)]
      stop(paste0("The following classIDs are associated with ", testname, ", but are not in the Sections table in the TAB: ",VectorSentence(badIDs)))
    }
  } else { # if there are no custom sections
    # Determine the courses associated with this test code
    coursecodes = colnames(Coursecode2Testcode)[Coursecode2Testcode[Coursecode2Testcode$testcode == testcode,] == 1]
    courses = Coursecode2Course$Course[Coursecode2Course$CourseCode %in% coursecodes]
    # Get all the sections of those courses
    currentSections = Sections[Sections$ClassName %in% courses,]
  } # /if-else to determine sections
  
  return(currentSections)
} # /function
