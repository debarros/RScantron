#server.R

shinyServer(function(input, output, session) {
  
  ####### Section 0: Initialize Variables #######
  
  caLocation = character()
  AvailableActions = c("choose one", "login")

  
  ####### Section 1: Display Components #######
  # ChooseAction is the main dropdown box, displaying what actions are currently available
  # Param1 through Param4 are widgets that change depending on what action is selected, for parameters related to the action
  # Button is an action button
  
  
  output$ChooseAction = renderUI(selectInput("Action", "What do you want to do?",AvailableActions,"choose one"))
  
  output$Param1 = renderUI({
    if(is.null(input$Action)){return()
    }else if(input$Action == "login"){textInput("username", "*Username:","")
    }else if(input$Action == "Get Published Test Folders"){textInput("SkipTestFolder", "What folder should be skipped?","")
    }else if(input$Action == "Get Draft Folders"){textInput("SkipDraftFolder", "What draft folder should be skipped?","")
    }else if(input$Action == "Get Scheduled Session Folders"){textInput("SkipSessionFolder", "What session folder should be skipped?","")
    }else return()
  })
  
  output$Param2 = renderUI({
    if(is.null(input$Action)){return()
    } else if(input$Action == "login"){textInput("password", "*Password:", "")
    }else return()                                     
  })
  
  output$Param3 = renderUI({
    if(is.null(input$Action)){return()
    } else if(input$Action == "login"){textInput("SiteCode", "*SiteID", "")
    }else return()
  })
  
  output$Button = renderUI({
    if(is.null(input$Action) || input$Action == "choose one"){return()
    }else return(submitButton("Go"))
  })
  
  ####### Section 2: Function Calls #######
  # Each function loads a different set of information from Scantron.
  # Some functions depend on each other.  All depend on login.
  # If running a function is the prereq for another function, the second is added to AvailableOptions.
  
  
  #Login and return the cURL handle
  ScantronHandle = reactive({
    if (is.null(input$Action)){
      return()
    } else if (input$Action == "login"){
      if ("Get Published Test Folders" %in% AvailableActions){
      }else  AvailableActions = c(AvailableActions, "Get Published Test Folders", "Get Draft Folders", "Get Scheduled Session Folders", "Get Students")
      updateSelectInput(session, ChooseAction, selected = "choose one")
      return(login(username, password, SiteCode, caLocation))
    } else return()
  })
  
  # Get the complete list of published test folders with their folder ID's  
  TestFolderFrame = reactive({
    if (is.null(input$Action)){
      return()
    } else if (input$Action == "Get Published Test Folders"){
      if ("Get Tests" %in% AvailableAction){
      }else AvailableActions = c(AvailableActions, "Get Tests")
      return(FindFolders(ScantronHandle, "t", SkipTestFolder))
    } else return()
  })  
  
  # Get the complete list of published test folders with their folder ID's    
  DraftFolderFrame = reactive({
    if (is.null(input$Action)){
      return()
    } else if (input$Action == "Get Draft Test Folders"){
      if ("Get Drafts" %in% AvailableAction){
      }else AvailableActions = c(AvailableActions, "Get Drafts")
    return(FindFolders(ScantronHandle, "d", SkipDraftFolder))
    } else return()
  })
  
  # Get the complete list of published test folders with their folder ID's  
  SessionFolderFrame = reactive({
    if (is.null(input$Action)){
      return()
    } else if (input$Action == "Get Scheduled Session Folders"){
      if ("Get Scheduled Sessions" %in% AvailableAction){
      }else AvailableActions = c(AvailableActions, "Get Scheduled Sessions")
    return(FindFolders(ScantronHandle, "s", SkipSessionFolder))
  } else return()
  })
  
  # Get the complete list of tests with their test ID's and containing folders
  TestFrame = reactive({
    if (is.null(input$Action)){
      return()
    } else if (input$Action == "Get Tests"){
      if ("Another Option" %in% AvailableAction){
      }else AvailableActions = c(AvailableActions, "Another Option")
    return(FindTests(TestFolderFrame))
  } else return()
  })
  
  # Get the complete list of students
  StudentFrame = reactive({
    if (is.null(input$Action)){
      return()
    } else if (input$Action == "Get Students"){
      if ("Get Testing Events" %in% AvailableAction){
      }else AvailableActions = c(AvailableActions, "Get Testing Events")
      return(FindTests(TestFolderFrame))
    return(FindStudents(ScantronHandle))
  } else return()
  })
  
  # Get the complete list of instances in which a student has taken a test
  EventFrame = reactive({  
    if (is.null(input$Action)){
      return()
    } else if (input$Action == "Get Testing events"){
      if ("Another Option" %in% AvailableAction){
      }else AvailableActions = c(AvailableActions, "Another Option")
      return(FindTests(TestFolderFrame))
    return(FindEvents(StudentFrame, ScantronHandle))
  } else return()
  })
  
}) #end of shinyServer function