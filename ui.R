#ui.R

#Note:
# Some of the outputs will be CSV files of the various *Frame variables
# Later outputs will probably be similar, but based on later functions


shinyUI(fluidPage(
  
  titlePanel(h1("Log Into Scantron Achievement Series")),
  
  
  sidebarLayout(

    sidebarPanel(),#end of sidebarPanel
    
    mainPanel(
      
      uiOutput("Button"),
      uiOutput("ChooseAction"),
      uiOutput("Param1"),
      uiOutput("Param2"),
      uiOutput("Param3")
      
      
    )#end of mainPanel
    
  )#end of SidebarLayout
  
))#end of shinyUI