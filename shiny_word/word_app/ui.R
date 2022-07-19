library(shiny)

ui = fluidPage(
  # Titil
  titlePanel("Uploading Files"),
  
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel
    (
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
                ) 
    ),
    # Add plots
    mainPanel("main panel",
              fluidRow(
                # Split screen
                verticalLayout(plotOutput("ageGroupPlot"),
                               plotOutput("wordCount"),
                               tableOutput('table'))
                      )
              )
          )
)