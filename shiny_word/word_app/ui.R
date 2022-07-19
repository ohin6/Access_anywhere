library(shiny)

ui = fluidPage(
  # Title
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
                               sliderInput("integer", "Integer:",
                                           min = 0, max = 100,
                                           value = 30),
                               tableOutput('table'))
                      )
              )
          )
)