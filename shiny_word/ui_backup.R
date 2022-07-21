library(shiny)

not_sel <- "Not Selected"




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
                           ".csv"))
      
      
    ),
    # Add plots
    mainPanel(
      tabsetPanel(
        tabPanel('Age Group', plotOutput("ageGroupPlot")),
        tabPanel('Per Year', plotOutput("perYear")),
        tabPanel('Department', plotOutput("department"),
                 radioButtons("radio", label = h3("Site"),
                              choices = list("Royal" = 'f_RLH', "Ainetree" = 'f_AUH', 'Broadgreen' = 'f_BGH', 'All Sites'= 'f_RLH, f_AUH, f_BGH'), 
                              selected = 'f_RLH'),
                 downloadButton('Department', label = 'Download', class = '')),
        tabPanel('Comments', verticalLayout(
          splitLayout(cellWidths = c("50%", "50%"),
                      plotOutput("wordCount"),
                      plotOutput("wordCount2")),
          sliderInput("integer", "Integer:",
                      min = 0, max = 100,
                      value = 30)),
          plotOutput("wordCount3")
        ),
        
        tabPanel('Table', 
                 selectInput("select", "Select columns to display", c('No data entered'), multiple = TRUE),
                 selectInput("FilterageGroup", "ageGroup", c('No data entered')),
                 tableOutput('table'))
        
      )
    )
  )
)