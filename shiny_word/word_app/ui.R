library(shiny)
library(shinydlplot)


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
                           ".csv")),
      
      selectInput("ageGroup", "Age Group", c('No data entered')),
      selectInput("Year", "Year", c('No data entered')),
      selectInput("month", "Month", c('No data entered')),
      selectInput("site", "Site", c('No data entered')),
      selectInput("department", "Department", c('No data entered')),
      selectInput("satisfaction", "Patient Satisfaction", c('No data entered')),
      hr(),
      h5('Number of Inputs:'),
      textOutput('count')),
    # Add plots
    mainPanel(
      # Create tabs for different plots
      tabsetPanel(
        # Age group - Panel
        tabPanel('Age Group', plotOutput("ageGroupPlot")),
        # Experience per year - Panel
        tabPanel('Per Year', plotOutput("perYear")),
        # Experience per department and site - Panel
        tabPanel('Department', plotOutput("department"),
                 # radio buttons to filter by site
                 radioButtons("radio", label = h3("Site (please remove Site filters from side panel)"),
                              choices = list('All Sites'= 'f_RLH, f_AUH, f_BGH', "Royal" = 'f_RLH', "Ainetree" = 'f_AUH', 'Broadgreen' = 'f_BGH'), 
                              selected = 'f_RLH, f_AUH, f_BGH')),

        # word count - Panel
        tabPanel('Comments', verticalLayout(
          splitLayout(cellWidths = c("50%", "50%"), # side by side plot split
                      plotOutput("wordCount"), # wordcount 2 words
                      plotOutput("wordCount2")), # wordcount 3 words
          sliderInput("integer", "Number of top comments:",
                     # input slider for user defined number of phrases shown
                       min = 0, max = 100,
                      value = 30)),
          plotOutput("wordCount3") # word network
        ),
        # Raw data table - Panel
        tabPanel('Table', 
                 selectInput("select", "Select columns to display", c('No data entered'), multiple = TRUE), # select column inputs
                 radioButtons('comments', 'remove inputs with no comments?', list('No', 'Yes')), # filter samples with no comments
                 textInput("searchTerm", "Comments Search Term", ""),
                 h5('Number of Inputs in Table:'), textOutput('tableCount'),
                 downloadButton("downloadData", "Download Table"),
                 tableOutput('table'))
        )
      )
    )
  )
