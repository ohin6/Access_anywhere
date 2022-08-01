# import Libaries
library(shiny)
require(shinyjs)
# library(shinydlplot)

# Initiate UI
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
      # Select filter inputs
      selectInput("ageGroup", "Age Group", c('No data entered')),
      selectInput("Year", "Year", c('No data entered')),
      selectInput("month", "Month", c('No data entered')),
      selectInput("site", "Site", c('No data entered')),
      selectInput("department", "Department", c('No data entered')),
      selectInput("satisfaction", "Patient Satisfaction", c('No data entered')),
      hr(),
      
      # Show number of input files
      h5('Number of Inputs:'),
      textOutput('count')),
    
    # Main panel - display plots
    mainPanel(
      
      # Create tabs for different plots
      tabsetPanel(
        
        # Age group - Panel
        tabPanel('Age Group', 
                 h3('Plot'),
                 plotOutput("ageGroupPlot"),
                 tableOutput("ageTable"),
                 downloadButton("downloadageGroup", "Download Plot")),
        
        # Experience per year - Panel
        tabPanel('Per Year',
                 h3('Plot'),
                 plotOutput("perYear"),
                 downloadButton("downloadYear", "Download Plot")),
        
        # Experience per department and site - Panel
        tabPanel('Department',
                 h3('Filters'),
                 
                 # split view
                 splitLayout(cellWidths = c('50%', '50%'),
                             
                             # radio buttons to filter by site
                             radioButtons("radio", label = "Site (please remove Site filters from side panel)",
                                          choices = list('All Sites'= 'f_RLH, f_AUH, f_BGH', "Royal" = 'f_RLH', "Ainetree" = 'f_AUH', 'Broadgreen' = 'f_BGH'), 
                                          selected = 'f_RLH, f_AUH, f_BGH'),
                             
                             # User defined filter for minimum sample size per department
                             sliderInput("minSample", "Filter by departments with a minimum number of inputs",
                                         # input slider for user defined number of phrases shown
                                         min = 0, max = 500,
                                         value = 100)),
                 hr(),
                 
                 # Display plot
                 h3('Plot'),
                 plotOutput("department"),
                 
                 # Download button
                 downloadButton("downloadSatDep", "Download Plot"),
                 
        ),
        
        # word count - Panel
        tabPanel('Comments',
                 
                 # Download buttons for bi and trigrams
                 downloadButton("downloadBigram", "Download Bigram"),
                 downloadButton("downloadTrigram", "Download Trigram"),
                 
                 # Plots with split layout
                 h3('Plots'),
                 verticalLayout(
                   splitLayout(cellWidths = c("50%", "50%"), # side by side plot split
                               plotOutput("wordCount"), # wordcount 2 words
                               plotOutput("wordCount2")), # wordcount 3 words
                   
                   # User defined input for showing top phrases    
                   sliderInput("integer", "Number of top comments:",
                               min = 0, max = 100,
                               value = 30)),
                 hr(),
                 
                 # Download button for igraph
                 downloadButton("downloadiGraph", "Download iGraph"),
                 
                 # Plot igraph
                 h3('Plot'),
                 plotOutput("wordCount3") # word network
        ),
        
        
        # Raw data table - Panel
        tabPanel('Table',
                 selectInput("select", h4("Select columns to display"), choices = c('No data entered'), multiple = TRUE), # select column inputs
                 hr(),
                 
                 # Filter Comments with split view
                 h4('Filter Comments'),
                 splitLayout(cellWidths = c("60%", "40%"),
                             textInput("searchTerm", "Comments Search Term", "", width = '80%'),
                             radioButtons('comments', 'Remove inputs with no comments?', list('No', 'Yes'))), # filter samples with no comments
                 hr(),
                 
                 # Show number of inputs
                 h5('Number of Inputs in Table:'), textOutput('tableCount'),
                 hr(),
                 
                 # Download button
                 downloadButton("downloadData", "Download Table"),
                 
                 #Display table
                 h3('Table'), h6('Select at least one column to show table'),
                 tableOutput('table'))
      )
    )
  )
)
