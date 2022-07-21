library(shiny)
library(DT)

#allow for bigger file uploads (30Mbs)
options(shiny.maxRequestSize=30*1024^2)


ui = fluidPage(
  
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Include clarifying text ----
      #helpText(em("Note: This app requires file in csv format only!!")),
      helpText(em("Note:Select all the inputs and click on button as given below to exectute the app")),
      
      # input box for select columns
      selectInput("select", "Select columns to display", c('please enter data')),
      actionButton("update", "Update Data set", class = "btn-primary",style='padding:4px; font-size:120%')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2('The Mydata'),
      #tableOutput("mytable")
      tableOutput("mytable")
      
    )
  )
)


server <- function(session, input, output) {
  
  data <- reactive({
    req(input$dataset)
    read_csv(input$dataset$datapath)
  })
  
  filtereddata <- eventReactive({
    input$update
    data()
  },  {
    req(data())
    if(is.null(input$select) || input$select == "")
      data() else 
        data()[, colnames(data()) %in% input$select]
  })
  
  observeEvent(data(), {
    updateSelectInput(session, "select", choices=colnames(data()))
  })
  
  output$mytable  <- renderTable(filtereddata())
  
} 

shinyApp(ui = ui, server = server)