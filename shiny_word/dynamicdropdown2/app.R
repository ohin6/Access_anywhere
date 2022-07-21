library("tidyverse")
library("shiny")


setwd('Y:/Liverpool projects/Video_feedback/')


ui <- fluidPage(
  selectInput(inputId = 'colNames',
              label = 'choose column',
              choices = colnames())
)






server <- function(input, output, session) {
df = read_csv('../../rawData/raw_data.csv')


#update input id dynamically
observe({
  updateSelectInput(session, 'colnames', choices = colnames(df))
  
})
  
  
}

shinyApp(ui = ui, server = server)

