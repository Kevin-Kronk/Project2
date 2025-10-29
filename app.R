library(shiny)
library(bslib)

# Define the UI
ui <- page_sidebar(
  title = "Project 2",
  sidebar = sidebar(
    "Temporary"
  )
)

# Define the Server
server <- function(input, output) {
  
}

# Call the Function
  shinyApp(ui = ui, server = server)