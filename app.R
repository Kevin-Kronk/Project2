library(shiny)
library(bslib)
library(readxl)

# Define the UI
ui <- page_sidebar(
  title = "Project 2",
  sidebar = sidebar(
    "Temporary",
    selectInput(
      inputId = "ship",
      label = "Shipping Mode",
      choices = 
        list(
          "First Class",
          "Second Class",
          "Standard Class"
        )
    ),
    sliderInput(
      inputId = "quantity",
      label = "Quantity",
      min = 1,
      max = 100,
      value = c(1,100)
    ),
    sliderInput(
      inputId = "profit",
      label = "Profit",
      min = 1,
      max = 100,
      value = c(1,100)
    ),
    actionButton(
      inputId =  "subset",
      label = "Get The Data!"
    )
  ),
  navset_card_underline(
    nav_panel("About",
              "Describe the purpose of the app."),
    nav_panel("Data Download",
              tableOutput(outputId = "table")  
    ),
    nav_panel("Data Exploration",
              "Allow the user to obtain the numeric and graphical summaries.")
  )  
)

# Define the Server
server <- function(input, output) {
  # Load the dataset once
  superstore <- read_excel("US Superstore data.xls", sheet = 1)
  
  output$table <- renderTable({
    # Dependent on Pressing the Button
    input$subset

    # Use isolate to avoid dependence on ship
    isolate(superstore |>
    filter(superstore$`Ship Mode` == input$ship))
    })
}

# Call the Function
  shinyApp(ui = ui, server = server)