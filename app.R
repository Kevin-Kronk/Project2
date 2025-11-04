library(shiny)
library(bslib)
library(readxl)

# Define the UI
ui <- page_sidebar(
  title = "Project 2",
  sidebar = sidebar(
    "Temporary",
    selectInput(
      inputId = "segment",
      label = "Segment",
      choices = 
        list(
          "All",
          "Consumer",
          "Corporate",
          "Home Office"
        ),
      selected = "All"
    ),
    selectInput(
      inputId = "category",
      label = "Category",
      choices = 
        list(
          "All",
          "Office Supplies",
          "Furniture",
          "Technology"
        ),
      selected = "All"
    ),
    selectInput(
      inputId = "region",
      label = "Region",
      choices = 
        list(
          "All",
          "Central",
          "South",
          "East",
          "West"
        ),
      selected = "All"
    ),
    selectInput(
      inputId = "ship",
      label = "Shipping Mode",
      choices = 
        list(
          "All",
          "Same Day",
          "First Class",
          "Second Class",
          "Standard Class"
        ),
      selected = "All"
    ),
    selectInput(
      inputId = "num1",
      label = "Select Numeric Variable 1",
      choices = 
        list(
          "Sales",
          "Quantity",
          "Discount",
          "Profit"
        ),
      selected = "Sales"
    ),
    sliderInput(
      inputId = "slider1",
      label = "Numeric Variable 1",
      min = 1,
      max = 100,
      value = c(1,100),
      step = 0.1
    ),
    selectInput(
      inputId = "num2",
      label = "Select Numeric Variable 2",
      choices = 
        list(
          "Sales",
          "Quantity",
          "Discount",
          "Profit"
        ),
      selected = "Quantity"
    ),
    sliderInput(
      inputId = "slider2",
      label = "Numeric Variable 2",
      min = 1,
      max = 100,
      value = c(1,100),
      step = 0.1
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
server <- function(input, output, session) {
  # Load the dataset once
  superstore <- read_excel("US Superstore data.xls", sheet = 1,
                           col_names = c("Row_ID", "Order_ID", "Order_Date",
                                         "Ship_Date", "Ship_Mode", "Customer_ID",
                                         "Customer_Name", "Segment", "Country",
                                         "City", "State", "Postal_Code",
                                         "Region", "Product_ID", "Category",
                                         "Sub_Category", "Product_Name", "Sales",
                                         "Quantity", "Discount", "Profit"),
                           col_types = c("numeric", "text", "date", "date",
                                         "text", "text", "text", "text", "text",
                                         "text", "text", "text", "text", "text",
                                         "text", "text", "text", "numeric",
                                         "numeric", "numeric", "numeric"),
                           skip = 1
  )
  
  
  #This code makes sure the select boxes update so they can't select the same variable in both!
  #first, update the 'num2' selections available
  observeEvent(input$num1, {
    num1 <- input$num1
    num2 <- input$num2
    choices <- c("Sales","Quantity","Discount","Profit")
    if (num1 != num2){
      choices <- choices[-which(choices == num1)]
      updateSelectizeInput(session,
                           "num2",
                           choices = choices,
                           selected = num2)
    }
  })
  #now, update the 'num1' selections available
  observeEvent(input$num2, {
    num1 <- input$num1
    num2 <- input$num2
    choices <- c("Sales","Quantity","Discount","Profit")
    if (num1 != num2){
      choices <- choices[-which(choices == num2)]
      updateSelectizeInput(session,
                           "num1",
                           choices = choices,
                           selected = num1)
    }
  })
  
  # Update the Numeric Variable Selection
  observe({
  updateSliderInput(session,
                    "slider1",
                    label = input$num1,
                    min = min(round(superstore[[input$num1]], 1)),
                    max = max(round(superstore[[input$num1]], 1)),
                    value = range(superstore[[input$num1]]),
                    step = 0.1)
  })
  
  observe({
    updateSliderInput(session,
                      "slider2",
                      label = input$num2,
                      min = min(round(superstore[[input$num2]], 1)),
                      max = max(round(superstore[[input$num2]], 1)),
                      value = range(superstore[[input$num2]]),
                      step = 0.1)
  })
    
  output$table <- renderTable({
    # Dependent on Pressing the Button
    input$subset

    # Use isolate to avoid dependence on ship
    isolate(superstore |>
    filter(superstore$`Ship Mode` == input$ship,
           superstore$Region == input$region))
    })
}

# Call the Function
  shinyApp(ui = ui, server = server)