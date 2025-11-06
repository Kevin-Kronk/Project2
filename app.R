library(shiny)
library(bslib)
library(readxl)
library(tidyverse)
library(DT)

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
          "None",
          "Sales",
          "Quantity",
          "Discount",
          "Profit"
        ),
      selected = "None"
    ),
    
    uiOutput("slider1"), 
    
    selectInput(
      inputId = "num2",
      label = "Select Numeric Variable 2",
      choices = 
        list(
          "None",
          "Sales",
          "Quantity",
          "Discount",
          "Profit"
        ),
      selected = "None"
    ),
    
    uiOutput("slider2"),
    
    actionButton(
      inputId =  "subset",
      label = "Get The Data!"
    )
  ),
  # To set it so that the navset_card_underline can scroll down
  tags$style(HTML(".navset-card-underline .card {
                  max-height: 1200px;
                  overflow-y: auto;}")),
  navset_card_underline(
    nav_panel("About",
              "Describe the purpose of the app."),
    nav_panel("Data Download",
              downloadButton(outputId = "downloadData"),
              DT::dataTableOutput(outputId = "table")
    ),
    nav_panel("Data Exploration",
              headerPanel("Data Selection"),
              radioButtons(inputId = "data_type", 
                           label = "Type of Data to Explore",
                           choices = list("Categorical", "Numerical"),
                           inline = TRUE),
              conditionalPanel(
                condition = "input.data_type == `Categorical`",
                fluidRow(column(width=4, 
                                selectInput(
                                  inputId = "one_cat_var",
                                  label = "Choose a Categorical Variable:",
                                  choices = 
                                    list(
                                      "Ship_Mode",
                                      "Segment",
                                      "Region",
                                      "Category"
                                    ),
                                  selected = "Ship_Mode"
                                )),
                         column(width = 4,
                                selectInput(
                                  inputId = "two_cat_var",
                                  label = "Choose a Second Categorical Variable:",
                                  choices = 
                                    list(
                                      "Ship_Mode",
                                      "Segment",
                                      "Region",
                                      "Category"
                                    ),
                                  selected = "Segment"
                                )),
                         column(width = 4,
                                selectInput(
                                  inputId = "three_num_var",
                                  label = "Choose a Plot Numerical Variable:",
                                  choices = 
                                    list(
                                      "Sales",
                                      "Quantity",
                                      "Discount",
                                      "Profit"
                                    ),
                                  selected = "Sales"
                                ))),
                h3(textOutput("one_way_title")),
                tableOutput("one_way_cont"),
                h3(textOutput("two_way_title")),
                tableOutput("two_way_cont"),
                h3(textOutput("bar_title")),
                plotOutput("bar_plot"),
                h3(textOutput("box_title")),
                plotOutput("box_plot"),
                h3(textOutput("heatmap_title")),
                plotOutput("heatmap")
              ),
              conditionalPanel(
                condition = "input.data_type == `Numerical`",
                fluidRow(column(width=4, 
                                selectInput(
                                  inputId = "num_sum_stat",
                                  label = "Choose a Numerical Variable:",
                                  choices = 
                                    list(
                                      "Sales",
                                      "Quantity",
                                      "Discount",
                                      "Profit"
                                    ),
                                  selected = "Sales"
                                )),
                         column(width = 4,
                                selectInput(
                                  inputId = "cat_level",
                                  label = "Across Levels of Categorical Variable:",
                                  choices = 
                                    list(
                                      "Ship_Mode",
                                      "Segment",
                                      "Region",
                                      "Category"
                                    ),
                                  selected = "Ship_Mode"
                                )),
                         column(width=4, 
                                selectInput(
                                  inputId = "num_sum_stat_2",
                                  label = "Choose a Plot Numerical Variable:",
                                  choices = 
                                    list(
                                      "Sales",
                                      "Quantity",
                                      "Discount",
                                      "Profit"
                                    ),
                                  selected = "Quantity"
                                )),),
                h3(textOutput("num_sum_stats_title")),
                tableOutput("basic_sum_stats"),
                h3(textOutput("cat_level_title")),
                tableOutput("levels_sum_stats"),
                h3(textOutput("density_title")),
                plotOutput("density_plot"),
                h3(textOutput("layered_density_title")),
                plotOutput("layered_density_plot"),
                h3(textOutput("scatter_title")),
                plotOutput("scatter_plot")
                )
              )
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
    choices <- c("None", "Sales","Quantity","Discount","Profit")
    if (num1 != "None") {
      choices <- choices[-which(choices == num1)]
    }
    updateSelectizeInput(session,
                         "num2",
                         choices = choices,
                         selected = num2)
  })
  
  #now, update the 'num1' selections available
  observeEvent(input$num2, {
    num1 <- input$num1
    num2 <- input$num2
    choices <- c("None", "Sales","Quantity","Discount","Profit")
    if (num2 != "None") {
      choices <- choices[-which(choices == num2)]
    }
    updateSelectizeInput(session,
                         "num1",
                         choices = choices,
                         selected = num1)
  })
  
  # Update the Numeric Variable Selection
  output$slider1 <- renderUI({
    if (input$num1 == "None") {
      return(NULL)
    }
    
    sliderInput(
      inputId = "slider1_vals",
      label = input$num1,
      min = min(round(superstore[[input$num1]], 1)),
      max = max(round(superstore[[input$num1]], 1)),
      value = range(superstore[[input$num1]]),
      step = 0.1
    )
  })
  
  output$slider2 <- renderUI({
    if (input$num2 == "None") {
      return(NULL)
    }
    
    sliderInput(
      inputId = "slider2_vals",
      label = input$num2,
      min = min(round(superstore[[input$num2]], 1)),
      max = max(round(superstore[[input$num2]], 1)),
      value = range(superstore[[input$num2]]),
      step = 0.1
    )
  })
  
  filtered_data <- reactive({
    # Dependent on Pressing the Button
    input$subset
    
    # Use isolate to avoid dependence on the other widgets 
    isolate(superstore |>
              filter(if (input$segment != "All") superstore$Segment == input$segment
                     else TRUE,
                     if (input$category != "All") superstore$Category == input$category
                     else TRUE,
                     if (input$region != "All") superstore$Region == input$region
                     else TRUE,
                     if (input$ship != "All") superstore$Ship_Mode == input$ship
                     else TRUE,
                     if (input$num1 != "None") 
                       (superstore[[input$num1]] >= input$slider1_vals[1] & 
                          superstore[[input$num1]] <= input$slider1_vals[2]) else TRUE,
                     if (input$num2 != "None") 
                       (superstore[[input$num2]] >= input$slider2_vals[1] & 
                          superstore[[input$num2]] <= input$slider2_vals[2]) else TRUE
              ))
    })
  
    
  output$table <- DT::renderDataTable({
    # Output the filtered dataset
    filtered_data()
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Superstore-Data-", Sys.Date(), ".csv", sep='')
    },
    content = function(con) {
      write.csv(filtered_data(), con)
    }
  )
  
  # Categorical Variables
  
  observeEvent(input$one_cat_var, {
    cat1 <- input$one_cat_var
    cat2 <- input$two_cat_var
    choices <- c("Ship_Mode", "Segment","Region","Category")
    if (cat1 != cat2) {
      choices <- choices[-which(choices == cat1)]
    }
    updateSelectizeInput(session,
                         "two_cat_var",
                         choices = choices,
                         selected = cat2)
  })
  
  observeEvent(input$two_cat_var, {
    cat1 <- input$one_cat_var
    cat2 <- input$two_cat_var
    choices <- c("Ship_Mode", "Segment","Region","Category")
    if (cat2 != cat1) {
      choices <- choices[-which(choices == cat2)]
    }
    updateSelectizeInput(session,
                         "one_cat_var",
                         choices = choices,
                         selected = cat1)
  })
  
  
  output$one_way_title <- renderText({
    paste(input$one_cat_var, "One-Way Contingency Table")
  })
  
  output$two_way_title <- renderText({
    paste(input$one_cat_var, 
          "and", 
          input$two_cat_var,
          "Two-Way Contingency Table")
  })
  
  output$one_way_cont <- renderTable({
    filtered_data() |>
      group_by(!!sym(input$one_cat_var)) |>
      summarize(count = n())
  })
  
  output$two_way_cont <- renderTable({
    filtered_data() |>
      group_by(!!sym(input$one_cat_var), !!sym(input$two_cat_var)) |>
      summarize(count = n(), .groups = "drop") |>
      pivot_wider(names_from = !!sym(input$one_cat_var), values_from = count)
  })
  
  output$bar_title <- renderText({
    paste("Bar Plot of", input$one_cat_var)
  })
  
  output$bar_plot <- renderPlot({
    ggplot(filtered_data(), aes(!!sym(input$one_cat_var))) +
      geom_bar()
  })
  
  output$box_title <- renderText({
    paste("Box Plot of", input$three_num_var,
          "by", input$one_cat_var,
          "faceted by", input$two_cat_var)
  })
  
  # Not a Categorical Plot, but its using two categorical variables for the
  # y and facet. Also it fits more nicely on this tab. 
  output$box_plot <- renderPlot({
    ggplot(filtered_data(), 
           aes(!!sym(input$one_cat_var), 
               !!sym(input$three_num_var), 
               color = !!sym(input$one_cat_var))) +
      geom_boxplot() +
      facet_wrap(as.formula(paste("~", input$two_cat_var)))
  })
  
  output$heatmap_title <- renderText({
    paste("Proportion Heatmap of", input$one_cat_var, 
          "per", input$two_cat_var)
  })
  
  output$heatmap <- renderPlot({
    store_heat <- filtered_data() |>
      group_by(!!sym(input$one_cat_var), !!sym(input$two_cat_var)) |>
      summarize(Count = n(), .groups = "drop") |>
      mutate(Proportion = Count / sum(Count))
    
    # Create a heatmap using the proportion of each group
    ggplot(store_heat, aes(!!sym(input$one_cat_var), 
                           !!sym(input$two_cat_var), 
                           fill = Proportion)) +
      geom_tile() 
  })
  
  
  # Numerical Variables
  
  observeEvent(input$num_sum_stat, {
    num1 <- input$num_sum_stat
    num2 <- input$num_sum_stat_2
    choices <- c("Sales", "Quantity","Discount","Profit")
    if (num1 != num2) {
      choices <- choices[-which(choices == num1)]
    }
    updateSelectizeInput(session,
                         "num_sum_stat_2",
                         choices = choices,
                         selected = num2)
  })
  
  observeEvent(input$num_sum_stat_2, {
    num1 <- input$num_sum_stat
    num2 <- input$num_sum_stat_2
    choices <- c("Sales", "Quantity","Discount","Profit")
    if (num2 != num1) {
      choices <- choices[-which(choices == num2)]
    }
    updateSelectizeInput(session,
                         "num_sum_stat",
                         choices = choices,
                         selected = num1)
  })
  
  output$num_sum_stats_title <- renderText({
    paste(input$num_sum_stat, "Summary Statistics")
  })
  
  output$cat_level_title <- renderText({
    paste(input$num_sum_stat, "Summary Statistics across levels of", 
          input$cat_level)
  })
  
  output$basic_sum_stats <- renderTable({
    
    sales_summary <- round(summary(filtered_data()[[input$num_sum_stat]]), 3)
    sales_summary <- c(sales_summary, 
                       "Std." = round(sd(filtered_data()[[input$num_sum_stat]]), 3))
    as.data.frame(t(sales_summary))
  })
  
  output$levels_sum_stats <- renderTable({
    filtered_data() |>
      group_by(!!sym(input$cat_level)) |>
      summarize(!!paste0("Mean_", 
                         input$num_sum_stat) := mean(get(input$num_sum_stat)), 
                !!paste0("Med_", 
                         input$num_sum_stat) := median(get(input$num_sum_stat)),
                !!paste0("SD_", 
                         input$num_sum_stat) := sd(get(input$num_sum_stat)),
                !!paste0("IQR_", 
                         input$num_sum_stat) := IQR(get(input$num_sum_stat)))
  })
  
  output$density_title <- renderText({
    paste("Density Plot of", input$num_sum_stat)
  })
  
  output$density_plot <- renderPlot({
    ggplot(filtered_data(), aes(!!sym(input$num_sum_stat))) +
      geom_density(alpha = 0.5)
  })
  
  output$layered_density_title <- renderText({
    paste("Density Plot of", input$num_sum_stat, 
          "per", input$cat_level)
  })
  
  output$layered_density_plot <- renderPlot({
    ggplot(filtered_data(), aes(!!sym(input$num_sum_stat))) + 
      geom_density(alpha = 0.5, aes(fill = !!sym(input$cat_level)))
  })
  
  output$scatter_title <- renderText({
    paste(input$num_sum_stat, "vs",
          input$num_sum_stat_2, "Scatter Plot")
  })
  
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes(!!sym(input$num_sum_stat), 
                                !!sym(input$num_sum_stat_2),
                                color = !!sym(input$cat_level))) +
      geom_point()
  })
  
}

# Call the Function
  shinyApp(ui = ui, server = server)