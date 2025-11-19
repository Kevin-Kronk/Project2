library(shiny)
library(bslib)
library(readxl)
library(tidyverse)
library(DT)

# Define the UI
ui <- page_sidebar(
  title = "Project 2",
  
  sidebar = sidebar(
    h4("Subset the Data"),
    # Allows user to select the Segment variable
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
    
    # Allows user to select the Category variable
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
    
    # Allows user to select the Region variable
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
    
    # Allows user to select the Ship_Mode variable
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
    
    # Allows user to select a numeric variable to filter with
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
    
    # The dynamic slider created for the numeric variable chosen above
    uiOutput("slider1"), 
    
    # The second numeric variable to filter with
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
    
    # The dynamic slider for the second numeric variable
    uiOutput("slider2"),
    
    # Button to perform the subset of the dataset
    actionButton(
      inputId =  "subset",
      label = "Get The Data!"
    )
  ),
  
  # To set it so that the navset_card_underline can fit everything and scroll
  #tags$style(HTML(".navset-card-underline .card {
  #                max-height: 1200px;
  #                overflow-y: auto;}")),
  # Creates the three tabs
  navset_card_underline(
    nav_panel("About",
              h3("Purpose of the App"),
              "This app allows users to explore the US Superstore dataset. The 
              About tab explains the app and the dataset. Users can filter the 
              data by categorical or numerical variables on the sidebar as well 
              as view and download the filtered data in the Data Download tab. 
              In the Data Exploration tab users can choose which variables to 
              view their contingency tables or summary statistics along with various
              different plots.",
              tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/d/d8/Superstore_%28Universal_Television_sitcom%29.svg",
                       width = "300px",
                       height = "300px"),
              h3("The Data"),
              "The US Superstore data is from 2014-2018 and can be found on Kaggle.
              It seems to be based on a fictional store in the show Superstore and
              gives a sense for the kinds of purchasing trends from consumers
              in the United States. For more information visit:",
              a("US Superstore Data Kaggle", 
                href = "https://www.kaggle.com/datasets/juhi1994/superstore/data")
              ),
    
    nav_panel("Data Download",
              downloadButton(outputId = "downloadData"),
              DT::dataTableOutput(outputId = "table")
              ),
    
    nav_panel("Data Exploration",
              headerPanel("Data Selection"),
              # Select which type of data to look at summaries and plots of
              radioButtons(inputId = "data_type", 
                           label = "Type of Data to Explore",
                           choices = list("Categorical", "Numerical"),
                           inline = TRUE),
              
              # Only shows panel when Categorical is chosen
              conditionalPanel(
                condition = "input.data_type == `Categorical`",
                # Select Inputs on one row
                fluidRow(column(width=4, 
                                # Select the first categorical variable
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
                                  )
                                ),
                         
                         # Select the second categorical variable
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
                                  )
                                ),
                         
                         # Select the numerical variable for plotting
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
                                  )
                                )
                         ),
                
                # All of the categorical titles, table outputs, and plot outputs
                h3(textOutput("one_way_title")),
                tableOutput("one_way_cont"),
                h3(textOutput("two_way_title")),
                tableOutput("two_way_cont"),
                h3(textOutput("bar_title")),
                plotOutput("bar_plot", height = "33%"),
                h3(textOutput("box_title")),
                plotOutput("box_plot", height = "33%"),
                h3(textOutput("heatmap_title")),
                plotOutput("heatmap", height = "33%")
                ),
              
              # Only shows panel when Numerical is selected
              conditionalPanel(
                condition = "input.data_type == `Numerical`",
                # Row for all of the Inputs
                fluidRow(column(width=4, 
                                # Select the first numerical variable
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
                                  )
                                ),
                         
                         # Select the categorical variable to view across the numerical
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
                                  )
                                ),
                         
                         # Select the numerical variable for plotting
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
                                  )
                                )
                         ),
                
                # All of the numerical titles, table outputs, and plot outputs
                h3(textOutput("num_sum_stats_title")),
                tableOutput("basic_sum_stats"),
                h3(textOutput("cat_level_title")),
                tableOutput("levels_sum_stats"),
                h3(textOutput("density_title")),
                plotOutput("density_plot", height = "33%"),
                h3(textOutput("layered_density_title")),
                plotOutput("layered_density_plot", height = "33%"),
                h3(textOutput("scatter_title")),
                plotOutput("scatter_plot", height = "33%")
                )
              )
    )
  )

# Define the Server
server <- function(input, output, session) {
  # Load the full dataset once from an excel file with column names and types
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
  
  # Sidebar: Only allow the user to select each numerical variable once
  # or allow both to be None
  # dynamically updates options as new selections are made
  
  # Updates the num2 selections available
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
  
  # Updates the num1 selections available
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
  
  # Sidebar: Creates the slider when a numerical variable is selected
  output$slider1 <- renderUI({
    # No slider is None is selected
    if (input$num1 == "None") {
      return(NULL)
    }
    
    # Creates the slider values based on the numerical variable selected
    sliderInput(
      inputId = "slider1_vals",
      label = input$num1,
      min = min(round(superstore[[input$num1]], 1)),
      max = max(round(superstore[[input$num1]], 1)),
      value = range(superstore[[input$num1]]),
      step = 0.1
      )
    })
  
  # Sidebar: Creates the slider when the second numerical variable is selected
  output$slider2 <- renderUI({
    # No slider is None is selected
    if (input$num2 == "None") {
      return(NULL)
    }
    
    # Creates the slider values based on the numerical variable selected
    sliderInput(
      inputId = "slider2_vals",
      label = input$num2,
      min = min(round(superstore[[input$num2]], 1)),
      max = max(round(superstore[[input$num2]], 1)),
      value = range(superstore[[input$num2]]),
      step = 0.1
      )
    })
  
  # Filters the dataset 
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
                     )
            )
    })
  
  # Renders the dataset to the Data Download tab
  output$table <- DT::renderDataTable({
    # Output the filtered dataset
    filtered_data()
    })
  
  # Downloads the data if the download button is selected
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Superstore-Data-", Sys.Date(), ".csv", sep='')
    },
    content = function(con) {
      write.csv(filtered_data(), con)
      }
    )
  
  # Categorical Variables
  
  # Ensures that the two categorical variables are not the same
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
  
  # Dynamically creates the title for the One-Way Contingency Table
  output$one_way_title <- renderText({
    paste(input$one_cat_var, "One-Way Contingency Table")
    })
  
  # Generates the One-Way Contingency Table based on the variable chosen
  output$one_way_cont <- renderTable({
    filtered_data() |>
      group_by(!!sym(input$one_cat_var)) |>
      summarize(count = n())
    })
  
  # Dynamically creates the title for the Two-Way Contingency Table
  output$two_way_title <- renderText({
    paste(input$one_cat_var, 
          "and", 
          input$two_cat_var,
          "Two-Way Contingency Table")
  })
  
  # Generates the Two-Way Contingency Table based on the variables chosen
  output$two_way_cont <- renderTable({
    filtered_data() |>
      group_by(!!sym(input$one_cat_var), !!sym(input$two_cat_var)) |>
      summarize(count = n(), .groups = "drop") |>
      pivot_wider(names_from = !!sym(input$one_cat_var), values_from = count)
    })
  
  # Dynamically creates the title for the Bar Plot
  output$bar_title <- renderText({
    paste("Bar Plot of", input$one_cat_var)
    })
  
  # Creates the Bar Plot based on the variable chosen
  output$bar_plot <- renderPlot({
    ggplot(filtered_data(), aes(!!sym(input$one_cat_var))) +
      geom_bar()
    })
  
  # Dynamically creates the title for the Box Plot
  output$box_title <- renderText({
    paste("Box Plot of", input$three_num_var,
          "by", input$one_cat_var,
          "faceted by", input$two_cat_var)
    })
  
  # A Box Plot is not a Categorical Plot, but its using two categorical variables 
  # for the y and facet. Also it fits more nicely on this tab. 
  output$box_plot <- renderPlot({
    ggplot(filtered_data(), 
           aes(!!sym(input$one_cat_var), 
               !!sym(input$three_num_var), 
               color = !!sym(input$one_cat_var))) +
      geom_boxplot() +
      facet_wrap(as.formula(paste("~", input$two_cat_var)))
    })
  
  # Dynamically creates the title for the Heatmap Plot
  output$heatmap_title <- renderText({
    paste("Proportion Heatmap of", input$one_cat_var, 
          "per", input$two_cat_var)
    })
  
  # Creates the heatmap plot based on the two categorical variables chosen
  output$heatmap <- renderPlot({
    # Calculate the proportion of each categorical variable
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
  
  # Ensures the two numerical variables are not the same
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
  
  # Dynamically creates the title for the summary statistics
  output$num_sum_stats_title <- renderText({
    paste(input$num_sum_stat, "Summary Statistics")
    })
  
  # Creates the summary statistics table
  output$basic_sum_stats <- renderTable({
    # Gets the summary data and adds the standard deviation
    sales_summary <- round(summary(filtered_data()[[input$num_sum_stat]]), 3)
    sales_summary <- c(sales_summary, 
                       "Std Dev." = round(sd(filtered_data()[[input$num_sum_stat]]), 3))
    as.data.frame(t(sales_summary))
    })
  
  # Dynamically creates the title for the summary statistic across levels
  output$cat_level_title <- renderText({
    paste(input$num_sum_stat, "Summary Statistics across levels of", 
          input$cat_level)
  })
  
  # Creates the summary statistics table across levels of a categorical variable
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
  
  # Dynamically creates the title for the density plot
  output$density_title <- renderText({
    paste("Density Plot of", input$num_sum_stat)
    })
  
  # Creates the density plot based on the numerical variable chosen
  output$density_plot <- renderPlot({
    ggplot(filtered_data(), aes(!!sym(input$num_sum_stat))) +
      geom_density(alpha = 0.5)
    })
  
  # Dynamically creates the title for the density plot across a categorical variable
  output$layered_density_title <- renderText({
    paste("Density Plot of", input$num_sum_stat, 
          "per", input$cat_level)
    })
  
  # Creates the density plot across a categorical variable chosen
  output$layered_density_plot <- renderPlot({
    ggplot(filtered_data(), aes(!!sym(input$num_sum_stat))) + 
      geom_density(alpha = 0.5, aes(fill = !!sym(input$cat_level)))
    })
  
  # Dynamically creates the scatter plot title
  output$scatter_title <- renderText({
    paste(input$num_sum_stat, "vs",
          input$num_sum_stat_2, "Scatter Plot")
    })
  
  # Creates the scatter plot based on the two numerical variables chosen
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes(!!sym(input$num_sum_stat), 
                                !!sym(input$num_sum_stat_2),
                                color = !!sym(input$cat_level))) +
      geom_point()
    })
  }

# Call the Function
  shinyApp(ui = ui, server = server)