library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)
library(DT)
library(data.table)
library(tidyr)  # Ensure tidyr is loaded

# Load the data
load("title_pdata.rda")

# Convert
pdatatitle <- titlepdata

# Convert year column to numeric if it's a factor or character
pdatatitle$Year <- as.numeric(as.character(pdatatitle$Year))

# Function to replace periods with spaces in column names
replace_periods <- function(name) {
  gsub("\\.", " ", name)
}

# Define the UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Custom CSS for improved styling
  tags$head(
    tags$style(HTML("
      .custom-button {
        font-size: 14px;
        font-weight: bold;
        border-radius: 5px;
        padding: 5px 10px;
        margin-top: 5px;
        margin-bottom: 5px;
        border: none;
        color: #fff;
        cursor: pointer;
      }
      .select-all-btn {
        background-color: #007bff;
      }
      .select-all-btn:hover {
        background-color: #0056b3;
      }
      .deselect-all-btn {
        background-color: #dc3545;
      }
      .deselect-all-btn:hover {
        background-color: #c82333;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px; position: fixed; height: 100%; overflow-y: auto; overflow-x: hidden; width: 300px;",  # Adjust the width as necessary
      
      # Slider input for year range
      sliderInput("year_range", "Select Year Range:",
                  min = min(pdatatitle$Year, na.rm = TRUE),
                  max = max(pdatatitle$Year, na.rm = TRUE),
                  value = c(min(pdatatitle$Year, na.rm = TRUE), max(pdatatitle$Year, na.rm = TRUE)),
                  step = 1,
                  sep = ""
      ),
      
      # Checkbox group for trend variables
      div(
        style = "max-height: 300px; overflow-y: auto;",  # Adjust this if needed
        checkboxGroupInput("trend_variables", "Select Trend Variables:",
                           choices = setdiff(names(pdatatitle), c("Year", "Country")) %>% 
                             sapply(replace_periods),
                           selected = replace_periods(names(pdatatitle)[which(names(pdatatitle) != "Year" & names(pdatatitle) != "Country")[1]]),
                           inline = TRUE
        )
      ),
      
      # Checkbox group for countries without scrolling
      checkboxGroupInput("selected_countries", "Select Countries:",
                         choices = setNames(unique(pdatatitle$Country), unique(pdatatitle$Country)),
                         selected = unique(pdatatitle$Country),
                         inline = TRUE
      ),
      
      # Add "Select All" and "Deselect All" buttons with improved styling
      actionButton("select_all", "Select All", class = "custom-button select-all-btn"),
      actionButton("deselect_all", "Deselect All", class = "custom-button deselect-all-btn"),
      
      # Custom CSS for country checkboxes
      tags$head(
        tags$style(HTML("
      .checkbox-group input[type='checkbox'] {
        display: none;
      }
      .checkbox-group label {
        position: relative;
        padding-left: 30px;
        cursor: pointer;
      }
      .checkbox-group label::before {
        content: '\\2714';  /* Checkmark */
        position: absolute;
        left: 0;
        top: 50%;
        transform: translateY(-50%);
        color: #28a745;  /* Green checkmark */
        display: none;
      }
      .checkbox-group input[type='checkbox']:checked + label::before {
        content: '\\2718';  /* X mark */
        color: #dc3545;  /* Red X mark */
      }
    "))
      )
    ),
    mainPanel(
      width = 9,  # Make the main panel take up more space
      fluidRow(
        column(12,
               div(style = "text-align: center",  # Reduced padding to optimize space
                   HTML("<h4 style='font-weight: bold;'>Aggregate Trends over Time</h4>")
               ),
               div(style = "padding-bottom: 20px;",  # Reduced padding to optimize space
                   plotlyOutput("cumulative_trend_plot", height = "400px")  # Fixed height for consistency
               )
        )
      ),
      fluidRow(
        column(12,
               div(style = "text-align: center",  # Reduced padding to optimize space
                   HTML("<h4 style='font-weight: bold;'>Country-Level Trends over Time</h4>")
               ),
               div(style = "padding-bottom: 20px;",  # Reduced padding to optimize space
                   plotlyOutput("interactive_trend_plot", height = "400px")  # Fixed height for consistency
               )
        )
      ),
      fluidRow(
        column(12,
               div(style = "text-align: center",  # Reduced padding to optimize space
                   HTML("<h4 style='font-weight: bold;'>Movement in Trend Variables</h4>
                        <h5 style='color: #555;'>The table displays the percent change of the selected trend variables over the selected year range, with countries ordered from largest absolute percent change to smallest</h5>")
               )
        )
      ),
      fluidRow(
        column(12,
               div(style = "position: relative",
                   dataTableOutput("percentage_change_table")  # Table showing percentage changes
               )
        )
      ),
      fluidRow(
        column(12, textOutput("percentage_change_message"))  # Small text message
      ),
      
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to filter data based on inputs
  filtered_data <- reactive({
    req(input$trend_variables, input$selected_countries)  # Ensure inputs are provided
    
    pdatatitle %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>%
      filter(Country %in% input$selected_countries)
  })
  
  # Update the selected countries when "Select All" button is pressed
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "selected_countries", selected = unique(pdatatitle$Country))
  })
  
  # Update the selected countries when "Deselect All" button is pressed
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "selected_countries", selected = character(0))
  })
  
  # Render the interactive cumulative trend plot
  output$cumulative_trend_plot <- renderPlotly({
    req(input$trend_variables, input$selected_countries)  # Ensure inputs are provided
    
    data <- filtered_data()
    variables <- gsub(" ", "\\.", input$trend_variables)
    
    cumulative_data_list <- lapply(variables, function(variable) {
      data %>%
        group_by(Year) %>%
        summarise(CumulativeAmount = sum(!!sym(variable), na.rm = TRUE), .groups = 'drop') %>%
        mutate(Variable = variable)
    })
    
    cumulative_data <- bind_rows(cumulative_data_list)
    
    plot_ly(cumulative_data, x = ~Year, y = ~CumulativeAmount, color = ~Variable, type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = paste("Aggregate Trends from", input$year_range[1], "to", input$year_range[2]),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Cumulative Amount")
      )
  })
  
  # Render the interactive plot by country
  output$interactive_trend_plot <- renderPlotly({
    req(input$trend_variables, input$selected_countries)  # Ensure inputs are provided
    
    data <- filtered_data()
    variables <- gsub(" ", "\\.", input$trend_variables)
    
    p <- ggplot(data) +
      geom_line(aes(x = Year, y = !!sym(variables[1]), color = Country)) +
      labs(
        title = paste("Domestic Trends from", input$year_range[1], "to", input$year_range[2]),
        x = "Year",
        y = paste(input$trend_variables, collapse = " / ")
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
      )
    
    if (length(variables) > 1) {
      for (var in variables[-1]) {
        p <- p + geom_line(aes(x = Year, y = !!sym(var), color = Country), linetype = "dashed")
      }
    }
    
    ggplotly(p)
  })
  
  # Render the percentage change table
  output$percentage_change_table <- renderDataTable({
    req(input$trend_variables, input$selected_countries)  # Ensure inputs are provided
    
    data <- filtered_data()
    variables <- gsub(" ", "\\.", input$trend_variables)
    
    if (nrow(data) == 0 || length(variables) == 0) {
      return(
        datatable(
          data.frame(Message = "Please select at least 1 input variable and 1 input country"),
          options = list(
            searching = FALSE,
            paging = FALSE,
            info = FALSE
          )
        )
      )
    }
    
    # Define a small number for replacement
    small_number <- 1e-12
    
    # Calculate percentage change for all variables
    change_data_list <- lapply(variables, function(variable) {
      data %>%
        group_by(Country) %>%
        arrange(Country, Year) %>%
        summarise(
          FirstValue = ifelse(first(!!sym(variable)) == 0, small_number, first(!!sym(variable))),
          LastValue = ifelse(last(!!sym(variable)) == 0, small_number, last(!!sym(variable))),
          PercentageChange = ifelse(
            FirstValue != 0,
            (LastValue - FirstValue) / FirstValue * 100,
            NA
          ),
          FormattedChange = paste0(ifelse(PercentageChange > 0, "+", "-"), round(abs(PercentageChange), 2)),
          Variable = variable
        ) %>%
        select(Country, FormattedChange, Variable)
    })
    
    # Bind all variables' change data and pivot to wider format
    change_data <- bind_rows(change_data_list) %>%
      pivot_wider(names_from = Variable, values_from = FormattedChange)
    
    # Replace periods with spaces in the column names
    names(change_data) <- sapply(names(change_data), replace_periods)
    
    # Update the column names to include the trend variable names
    for (var in input$trend_variables) {
      var_name <- gsub("\\.", " ", var)
      names(change_data)[names(change_data) == var_name] <- paste0("% Change in ", var_name)
    }
    
    # Order by the percentage change of the first variable if applicable
    if (length(variables) > 0) {
      first_variable <- gsub("\\.", " ", variables[1])
      if (first_variable %in% names(change_data)) {
        change_data <- change_data %>%
          arrange(desc(as.numeric(change_data[[first_variable]])))
      }
    }
    
    datatable(
      change_data,
      options = list(
        searching = TRUE,
        paging = FALSE,  # Removed pagination
        info = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(className = 'dt-body', targets = "_all")
        )
      )
    ) %>%
      formatStyle(
        columns = names(change_data),
        backgroundColor = styleInterval(0, c('#f2dede', '#dff0d8')),
        color = styleInterval(0, c('#000000', '#000000'))
      )
  })
  
  # Render the small text message below the table
  output$percentage_change_message <- renderText({
    "*Any zero values encountered in the percentage change computation were replaced by a very small number (1e-12) to avoid division errors."
  })
}

# Run the app
shinyApp(ui = ui, server = server)
