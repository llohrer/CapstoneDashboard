library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

ui <- fluidPage(
  titlePanel("Variable Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      p("Select a variable from the dropdown to learn more about it"),
      selectInput("variable", "Choose a variable:", 
                  choices = c("Wine Production per Capita" = "Wine.Production.per.Capita",
                              "Wine Imports per Capita" = "Wine.Imports.per.Capita",
                              "Wine Exports per Capita" = "Wine.Exports.per.Capita",
                              "Real GDP per Capita" = "Real.GDP.per.Capita",
                              "Exchange Rate" = "Exchange.Rate",
                              "Wine Consumption per Adult" = "Wine.Consumption.per.Adult"),
                  selectize = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 fluidRow(
                   column(6,
                          h4("Variable Description"),
                          uiOutput("description")
                   ),
                   column(6,
                          h4("Descriptive Statistics"),
                          uiOutput("descriptiveStats")
                   )
                 ),
                 hr(),
                 tabsetPanel(
                   id = "plotTabs",
                   tabPanel("Scatter Plot", plotlyOutput("scatterPlot")),
                   tabPanel("Time Plot",
                            uiOutput("timePlot"),
                            plotlyOutput("filteredTimePlot")
                   )
                 )
        )
      )
    )
  ),
  
  tags$head(
    tags$style(HTML("
      .sidebar {
        background-color: #f5f5f5;
        padding: 20px;
        border-radius: 10px;
        border: 1px solid #ddd;
      }
      .main-panel {
        padding: 20px;
      }
      h4 {
        font-weight: bold;
        color: #333;
      }
      .tabbable > .nav > li > a {
        color: #555;
        font-weight: bold;
      }
      .tabbable > .nav > li.active > a {
        background-color: #007bff;
        color: white;
      }
      .tabbable > .tab-content {
        padding: 20px;
      }
      .panel-title {
        font-size: 1.5em;
        margin-bottom: 20px;
      }
      .plotly-graph {
        border: 1px solid #ddd;
        border-radius: 10px;
      }
    "))
  )
)



server <- function(input, output, session) {
  # Load the dataset
  load("title_pdata.rda")
  
  # Convert
  vardata <- titlepdata
  
  vardata <- vardata %>%
    rename(
      Wine.Production.per.Capita = `Wine.Production..Liters.per.Capita.`,
      Wine.Imports.per.Capita = `Wine.Imports..Liters.per.Capita.`,
      Wine.Exports.per.Capita = `Wine.Exports..Liters.per.Capita.`,
      Real.GDP.per.Capita = `Real.GDP.per.Capita..1990.Int.l...`,
      Exchange.Rate = `Exchange.Rate`,
      Wine.Consumption.per.Adult = `Wine.Consumption..Liters.per.Adult.`
    ) %>%
    mutate(
      Wine.Production.per.Capita = as.numeric(Wine.Production.per.Capita),
      Wine.Imports.per.Capita = as.numeric(Wine.Imports.per.Capita),
      Wine.Exports.per.Capita = as.numeric(Wine.Exports.per.Capita),
      Real.GDP.per.Capita = as.numeric(Real.GDP.per.Capita),
      Exchange.Rate = as.numeric(Exchange.Rate),
      Wine.Consumption.per.Adult = as.numeric(Wine.Consumption.per.Adult)
    ) %>%
    drop_na()  # Remove rows with missing values
  
  # Function to generate descriptive statistics as a formatted paragraph
  get_descriptive_stats <- function(data, var) {
    stats <- data %>%
      summarize(
        Mean = mean(.data[[var]], na.rm = TRUE),
        Median = median(.data[[var]], na.rm = TRUE),
        SD = sd(.data[[var]], na.rm = TRUE),
        Min = min(.data[[var]], na.rm = TRUE),
        Max = max(.data[[var]], na.rm = TRUE),
        Count = n()
      )
    
    stats_text <- paste(
      "<p><b>Mean:</b> ", round(stats$Mean, 2), "<br>",
      "<b>Median:</b> ", round(stats$Median, 2), "<br>",
      "<b>Standard Deviation:</b> ", round(stats$SD, 2), "<br>",
      "<b>Minimum:</b> ", round(stats$Min, 2), "<br>",
      "<b>Maximum:</b> ", round(stats$Max, 2), "<br>",
      "<b>Count:</b> ", stats$Count, "</p>"
    )
    
    return(stats_text)
  }
  
  output$descriptiveStats <- renderUI({
    var <- input$variable
    
    stats_text <- get_descriptive_stats(vardata, var)
    
    HTML(stats_text)
  })
  
  output$scatterPlot <- renderPlotly({
    var <- input$variable
    plot_data <- vardata %>%
      select(all_of(var), Wine.Consumption.per.Adult)
    
    p <- ggplot(plot_data, aes(x = .data[[var]], y = Wine.Consumption.per.Adult)) +
      geom_point(aes(color = Wine.Consumption.per.Adult), size = 2, alpha = 0.6) +  # Reduced size of the points
      labs(title = paste(gsub("\\.", " ", var), "vs. Wine Consumption"),
           x = gsub("\\.", " ", var),
           y = "Wine Consumption per Adult") +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold")
      )
    
    ggplotly(p)
  })
  
  
  
  
  output$timePlot <- renderUI({
    var <- input$variable
    
    vardata <- vardata %>%
      mutate(Year = as.numeric(as.character(Year)))  # Convert 'Year' to numeric if it's not already
    
    by_country_data <- vardata %>%
      group_by(Year, Country) %>%
      summarize(Value = sum(.data[[var]], na.rm = TRUE), .groups = 'drop') %>%
      ungroup()
    
    x_min <- 1961
    x_max <- 2018
    
    all_years <- seq(from = x_min, to = x_max, by = 1)
    
    year_breaks <- all_years[all_years %% 10 %in% c(0, 5)]
    
    p <- ggplot(by_country_data, aes(x = Year, y = Value, fill = Country)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = paste("Time Series of", gsub("\\.", " ", var), "by Country"),
           x = "Year",
           y = gsub("\\.", " ", var)) +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5)  # Align x-axis labels horizontally
      ) +
      scale_x_continuous(
        limits = c(x_min, x_max),  # Set fixed range
        breaks = year_breaks,       # Set specific breaks
        labels = as.character(year_breaks)  # Directly use year values as labels
      )
    
    # Convert ggplot to interactive plotly plot
    p_plotly <- ggplotly(p, tooltip = "text")  # Add tooltip if needed
    
    # Enable double-click filtering functionality
    p_plotly <- p_plotly %>% layout(
      legend = list(
        itemclick = "toggleothers"  # Toggle visibility of other traces on legend item click
      )
    )
    
    # Render the interactive plot
    p_plotly
  })
  
  output$description <- renderUI({
    var <- input$variable
    
    descriptions <- list(
      `Wine.Production.per.Capita` = "Wine Production per Capita: This variable represents the amount of wine produced per person. Historical data show significant changes over time, with production per capita varying considerably between countries and periods. Generally, higher wine production might indicate a greater availability of wine, potentially influencing wine consumption.",
      `Wine.Imports.per.Capita` = "Wine Imports per Capita: This measures the amount of wine imported per person. The data reveal changes in import patterns, especially after the 1970s, with significant increases in countries like the United States and China. Higher wine imports could be indicative of a higher demand for foreign wines, potentially affecting local wine consumption patterns.",
      `Wine.Exports.per.Capita` = "Wine Exports per Capita: This variable tracks the amount of wine exported per person. For example, Spain and Chile now have high export levels per capita, indicating a strong export market compared to historical levels. While exports might not directly influence domestic consumption, they can reflect the global competitiveness of local wine production.",
      `Real.GDP.per.Capita` = "Real GDP per Capita: This represents the economic output per person, adjusted for inflation. It helps in understanding the economic context in which wine production and consumption occur. Higher GDP per capita often correlates with greater disposable income, which might influence wine consumption patterns.",
      `Exchange.Rate` = "Exchange Rate: The rate at which one currency can be exchanged for another. Exchange rates influence import costs and thus consumption levels. A fluctuating exchange rate can affect the cost of imported wines, potentially influencing domestic wine consumption.",
      `Wine.Consumption.per.Adult` = "Wine Consumption per Adult: This variable measures the average amount of wine consumed per adult. It provides insights into drinking habits and preferences. Changes in this metric can reflect shifts in cultural attitudes towards wine, economic conditions, or the impact of public health policies on alcohol consumption."
    )
    
    HTML(descriptions[[input$variable]])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

ui <- fluidPage(
  titlePanel("Variable Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      p("Select a variable from the dropdown to learn more about it"),
      selectInput("variable", "Choose a variable:", 
                  choices = c("Wine Production per Capita" = "Wine.Production.per.Capita",
                              "Wine Imports per Capita" = "Wine.Imports.per.Capita",
                              "Wine Exports per Capita" = "Wine.Exports.per.Capita",
                              "Real GDP per Capita" = "Real.GDP.per.Capita",
                              "Exchange Rate" = "Exchange.Rate",
                              "Wine Consumption per Adult" = "Wine.Consumption.per.Adult"),
                  selectize = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 fluidRow(
                   column(6,
                          h4("Variable Description"),
                          uiOutput("description")
                   ),
                   column(6,
                          h4("Descriptive Statistics"),
                          uiOutput("descriptiveStats")
                   )
                 ),
                 hr(),
                 tabsetPanel(
                   id = "plotTabs",
                   tabPanel("Scatter Plot", plotlyOutput("scatterPlot")),
                   tabPanel("Time Plot",
                            uiOutput("timePlot"),
                            plotlyOutput("filteredTimePlot")
                   )
                 )
        )
      )
    )
  ),
  
  tags$head(
    tags$style(HTML("
      .sidebar {
        background-color: #f5f5f5;
        padding: 20px;
        border-radius: 10px;
        border: 1px solid #ddd;
      }
      .main-panel {
        padding: 20px;
      }
      h4 {
        font-weight: bold;
        color: #333;
      }
      .tabbable > .nav > li > a {
        color: #555;
        font-weight: bold;
      }
      .tabbable > .nav > li.active > a {
        background-color: #007bff;
        color: white;
      }
      .tabbable > .tab-content {
        padding: 20px;
      }
      .panel-title {
        font-size: 1.5em;
        margin-bottom: 20px;
      }
      .plotly-graph {
        border: 1px solid #ddd;
        border-radius: 10px;
      }
    "))
  )
)



server <- function(input, output, session) {
  # Load the dataset
  load("title_pdata.rda")
  
  # Convert
  vardata <- titlepdata
  
  vardata <- vardata %>%
    rename(
      Wine.Production.per.Capita = `Wine.Production..Liters.per.Capita.`,
      Wine.Imports.per.Capita = `Wine.Imports..Liters.per.Capita.`,
      Wine.Exports.per.Capita = `Wine.Exports..Liters.per.Capita.`,
      Real.GDP.per.Capita = `Real.GDP.per.Capita..1990.Int.l...`,
      Exchange.Rate = `Exchange.Rate`,
      Wine.Consumption.per.Adult = `Wine.Consumption..Liters.per.Adult.`
    ) %>%
    mutate(
      Wine.Production.per.Capita = as.numeric(Wine.Production.per.Capita),
      Wine.Imports.per.Capita = as.numeric(Wine.Imports.per.Capita),
      Wine.Exports.per.Capita = as.numeric(Wine.Exports.per.Capita),
      Real.GDP.per.Capita = as.numeric(Real.GDP.per.Capita),
      Exchange.Rate = as.numeric(Exchange.Rate),
      Wine.Consumption.per.Adult = as.numeric(Wine.Consumption.per.Adult)
    ) %>%
    drop_na()  # Remove rows with missing values
  
  # Function to generate descriptive statistics as a formatted paragraph
  get_descriptive_stats <- function(data, var) {
    stats <- data %>%
      summarize(
        Mean = mean(.data[[var]], na.rm = TRUE),
        Median = median(.data[[var]], na.rm = TRUE),
        SD = sd(.data[[var]], na.rm = TRUE),
        Min = min(.data[[var]], na.rm = TRUE),
        Max = max(.data[[var]], na.rm = TRUE),
        Count = n()
      )
    
    stats_text <- paste(
      "<p><b>Mean:</b> ", round(stats$Mean, 2), "<br>",
      "<b>Median:</b> ", round(stats$Median, 2), "<br>",
      "<b>Standard Deviation:</b> ", round(stats$SD, 2), "<br>",
      "<b>Minimum:</b> ", round(stats$Min, 2), "<br>",
      "<b>Maximum:</b> ", round(stats$Max, 2), "<br>",
      "<b>Count:</b> ", stats$Count, "</p>"
    )
    
    return(stats_text)
  }
  
  output$descriptiveStats <- renderUI({
    var <- input$variable
    
    stats_text <- get_descriptive_stats(vardata, var)
    
    HTML(stats_text)
  })
  
  output$scatterPlot <- renderPlotly({
    var <- input$variable
    plot_data <- vardata %>%
      select(all_of(var), Wine.Consumption.per.Adult)
    
    p <- ggplot(plot_data, aes(x = .data[[var]], y = Wine.Consumption.per.Adult)) +
      geom_point(aes(color = Wine.Consumption.per.Adult), size = 2, alpha = 0.6) +  # Reduced size of the points
      labs(title = paste(gsub("\\.", " ", var), "vs. Wine Consumption"),
           x = gsub("\\.", " ", var),
           y = "Wine Consumption per Adult") +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold")
      )
    
    ggplotly(p)
  })
  
  
  
  
  output$timePlot <- renderUI({
    var <- input$variable
    
    vardata <- vardata %>%
      mutate(Year = as.numeric(as.character(Year)))  # Convert 'Year' to numeric if it's not already
    
    by_country_data <- vardata %>%
      group_by(Year, Country) %>%
      summarize(Value = sum(.data[[var]], na.rm = TRUE), .groups = 'drop') %>%
      ungroup()
    
    x_min <- 1961
    x_max <- 2018
    
    all_years <- seq(from = x_min, to = x_max, by = 1)
    
    year_breaks <- all_years[all_years %% 10 %in% c(0, 5)]
    
    p <- ggplot(by_country_data, aes(x = Year, y = Value, fill = Country)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = paste("Time Series of", gsub("\\.", " ", var), "by Country"),
           x = "Year",
           y = gsub("\\.", " ", var)) +
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5)  # Align x-axis labels horizontally
      ) +
      scale_x_continuous(
        limits = c(x_min, x_max),  # Set fixed range
        breaks = year_breaks,       # Set specific breaks
        labels = as.character(year_breaks)  # Directly use year values as labels
      )
    
    # Convert ggplot to interactive plotly plot
    p_plotly <- ggplotly(p, tooltip = "text")  # Add tooltip if needed
    
    # Enable double-click filtering functionality
    p_plotly <- p_plotly %>% layout(
      legend = list(
        itemclick = "toggleothers"  # Toggle visibility of other traces on legend item click
      )
    )
    
    # Render the interactive plot
    p_plotly
  })
  
  output$description <- renderUI({
    var <- input$variable
    
    descriptions <- list(
      `Wine.Production.per.Capita` = "Wine Production per Capita: This variable represents the amount of wine produced per person. Historical data show significant changes over time, with production per capita varying considerably between countries and periods. Generally, higher wine production might indicate a greater availability of wine, potentially influencing wine consumption.",
      `Wine.Imports.per.Capita` = "Wine Imports per Capita: This measures the amount of wine imported per person. The data reveal changes in import patterns, especially after the 1970s, with significant increases in countries like the United States and China. Higher wine imports could be indicative of a higher demand for foreign wines, potentially affecting local wine consumption patterns.",
      `Wine.Exports.per.Capita` = "Wine Exports per Capita: This variable tracks the amount of wine exported per person. For example, Spain and Chile now have high export levels per capita, indicating a strong export market compared to historical levels. While exports might not directly influence domestic consumption, they can reflect the global competitiveness of local wine production.",
      `Real.GDP.per.Capita` = "Real GDP per Capita: This represents the economic output per person, adjusted for inflation. It helps in understanding the economic context in which wine production and consumption occur. Higher GDP per capita often correlates with greater disposable income, which might influence wine consumption patterns.",
      `Exchange.Rate` = "Exchange Rate: The rate at which one currency can be exchanged for another. Exchange rates influence import costs and thus consumption levels. A fluctuating exchange rate can affect the cost of imported wines, potentially influencing domestic wine consumption.",
      `Wine.Consumption.per.Adult` = "Wine Consumption per Adult: This variable measures the average amount of wine consumed per adult. It provides insights into drinking habits and preferences. Changes in this metric can reflect shifts in cultural attitudes towards wine, economic conditions, or the impact of public health policies on alcohol consumption."
    )
    
    HTML(descriptions[[input$variable]])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

