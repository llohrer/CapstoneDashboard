library(shiny)
library(shinythemes)
library(plotly)
library(DT)

# Load the data from .rda files
load("coded_panel.rda")

# Ensure Year is numeric
coded_panel$Year <- as.numeric(as.character(coded_panel$Year))

ui <- fluidPage(
  # Custom CSS for styling
  tags$head(tags$style(HTML("
    body {
      background-color: #f4f4f9; /* Light gray background for the body */
      font-family: 'Roboto', sans-serif; /* Modern font */
      margin: 0;
      padding: 0;
    }

    .year-selector {
      position: absolute;
      top: 650px; /* Adjust this to move selector down */
      left: 20px;
      background: rgba(255, 255, 255, 0.9);
      padding: 20px;
      border-radius: 12px;
      box-shadow: 0 6px 12px rgba(0, 0, 0, 0.3);
      z-index: 1000;
      max-width: 320px;
      font-size: 16px;
    }

    .map-container {
      position: relative;
      border: 1px solid #ddd;
      border-radius: 5px;
      box-shadow: 0 6px 12px rgba(0, 0, 0, 0.3);
      margin: 0px auto;
      height: calc(100vh - 150px); /* Ensure sufficient height */
      width: calc(100vw - 40px);
      background-color: #ffffff;
    }

    .info-text {
      background: rgba(255, 255, 255, 0.9);
      padding: 20px;
      border-radius: 12px;
      box-shadow: 0 6px 12px rgba(0, 0, 0, 0.3);
      margin: 20px auto;
      max-width: 80%;
      text-align: center;
    }

    .info-text h2 {
      font-size: 28px;
      font-weight: bold;
      color: #0056b3;
      margin-bottom: 10px;
    }

    .info-text p {
      font-size: 18px;
      color: #333333;
      line-height: 1.6;
    }

    .slider-output {
      font-size: 18px;
      font-weight: 600;
      margin-top: 10px;
    }

    .table-container {
      margin: 20px auto;
      width: 100%;
    }

    .table-title {
      font-size: 20px;
      font-weight: bold;
      text-align: center;
      margin-bottom: 0px;
      color: #0056b3;
    }

    .datatable-container {
      border: 1px solid #ddd;
      border-radius: 8px;
      box-shadow: 0 6px 12px rgba(0, 0, 0, 0.3);
      background-color: #ffffff;
      margin-bottom: 20px;
    }

    .table-column {
      padding: 10px;
      width: 100%;
    }
  "))),
  
  # Information Panel
  div(class = "info-text",
      h2("Global Wine Consumption Trends"),
      p("Explore wine consumption data from 29 countries from 1961 to 2018. Dive in to discover regional differences and global trends, and see how wine consumption has shifted over the decades. Remember, depending on the unit of measurement you use, the same data can reveal very different insights!")
  ),
  
  # Year Selector and Variable Selector
  div(class = "year-selector",
      sliderInput("year", "Select Year or Animate to Explore:", 
                  min = min(coded_panel$Year, na.rm = TRUE), 
                  max = max(coded_panel$Year, na.rm = TRUE), 
                  value = min(coded_panel$Year, na.rm = TRUE), 
                  step = 1,
                  ticks = TRUE, # Show tick marks
                  animate = animationOptions(interval = 500, loop = TRUE)
      ),
      selectInput("variable", "Unit of Measurement:", 
                  choices = c("Volume (Kiloliters)" = "ConVol",
                              "Liters per Capita" = "ConPerCap",
                              "Liters per Adult" = "ConPerAdult"),
                  selected = "ConVol"
      )
  ),
  
  # Layout for Map and Tables
  fluidRow(
    column(12,
           div(class = "map-container",
               plotlyOutput("map", height = "calc(100vh - 150px)")  # Adjust height accordingly
           )
    )
  ),
  
  fluidRow(
    column(6,
           div(class = "table-container",
               div(class = "table-title", "Top 5 Consumers"),
               div(class = "datatable-container",
                   DTOutput("topConsumersTable")
               )
           )
    ),
    column(6,
           div(class = "table-container",
               div(class = "table-title", "Bottom 5 Consumers"),
               div(class = "datatable-container",
                   DTOutput("bottomConsumersTable")
               )
           )
    )
  )
)

server <- function(input, output, session) {
  # Reactive expression for the year range
  selected_year_range <- reactive({
    c(min(coded_panel$Year, na.rm = TRUE), max(coded_panel$Year, na.rm = TRUE))
  })
  
  # Update the slider range dynamically
  observe({
    year_range <- selected_year_range()
    updateSliderInput(session, "year",
                      min = year_range[1],
                      max = year_range[2],
                      value = year_range[1])
  })
  
  # Render the choropleth map
  output$map <- renderPlotly({
    # Get the current year and selected variable
    year_to_display <- input$year
    variable_to_display <- input$variable
    
    # Define the variable name for the legend
    variable_name <- switch(variable_to_display,
                            "ConVol" = "Volume (Kiloliters)",
                            "ConPerCap" = "Liters per Capita",
                            "ConPerAdult" = "Liters per Adult")
    
    # Filter data based on the selected year
    filtered_data <- subset(coded_panel, Year == year_to_display)
    
    # Create the choropleth map with updated color scheme
    plot_ly(
      data = filtered_data,
      type = 'choropleth',
      locations = ~Country,
      locationmode = 'country names',
      z = filtered_data[[variable_to_display]],
      text = ~paste('<b>', Country, '</b><br>', 
                    variable_name, ': ', filtered_data[[variable_to_display]], ' liters'),
      hoverinfo = 'text',
      colorscale = "Viridis",
      colorbar = list(
        title = variable_name  # Dynamic legend title
      )
    ) %>%
      layout(
        title = list(
          text = paste("<b>The Wine Consumption Landscape by", variable_name, "in Year", as.character(input$year), "</b>"),
          font = list(size = 26, color = "#0056b3"),  # Darker blue for the title
          x = 0.5,  # Center title horizontally
          xanchor = "center"  # Anchor the title in the center
        ),
        geo = list(
          showcoastlines = TRUE,
          coastlinecolor = '#000000',  # Use hex code
          showland = TRUE,
          landcolor = '#FFFFFF',       # Use hex code
          showocean = TRUE,
          oceancolor = '#F2F2F2',      # Use hex code
          showlakes = TRUE,
          lakecolor = '#FFFFFF'        # Use hex code
        ),
        margin = list(t = 80)  # Adjust top margin to add buffer above the title
      )
  })
  
  # Helper function to get filtered data
  get_filtered_data <- reactive({
    filtered_data <- subset(coded_panel, Year == input$year)
    variable_to_display <- input$variable
    
    # Filter out rows with missing or invalid country names
    filtered_data <- filtered_data[!is.na(filtered_data$Country) & filtered_data$Country != "", ]
    
    # Ensure that only country names and values are included
    filtered_data <- filtered_data[, c("Country", variable_to_display)]
    
    filtered_data
  })
  
  # Render the Top 5 Consumers table
  output$topConsumersTable <- renderDT({
    filtered_data <- get_filtered_data()
    variable_to_display <- input$variable
    variable_name <- switch(variable_to_display,
                            "ConVol" = "Volume (Kiloliters)",
                            "ConPerCap" = "Liters per Capita",
                            "ConPerAdult" = "Liters per Adult")
    
    # Sort and select top 5 countries
    top_5 <- head(filtered_data[order(-filtered_data[[variable_to_display]]), ], 5)
    
    top_5_table <- data.frame(
      Country = top_5$Country,
      Value = top_5[[variable_to_display]],
      stringsAsFactors = FALSE
    )
    
    datatable(top_5_table,
              colnames = c("Country", variable_name),
              options = list(
                paging = FALSE,
                searching = FALSE,
                info = FALSE,
                ordering = FALSE,
                autoWidth = TRUE,
                dom = 't'
              ),
              rownames = FALSE)
  })
  
  # Render the Bottom 5 Consumers table
  output$bottomConsumersTable <- renderDT({
    filtered_data <- get_filtered_data()
    variable_to_display <- input$variable
    variable_name <- switch(variable_to_display,
                            "ConVol" = "Volume (Kiloliters)",
                            "ConPerCap" = "Liters per Capita",
                            "ConPerAdult" = "Liters per Adult")
    
    # Sort and select bottom 5 countries
    bottom_5 <- head(filtered_data[order(filtered_data[[variable_to_display]]), ], 5)
    
    bottom_5_table <- data.frame(
      Country = bottom_5$Country,
      Value = bottom_5[[variable_to_display]],
      stringsAsFactors = FALSE
    )
    
    datatable(bottom_5_table,
              colnames = c("Country", variable_name),
              options = list(
                paging = FALSE,
                searching = FALSE,
                info = FALSE,
                ordering = FALSE,
                autoWidth = TRUE,
                dom = 't'
              ),
              rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)

