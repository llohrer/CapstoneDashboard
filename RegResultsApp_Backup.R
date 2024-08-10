
# List of packages to load 
library(shiny)
library(DT)      
library(ggplot2) 
library(shinythemes)
library(tibble)
library(plotly)


# List of .rda files to load
files <- c(
  "robust_se_model_1.rda", 
  "robust_se_model_2.rda", 
  "robust_se_model_3.rda", 
  "robust_se_model_4.rda", 
  "robust_se_model_5.rda", 
  "robust_se_model_6.rda",
  "reg_summaries.rda",
  "model_statistics.rda",
  "model_formulas.rda"
)

# Load each .rda file
for (file in files) {
  load(file)
}

# Helper function to format numbers consistently
format_numbers <- function(df, digits = 8) {
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) {
      format(round(col, digits), nsmall = digits)
    } else {
      col
    }
  })
  df
}

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # Custom CSS to center titles, add spacing, and format text
  tags$head(
    tags$style(HTML("
      .title-center { text-align: center; font-weight: bold; }
      .title-center h3, .title-center h4 { font-weight: bold; }
      .explanatory-text { margin-top: 20px; font-size: 14px; }
      .well-panel { margin-bottom: 20px; }
      .plot-container { margin-top: 20px; }
      .test-results { font-size: 14px; }
      .interpretation-text { margin-top: 20px; font-size: 14px; }
      .interpretation-visuals { margin-top: 20px; }
      .summary-box { margin-top: 20px; padding: 15px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9; }
      .plot-spacing { margin-bottom: 20px; }
    "))
  ),
  
  titlePanel("Regression Results Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model", "Select Model:",
                  choices = paste0("Model ", seq_along(model_summaries)),
                  selected = "Model 1"),
      selectInput("errorType", "Select Error Type:",
                  choices = c("Normal Standard Errors", "Robust Standard Errors"),
                  selected = "Normal Standard Errors"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        id = 'tabs',
        type = "tabs",
        
        # Tab for Model Results
        tabPanel("Results",
                 wellPanel(
                   uiOutput("coefficientsTableTitle"),
                   DT::dataTableOutput("coefficientsTable"),
                   HTML("<div class='explanatory-text'>
                          <small>*Wine consumption is expressed in liters/adult; wine production, imports, and exports are expressed in liters/capita; real GDP is expressed at 1990 International Geary-Khamis dollars per capita; the exchange rate is expressed in LCU per US$, period average; lagged variables are lagged by 1 year.</small>
                        </div>")
                 ),
                 wellPanel(
                   h4("Model Fit Statistics", class = "title-center"),
                   DT::dataTableOutput("additionalStatsTable")
                 )
        ),
        
        # Tab for Residual Analysis
        tabPanel("Residual Analysis",
                 wellPanel(
                   h3("Residual Analysis", class = "title-center")
                 ),
                 fluidRow(
                   column(6, plotOutput("residualsPlot", height = "300px")),
                   column(6, plotOutput("acfPlot", height = "300px"))
                 ),
                 wellPanel(
                   div(class = "test-results", uiOutput("bpTestResults"))
                 ),
                 wellPanel(
                   h4("Robust Standard Errors", class = "title-center"),
                   DT::dataTableOutput("robustSEsTable")
                 )
        ),
        
        # Updated Tab for Interpretations
        tabPanel("Interpretations",
                 wellPanel(
                   h3("Interpretations", class = "title-center")
                 ),
                 fluidRow(
                   column(12,
                          plotlyOutput("coefficientPlot", height = "400px")
                   )
                 ),
                 fluidRow(
                   column(12,
                          div(class = "summary-box", uiOutput("interpretationSummary"))
                   )
                 )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Reactive expression to get the selected model's results
  selected_model <- reactive({
    model_index <- as.numeric(sub("Model ", "", input$model))
    model_data <- model_summaries[[paste0("Model_", model_index)]]
    
    list(
      model_data = model_data,
      formula = model_formulas[[paste0("Model ", model_index)]]
    )
  })
  
  # Helper function to rename columns based on error type
  rename_columns <- function(df, error_type) {
    if (error_type == "Normal Standard Errors") {
      df <- df %>%
        rename(
          Variable = term,
          Estimate = estimate,
          Std_Error = std.error,
          t_value = statistic,
          Pr_It = p.value
        )
    } else if (error_type == "Robust Standard Errors") {
      df <- df %>%
        rename(
          Variable = Variable,
          Estimate = Estimate,
          Std_Error = Std_Error,
          t_value = t_value,
          Pr_It = Pr_It
        )
    }
    return(df)
  }
  
  # Render the title for Coefficients Table dynamically
  output$coefficientsTableTitle <- renderUI({
    formula_text <- selected_model()$formula
    if (!is.null(formula_text)) {
      h3(paste(formula_text), class = "title-center")
    } else {
      h3("Coefficients Table", class = "title-center")
    }
  })
  
  # Render the Coefficients Table
  output$coefficientsTable <- DT::renderDataTable({
    model_data <- selected_model()$model_data
    if (!is.null(model_data)) {
      coefficients_data <- format_numbers(model_data$tidy, 8)
      datatable(coefficients_data, options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE,
        columnDefs = list(list(
          targets = "_all",
          className = "dt-center"
        )),
        dom = 't'
      )) %>% 
        formatStyle(columns = colnames(coefficients_data), 
                    fontWeight = 'bold', 
                    textAlign = 'center',
                    backgroundColor = styleEqual(colnames(coefficients_data), rep('lightgrey', length(colnames(coefficients_data)))))
    } else {
      datatable(data.frame(), options = list(
        searching = FALSE,
        paging = FALSE,
        info = FALSE,
        dom = 't'
      )) %>% 
        formatStyle(columns = colnames(data.frame()), 
                    fontWeight = 'bold', 
                    textAlign = 'center')
    }
  })
  
  # Load Robust Standard Errors data
  load_model_robust_se <- function(model_number) {
    load(paste0("robust_se_model_", model_number, ".rda"))
    get(paste0("model_", model_number, "_data"))
  }
  
  # Render the Robust Standard Errors table
  output$robustSEsTable <- DT::renderDataTable({
    model_number <- as.numeric(sub("Model ", "", input$model))
    robust_se_data <- load_model_robust_se(model_number)
    robust_se_data <- format_numbers(robust_se_data, 8)
    
    datatable(robust_se_data, options = list(
      searching = FALSE,
      paging = FALSE,
      info = FALSE,
      columnDefs = list(list(
        targets = "_all",
        className = "dt-center"
      )),
      dom = 't'
    )) %>% 
      formatStyle(columns = colnames(robust_se_data), 
                  fontWeight = 'bold', 
                  textAlign = 'center',
                  backgroundColor = styleEqual(colnames(robust_se_data), rep('lightgrey', length(colnames(robust_se_data)))))
  })
  
  # Render the Residuals vs Fitted plot
  output$residualsPlot <- renderPlot({
    model_data <- selected_model()$model_data
    if (!is.null(model_data)) {
      df <- model_data$combined_data
      ggplot(df, aes(x = FittedValues, y = Residuals)) +
        geom_point(alpha = 0.6) +
        labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs Fitted Values") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.title = element_text(size = 14))
    } else {
      ggplot() + labs(title = "No data available")
    }
  })
  
  # Render the ACF plot
  output$acfPlot <- renderPlot({
    model_data <- selected_model()$model_data
    if (!is.null(model_data)) {
      acf_data <- model_data$acf
      ggplot(data.frame(lag = seq_along(acf_data) - 1, acf = acf_data), aes(x = lag, y = acf)) +
        geom_bar(stat = "identity") +
        labs(x = "Lag", y = "ACF", title = "ACF of Residuals") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold"),
              axis.title = element_text(size = 14))
    } else {
      ggplot() + labs(title = "No data available")
    }
  })
  
  # Render the Additional Statistics comparison table in "Results" tab
  output$additionalStatsTable <- DT::renderDataTable({
    model_statistics_formatted <- as.data.frame(model_statistics, row.names = NULL)
    rownames(model_statistics_formatted) <- seq_len(nrow(model_statistics_formatted))
    formatted_stats <- format_numbers(model_statistics_formatted, 8)
    datatable(formatted_stats[, -1], options = list(
      searching = FALSE,
      paging = FALSE,
      info = FALSE,
      columnDefs = list(list(
        targets = "_all",
        className = "dt-center"
      )),
      dom = 't'
    )) %>% 
      formatStyle(columns = colnames(formatted_stats[, -1]), 
                  fontWeight = 'bold', 
                  textAlign = 'center')
  })
  
  # Render the Breusch-Pagan Test results
  output$bpTestResults <- renderUI({
    model_data <- selected_model()
    if (!is.null(model_data$model_data)) {
      bp_test <- model_data$model_data$breusch_pagan_test
      if (!is.null(bp_test)) {
        result <- paste0(
          "<div class='test-results' style='text-align: center;'>",
          "<h4>Breusch-Pagan Test</h4>",
          "<p><strong>Null Hypothesis:</strong> Homoscedasticity (constant variance of residuals)</p>",
          "<p><strong>Test Statistic:</strong> ", format(round(bp_test$test_statistic, 8), nsmall = 8), "</p>",
          "<p><strong>P-Value:</strong> ", format(round(bp_test$p_value, 8), nsmall = 8), "</p>",
          "<p>", ifelse(bp_test$p_value < 0.05, 
                        "<strong style='color: red;'>Heteroscedasticity detected; reject the null hypothesis!</strong>", 
                        "<strong style='color: green;'>No heteroscedasticity detected; fail to reject the null hypothesis</strong>"), "</p>",
          "</div>"
        )
      } else {
        result <- "<div class='test-results' style='text-align: center;'><h4>Breusch-Pagan Test</h4><p>No data available</p></div>"
      }
      HTML(result)
    } else {
      HTML("<div class='test-results' style='text-align: center;'><h4>Breusch-Pagan Test</h4><p>No data available</p></div>")
    }
  })
  
  # Render the Coefficients Plot based on selected error type
  output$coefficientPlot <- renderPlotly({
    model_number <- as.numeric(sub("Model ", "", input$model))
    error_type <- input$errorType
    model_data <- selected_model()$model_data
    
    if (!is.null(model_data)) {
      if (error_type == "Normal Standard Errors") {
        # Use model_data$tidy directly
        plot_data <- model_data$tidy %>%
          rename_columns(., error_type) %>%
          mutate(variable = reorder(Variable, Estimate)) %>%
          ggplot(aes(x = Estimate, y = variable)) +
          geom_point(aes(size = Std_Error, color = Pr_It < 0.05), alpha = 0.8) +
          scale_color_manual(values = c("black", "red")) +
          labs(x = "Coefficient Estimate", y = "", title = "Coefficient Estimates with Standard Errors") +
          theme_minimal() +
          theme(plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 14))
      } else if (error_type == "Robust Standard Errors") {
        robust_se_data <- load_model_robust_se(model_number)
        plot_data <- robust_se_data %>%
          rename_columns(., error_type) %>%
          mutate(variable = reorder(Variable, Estimate)) %>%
          ggplot(aes(x = Estimate, y = variable)) +
          geom_point(aes(size = Std_Error, color = Pr_It < 0.05), alpha = 0.8) +
          scale_color_manual(values = c("black", "red")) +
          labs(x = "Coefficient Estimate", y = "", title = "Coefficient Estimates with Robust Standard Errors") +
          theme_minimal() +
          theme(plot.title = element_text(size = 16, face = "bold"),
                axis.title = element_text(size = 14))
      }
      
      ggplotly(plot_data)
    } else {
      ggplotly(ggplot() + labs(title = "No data available"))
    }
  })
  
  # Render the Interpretation Summary
  output$interpretationSummary <- renderUI({
    model_data <- selected_model()$model_data
    error_type <- input$errorType
    
    if (!is.null(model_data)) {
      # Handle column names based on the error type
      if (error_type == "Normal Standard Errors") {
        coefficients <- model_data$tidy %>%
          rename(
            Variable = term,
            Estimate = estimate,
            Std_Error = std.error,
            t_value = statistic,
            Pr_It = p.value
          )
      } else if (error_type == "Robust Standard Errors") {
        coefficients <- load_model_robust_se(as.numeric(sub("Model ", "", input$model))) %>%
          rename(
            Variable = Variable,
            Estimate = Estimate,
            Std_Error = Std_Error,
            t_value = t_value,
            Pr_It = Pr_It
          )
      }
      
      # Significance level
      significance_level <- 0.05
      
      # Create interpretation text
      interpretation_text <- sapply(seq_len(nrow(coefficients)), function(i) {
        variable <- coefficients$Variable[i]
        estimate <- coefficients$Estimate[i]
        p_value <- coefficients$Pr_It[i]
        significance <- ifelse(p_value < significance_level, "statistically significant", "not statistically significant")
        significance_text <- ifelse(significance == "not statistically significant",
                                    paste0("However, the effect is not statistically significant at the ", significance_level, " level."),
                                    "")
        direction <- ifelse(estimate > 0, "increase", "decrease")
        paste0(
          "<p><strong>", variable, "</strong>: Holding all other variables constant, a 1-unit increase in ", variable,
          " is associated with a ", format(abs(estimate), digits = 4), " liters per capita ", direction, 
          " in wine consumption. ", significance_text, "</p>"
        )
      })
      
      # Combine all interpretation text
      HTML(paste0(
        "<div class='interpretation-text'>",
        "<h4>Interpretations</h4>",
        paste(interpretation_text, collapse = ""),
        "</div>"
      ))
    } else {
      HTML("<div class='interpretation-text'><h4>Interpretations</h4><p>No data available</p></div>")
    }
  })
  

}



# Run the application
shinyApp(ui = ui, server = server)
