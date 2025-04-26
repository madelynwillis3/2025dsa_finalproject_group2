library(shiny)
library(tidyverse)
library(plotly)
library(png)
library(grid)

# Load df_model
df_model <- read_csv("../data/df_model.csv")

# Define UI
ui <- fluidPage(
  titlePanel("EDA and Variable Importance for Yield"),
  
  # Use tabbed navigation
  tabsetPanel(
    id = "tabs",
    tabPanel("EDA Yield/Variables",
             selectInput(
               "group_var",
               "Select Grouping Variable:",
               choices = names(df_model)[!(names(df_model) %in% c("yield_mg_ha", "hybrid"))],  # Exclude 'yield_mg_ha' and 'hybrid'
               selected = "previous_crop"
             ),
             plotlyOutput("boxplot")
    ),
    tabPanel("Yield Density", plotlyOutput("density_plot")),
    tabPanel("Variable Correlation Matrix", plotlyOutput("correlation_plot")),
    tabPanel("Variable Importance Based on Model Type",
             selectInput(
               "model_type",
               "Select Model for Variable Importance:",
               choices = c("XGBoost", "Random Forest"),
               selected = "XGBoost"
             ),
             plotOutput("variable_importance_plot", height = "600px", width = "800px")  # Set larger height and width
    ),
    tabPanel("Results: Predicted vs Actual",
             selectInput(
               "results_model_type",
               "Select Model for Results:",
               choices = c("XGBoost", "Random Forest"),
               selected = "XGBoost"
             ),
             plotOutput("results_plot", height = "600px", width = "800px")  # Display the results plot
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Boxplot
  output$boxplot <- renderPlotly({
    filtered_data <- df_model %>%
      filter(!is.na(!!sym(input$group_var)), !is.na(yield_mg_ha))
    filtered_data[[input$group_var]] <- as.factor(filtered_data[[input$group_var]])
    p <- ggplot(filtered_data, aes_string(x = input$group_var, y = "yield_mg_ha")) +
      geom_boxplot() +
      labs(
        title = paste("Boxplot of Yield by", input$group_var),
        x = input$group_var,
        y = "Yield (mg/ha)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # Density Plot
  output$density_plot <- renderPlotly({
    p <- ggplot(df_model, aes(x = yield_mg_ha)) +
      geom_density(fill = "blue", alpha = 0.5) +
      labs(
        title = "Density Plot of Yield",
        x = "Yield (mg/ha)",
        y = "Density"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Correlation Heatmap
  output$correlation_plot <- renderPlotly({
    correlations <- df_model %>%
      select(where(is.numeric)) %>%
      cor(use = "complete.obs")
    p <- ggcorrplot(correlations, lab = TRUE, colors = c("red", "white", "blue")) +
      labs(title = "Variable Correlation Matrix")
    ggplotly(p)
  })
  
  # Variable Importance Plot
  output$variable_importance_plot <- renderPlot({
    if (input$model_type == "XGBoost") {
      xgb_final_spec %>%
        fit(yield_mg_ha ~ ., data = bake(prep(df_recipe), new_data = df_train)) %>%
        vi() %>%
        mutate(Variable = fct_reorder(Variable, Importance)) %>%
        ggplot(aes(x = Importance, y = Variable)) +
        geom_col() +
        scale_x_continuous(expand = c(0, 0)) +
        labs(
          title = "Variable Importance - XGBoost",
          y = NULL
        ) +
        theme_minimal()
    } else {
      final_rf %>%
        fit(yield_mg_ha ~ ., data = bake(prep(df_recipe), new_data = df_train)) %>%
        vi() %>%
        mutate(Variable = fct_reorder(Variable, Importance)) %>%
        ggplot(aes(x = Importance, y = Variable)) +
        geom_col() +
        scale_x_continuous(expand = c(0, 0)) +
        labs(
          title = "Variable Importance - Random Forest",
          y = NULL
        ) +
        theme_minimal()
    }
  })
  
  # Results: Predicted vs Actual Plot
  output$results_plot <- renderPlot({
    # Load the appropriate results plot based on user selection
    if (input$results_model_type == "XGBoost") {
      img_path <- "../output/xgb_results.png"  # Path to XGBoost results plot
    } else {
        img_path <- "../output/rf_results.png"  # Path to Random Forest results plot
    }
    
    # Render the saved image
    if (file.exists(img_path)) {
      img <- png::readPNG(img_path)
      grid::grid.raster(img, width = unit(1, "npc"), height = unit(1, "npc"))  # Fit the image to the full plot area
    } else {
      plot.new()
      text(0.5, 0.5, "Results Plot Not Found", cex = 1.5)
    }
  })
}

# Run the App
shinyApp(ui = ui, server = server)