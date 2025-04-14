library(shiny)
library(DT)
library(shinythemes)
library(CoopGame)
library(ggplot2)
library(tidyr)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"), # Optional theme for styling

  # App title
  titlePanel("Tabbed App Example"),

  # Tabs
  tabsetPanel(

    # Tab A: Numeric Inputs
    tabPanel(
      "Tab A: Numeric Inputs",
      sidebarLayout(
        # Sidebar panel for sliders
        sidebarPanel(
          h3("Player Scores"),
          sliderInput("num_players", "Number of Players:", min = 1, max = 10, value = 3),
          uiOutput("player_inputs")
        ),

        # Main panel for the bar chart and table
        mainPanel(
          h3("Player Scores and Shapley Values"),
          plotOutput("bar_chart")
        )
      )
    ),

    # Tab B: Editable Data Table
    tabPanel(
      "Tab B: Editable Data Table",
      fluidRow(
        column(12,
               h3("Editable Data Table"),
               DTOutput("editable_table")
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Generate dynamic numeric inputs based on the number of players
  output$player_inputs <- renderUI({
    num_players <- input$num_players
    lapply(1:num_players, function(i) {
      numericInput(paste0("player", i), paste("Player", i), value = 0, min = 0)
    })
  })

  # Reactive function to calculate Shapley values
  shapley_data <- reactive({
    num_players <- input$num_players
    scores <- sapply(1:num_players, function(i) {
      input_id <- paste0("player", i)
      if (!is.null(input[[input_id]])) input[[input_id]] else 0
    })

    coalitions <- list()
    for (size in 1:num_players) {
      subsets <- combn(num_players, size, simplify = FALSE)
      for (subset in subsets) {
        coalition_value <- max(scores[subset])
        coalitions[[length(coalitions) + 1]] <- list(
          Players = paste("Player", subset, collapse = ", "),
          MaxValue = coalition_value
        )
      }
    }

    coalition_values <- sapply(coalitions, function(coalition) coalition$MaxValue)
    shapley_values <- shapleyValue(coalition_values)

    data.frame(
      Player = paste("Player", 1:num_players),
      InputValue = scores,
      ShapleyValue = shapley_values
    )
  })

  # Render bar chart
  output$bar_chart <- renderPlot({
    data <- shapley_data()
    data_long <- pivot_longer(data, cols = c(InputValue, ShapleyValue), names_to = "Metric", values_to = "Value")

    ggplot(data_long, aes(x = Player, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), vjust = -0.5, fontface = "bold", size = 5) +
      labs(y = "Value", fill = "Legend") +
      theme_minimal()
  })

  # Reactive data for the editable table
  reactive_table <- reactiveVal(
    data.frame(
      Hospital = c("A", "B", "C"),
      `Number of machines` = c(3,2,0),
      `New machine cost` = c(0,0,1000000),
      `Maintenance per machine` = rep(100000, 3),
      `Utilisation cost per percent per machine` = rep(4000, 3),
      `Required Utilisation (percentage)` = c(255, 170, 60),
      stringsAsFactors = FALSE
    )
  )

  # Render the editable table
  output$editable_table <- renderDT({
    datatable(
      reactive_table(),
      editable = TRUE,
      options = list(
        dom = 't',
        paging = FALSE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '100px', targets = "_all")) # Fix column widths
      )
    )
  })

  # Handle edits to the table
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    table <- reactive_table()
    table[info$row, info$col] <- info$value
    reactive_table(table)
  })
}

# Run the app
shinyApp(ui, server)

