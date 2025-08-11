library(shiny)
library(DT)
library(shinythemes)
library(CoopGame)
library(ggplot2)
library(tidyr)
library(tibble)

# ct_table_default <- tibble::tibble(
#   Hospital = c("A", "B", "C"),
#   `Number of machines` = c(3,2,0),
#   `New machine cost` = c(0,0,1000000),
#   `Maintenance per machine` = c(100000, 100000, 100000),
#   `Utilisation cost per percent per machine` = c(4000, 4000, 4000),
#   `Required Utilisation (percentage)` = c(255, 170, 60)
# )


# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # App title
  titlePanel("Shapely Value App"),
  
  # Tabs
  tabsetPanel(
    
    tabPanel(
      "Shapley Value Demo",
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
    
    tabPanel(
      "CT Scanner Example",
      fluidRow(
        column(12,
               DTOutput("editable_table"),
               uiOutput("hospital_sliders")
               #DTOutput("ct_debug"),
               #plotOutput("ct_plot")
        )
      )
    )
  )
)



# Define Server
server <- function(input, output, session) {
  
  # Generate dynamic numeric inputs based on the number of players ----
  output$player_inputs <- renderUI({
    num_players <- input$num_players
    lapply(1:num_players, function(i) {
      numericInput(paste0("player", i), paste("Player", i), value = 0, min = 0)
    })
  })
  
  # Reactive function to calculate Shapley values ----
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
  
  # Render bar chart ----
  output$bar_chart <- renderPlot({
    data <- shapley_data()
    data_long <- pivot_longer(data, cols = c(InputValue, ShapleyValue), names_to = "Metric", values_to = "Value")
    
    ggplot(data_long, aes(x = Player, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), vjust = -0.5, fontface = "bold", size = 5) +
      labs(y = "Value", fill = "Legend") +
      theme_minimal()
  })
  
  # Generate dynamic slider inputs based on the hospitals in ct example ----
  # output$hospital_sliders <- renderUI({
  #   hospitals <- reactive_table()[["Hospital"]]
  #   lapply(hospitals, function(i) {
  #     sliderInput(paste0("hospital", i), paste("Hospital", i),
  #                 value = 0, min = 0, max = reactive_table()[["Max utilisation"]])
  #   })
  # })
  
  # Reactive data for the editable table ----
  reactive_table <- reactiveVal(
    tibble::tibble(
      Hospital = c("A", "B", "C"),
      `Number of machines` = c(3,2,0),
      `New machine cost` = c(0,0,1000000),
      `Maintenance per machine` = c(100000, 100000, 100000),
      `Utilisation cost per percent per machine` = c(4000, 4000, 4000),
      `Required Utilisation (percentage)` = c(255, 170, 60),
      `A,B,C grouped utilisations` = c(300, 185, 0),
      `A,B grouped utilisations` = c(300, 125, 0)
    )|> 
      dplyr::mutate(`Cost for Hospital` = 
                      (`Number of machines`*
                         `Maintenance per machine`)+
                      (`Utilisation cost per percent per machine`*
                         `Required Utilisation (percentage)`)+
                      `New machine cost`,
                    `Max utilisation` = 
                      `Number of machines`* 
                      100
      )
  )
  
  
  # Render the editable table ----
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
    tmp_table <- reactive_table()
    tmp_table[info$row, info$col] <- info$value
    reactive_table(tmp_table)
  })
  
  output$ct_debug <- renderDT({
    reactive_table()
  })
  
  # CT example graph ----
  
  output$ct_plot <- renderPlot({
    shiny::req(reactive_table())
    
    input_data <- reactive_table()
    # two games 
    # one ongoing usage cost sharing game
    # one single payment of shared savings game
    # the number of new machines should likely be calculated based on required utilisation 
    
    ct_input_value <- c(1320000,
                         880000,
                        1240000,
                        2200000,
                        2560000,
                        2120000,
                        2440000
    )
    
    ct_shapley_value <- shapleyValue(ct_input_value)
    
    
    hosp_group_costs <- tibble::tibble(
      tibble::tibble(
        Hospital = reactive_table()$Hospital,
        InputValue = reactive_table()$`Cost for Hospital`,
        ShapleyValue = ct_shapley_value
      )) |> 
      pivot_longer(cols = c(InputValue, 
                            ShapleyValue), 
                   names_to = "Metric", 
                   values_to = "Value")
    
    hosp_group_costs |> 
      ggplot(aes(x = Hospital, 
                 y = Value,
                 fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = round(Value, 2)), 
                position = position_dodge(width = 0.9), 
                vjust = -0.5,
                fontface = "bold",
                size = 5) +
      labs(y = "Value", fill = "Legend") +
      theme_minimal()
    
    
    # hospital_subset <- unlist(
    #   lapply(1:nrow(reactive_table()), 
    #          function(k) combn(reactive_table()$Hospital, k, simplify = FALSE)),
    #   recursive = FALSE
    # )
    
    # hospital_costs_df <- tibble::tibble(
    #   group = sapply(hospital_subset, function(x) paste(x, collapse = ","))
    #   #total_utilisation = 
    # ) #|> 
    #   #dplyr::mutate(`Grouped utilisations` = )
    
    
  })
  
  
  
}




# Run the app
shinyApp(ui, server)

