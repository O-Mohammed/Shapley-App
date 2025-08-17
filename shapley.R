library(shiny)
library(DT)
library(shinythemes)
library(CoopGame)
library(ggplot2)
library(tidyr)
library(tibble)

safe_shapley <- function(v){
  if(all(v ==0)){
    0
  } else{
    CoopGame::shapleyValue(v)
  }
}

generate_coalitions <- function(players) {
  n <- length(players)
  coalitions <- unlist(
    lapply(1:n, function(k) {
      combn(players, k, simplify = FALSE)
    }), 
    recursive = FALSE
  )
  return(coalitions)
}


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
               DTOutput("ct_coalitions_df"),
               plotOutput("ct_plot")
               
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
    shapley_values <- safe_shapley(coalition_values)
    
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
  ct_initial_data <- reactiveVal(
    tibble::tibble(
      Hospital = c("A", "B", "C"),
      `Number of machines` = c(3,2,0),
      `New machine cost` = c(0,0,1000000),
      `Maintenance per machine` = c(100000, 100000, 100000),
      `Utilisation cost per percent per machine` = c(4000, 4000, 4000),
      `Required Utilisation (percentage)` = c(255, 170, 60)
    )
  )
  
  
  ct_calculated_table <- reactive({
    ct_initial_data() |> 
      dplyr::mutate(`Max utilisation` = 
                      `Number of machines`* 100,
                    
                    `New machines required` = dplyr::if_else(
                      `Required Utilisation (percentage)` >
                        `Max utilisation`, 
                      ceiling(
                        (`Required Utilisation (percentage)`-
                           `Max utilisation`)/
                          100), 
                      0),
                    
                    `Initial cost for Hospital` = `New machine cost`*
                      `New machines required`,
                    
                    `Ongoing cost for Hospital` = 
                      ((`Number of machines`+ 
                          `New machines required`) *
                         `Maintenance per machine`)+
                      (`Utilisation cost per percent per machine`*
                         `Required Utilisation (percentage)`)
                    )
  })
  
  
  
  # Render the editable table ----
  output$editable_table <- renderDT({
    datatable(
      ct_calculated_table(),
      editable = list(target = "cell", disable = list(columns = c(7,8))),
      options = list(
        dom = 't',
        paging = FALSE,
        autoWidth = TRUE,
        ordering = F,
        columnDefs = list(list(width = '100px', targets = "_all")) # Fix column widths
      )
    )
  }
  )
  
  # Handle edits to the table
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    table <- ct_calculated_table()
    table[info$row, info$col] <- info$value
    ct_initial_data(table)
  })
  
  
  # CT coalitions table ----
  
  #shiny::observe({
    ct_coalitions_df <- shiny::reactive({
      
      generated_coalitions <- generate_coalitions(ct_calculated_table()$Hospital)
      
      tibble::tibble(coalitions = generated_coalitions,
                     coalitions_tmp = generated_coalitions) |> 
        dplyr::mutate(coalition_group = dplyr::row_number()) |> 
        unnest(cols = coalitions_tmp) |> 
        dplyr::rename("Hospital" = coalitions_tmp) |> 
        dplyr::left_join(ct_calculated_table(),
                         by = "Hospital") |>
        dplyr::group_by(coalition_group) |> 
        dplyr::mutate(coalition_required_utilisation = sum(`Required Utilisation (percentage)`),
                      coalition_max_utilisation = sum(`Max utilisation`),
                      current_machines = sum(`Number of machines`),
                      new_machines_needed = dplyr::if_else(coalition_required_utilisation >
                                                             coalition_max_utilisation,
                                                           ceiling((coalition_required_utilisation - 
                                                                      coalition_max_utilisation)/100),
                                                           0),
                      total_new_machines_cost = new_machines_needed*sum(`New machine cost`)) |> 
        dplyr::ungroup() |> 
        dplyr::mutate(coalitions = purrr::map_chr(coalitions, 
                                                  ~ paste0("{", paste(.x, collapse = ", "), "}")
        )
        
        ) |> 
        dplyr::select(coalitions, 
                      coalition_required_utilisation,
                      coalition_max_utilisation,
                      current_machines,
                      new_machines_needed,
                      `Maintenance per machine`,
                      `Utilisation cost per percent per machine`,
                      total_new_machines_cost) |> 
        dplyr::distinct() |> 
        dplyr::mutate(total_machines_needed = current_machines + new_machines_needed,
                      total_maintenance_cost = `Maintenance per machine`*total_machines_needed,
                      total_utilisation_cost = `Utilisation cost per percent per machine`*coalition_required_utilisation) |> 
        dplyr::select(coalitions, 
                      coalition_required_utilisation,
                      coalition_max_utilisation,
                      current_machines,
                      new_machines_needed,
                      `Maintenance per machine`,
                      `Utilisation cost per percent per machine`,
                      total_machines_needed,
                      total_new_machines_cost,
                      total_maintenance_cost,
                      total_utilisation_cost) |> 
        dplyr::mutate(cost_saving_game_values = total_new_machines_cost,
                      cost_sharing_game_values = total_maintenance_cost + total_utilisation_cost)
    })
    
  
  
  output$ct_coalitions_df <- renderDT({
    datatable(ct_coalitions_df(),
      options = list(
      dom = 't',
      paging = FALSE,
      ordering = F,
      autoWidth = TRUE)
    )
    
  })
  
  
  # CT example graph ----
  
  output$ct_plot <- renderPlot({
    shiny::req(ct_coalitions_df())
    
    input_data <- ct_coalitions_df()
    # two games 
    # one ongoing usage cost sharing game
    # one single payment of shared savings game
    # the number of new machines should likely be calculated based on required utilisation 
    
    cost_saving_value <- safe_shapley(input_data$cost_saving_game_values)
    
    cost_sharing_value <- safe_shapley(input_data$cost_sharing_game_values)
    
    #ct_shapley_value <- cost_saving_value+cost_sharing_value 
    
    
    hosp_group_costs <- tibble::tibble(
      tibble::tibble(
        Hospital = ct_calculated_table()$Hospital,
        Initial_Cost = ct_calculated_table()$`Initial cost for Hospital`,
        Ongoing_Cost = ct_calculated_table()$`Ongoing cost for Hospital`,
        Shap1_Buy_in = cost_saving_value,
        Shap2_Ongoing = cost_sharing_value#,
        #ShapleyValue = ct_shapley_value
      )) |> 
      pivot_longer(cols = c(Initial_Cost,
                            Ongoing_Cost,
                            Shap1_Buy_in,
                            Shap2_Ongoing), 
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

