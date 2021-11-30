library(shiny)
library(tidyverse)

ui <- 
  navbarPage(title = "Visualization of Results",
    tabPanel(title = "Solution",
      sidebarLayout(
        sidebarPanel(
          p("Please select a solution method, an arrival rate variance and a specific number of UAVs."),
          selectInput(inputId = "solution_methods",
                      label = "Choose solution methods to compare",
                      choices = c("GA:TOT", "GA:SAFE", "GA:ARV", "KM:WCSS"),
                      selected = c("GA:TOT","KM:WCSS"),
                      multiple = TRUE),
          selectInput(inputId = "arrival_rate_variance",
                      label = "Choose arrival rate variance",
                      choices = c("low", "medium", "high"),
                      selected = c("low","medium","high"),
                      multiple = TRUE),
          selectInput(inputId = "number_of_uavs",
                      label="Choose number of UAVs",
                      choices = c("low", "medium", "high"),
                      selected = c("low", "medium", "high"),
                      multiple = TRUE)
        ),
        mainPanel(
          plotOutput("plot_objective", width = "100%")
        )
      )
  ),
  tabPanel(title = "Simulation",
    sidebarLayout(
      sidebarPanel(
        p("Please select a solution method, an arrival rate variance and a specific number of UAVs."),
        selectInput(inputId = "solution_methods_sim",
                    label = "Choose solution methods to compare",
                    choices = c("GA:TOT", "GA:SAFE", "GA:ARV", "KM:WCSS"),
                    selected = c("GA:TOT","KM:WCSS"),
                    multiple = TRUE),
        selectInput(inputId = "arrival_rate_variance_sim",
                    label = "Choose arrival rate variance",
                    choices = c("low", "medium", "high"),
                    selected = c("low", "medium", "high"),
                    multiple = TRUE),
        selectInput(inputId = "number_of_uavs_sim",
                    label="Choose number of UAVs",
                    choices = c("low", "medium", "high"),
                    selected = c("low", "medium", "high"),
                    multiple = TRUE)
      ),
      mainPanel(
        # h4("Utilization (top row is number of UAVs, bottom is arrival rate variance)"), br(),
        # plotOutput("plot_utilization", height = "750px"),
        h4("Ratio between number of demand generated vs number of demands served"), br(),
        plotOutput("plot_response_rate", height = "300px"),
        h4("Mean response time per demand point in seconds"), br(),
        plotOutput("plot_service_time", height = "300px")
      )
    )
  )
)


placeholder <- function() {
  ggplot() + geom_text(aes(x = 1, y = 1, label = "Placeholder")) + theme_void()
}

server <- function(input, output) {
  # source('2d-instance.R')
  cat("Reading solution results... ")
  solution_result <- readRDS("./solution-results.rds")
  cat("done!\n")
  cat("Reading simulation results... ")
  simulation_result <- readRDS("./simulation-results.rds")
  cat("done!\n")
  theme_set(theme_bw(base_size=16))
  
  # Helper functions
  uav_height <- reactive({length(input$number_of_uavs)})
  
  mean_coverage <- function(x) {
    mc <- do.call(
      bind_rows,
      lapply(
        x,
        function(z) z$demandPerformance %>% 
          summarise(demand_coverage = sum(nCovered)/sum(nGenerated))
      )
    )
    as.numeric(mc)
  }
  
  mean_response <- function(x) {
    mc <- do.call(
      bind_rows,
      lapply(
        x,
        function(z) z$demandPerformance %>%
          filter(nCovered != 0) %>%
          mutate(mean_response = totalResponseTime/nCovered) %>%
          summarise(mean_response = mean(mean_response))
      )
    )
    as.numeric(mc)
  }
  
  ### SOLUTION COMPARISON
  output$plot_objective <- renderPlot({
    solution_result %>%
      filter(`Solution method` %in% input$solution_methods,
             Objective %in% c("TOT", "WCSS"),
             `Number of UAVs` %in% input$number_of_uavs,
             `Arrival rate variance` %in% input$arrival_rate_variance
             ) %>%
      ggplot(aes(x = `Arrival rate variance`, 
                 y = `Objective value`,
                 color = `Solution method`)) +
      geom_boxplot() +
      facet_wrap(`Number of UAVs`~Objective, scales = "free", nrow = length(input$number_of_uavs))
  }, height = reactive({uav_height()*2.5*100}), width = 1000)
  
  ### SIMULATION COMPARISON
  # output$plot_utilization <- renderPlot({
  #   simulation_result %>%
  #     unnest(cols = utilization) %>%
  #     unnest(cols = utilization) %>%
  #     filter(`Solution method` %in% input$solution_methods_sim,
  #            `Number of UAVs` %in% input$number_of_uavs_sim,
  #            `Arrival rate variance` %in% input$arrival_rate_variance_sim,
  #            row_number() %% 120 == 0) %>% # reduce number of observations to plot
  #     ggplot(aes(x=time, 
  #                y=inUse/time, 
  #                group = file, 
  #                color = `Solution method`)) + 
  #     geom_line() +
  #     facet_wrap(`Number of UAVs`~`Arrival rate variance`)
  # }, height = reactive({length(input$number_of_uavs_sim)*2.5*100}), 
  #    width = 1000)
  
  output$plot_response_rate <- renderPlot({
    simulation_result %>%
      filter(`Solution method` %in% input$solution_methods_sim,
             `Number of UAVs` %in% input$number_of_uavs_sim,
             `Arrival rate variance` %in% input$arrival_rate_variance_sim) %>%
      rowwise() %>%
      mutate(mean_coverage = mean_coverage(metric)) %>%
      select(-c(metric, utilization)) %>%
      group_by(`Solution method`, `Number of UAVs`, `Arrival rate variance`) %>%
      summarise(t = (sd(mean_coverage)/sqrt(20))*pt(c(.975), 1),
                mean_coverage = mean(mean_coverage)) %>%
      ggplot(aes(x = `Arrival rate variance`,
                 y = mean_coverage,
                 fill = `Solution method`)) +
      geom_bar(stat = "identity", color = "black", position = position_dodge()) +
      geom_errorbar(aes(ymin = mean_coverage - t, ymax = mean_coverage + t),
                    position = position_dodge(.9), width = .15) +
      facet_wrap(~ `Number of UAVs`, nrow = 1)
  }, height = 300, 
     width = 1000)
  
  output$plot_service_time <- renderPlot({
    simulation_result %>%
      filter(`Solution method` %in% input$solution_methods_sim,
             `Number of UAVs` %in% input$number_of_uavs_sim,
             `Arrival rate variance` %in% input$arrival_rate_variance_sim) %>%
      rowwise() %>%
      mutate(mean_response = mean_response(metric)) %>%
      select(-c(metric, utilization)) %>%
      group_by(`Solution method`, `Number of UAVs`, `Arrival rate variance`) %>%
      summarise(t = (sd(mean_response)/sqrt(20))*pt(c(.975), 1),
                mean_response = mean(mean_response)) %>%
      ggplot(aes(x = `Arrival rate variance`,
                 y = mean_response,
                 fill = `Solution method`)) +
      geom_bar(stat = "identity", color = "black", position = position_dodge()) +
      geom_errorbar(aes(ymin = mean_response - t, ymax = mean_response + t),
                    position = position_dodge(.9), width = .2) +
      facet_wrap(~ `Number of UAVs`, nrow = 1)
  }, height = 300, 
     width = 1000)
}

shinyApp(ui = ui, server = server)
