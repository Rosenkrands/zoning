library(shiny)

ui <- fluidPage(
  titlePanel("Visualization of Results"),
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
                  choices = c("low", "medium", "high", "all"),
                  selected = c("low","medium","high"),
                  multiple = TRUE),
      selectInput(inputId = "number_of_uavs",
                  label="Choose number of UAVs",
                  choices = c("low", "medium", "high"),
                  selected = c("low", "medium", "high"),
                  multiple = TRUE)
    ),
    mainPanel(
      plotOutput("plot_utilization", width = "100%")
    )
  )
)

placeholder <- function() {
  ggplot() + geom_text(aes(x = 1, y = 1, label = "Placeholder")) + theme_void()
}

server <- function(input, output) {
  source('2d-instance.R')
  result <- readRDS("./solution-results.rds")
  theme_set(theme_bw(base_size=16))
  
  uav_height <- reactive({length(input$number_of_uavs)})
  
  output$plot_utilization <- renderPlot({
    result %>%
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
}

shinyApp(ui = ui, server = server)
