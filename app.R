library(shiny)

ui <- fluidPage(
  titlePanel("Solution Representation 3 (Grid approach with GA to select centroids)"),
  sidebarLayout(
    sidebarPanel(
      withMathJax(
        "As a benchmark for our future candidate centroid approaches, we have here just a grid of centroids with a certain dimension."),
      p("In this simple example we represent a solution as a bit string, for example $$ (1,0,0,0,0,0,0,1,0)$$ that corresponds to a 3 by 3 grid where centroids 1 and 8 is used."),
      p("Then for each demand point we assign it to the closest centroid point."),
      p("Different types of plots can be chosen for both the top and bottom plot, using the dropdowns below."),
      sliderInput(inputId = "no_of_points",
                  label = "Choose number of demand points",
                  value = 80, min = 20, max = 100),
      sliderInput(inputId = "dimension",
                  label = "Choose dimension of grid",
                  value = 7, min = 3, max = 10),
      selectInput(inputId = "type1",
                  label="Type of top plot",
                  choices = c("point", "centroid", "chosen", "group", "voronoi"),
                  selected = "centroid"),
      selectInput(inputId = "type2",
                  label="Type of bottom plot",
                  choices = c("point", "centroid", "chosen", "group", "voronoi"),
                  selected = "group")
    ),
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  )
)

server <- function(input, output) {
  source('2d-instance.R')
  set.seed(123)
  instance <- reactive({
    generate_2d_instance(
      no_of_points = input$no_of_points,
      interval = c("min" = -10, "max" = 10)
    )
  })
  centroids <- reactive({
    grid_centroids(instance(), dimension = input$dimension)
  })
  solution <- reactive({
    solve_ga(instance(), centroids())
  })
  output$plot1 <- renderPlot({
    plot_2d(instance(), centroids(), solution(), type = input$type1)
  })
  output$plot2 <- renderPlot({
    plot_2d(instance(), centroids(), solution(), type = input$type2)
  })
}

shinyApp(ui = ui, server = server)
