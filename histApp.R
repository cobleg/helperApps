
## An app to create an interactive histogram
# now tested

histApp <- function(x) {
  if(missing(x))
    {

     library(datasets)
      x <- iris
    }
  server <- function(input, output) {

    check.1 <- sapply(x, is.numeric) # scan for numeric data
    check.2 <- sapply(x, is.factor) # scan for factor data

    vars.numeric <- names(x[ check.1 ])

  output$selector <- renderUI({
    selectInput(choices = vars.numeric, label = "Select variable", inputId = "selection")

  })

  output$distPlot <- renderPlot({
    if(!is.null(input$selection)){

      hist(x[,input$selection], col = 'darkgray', border = 'white', main = input$selection, xlab = NULL, breaks = input$bins)
    }
  })

}

  # ui.R
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(

        uiOutput("selector"),

        sliderInput("bins", "Select number of bins", min = 10, max = 100, value = 20)
      ),

      mainPanel(plotOutput("distPlot"))
    )
  )

  shinyApp(ui = ui, server = server)

  }

