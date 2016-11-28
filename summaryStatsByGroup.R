
# an app to create summary statistics by factor
groupSummaryStats <- function(z){

    if(missing(z))
    {
      library(datasets)
      z <- mtcars
      
    }
    
    # check for installation of required packages
    list.of.packages <- c("psych")
    new.packages.1 <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if( length(new.packages.1) ) 
    {
      install.packages(new.packages.1, quiet = TRUE)
    }
    
    # load required packages
    library(datasets)
    library(psych)
    
    # convert any character variables to factors
    z[sapply(z, is.character)] <- lapply(z[sapply(z, is.character)], as.factor)
    
    # scan for binary variables
    check.3 <- apply(z,2,function(x) { all(na.omit(x) %in% 0:1) })
    
    # if there are any binary variables, convert them to factors
    if( any( check.3 ) )
    {
      z[ check.3 ] <- lapply(z[ check.3 ], as.factor)
    }
    
    check.2 <- sapply(z, is.factor) # scan for factor data
 
    # if there are no factors and the row names are characters, then use the row names as factor column
    if( !any( check.2 ) ) 
        { if( is.character(row.names(z)) )
          {
           z <- cbind(factor = as.factor(row.names(z)), z)
          } else
          {
          stop( "Can't find any factor variables to group on")
          }}
    
    check.2 <- sapply(z, is.factor) # scan for factor data
 
    vars.factor <- names(z[ check.2 ]) # list of factor variable names
    check.1 <- sapply(z, is.numeric) # scan for numeric data
    server <- function(input, output)
      {
      
      output$selector <- renderUI({
        selectInput(choices = vars.factor, label = "Select group variable", inputId = "selection")
        
      })
      
      output$table <- renderDataTable({
        if(!is.null(input$selection))
          {
          
          group.1 <- z[ , input$selection ]
          results.table <- describeBy(x = z[ check.1 ], group = group.1)
          results.df <- do.call(rbind.data.frame, results.table)
          results.df <- cbind( variable = as.vector(sapply(results.table, row.names)), results.df )
          results.order <- order(results.df[, c(1)], results.df[, c(2)])
          results.df <- results.df[ results.order, ]
          
          return(results.df)
          }
      },  options = list(orderClasses = TRUE))

    }
 
    ui <- fluidPage(
      sidebarLayout(
        sidebarPanel(
          uiOutput("selector")
        ),
        mainPanel(dataTableOutput('table'))
      )
    )
    
    shinyApp(ui = ui, server = server)
}
