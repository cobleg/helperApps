
# An app to create scatter plots
scatterApp <- function(z) {
  if(missing(z))
  {
    library(datasets)
    z <- mtcars
  }
  
  # convert any character variables to factors
  z[sapply(z, is.character)] <- lapply(z[sapply(z, is.character)], as.factor)
  
  # scan for binary variables
  check.binary <- apply(z,2,function(x) { all(na.omit(x) %in% 0:1) })
  
  # if there are any binary variables, convert them to factors
  if( any(check.binary) )
  {
    z[ check.binary ] <- lapply(z[ check.binary ], as.factor)
  }
  
  check.factor <- sapply(z, is.factor) # scan for factor data
  
  if( !any( check.factor ) )
  {
    # if there are no factors and the row names are characters, then use the row names as factor column
    if( is.character(row.names(z)) )
    {
      z <- cbind(factor = as.factor(row.names(z)), z)
    }else
    {
      stop( "Can't find any factor variables to group on")
    }
  }
  
  server <- function(input, output){
    
    # check for installation of required packages
    list.of.packages <- c("RColorBrewer")
    new.packages.1 <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages.1))
      {
      install.packages(new.packages.1)
      }
    
    # load required packages
    library(RColorBrewer)
    library(datasets)
    
    # check for numeric data
    check.1 <- sapply(z, is.numeric) # scan for numeric data
    check.2 <- sapply(z, is.factor) # scan for factor data
    
    vars.numeric <- names(z[ check.1 ])
    vars.factor <-  names(z[ check.2 ])
    
    # check for factor data
    if( length( vars.factor )  > 0 )
      {
      factors.num <- length( vars.factor )
      } else {
      factors.num <- 0
      }
    
    panel.pearson <- function(x, y, ...) 
      {
      horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
      vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
      text(horizontal, vertical, format(abs(cor(x,y)), digits=2)) 
      }
    
    output$selector <- renderUI({
      selectInput(choices = vars.factor, label = "Select factor variable", inputId = "selection")
      
    })
    
    output$scatterPlot <- renderPlot({
      
      if( !is.null( input$selection ) ){
      # create scatter plot matrix
      if( length(vars.factor) >= 1 )
       {
      if( ( nlevels( z[ ,input$selection ] ) ) == 2 )
      {
       
         pairs( z[ ,vars.numeric ], pch = 21, bg = c("red", "blue")[ unclass( z[ ,input$selection ] ) ],
               upper.panel=panel.pearson, oma = c(5, 4, 5, 20) )
        } else
          {
            # create color vector
            factor.colours <- data.frame( factor.1 = levels( z[ ,input$selection ] ), colour = I(brewer.pal((nlevels(z[, input$selection ])) , name = "Dark2" )) )

            pairs( z[ ,vars.numeric], main = " ", pch = 21, bg = factor.colours$colour[ match(z[ ,input$selection], factor.colours$factor.1) ],
                  upper.panel=panel.pearson, oma = c(5, 4, 5, 20))
            legend('right',
                   legend = as.character(factor.colours$factor.1),
                   col = factor.colours$colour, pch = par("pch"), bty = 'n', xjust = 1)
          }
        }
      }
      })
  }

  # ui.R
  ui <- fluidPage(
    fluidRow(
      
      column(4, 
        uiOutput("selector")
      ),
      
     column(8, plotOutput("scatterPlot"))
    )
  )
  
  shinyApp(ui = ui, server = server)
  
  }

