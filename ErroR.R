################################################################################
#Blank Slate
#Load Libraries

rm(list=ls())
gc()

if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}

if(!require(dplR)){
  install.packages("dplR")
  library(dplR)
}

if(!require(MASS)){
  install.packages("MASS")
  library(MASS)
}

if(!require(reshape2)){
  install.packages("reshape2")
  library(reshape2)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

## Only run examples in interactive R sessions
if (interactive()) {
  
################################################################################
#User interface to contain a sidebar and main panel
  ui <- fluidPage(
    tabsetPanel(
      tabPanel("Data Loading", fluid = TRUE,
        # Sidebar with file selection and chronology plot
          sidebarPanel(
            fileInput("file1", "Choose Chronology File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
            plotOutput("plot", brush = "plot_brush"),
          ),
          # Main panel with full RWI data frame
          mainPanel(
            # conditionalPanel(
            #   condition = "inFile.datapath == 'abc'",
            #   plotOutput("plot", brush = "plot_brush")
            # )
    
            tableOutput("RWI")
        )
      ),
      tabPanel("RWI Stats", fluid = TRUE,
               sidebarPanel(
                 sliderInput("WinLen", "Window Length (yrs)",min = 5,max = 200,value = 50
                 ),
                 actionButton("do", "Create Plot (may take up to 60 sec)")
               ),
               mainPanel(
                 plotOutput("plot2", brush = "plot_brush")
               )
      )
    )
  )
  
  server <- function(input, output) {
    
    rwiStatsPlot <- function(rwi1, winLen = 21){
      df1 <- rwi.stats.running(rwi1, window.length = winLen, window.overlap = (winLen-1))
      df2 <- melt(data.frame("Year" = df1$mid.year, "rbar" = df1$rbar.eff, "eps" = df1$eps), id.vars = "Year")
      p1 <- ggplot(data=df2, mapping = aes(x=Year, y=value, color=variable)) + geom_line()
      return(p1)
    }
    
    
    data1 <- reactive({
      
      inFile <- input$file1
      
      if(is.null(inFile)){
        return()
      }
      
      df <- read.rwl(inFile$datapath)
      df
    })
    
    
    
    output$RWI <- renderTable({

       if (is.null(data1()))
         return(NULL)

      data1()
    })
    
    output$plot <- renderPlot({

       if (is.null(data1()))
         return(NULL)
      plot(chron(data1()))
    }, res = 96)
    
    observeEvent(input$do, { 
      output$plot2 <- renderPlot({
        
        if (is.null(data1()))
          return(NULL)
        rwiStatsPlot(data1(), winLen = isolate(input$WinLen))
      }, res = 96)
      })
  }
  
  shinyApp(ui, server)
}