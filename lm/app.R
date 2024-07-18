library(shiny)
library(tidyverse)
library(reshape2)
library(ggpubr)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Linear Model Shiny App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # Horizontal line ----
      
      tags$hr(),
      actionButton("go","Plot Linear Model"),
      actionButton("compare","Compare the Difference"),
      actionButton("reset","Reset the Graph")
      
      
    ),
    
    
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("lmPlot"),
      tableOutput("contents")
    )
  )
)



# Define server logic required to draw a histogram
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  lmdata <- reactiveValues()
  
  dataInput <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    return(df)
  })
  
  observeEvent(input$go,{
    update_lm()
  }
  )
  
  observeEvent(input$reset,{
    update_reset()
  })
  
  observeEvent(input$compare,{
    update_compare()
  })
  
  
  update_compare <- function(){
    df <- dataInput()
    model <- lm(y~x,data=df)
    df$fitted <- model$fitted.values
    names(df) <- c("original","test_x","predicted")
    dfm <- melt(df,id.var="test_x",variable.name="y_type",value.name = "y_value")
    output$distPlot <- renderPlot({
      plot1 <- ggplot(dfm, aes(x = test_x, y =y_value , shape = y_type, color = y_type))+
        geom_point()+
        geom_abline(slope = s,intercept = i,colour="yellow")+
        scale_color_manual(values = c("original" = '#ff00ff','predicted' = '#3399ff')) + 
        scale_shape_manual(values = c('original' = 17, 'predicted' = 16))+
        ggtitle("Scatter Plot and Linear Model") +
        xlab("x value")+
        ylab("y value")+
        theme_classic()
      plot1
    })
    
  }
  
  update_reset <- function(){
    output$distPlot <- renderPlot({
      plot1 <- ggplot(mapping = aes(x=dataInput()$x,y=dataInput()$y)) +
        geom_point(colour="black",size=1) +
        xlab("x") +
        ylab("y") +
        ggtitle("Scatter plot on orginal data")+
        theme_classic()
      plot1
    })
  }
  
  
  update_lm <- function(){
    model <- lm(y~x,data=dataInput())
    s <- coefficients(model)[2]
    i <- coefficients(model)[1]
    output$distPlot <- renderPlot({
      ggplot(data=dataInput(),mapping = aes(x=dataInput()$x,y=dataInput()$y))+
        geom_point(colour="red",size=1)+
        geom_abline(slope = s,intercept=i,color="blue")+
        ggtitle("Regression Line on Original Model") +
        xlab("x value")+
        ylab("y value")+
        stat_regline_equation(label.x = 1, label.y = 10)+
        stat_cor(label.x = 1, label.y = 11) +
        theme_classic()
    })
  }
  
  
  
  output$distPlot <- renderPlot({
    plot1 <- ggplot(mapping = aes(x=dataInput()$x,y=dataInput()$y)) +
      geom_point(colour="black",size=1) +
      xlab("x") +
      ylab("y") +
      ggtitle("Scatter plot on orginal data")+
      theme_classic()
    plot1
  })
  
  
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    
    if(input$disp == "head") {
      return(head(dataInput()))
    }
    else {
      return(dataInput())
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
