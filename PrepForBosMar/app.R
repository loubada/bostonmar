library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)


ui <- fluidPage( theme = shinytheme('darkly'),
  #shinythemes::themeSelector(),
  
  h1("Prep for the next Boston Marathon!"),
  
  br(),
  
  h4("You think you're ready for the Boston Marathon? Are you sure you're going to make your goal time? Checkout the splits you should aim for and good luck for the race!"),
  
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Demographics :"),
      numericInput("age", label = h5("Age : "), value = 42),
      radioButtons("gender", label = h5("Gender : "),
                   choices = list("Male" = 1, "Female" = 2, "Don't care!" = 3), 
                   selected = 3),
      selectInput("country", label = h5("Country :"), 
                  choices = c("-", countries_u), 
                  selected = NULL),
      
      hr(),
      
      h4("Your times (min) :"),
      sliderInput("pastT", label = h5("Past Time : "), min = 120, 
                  max = 520, value = 0),
      sliderInput("goalT", label = h5("Goal Time : "), min = 120, 
                  max = 520, value = 0),
      
      hr(),
      
      actionButton("lucky", label = "Feeling lucky?")

      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Compare yourself to others", plotOutput("av_plot")), 
        tabPanel("Your goal time splits", tableOutput("av_table")), 
        tabPanel("Yes I am!", 
                 br(),
                 br(),
                 TextOutput("fun"))
      )
    )
  )
  

)
  
     

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$av_plot <- renderPlot({
     # input$age
     # input$gender 
     # input$country
     # input$age
    
   })
   
   output$av_table <- renderDataTable({
     # input$age
     # input$gender 
     # input$country
     # input$age
     
   })
   
   output$fun <- renderPrint(
     {
       cat("In 2017, an 84 runner participated to the race! She also crossed the finished line in 2015 and 2016!")
       }
)
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

