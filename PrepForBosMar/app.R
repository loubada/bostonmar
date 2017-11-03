library(shiny)
library(ggplot2)
library(shinythemes)


ui <- fluidPage( theme = shinytheme('darkly'),
  #shinythemes::themeSelector(),
  
  h1("Prep for the next Boston Marathon!"),
  
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput("age", label = h4("Age : "), value = 42),
      radioButtons("gender", label = h3("Gender : "),
                   choices = list("Male" = 1, "Female" = 2, "N/A" = 3), 
                   selected = 3),
      selectInput("country", label = h3("Country :"), 
                  choices = c("-", "hi", "bye"), 
                  selected = NULL),
      
      br(),
      
      h3("Your times (min) :"),
      sliderInput("pastT", label = h5("Past Time : "), min = 120, 
                  max = 520, value = 0),
      sliderInput("goalT", label = h5("Goal Time : "), min = 120, 
                  max = 520, value = 0)

      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Compare yourself to others", plotOutput("plot")), 
        tabPanel("Your goal time splits", tableOutput("table")), 
        tabPanel("Yes I am!", verbatimTextOutput("fun"))
      )
    )
  )
  

)
  
     

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

