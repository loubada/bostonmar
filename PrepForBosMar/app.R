library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Prep for the next Boston Marathon!"),
  
  sidebarLayout(
    
    sidebarPanel(
      # Inputs 
      numericInput("Age", label = h3("Age : "), value = 42),
      radioButtons("Gender", label = h3("Gender : "),
                   choices = list("Male" = 1, "Female" = 2), 
                   selected = NULL),
      selectInput("Country", label = h3("Country :"), 
                  choices = c("-", countries_u), 
                  selected = NULL)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Compare yourself to others", plotOutput("plot")), 
        tabPanel("Your goal time splits", verbatimTextOutput("summary")), 
        tabPanel("Yes I do!", tableOutput("table"))
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

