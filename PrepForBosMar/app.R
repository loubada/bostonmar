library(shiny)
library(ggplot2)
library(shinythemes)


ui <- fluidPage( theme = shinytheme('darkly'),
  #shinythemes::themeSelector(),
  
  titlePanel("Prep for the next Boston Marathon!"),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput("age", label = h4("Age : "), value = 42),
      radioButtons("gender", label = h3("Gender : "),
                   choices = list("Male" = 1, "Female" = 2, "N/A" = 3), 
                   selected = 3),
      selectInput("country", label = h3("Country :"), 
                  choices = c("-", countries_u), 
                  selected = NULL),
      tags$div( class="btn-group open",
        tags$a( href="#", class="btn btn-default"),
        tags$a( href="#", class="btn btn-default dropdown-toggle", 'data-toggle' ="dropdown", 'aria-expanded' ="true", tags$span( class="caret")),
        tags$ul( class="dropdown-menu",
        tags$li("Action"),
        tags$li("Another actio"))
        #<li><a href="#">Another action</a></li>
        #<li><a href="#">Something else here</a></li>
       # <li class="divider"></li>
        #<li><a href="#">Separated link</a></li>
        )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Compare yourself to others", plotOutput("plot")), 
        tabPanel("Your goal time splits", tableOutput("table")), 
        tabPanel("Yes I do!", verbatimTextOutput("fun"))
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

