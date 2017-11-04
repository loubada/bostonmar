library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library (CyCyFns)

ui <- fluidPage( theme = shinytheme('darkly'),
  #shinythemes::themeSelector(),`
  #push again`
  
  h1("Prep for the next Boston Marathon!"),
  
  br(),
  
  h4("You think you're ready for the Boston Marathon? Are you sure you're going to make your goal time? Checkout the splits you should aim for and good luck for the race!"),
  
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Demographics :"),
      numericInput(inputId = "age", label = h5("Age : "), value = 42),
      radioButtons(inputId = "gender", label = h5("Gender : "),
                   choices = list("Male" = 1, "Female" = 2, "Don't care!" = 3), 
                   selected = 3, inline = TRUE),
      selectInput(inputId = "country", label = h5("Country :"), 
                  choices = c("-", countries_u), 
                  selected = NULL),
      
      hr(),
      
      h4("Your times (min) :"),
      sliderInput(inputId = "pastT", label = h5("Past Time : "), min = 120, 
                  max = 520, value = 0),
      sliderInput(inputId = "goalT", label = h5("Goal Time : "), min = 120, 
                  max = 520, value = 0),
      
      hr(),
      
      actionButton(inputId = "lucky", label = "Feeling lucky?"),
      br(),
      br(),
      h6(textOutput("go")),

      uiOutput("sound")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Compare yourself to others", plotOutput("av_plot")), 
        tabPanel("Your goal time splits", tableOutput("av_table")), 
        tabPanel("Yes I am!", 
                 br(),
                 br(),
                 textOutput("fun"),
                 br(),
                 br(),
                 h6("Still feeling lucky? Click again!"))
        
      )
    )
  )
  

)
  
     

server <- function(input, output) {
   #Issue: not plotting the graph! Maybe the renderPlot function only works with plot functions and not ggplot functions
   output$av_plot <- renderPlot({
     data_to_plot <- demographics_filter(data_all, age = input$age, gender = input$gender, nationality = input$country)
     
     data_to_plot <- rbind(pasttime(input$pastT, data_to_plot), goaltime(input$goalT, data_to_plot),
                           top10percentmean(data_to_plot),bottom20percentmean(data_to_plot))
     
    ggplot(data = (data_to_plot),
                 aes(x = milestone_km, y = mean_time, color = Label)) +
       geom_point() +
       geom_smooth() +
       labs(x = "Distance run", y = "Time since departure")
   })
   
   output$av_table <- renderDataTable({
     # input$age
     # input$gender 
     # input$country
     # input$age
     # input$pastT
     # input$goalT
     
   })
   
   output$fun <- renderPrint({
     x <- cat("In 2017, an 84 year old runner participated in the race! She also crossed the finished line in 2015 and 2016!")
   })
   observeEvent(input$lucky, {
     output$go <- renderPrint({
       cat("Go to the 3rd tab!")
     })
     # still figuring out how to add sound
     output$sound <- renderUI( {tags$audio(src = "Yippy.mp3", type = "audio/mp3", autoplay = NA)})
   })
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

