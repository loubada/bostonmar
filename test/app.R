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
                     numericInput(inputId = "Age", label = h5("Age : "), value = NULL, min = 18),
                     radioButtons(inputId = "Gender", label = h5("Gender : "),
                                  choices = list("Male", "Female", "Don't care!"), 
                                  selected = "Don't care!", inline = TRUE),
                     selectInput(inputId = "Country", label = h5("Country :"), 
                                 choices = c("-", countries_u), 
                                 selected = "-"),
                     
                     hr(),
                     
                     h4("Your times (min) :"),
                     sliderInput(inputId = "pastT", label = h5("Past Time : "), min = 120, 
                                 max = 520, value = NULL),
                     sliderInput(inputId = "goalT", label = h5("Goal Time : "), min = 120, 
                                 max = 520, value = 210),
                     
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
                       tabPanel("Your goal time splits", tableOutput('av_table')), 
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
  #Issue: not plotting the graph! It does plot though if we give specific values, maybe an issue with the inputs
  
  
  Gender <- reactive({
    if (input$Gender == "Male"){
      "M"
    }
    else if (input$Gender == "Female"){
      "F"
    }
    else {
      NULL
    }
  })
  
  
  output$av_plot <- renderPlot({
    
    if (is.null(Gender())){
      if (input$Country == "-"){
        data_to_plot <- demographics_filter(data = data_all, age = input$Age)
        
        data_to_plot <- rbind(pasttime(input$pastT, data_to_plot), goaltime(input$goalT, data_to_plot),
                              top10percentmean(data_to_plot),bottom20percentmean(data_to_plot))
        
        ggplot(data = data_to_plot,
               aes(x = milestone_km, y = mean_time, color = Label)) +
          geom_point() +
          geom_smooth() +
          labs(x = "Distance run", y = "Time since departure") 
      }
      else {
        data_to_plot <- demographics_filter(data = data_all, age = input$Age, nationality = input$Country)
        
        data_to_plot <- rbind(pasttime(input$pastT, data_to_plot), goaltime(input$goalT, data_to_plot),
                              top10percentmean(data_to_plot),bottom20percentmean(data_to_plot))
        
        ggplot(data = data_to_plot,
               aes(x = milestone_km, y = mean_time, color = Label)) +
          geom_point() +
          geom_smooth() +
          labs(x = "Distance run", y = "Time since departure") 
      }
      
    }
    else {
      if (input$Country == "-"){
        data_to_plot <- demographics_filter(data = data_all, age = input$Age, gender = Gender())
        
        data_to_plot <- rbind(pasttime(input$pastT, data_to_plot), goaltime(input$goalT, data_to_plot),
                              top10percentmean(data_to_plot),bottom20percentmean(data_to_plot))
        
        ggplot(data = data_to_plot,
               aes(x = milestone_km, y = mean_time, color = Label)) +
          geom_point() +
          geom_smooth() +
          labs(x = "Distance run", y = "Time since departure") 
      }
      else {
        data_to_plot <- demographics_filter(data = data_all, age = input$Age, gender = Gender(), nationality = input$Country)
        
        data_to_plot <- rbind(pasttime(input$pastT, data_to_plot), goaltime(input$goalT, data_to_plot),
                              top10percentmean(data_to_plot),bottom20percentmean(data_to_plot))
        
        ggplot(data = data_to_plot,
               aes(x = milestone_km, y = mean_time, color = Label)) +
          geom_point() +
          geom_smooth() +
          labs(x = "Distance run", y = "Time since departure") 
      }
      
      
    }
    
    
  })
  
  
  
  
  ###############################################

  data_for_table <- reactive({
    if(is.null(Gender())){
      if(input$Country == "-"){
        dt <- demographics_filter(data = data_all, age = input$Age)
        dt <-  goaltime(input$goalT, dt)
        dt %>% filter(dt$Label == "Goal_time_0to15_mins_Faster") %>% select(mean_time, milestone_km)
      }
      else{
        dt <- demographics_filter(data = data_all, age = input$Age, nationality = input$Country)
        dt <-  goaltime(input$goalT, dt)
        dt %>% filter(dt$Label == "Goal_time_0to15_mins_Faster") %>% select(mean_time, milestone_km)
      }
      
    }
    else{
      if(input$Country == "-"){
        dt <- demographics_filter(data = data_all, age = input$Age, gender = Gender())
        dt <-  goaltime(input$goalT, dt)
        dt %>% filter(dt$Label == "Goal_time_0to15_mins_Faster") %>% select(mean_time, milestone_km)
      }
      else{
        dt <- demographics_filter(data = data_all, age = input$Age, gender = Gender(), nationality = input$Country)
        dt <-  goaltime(input$goalT, dt)
        dt %>% filter(dt$Label == "Goal_time_0to15_mins_Faster") %>% select(mean_time, milestone_km)
      }
      
    }
    
    
    })

  
  output$av_table <- renderTable(
    {data_for_table()}
   
  )
  
  
  ###############################################  
  
  output$fun <- renderPrint({
    x <- cat("In 2017, an 84 year old runner participated in the race! She also crossed the finished line in 2015 and 2016!")
  })
  observeEvent(input$lucky, {
    output$go <- renderPrint({
      cat("Go to the 3rd tab!")
    })
    output$sound <- renderUI( {tags$audio(src = "Yippy.mp3", type = "audio/mp3", autoplay = NA)})
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)