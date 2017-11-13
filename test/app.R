# LOUISE AND CYCY

library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library (CyCyFns)



########################################################################################################################################  

############################################################        UI      ############################################################ 

########################################################################################################################################



ui <- fluidPage(
  theme = shinytheme('darkly'),
  #shinythemes::themeSelector(),`
  #push again`
  
  h1("Prep for the next Boston Marathon!"),
  
  br(),
  
  h4(
    "You think you're ready for the Boston Marathon? Are you sure you're going to make your goal time? Checkout the splits you should aim for and good luck for the race!"
  ),
  
  br(),


###########################################################        SIDEBAR      ###########################################################

######################### Inputs #########################
    
  sidebarLayout(
    sidebarPanel(
      h4("Demographics :"),
      numericInput(
        inputId = "Age",
        label = h5("Age : "),
        value = NULL,
        min = 18
      ),
      radioButtons(
        inputId = "Gender",
        label = h5("Gender : "),
        choices = list("Male", "Female", "Don't care!"),
        selected = "Don't care!",
        inline = TRUE
      ),
      selectInput(
        inputId = "Country",
        label = h5("Country :"),
        choices = c("-", unique(levels(data_all$Country))),
        selected = "-"
      ),
      
      hr(),

      h4("Your times (min) :"),
      
      checkboxInput("question_past", "I have ran a marathon already"),
      conditionalPanel(
        condition = "input.question_past == true",
        sliderInput(
          inputId = "pastT",
          label = h5("Past Time : "),
          min = 120,
          max = 520,
          value = NULL
        )
      ),
     
      checkboxInput("question_goal", "I want to benchmark my time"),
      conditionalPanel(
        condition = "input.question_goal == true",
        sliderInput(
          inputId = "goalT",
          label = h5("Goal Time : "),
          min = 120,
          max = 520,
          value = NULL
        )
      ),
      

######################### Conditional Panel #########################      
      

      conditionalPanel(
        condition = c("input.question_goal == true", "input.goalT >= 390 "),
        h6 ("Warning: You have to finish in less than 6h30 if you want to receive your medal ")
      ),
      conditionalPanel(
        condition = c("input.goalT <= 123 ", "input.question_goal == true"),
        h6 ("Impressive: You would break the record on the marathon")
      ),
      conditionalPanel(
        condition = c("input.pastT >= input.goalT + 60", "input.question_goal == true", "input.question_past == true"),
        h4 ("Do you really think you can improve that much?")
      ),
      
      hr(),
      
      actionButton(inputId = "lucky", label = "Feeling lucky?"),
      br(),
      br(),
      h6(textOutput("go")),
      
      uiOutput("sound")
      
    ),


###########################################################        MAIN      ###########################################################

######################### Outputs #########################      
        

    mainPanel(tabsetPanel(
      tabPanel("Compare yourself to others", plotOutput("av_plot")),
      tabPanel("Your goal time splits", 
               tableOutput('av_table'),
               br(),
               h6("PS. The milestones are in kilometers and the goals are in minutes")),
      tabPanel(
        "Yes I am!",
        br(),
        br(),
        textOutput("fun"),
        br(),
        br(),
        h6("Still feeling lucky? Click again!")
      )
      
    ))
  )
  
  
)


########################################################################################################################################  

##########################################################        SERVER      ########################################################## 

########################################################################################################################################


server <- function(input, output) {
  #Issue: not plotting the graph! It does plot though if we give specific values, maybe an issue with the inputs
  
  
  Gender <- reactive({
    if (input$Gender == "Male") {
      "M"
    }
    else if (input$Gender == "Female") {
      "F"
    }
    else {
      NULL
    }
  })
  
###########################################################        PLOT      ###########################################################
    
  output$av_plot <- renderPlot({
    if (is.null(Gender())) {
      if (input$Country == "-") {
        if (is.numeric(input$Age)) {
          if (input$question_past == FALSE) {
            if (input$question_goal == FALSE) {
          
##################### Inputs : AGE ########################
          
          data_to_plot <-
            demographics_filter(data = data_all, age = input$Age)
          
          data_to_plot <-
            rbind(
              top10percentmean(data_to_plot),
              bottom20percentmean(data_to_plot)
            )
          
          ggplot(data = data_to_plot,
                 aes(
                   x = milestone_km,
                   y = mean_time,
                   color = Label
                 )) +
            geom_point() +
            geom_smooth() +
            labs(x = "Distance run", y = "Time since departure")
              }
              else{
          
##################### Inputs : AGE + GOAL TIME ########################
          
          data_to_plot <-
              demographics_filter(data = data_all, age = input$Age)
          
          data_to_plot <-
            rbind(
              goaltime(input$goalT, data_to_plot),
              top10percentmean(data_to_plot),
              bottom20percentmean(data_to_plot)
            )
          
          ggplot(data = data_to_plot,
                 aes(
                   x = milestone_km,
                   y = mean_time,
                   color = Label
                 )) +
            geom_point() +
            geom_smooth() +
            labs(x = "Distance run", y = "Time since departure")
              }
        
            }
          else {
          if (input$question_goal == FALSE) {
          
          ##################### Inputs : AGE + PAST TIME ########################
          
          data_to_plot <-
            demographics_filter(data = data_all, age = input$Age)
          
          data_to_plot <-
            rbind(
              pasttime(input$pastT, data_to_plot),
              top10percentmean(data_to_plot),
              bottom20percentmean(data_to_plot)
            )
          
          ggplot(data = data_to_plot,
                 aes(
                   x = milestone_km,
                   y = mean_time,
                   color = Label
                 )) +
            geom_point() +
            geom_smooth() +
            labs(x = "Distance run", y = "Time since departure")
        }
          else{
          
          ##################### Inputs : AGE + GOAL TIME + PAST TIME ########################
          
          data_to_plot <- demographics_filter(data = data_all, age = input$Age)
          
          data_to_plot <-
            rbind(
              pasttime(input$pastT, data_to_plot),
              goaltime(input$goalT, data_to_plot),
              top10percentmean(data_to_plot),
              bottom20percentmean(data_to_plot)
            )
          
          ggplot(data = data_to_plot,
                 aes(
                   x = milestone_km,
                   y = mean_time,
                   color = Label
                 )) +
            geom_point() +
            geom_smooth() +
            labs(x = "Distance run", y = "Time since departure")
        }
        
          }
        }
        else{
            if (input$question_past == FALSE) {
              if (input$question_goal == FALSE) {
                
                ##################### Inputs : NOTHING ########################
                
                data_to_plot <-
                  demographics_filter(data = data_all)
                
                data_to_plot <-
                  rbind(
                    top10percentmean(data_to_plot),
                    bottom20percentmean(data_to_plot)
                  )
                
                ggplot(data = data_to_plot,
                       aes(
                         x = milestone_km,
                         y = mean_time,
                         color = Label
                       )) +
                  geom_point() +
                  geom_smooth() +
                  labs(x = "Distance run", y = "Time since departure")
              }
              else{
                
                ##################### Inputs : GOAL TIME ########################
                
                data_to_plot <- data_all
                
                data_to_plot <-
                  rbind(
                    goaltime(input$goalT, data_to_plot),
                    top10percentmean(data_to_plot),
                    bottom20percentmean(data_to_plot)
                  )
                
                ggplot(data = data_to_plot,
                       aes(
                         x = milestone_km,
                         y = mean_time,
                         color = Label
                       )) +
                  geom_point() +
                  geom_smooth() +
                  labs(x = "Distance run", y = "Time since departure")
              }
            }
            else {
              if (input$question_goal == FALSE) {
                ##################### Inputs : PAST TIME ########################
                
                data_to_plot <-
                  demographics_filter(data = data_all)
                
                data_to_plot <-
                  rbind(
                    pasttime(input$pastT, data_to_plot),
                    top10percentmean(data_to_plot),
                    bottom20percentmean(data_to_plot)
                  )
                
                ggplot(data = data_to_plot,
                       aes(
                         x = milestone_km,
                         y = mean_time,
                         color = Label
                       )) +
                  geom_point() +
                  geom_smooth() +
                  labs(x = "Distance run", y = "Time since departure")
              }
              else{
                ##################### Inputs : GOAL TIME + PAST TIME ########################
                
                data_to_plot <- data_all
                
                data_to_plot <-
                  rbind(
                    pasttime(input$pastT, data_to_plot),
                    goaltime(input$goalT, data_to_plot),
                    top10percentmean(data_to_plot),
                    bottom20percentmean(data_to_plot)
                  )
                
                ggplot(data = data_to_plot,
                       aes(
                         x = milestone_km,
                         y = mean_time,
                         color = Label
                       )) +
                  geom_point() +
                  geom_smooth() +
                  labs(x = "Distance run", y = "Time since departure")
              }
              
            }
          }
      }
      else {
        if (is.numeric(input$Age)) {
          if (input$question_past == FALSE) {
            if (input$question_goal == FALSE) {
              
              ##################### Inputs : COUNTRY + AGE ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, age = input$Age, nationality = input$Country)
              
              data_to_plot <-
                rbind(
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            else{
              
              ##################### Inputs : COUNTRY + AGE + GOAL TIME ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, age = input$Age, nationality = input$Country)
              
              data_to_plot <-
                rbind(
                  goaltime(input$goalT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            
          }
          else {
            if (input$question_goal == FALSE) {
              
              ##################### Inputs : COUNTRY + AGE + PAST TIME ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, age = input$Age, nationality = input$Country)
              
              data_to_plot <-
                rbind(
                  pasttime(input$pastT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            else{
              
              ##################### Inputs : COUNTRY + AGE + GOAL TIME + PAST TIME ########################
              
              data_to_plot <- demographics_filter(data = data_all, age = input$Age, nationality = input$Country)
              
              data_to_plot <-
                rbind(
                  pasttime(input$pastT, data_to_plot),
                  goaltime(input$goalT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            
          }
        }
        else{
          if (input$question_past == FALSE) {
            if (input$question_goal == FALSE) {
              
              ##################### Inputs : COUNTRY ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, nationality = input$Country)
              
              data_to_plot <-
                rbind(
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            else{
              
              ##################### Inputs : COUNTRY + GOAL TIME ########################
              
              data_to_plot <- 
                demographics_filter(data = data_all, nationality = input$Country)
              
              data_to_plot <-
                rbind(
                  goaltime(input$goalT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
          }
          else {
            if (input$question_goal == FALSE) {
              ##################### Inputs : COUNTRY + PAST TIME ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, nationality = input$Country)
              
              data_to_plot <-
                rbind(
                  pasttime(input$pastT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            else{
              ##################### Inputs : COUNTRY + GOAL TIME + PAST TIME ########################
              
              data_to_plot <- 
                demographics_filter(data = data_all, nationality = input$Country)
              
              data_to_plot <-
                rbind(
                  pasttime(input$pastT, data_to_plot),
                  goaltime(input$goalT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            
          }
        }
      }
    }  
    else {
      if (input$Country == "-") {
        if (is.numeric(input$Age)) {
          if (input$question_past == FALSE) {
            if (input$question_goal == FALSE) {
              
              ##################### Inputs : GENDER + AGE ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, age = input$Age, gender = Gender())
              
              data_to_plot <-
                rbind(
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            else{
              
              ##################### Inputs : GENDER + AGE + GOAL TIME ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, age = input$Age, gender = Gender())
              
              data_to_plot <-
                rbind(
                  goaltime(input$goalT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            
          }
          else {
            if (input$question_goal == FALSE) {
              
              ##################### Inputs : GENDER + AGE + PAST TIME ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, age = input$Age, gender = Gender())
              
              data_to_plot <-
                rbind(
                  pasttime(input$pastT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            else{
              
              ##################### Inputs : GENDER + AGE + GOAL TIME + PAST TIME ########################
              
              data_to_plot <- 
                demographics_filter(data = data_all, age = input$Age, gender = Gender())
              
              data_to_plot <-
                rbind(
                  pasttime(input$pastT, data_to_plot),
                  goaltime(input$goalT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            
          }
        }
        else{
          if (input$question_past == FALSE) {
            if (input$question_goal == FALSE) {
              
              ##################### Inputs : GENDER ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, gender = Gender())
              
              data_to_plot <-
                rbind(
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            else{
              
              ##################### Inputs : GENDER + GOAL TIME ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, gender = Gender())
              
              data_to_plot <-
                rbind(
                  goaltime(input$goalT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
          }
          else {
            if (input$question_goal == FALSE) {
              ##################### Inputs : GENDER + PAST TIME ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, gender = Gender())
              
              data_to_plot <-
                rbind(
                  pasttime(input$pastT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            else{
              ##################### Inputs : GENDER + GOAL TIME + PAST TIME ########################
              
              data_to_plot <- 
                demographics_filter(data = data_all, gender = Gender())
              
              data_to_plot <-
                rbind(
                  pasttime(input$pastT, data_to_plot),
                  goaltime(input$goalT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            
          }
        }
      }
      else {
        if (is.numeric(input$Age)) {
          if (input$question_past == FALSE) {
            if (input$question_goal == FALSE) {
              
              ##################### Inputs : GENDER + COUNTRY + AGE ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, age = input$Age, nationality = input$Country, gender = Gender())
              
              data_to_plot <-
                rbind(
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            else{
              
              ##################### Inputs : GENDER + COUNTRY + AGE + GOAL TIME ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, age = input$Age, nationality = input$Country, gender = Gender())
              
              data_to_plot <-
                rbind(
                  goaltime(input$goalT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            
          }
          else {
            if (input$question_goal == FALSE) {
              
              ##################### Inputs : GENDER + COUNTRY + AGE + PAST TIME ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, age = input$Age, nationality = input$Country, gender = Gender())
              
              data_to_plot <-
                rbind(
                  pasttime(input$pastT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            else{
              
              ##################### Inputs : GENDER + COUNTRY + AGE + GOAL TIME + PAST TIME ########################
              
              data_to_plot <- 
                demographics_filter(data = data_all, age = input$Age, nationality = input$Country, gender = Gender())
              
              data_to_plot <-
                rbind(
                  pasttime(input$pastT, data_to_plot),
                  goaltime(input$goalT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            
          }
        }
        else{
          if (input$question_past == FALSE) {
            if (input$question_goal == FALSE) {
              
              ##################### Inputs : GENDER + COUNTRY ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, nationality = input$Country, gender = Gender())
              
              data_to_plot <-
                rbind(
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            else{
              
              ##################### Inputs : GENDER + COUNTRY + GOAL TIME ########################
              
              data_to_plot <- 
                demographics_filter(data = data_all, nationality = input$Country, gender = Gender())
              
              data_to_plot <-
                rbind(
                  goaltime(input$goalT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
          }
          else {
            if (input$question_goal == FALSE) {
              ##################### Inputs : GENDER + COUNTRY + PAST TIME ########################
              
              data_to_plot <-
                demographics_filter(data = data_all, nationality = input$Country, gender = Gender())
              
              data_to_plot <-
                rbind(
                  pasttime(input$pastT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            else{
              ##################### Inputs : GENDER + COUNTRY + GOAL TIME + PAST TIME ########################
              
              data_to_plot <- 
                demographics_filter(data = data_all, nationality = input$Country, gender = Gender())
              
              data_to_plot <-
                rbind(
                  pasttime(input$pastT, data_to_plot),
                  goaltime(input$goalT, data_to_plot),
                  top10percentmean(data_to_plot),
                  bottom20percentmean(data_to_plot)
                )
              
              ggplot(data = data_to_plot,
                     aes(
                       x = milestone_km,
                       y = mean_time,
                       color = Label
                     )) +
                geom_point() +
                geom_smooth() +
                labs(x = "Distance run", y = "Time since departure")
            }
            
          }
        }
      }
    } 
    
    
  })
  
  
  
##########################################################        TABLE      ##########################################################
  
  data_for_table <- reactive({
    if (is.null(Gender())) {
      if (input$Country == "-") {
        if (is.numeric(input$Age)){
          
##################### Inputs : AGE ########################        
          
          dt <- demographics_filter(data = data_all, age = input$Age)
          dt <-  goaltime(input$goalT, dt)
          dt %>% filter(dt$Label == "Goal_time_0to15_mins_Faster") %>% select(milestone_km, mean_time) %>% rename(Milestone = milestone_km, Goal = mean_time)
        }
        else{
          
##################### Inputs : NULL #######################
          
          dt <- data_all
          dt <-  goaltime(input$goalT, dt)
          dt %>% filter(dt$Label == "Goal_time_0to15_mins_Faster") %>% select(milestone_km, mean_time) %>% rename(Milestone = milestone_km, Goal = mean_time)
        }
        
      }
      else{
        if (is.numeric(input$Age)){
          
################# Inputs : AGE + COUNTRY ##################
        
          dt <-
            demographics_filter(
              data = data_all,
              age = input$Age,
              nationality = input$Country
            )
          dt <-  goaltime(input$goalT, dt)
          dt %>% filter(dt$Label == "Goal_time_0to15_mins_Faster") %>% select(milestone_km, mean_time) %>% rename(Milestone = milestone_km, Goal = mean_time)
        }
        else{
          
################### Inputs : COUNTRY ######################
          
          dt <-
            demographics_filter(
              data = data_all,
              nationality = input$Country
            )
          dt <-  goaltime(input$goalT, dt)
          dt %>% filter(dt$Label == "Goal_time_0to15_mins_Faster") %>% select(milestone_km, mean_time) %>% rename(Milestone = milestone_km, Goal = mean_time)
        }
        
      }
      
    }
    else{
      if (input$Country == "-") {
        if (is.numeric(input$Age)){
          
################## Inputs : AGE + GENDER #####################
          
          dt <-
            demographics_filter(data = data_all,
                                age = input$Age,
                                gender = Gender())
          dt <-  goaltime(input$goalT, dt)
          dt %>% filter(dt$Label == "Goal_time_0to15_mins_Faster") %>% select(milestone_km, mean_time) %>% rename(Milestone = milestone_km, Goal = mean_time)
        }
        else{
          
#################### Inputs : GENDER #########################
          
          dt <-
            demographics_filter(data = data_all,
                                gender = Gender())
          dt <-  goaltime(input$goalT, dt)
          dt %>% filter(dt$Label == "Goal_time_0to15_mins_Faster") %>% select(milestone_km, mean_time) %>% rename(Milestone = milestone_km, Goal = mean_time)
        }
        
      }
      else{
        if (is.numeric(input$Age)){
          
#########@## Inputs : AGE + GENDER + COUNTRY ###############          
          
          dt <-
            demographics_filter(
              data = data_all,
              age = input$Age,
              gender = Gender(),
              nationality = input$Country
            )
          dt <-  goaltime(input$goalT, dt)
          dt %>% filter(dt$Label == "Goal_time_0to15_mins_Faster") %>% select(milestone_km, mean_time) %>% rename(Milestone = milestone_km, Goal = mean_time)
        }
        else{
          
############### Inputs : COUNTRY + GENDER ##################
          
          dt <-
            demographics_filter(
              data = data_all,
              gender = Gender(),
              nationality = input$Country
            )
          dt <-  goaltime(input$goalT, dt)
          dt %>% filter(dt$Label == "Goal_time_0to15_mins_Faster") %>% select(milestone_km, mean_time) %>% rename(Milestone = milestone_km, Goal = mean_time)
        }
        
      }
      
    }
    
    
  })
  
  
  output$av_table <- renderTable({
    data_for_table()
  })
  
  
##########################################################        LUCKY      ##########################################################
  
  x <- c("In 2017, an 84 year old runner participated in the race! She also crossed the finished line in 2015 and 2016!",
         "The faster runner in 2015 completed the Boston marathon in 129.2833 min!",
         "The faster runner in 2016 completed the Boston marathon in 132.75 min!",
         "The faster runner in 2017 completed the Boston marathon in 129.6167 min!",
         "On average, women take 20.7 minutes more to complete the marathon",
         "The mode age is 45. The Boston Marathon is for people going through a mid-life crisis, huh?",
         "In 2016, it took a man more than 8 hours (505.15 min) to complete the race. All that matters is crossing that finish line!"
         
         
    
  )
  
  
  observeEvent(input$lucky, {
    output$go <- renderPrint({
      cat("Go to the 3rd tab!")
    })
    output$sound <-
      renderUI({
        tags$audio(src = "Yippy.mp3",
                   type = "audio/mp3",
                   autoplay = NA)
      })
    rand <- sample(1:length(x), 1)
    output$fun <- renderPrint({
      cat(x[rand]
        )
    })
  })
  
  
  
}

########################################################################################################################################  

#########################################################        RUN APP      ########################################################## 

########################################################################################################################################


shinyApp(ui = ui, server = server)

