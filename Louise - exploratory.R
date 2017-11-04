library(dplyr)
library(ggplot2)

d2015<-data_2015 %>% select(-X, -X.1) %>% mutate(Year = 2015)
d2016<-data_2016 %>% select(-X) %>% mutate(Year = 2016)
d2017<-data_2017 %>% select(-X, -X.1) %>% mutate(Year = 2017)


#2015 
summary(d2015)
q1_2015 <- quantile(d2015$Age, 0.25)
q2_2015 <- quantile(d2015$Age, 0.5)
q3_2015 <- quantile(d2015$Age, 0.75)

youngest_2015 <- d2015 %>% filter(Age <= 33) 
younger_2015 <- d2015 %>% filter(Age <= 33) 

#2016 
summary(d2016)
q1_2016 <- quantile(d2016$Age, 0.25)
q2_2016 <- quantile(d2016$Age, 0.5)
q3_2016 <- quantile(d2016$Age, 0.75)

youngest_2016 <- d2016 %>% filter(Age <= q1_2016) 

#2017 
summary(d2015)
q1_2017 <- quantile(d2017$Age, 0.25)
q2_2017 <- quantile(d2017$Age, 0.5)
q3_2017 <- quantile(d2017$Age, 0.75)

youngest_2017<- d2017 %>% filter(Age <= q1_2017)

#combination
#youngest <- combine(youngest_2015, youngest_2016, youngest_2017)
youngest2 <- bind_rows(youngest_2015, youngest_2016, youngest_2017)
summary(yousummngest)


#test
all <- bind_rows(d2015, d2016, d2017)
dim(d2015 %>% filter(Country == "FRA"))
dim(d2016 %>% filter(Country == "FRA"))
dim(d2017 %>% filter(Country == "FRA"))


ggplot(data = all,
       aes(x = Age)) + geom_bar()

mean(all$Age)
median(all$Age)

countries_mix <- c(levels(d2015$Country), levels(d2016$Country), levels(d2017$Country))
countries_u <- unique(countries_mix)
class(levels(d2015$Country))

devtools::install_github("ruoyzhang/FunctionsForCyCy")

server <- function(input, output) {
  
  #################################
  if (input$Gender == "Don't care!"){   
    
    output$av_plot <- renderPlot({
      
      
      data_to_plot <- demographics_filter(data = data_all, age = input$Age, nationality = input$Country)
      
      data_to_plot <- rbind(pasttime(input$pastT, data_to_plot), goaltime(input$goalT, data_to_plot),
                            top10percentmean(data_to_plot),bottom20percentmean(data_to_plot))
      
      ggplot(data = data_to_plot,
             aes(x = milestone_km, y = mean_time, color = Label)) +
        geom_point() +
        geom_smooth() +
        labs(x = "Distance run", y = "Time since departure")
      
    })
    
    output$av_table <- renderDataTable({
      
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
    })}
  
  
  ####################################@
  
  
  else{
    
    Gender <- reactive({
      if (input$Gender == "Male"){
        "M"
      }
      else {
        "F"
      }
    })
    
    
    output$av_plot <- renderPlot({
      
      
      data_to_plot <- demographics_filter(data_all, input$Age, input$Gender, input$Country)
      
      data_to_plot <- rbind(pasttime(input$pastT, data_to_plot), goaltime(input$goalT, data_to_plot),
                            top10percentmean(data_to_plot),bottom20percentmean(data_to_plot))
      
      ggplot(data = data_to_plot,
             aes(x = milestone_km, y = mean_time, color = Label)) +
        geom_point() +
        geom_smooth() +
        labs(x = "Distance run", y = "Time since departure")
      
    })
    
    output$av_table <- renderDataTable({
      
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
  
  
  
}
