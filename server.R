library(RCurl)
library(foreign)
library(ResourceSelection)
library(pander)
library(tidyverse)

shinyServer(function(input, output) {
  #Retrieve .csv file from github
  GSSc <- "https://raw.githubusercontent.com/dylanjm/PartyID-Dataset/master/Data/PartyID.csv" %>%
    getURL %>% textConnection %>% read.csv %>% 
    mutate_at(c("sex","race","degree","attend"), as.factor)
  
  #Run Logistic Regression
  party.glm <-glm(partyid ~  ., data = GSSc, family = binomial)
  hoslem <- hoslem.test(party.glm$y, party.glm$fitted)
  summary <- summary(party.glm)
  
  #Create Users Profile
  PredictiveData <- reactive({
    data.frame(
      age = input$age, sex = input$sex,
      race = input$race, degree = input$degree, attend = input$chruch
    )
  })
  
  #Predict User's PARTYID
  partyPredict <- eventReactive(input$submit, {
    newdata <- PredictiveData() %>%
      mutate_at(c("sex","race","degree","attend"), as.factor)
    
    predict(party.glm, newdata, type = "response")
  })
  
  #Generate a probability
  output$answer <- renderPrint({
    cat("Probability of Affiliating with the Republican Party: ")
    answer <- round(partyPredict() * 100, 2)
    cat(paste0(answer,"%"))
  })
  
  # Automate output of presentation
  lapply(1:9, function(i) {
    output[[paste0('slide', i)]] <- renderUI({
      img(src = paste0('Slide',i,'.png'))
    })
  })
  
  #Info on the logistic model
  output$model <- renderPrint({
    print(summary)
    print(hoslem)
  })
  
  # Autmoate Graphics Function
  GssPlotFunction <- function(v, t){
    print(ggplot(data = GSSc, aes(x = v, fill = partyid)) + 
      geom_bar(stat = "count", position = "stack",size = .3) + 
      guides(fill = F) + scale_fill_manual(values = c("#2d5986", "#cc0000")) + 
      xlab("") + ylab("Particpants") + ggtitle(t))
  }
  
  # Titles of plots
  title <- c("Democrat or Republican","Age of Participants","Sex of Participants",
             "Race of Participants","Degree of Participants","Church Attendance of Participants")
  
  # Output the Plots
  lapply(1:5, function(i){
    output[[paste0('plot',i)]] <- renderPlot({
      GssPlotFunction(GSSc[,i], title[[i]])
    })
  })
})
