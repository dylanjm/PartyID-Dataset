library(RCurl)
library(foreign)
library(ResourceSelection)
library(pander)
library(ggplot2)

shinyServer(function(input, output) {
  #Retrieve .csv file from github
  url <-
    "https://raw.githubusercontent.com/dylanjm/PartyID-Dataset/master/Data/PartyID.csv"
  GSSc <- getURL(url)
  GSSc <- read.csv(textConnection(GSSc))
  #Make the variables factors
  GSSc$sex <- as.factor(GSSc$sex)
  GSSc$race <- as.factor(GSSc$race)
  GSSc$degree <- as.factor(GSSc$degree)
  GSSc$attend <- as.factor(GSSc$attend)
  #Run Logistic Regression
  party.glm <-
    glm(partyid ~  age + sex + race + degree + attend,
        data = GSSc,
        family = binomial)
  hoslem <- 
    hoslem.test(party.glm$y, party.glm$fitted)
  summary <- summary(party.glm)
  #Create Users Profile
  PredictiveData <- reactive({
    data.frame(
      age = input$age,
      sex = input$sex,
      race = input$race,
      degree = input$degree,
      attend = input$chruch
    )
  })
  #Predict User's PARTYID
  partyPredict <- eventReactive(input$submit, {
    newdata <- PredictiveData()
    newdata$sex <- as.factor(newdata$sex)
    newdata$race <- as.factor(newdata$race)
    newdata$degree <- as.factor(newdata$degree)
    newdata$attend <- as.factor(newdata$attend)
    predict(party.glm, newdata, type = "response")
  })
  
  #Generate a summary of the dataset
  output$answer <- renderPrint({
    partyPredict()
  })
  
  #Retrieve Slideshow
  output$slide1 <- renderUI({
    pres1 <- img(src = 'Slide01.png')
  })
  
  output$slide2 <- renderUI({
    pres2 <- img(src = 'Slide02.png')
  })
  
  output$slide3 <- renderUI({
    pres3 <- img(src = 'Slide03.png')
  })
  output$slide4 <- renderUI({
    pres4 <- img(src = 'Slide04.png')
  })
  output$slide5 <- renderUI({
    pres5 <- img(src = 'Slide05.png')
  })
  output$slide6 <- renderUI({
    pres6 <- img(src = 'Slide06.png')
  })
  output$slide7 <- renderUI({
    pres7 <- img(src = 'Slide12.png')
  })
  output$slide8 <- renderUI({
    pres8 <- img(src = 'Slide13.png')
  })
  output$slide9 <- renderUI({
    pres9 <- img(src = 'Slide14.png')
  })
  
  #Info on the logistic model
  output$model <- renderPrint({
    print(summary)
    print(hoslem)
  })
  
  #Dem & Rep
  output$plot1 <- renderPlot({
    DR <- ggplot(data = GSSc, aes(x = partyid, fill = partyid)) 
    DR <- DR + geom_bar(stat = "count", position = position_dodge(),size = .3) 
    DR <- DR + guides(fill = F) + scale_fill_manual(values = c("#2d5986", "#cc0000")) 
    DR <- DR + xlab("") + ylab("Particpants") + ggtitle("Democrat or Republican")
    print(DR)
  })
  
  #Age of Participants
  output$plot2 <- renderPlot({
    AGE <- ggplot(data = GSSc, aes(x = age, fill = partyid))
    AGE <- AGE + geom_bar(stat = "count", position = "stack",size = .3)
    AGE <- AGE + guides(fill = F) + scale_fill_manual(values = c("#2d5986", "#cc0000"))
    AGE <- AGE + xlab("") + ylab("Particpants") + ggtitle("Age of Participants") + xlim(18, 88)
    print(AGE)
  })
  
  #Sex of Participants 
  output$plot3 <- renderPlot({
    SEX <- ggplot(data = GSSc, aes(x = sex, fill = partyid))
    SEX <- SEX + geom_bar(stat = "count", position = "stack",size = .3)
    SEX <- SEX + guides(fill = F) + scale_fill_manual(values = c("#2d5986", "#cc0000"),labels = c("Male", "Female"))
    SEX <- SEX + xlab("") + ylab("") + ggtitle("Sex of Participants")
    print(SEX)
  })
  
  #Race of Participants
  output$plot3 <- renderPlot({
    RACE <- ggplot(data = GSSc, aes(x = race, fill = partyid))
    RACE <- RACE + geom_bar(stat = "count", position = "stack", size = .3)
    RACE <- RACE + guides(fill = F) + scale_fill_manual(values = c("#2d5986", "#cc0000"))
    RACE <- RACE + xlab("") + ylab("") + ggtitle("Race of Participants")
    print(RACE)
  })
  
  #Degree of Participants
  output$plot4 <- renderPlot({
    DEGREE <- ggplot(data = GSSc, aes(x = degree, fill = partyid))
    DEGREE <- DEGREE + geom_bar(stat = "count", position = "stack", size = .3)
    DEGREE <- DEGREE + guides(fill = F) + scale_fill_manual(values = c("#2d5986", "#cc0000"))
    DEGREE <- DEGREE + xlab("") + ylab("") + ggtitle("Degree of Participants")
    print(DEGREE)
  })
  
  #Church of Participants
  output$plot5 <- renderPlot({
    CHURCH <- ggplot(data = GSSc, aes(x = attend, fill = partyid))
    CHURCH <- CHURCH + geom_bar(stat = "count", position = "stack", size = .3)
    CHURCH <- CHURCH + guides(fill = F) + scale_fill_manual(values = c("#2d5986", "#cc0000"))
    CHURCH <- CHURCH + xlab("") + ylab("") + ggtitle("Church Attendance of Participants")
    print(CHURCH)
  })
})

