library(shiny)
library(RMySQL)

shinyUI(fluidPage(
  titlePanel("Predicting Political Party Affiliation",
             tags$head(tags$style(type = "text/css", "h2{color:white;}
                                  h2{ font-size: 40px;}
                                  a{color:#002266"))),
    sidebarPanel(
      tags$head(tags$style("body {background-color:#808080;}")),
      numericInput("age", label = h4("Age:"), 18,
                   min = 18, max = 100),
      selectInput("sex", label = h4("Sex:"),
                  c("Choose one" = "",
                    "Male" = 1,
                    "Female" = 2)), 
      selectInput("race", label = h4("Race:"),
                  c("Choose one" = "",
                    "White" = 1,
                    "Black" = 2,
                    "Other" = 3)),
      selectInput("degree", label = h4("Level of Education:"),
                  c("Choose one" = "",
                    "No Highschool Diploma" = 0,
                    "Highschool Diploma" = 1,
                    "Some College" = 2,
                    "Associates Degree" = 3,
                    "Bachelors Degree" = 4)),
      selectInput("chruch", label = h4("Church Attendance:"),
                  c("Choose one" = "",
                    "Never" = 0,
                    "Sometimes Once a Year" = 1,
                    "Several Times a Year" = 2,
                    "Once a Month" = 3,
                    "2-3 Times a Month" = 4,
                    "Nearly Every Week" = 6,
                    "Every Week" = 7,
                    "More than Once a Week" = 8)),
      helpText(em("*please fill out all fields")),
      actionButton("submit", "Submit")),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Probability", verbatimTextOutput("answer")),
      tabPanel("Presentation", uiOutput("slide1"), uiOutput("slide2"),
               uiOutput("slide3"), uiOutput("slide4"), uiOutput("slide5"),
               uiOutput("slide6"), uiOutput("slide7"), uiOutput("slide8"), uiOutput("slide9")),
      tabPanel("Graphics", plotOutput("plot1"), plotOutput("plot2"),
               plotOutput("plot3"), plotOutput("plot4"), plotOutput("plot5")),
      tabPanel("Model Info", verbatimTextOutput("model"))
    ),
    img(src='america.png',
        tags$head(tags$style(
          type="text/css",
          "img {
          width: 100%;
          display: block;
          margin-left: auto;
          margin-right: auto;
          }")))
    )
  )
)

