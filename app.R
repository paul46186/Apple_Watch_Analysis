#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(caTools)
library(shinydashboard)
library(rsconnect)
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(ggdark)
library(readxl)
library(ggthemr)
library(devtools)
library(caret)
library(dplyr)
library(bslib)
library(randomForest)

#setwd("C:/Users/pchen03/OneDrive - Kearney/Desktop/R/Shiny/Apple Watch")

#read csv
df = read.table('Apple Watch.csv',sep=",",header=TRUE)

#make the date column as date format
df <- transform(df,Date=as.Date(Date,"%m/%d/%y"))

#create a month column for future filter
df$month <- as.numeric(format(df$Date,'%m'))

str(df)

header=dashboardHeader(title = "Paul's Health Data")
sidebar=dashboardSidebar( sidebarMenu(
  menuItem("Activity", tabName = "Activity", icon = icon("dashboard")),
  menuItem("Sleep", tabName = "Sleep", icon = icon("th"))
))

body=  dashboardBody(
  tabItems(
    tabItem(tabName = "Activity",
            fluidRow(
              box(title = "Month",selectInput("month", "Month:",
                                              c("Jan" = 1,"Feb" = 2,"Mar" = 3,"Apr" = 4,"May" = 5,"Jun" = 6,"Jul" = 7,"Aug" = 8)), height = "150px"
              ),
              box(
                title = "Ayo!!What's up!", background = "light-blue",
                "The data was directly extracted from Paul's Apple Watch :)",br(),
                "From 2022-1 to 2022-8",br(),"This page is used to track my excerise time and weight", height = "150px"
              )
            ),
            
            fluidRow(
              box(plotOutput("Excerise", height = "300px")),
              box(plotOutput("Kcal", height = "300px"))
              
            ),
            fluidRow(
              valueBoxOutput("avgtime"),
              valueBoxOutput("avgKcal"),
              valueBoxOutput("avgweight")
            ),
            fluidRow(
              box(plotOutput("Weight", height = "300px"),width=6),
              box(plotOutput("Fat", height = "300px"),width=6)
              
            )
    ),
    
    
    tabItem(tabName = "Sleep",
            fluidRow(
              box(title = "Month",selectInput("Month", "Month:",
                                              c("Jan" = 1,"Feb" = 2,"Mar" = 3,"Apr" = 4,"May" = 5,"Jun" = 6,"Jul" = 7,"Aug" = 8)), height = "150px"),
              box(
                title = "Hey!!", background = "navy",
                "The data was directly extracted from Paul's Apple Watch :)",br(),
                "From 2022-1 to 2022-8",br(),"This page is used to track my sleep time and quality", height = "150px"
              )
              
            ),
            
            fluidRow(
              box(plotOutput("SleepTime", height = "300px"),width=12)
            ),
            
            fluidRow(
              valueBoxOutput("SleepGoal"),
              valueBoxOutput("SleepAvg"),
              valueBoxOutput("SleepQuality")
            ),
            fluidRow(
              box(plotOutput("SleepIdle", height = "300px"),width=12)
            )
    )
  )
)



ui <- dashboardPage(header,sidebar,body, skin='purple')

server <- function(input, output) {
  
  data1 <- reactive({
    subset(df, df$month == input$month)})
  
  data2 <- reactive({
    subset(df, df$month == input$Month)})
  
  output$Kcal <- renderPlot({
    ggplot(as.data.frame(data1()), aes(x = Date, y = Active.Energy)) +
      geom_line(size=1)+
      geom_smooth(method=lm,size=0.7)+
      labs(x = "Date", y= "Active Energy (Kcal)")+theme(legend.position = 'bottom')
  })
  
  output$Excerise <- renderPlot({
    ggplot(as.data.frame(data1()), aes(x = Date, y = Exercise.Time)) +
      geom_col(fill='#004C99')+
      labs(x = "Date", y= "Exercise Time (min)")
    
  })
  
  output$avgtime <- renderValueBox({
    valueBox(
      paste0(round((sum(as.data.frame(data1())$Exercise.Time,na.rm=TRUE)/nrow(as.data.frame(data1())[!is.na(as.data.frame(data1())$Exercise.Time),])),2)
      ) , 
      "Average Excerise Time", icon = icon("thumbs-up"),
      color = "purple"
    )
  })
  
  
  output$avgKcal <- renderValueBox({
    valueBox(
      paste0(round((sum(as.data.frame(data1())$Active.Energy,na.rm=TRUE)/nrow(as.data.frame(data1())[!is.na(as.data.frame(data1())$Active.Energy),])),2)
      ) , 
      "Average Kcal", icon = icon("thumbs-up"),
      color = "blue"
    )
  })
  
  output$avgweight <- renderValueBox({
    valueBox(
      paste0(round((sum(as.data.frame(data1())$Weight,na.rm=TRUE)/nrow(as.data.frame(data1())[!is.na(as.data.frame(data1())$Weight),])),2)
      ) , 
      "Average Weight (kg)", icon = icon("thumbs-up"),
      color = "green"
    )
  })
  
  output$Weight <- renderPlot({
    ggplot(as.data.frame(data1())[!is.na(as.data.frame(data1())$Weight),], aes(x = Date, y = Weight)) +
      geom_line(size=1)+geom_point()+geom_smooth(method=lm, se=FALSE,size=0.7)+
      labs(x = "Date", y= "Weight (kg)")
    
  })
  
  output$Fat <- renderPlot({
    ggplot(as.data.frame(data1())[!is.na(as.data.frame(data1())$Body.Fat),], aes(x = Date, y = Body.Fat)) +
      geom_line(size=1)+geom_point()+geom_smooth(method=lm, se=FALSE,size=0.7)+
      labs(x = "Date", y= "Body Fat (%)")
    
  })
  
  output$SleepTime <- renderPlot({
    ggplot(as.data.frame(data2())) +
      geom_col( aes(x = Date, y = Sleep.Asleep ,fill = Sleep.Asleep >8))+
      geom_hline(aes(yintercept=8), color="black")+
      scale_colour_manual(name = 'Enough Sleep', values = setNames(c('green','red'),c(T, F))) +
      labs(x = "Date", y= "Asleep hours")
    
  })
  
  output$SleepGoal <- renderValueBox({
    valueBox(
      paste0(
        sum(as.data.frame(data2())$Sleep.Asleep > 8,na.rm=TRUE)
      ) , 
      "Days Reach 8 hours Sleep", icon = icon("thumbs-up"),
      color = "teal"
    )
  })
  
  output$SleepAvg <- renderValueBox({
    valueBox(
      paste0(
        round((sum(as.data.frame(data2())$Sleep.Asleep,na.rm=TRUE)/nrow(as.data.frame(data2())[!is.na(as.data.frame(data2())$Sleep.Asleep),])),2)
      ) , 
      "Average Sleeping Hours", icon = icon("thumbs-up"),
      color = "black"
    )
  })
  
  output$SleepQuality <- renderValueBox({
    valueBox(
      paste0(
        round((sum(as.data.frame(data2())$Sleep.Idel,na.rm=TRUE)/nrow(as.data.frame(data2())[!is.na(as.data.frame(data2())$Sleep.Idel),])),2)
      ) , 
      "Average Hours to Fall Asleep", icon = icon("thumbs-up"),
      color = "olive"
    )
  })
  
  output$SleepIdle <- renderPlot({
    ggplot(as.data.frame(data2()), aes(x = Date, y = Sleep.Idel)) +
      geom_col(fill='#006666')+
      labs(x = "Date", y= "Time to fall asleep")
    
  })
  
  
  
}

shinyApp(ui, server)