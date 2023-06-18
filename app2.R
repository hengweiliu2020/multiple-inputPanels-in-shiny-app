
# use multiple inputPanels and actionbutton
# select multiple items in each inputPanel
# Anthor: Hengwei Liu


library(haven)
library(shinydashboard)
library(DT)
library(tidyr)
library(shiny)
library(ggplot2)
library(dplyr)
library(stringi)
library(shinythemes)



mat <- matrix(NA, nrow = 30, ncol = 4)
xy <- data.frame(mat)

for (i in seq(1,30)){
  
 xy[[i,1]] <- ifelse(i%%2, 'Male','Female')
  
}

for (i in seq(1,10)){
  xy[[i,2]] <- 'White'
  xy[[i+10,2]] <- 'Black or African American'
  xy[[i+20,2]] <- 'Asian or Pacific Islander'
}

for (i in seq(1,30)){
  if (i%%3==0) {xy[[i,3]] <- 'Hispanic or Latino'}
  if (i%%3==1) {xy[[i,3]] <- 'Not Hispanic or Latino'}
  if (i%%3==2) {xy[[i,3]] <- 'Not Reported'}
}

for (i in seq(1,30)){
  xy[i,4]=as.character(i)
}

xy$SEX <- xy$X1
xy$RACE <- xy$X2
xy$ETHNIC <- xy$X3
xy$SUBJID <- xy$X4

xy <- xy[c('SEX','RACE','ETHNIC','SUBJID')]
print(xy)

ui <-
  dashboardPage(
    skin="blue",
    dashboardHeader(title="Data Review"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Demographics", tabName="demog")
        
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName="demog",
                fluidPage(
                  theme = shinytheme("cerulean"),
                  navbarPage(
                    theme = "cerulean", 
                    "Demographics",
                    
                    
                    
                    
                    tabPanel("Table", 
                             mainPanel(tableOutput("table_demog"))
                    ),
                    tabPanel("Listing", 
                              
                             inputPanel( selectInput("race","Select a race:",  multiple=TRUE, unique(xy$RACE))),
                             inputPanel( selectInput("sex","Select a sex:",  multiple=TRUE, unique(xy$SEX))),
                             inputPanel( selectInput("ethnic","Select an ethnicity:",  multiple=TRUE, unique(xy$ETHNIC))),
                             actionButton("go_button", "apply"),
                    
                    
                             mainPanel(tableOutput("listing_demog"))
                         
                             
                             
  
                    )
                    
                    
                    
                  ) 
                )
        )
        
     
        
      )))

server <- function(input, output, session) {
  
 
  
  goButton <- eventReactive(input$go_button,{
    
    
    updateSelectInput(session, "sex", choices = c(xy$SEX), selected = input$sex)
    updateSelectInput(session, "race", choices = c(xy$RACE), selected = input$race)
    updateSelectInput(session, "ethnic", choices = c(xy$ETHNIC), selected = input$ethnic)
   
    
    
      xy2 <- xy[(xy$SEX %in% c(input$sex) & xy$RACE %in% c(input$race) & xy$ETHNIC %in% c(input$ethnic)),]
      
      xy2
      
    
    
    
  })
  
  output$listing_demog <- renderTable(
    
    expr = return(goButton()), 
    
    )
    
    
  
  
  
}

shinyApp(ui=ui, server=server)


