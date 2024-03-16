#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse) #loaded tidyverse because I'll most probably be using it
library(rsconnect) #loaded rsconnect in attempt of deploying

ui <- fluidPage(

      titlePanel("shiny_week8"), #changed title to make it shiny_week8 on browser and app

    sidebarLayout(
        sidebarPanel(
            radioButtons("genderbutton", "Would you like to see that data for:",choices = c("All","Male","Female"),"All"), # made a radio button that has a label of "would you like to..." and give the choices seen with a default of "All". I also gave it an ID of "genderbutton". I chose a button because I felt it would be easier for this to be a button over a drop down choice.
            radioButtons("before", "Would you like to include participants that completed the assessment before July 1st, 2017?", c("Yes","No"),"Yes"),# made a radio button that has a label of "would you like to..." and give a yes or no with a default of "Yes". I also gave it an ID of "before". I chose a button because I felt it would be easier for this to be a button over a drop down choice.
            selectInput("ErrorBand","What would you like to do with the error bands?",c("Display Error Band", "Suppress Error Band"),"Display Error Band") # made a selector that has a label of "what would you like to..." and give the choices seen with a default of "Display Error Band". I also gave it an ID of "ErrorBand". I chose a selector this time because I thought three buttons wouldn't look great, plus this one had longer choices, so a selector felt nicer than a radio button
            
        ),

        mainPanel(
           plotOutput("Plot") #changed the id to plot
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Plot <- renderPlot({ #changed the output$somethingPlot to output$Plot, to match new ID
        
      shinydata <- readRDS("./data.rds")
      
      if(input$genderbutton != "All"){ shinydata <- shinydata%>% #made a dataset that changes based on selected gender (used an if not equal to All, then filter based on input value)
        filter(gender==input$genderbutton)
      }
      
      if(input$before=="No"){shinydata <- shinydata%>%
        filter(timeEnd>=ymd("2017-07-01"))} #made the data change depending on the answer to the second question, also used ymd to determine date because the format I was using before wasn't working and dates are annoying and lubridate makes my life easier.

        shinydata %>% #made the same graph as last time, but will edit it slightly.
          ggplot(aes(x=mq1_6,y=mq8_10))+
          geom_point() +
          geom_smooth( method = "lm",color="purple",se= ifelse(input$ErrorBand=="Display Error Band",TRUE,FALSE))+ #added an extra ifelse statement here to turn on or off the error bands depending on the response (give a TRUE or FALSE output depending on input)
          labs(x= "Mean of Q1 through Q6", y= "Mean of Q8 through Q10")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

