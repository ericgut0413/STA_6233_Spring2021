library(shiny)
library(tidyverse)
library(scales)
library(plotly)
load(url("https://raw.githubusercontent.com/ericgut0413/STA_6233_Spring2021/master/Data/NBA-Fan-Impact.RData"))
str(t)
t<-t %>% dplyr::rename(Season=1)
str(t)
###Define UI for application that plots features of NBA-Fan-Impact data###
ui <- fluidPage(
  
  # Application title
  titlePanel("Fan Impact on NBA Regular Season Games: Limited Fan Attendance in 2021 Season"),
  
  # Sidebar layout
  sidebarLayout(
    #Inputs: Select which inputs from the data we want to display
    sidebarPanel(
      
      # Select Colors
      selectInput(inputId = "color_p", 
                  label = "Choose Point Color:",
                  choices = c("Blue", "Red", "Green", "Black", "Purple", "Orange"), 
                  selected = "Black"),
      
      selectInput(inputId = "color_l", 
                  label = "Choose Line Color:",
                  choices = c("Blue", "Red", "Green", "Black", "Purple", "Orange"), 
                  selected = "Black"),
      
      hr(), #Horizontal Line for visual separation
      
      # Set min/max Values for NBA Regular Seasons selected
      selectInput(inputId = "min", 
                  label = "Select Range (Min):", 
                  choices = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
                  selected= 2011),
      
      selectInput(inputId = "max",
                  label = "Select Range (Max):", 
                  choices = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021),
                  selected= 2021)
    ),
    
    # Output: Show scatterplot --------------------------------------
    mainPanel(
      tabsetPanel(
        tabPanel("Stat: Winning Percent", plotlyOutput(outputId = "scatterplot_1")),
        tabPanel("Stat: Point Differential", plotlyOutput(outputId = "scatterplot_2")),
        tabPanel("Stat: Field Goal Percent", plotlyOutput(outputId = "scatterplot_3")),
        tabPanel("Stat: Away Field Goal Percent", plotlyOutput(outputId = "scatterplot_4")),
        tabPanel("Stat: Away Three Point Percent", plotlyOutput(outputId = "scatterplot_5")),
        tabPanel("Stat: Away Free Throw Percent", plotlyOutput(outputId = "scatterplot_6")),
        tabPanel("Stat: Away Turnovers", plotlyOutput(outputId = "scatterplot_7"))
        
        #tabPanel("Data",  DT::dataTableOutput(outputId="datasheet"))
      )
    )
  )
)

# Define server logic required to create the scatterplot 
server <- function(input, output) {
  
  output$scatterplot_1<-renderPlotly({
    ggplot(data = t, aes_string(x = t$Season, y = t$WinPercent)) + geom_point(colour=input$color_p) + 
      geom_line(color= input$color_l) + xlim(as.numeric(input$min), as.numeric(input$max)) + 
      scale_y_continuous() + ggtitle("Home Team Winning Percentage") + xlab("Season") + ylab("Percent (%)") 
  }) 
  
  output$scatterplot_2<-renderPlotly({
    ggplot(data = t, aes_string(x = t$Season, y = t$PointDifferentialPG)) + geom_point(colour=input$color_p) + 
      geom_line(color= input$color_l) + xlim(as.numeric(input$min), as.numeric(input$max)) + 
      scale_y_continuous() + ggtitle("Home Team Point Differential Per Game") + xlab("Season") + ylab("Points") 
  })
  
  output$scatterplot_3<-renderPlotly({
    ggplot(data = t, aes_string(x = t$Season, y = t$FGPercent)) + geom_point(colour=input$color_p) + 
      geom_line(color= input$color_l) + xlim(as.numeric(input$min), as.numeric(input$max)) + 
      scale_y_continuous() + ggtitle("Home Team Field Goal Percentage") + xlab("Season") + ylab("Percent (%)") 
  })
  
  output$scatterplot_4<-renderPlotly({
    ggplot(data = t, aes_string(x = t$Season, y = t$AwayFGPercent)) + geom_point(colour=input$color_p) + 
      geom_line(color= input$color_l) + xlim(as.numeric(input$min), as.numeric(input$max)) + 
      scale_y_continuous() + ggtitle("Away Team Field Goal Percentage") + xlab("Season") + ylab("Percent (%)") 
  })
  
  output$scatterplot_5<-renderPlotly({
    ggplot(data = t, aes_string(x = t$Season, y = t$Away3PPercent)) + geom_point(colour=input$color_p) + 
      geom_line(color= input$color_l) + xlim(as.numeric(input$min), as.numeric(input$max)) + 
      scale_y_continuous() + ggtitle("Away Team Three Point Percentage") + xlab("Season") + ylab("Percent (%)") 
  })
  
  output$scatterplot_6<-renderPlotly({
    ggplot(data = t, aes_string(x = t$Season, y = t$AwayFTPercent)) + geom_point(colour=input$color_p) + 
      geom_line(color= input$color_l) + xlim(as.numeric(input$min), as.numeric(input$max)) + 
      scale_y_continuous() + ggtitle("Away Team Free Throw Percentage") + xlab("Season") + ylab("Percent (%)") 
  })
  
  output$scatterplot_7<-renderPlotly({
    ggplot(data = t, aes_string(x = t$Season, y = t$AwayTOVPG)) + geom_point(colour=input$color_p) + 
      geom_line(color= input$color_l) + xlim(as.numeric(input$min), as.numeric(input$max)) + 
      scale_y_continuous() + ggtitle("Away Team Turnovers Per Game") + xlab("Season") + ylab("Turnovers") 
  })
 
}
# Run the application 
shinyApp(ui = ui, server = server)