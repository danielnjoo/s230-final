# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)

data <- read.csv('final_data.csv')
data$date<-as.Date(data$date)
min_date <- as.Date("2014-09-02")
max_date <- as.Date("2017-05-12")

# Shiny apps are split into UI and Server components, data loading can be done ^
ui <- fluidPage(
  
  # we can use CSS styling here since it's a web app... this is for the slider labels; more info: https://shiny.rstudio.com/articles/css.html
  tags$style(type = "text/css", "label {font-weight:400;}"),
   
   titlePanel("Swipes at Val"),
   h6("Final Project for S230 by Rana Barghout, Alex Liu, Daniel Njoo"),
   hr(),
   
   # sidebar layout is a common design pattern and is the default if you start a new Shiny app
   sidebarLayout(
      sidebarPanel(
        # min date of data = "2014-09-02", max = "2017-05-12", 983d difference, 141 weeks
         h4("Choose start and end dates (in weeks)"),
         sliderInput("start", "Start date:", min = 1, max = 140, value = 1),
         sliderInput("end", "End date:", min = 1, max = 141, value = 141),
         hr(),
         h4("Choose a meal"),
         pickerInput(
           inputId = "type", 
           label = "Meal type", 
           choices = c("All", "Breakfast", "Lunch", "Dinner"), options = list(`actions-box` = TRUE), 
           multiple = F
         ),
         hr(),
         h4("Additional Options"),
         p("Color:"),
         materialSwitch(inputId = "events", label = "Events", right = T, status = "primary"),
         materialSwitch(inputId = "menu", label = "Menu", right = T, status = "primary")
      ),
      
      # show plot, this plot is rendered in the server section below
      mainPanel(
         plotOutput("distPlot")
      )
   )
)


# server logic that reacts to inputs
server <- function(input, output, session) {
 
  observe({
    # validates that end > start; another way to validate: https://shiny.rstudio.com/articles/validation.html
    if(input$end<input$start){
      updateSliderInput(session, "end", value=input$start+1)
    }
    updateSliderInput(session, "start", label=paste0("Start date: ", min_date+input$start*7))
    updateSliderInput(session, "end", label=paste0("End date: ", min_date+input$end*7))
  })
  
  # height and width can be dynamically assigned based on screen size, more: https://stackoverflow.com/questions/44324783/dynamically-adjust-height-and-or-width-of-shiny-plotly-output-based-on-window-si
   output$distPlot <- renderPlot(height=580,{

    # check what meal type 
     if(!is.null(input$type)){
      if(input$type=="Breakfast"){
         data<-data %>% filter(type=="Breakfast")
       }
       else if(input$type=="Lunch"){
         data<-data %>% filter(type=="Lunch")
       }
       else if(input$type=="Dinner"){
         data<-data %>% filter(type=="Dinner")
       }
     }

    # create base ggplot plot based on start/end parameters
     data %>% filter(date>min_date+input$start*7 & date<=min_date+input$end*7) %>% 
       ggplot(aes(date,count)) +
       theme(axis.text.x = element_text(angle=45,hjust=1,size=15)) +
       theme(axis.text.y = element_text(size=15)) +
       xlab("Date") -> g
     
     # check for events
     if(isTRUE(input$events)){
       g <- g + geom_point(aes(col=event))
     }
     # check for menu
     else if(isTRUE(input$menu)){
       
       #but only show top 6 categories 
       data %>%
         group_by(tag66) %>%
         summarise(count=n()) %>%
         arrange(desc(count)) %>%
         .$tag66 %>%
         substr(1,10) %>% 
         head() -> list; list[2:6] -> list # remove free cage egg
       
       data$Meal <- substr(data$tag66,1,10)
       
       g <- g + 
         geom_point(data=data, aes(col=Meal)) + 
         scale_color_discrete(breaks=list)
       }
     else{
       g <- g + 
         geom_line()
     }
     #display
     g
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

