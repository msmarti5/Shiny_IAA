#Visualization Application
#### Loading packages ###

library(shiny)
library(rsconnect)
library(ggplot2)
library(shinyjs)
library(leaflet)
library(tidyverse)
library(flexdashboard)
library(shinydashboard)

#### Data Manipulation ###
#Note that this is done outside of the ui/server portion of the code

### UI of APP ###

ui = dashboardPage(
  title = "Visualization Project",
  
  #Changing color of dashboard
  
  skin = "red",
  
  # Application header. This header is the bar along the top portion of the dashboard.
  #HTML tags that control icons on the header.
  
  dashboardHeader(
    #title = tags$a(
      #tags$img(src='Martin.png',
      #         height = "45px",
      #         width = "150px"),
      #         title = "Martin"),
      #          titleWidth = 300
    ),
  
  dashboardSidebar(
    sidebarMenu(
      br(),
      selectInput(
        "example",
        "Example",
        choices = c("Option 1","Option 2")
      ))
    ),
  
  dashboardBody(
    
    #These tags control the colors in the application
    
                  tags$head(
                    tags$style(
                          HTML('.skin-red .main-header .logo {
                              background-color: #222d32;
                              }
                              .skin-red .main-header .navbar .sidebar-toggle:hover {
                              background-color: #4F2F4F; 
                              }
                              .skin-red .main-header .logo:hover {
                              background-color: #1e282c;
                              }
                              .skin-red .main-header .navbar {
                              background-color: #8E388E;
                              }
                              .box-warning.box-solid.box>.box-header {
                              color:#fff;
                              background:#FF9E1B;
                              }
                              .box-warning.box-solid.box{
                              border-bottom-color:#FF9E1B;
                              border-left-color:#FF9E1B;
                              border-right-color#FF9E1B;
                              border-top-color:#FF9E1B;
                              }
                              .bg-orange{
                              background-color:#EB3300 !important;
                              }
                              .bg-yellow{
                              background-color:#FF9E1B !important;
                              }
                              .bg-red{
                              background-color:#63666A !important;
                              }
                              '))),
    #Creating the main application page
    
    fluidPage(
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      leafletOutput("map")
    )
    #Closing the dashboard body
  )
  #Closing the dashboard page
)

### Server of app ###
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$map <- renderLeaflet({
    leaflet() %>% addTiles()
  })
  #Closing server code
  }

#Launching application
shinyApp(ui = ui, server = server)