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
library(tigris)
library(shinydashboardPlus)

#### Data Manipulation ###
#Note that this is done outside of the ui/server portion of the code
#Reading files from google drive

accident18 = read_csv("accident18.csv")
vehicle18 = read_csv("vehicle18.csv")

#Joining

combined = left_join(accident18,vehicle18,by = c("ST_CASE"))
combined_2 = combined[!duplicated(as.list(combined))] %>% 
  mutate(STATE.x = toupper(STATE.x)) %>%
  mutate(STATE.x = ifelse(STATE.x == "DIST OF COLUMBIA","DISTRICT OF COLUMBIA",STATE.x))

#Getting county names
Codes = read_excel("Codes.xlsx",skip = 1)
Codes_1 = Codes %>% select(`State Name`,`County Code`,`County Name`,`State Code`) %>% mutate(`County Code`= as.numeric(`County Code`),`State Code`= as.numeric(`State Code`)) %>% unique()
combined_3 = left_join(combined_2,Codes_1,by=c("STATE.x"="State Name","COUNTY"="County Code"))
colnames(combined_3)[c(149,150)] = c("County_name","State_code")
combined_4 = combined_3 %>% mutate(County_name = tolower(County_name)) %>% mutate(County_name = toTitleCase(County_name)) 
combined_4$GEOID = paste(combined_4$State_code,combined_4$COUNTY,sep=",")

#Reading populations
Population = read_csv("Population.csv")
#Joining population estimates
Population_1 = Population %>% select(STATE,COUNTY,POPESTIMATE2017) %>% 
  mutate_all(as.numeric) %>% mutate(GEOID = paste(STATE,COUNTY,sep=",")) %>% select(4,3)
combined_5 = left_join(combined_4,Population_1) %>% filter(STATE.x == "TEXAS")

#Totals per county
Totals <- combined_5 %>%
  group_by(County_name,GEOID) %>%
  summarize(total=n()/mean(POPESTIMATE2017))

#Reading state file
counties = counties(48,cb=T) 
counties$STATEFP = as.numeric(counties$STATEFP)
counties$COUNTYFP = as.numeric(counties$COUNTYFP)
counties$GEOID = paste(counties$STATEFP,counties$COUNTYFP,sep=",")
#Joining the fatal accident rates to the spatial data frame
states_merged_sb <- tigris::geo_join(counties, Totals, by="GEOID")

#Creating color scheme for county fatal accident rates
pal <- colorNumeric("Greens", domain=states_merged_sb$total)

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
        "Weather", 
        "Weather Conditions",
        choices = c("All",combined_5$WEATHER1 %>% unique() %>% sort())
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
      boxPlus(
        width = 12,
        closable = FALSE, 
        status = "warning", 
        solidHeader = TRUE, 
      leafletOutput("map")
      )
    )
    #Closing the dashboard body
  )
  #Closing the dashboard page
)

### Server of app ###
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$map <- renderLeaflet({
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Fatal Accidents",
     states_merged_sb$County_name, states_merged_sb$total
    ) %>% lapply(htmltools::HTML)
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron")  %>% 
      addPolygons(data = states_merged_sb , 
                  fillColor = ~pal(states_merged_sb$total), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))%>%
      addMarkers(data = Incident_data(),lng = ~as.numeric(LONGITUD),
                 lat = ~as.numeric(LATITUDE),
                 clusterOptions = markerClusterOptions())
     
  })
  
  Incident_data = reactive({
    if(input$Weather == "All"){
      combined_5
    } else {
    combined_5 %>% filter(WEATHER1 == input$Weather)
    }
  })
  
  #Closing server code
  }

#Launching application
shinyApp(ui = ui, server = server)
