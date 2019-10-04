#### Loading packages ###

library(shiny)
library(rsconnect)
library(ggplot2)
library(shinyjs)
library(leaflet)
library(ggthemes)
library(dplyr)
library(tigris)
library(purrr)
library(flexdashboard)
library(dplyr)
library(readr)
library(shinydashboard)

#### Data Manipulation ###

#Note that this is done outside of the ui/server portion of the code

MSA_Populations <- read.csv("MSA Populations.csv",stringsAsFactors = FALSE)
MSA_Populations_1 = MSA_Populations %>% select(id=GC.target.geo.id2,11:16)
MSA_Populations_1_m = MSA_Populations %>% select(GEOID=GC.target.geo.id2,11:16)
Data_function = function(data){
  i = 1
  data_list = list()
  while(i<ncol(data)){
    data_list[[i]] = data %>% select(1,i+1)
    i=i+1
  }
  return(data_list)
}
MSA_Pops_Total = Data_function(MSA_Populations_1)
MSA_Pops_Total_m = Data_function(MSA_Populations_1_m)

#Survey Data
Smart = list()

Smart[[1]] =  read_csv("Smart_11_a.csv")
Smart[[2]] =  read_csv("Smart_12_a.csv")
Smart[[3]] =  read_csv("Smart_13_a.csv")
Smart[[4]] =  read_csv("Smart_14_a.csv")
Smart[[5]] =  read_csv("Smart_15_a.csv")
Smart[[6]] =  read_csv("Smart_16_a.csv")

#Geographic data for smaller areas
cb = core_based_statistical_areas(cb = TRUE)
cb@data$id=cb@data$GEOID

#Geographic data for larger areas
metro_d = metro_divisions()
metro_d@data$id = metro_d@data$METDIVFP

### UI of APP ###

ui = dashboardPage(
  title = "Map",
  
  #Changing color of dashboard
  
  skin = "red",
  
  # Application header. This header is the bar along the top portion of the dashboard.
  #HTML tags that control icons on the header.
  
  dashboardHeader(
    title = tags$a(
                   tags$img(src='Martin.png',
                            height = "45px",
                            width = "150px"),
                   title = "Martin"),
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      br(),
      selectInput(
        "indication",
        "Indication",
        
        #The choices are the different indications listed in the database.
        
        choices = c("Depression"= "addepev2","Diabetes"="diabete3","Arthritis"="havarth3","Skin Cancer"="chcscncr","Other Cancer"="chcocncr"),
        
        
        #Ensure user can not select more than one indication.
        
        multiple = FALSE
      ),
      br(),
      sliderInput("time", "Year",2011, 
                 2016,
                  value = 2016,
                  step=1,
                  sep = "",
                  animate =
                    animationOptions(interval = 3000, loop = TRUE)),
      br(),
      br(),
     fluidRow(
       column(12,
              align = 'center',
              textOutput("selected_text")
               )
       ),
     tags$head(
       tags$style(
         "#selected_text{
         font-size: 12px;
         font-style: bold;
         }"
              )
       ),
     
     br(),
     plotOutput("plot", height = "325px"),
     br(),
     br(),
  
     conditionalPanel(condition = "output.selected_text != 'Selected: All'",
      gaugeOutput("rate", height = "130px")
      )
    )),
  dashboardBody(
    tags$head(tags$style(HTML('
        .skin-red .main-header .logo {
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
  
 disease_type = eventReactive(input$indication,{
   if(input$indication=="addepev2"){return("Depression")}
   if(input$indication=="diabete3"){return("Diabetes")}
   if(input$indication=="havarth3"){return("Arthritis")}
   if(input$indication=="chcscncr"){return("Skin Cancer")}
   if(input$indication=="chcocncr"){return("Other Cancer")}

 })
  
  points <- reactive({
    cb_reactive()@data %>% 
      filter(Year==input$time)
  })
  
  points_1 <- reactive({
    metro_reactive()@data %>% 
      filter(Year==input$time)
  })
  
  points_map<- reactive({
    cb_reactive()@data %>% 
      filter(Year<=input$time)
  })
  
  points_map_1<- reactive({
    metro_reactive()@data %>% 
      filter(Year<=input$time)
  })
  
  cb_reactive = reactive({
    Smart_Rates = Smart %>% map(~select(.x,input$indication,x.mmsa))%>% map(~group_by(.x,id=as.character(x.mmsa))) %>% map(~summarise_at(.x,funs(Rate = (sum(. == 1,na.rm = TRUE)/n())*100),.vars= 1))
    
    ##Only need rates from msa's that appear in every element
    
    IDS = sapply(Smart_Rates, `[[`, 1) %>% unlist %>% table()
    IDS_1 = IDS[IDS==6]  %>% names()
    
    Smart_Rates_1 = Smart_Rates %>% map(~filter(.x,id %in% IDS_1))
    
    cb_test=subset(cb, GEOID %in% unique(Smart_Rates_1[[1]]$id))
    
    Complete_cb_data= list()
    Complete_cb_data_1=Smart_Rates_1  %>% map(~left_join(cb_test@data,.x)) 
    Complete_cb_data_1[[1]]$Year = 2011
    Complete_cb_data_1[[2]]$Year = 2012
    Complete_cb_data_1[[3]]$Year = 2013
    Complete_cb_data_1[[4]]$Year = 2014
    Complete_cb_data_1[[5]]$Year = 2015
    Complete_cb_data_1[[6]]$Year = 2016
    Complete_cb_data_1[[1]] =left_join(Complete_cb_data_1[[1]],MSA_Pops_Total[[1]])
    Complete_cb_data_1[[2]] = left_join(Complete_cb_data_1[[2]],MSA_Pops_Total[[2]])
    Complete_cb_data_1[[3]] = left_join(Complete_cb_data_1[[3]],MSA_Pops_Total[[3]])
    Complete_cb_data_1[[4]] = left_join(Complete_cb_data_1[[4]],MSA_Pops_Total[[4]])
    Complete_cb_data_1[[5]] = left_join(Complete_cb_data_1[[5]],MSA_Pops_Total[[5]])
    Complete_cb_data_1[[6]] = left_join(Complete_cb_data_1[[6]],MSA_Pops_Total[[6]])
    cb_test@data = bind_rows(Complete_cb_data_1)
    cb_test@data$population = cb_test@data[,12:17] %>% unlist() %>% na.omit() %>% as.numeric() 
    cb_test@data$inflicted = round(as.numeric(cb_test@data$population) * (cb_test@data$Rate/100))
   
    min_inflict = cb_test@data %>% group_by(id) %>% filter(Year == min(Year)) %>% select(id,min = inflicted)
    cb_test@data = left_join(cb_test@data,min_inflict)
    
    cb_test@data = cb_test@data %>% mutate(change_prev = ((inflicted - min) / min)*100)
    cb_test@data = cb_test@data %>% select(-c(12:17))
    return(cb_test)
    
  })
  
  metro_reactive = reactive({
    Smart_Rates = Smart %>% map(~select(.x,input$indication,x.mmsa))%>% map(~group_by(.x,id=as.character(x.mmsa))) %>% map(~summarise_at(.x,funs(Rate = (sum(. == 1,na.rm = TRUE)/n())*100),.vars=1))

    ##Only need rates from msa's that appear in every element
    
    IDS = sapply(Smart_Rates, `[[`, 1) %>% unlist %>% table()
    IDS_1 = IDS[IDS==6]  %>% names()
    
    Smart_Rates_1 = Smart_Rates %>% map(~filter(.x,id %in% IDS_1))
    
    metro_d_test = subset(metro_d, METDIVFP %in% unique(Smart_Rates_1[[1]]$id))
    
    Complete_metro_data= list()
    
    Complete_metro_data_1=Smart_Rates_1  %>% map(~left_join(metro_d_test@data,.x)) 
    Complete_metro_data_1[[1]]$Year = 2011
    Complete_metro_data_1[[2]]$Year = 2012
    Complete_metro_data_1[[3]]$Year = 2013
    Complete_metro_data_1[[4]]$Year = 2014
    Complete_metro_data_1[[5]]$Year = 2015
    Complete_metro_data_1[[6]]$Year = 2016
    Complete_metro_data_1[[1]] =left_join(Complete_metro_data_1[[1]],MSA_Pops_Total_m[[1]])
    Complete_metro_data_1[[2]] = left_join(Complete_metro_data_1[[2]],MSA_Pops_Total_m[[2]])
    Complete_metro_data_1[[3]] = left_join(Complete_metro_data_1[[3]],MSA_Pops_Total_m[[3]])
    Complete_metro_data_1[[4]] = left_join(Complete_metro_data_1[[4]],MSA_Pops_Total_m[[4]])
    Complete_metro_data_1[[5]] = left_join(Complete_metro_data_1[[5]],MSA_Pops_Total_m[[5]])
    Complete_metro_data_1[[6]] = left_join(Complete_metro_data_1[[6]],MSA_Pops_Total_m[[6]])
    metro_d_test@data = bind_rows(Complete_metro_data_1)
    metro_d_test@data$population = metro_d_test@data[,16:21]  %>%unlist()%>% na.omit() %>% as.numeric() 
    metro_d_test@data$inflicted = round(as.numeric(metro_d_test@data$population) * (metro_d_test@data$Rate/100))
    min_inflict_1 = metro_d_test@data %>% group_by(id) %>% filter(Year == min(Year)) %>% select(id,min = inflicted)
    metro_d_test@data = left_join(metro_d_test@data,min_inflict_1)
    metro_d_test@data = metro_d_test@data %>% mutate(change_prev = ((inflicted - min) / min)*100)
    metro_d_test@data =metro_d_test@data %>% select(-c(16:21))
    return(metro_d_test)
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$map <- renderLeaflet({
    bins <- seq(min(c(cb_reactive()@data$change_prev,metro_reactive()@data$change_prev)),max(c(cb_reactive()@data$change_prev,metro_reactive()@data$change_prev)), length.out = 9)
    pal <- colorBin("viridis", domain = cb_reactive()@data$change_prev, bins = bins)
    leaflet(cb_reactive()) %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 4) %>% addLegend(pal = pal,title="Reported % of Population", values = ~Rate,opacity=.7,position = "bottomright", labFormat = labelFormat(suffix = '%', between = '% - '))
    })
  
  observe({
    bins <- seq(min(c(cb_reactive()@data$change_prev,metro_reactive()@data$change_prev)),max(c(cb_reactive()@data$change_prev,metro_reactive()@data$change_prev)), length.out = 9)
    pal <- colorBin("viridis", domain = cb_reactive()@data$change_prev, bins = bins)
    labels <- sprintf(
      "<strong>%s</strong><br/>Population: %s <br/>%s Rate: %g%%<br/>Estimated Inflicted: %s<br/>Total Percent Inflicted Change Since 2011: %s%%",
      points()$NAME,points()$population,disease_type(),round(points()$Rate,2), points()$inflicted, round(points()$change_prev,2)
    ) %>% lapply(htmltools::HTML)
    
    bins_1 <- seq(min(c(cb_reactive()@data$change_prev,metro_reactive()@data$change_prev)),max(c(cb_reactive()@data$change_prev,metro_reactive()@data$change_prev)), length.out = 9)
    pal_1 <- colorBin("viridis", domain = cb_reactive()@data$change_prev, bins = bins_1)
    labels_1 <- sprintf(
      "<strong>%s</strong><br/>Population: %s <br/>%s Rate: %g%%<br/>Estimated Inflicted: %s<br/>Total Percent Inflicted Change Since 2011: %s%%",
      points_1()$NAME,points_1()$population,disease_type(), round(points_1()$Rate,2),points_1()$inflicted, round(points_1()$change_prev,2)
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = cb_reactive(),
                  layerId = ~id,
                  fillColor = ~pal(points()$change_prev),
                  weight =2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% addPolygons(data = metro_reactive(),
                                                         layerId = ~id,
                                                         fillColor = ~pal_1(points_1()$change_prev),
                                                         weight =2,
                                                         opacity = 1,
                                                         color = "white",
                                                         dashArray = "3",
                                                         fillOpacity = .7,
                                                         highlight = highlightOptions(
                                                           weight = 5,
                                                           color = "#666",
                                                           dashArray = "",
                                                           fillOpacity = 0.7,
                                                           bringToFront = TRUE),
                                                         label = labels_1,
                                                         labelOptions = labelOptions(
                                                           style = list("font-weight" = "normal", padding = "3px 8px"),
                                                           textsize = "15px",
                                                           direction = "auto"))
  })
  
  observe({
    bins <- seq(min(c(cb_reactive()@data$change_prev,metro_reactive()@data$change_prev)),max(c(cb_reactive()@data$change_prev,metro_reactive()@data$change_prev)), length.out = 9)
    pal <- colorBin("viridis", domain = cb_reactive()@data$change_prev, bins = bins)
    leafletProxy("map",data = cb_reactive()) %>% clearControls() %>%
      addLegend(pal = pal,title="Total Percent Inflicted Change Since 2011", values = ~change_prev,opacity=.7,position = "bottomright", labFormat = labelFormat(suffix = '%', between = '% - '))
  })
  
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$map_shape_click,{
     data_of_click$clickedMarker <- input$map_shape_click
  })
  
  observeEvent(input$map_click,{
                 data_of_click$clickedMarker = NULL }
  )
 
  output$rate = flexdashboard::renderGauge({
    if(!is.null(data_of_click$clickedMarker)){
      if(selected_city_class()){
      my_place=data_of_click$clickedMarker$id
      breaks = seq(min(c(cb_reactive()@data$Rate,metro_reactive()@data$Rate)),max(c(cb_reactive()@data$Rate,metro_reactive()@data$Rate)), length.out = 6)
     rate = points() %>% filter(id == my_place) %>% select(Rate) %>% unique() %>% as.numeric()
     } else{
      my_place=data_of_click$clickedMarker$id
      breaks = seq(min(c(cb_reactive()@data$Rate,metro_reactive()@data$Rate)),max(c(cb_reactive()@data$Rate,metro_reactive()@data$Rate)), length.out = 6)
      rate = points_1() %>% filter(id == my_place) %>% select(Rate) %>% unique() %>% as.numeric()
      }
      gauge(round(rate,2),
          min=round(breaks[1],2),
          max=round(max(c(cb_reactive()@data$Rate,metro_reactive()@data$Rate)),2),
          label = paste("% Inflicted"),
          symbol = '%',
          gaugeSectors(
            success = c(breaks[1],breaks[2]),
            warning = c(breaks[3],breaks[4]),
            danger = c(breaks[5],breaks[6])
          ))
    }})
  
  selected_city_class = eventReactive(input$map_shape_click,{
    my_place=data_of_click$clickedMarker$id
    if(my_place %in% cb_reactive()@data$id){
      TRUE
    } else{
      FALSE
    }
  })
  
  output$selected_text = renderText({
    if(!is.null(data_of_click$clickedMarker)){
      if(selected_city_class()){
      my_place=data_of_click$clickedMarker$id
      name = points() %>% filter(id == my_place) %>% select(NAME) %>% unique() %>% as.character()
      paste("Selected: ",name,sep = "")
      } else{
        my_place=data_of_click$clickedMarker$id
        name = points_1() %>% filter(id == my_place) %>% select(NAME) %>% unique() %>% as.character()
        paste("Selected: ",name,sep = "")
      }
    } else {
      paste("Selected: All")
    }
  })
  
  output$plot = renderPlot({
    if(!is.null(data_of_click$clickedMarker)){
      if(selected_city_class()){
        full_data = cb_reactive()@data
        full_data_1 =cb_reactive()@data
        testing = points_map()
        testing_1 = points_map()} else{
          full_data = metro_reactive()@data
          full_data_1 =metro_reactive()@data
          testing = points_map_1()
          testing_1 = points_map_1() 
        }
        my_place=data_of_click$clickedMarker$id
        full_data = full_data  %>% filter(id == my_place) %>% select(Year,inflicted) %>% group_by(Year) %>% summarise(Pop = inflicted) %>% mutate(panel = "Estimated # Inflicted") %>% filter(Pop == max(Pop))
        full_data_1 = full_data_1%>% filter(id == my_place) %>%  select(Year, population) %>% group_by(Year) %>% summarise(Pop = population) %>% mutate(panel = "Population #") %>% filter(Pop == max(Pop))
        full_data_2 = rbind(full_data, full_data_1)
        full_data_2$panel = factor(full_data_2$panel,levels = c("Population #","Estimated # Inflicted"))
        testing = testing %>% filter(id == my_place) %>% select(Year,inflicted) %>% group_by(Year) %>% summarise(Pop = inflicted)%>% mutate(panel = "Estimated # Inflicted")
        testing_1 = testing_1%>%filter(id == my_place)%>% select(Year, population) %>% group_by(Year) %>% summarise(Pop = population) %>% mutate(panel = "Population #")
        testing_2 = rbind(testing, testing_1)
        testing_2$panel = factor(testing_2$panel,levels = c("Population #","Estimated # Inflicted"))
        ggplot(full_data_2, aes(x=Year , y = Pop))+ geom_blank(data= full_data_2,aes(x=Year,y=Pop))+ facet_grid(panel ~.,scales = 'free_y') + geom_point(color ="#66CD00",size=2,data = testing_2,aes(x=Year,y=Pop)) +geom_line(color ="#66CD00",size=1,data = testing_2,aes(x=Year,y=Pop)) + theme_economist()+theme(strip.text = element_text(size = 10,color= "#ffffff"), axis.line.x = element_line(color = "#ffffff"), strip.background = element_rect(fill="#8E388E"),axis.ticks.x = element_line(color = "#ffffff"), axis.text = element_text(color ="#ffffff" ), panel.grid.major.y= element_line(color="#ffffff"), plot.background = element_rect(fill="#222d32"), panel.background = element_rect(fill = "#222d32"), axis.title.x = element_text(margin = margin(t=25),face = "bold",color ="#ffffff" ), legend.title = element_blank(), axis.title.y = element_blank()) + scale_y_continuous(labels = scales::comma) + coord_cartesian(xlim =c(min(c(cb_reactive()@data$Year,metro_reactive()@data$Year)),max(c(cb_reactive()@data$Year,metro_reactive()@data$Year)))) 
         } else {
    full_data = rbind(cb_reactive()@data%>% select(Year,inflicted),metro_reactive()@data%>% select(Year,inflicted))
    full_data_1 = rbind(cb_reactive()@data%>% select(Year,population),metro_reactive()@data%>% select(Year,population))
    full_data = full_data  %>% group_by(Year) %>% summarise(Pop = sum(inflicted))%>% mutate(panel = "Estimated # Inflicted") %>% filter(Pop == max(Pop))
    full_data_1 = full_data_1 %>% group_by(Year) %>% summarise(Pop = sum(population)) %>% mutate(panel = "Population #") %>% filter(Pop == max(Pop))
    full_data_2 = rbind(full_data, full_data_1)
    full_data_2$panel = factor(full_data_2$panel,levels = c("Population #","Estimated # Inflicted"))
    testing = rbind(points_map()%>% select(Year,inflicted),points_map_1()%>% select(Year,inflicted))
    testing_1 = rbind(points_map()%>% select(Year, population),points_map_1()%>% select(Year, population))
    testing = testing  %>% group_by(Year) %>% summarise(Pop = sum(inflicted))%>% mutate(panel = "Estimated # Inflicted")
    testing_1 = testing_1 %>% group_by(Year) %>% summarise(Pop = sum(population)) %>% mutate(panel = "Population #")
    testing_2 = rbind(testing, testing_1)
    testing_2$panel = factor(testing_2$panel,levels = c("Population #","Estimated # Inflicted"))
    ggplot(full_data_2, aes(x=Year , y = Pop))+ geom_blank(data= full_data_2,aes(x=Year,y=Pop))+ facet_grid(panel ~.,scales = 'free_y') + geom_point(color ="#66CD00",size=2,data = testing_2,aes(x=Year,y=Pop)) +geom_line(color ="#66CD00",size=1,data = testing_2,aes(x=Year,y=Pop)) + theme_economist()+theme(strip.text = element_text(size = 10,color= "#ffffff"), axis.line.x = element_line(color = "#ffffff"), strip.background = element_rect(fill="#8E388E"),axis.ticks.x = element_line(color = "#ffffff"), axis.text = element_text(color ="#ffffff" ), panel.grid.major.y= element_line(color="#ffffff"), plot.background = element_rect(fill="#222d32"), panel.background = element_rect(fill = "#222d32"), axis.title.x = element_text(margin = margin(t=25),face = "bold",color ="#ffffff" ), legend.title = element_blank(), axis.title.y = element_blank()) + scale_y_continuous(labels = scales::comma) + coord_cartesian(xlim =c(min(c(cb_reactive()@data$Year,metro_reactive()@data$Year)),max(c(cb_reactive()@data$Year,metro_reactive()@data$Year)))) 
      }})
  }

#Launching application
shinyApp(ui = ui, server = server)