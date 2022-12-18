Sys.setlocale("LC_TIME", "English")
library(knitr)
library(readr)
library(mapsapi)
library(leaflet)
library(vroom)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tibble)
library(shiny)
setwd("E:/MBTAproj")
stops<-vroom("stops.csv")
stops<-stops[complete.cases(stops$stop_code),]
LRQ4_21<-vroom("LRTravelTimesQ4_21.csv")
LRQ1_22<-vroom("2022-Q1_LRTravelTimes.csv")
LRQ2_22<-vroom("2022-Q2_LRTravelTimes.csv")
LRQ3_22<-vroom("2022-Q3_LRTravelTimes.csv")


# Data cleaning and selection

T_2122<-rbind(LRQ4_21,LRQ1_22,LRQ2_22,LRQ3_22)

rm(LRQ4_21)
rm(LRQ1_22)
rm(LRQ2_22)
rm(LRQ3_22)


T_2122$service_date<-as.Date(T_2122$service_date)
T_2122$month<-month(T_2122$service_date)
T_2122$week<-week(T_2122$service_date)
T_2122$weekdays<-weekdays(T_2122$service_date,abbreviate = T)

#Randomly choose a week in these 12 months
sample_weeks<-c()
set.seed(2119)
for (i in 1:12) {
  weeks_in_month<-levels(as.factor(T_2122$week[T_2122$month==i]))
  sample_weeks<-c(sample_weeks,sample(weeks_in_month[2:(length(weeks_in_month)-1)],1))
}



smp_T<-T_2122[T_2122$week %in% as.numeric(sample_weeks),]

rm(T_2122)

smp_T$trip<-paste0(paste0("From ",smp_T$from_stop_id),paste0(" to ",smp_T$to_stop_id))

smp_T$trip<-as.factor(smp_T$trip)



#Shiny app

ui <- fluidPage(
  "I worked with Ziyang Lin to construct this shinny app",
  selectInput("weekdays","Weekdays",choices = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")),
  
  selectInput("trip", "Select a trip you are interested in (You can type From XXXXXX to define the start station so that the choice can be limited)", choices = unique(smp_T$trip)),
  
  textOutput("text"),
  
  plotOutput("plot"),
  
  "WARNING!!!! For the stop id = 70136,70137,70140,70141,70142 and 70143, The stops.csv does not contain the lat-lon infomation for them. So, errors will occur when plot the map.",
  
  leafletOutput("path"),
  
  "Above graph shows alternative routes on the map and the labs on routes indicates distance and estimated travel time given by google",
  
)



server <- function(input, output, session) {
  
  output$text<-renderText({
    mean_time<-mean(smp_T$travel_time_sec[smp_T$trip==input$trip & smp_T$weekdays==input$weekdays])
    paste0("On ",input$weekdays, ", the average travel time between these two stations is ", round(mean_time/60,1)," minutes")
  }
  )
  
  output$plot<-renderPlot(hist(smp_T$travel_time_sec[smp_T$trip==input$trip & smp_T$weekdays==input$weekdays],main="Distribution of travel time",xlab="Travel time", breaks = 100))
  
  key = "AIzaSyAzFwgIUaF9TG0W7hGxdAFP1CIs0uIs-P0"
  
  output$path<-renderLeaflet({
    
    original_stop<-unique(smp_T$from_stop_id[smp_T$trip==input$trip])
    end_stop<-unique(smp_T$to_stop_id[smp_T$trip==input$trip])
    
    doc = mp_directions(
      origin = unlist(stops[stops$stop_code==original_stop,c(8,7)]),
      destination = unlist(stops[stops$stop_code==end_stop,c(8,7)]),
      mode = "transit",
      alternatives = TRUE,
      key = key,
      quiet = TRUE
    )
    
    routes = mp_get_routes(doc)
    
    leaflet() %>% addProviderTiles(provider = providers$CartoDB.Positron) %>%
      addPolylines(data = routes, 
                   opacity = 1, 
                   weight = 7, 
                   color = ~palette("default"),
                   label = ~paste0(duration_text," ",distance_text),
                   labelOptions = labelOptions(noHide = TRUE))
  }
  )
  
}

shinyApp(ui,server)

















