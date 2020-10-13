library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(tidycovid19)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(DT)


#retreiving data
aus_states_data <- download_jhu_csse_covid19_data(type = "country_region", cached = TRUE) %>%
  filter(iso3c == "AUS") %>%
  rename("Date" = date,
         "Confirmed" = confirmed,
         "Deaths" = deaths,
         "Recovered" = recovered)

#calculating daily counts from cumulative counts
daily_counts <- aus_states_data %>%
  group_by(region) %>%
  arrange(Date) %>%
  mutate(daily_confirmed = replace_na((Confirmed - lag(Confirmed)),0),
         daily_deaths = replace_na((Deaths - lag(Deaths)),0),
         daily_recovered = replace_na((Recovered - lag(Recovered)),0)) %>%
  select(c(-Confirmed,-Deaths,-Recovered)) %>%
  rename("Confirmed" = daily_confirmed,
         "Deaths" = daily_deaths,
         "Recovered" = daily_recovered)

latest_vals <- aus_states_data %>%
  filter(Date == max(Date))



ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "map"),
      menuItem("Trend of spread", tabName = "states"),
      menuItem("Daily number of cases", tabName = "datatable"),
      menuItem("About", tabName = "about")
      )),
  
  dashboardBody(
    tags$style(type = "text/css", "#ausmap {height: calc(100vh - 340px) !important;}"),
    
    tabItems(
      tabItem("map",
              fluidPage(
              box(width = NULL,
                  fluidRow(align = "center",tags$h2("Australia COVID-19 Dashboard")),
                  fluidRow(column(12, align = "right", tags$h4(paste0("Last updated : ", max(aus_states_data$Date))))),
                  valueBox(sum(latest_vals$Confirmed), "Confirmed Cases", icon = icon("lungs-virus")),
                  valueBox(sum(latest_vals$Deaths), "Deaths", icon = icon("skull-crossbones"), color = "red"),
                  valueBox(sum(latest_vals$Recovered), "Recovered", icon = icon("head-side-mask"), color = "green"),
                  column(6,
                         leafletOutput("ausmap"),
                         absolutePanel(top = 50, left = 60,
                                       radioGroupButtons(inputId = "casetype", label = NULL, choices = c("Confirmed", "Deaths", "Recovered"), selected = "Confirmed"))),
                  column(6, tags$h4(paste0("Total number of cases reported at ", max(aus_states_data$Date))),
                         dataTableOutput("stateslatestdatatable")
                  )))),
      
      
      tabItem("states",
              fluidPage(
              fluidRow(
                box(width = NULL,
                    fluidRow(align = "center",tags$h2("Trend of COVID-19 cases across the Australian states")),
                    fluidRow(column(12, align = "right", tags$h4(paste0("Last updated : ", max(aus_states_data$Date))))),
                  column(5, selectInput("state", label = "Select State :", choices = unique(aus_states_data$region))),
                  column(7,sliderInput(inputId = "date", label = "Select Date range:",
                                     min = min(aus_states_data$Date), max = max(aus_states_data$Date), 
                                     value = max(aus_states_data$Date), width = "600px"))
                  )),
              fluidRow(
                tabBox(width = NULL, id = "trendplots", height = "250px",
                  tabPanel("Cumulative Cases Trend", plotlyOutput("cumtrendplot"),
                           tags$h5("Hover over the plots to view a summary of the cumulative case count at date"),
                           dataTableOutput("hover")),
                  tabPanel("Daily Cases Trend",
                           radioGroupButtons(inputId = "casetypedailytrend", label = NULL, choices = c("Confirmed", "Deaths", "Recovered"), selected = "Confirmed"),
                           plotlyOutput("dailytrendplot"),
                           fluidRow(column(12, align = "right", "*the negative values may be attributed to the reversal of inaccurate reporting"))
                           )))
              )),
      
      
      tabItem("datatable",
              fluidPage(
                box(width = NULL,
                    fluidRow(align = "center",tags$h2("Daily number of COVID-19 cases reported in Australia")),
                    fluidRow(column(12, align = "right", tags$h4(paste0("Last updated : ", max(aus_states_data$Date))))),
                    dateInput("datatabledate", "Select Date : ",
                              value = max(daily_counts$Date),
                              min = min(daily_counts$Date),
                              max = max(daily_counts$Date),
                              width = 400),
                    valueBoxOutput("vboxconfirmed"),
                    valueBoxOutput("vboxdeaths"),
                    valueBoxOutput("vboxrecovered"),
                    dataTableOutput("datatable")
              ))),
      
      
      tabItem("about",
              fluidPage(
                box(width = NULL,
                    tags$h1("About"),
                    tags$h4("The aim of this dashboard is to visualise the spread of COVID-19 in Australia and compare the spread across states.
                            The dashboard visualizes the number of confirmed COVID-19 cases, recoveries and deaths reported due to COVID-19
                            in Australia at date, as well as the trend of spread across the states over time. Further, the dashboard also
                            provides the daily case numbers reported in each state."),
                    tags$h2("Background"),
                    tags$h4("In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China.
                            Caused by a new type of coronavirus, the disease is now commonly referred to as COVID-19 and
                            was first confirmed in Australia in late January 2020. Since then, COVID-19 has spread across all states of Australia
                            and caused many deaths."),
                    tags$h2("Data Source"),
                    tags$a(href="https://github.com/CSSEGISandData/COVID-19",
                    tags$h4("COVID-19 Data Repositiory by the Center for Systems Science and Engineering at John Hopkins University")),
                    tags$h4(paste0("The data was last updated on ", max(aus_states_data$Date),", and is updated daily")),
                    tags$h2("Creator"),
                    tags$h4("Dilinie Seimon")
              )))
      )
  )
)



server <- function(input, output) {
  
  #chloropleth map
  output$ausmap <- renderLeaflet({
    
    selected_col <- switch(input$casetype,
                           "Confirmed"="Blues",
                           "Deaths"="Reds",
                           "Recovered"="Greens")
    
    states_data <-  read_sf("https://raw.githubusercontent.com/rowanhogan/australian-states/master/states.geojson") %>%
      left_join(latest_vals %>% select(region,selected=input$casetype), by=c("STATE_NAME" = "region"))
    
    pal <- colorQuantile(selected_col, domain = states_data$selected, n = 8)
    
    leaflet(states_data, options = leafletOptions(minZoom = 3)) %>%
      addTiles() %>%
      addPolygons(fillOpacity=0.8,
                  color=~pal(selected),
                  weight=2,
                  highlight = highlightOptions(weight = 3, bringToFront = TRUE),
                  layerId = ~STATE_NAME,
                  label = ~STATE_NAME)%>%
      addLegend(pal = pal, values = ~selected, opacity = 0.7, title = "Proportion of spread", position = "bottomright") %>%
      fitBounds(110.246193, -45.322817, 155.226126, 0.088012)
    
    
  })
  
  #cumulative data table
  output$stateslatestdatatable = renderDataTable({
    latest_vals %>%
      select(region, Confirmed, Deaths, Recovered) %>%
      datatable(options = list(dom = 'Bts'),
                rownames = FALSE,
                colnames = c("State" = 1, "Confirmed" = 2, "Deaths" = 3, "Recovered" = 4))
  })
  
  #cumulative trend plot
  output$cumtrendplot <- renderPlotly({
    ggplotly(
      aus_states_data %>%
        filter(region == input$state,
               Date <= input$date) %>%
        ggplot(aes(x = Date,
                   text = paste('State:' , input$state))) +
        geom_line(aes(y = Confirmed, color = "Confirmed")) +
        geom_line(aes(y = Deaths, color = "Deaths")) +
        geom_line(aes(y = Recovered, color = "Recovered")) +
        labs(x = "Date",
             y = "Cumulative Cases") +
        theme_light() +
        scale_colour_manual(name="Case Type",
                            values=c("Confirmed"="#42c5f5",
                                     "Deaths" = "#d43928",
                                     "Recovered" = "#0ca654")) +
        scale_x_date(date_breaks = "months" , date_labels = "%b-%y"),
      tooltip = c("text", "x", "y")
    ) %>%
      config(displayModeBar = F)
  })
  
  #cumulative counts on hover datatable
  output$hover <- renderDataTable({
    hover_data <- event_data("plotly_hover")
    req(hover_data)
    aus_states_data %>%
      filter(region == input$state,
             Date == as.Date(origin = "1970-01-01", hover_data$x)) %>%
      mutate(PercentDeaths = paste0(round((Deaths /Confirmed)*100, digits = 2),"%"),
             PercentRecoveries = paste0(round((Recovered / Confirmed)*100, digits = 2),"%")) %>%
      select(region, Date, Confirmed,Deaths, PercentDeaths, Recovered, PercentRecoveries) %>%
      rename("Number of Confirmed Cases" = Confirmed,
             "Number of Deaths" = Deaths,
             "Number of Recoveries" = Recovered,
             "Percentage of Deaths" = PercentDeaths,
             "Percentage of Recoveries" = PercentRecoveries) %>%
      datatable(options = list(dom='t',ordering=F,
                               columnDefs = list(list(className = 'dt-right', targets = 2:6))),
                rownames = FALSE,
                colnames = c("State" = 1))
  })
  
  #daily trend plot
  output$dailytrendplot <- renderPlotly({
    ggplotly(
      daily_counts %>%
        filter(region == input$state,
               Date <= input$date) %>%
        ggplot(aes_string(x = 'Date', y = input$casetypedailytrend)) +
        geom_col()+
        labs(x = "Date",
             y = "Daily Cases") +
        theme_light() +
        scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
    ) %>%
      config(displayModeBar = F)
  })
  
  #daily counts filtered table
  output$datatable = renderDataTable({
    daily_counts %>%
      filter(Date == input$datatabledate) %>%
      select(region, Confirmed, Deaths, Recovered) %>%
      datatable(options = list(dom = 'Bts'),
                rownames = FALSE,
                colnames = c("State" = 1, "Confirmed" = 2, "Deaths" = 3, "Recovered" = 4))
  })
  
  #daily confirmed cases fiiltered valuebox
  output$vboxconfirmed <- renderValueBox({
    valueBox(
      sum((daily_counts %>% filter(Date == max(input$datatabledate)))$Confirmed),
      "Confirmed Cases",
      icon = icon("lungs-virus")
    )
  })
  
  #daily deaths fiiltered valuebox
  output$vboxdeaths <- renderValueBox({
    valueBox(
      sum((daily_counts %>% filter(Date == max(input$datatabledate)))$Deaths),
      "Deaths",
      icon = icon("skull-crossbones"),
      color = "red"
    )
  })
  
  #daily recoveries fiiltered valuebox
  output$vboxrecovered <- renderValueBox({
    valueBox(
      sum((daily_counts %>% filter(Date == max(input$datatabledate)))$Recovered),
      "Recovered",
      icon = icon("head-side-mask"),
      color = "green"
    )
  })
  
}


shinyApp(ui = ui, server = server)