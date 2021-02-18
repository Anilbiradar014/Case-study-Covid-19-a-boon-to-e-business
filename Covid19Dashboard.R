#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#

library(shiny)
library(covid19.analytics)
library(dplyr)
library(rstan)
library(prophet)
library(lubridate)
library(ggplot2)
library(shinythemes)
library(plotly)
library(RColorBrewer)
library(leaflet)
library(countrycode)
library(readr)
library(tidyr)
library(shinyWidgets)
library(shinydashboard) 

tsc <- covid19.data(case = 'ts-confirmed')
tsc %>% head(5)
#ts-deaths
#ts-recovered
tsc %>% tail()
ag <- covid19.data(case = 'aggregated')
timeseries_confirmed <- covid19.data('ts-confirmed')
country_list=as.list(tsc$Country.Region)


tsc %>% dim()
tsc
tsc_col=dim(tsc)
column_tsc=tsc_col[1]
column_tsc
tsc_col


# Code for World Map 
timeseries_ISOCode <-timeseries_confirmed
timeseries_ISOCode$iso_code <- countrycode(timeseries_confirmed$Country.Region, 'country.name', 'iso3c')


timeseries_ISOCode <- read.csv('covid19data.csv')
timeseries_ISOCode %>% head()
dim(timeseries_ISOCode)
timeseries_ISOCode <- timeseries_ISOCode %>% gather(c(5:252),key = 'Date', value = 'Confirmed_Cases')
timeseries_ISOCode %>% head()

timeseries_ISOCode$Date <-substring(timeseries_ISOCode$Date,2)
timeseries_ISOCode %>% head()
timeseries_ISOCode %>% head()
timeseries_ISOCode$NewDate <- as.Date(timeseries_ISOCode$Date,format="%Y.%m.%d")
timeseries_ISOCode %>% head()
# ----------------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage(
    "COVID19: A Boon To E-Business",
    tabPanel("Global COVID 19 Cases",
             sidebarLayout(
               sidebarPanel(
                 h2("The COVID Graph"),
                 p("This graphs allows you to understand the way the COVID-19 cases have increased across the globe"),
                 p("Please use the PLAY button located below the seek bar to visualize the growth of COVID-19.")
               ),
               mainPanel(
                 fluidRow(
                   box(plotlyOutput("worldwidePlot"), width = 40),
                 ),
                 div(style = "margin: auto; width: 150%;margin-left: 10%", 
                     sliderInput("DatesMerge",
                                 "Dates:",
                                 min = as.Date("2020-01-22","%Y-%m-%d"),
                                 max = as.Date("2020-09-25","%Y-%m-%d"),
                                 value=as.Date("2020-01-22"),
                                 timeFormat="%Y-%m-%d",
                                 animate = animationOptions(interval = 30, loop = FALSE)))
               )
             )
    ),
    tabPanel("Graphs",
             sidebarLayout(
               sidebarPanel(
                 selectInput("Graphcountry","Countries",
                             choices=country_list,selected="US"
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   type="tabs",
                   tabPanel("Cases",plotOutput("tots",width = "900px", height = "600px")),
                   tabPanel("Growth",plotOutput("growth",width = "900px", height = "600px"))
                 )
               )
             )
    ),
    tabPanel("Model",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "country" ,
                   "Countries",
                   choices = country_list,selected="US")
                 
               ),
               mainPanel(tabsetPanel(
                 type = "tab",
                 tabPanel("SIR", plotOutput("sir", width = "900px", height = "600px")),
                 tabPanel("Prophet",plotOutput("prophet", width = "900px", height = "600px")),
                 tabPanel("Linear", plotOutput("linear", width = "900px", height = "600px"))
                 
                 
               ))
             ),tags$br() ,tags$br() ,tags$br() ,tags$br() ,tags$br() ,tags$br(), 
             tags$h5("WIL PROJECT-50")
    ),
    tabPanel("Developers",
             sidebarLayout(
               sidebarPanel(
                 p("COVID-19: A Boon To E-Business")
               ),
               mainPanel(
                 p("This ShinyApp was created as part of Case Studies in Data Science Final Project Report"),
                 p("By"),
                 p("Anil Kumar Biradar s3798024"),
                 p("Amogha Amaresh s3789160"),
                 p("Aravind Sanjeevi s3755232"),
                 p("Harshitha Reddy s3797186"),
                 p("Shivani Deshmukh s3805479")
               )
             )
    )
  )
)



server <- function(input, output) {
  
  output$selected_country <- renderText({
    paste("The country you have selected is",input$country)
  })
  
  output$sir=renderPlot({
    print("SIR printing")
    generate.SIR.model(timeseries_confirmed, input$country)
  })
  
  
  output$prophet=renderPlot({
    
    tsc <- covid19.data(case = 'ts-confirmed')
    tsc <- tsc %>% filter(Country.Region == input$country)
    tsc <- data.frame(t(tsc))
    tsc <- cbind(rownames(tsc), data.frame(tsc, row.names = NULL))
    colnames(tsc) <- c('Date', 'Confirmed')
    tsc <- tsc[-c(1:4),]
    tsc$Date <- ymd(tsc$Date)
    tsc$Confirmed <- as.numeric(tsc$Confirmed)
    ds <- tsc$Date
    y <- tsc$Confirmed
    df <- data.frame(ds, y)
    
    
    # Forecasting
    m <- prophet(df)
    
    
    # Prediction
    future <- make_future_dataframe(m, periods = 100)
    forecast <- predict(m, future)
    
    
    # Plot forecast
    
    #plot(m, forecast)
    prophet_plot_components(m, forecast)
    #dyplot.prophet(m, forecast)
    
    
    
  })
  
  output$linear=renderPlot({
    print("linear printing")
    tots.per.location(timeseries_confirmed, geo.loc =input$country)
    
  })
  output$tots=renderPlot({
    #ag <- covid19.data(case = 'aggregated')
    #timeseries_confirmed <- covid19.data('ts-confirmed')
    ts_all <- covid19.data('ts-all')
    #report.summary(Nentries = 10, graphical.output = F)
    #report.summary(Nentries = 10, graphical.output = T)
    #options(scipen = 999)
    totals.plt(ts_all, c(input$Graphcountry))
    
  })
  output$growth=renderPlot({
    growth.rate(timeseries_confirmed, geo.loc =input$Graphcountry)
    
  })
  output$worldwidePlot <- renderPlotly({
    l <- list(color = toRGB("black"), width = 0.5)
    TimeSelectedData <- reactive({
      
      timeseries_ISOCode%>%filter(NewDate == input$DatesMerge)
      
    })
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    print(input$DatesMerge)
    fig <-
      plot_ly(
        timeseries_ISOCode,
        type = 'choropleth',
        locations = timeseries_ISOCode$iso_code,
        z = timeseries_ISOCode$Confirmed_Cases,
        frame = ~countrycases$Date,
        colorscale = "Purples"
      )
    fig <- plot_geo(TimeSelectedData())
    fig <- fig %>% add_trace(
      z = ~Confirmed_Cases, color = ~Confirmed_Cases, colors = 'Reds',
      text = ~Country.Region, locations = ~iso_code, marker = list(line = l)
    )
    fig <- fig %>% colorbar(title = 'Confirmed_Cases', tickprefix = '')
    
    
    fig})
}


# Run the application 
shinyApp(ui = ui, server = server)