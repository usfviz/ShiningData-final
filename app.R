rm(list=ls())
cat("\014")

# setwd("~/Documents/2017Spring/MSAN622/project")

load_package <- function(package_name){
  if(package_name %in% rownames(installed.packages()) == FALSE) {install.packages(package_name)}
}

load_package('shiny'); library(shiny)
load_package('leaflet'); library(leaflet)
load_package('plotly'); library(plotly)
load_package('sunburstR'); library(sunburstR)
load_package('shinythemes'); library(shinythemes)
load_package('dygraphs'); library(dygraphs)
load_package('dplyr'); library(dplyr)
load_package('tidyr'); library(tidyr)
load_package('tidyquant'); library(tidyquant)
load_package('googleVis'); library(googleVis)
load_package('RColorBrewer'); library(RColorBrewer)
if('parcoords' %in% rownames(installed.packages()) == FALSE) {devtools::install_github("timelyportfolio/parcoords")}
library(parcoords)

###### Load data ######
# setwd("/Users/vyakhya.sachdeva/github/Data_Viz_Project/ShiningData-/project-prototype")
airbnb1 <- read.csv("listings.csv")
airbnb <- read.csv("listings_2.csv")
airbnb_all <- merge(x = airbnb1, y = airbnb, by = "id", all.x = TRUE)


colnames(airbnb)
airbnb$host_since <- as.Date(airbnb$host_since, "%Y-%m-%d")
airbnb$yearmonth <- format(airbnb$host_since, "%Y-%m")
airbnb$weekday <- weekdays(airbnb$host_since)
airbnb$year <- format(airbnb$host_since, "%Y")
airbnb$month <- format(airbnb$host_since, "%m")
airbnb$price <- gsub("$", "", airbnb$price)
airbnb$price <- tidyr::extract_numeric(airbnb$price)
airbnb <- airbnb[-c(6776), ]
airbnb <- airbnb[airbnb$price < 1000, ]

summary(airbnb$price)
airbnb_price <- aggregate(price ~ neighbourhood_cleansed, data = airbnb, FUN = mean)
airbnb_price$type <- 'Neighborhood'

###### Supporting function ######
# Create color palette for map scatter
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = airbnb$reviews_per_month)

# Shiny
if (interactive()) {
  options(device.ask.default = FALSE)
  ##### ui #######
  ui <- navbarPage("Airbnb dataset",
                   theme = shinytheme("cerulean"),
                   # titlePanel('Airbnb dataset'),
                   tabPanel(
                     'Geo-Scatter',
                     sidebarPanel(
                       selectInput("roomType", "Room Type", c('All', "Entire home/apt", "Private room", "Shared room")),
                       selectInput("bedType", "Bed Type", c('All', "Real Bed", "Airbed", "Couch", "Futon", "Pull-out Sofa")),
                       selectInput("cancel", "Cancellation Policy", c('All', "flexible", "strict", "moderate", "super_strict_60", "super_strict_30")),
                       sliderInput("year", tags$h5("Year of dataset:"),
                                   sep = '', min = 2008, max = 2016, value = 2016, animate = TRUE,
                                   width = '90%'
                       ),
                       fluidRow(
                         column(12, align="center",
                                plotOutput("scatterplot", height = 350, width = 180,
                                           dblclick = "scatterplot_dblclick",
                                           hover = "plot_hover",
                                           click = "table_or_click",
                                           brush = brushOpts(
                                             id = "scatterplot_brush",
                                             resetOnNew = TRUE
                                           )
                                ), 
                                tags$h5("Reset Neighborhood selection:"),
                                actionButton("reset_input", "Reset inputs", class = "btn-primary")
                         )
                       ),
                       uiOutput("hover_info")
                     ),
                     
                     mainPanel(
                       leafletOutput("SFmap", width = "1000px",height="1000px")
                     )
                   ),
                   
                   tabPanel(
                     'Parallel Coordinates',
                     sidebarPanel(
                       wellPanel(
                         selectInput("neighborhood1", "Neighborhood_1", unique(airbnb$neighbourhood_cleansed), selected = 'Mission'),
                         selectInput("neighborhood2", "Neighborhood_2", unique(airbnb$neighbourhood_cleansed), selected = 'Visitacion Valley'),
                         selectInput("neighborhood3", "Neighborhood_3", unique(airbnb$neighbourhood_cleansed), selected = 'Pacific Heights'),
                         selectInput("neighborhood4", "Neighborhood_4", unique(airbnb$neighbourhood_cleansed), selected = 'Parkside')
                       )
                     ),
                     mainPanel(
                       parcoordsOutput("parcoords", width = "900px", height = "450px")
                     )
                   ),
                   
                   tabPanel(
                     'Sankey plots',
                     sidebarPanel(
                       wellPanel(
                         selectInput("san_neighborhood1", "Neighborhood_1", unique(airbnb$neighbourhood_cleansed), selected = 'Mission'),
                         selectInput("san_neighborhood2", "Neighborhood_2", unique(airbnb$neighbourhood_cleansed), selected = 'Visitacion Valley'),
                         selectInput("san_neighborhood3", "Neighborhood_3", unique(airbnb$neighbourhood_cleansed), selected = 'Pacific Heights'),
                         selectInput("san_neighborhood4", "Neighborhood_4", unique(airbnb$neighbourhood_cleansed), selected = 'Parkside'),
                         selectInput("compare", "Factor to Compare", c('Price' = 'price', "Number of Reviews"="number_of_reviews", "Scaore Rating" = 'review_scores_rating'))
                       )
                     ),
                     mainPanel(
                       htmlOutput('sankey', width = "800px", height = "400px")
                       
                     )
                     
                   ),
                   
                   tabPanel(
                     'Sunburst',
                     sunburstOutput('sunburst', width = "100%", height = "400px")
                   ),
                   
                   tabPanel(
                     'Time-series',
                     dygraphOutput('timeseries', width = "100%", height = "400px"),
                     selectInput("factor", "Factors", c('Room Type' = 'room_type', "Bed Type" = 'bed_type', "Property Type" = 'property_type', "Neighborhood"="neighbourhood_cleansed"))
                   ),
                   
                   # tabPanel(
                   #   'Bar plot',
                   #   sidebarPanel(
                   #     wellPanel(
                   #       selectInput("bar_neighborhood1", "Neighborhood_1", unique(airbnb$neighbourhood_cleansed), selected = 'Mission'),
                   #       selectInput("bar_neighborhood2", "Neighborhood_2", unique(airbnb$neighbourhood_cleansed), selected = 'Visitacion Valley'),
                   #       selectInput("bar_neighborhood3", "Neighborhood_3", unique(airbnb$neighbourhood_cleansed), selected = 'Pacific Heights'),
                   #       selectInput("bar_neighborhood4", "Neighborhood_4", unique(airbnb$neighbourhood_cleansed), selected = 'Parkside')
                   #     )
                   #   ),
                   #   
                   #   mainPanel(
                   #     fluidRow(
                   #       column(9,
                   #              plotOutput("bar", click = "plot_click", hover = "plot_hover")
                   #       ),
                   #       column(3,
                   #              selectInput("orderby", "Order by", c('Average Price' = 'price', "Number of Reviews"="number_of_reviews", "Month availability" = 'availability_30'))
                   #       )
                   #     )
                   #   )
                   # ),
                   
                   tags$head(
                     tags$style(HTML("
                                     
                                     .multicol {
                                     
                                     -webkit-column-count: 3; /* Chrome, Safari, Opera */
                                     
                                     -moz-column-count: 3; /* Firefox */
                                     
                                     column-count: 3;
                                     
                                     }
                                     
                                     "
                      ))
      )
  )
    
  
  
  #### server ######
  server <- function(input, output,session) {
    
    # UI for hover on sider bar scatter plot
    output$hover_info <- renderUI({
      row.names(airbnb_price) <- airbnb_price$neighbourhood_cleansed
      hover <- input$plot_hover
      point <- nearPoints(airbnb_price, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 200, "px; top:", top_px+180, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Neighborhood: </b>", rownames(point), "<br/>", "<b> Average price: </b>", round(airbnb_price[rownames(point), ]$price, digits=2), "<br/>")))
      )
    })
    
    ## Interactive Scatterplot (adopted from http://shiny.rstudio.com/gallery/plot-interaction-zoom.html)
    output$scatterplot <- renderPlot({
      ggplot(airbnb_price,aes(x=type, y=price)) + theme(legend.position="bottom") + geom_boxplot() + geom_point(size = 4) + theme(text = element_text(size=20))
    })
    
    observeEvent(input$scatterplot_dblclick, {
      brush <- input$scatterplot_brush
      if (!is.null(brush)) {
        scatterplot_ranges$scatterplot_x <- c(brush$xmin, brush$xmax)
        scatterplot_ranges$scatterplot_y <- c(brush$ymin, brush$ymax)
      } else {
        scatterplot_ranges$scatterplot_x <- NULL
        scatterplot_ranges$scatterplot_y <- NULL
      }
    })
    
    # Reset Buttom
    rest <- reactiveValues(df = NULL)
    observeEvent(input$reset_input, {
      # Reset both the range and the first click, if any.
      rest$df <- subset(airbnb, year == input$year)
    })
    
    # Render Map plot
    output$SFmap <- renderLeaflet({
      row.names(airbnb_price) <- airbnb_price$neighbourhood_cleansed
      if(!is.null(input$table_or_click)){
        points <- nearPoints(airbnb_price, input$table_or_click, threshold = 5, maxpoints = 1, addDist=FALSE)  
        air_sub <- subset(airbnb, neighbourhood_cleansed == rownames(points))
      } else {
        air_sub <- airbnb
      } 
      
      if (!is.null(rest$df)){
        air_sub <- rest$df
      }
      
      if (input$roomType != 'All'){
        air_sub <- subset(air_sub, (room_type == input$roomType) & (year == input$year))
      }
      else{
        air_sub <- subset(air_sub, year <= input$year)
      }
      
      if (input$bedType != 'All'){
        air_sub <- subset(air_sub, bed_type == input$bedType)
      }
      
      if (input$cancel != 'All'){
        air_sub <- subset(air_sub, cancellation_policy == input$cancel)
      }
      
      m <- leaflet() %>%
        #addTiles() %>%
        #addProviderTiles("Stamen.Watercolor") %>%
        addProviderTiles(provider = "Esri.WorldTopoMap") %>%
        setView(lat = 37.776264, lng = -122.445297, zoom = 13) %>%
        addCircleMarkers(data = air_sub, lng = ~longitude, lat = ~latitude,
                         color = 'blue',
                         fillColor = ~pal(reviews_per_month), fillOpacity = 1, 
                         radius = ~1.2*reviews_per_month,
                         popup = ~paste("<h4>", name, "</h4>",
                                        "Price: $", price, "<br>",
                                        "Host Name: ", host_name,"<br>",
                                        "Host Since: ", host_since,"<br>",
                                        "Neighbourhood: ", neighbourhood, "<br>", 
                                        "Bed Type: ", bed_type, "<br>",
                                        "Room Type: ", room_type, "<style> div.leaflet-popup-content-wrapper { opacity: 0.8; } </style>",sep = ""))
    })
    
    # Render Bar plot
    output$bar <- renderPlot({
      neighborhood_list <- c(input$bar_neighborhood1, input$bar_neighborhood2, input$bar_neighborhood3, input$bar_neighborhood4)
      air_select <- airbnb[airbnb$neighbourhood_cleansed %in% neighborhood_list,]
      myData <- aggregate( . ~ neighbourhood_cleansed,data = air_select[, c(input$orderby, 'neighbourhood_cleansed')], FUN = function(x) c(mean = mean(x), sd = sd(x),n = length(x)))
      myData <- do.call(data.frame, myData)
      names(myData) <- c('Neighborhood', 'mean', 'sd', 'n')
      myData$se <- myData$sd / sqrt(myData$n)
      # tapply(myData$price.mean, list(myData$neighbourhood_cleansed),function(x) c(x = x))
      myData <- myData[with(myData, order(mean)), ]
      myData <- head(myData,10)
      
      dodge <- position_dodge(width = 0.9)
      limits <- aes(ymax = myData$mean + myData$se,
                    ymin = myData$mean - myData$se)
      
      order_price <- myData$Neighborhood
      ggplot(data = myData, aes(x = Neighborhood, y = mean)) +geom_bar(stat = "identity", position = dodge, fill = "dodgerblue3") +
        geom_errorbar(limits, position = dodge, width = 0.25) +
        scale_x_discrete(limits = order_price) + coord_flip() + xlab("Neighborhood") + ylab("Average") + ggtitle("Average of neighborhood") + theme(text = element_text(size=20))
    })
    
    # Render sunburst plot
    output$sunburst <- renderSunburst({
      df <- data.frame(airbnb$host_since, airbnb$room_type)
      df$year = format(df$airbnb.host_since, "%Y")
      df$property_type <- airbnb$property_type
      df$bedrooms <- paste(airbnb$bedrooms, " bedroom")
      df$bathrooms <- paste(airbnb$bathrooms, " bathroom")
      df$path = paste(df$property_type, df$airbnb.room_type, df$bedrooms, df$bathrooms, sep="-")
      df$count = rep(1, nrow(df))
      df <- na.omit(df)
      
      sunburst(
        data.frame(xtabs(count~path,df)),
        # added a degree of difficulty by providing
        # not easily sortable names
        sortFunction = htmlwidgets::JS(
          "
          function(a,b){
          abb = {
          2008:-14,
          2009:-13,
          2010:-12,
          2011:-11,
          2012:-10,
          2013:-9,
          2014:-8,
          2015:-7,
          2016:-6,
          2017:-5,
          Q1:-4,
          Q2:-3,
          Q3:-2,
          Q4:-1,
          Jan:1,
          Feb:2,
          Mar:3,
          Apr:4,
          May:5,
          Jun:6,
          Jul:7,
          Aug:8,
          Sep:9,
          Oct:10,
          Nov:11,
          Dec:12
          }
          return abb[a.name] - abb[b.name];
          }
          "
        )
        )
      })
    
    output$timeseries <- renderDygraph({
      air_groupby <- airbnb %>% group_by_('yearmonth', input$factor) %>% tally()
      names(air_groupby) <- c("yearmonth", "room_type", "n")
      air_groupby <- spread(air_groupby, room_type, n)
      air_groupby[is.na(air_groupby)] <- 0
      air_groupby <- air_groupby[-nrow(air_groupby),] 
      air_groupby <- xts(air_groupby[,-1], order.by=as.yearmon(air_groupby$yearmonth, "%Y-%m"))
      dygraph(air_groupby) %>% dyRangeSelector()  %>% dyOptions(stackedGraph = TRUE)
    })
    
    output$sankey <- renderGvis({
      neighborhood_list <- c(input$san_neighborhood1, input$san_neighborhood2, input$san_neighborhood3, input$san_neighborhood4)
      air_select <- airbnb[airbnb$neighbourhood_cleansed %in% neighborhood_list,]
      summaryPrice <- quantile(air_select[,c(input$compare)], na.rm = TRUE)
      if (summaryPrice[4] == summaryPrice[5]){
        air_select$precentage <- cut(air_select$price, c(summaryPrice[[1]],summaryPrice[[2]],summaryPrice[[3]],summaryPrice[[4]]), labels=c('0%-25%', '25%-50%', '50%-100%'))
      }
      else{
        air_select$precentage <- cut(air_select$price, c(summaryPrice[[1]],summaryPrice[[2]],summaryPrice[[3]],summaryPrice[[4]],summaryPrice[[5]]), labels=c('0%-25%', '25%-50%', '50%-75%', '75%-100%'))
        
      }
      air_groupby <- air_select %>% group_by(neighbourhood_cleansed, precentage) %>% tally()
      names(air_groupby) <- c("From", "To", "Weight")
      gvisSankey(air_groupby, from="From", to="To", weight="Weight", options=list(width="800px"))
    }) 
    
    output$parcoords = renderParcoords({
      neighborhood_list <- c(input$neighborhood1, input$neighborhood2, input$neighborhood3, input$neighborhood4)
      air_select <- airbnb[airbnb$neighbourhood_cleansed %in% neighborhood_list,]
      
      parcoords(
        air_select[,c('neighbourhood_cleansed', 'room_type', 'bedrooms', 'bed_type', 'price', 'review_scores_rating','reviews_per_month')]
        , rownames=F
        , brushMode="1d"
        , color = list(
          colorScale = htmlwidgets::JS(sprintf(
            'd3.scale.ordinal().range(%s).domain(%s)'
            ,jsonlite::toJSON(RColorBrewer::brewer.pal(4,'Set1'))
            ,jsonlite::toJSON(as.character(unique(airbnb[['neighbourhood_cleansed']])))
          ))
          ,colorBy = 'neighbourhood_cleansed'
        )
      )
    })
    }
  shinyApp(ui, server)
}