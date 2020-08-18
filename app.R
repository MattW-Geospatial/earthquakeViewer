
# app development
library(shiny)

# data handling
library(tidyverse)
library(lubridate)

# mapping/ spatial data
library(tmap)
library(leaflet)
library(sf)

# better tables
library(DT)

# web/ json
library(httr)
library(jsonlite)
library(geojsonio)

# plotting
library(ggplot2)
library(ggthemes)
library(plotly)



## Global functions and data #######################################################################

getEarthquakes <- function(bbox = c(164.0,-49.0,180.0,-32.0), 
                           startdate = today()-30, 
                           enddate = today()) {
    #' getEarthquakes:
    #' Pulls earthquakes from the GeoNet API and returns an sf object.
    #' Defaults to last 30 days and all of New Zealand.
    #' 
    #' Usage:
    #' getEarthquakes(bbox = c(164.0,-49.0,180.0,-32.0), 
    #'                startdate = today()-30, 
    #'                enddate = today() )
    #'                
    
    query = list(bbox = paste(as.character(bbox), collapse=","), 
                 startdate = startdate, 
                 enddate = enddate)
    
    EQ.URL <- "https://quakesearch.geonet.org.nz/geojson"
    EQ.GeoJSON <- GET(EQ.URL, query = query)
    
    EQ.data <- rawToChar(EQ.GeoJSON$content) %>% 
        geojson_sf() %>%                                 # convert to sf
        filter(magnitude > 0) %>%                        # remove a few errors
        mutate(linM = 10^magnitude) %>%                  # for plotting
        mutate(origintime = as_datetime(origintime, tz = "NZ")) %>% # process time string to a date
        mutate(modificationtime = as_datetime(modificationtime)) %>%
        mutate(dateNZ = as_date(origintime, tz = "NZ"))  # the date in NZ, converted from UTC
    
    return(EQ.data)
}


aboutText <- 
  "<h3>NZ Earthquake viewer</h3>
  <p>This is an R Shiny app which demonstrates web queries to map and plot New Zealand earthquakes which were recorded by <a href=/'https://api.geonet.org.nz//'>GeoNet</a>.</p>
  <p>&nbsp;</p>
  <hr/>
  <h4>Data providers</h4>
  <p>All data presented are copyright of the respective owners and they are gratefully acknowledged:</p>
  <ul>
    <li>Earthquake data: <a href='https://api.geonet.org.nz//'>GeoNet</a>. Data license: <a href='http://creativecommons.org/licenses/by/3.0/nz//'>Creative Commons Attribution 3.0 New Zealand</a>. Copyright <a href='https://www.gns.cri.nz//'>GNS Science</a>.</li>
    <li>Active fault data: <a href='https://data.gns.cri.nz/af//'>GNS Science</a>, as detailed in <a href='https://dx.doi.org/10.1080/00288306.2015.1112818'>Langridge et al. (2016)</a>. The data were obtained in August 2020 and have a scale of 1:250,000: they should not be used at larger scales.</li>
  </ul>
  <hr/>
  <h4>Source code</h4>
  <p>The R source code for the app <a href='https://github.com/MattW-Geospatial/earthquakeViewer'>can be obtained from Github here</a>.</p>
  <hr/>
  <h4>Version history</h4>
  <ul>
    <li>0.1   : 2020-08-14: Initial alpha release</li>
  </ul>
  <hr/>
  <h4>App licence</h4>
  <p>&#169; Copyright 2020 Matt Wilson/ Geospatial Research Institute Toi Hangarau</p>
  <p>Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the \"Software\"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:</p>
  <p>The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.</p>
  <p>THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.</p>
"

# Load in fault lines and create a tmap object to add to maps
faultlines <- st_read("NZAFD_Aug_2020_WGS_84.shp") %>% 
  tm_shape(name = "Active Faults") + tm_lines(col = "#cdcdcd")


## User interface #################################################################################

# Define UI for application
ui <- navbarPage("NZ Earthquake viewer", id="nav",
                 
                 
                 # Set up the different tabs:
                 ## Map - main panel #############################################################################
                 tabPanel("Map",
                          div(class="outer",
                              
                              tags$head( # Headers for the web page
                                  # include a custom CSS: important for a full screen map
                                  includeCSS("styles.css"),
                                  
                                  # include a custom JavaScript function
                                  #includeScript("gomap.js") 
                              ),
                              
                              # If not using custom CSS, set height of tmapOutput to a number instead of percent
                              tmapOutput(outputId = "map", width="100%", height="100%"),
                              
                              # Create a nice panel to store some controls
                              absolutePanel(id = "controls", 
                                            draggable = FALSE, left = 40, bottom = 40,
                                            width = "auto", height = "auto",
                                            
                                            # Date selector for loading new data. Defaults to last 30 days.
                                            dateRangeInput(inputId = "daterange", h4("Date range:"),
                                                           start = today()-30,
                                                           end = today(),
                                                           max = today()),
                                            
                                            selectInput(inputId = "quickselect", label = "Quick select:", 
                                                        choices = c("[select]" = "none",
                                                                    "Kaikoura, November 2016" = "kaikoura",
                                                                    "Christchurch, February 2011" = "christchurch",
                                                                    "Canterbury/ Darfield, September 2010" = "canterbury",
                                                                    "Last year" = "lastyear")
                                            ),
                                            
                                            # Load new data only when button is pushed (not when one of the dates is changed)
                                            actionButton("loadButton", "Load"), 
                                            
                                            hr(), 
                                            
                                            # Allow filtering of earthquakes by magnitude
                                            sliderInput(inputId = "magnitudeSlider", label = h4("Magnitude range:"), 
                                                        value = c(0, 9), min = 0, max = 9),
                                            
                                            # Allow selection of plotting depth or magnitude
                                            radioButtons(inputId = "var", label = h4("Variable:"), 
                                                         choices = c("Magnitude" = "magnitude", "Depth" = "depth"),
                                                         inline = TRUE)
                                            
                              ),
                              
                              tags$div(id="cite",
                                       'Data credit: ', tags$em('GeoNet'), ', https://api.geonet.org.nz/.'
                              )
                          )
                 ),
                 
                 
                 ## Data table panel #########################################################################
                 tabPanel("Data table",
                          DT::dataTableOutput(outputId = "datatable"),
                          tags$div(id="cite",
                                   'Data credit: ', tags$em('GeoNet'), ', https://api.geonet.org.nz/.'
                          )
                 ),
                 
                 
                 ## Plots panel ##############################################################################
                 tabPanel("Plots",
                          
                          # Add some plots here for selected data
                          # Scatterplot of time (x) vs magnitude (y). Size point by magnitude, coloured by depth.
                          # Histogram of magnitude 
                          # Histogram of depth
                          plotlyOutput("scatterPlot"),
                            
                          
                          fluidRow(
                            column(6,
                                plotlyOutput("histMag")
                            ),
                            column(6,
                                plotlyOutput("histDepth")                    
                            )
                          )
                          

                 ),
                 
                 
                 ## About panel ##############################################################################
                 tabPanel("About",
                          htmlOutput("about")
                 )
                 
)



## Define server logic ########################################################################

server <- function(input, output, session) {
    
    # Specify earthquakes object here so that it is available to all functions within server
    # initialise earthquakes
    EQ.data <- getEarthquakes() # master dataset
    Earthquakes <- EQ.data      # dataset copy which will be filtered as needed
    
    
    # This helper function will return Earthquakes based on selected values - 
    # at the moment, based on the slider input for magnitude. 
    # Defaults to the master dataset: note that this is important, 
    # so that we don't loose data through previous filtering.
    processEarthquakes <- function(AllEQs = EQ.data) {
        cat("processEarthquakes (filter) | ")
        
        SelectedEQs <- AllEQs %>%
            filter(magnitude >= input$magnitudeSlider[1]) %>%
            filter(magnitude <= input$magnitudeSlider[2])
        
        return(SelectedEQs)
    }
    
    
    # Create a function which will return a tmap object to add to the basemap. Creating this 
    # as a function means that it is easier to make style changes. The default values are
    # used for initialisation, then changed as needed.
    earthquakeLayer <- function(data, var = "magnitude", breaks = seq(0,8), palette = "YlOrRd", title.col = "Magnitude") {
        cat("earthquakeLayer | ")
      
        tm_shape(data, name = "Earthquakes") +
            tm_bubbles(size = "linM", size.max = 10^8,
                       col = var, palette = palette,
                       style = "fixed", breaks = breaks,
                       id = "origintime", title.col = title.col,
                       popup.vars = c("magnitude","depth"),
                       scale = 10, alpha = 0.5,
                       zindex = 401) # zindex is specified so that the layer can be removed
    }
    
    # When the loadButton event happens, obtain earthquakes for dates specified 
    # in the dateRangeInput(), inputId = "daterange"
    observeEvent(input$loadButton, {
        cat("oberveEvent (loading Earthquakes) | ")
        # The use of <<- saves EQ.data back to the object previously defined within server()
        # The use of a master object enables the data to be reset without a need to reload. 
        EQ.data <<- getEarthquakes(startdate = input$daterange[1], 
                                   enddate = input$daterange[2])
    })
    
    # Set the dates to events of interest
    observeEvent(input$quickselect, {
        cat("quickselect | ")
        if (input$quickselect == "none") {
            cat("do nothing | ")
        } else if (input$quickselect == "kaikoura") {
            cat("event selected: kaikoura | ")
            updateDateRangeInput(session, inputId = "daterange", 
                                 start = as_date("2016-11-13"),
                                 end = as_date("2016-11-21"))
        } else if (input$quickselect == "christchurch") {
            cat("event selected: christchurch | ")
            updateDateRangeInput(session, inputId = "daterange", 
                                 start = as_date("2011-02-21"),
                                 end = as_date("2011-03-01"))
        } else if (input$quickselect == "canterbury") {
            cat("event selected: canterbury | ")
            updateDateRangeInput(session, inputId = "daterange", 
                                 start = as_date("2010-09-03"),
                                 end = as_date("2010-09-11"))
        } else if (input$quickselect == "lastyear") {
            cat("event selected: lastyear | ")
            updateDateRangeInput(session, inputId = "daterange", 
                                 start = today()-365,
                                 end = today())
        }
        
    })
    
    # Create an event listener, so that the map and datatable can react
    # to multiple different events
    eventListen <- reactive({
        cat("reactive (eventListen) | ")
        
        # Task 1(b)
        # Add the input for your sliderInput() to the list below so that the map
        # and table respond to changes on the slider. e.g. if the inputId of your 
        # slider is "magnitudeSlider", then you need to add input$magnitudeSlider
        
        # Task 2(b)
        # Add the radioButton() input to this list.
        list(input$loadButton,
             input$magnitudeSlider,
             input$var)
    })
    
    
    
    
    ## Map Tab ####################################################################################
    
    # Initialise the map with the app first starts
    output$map <- renderTmap( {
        cat("renderTmap (initialise map) | ")
        
        # Initialise the map with some nice basemaps and the Earthquakes data, rendered using
        # earthquakeLayer() with default settings
        tm_basemap(c("Esri.OceanBasemap","CartoDB.DarkMatter","OpenStreetMap.Mapnik"),alpha = 0.7) +
            faultlines +
            earthquakeLayer(Earthquakes)
    })
    
    # Update the map when events in the eventListen() reactive happen
    observeEvent(eventListen(),{
        cat("observeEvent (update map) | ")
        
        # Process the earthquake data, using <<- to save back to the 
        # object created within server()
        Earthquakes <<- processEarthquakes() # update the Earthquakes object (i.e. filter etc)
        
        if (nrow(Earthquakes) == 0) {
            showNotification("No Earthquakes.", type = "message", duration = 3)
            cat("No data, don't update map | ")
            return() 
        }
        
        var <- input$var # Get the variable selected in the radioButton
        
        # Define the breaks and palette depending on the selected variable
        if(var == "magnitude") {
            breaks <- seq(0,8)
            palette <- heat.colors(8, rev = TRUE)
            title.col <- "Magnitude"
        } else if(var == "depth") {
            breaks <- c(0,15,40,100,200,1000)
            palette <- "-viridis"
            title.col <- "Depth (km)"
        }
        
        
        # Use tmapProxy to update the map: this means that it won't be completely
        # recreated each time something changes 
        tmapProxy("map", session, {
            tm_remove_layer(401) +  # remove the old earthquakes layer, specified by the zindex
                earthquakeLayer(Earthquakes,var = var, palette = palette, breaks = breaks, title.col = title.col) # add the new layer
            
        })
    }, ignoreInit = TRUE)
    
    
    
    ## Data table Tab #############################################################################
    
    # Create an eventReactive which produces an updated table when 
    # events in the eventListen() reactive happen
    dataTable <- eventReactive(eventListen(), { 
        
        # Obtain the set of required earthquakes
        Earthquakes <<- processEarthquakes()
        
        DT::datatable(data = Earthquakes, escape = FALSE)
    }, ignoreNULL=FALSE)
    
    # Produce the table with updated data: when dataTable changes, 
    # this will react and run
    output$datatable <- DT::renderDataTable({
        dataTable()
    })
    
    
    
    ## Plots Tab ##################################################################################
    
    observeEvent(eventListen(),{

        output$histMag <- renderPlotly({
            ggplotly(
                ggplot(data = Earthquakes, aes(magnitude)) + 
                    geom_histogram() + 
                    theme_gdocs()
            )
        })
        output$histDepth <- renderPlotly({
          ggplotly(
            ggplot(data = Earthquakes, aes(depth)) + 
              geom_histogram() + 
              theme_gdocs()
          )
        })
        
        
       output$scatterPlot <- renderPlotly({
          ggplotly(
            ggplot(data = Earthquakes, aes(x = origintime, y = magnitude, size = linM, color = depth)) + 
              geom_point(alpha = 0.7) + 
              scale_size(limits = c(0.1, 10^8), range = c(1,12)) +
              theme_gdocs()
          )
          
          
        })

    })
    
    
    
    
    ## About Tab ##################################################################################
    
    ## Add text to the About tab
    output$about <- renderText({ aboutText })
    
    
}


## Run the application ##########################################################################

shinyApp(ui = ui, server = server)


