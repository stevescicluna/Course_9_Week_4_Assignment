# Interactive earthquake map - selection of 1,000 South Pacific earthquakes since 1964
# Developing Data Products week 4 assignment
# Steve Scicluna
# 22 May 2019

# Load software packages
        library(shiny)
        library(leaflet)
        library(RColorBrewer)

# Set up UI
ui <- bootstrapPage(tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        titlePanel("Selected South Pacific Earthquakes since 1964"),
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(top = 10, right = 100,
                      
# Set up slider to select minimum and maximum quake magnitude in 0.1 Richter scale increments.
        sliderInput("range", "Select Minimum and Maximum Magnitude", min(quakes$mag), max(quakes$mag),
        value = range(quakes$mag), step = 0.1),

# Set up colour scheme selector to colour code quake magnitude.
        selectInput("colors", "Select Color Scheme", rownames(subset(brewer.pal.info, category %in% c("seq", "div")))),

# Set up checkbox to select whether to show colour scheme legend or not.
        checkboxInput("legend", "Show legend", TRUE)))

# Set up server
server <- function(input, output, session) {
        
# Filter quake dataset based on selected minimum and maximum magnitude.
        filteredData <- reactive({
                quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
                })
        
# Apply selected colour scheme.
        colorpal <- reactive({colorNumeric(input$colors, quakes$mag)})

# Render map using leaflet package and min/max lat/long from quakes dataset
        output$map <- renderLeaflet({
                leaflet(quakes) %>%
                        addTiles() %>%
                        fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
                })
        
# Plot quake locations on map, using selected colour scheme and circle size to denote magitude using logarithmic scale.
# Click circles to see lat, long, magnitude and depth of each quake.
        observe({
                pal <- colorpal()
                leafletProxy("map", data = filteredData()) %>%
                        clearShapes() %>%
                        addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                        fillColor = ~pal(mag), fillOpacity = 0.7,
                        popup = ~paste("Lat", lat,",", "Long", long,",", "Magnitude", mag,",", "Depth", depth, "km"))
                })
        
# If 'Show legend' is selected, show legend for selected colour scheme
        observe({
                proxy <- leafletProxy("map", data = quakes)
                proxy %>% clearControls()
                if (input$legend)
                        {pal <- colorpal()
                        proxy %>% addLegend(position = "topright", pal = pal, values = ~mag)}
                })
        }

# Run app
shinyApp(ui, server)