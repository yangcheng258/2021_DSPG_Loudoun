#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(leaflet.providers)


map <- read_excel(paste0(getwd(),"/data/combined-programs.xlsx")) 

loudoun_locations <- map %>%
    filter(County == "Loudoun") %>% 
    rename(Longitude = Latitude, Latitude = Longitude) %>%
    select(Program, Longitude, Latitude, Pillars, Subpopulation, Qualification, Description, Website) %>%
    filter(Longitude != "Online" & Longitude != "Mulitple locations") %>% drop_na()

loudoun_locations$Longitude <- as.numeric(loudoun_locations$Longitude)
loudoun_locations$Latitude <- as.numeric(loudoun_locations$Latitude)

allegheny_locations <- map%>%
    filter(County == "Allegheny") %>% 
    rename(Longitude = Latitude, Latitude = Longitude) %>%
    select(Program, Longitude, Latitude, Pillars, Subpopulation,  Qualification, Description, Website) %>%
    filter(Longitude != "Online" & Longitude != "Mulitple locations") %>% drop_na()

allegheny_locations$Longitude <- as.numeric(allegheny_locations$Longitude)
allegheny_locations$Latitude <- as.numeric(allegheny_locations$Latitude)


subpop_levels <- c("Foster Care", "Juvenile Detention")
subpop_pal <- colorFactor(pal = c('red', 'green'),
                          levels = subpop_levels)

Pillar_levels <- unique(loudoun_locations$Pillars)
Pillar_pal <- colorFactor(pal = c('red', 'yellow', 'blue', 'orange', 'green', 'pink'), 
                          levels = Pillar_levels)






# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"), 
           leafletOutput("map1"), 
           br(), 
           leafletOutput("map2"),
           br(), 
           leafletOutput("map3"),
           br(), 
           leafletOutput("map4")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$map1 <- renderLeaflet({
        
        
        locations_foster <- loudoun_locations %>% filter(Subpopulation == "Foster Care")
        locations_juvie <- loudoun_locations %>% filter(Subpopulation == "Juvenile Detention")
        
        
        l_sub <- leaflet(options = leafletOptions(minzoom = 12)) %>% 
            setView(lng = -77.431622, lat = 38, zoom = 7) %>% 
            addProviderTiles("CartoDB") %>% 
            addCircleMarkers(data = locations_juvie, lng = ~Longitude, 
                             lat = ~Latitude, 
                             popup = ~paste0("<b>", locations_juvie$Program,"</b>", "<br/>", "<b>", 
                                             "Qualifications: ", "</b>", locations_juvie$Qualification, 
                                             "<br/>","<b>","Description: ", "</b>", locations_juvie$Description, 
                                             "<br/>","<b>","Website: ", "</b>", "<a>",locations_juvie$Website, "</a>"), 
                             group = "Juvenile Detention", radius = 2, color = ~subpop_pal(Subpopulation)) %>% 
            addCircleMarkers(data = locations_foster, lng = ~Longitude, 
                             lat = ~Latitude, 
                             popup = ~paste0("<b>", locations_foster$Program,"</b>", "<br/>", "<b>", 
                                             "Qualifications: ", "</b>", locations_foster$Qualification,
                                             "<br/>","<b>","Description: ", "</b>", locations_foster$Description,
                                             "<br/>","<b>","Website: ", "</b>", "<a>",locations_foster$Website, "</a>"), 
                             group = "Foster Care", radius = 2, color = ~subpop_pal(Subpopulation)) %>% 
            addLayersControl(overlayGroups = c("Foster Care", "Juvenile Detention"), 
                             options = layersControlOptions(collapsed = FALSE))
        
        l_sub
        
        
        
    })
    output$map2 <- renderLeaflet({
        
        
        allegheny_foster <- allegheny_locations %>% filter(Subpopulation == "Foster Care")
        allegheny_juvie <- allegheny_locations %>% filter(Subpopulation == "Juvenile Detention")
        
        
        a_sub <- leaflet(options = leafletOptions(minzoom = 12)) %>% 
            setView(lng = -79.9, lat = 40.5, zoom = 10) %>% 
            addProviderTiles("CartoDB") %>% 
            addCircleMarkers(data = allegheny_juvie, lng = ~Longitude, 
                             lat = ~Latitude, 
                             popup = ~paste0("<b>", allegheny_juvie$Program,"</b>", "<br/>", "<b>", 
                                             "Qualifications: ", "</b>", allegheny_juvie$Qualification, 
                                             "<br/>","<b>","Description: ", "</b>", allegheny_juvie$Description, 
                                             "<br/>","<b>","Website: ", "</b>", "<a>",allegheny_juvie$Website, "</a>"), 
                             group = "Juvenile Detention", radius = 2, color = ~subpop_pal(Subpopulation)) %>% 
            addCircleMarkers(data = allegheny_foster, lng = ~Longitude, 
                             lat = ~Latitude, 
                             popup = ~paste0("<b>", allegheny_foster$Program,"</b>", "<br/>", "<b>", 
                                             "Qualifications: ", "</b>", allegheny_foster$Qualification,
                                             "<br/>","<b>","Description: ", "</b>", allegheny_foster$Description,
                                             "<br/>","<b>","Website: ", "</b>", "<a>",allegheny_foster$Website, "</a>"), 
                             group = "Foster Care", radius = 2, color = ~subpop_pal(Subpopulation)) %>% 
            addLayersControl(overlayGroups = c("Foster Care", "Juvenile Detention"), 
                             options = layersControlOptions(collapsed = FALSE))
        
        a_sub
        
        
        
    })
    
    output$map3 <- renderLeaflet({
        
        
        a_sub <- allegheny_locations %>% 
            leaflet( options = leafletOptions(minzoom = 12)) %>%
            setView(lng = -79.957030, lat = 40.5, zoom = 10) %>% 
            addProviderTiles("CartoDB") %>%
            addCircleMarkers(lng = ~Longitude,
                             lat = ~Latitude,
                             popup = ~paste0("<b>", allegheny_locations$Program, "</b>", "<br/>", "<b>", "Qualifications: ", "</b>", 
                                             allegheny_locations$Qualification, "<br/>","<b>","Description: ", "</b>", 
                                             allegheny_locations$Description, "<br/>","<b>","Website: ", "</b>", "<a>",
                                             allegheny_locations$Website, "</a>"),
                             group = ~allegheny_locations$Subpopulation, radius = 2, color = ~subpop_pal(Subpopulation)) %>%
            addLayersControl(overlayGroups = c("Foster Care", "Juvenile Detention"),
                             options = layersControlOptions(collapsed = FALSE))
        a_sub
        
        
    })
    
    output$map4 <- renderLeaflet({
        
        
        a_pill <- allegheny_locations %>%  
            leaflet(options = leafletOptions(minzoom = 12)) %>% 
            setView(lng = -79.957030, lat = 40.5, zoom = 10) %>% 
            addProviderTiles("CartoDB") %>% 
            addCircleMarkers(lng = ~Longitude, 
                             lat = ~Latitude, 
                             popup = ~paste0("<b>", allegheny_locations$Program, "</b>", "<br/>", "<b>", "Qualifications: ", "</b>", 
                                             allegheny_locations$Qualification, "<br/>","<b>","Description: ", "</b>", 
                                             allegheny_locations$Description, "<br/>","<b>","Website: ", "</b>", "<a>",
                                             allegheny_locations$Website, "</a>"),
                             radius = 2, 
                             group = ~allegheny_locations$Pillars, 
                             color = ~Pillar_pal(Pillars)) %>%  
            addLayersControl(position = "bottomleft",
                             overlayGroups = Pillar_levels, 
                             options = layersControlOptions(collapsed = FALSE)) %>%
            addLegend(title = "Service Type", position = "topleft", pal = Pillar_pal, values = Pillar_levels)
        
        a_pill
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
