library(shiny)
library(leaflet)
library(shinydashboard)
header <- dashboardHeader(
  title = "Twin Cities Buses"
)
body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("busmap", height = 500)
           ),
           box(width = NULL,
               uiOutput("numVehiclesTable")
           )
    ),
    column(width = 3,
           box(width = NULL, status = "warning",
               uiOutput("routeSelect"),
               checkboxGroupInput("directions", "Show",
                                  choices = c(
                                    Northbound = 4,
                                    Southbound = 1,
                                    Eastbound = 2,
                                    Westbound = 3
                                  ),
                                  selected = c(1, 2, 3, 4)
               ),
               p(
                 class = "text-muted",
                 paste("Note: a route number can have several different trips, each",
                       "with a different path. Only the most commonly-used path will",
                       "be displayed on the map."
                 )
               ),
               actionButton("zoomButton", "Zoom to fit buses")
           ),
           box(width = NULL, status = "warning",
               selectInput("interval", "Sort",
                           choices = c(
                             "State" = 30,
                             "DRG" = 60,
                             "Cost" = 120,
                             "Quality Score" = 300,
                             "Preference" = 600
                           ),
                           selected = "60"
               ),
               uiOutput("timeSinceLastUpdate"),
               actionButton("refresh", "Refresh now"),
               p(class = "text-muted",
                 br(),
                 "Source data updates every 30 seconds."
               )
           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)