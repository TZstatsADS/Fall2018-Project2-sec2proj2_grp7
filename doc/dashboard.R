library(shinydashboard)
#library(googleway)
library(shiny)
library(dplyr)
library(leaflet)
library(leaflet.extras)



hospital_info <- read.csv("../data/hospital_full.csv")
#hospital_address <- read.csv("../data/hospital_address.csv")
DRG <- as.vector(unique(hospital_info$DRG.Definition))
hospital_names <- as.vector(unique(hospital_info$Hospital.Name))
states <- as.vector(unique(hospital_info$State))






ui <- dashboardPage(
  dashboardHeader(title = "HospitalCare"),
  skin = "yellow",
  # Sidebar - Introduction + Overview(Trend+Heatmap+Comparison) + Recommendation
  dashboardSidebar(
    sidebarMenu(id="menu",
                menuItem("Introduction", tabName = "Introduction", icon = icon("info"),
                         menuSubItem("User Manual", tabName = "UserManual", icon=icon("book")),
                         menuSubItem("About Team", tabName = "TeamInfo",icon=icon("users"))),
                menuItem("Overview", tabName = "Overview", icon = icon("th"),
                         menuSubItem("Medicare Cost Trend", tabName = "CostTrend",icon = icon("bar-chart-o")),
                         menuSubItem("States Comparison", tabName = "CostByState", icon=icon("map")),
                         menuSubItem("Hospital Comparison", tabName="HospitalCompare",icon=icon("stethoscope"))
                ),
                menuItem("Find Your Hospital", tabName = "Recommendation", icon = icon("medkit"))
    )
  ),
  # Body
  dashboardBody(
    tabItems(
      #Introduction tab - user manual
      tabItem(tabName = "UserManual",
              fluidRow(
                box(title= "User Manual",width = 12)
              )),
      #Introduction tab - about team
      tabItem(tabName = "TeamInfo",
              fluidRow(
                box(title="Team Members",width=12)
              )),
      #Cost Trend tab - trend plot
      tabItem(tabName = "CostTrend",
              fluidRow(
                box(title = "Cost Trend",width=12,
                    htmlOutput("trend"))
              )),
      # Cost Bt state tab - heatmap
      tabItem(tabName = "CostByState",
              fluidRow(
                box(title = "Heatmap", width = 12,
                    htmlOutput("heatmap"))
              )),
      # Hospital Comparison - table
      tabItem(tabName = "HospitalCompare",
              fluidRow(
                box(width=12,
                    selectizeInput("select_hospital", label = "Select hospital:",
                                   choice = hospital_names, selected = c("MOUNT SINAI HOSPITAL","NEW YORK-PRESBYTERIAN HOSPITAL"), 
                                   multiple = TRUE))
              )),
      #Recommendation tab - map + recommendation filter + table + infobox
      tabItem(tabName = "Recommendation",
              fluidRow(
                column(width=4,
                       box(title="Preference Control",
                           background="navy",
                           collapsible=TRUE,
                           width=NULL,
                           selectizeInput("r.state", label = "Select State:", 
                                          choice = states, selected = "NY"),
                           sliderInput("r.cost",label= "Cost Range:",min=2000, max=20000,value=c(3000,15000)),
                           selectizeInput("r.care",label="Most Cared Hospital Quality:", 
                                              choices=c("Selected","Mortality","Safety of Care","Readmission Rate","Patient Experience",
                                                        "Effectiveness of Care","Timeliness of Care","Efficient Use of Medical Imaging"),
                                              selected = c("Selected")),
                           selectInput("r.drg", label="Select Your Diagnosis Related Group:",choices = DRG,
                                       selected="039 - EXTRACRANIAL PROCEDURES W/O CC/MCC"),
                           submitButton("Submit",width='100%')),
                       valueBox(subtitle="Average Quality",value=70,color="yellow",
                                icon=icon("thumbs-up",lib="glyphicon"),width=NULL),
                       valueBox(subtitle="Total Number of Hospitals",value=100,color="purple",
                                icon=icon("list"),width=NULL)
                ),
                column(width=8,
                       box(title="Navigation",
                           width=NULL,
                           leafletOutput(outputId="map",height="300px")),
                       box(title = "Search Result",width = NULL,
                           dataTableOutput(outputId="r.df"))
                ))
      )
    )
  )
)

server <- function(input, output) {
  r.state <- reactive(input$r.state)
  # # Reactive expression for the map data subsetted to what the user selected
  # filteredData <- reactive({
  #   hospital_address[hospital_address$State == input$r.state,]
  # })
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  # new icon style
  icon.flag <- makeAwesomeIcon(icon = "flag", markerColor = "red" ,iconColor = "white")
  # Observe event - select row in table
  observeEvent(input$r.df_rows_selected, {
    row_selected = r.data()[input$r.df_rows_selected,]
    proxy <- leafletProxy('map')
    print(row_selected)
    proxy %>% addAwesomeMarkers(lng=row_selected$long, lat=row_selected$lat,icon=icon.flag,layerId = as.character(row_selected$Provider.Id))
    # Reset previously seleted marker
    if(!is.null(prev_row())) {
      proxy %>% addMarkers(lng=prev_row()$lon, lat=prev_row()$lat,layerId=as.character(prev_row()$Provider.Id))
    }
    # set new value to reactiveVal
    prev_row(row_selected)
  })
  #leaflet map output (interactive with user's input)
  output$map <- renderLeaflet({
    leaflet(data=df3()) %>% addTiles() %>% addAwesomeMarkers(lng = ~lon, lat = ~lat, clusterOptions=markerClusterOptions(),
                                                               popup=paste(df3$Hospital.Name,"\nAddress:",df3$Full),
                                                             layerId = as.character(df3()$Provider.Id))
    })
  # observe Event - click on map
  observeEvent(input$map_marker_click, {
    clickId <- input$map_marker_click$id
    dataTableProxy('r.df') %>% 
      selectRows(which(df3()$id == clickId)) %>%
      selectPage(which(input$r.df_rows_all == clickId) %/% input$r.df_state$length+1)
  })
  }

shinyApp(ui, server)

