library(shinydashboard)
library(googleway)
library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "HospitalCare"),
  skin = "yellow",
  # Sidebar - Introduction + Overview(Trend+Heatmap+Comparison) + Recommendation + Teaminfo
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
                                   choice = c("hospital1","hospital2"), selected = "hospital1", 
                                   multiple = TRUE))
              )),
      #Recommendation tab - map + recommendation filter + table
      tabItem(tabName = "Recommendation",
              fluidRow(
                  column(width=4,
                         box(title="Preference Control",
                             background="navy",
                             collapsible=TRUE,
                             width=NULL,
                             selectizeInput("select_state", label = "Select State:", 
                                            choice = c("NY","PA","VA","CA"), selected = "NY"),# later change choice to column name state
                             sliderInput("CostRange",label= "Cost Range:",min=2000, max=10000,value=c(5000,6000)),
                             checkboxGroupInput("quality_concern",label="Hospital Quality Star Ratings:", choices=c("1","2","3","4","5"),
                                                selected = c("3","4","5"),inline = TRUE),
                             selectInput("select_DRG", label="Select Your Diagnosis Related Group:",choices = c("example1","example2"),
                                         selected="example1")),
                         valueBox(subtitle="Average Quality",value=70,color="yellow",
                                  icon=icon("thumbs-up",lib="glyphicon"),width=NULL),
                         valueBox(subtitle="Total Number of Hospitals",value=100,color="purple",
                                  icon=icon("list"),width=NULL)
                         ),
                  column(width=8,
                         box(title="Navigation",
                             width=NULL,
                             google_mapOutput(outputId="map",height="300px")),
                         box(title = "Search Result",width = NULL,
                             dataTableOutput(outputId="hospitalinfo"))
                ))
      )
    )
  )
)

server <- function(input, output) {
#  set_key("AIzaSyDdlgehS7a81ffnUqrpnHdJASMQPZsRdpU")
  
#  output$map <- renderGoogle_map({
#    google_map(search_box = TRUE,location = c(40.7589, -73.9851),
#               zoom = 10)
#  })
}

shinyApp(ui, server)

