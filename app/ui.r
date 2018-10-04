library(shiny)
library(leaflet)
library(shinydashboard)
header <- dashboardHeader(
  title = "Hospital Recomendation"
)
body <- dashboardBody(
  dropdownMenu(type = "tasks", badgeStatus = "success",
               taskItem(value = 90, color = "green",
                        "Documentation"
               ),
               taskItem(value = 17, color = "aqua",
                        "Project X"
               ),
               taskItem(value = 75, color = "yellow",
                        "Server deployment"
               ),
               taskItem(value = 80, color = "red",
                        "Overall project"
               )
  ) 
    
  )

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)