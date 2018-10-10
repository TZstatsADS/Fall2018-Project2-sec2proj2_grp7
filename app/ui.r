packages.used=c("shiny", "plotly", "shinydashboard", "leaflet")


# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}


library(shiny)
library(plotly)
library(leaflet)
library(shinydashboard)

  dashboardPage(
    dashboardHeader(title = "Hospital For You"),
    skin = "green",
    dashboardSidebar(
      width = 260,
      sidebarMenu(id="tabs",
                  menuItem("Welcome", tabName = "Welcome1", icon = icon("book")),
                  menuItem("Introduction",  icon = icon("file-text-o"),
                           menuSubItem("Read Me", tabName = "ReadMe", icon = icon("angle-right")),
                           menuSubItem("Criterion Measurement", tabName = "CM", icon = icon("angle-right")),
                           menuSubItem("About Team", tabName = "AboutTeam", icon = icon("angle-right"))),
                  menuItem("Summary Statistics", tabName = "SummaryStat", icon = icon("area-chart")),
                  menuItem("Hospital Recommendation", tabName = "HospitalRecommend", icon=icon("table"))),
      hr(),
      
      selectInput("state", label = "State", 
                choices = c("Select","AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN",
                            "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV",
                            "NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN",
                            "TX","UT","VT","VA","WA","WV","WI","WY"), selected = "Select"),
      selectInput("type", label = "Type", 
                  choices = c("Select","Acute Care Hospitals","Critical Access Hospitals","Childrens"), selected = "Select"),
      hr(),
      strong("Please select your preferences: "),
      # Criterion for hospitals
      radioButtons("care1",label = "Mortality",
                   choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                   selected = 2, inline = T),
      radioButtons("care2",label = "Safety of Care",
                   choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                   selected = 2, inline = T),
      radioButtons("care3",label = "Readmission Rate",
                   choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                   selected = 2, inline = T),
      radioButtons("care4",label = "Patient Experience",
                   choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                   selected = 2, inline = T),
      radioButtons("care5",label = "Effectiveness of Care",
                   choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                   selected = 2, inline = T),
      radioButtons("care6",label = "Timeliness of Care",
                   choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                   selected = 2, inline = T),
      radioButtons("care7",label = "Efficient Use of Medical Imaging",
                   choices = list("Very Care"=3,"Care"=2,"Not Care"=1),
                   selected = 2, inline = T),
      submitButton("Submit",width='100%')
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Welcome1",
                mainPanel(
                  img(src = "logo.png",height=800,width=1000)
                )),
        
        tabItem(tabName = "SummaryStat",
                fluidRow(
                  tabBox(width=12,
                         tabPanel(title="Variable Importance",width=12, plotlyOutput("VI", height = 700)),
                         tabPanel(title="Number of Hospitals by State\n",plotlyOutput("NHS", height = 700)), 
                         tabPanel(title="Hospital Quality by State\n",width=12,plotlyOutput("HQS", height = 700))
                         )
                  )),  
        
        tabItem(tabName = "HospitalRecommend",
        fluidRow(
        tabBox(width=12,
               tabPanel(title="Map",width = 12,solidHeader = T,leafletOutput("map"))
              ),
        tabBox(width = 12,
           
           tabPanel('MediCare Assessment',
                    dataTableOutput("tableinfo"),
                    tags$style(type="text/css", '#myTable tfoot {display:none;}')),
           tabPanel('Personalized Ranking',
                    dataTableOutput("tablerank"),
                    tags$style(type="text/css", '#myTable tfoot {display:none;}')
                    ))
    )),
        tabItem(tabName = "ReadMe",
            mainPanel(
              h2(textOutput("read0")),
              textOutput("read1"),
              hr(),
              h3(textOutput("read2")),
              textOutput("read3"),
              textOutput("read4"),
              textOutput("read5"),
              textOutput("read6"),
              hr(),
              h3(textOutput("read7")),
              textOutput("read8"),
              a("Here",href = "https://www.medicare.gov/hospitalcompare/search.html")
              )),
        tabItem(tabName = "CM",
                mainPanel(
                  h3(textOutput("read9")),
                  textOutput("read10"),
                  textOutput("read11"),
                  textOutput("read12"),
                  textOutput("read13"),
                  textOutput("read14"),
                  textOutput("read15"),
                  textOutput("read16"),
                  hr(),
                  textOutput("read17"),
                  a("Here",href = "https://www.medicare.gov/hospitalcompare/Data/Measure-groups.html")
                )),
        tabItem(tabName = "AboutTeam",
            mainPanel(
              h3(textOutput("team0")),
              textOutput("team1"),
              textOutput("team2"),
              textOutput("team3"),
              textOutput("team4"),
              textOutput("team5"),
              textOutput("team6"),
              hr(),
              textOutput("team7")
            ))
        
      )
    ))
    
  
