library(shinydashboard)

shinyUI(dashboardPage(
  #header
  dashboardHeader(title = "HospitalCare"),
  skin = "blue",
  #sidebar - Introduction + Overview(Trend+Heatmap+Comparison) + Recommendation
  dashboardSidebar(
    sidebarMenu(
      id = "menu",
      menuItem("Welcome", tabName = "Welcome1", icon = icon("book")),
      menuItem(
        "About",
        tabName = "About",
        icon = icon("info"),
        menuSubItem("User Manual", tabName = "UserManual", icon = icon("book")),
        menuSubItem("About Team", tabName = "TeamInfo", icon = icon("users"))
      ),
      menuItem(
        "Overview",
        tabName = "Summary",
        icon = icon("th"),
        menuSubItem(
          "State Overview",
          tabName = "Overview",
          icon = icon("map-o")
        ),
        menuSubItem(
          "Spending Exploration",
          tabName = "Explore",
          icon = icon("balance-scale")
        ),
        menuSubItem(
          "Hospital Comparison",
          tabName = "Hospital_Comparison",
          icon = icon("medkit")
        )
      ),
      menuItem(
        "Find Your Hospital",
        tabName = "Recommendation",
        icon = icon("medkit")
      )
    ),
    conditionalPanel(
      condition = "input.menu == 'Hospital_Comparison'",
      selectizeInput(
        "select_year_2",
        label = h5("Select year:"),
        choice = year,
        selected = "2016"
      ),
      
      selectizeInput(
        "select_DRG_2",
        label = h5("Select DRG:"),
        choice = DRG,
        selected = "470 - MAJOR JOINT REPLACEMENT OR REATTACHMENT OF LOWER EXTREMITY W/O MCC"
      ),
      
      selectizeInput(
        "select_hospital",
        label = h5("Select hospital:"),
        choice = hospital,
        selected = "10033",
        multiple = TRUE
      )
    ),
    conditionalPanel(
      condition = "input.menu == 'Overview'",
      selectizeInput(
        "select_year",
        label = h5("Select year:"),
        choice = year,
        selected = "2016"
      ),
      selectizeInput(
        "select_DRG",
        label = "Select Diagnosis Related Group:",
        choice = DRG,
        selected = "470 - MAJOR JOINT REPLACEMENT OR REATTACHMENT OF LOWER EXTREMITY W/O MCC"
      )
    )
  ),
  #body
  dashboardBody(
    tabItems(
      tabItem(tabName = "Welcome1",
              mainPanel(
                img(
                  src = "logo.jpg",
                  height = 700,
                  width = 1350
                )
              )),
      #Introduction tab - user manual
      tabItem(tabName = "UserManual",
              fluidRow(
                mainPanel(
                  h3(textOutput("read0")),
                  h3(textOutput("read1")),
                  hr(),
                  textOutput("read2"),
                  textOutput("read3"),
                  textOutput("read4"),
                  textOutput("read5"),
                  hr(),
                  h3(textOutput("read6")),
                  textOutput("read7"),
                  textOutput("read8"),
                  textOutput("read9"),
                  textOutput("read10")
                  
                )
              ))
      ,
      #Introduction tab - about team
      tabItem(tabName = "TeamInfo",
              fluidRow(
                mainPanel(
                  h3(textOutput("team0")),
                  textOutput("team1"),
                  textOutput("team2"),
                  hr(),
                  textOutput("team3"),
                  textOutput("team4"),
                  textOutput("team5"),
                  textOutput("team6"),
                  textOutput("team7"),
                  textOutput("team8")
                  
                )
              )),
      
      ## tab Inpatient- Overview ##
      tabItem(tabName = "Overview",
              
              # fluidRow(
              tabsetPanel(
                tabPanel('Map',
                         fluidRow(
                           box(width = 6, title = "Average Charges by State",
                               htmlOutput("geo_plot")),
                           box(width = 6, title = "Average Medicare Payments by State",
                               htmlOutput("g_plot"))
                           #   box(width = 3,
                           #       selectizeInput("select_year", label = "Select year:",
                           #                   choice = year, selected = "2014"),
                           #       selectizeInput("select_cost", label = "Select charges/payments:",
                           #                   choice = cost, selected = "Charges"),
                           #       selectizeInput("select_DRG", label = "Select Diagnosis Related Group)",
                           #                   choice = DRG, selected = "470 - MAJOR JOINT REPLACEMENT OR REATTACHMENT OF LOWER EXTREMITY W/O MCC")))
                         ),
                         
                         fluidRow(
                           box(width = 6,
                               htmlOutput("hist_plot")),
                           box(width = 6,
                               htmlOutput("histogram_plot"))
                           
                         )),
                tabPanel('Explore by State',
                         fluidRow(
                           box(width = 6,
                               htmlOutput("col_plot")),
                           box(width = 6,
                               htmlOutput("con_plot"))
                           
                         )),
                
                tabPanel('Table',
                         dataTableOutput("sum_table"))
                
                
              )),
      ## tab Explore ##
      tabItem(tabName = "Explore",
              tabsetPanel(
                tabPanel('Top Spending',
                         fluidRow(
                           box(
                             width = 8,
                             htmlOutput("bubble_plot"),
                             "The size of bubble represents total Medicare Spending for each DRG",
                             br(),
                             "Total Medicare Spending = Total Discharges * Average Medicare Payments"
                           ),
                           box(
                             width = 4,
                             sliderInput(
                               "select_top",
                               label = h5(
                                 "Select top most expensive (according to total Medicare Spending) DRG:"
                               ),
                               min = 5,
                               max = 100,
                               value = 5,
                               step = 5
                             )
                           )
                         )),
                
                tabPanel('Year Trending',
                         fluidRow(
                           box(width = 9,
                               htmlOutput("line_plot")),
                           box(
                             width = 3,
                             
                             selectizeInput(
                               "select_DRG_3",
                               label = h3("Select DRG:"),
                               choice = DRG,
                               selected = "470 - MAJOR JOINT REPLACEMENT OR REATTACHMENT OF LOWER EXTREMITY W/O MCC"
                             ),
                             selectizeInput(
                               "select_hospital_2",
                               label = h3("Select hospital:"),
                               choice = hospital,
                               selected = c("SOUTHEAST ALABAMA MEDICAL CENTER"),
                               multiple = F
                             )
                             
                           )
                         ))
              )),
      
      
      ## tab Hospital Comparison ##
      tabItem(tabName = "Hospital_Comparison",
              tabsetPanel(
                tabPanel('Chart',
                         fluidRow(box(
                           width = 12,
                           htmlOutput("bar_plot")
                         ))),
                
                
                tabPanel('Table',
                         fluidRow(box(
                           width = 12, htmlOutput("subtable1")
                         )))
              ))
      
    )
  )
  
  
  
))