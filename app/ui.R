ui <- dashboardPage(
  dashboardHeader(title = "HospitalCare"),
  skin = "yellow",
  # Sidebar - Introduction + Overview(Trend+Heatmap+Comparison) + Recommendation
  dashboardSidebar(sidebarMenu(
    id = "menu",
    menuItem(
      "Introduction",
      tabName = "Introduction",
      icon = icon("info"),
      menuSubItem("User Manual", tabName = "UserManual", icon =
                    icon("book")),
      menuSubItem("About Team", tabName = "TeamInfo", icon =
                    icon("users"))
    ),
    menuItem(
      "Overview",
      tabName = "Overview",
      icon = icon("th"),
      menuSubItem(
        "Medicare Cost Trend",
        tabName = "CostTrend",
        icon = icon("bar-chart-o")
      ),
      menuSubItem(
        "States Comparison",
        tabName = "CostByState",
        icon = icon("map")
      ),
      menuSubItem(
        "Hospital Comparison",
        tabName = "HospitalCompare",
        icon = icon("stethoscope")
      )
    ),
    menuItem(
      "Find Your Hospital",
      tabName = "Recommendation",
      icon = icon("medkit")
    )
  )),
  # Body
  dashboardBody(
    tabItems(
      #Introduction tab - user manual
      tabItem(tabName = "UserManual",
              fluidRow(box(
                title = "User Manual", width = 12
              ))),
      #Introduction tab - about team
      tabItem(tabName = "TeamInfo",
              fluidRow(box(
                title = "Team Members", width = 12
              ))),
      #Cost Trend tab - trend plot
      tabItem(tabName = "CostTrend",
              fluidRow(
                box(title = "Cost Trend", width = 12,
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
              fluidRow(box(
                width = 12,
                selectizeInput(
                  "select_hospital",
                  label = "Select hospital:",
                  choice = hospital_names,
                  selected = c("MOUNT SINAI HOSPITAL", "NEW YORK-PRESBYTERIAN HOSPITAL"),
                  multiple = TRUE
                )
              ))),
      #Recommendation tab - map + recommendation filter + table + infobox
      tabItem(tabName = "Recommendation",
              fluidRow(
                column(
                  width = 4,
                  box(
                    title = "Preference Control",
                    background = "navy",
                    collapsible = TRUE,
                    width = NULL,
                    selectizeInput(
                      "r.state",
                      label = "Select State:",
                      choice = states,
                      selected = "NY"
                    ),
                    sliderInput(
                      "r.cost",
                      label = "Cost Range:",
                      min = 2500,
                      max = 160000,
                      value = c(3000, 20000)
                    ),
                    selectizeInput(
                      "r.care",
                      label = "Most Cared Hospital Quality:",
                      choices = c(
                        "Selected",
                        "Mortality",
                        "Safety of Care",
                        "Readmission Rate",
                        "Patient Experience",
                        "Effectiveness of Care",
                        "Timeliness of Care",
                        "Efficient Use of Medical Imaging"
                      ),
                      selected = c("Selected")
                    ),
                    selectInput(
                      "r.drg",
                      label = "Select Your Diagnosis Related Group:",
                      choices = DRG,
                      selected = "039 - EXTRACRANIAL PROCEDURES W/O CC/MCC"
                    )
                    #submitButton("Submit",width='100%')
                  ),
                  valueBoxOutput("vbox_1",width = NULL),
                  valueBoxOutput("vbox_2",width = NULL),
                  valueBoxOutput("vbox_3",width = NULL)
                ),
                column(
                  width = 8,
                  box(
                    #title = "Navigation",
                    width = NULL,
                    leafletOutput(outputId = "map")
                  ),
                  box(
                    title = "Search Result",
                    width = NULL,
                    dataTableOutput(outputId = "r.df")
                  )
                )
              ))
    )
  )
)
