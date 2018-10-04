library(shinydashboard)
library(googleway)
library(shiny)

# A function to calculate score
getscore <- function(observation, care.w) {
  if (length(ordinal) != 7 | length(care.w) != 7) {return(NA)}
  in.w <- c(20, 20, 20, 20, 8, 8, 4) # order is (M, S, R, P, Effect, T, Effic)
  f.w <- in.w * care.w/sum(in.w * care.w)
  ordinal <- observation[c("Mortality national comparison", "Safety of care.national comparison",
                           "Readmission national comparison", "Patient experience national comparison",
                           "Effectiveness of care national comparison",
                           "Timeliness of care national comparison",
                           "Efficient use of medical imaging national comparison")]
  return(sum(ordinal * f.w))
}





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
                                   choice = c("hospital1","hospital2"), selected = "hospital1", 
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
                                          choice = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN",
                                                     "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV",
                                                     "NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN",
                                                     "TX","UT","VT","VA","WA","WV","WI","WY"), selected = "NY"),
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
                           google_mapOutput(outputId="map",height="300px")),
                       box(title = "Search Result",width = NULL,
                           dataTableOutput(outputId="r.df"))
                ))
      )
    )
  )
)

server <- function(input, output) {
  # set_key("AIzaSyDdlgehS7a81ffnUqrpnHdJASMQPZsRdpU")
  # 
  # 
  # long <- state.center$x[which("NY"==state.abb)]
  # lat <- state.center$y[which("NY"==state.abb)]
  # 
  #   output$map <- renderGoogle_map({
  #     google_map(search_box = TRUE,location = c(lat,long),
  #                zoom = 7)
  #   })
  # # load data
  # # Haven't been tested; could be replaced by read.csv()
  # #load("./hospital\ info.csv")
  # #hospital_info <- read.csv("../data/hospital_info.csv")
  # # Input
  # r.state <- reactive(input$r.state)
  # r.drg <- reactive(input$r.drg)
  # r.care <- reactive(input$r.care)
  # r.score <- reactive(input$r.score)
  # #r.cost <- reactive(input$r.cost)
  # 
  # # Filter data by state and DRG
  # f1 <- reactive({
  #   if (r.state() == "Select") {f1 <- hospital_info}
  #   else {hospital_info %>% filter(State == r.state())}
  # })
  # 
  # f2 <- reactive({
  #   if (r.drg() == "Select") {f2 <- f1}
  #   else {f1 %>% filter(DRG.Definition == r.drg())}
  # })
  # 
  # # filter by cost
  # 
  # # Define care weight by input
  # o.c.w <- c(1, 1, 1, 1, 1, 1, 1)
  # name <- c("Selected","Mortality","Safety of Care","Readmission Rate","Patient Experience",
  #           "Effectiveness of Care","Timeliness of Care","Efficient Use of Medical Imaging")
  # c.weight <- reactive({
  #   if (r.care() == "Select") {o.c.w
  #   } else {o.c.w[which(name == r.care())] <- 2; o.c.w}
  # })
  # 
  # # Apply getscore function to every row of the selected data
  # score.res <- reactive(apply(f2, 1, getscore, care.w = c.weight))
  # 
  # # Filter by min score
  # score.res <- reactive(score.res)
  # r.order <- reactive(order(score.res, decreasing = T))
  # #f2$rank <- reactive(floor(frankv(score.res, order = -1, ties.method = "min")))
  # f2$pay.with.medi <- f2$Average.Total.Payments - f2$Average.Medicare.Payments
  # 
  # # Data table output
  # output$r.df <- renderDataTable({
  #   final.d <- f2[
  #     r.order, 
  #     c("Hospital Name", "Address", "City", "State", "Zipcode",
  #                            "Phone Number", "Average Total Payments", "pay.with.medi")]
  #   final.d}, options = list(orderClasses = TRUE,
  #                            iDisplayLength = 5, lengthMenu = c(5, 10, 15, 20)
  #   ))
  # 

}

shinyApp(ui, server)

