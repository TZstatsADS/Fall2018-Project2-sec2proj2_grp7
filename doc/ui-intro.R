# Body
dashboardBody(
  tabItems(
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
                textOutput("read10"),
                box(title= "User Manual",width = 12)
              )),
            #Introduction tab - about team
            tabItem(tabName = "TeamInfo",
                    fluidRow(
                      mainPanel(
                        h3(textOutput("team0")),
                        textOutput("team1"),
                        textOutput("team2"),
                        textOutput("team3"),
                        textOutput("team4"),
                        textOutput("team5"),
                        textOutput("team6"),
                        textOutput("team7"),
                        textOutput("team8"),
                        box(title="Team Members",width=12)
                      )),
                    
                    
                    
                    
                    
                    
                    
                    
                    