server <- function(input, output) {
  #data <- hospital_info
  r.state <- reactive({
    input$r.state
  })
  r.drg <- reactive({
    substr(input$r.drg, 1, 3)
  })
  r.care <- reactive({
    input$r.care
  })
  r.cost <- reactive({
    input$r.cost
  })
  # filter data with state
  df1 <- reactive({
    
      df1 <- hospital_info %>% filter(State == r.state())
    
  })
  # filter data with selected DRG group
  df2 <- reactive({
   
      df2 <- df1() %>% filter(drg == r.drg())
    
  })
  # filter data with cost range
  df3 <- reactive({
    df3 <- df2() %>% filter(Average.Total.Payments >= r.cost()[1] &
                                             Average.Total.Payments <= r.cost()[2]) 
  })
  
  
  
  # reweight with selected most care aspect
  # Calculate care vector
  o.c.w <- c(1, 1, 1, 1, 1, 1, 1)
  name <- c(
    "Mortality",
    "Safety of Care",
    "Readmission Rate",
    "Patient Experience",
    "Effectiveness of Care",
    "Timeliness of Care",
    "Efficient Use of Medical Imaging"
  )
  c.weight <- reactive({
    if (r.care() == "Select") {
      o.c.w
    } else {
      o.c.w[which(name == r.care())] <- 2
      o.c.w
    }
  })
  
  # Scores of hospitals in the selected state
  score <-
    reactive(apply(data.frame(df3()), 1, getscore, care.w = c.weight()))
  
  # orders for hospitals
  ord <- reactive(order(score(), decreasing = TRUE))
  
  # ranks for hospitals
  rk <-
    reactive(floor(frankv(
      score(), order = -1, ties.method = "min"
    )))
  #Reactive expression for the map/recommendation data subsetted to what the user selected
  df4 <- reactive(cbind(df3(), Order = ord(), Rank = rk()))
  
  # Outputs
  output$r.df = renderDataTable({
    rankedtable <-
      DT::datatable(
        #cbind(df4()$Rank[ord()], df4()[ord(), c("Hospital.Name")]),
        df4()[ord(),c("Rank","Hospital.Name","Address")],
        selection = "single",
        options = list(stateSave = TRUE,pageLength=5,lengthMenu=c(5,10,15,20)),rownames = FALSE
      )
    rankedtable
  }
  )
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  # new icon style
  icon.flag <-
    makeAwesomeIcon(icon = "flag",
                    markerColor = "red" ,
                    iconColor = "white")
  # Observe event - select row in table
  observeEvent(input$r.df_rows_selected, {
    row_selected = df4()[input$r.df_rows_selected, ]
    proxy <- leafletProxy('map')
    proxy %>% addAwesomeMarkers(
      lng = row_selected$lon,
      lat = row_selected$lat,
      icon = icon.flag,
      layerId = as.character(row_selected$Provider.ID),
      popup = content.fun(row_selected)
    ) %>% fitBounds(
      lng1 = row_selected$lon + 0.1,
      lng2 = row_selected$lon - 0.1,
      lat1 = row_selected$lat + 0.1,
      lat2 = row_selected$lat - 0.1
    )
    # Reset previously seleted marker
    if (!is.null(prev_row())) {
      proxy %>% addMarkers(
        lng = prev_row()$lon,
        lat = prev_row()$lat,
        layerId = as.character(prev_row()$Provider.ID),
        popup = content.fun(prev_row())
      )
    }
    # set new value to reactiveVal
    prev_row(row_selected)
  })
  
  #leaflet map output (interactive with user's input)
  output$map <- renderLeaflet({
    leaflet(data = df4()) %>% addTiles() %>% addAwesomeMarkers(
      lng = ~ lon,
      lat = ~ lat,
      popup = content.fun(df4()),
      layerId = as.character(df4()$Provider.ID)
    )
  })
  # observe Event - click on map
  observeEvent(input$map_marker_click, {
    clickId <- input$map_marker_click$Provider.ID
    dataTableProxy("r.df") %>%
      selectRows(which(df4()$Provider.ID == clickId)) %>%
      selectPage(which(input$r.df_rows_all == clickId) %/%
                   input$r.df_state$length + 1)
  })

  #value box - number of hospitals
  output$vbox_1 <- renderValueBox({
    valueBox(
      subtitle = "Number of Hospitals",
      value = nrow(df4()),
      color = "purple",
      icon = icon("list")
    )
  })
  
  #value box - average quality
  output$vbox_2 <- renderValueBox({
    valueBox(
      subtitle = "Average Quality",
      value = round(100 * mean(score()[score() > 0]) / max(score()), 0),
      color = "yellow",
      icon = icon("thumbs-up", lib = "glyphicon")
    )
  })
  
  #value box - average cost
  output$vbox_3 <- renderValueBox({
    valueBox(
      subtitle = "Average Cost",
      value = paste(round(
        mean(df4()$Average.Total.Payments, na.rm = TRUE), 0
      ), "$"),
      color = "blue",
      icon = icon("credit-card")
    )
  })
  
}

