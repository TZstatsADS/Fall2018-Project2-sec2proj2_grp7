

getscore <- function(observation, care.w) {
  in.w <- c(20, 20, 20, 20, 8, 8, 4) # order is (M, S, R, P, Effect, T, Effic)
  f.w <- in.w * care.w/sum(in.w * care.w)
  ordinal <- observation[c(22, 24, 26, 28, 30, 32, 34)]
  if (length(ordinal) != 7 | length(care.w) != 7) {return(NA)}
  ordinal <- replace(ordinal, ordinal == "Above the national average", 3)
  ordinal <- replace(ordinal, ordinal == "Below the national average", 1)
  ordinal <- replace(ordinal, ordinal == "Same as the national average", 2)
  ordinal <- as.numeric(ordinal)
  return(sum(ordinal * f.w))
}


t.server <- shinyServer(function(input, output){
  #Load data

  
  #Inputs
  
  r.state <- reactive({input$r.state})
  r.drg <- reactive({substr(input$r.drg, 1, 3)})
  r.care <- reactive({input$r.care})
  r.cost <- reactive({input$r.cost})
  
  df1 <- reactive({
    if (r.state() == "Select") {df1 <- hospital_info} 
    else {
      df1 <- hospital_info %>% filter(State == r.state())}})  
  
  df2 <- reactive({
    if (r.drg() == "Select") {df2 <- df1()}
    else{
      df2 <- df1() %>% filter(drg == r.drg())}})
  
  # Calculate care vector
  o.c.w <- c(1, 1, 1, 1, 1, 1, 1)
  name <- c("Mortality","Safety of Care","Readmission Rate",
            "Patient Experience", "Effectiveness of Care",
            "Timeliness of Care","Efficient Use of Medical Imaging")
  c.weight <- reactive({
    if (r.care() == "Select") {o.c.w
    } else {o.c.w[which(name == r.care())] <- 2; o.c.w}
  })
  
  # Scores of hospitals in the selected state
  score <- reactive(apply(data.frame(df2()),1,getscore,care.w = c.weight()))
  
  # orders for hospitals
  ord <- reactive(order(score(),decreasing = TRUE))
  
  # ranks for hospitals
  rk <- reactive(floor(frankv(score(),order = -1,ties.method = "min")))
  
  df3 <- reactive(cbind(df2(),Order = ord(),Rank = rk()))
  
  # Outputs
  output$r.df = renderDataTable({
    rankedtable <- cbind(df3()$Rank[ord()],df3()[ord(),c("Hospital Name","Address","City",
                                                       "State")])
    rankedtable
  },options = list(orderClasses = TRUE, pageLength = 5, lengthMenu = c(5, 10, 15, 20)))
  

})
  
  