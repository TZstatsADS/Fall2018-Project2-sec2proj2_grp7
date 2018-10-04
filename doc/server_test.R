# Define packages that need to be installed
p.need <- c("shiny", "ggplot2", "dplyr", "data.table")
p.nti <- setdiff(p.need, intersect(installed.packages()[, 1], p.need))

# Install packages
if (length(p.nti) > 0) {
  install.packages(p.nti, dependencies = T)
}

library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)

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

# Build server
shinyServer(function(input, output){
  # load data
  # Haven't been tested; could be replaced by read.csv()
  #load("./hospital\ info.csv")
  hospital_info <- read.csv("../data/hospital_info.csv")
  # Input
  r.state <- reactive(input$r.state)
  r.drg <- reactive(input$r.drg)
  r.care <- reactive(input$r.care)
  r.score <- reactive(input$r.score)
  r.cost <- reactive(input$r.cost)
  
  # Filter data by state and DRG
  f1 <- reactive({
    if (r.state() == "Select") {f1 <- hospital_info}
    else {hospital_info %>% filter(State == r.state())}
  })
  
  f2 <- reactive({
    if (r.drg() == "Select") {f2 <- f1}
    else {f1 %>% filter(substr(`DRG Definition`, 1, 3) == substr(r.drg(), 1, 3))}
  })
  
  # filter by cost
  
  # Define care weight by input
  o.c.w <- c(1, 1, 1, 1, 1, 1, 1)
  name <- c("Mortality national comparison", "Safety of care.national comparison",
            "Readmission national comparison", "Patient experience national comparison",
            "Effectiveness of care national comparison",
            "Timeliness of care national comparison",
            "Efficient use of medical imaging national comparison")
  c.weight <- reactive({
    if (r.care() == "Select") {o.c.w
    } else {o.c.w[which(o.c.w == r.care())] <- 2; o.c.w}
  })
  
  # Apply getscore function to every row of the selected data
  score.res <- reactive(apply(f2, 1, getscore, care.w = c.weight))
  
  # Filter by min score
  score.res <- reactive(score.res[score.res >= r.score()])
  r.order <- reactive(order(score.res, decreasing = T))
  f2$rank <- reactive(floor(frankv(score.res, order = -1, ties.method = "min")))
  f2$pay.with.medi <- f2$`Average Total Payments` - f2$`Average Medicare Payments`
  
  # Data table output
  output$r.df <- renderDataTable({
    final.d <- f2[r.order, c("rank", "Hospital Name", "Address", "City", "State", "Zipcode", 
                             "Phone Number", "Average Total Payments", "pay.with.medi")]
    final.d}, options = list(orderClasses = TRUE, 
                             iDisplayLength = 5, lengthMenu = c(5, 10, 15, 20)
    ))
  
  
})





