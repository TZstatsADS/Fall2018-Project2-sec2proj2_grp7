packages.used=c("dplyr", "plotly", "shiny", "leaflet", "scales", 
                "lattice", "htmltools", "maps", "data.table", 
                "dtplyr", "mapproj", "randomForest", "ggplot2", "rpart")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(shiny)
library(leaflet)
library(scales)
library(lattice)
library(dplyr)
library(htmltools)
library(maps)
library(plotly)
library(data.table)
library(dtplyr)
library(mapproj)
library(randomForest)
library(ggplot2)
library(rpart)

# calculate scores for a hospital 

calScore <- function(row,care.vec){
  # weight suggested for 7 criterion
  origin.weight <- c(11,11,11,11,2,2,2) 
  # care weight for 7 criterion
  care.weight <- origin.weight*care.vec/sum(origin.weight*care.vec)
  # hospital scores for 7 criterion
  criterion.score <- as.numeric(c(row[c(15,17,19,21,23,25,27)]))
  
  temp <- ifelse(is.na(criterion.score),0,care.weight)
  update.weight <- temp/sum(temp)
  
  score <- update.weight*criterion.score
  return(sum(score,na.rm = TRUE))
}

# switch payment to dollar signs

payswitch <- function(payment){
  if(is.na(payment)) {return("Not Avaliable")}
  else {if(payment<=1.5) {return("$")}
    else{if(payment<2.5) {return("$$")}
      else{return("$$$")}}}
}

# switch overall rating

orswitch <- function(rating){
  if(is.na(rating)){return("Not Available")}
  else {return(as.numeric(rating))}
}

shinyServer(function(input, output){
  #read data
  load("./hos.RData")
  load("./importance.RData")
  load("./df.RData")
  load("./hospital_ratings.RData")
  
  data <- hos
  
  #Inputs
  
  state<-reactive({state<-input$state})
  type <- reactive({type <- input$type})
  
  care1 <- reactive({input$care1}) # Mortality
  care2 <- reactive({input$care2}) # Safety of care
  care3 <- reactive({input$care3}) # Readmission rate
  care4 <- reactive({input$care4}) # Patient experience
  care5 <- reactive({input$care5}) # Effectiveness of care
  care6 <- reactive({input$care6}) # Timeliness of care
  care7 <- reactive({input$care7}) # Efficient use of medical imaging
  
  v1<-reactive({
    if (state() == "Select") {v1<-hos} 
    else {
      selectstate<-state()
      v1<- hos %>% filter(State == state())}})  
  
  v2 <- reactive({
    if (type() == "Select") {v2 <- v1()}
    else{
      selecttype <- type()
      v2 <- v1() %>% filter(Hospital.Type == type())}})
  
  care.origin <- reactive(care.origin <- c(care1(),care2(),care3(),
                                           care4(),care5(),care6(),care7()))
  # Dataset for the selected state
  data.state <- reactive(data.state <- v2())
  
  # Care vector for 7 criterion
  care.vec <- reactive(as.numeric(care.origin()))
  
  # Scores of hospitals in the selected state
  score <- reactive(apply(data.frame(data.state()),1,calScore,care.vec = care.vec()))
  
  # orders for hospitals
  ord <- reactive(order(score(),decreasing = TRUE))
  
  
  # Care vector for 7 criterion
  care.vec <- reactive(as.numeric(care.origin()))
  
  # Scores of hospitals in the selected state
  score <- reactive(apply(data.frame(v2()),1,calScore,care.vec = care.vec()))
  
  # orders for hospitals
  ord <- reactive(order(score(),decreasing = TRUE))
  
  # ranks for hospitals
  rk <- reactive(floor(frankv(score(),order = -1,ties.method = "min")))
  
  v3 <- reactive({v3 <- cbind(v2(),Order = ord(),Rank = rk())})
  
  # Outputs
  
  output$tableinfo = renderDataTable(
      {
        data1 <- v2()
        infotable <- data1[, c(2, 3, 4, 9, 30, 31, 32, 33, 34, 13)]
        infotable$Hospital.overall.rating <- apply(data.frame(as.numeric(data1$Hospital.overall.rating)),
                                                          1,orswitch)
       colnames(infotable) <- c("Hospital Name","Address","City","Type", "Mortality", "Safety", "Readmission",
                                "Patient Experience", "Effectiveness", "Overall Rating")
        infotable

    },options = list(orderClasses = TRUE, iDisplayLength = 5, lengthMenu = c(5, 10, 15, 20)))
  
  
  output$tablerank = renderDataTable({
    rankedtable <- cbind(v3()$Rank[ord()],v3()[ord(),c(2,3,4,5,6,8,9,29)])

    rankedtable$payment <- apply(data.frame(rankedtable$payment),1,payswitch)
    colnames(rankedtable) <- c("Rank","Hospital Name","Address","City",
                               "State","ZIP","TEL","Type","Cost")
    
    rankedtable
  },options = list(orderClasses = TRUE, iDisplayLength = 5, lengthMenu = c(5, 10, 15, 20)))
  

  hospIcons <- iconList(emergency = makeIcon("emergency_icon.png", iconWidth = 25, iconHeight =30),
                        critical = makeIcon("critical_icon.png", iconWidth = 25, iconHeight =30),
                        children = makeIcon("children_icon.png", iconWidth = 20, iconHeight =30))
  
  
  
  output$map <- renderLeaflet({
    content <- paste(sep = "<br/>",
                     paste("<font size=1.8>","<font color=green>","<b>",v3()$Hospital.Name,"</b>"),
                     paste("<font size=1>","<font color=black>",v3()$Address),
                     paste(v3()$City, v3()$State, v3()$ZIP.Code, sep = " "),  
                     paste("(",substr(v3()[ ,"Phone.Number"],1,3),") ",substr(v3()[ ,"Phone.Number"],4,6),"-",substr(v3()[ ,"Phone.Number"],7,10),sep = ""), 
                     paste("<b>","Hospital Type: ","</b>",as.character(v3()$Hospital.Type)),  
                     paste("<b>","Provides Emergency Services: ","</b>",as.character(v3()[ ,"Emergency.Services"])),

                     paste("<b>","Overall Rating: ","</b>", as.character(v3()[ ,"Hospital.overall.rating"])),
                     paste("<b>","Personalized Ranking: ","</b>",v3()$Rank))
    
    
    mapStates = map("state", fill = TRUE, plot = FALSE)
    leaflet(data = mapStates) %>% addTiles() %>%
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
      addMarkers(v2()$lon, v2()$lat, popup = content, icon = hospIcons[v2()$TF], clusterOptions = markerClusterOptions())
  })
  
  output$read0<- renderText({"Introduction"})
  output$read1<- renderText({"Greetings! If you are thinking of finding a hospital you can go, you can just save your time and look at our app. Our group has created an app helping you to find the best hospitals around you based on your preference on 7 aspects of hospitals including mortality, safety of care, readmission rate, patient experience, effectiveness of care, timeliness of care and efficient use of medical imaging. With your choice, it will be so easy to find the one fits you the best."})
  output$read2<- renderText({"User Manual"})
  output$read3<- renderText({"-> Step 1: Choose the State you live in or you need to go to. Simultaneously, you can also specify the type of hospital you may go to."})
  output$read4<- renderText({"-> Step 2: Choose how much do your care about on the each of the seven aspects of a hospital."})
  output$read5<- renderText({"-> Step 3: Check the Medicare Assessment table for the basic information of all hospitals, and the most importantly check the Personalized Ranking table to see which are the best ones for you."})
  output$read6<- renderText({"-> Step 4: Click on the map to see the exact location of the hospital and gogogo!"})
  output$read7<- renderText({"Data Source"})
  output$read8<- renderText({"The data is provided by Medicare government website, for more information, click the link below:"})
  output$read9<- renderText({"Criterion Measurement"})
  output$read10<- renderText({"1. Mortality measures the death rate of patients."})
  output$read11<- renderText({"2. Safety of care measures the rate of certain complications and infections."})
  output$read12<- renderText({"3. Readmission measures the rate of unplanned readmission after treatment."})
  output$read13<- renderText({"4. Patient experience measures how well patients feel during treatment, surgery and hospitalization."})
  output$read14<- renderText({"5. Effectiveness of care measures how appropriately patients are treated."})
  output$read15<- renderText({"6. Timeliness of care measures the time patients waiting."})
  output$read16<- renderText({"7. Efficient use of medical imaging measures how efficiently the hospitals using medical imaging such as MRI and CT scans."})
  output$read17<- renderText({"For more information, click the link below:"})
  
  
  output$team0<- renderText({"About Team"})
  output$team1<- renderText({"This app is developed in Spring 2018 by: "})
  output$team2<- renderText({"-> Guo, Xiaoxiao (email: xg2282@columbia.edu)"})
  output$team3<- renderText({"-> He, Shan (email: sh3667@columbia.edu)"})
  output$team4<- renderText({"-> Utomo, Michael (email: mu2251@columbia.edu)"})
  output$team5<- renderText({"-> Wen, Lan (email: lw2773@columbia.edu)"})
  output$team6<- renderText({"-> Yao, Jingtian (email: jy2867@columbia.edu)"})
  output$team7<- renderText({"We are a group of Columbia University M.A. in Statistics students eager to make the world an easier place to live in, and we are taking a tiny step here by developing this app to help you find the best and most fitted hospitals. Good luck!"})
  
  
  
  output$VI <- renderPlotly({
    
    b <- ggplot(importance.df, aes(x=Variables, y=MeanDecreaseGini)) +   
         geom_point(size = importance.df$MeanDecreaseGini/12,  
                    color = c("#999999", "#E69F00", "#56B4E9", "#009E73",  
                              "#F0E442", "#0072B2", "#D55E00"),  
                    alpha = 0.6) +
         theme(axis.text.x = element_text(angle = 40))+
         ggtitle('Variable Importance')+
         ylab("Mean Drop Gini")+
         theme(plot.title=element_text(hjust=0.5))
    
    ggplotly(b) %>% layout(height = 700, width = 1000)
    
  }
  
  )
  
  output$NHS <- renderPlotly({
  
    c <- ggplot(df, aes(x=State, y = Freq))+
      geom_bar(stat="identity", aes(fill = df$Freq))+
      labs(title= "Number of Hospitals by State", x="State", y=NULL)+
      theme_classic()+
      theme(axis.text.x = element_text(angle=90, size = 8))+
      theme(plot.title= element_text(hjust=0.5, vjust=1))+
      scale_y_continuous(expand = c(0,0))+
      theme(plot.margin = unit(c(1,1,1,1), "cm"))
    ggplotly(c) %>% layout(height = 700, width = 1000)
    
      c + scale_fill_continuous(name="Frequency")
  }
  
  )
  
  output$HQS <- renderPlotly({
    
    d <- ggplot(hospital_ratings.df, aes(x=State, y=HospitalRating))+
      geom_bar(stat="identity", alpha = 0.7, fill = "#009E73") +
      labs(title="Hospital Quality by State", x="State", y="Quality - OverallRating (1-5)")+
      theme_classic()+
      theme(axis.text.x=element_text(angle=90, hjust=1, size = 8))+
      theme(plot.title=element_text(hjust=0.5))+
      ylim(0,5)+
      theme(plot.margin = unit(c(1,1,1,1), "cm"))
    ggplotly(d) %>% layout(height = 700, width = 1000)
    
  }
  
  )

 })

