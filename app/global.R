packages.used = c("shiny",
                  "shinydashboard",
                  "dplyr",
                  "DT",
                  "leaflet",
                  "data.table")

# check packages that need to be installed.
packages.needed = setdiff(packages.used,
                          intersect(installed.packages()[, 1],
                                    packages.used))
# install additional packages
if (length(packages.needed) > 0) {
  install.packages(packages.needed, dependencies = TRUE)
}

library(shinydashboard)
library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(data.table)


#load data
hospital_info <- read.csv("../data/hospital_2016.csv")
DRG <- as.vector(unique(hospital_info$DRG.Definition))
hospital_names <- as.vector(unique(hospital_info$Hospital.Name))
states <- as.vector(unique(hospital_info$State))
hospital_info$drg <- substring(hospital_info$DRG.Definition, 1, 3)
hospital_info$cost.w.medicare <- hospital_info$Average.Total.Payments - hospital_info$Average.Medicare.Payments


# getscore function used in Server
getscore <- function(observation, care.w) {
  in.w <-
    c(20, 20, 20, 20, 8, 8, 4) # order is (M, S, R, P, Effect, T, Effic)
  f.w <- in.w * care.w / sum(in.w * care.w)
  ordinal <- as.numeric(observation[c(19, 20, 21, 22, 23, 24, 25)])
  if (length(ordinal) != 7 | length(care.w) != 7) {
    return(NA)
  }
  ordinal <- replace(ordinal, ordinal == "Not Available", 0)
  return(sum(ordinal * f.w))
}

#popup content generator
content.fun <- function(selected) {
  content <- paste(
    sep = "<br/>",
    paste(
      "<font size=1.8>",
      "<font color=green>",
      "<b>",
      selected$Hospital.Name,
      "</b>"
    ),
    paste("<font size=1>", "<font color=black>", selected$Full),
    paste(
      "<b>",
      "Phone: ",
      "</b>",
      "(",
      substr(selected$Phone.Number, 1, 3),
      ") ",
      substr(selected$Phone.Number, 4, 6),
      "-",
      substr(selected$Phone.Number, 7, 10),
      sep = ""
    ),
    paste(
      "<b>",
      "Hospital Type: ",
      "</b>",
      as.character(selected$Hospital.Type)
    ),
    paste(
      "<b>",
      "Provides Emergency Services: ",
      "</b>",
      as.character(selected$Emergency.Services)
    )
  )
  
}

shinyApp(ui, server)
