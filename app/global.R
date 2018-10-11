packages.used = c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "DT",
  "leaflet",
  "data.table",
  "ggmap",
  "tidyr",
  "googleVis"
)

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
library(ggmap)
library(googleVis)
library(tidyr)


#load data
load("data.RData")
hospital_info <- read.csv("hospital_2016.csv")

DRG.2016 <- as.vector(unique(hospital_info$DRG.Definition))
states.2016 <- as.vector(unique(hospital_info$State))
hospital_info$drg <- substring(hospital_info$DRG.Definition, 1, 3)
hospital_info$cost.w.medicare <-
  hospital_info$Average.Total.Payments - hospital_info$Average.Medicare.Payments

year = list(
  "2011" = "2011",
  "2012" = "2012",
  "2013" = "2013",
  "2014" = "2014",
  "2015" = "2015",
  "2016" = "2016"
)
DRG = as.list(unique(in_total$DRG.Definition))
cost = colnames(in_total)[3:5]
hospital = as.list(unique(in_total_hospital$Provider.Name))
top = list("Top 5" = 5,
           "Top 10" = 10,
           "Top 20" = 20)

# getscore function used in Server
getscore <- function(observation, care.w) {
  in.w <-
    c(20, 20, 20, 20, 8, 8, 4) # order is (M, S, R, P, Effect, T, Effic)
  f.w <- in.w * care.w / sum(in.w * care.w)
  ordinal <- observation[c(19, 20, 21, 22, 23, 24, 25)]
  ordinal <- replace(ordinal, ordinal == "Not Available", 0)
  ordinal <- as.numeric(ordinal)
  if (length(ordinal) != 7 | length(care.w) != 7) {
    return(NA)
  }
  return(sum(ordinal * f.w))
}

hospital_info <- hospital_info %>%
  mutate(Hospital.overall.rating=replace(Hospital.overall.rating, Hospital.overall.rating=="Not Available", NA)) %>%
  mutate(Hospital.overall.rating=as.numeric(Hospital.overall.rating)) %>% as.data.frame()

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
