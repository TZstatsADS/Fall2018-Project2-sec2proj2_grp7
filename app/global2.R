library(dplyr)
library(shiny)
library(ggmap)
library(shiny)
library(googleVis)
library(tidyr)

load("../output/data.RData")


year = list("2011" = "2011", "2012" = "2012", "2013" = "2013", "2014" = "2014", "2015" = "2015", "2016" = "2016")
DRG = as.list(unique(in_total$DRG.Definition))
cost = colnames(in_total)[3:5]
hospital = as.list(unique(in_total_hospital$Provider.Name))
top = list("Top 5" = 5, "Top 10" = 10, "Top 20" = 20)


