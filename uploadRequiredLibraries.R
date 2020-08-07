
uploadRequiredLibraries <- function(){
  # install.packages("shinydashboard")
  library(plotly)
  library(shiny)
  library("ggplot2")
  library(magrittr) # needs to be run every time you start R and want to use %>%
  library(dplyr)    # alternatively, this also loads %>%
  library("tidyverse")
  library(shinydashboard)
  source("data manipulation.R")
  source("PlotMaker.R")
  source("SidebaryLayout.R")
  source("serverDesign.R")
  source("CreateDataFrame.R")
  source("RtBasedonManualTrace.R")
  source("RtBasedonAppAndManual.R")
  source("RtBasedonAppTrace.R")
  source("RtBasedonTwoPlots.R")
 # dat <- readRDS("05_22.rds")
}