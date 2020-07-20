
uploadRequiredLibraries <- function(){
  # install.packages("shinydashboard")
  # library(plotly)
  library(shiny)
  library("ggplot2")
  library(magrittr) # needs to be run every time you start R and want to use %>%
  library(dplyr)    # alternatively, this also loads %>%
  library("tidyverse")
  library(shinydashboard)
  source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/data manipulation.R")
  source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/PlotMaker.R")
  source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/SidebaryLayout.R")
  source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/serverDesign.R")
  source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/CreateDataFrame.R")
  source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/RtAccordingtoTraceFractionDelay.R")
  source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/RtAccordingtoTraceApp.R")
  source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/RtaccordingToAppAndManual.R")
  source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/TwoSenarioInOnePage.R")
 # dat <- readRDS("05_22.rds")
}