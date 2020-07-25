
library(shiny)
library("ggplot2")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")

dat <- readRDS("05_22.rds")

result <- dat %>% group_by(day,scn_id) %>% summarise(n=n())
result <- dat %>% group_by(day,scn_id) %>% summarise(mean(Rt))