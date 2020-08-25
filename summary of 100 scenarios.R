
library(shiny)
library("ggplot2")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")

dat <- readRDS("05_22.rds")

result <- dat %>% group_by(day,scn_id) %>% summarise(n=n())
saveRDS(result, file = "my_data.rds")


result <- dat %>% group_by(R0,
                           p.trace,
                           p.trace_app, 
                           p.symp, 
                           iso_delay_traced_max,
                           iso_delay_untraced_sd_max,
                           sd_contact_rate1,
                           day,
                           scn_id)%>%
  summarise(n=n())

# enhanced data
dat <- readRDS("05_22.rds")
addColum <- dat %>% summarise(n.new =n.new_S+n.new_I)
dat <- data.frame(dat,addColum)
saveRDS(dat, file = "enhanced_data.rds")
# Starts from here
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")
dat <- readRDS("enhanced_data.rds")
result <- dat %>% group_by(R0,
                           p.trace,
                           p.trace_app, 
                           p.symp, 
                           iso_delay_traced_max,
                           iso_delay_untraced_sd_max,
                           sd_contact_rate1,
                           day,
                           scn_id)%>%
  summarise(Rt_Q_05 =quantile(Rt, 0.05, na.rm=TRUE),
            Rt_Q_25 =quantile(Rt, 0.25, na.rm=TRUE),
            Rt_Q_50 =quantile(Rt, 0.50, na.rm=TRUE),
            Rt_Q_75 =quantile(Rt, 0.75, na.rm=TRUE),
            Rt_Q_95 =quantile(Rt, 0.95, na.rm=TRUE),
            
            n.active_Q_05 =quantile(n.active, 0.05, na.rm=TRUE),
            n.active_Q_25 =quantile(n.active, 0.25, na.rm=TRUE),
            n.active_Q_50 =quantile(n.active, 0.50, na.rm=TRUE),
            n.active_Q_75 =quantile(n.active, 0.75, na.rm=TRUE),
            n.active_Q_95 =quantile(n.active, 0.95, na.rm=TRUE),
            
            n.new_Q_05 =quantile(n.new, 0.05, na.rm=TRUE),
            n.new_Q_25 =quantile(n.new, 0.25, na.rm=TRUE),
            n.new_Q_50 =quantile(n.new, 0.50, na.rm=TRUE),
            n.new_Q_75 =quantile(n.new, 0.75, na.rm=TRUE),
            n.new_Q_95 =quantile(n.new, 0.95, na.rm=TRUE),
            
            n.total_Q_05 =quantile(n.total, 0.05, na.rm=TRUE),
            n.total_Q_25 =quantile(n.total, 0.25, na.rm=TRUE),
            n.total_Q_50 =quantile(n.total, 0.50, na.rm=TRUE),
            n.total_Q_75 =quantile(n.total, 0.75, na.rm=TRUE),
            n.total_Q_95 =quantile(n.total, 0.95, na.rm=TRUE),
            
            n.iso_Q_05 =quantile(n.iso, 0.05, na.rm=TRUE),
            n.iso_Q_25 =quantile(n.iso, 0.25, na.rm=TRUE),
            n.iso_Q_50 =quantile(n.iso, 0.50, na.rm=TRUE),
            n.iso_Q_75 =quantile(n.iso, 0.75, na.rm=TRUE),
            n.iso_Q_95 =quantile(n.iso, 0.95, na.rm=TRUE),
            )

saveRDS(result, file = "Newdata.rds")
result <- readRDS("Newdata.rds")

