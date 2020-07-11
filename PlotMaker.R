
library("ggplot2")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/data manipulation.R")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/SpecificPlotSetting.R")


# dat <- readRDS("05_22.rds")

returnPlot <- function(dat,
                       ouptColumn,
                       R0,
                       p.trace,
                       p.trace_app,
                       p.symp,
                       iso_delay_traced_max,
                       iso_delay_untraced_sd_max,
                       sd_contact_rate1) {
  
  output<- select100Scenarios(dat,
                              R0,
                              p.trace,
                              p.trace_app,p.symp,
                              iso_delay_traced_max,
                              iso_delay_untraced_sd_max,
                              sd_contact_rate1)
  
  
  
  # output<- select100Scenarios(dat, 2.5,.5,.5,.7,2,1,.3)
  # # ouptColumn <- "Rt"
  # yAxsis <- "n.incub"
  
  
   yAxsis <- ouptColumn
   results <- output%>% 
      select(day,yAxsis) %>% 
     group_by(day) %>%
     summarise(
       Q_05 = quantile(get(yAxsis), 0.05, na.rm=TRUE),
       Q_25 = quantile(get(yAxsis), 0.25, na.rm=TRUE),
       Rt_median = median(get(yAxsis), na.rm=TRUE),
       Q_75 = quantile(get(yAxsis), 0.75, na.rm=TRUE),
       Q_90 = quantile(get(yAxsis), 0.90, na.rm=TRUE)
     ) %>% ungroup()
   
   
   caption=NULL
   subtitle=NULL
   ylim=NULL
   title='Reproductive number Rt'
   paired.cols <- RColorBrewer::brewer.pal(12, "Paired")
   p <- ggplot2::ggplot(results, ggplot2::aes(x=day)) +
     ggplot2::geom_ribbon(
       ggplot2::aes(y=Rt_median,
                    ymin=Q_05,
                    ymax=Q_90),
       fill=paired.cols[7],
       alpha=0.8
     ) +
     ggplot2::geom_ribbon(
       ggplot2::aes(y=Rt_median,
                    ymin=Q_25,
                    ymax=Q_75),
       fill=paired.cols[8],
       alpha=0.5
     ) +
     ggplot2::geom_line(
       ggplot2::aes(y=Rt_median),
       color=paired.cols[8],
       size=1.2
     ) +
     ggplot2::geom_point(
       ggplot2::aes(y=Rt_median),
       color=paired.cols[8],
       size=3
     ) +
     # ggplot2::geom_hline(
     #   yintercept=1,
     #   linetype='dotdash'
     # ) +
     ggplot2::labs(x="Day", y=NULL) +
     ggplot2::scale_y_continuous(labels = scales::comma) 
     # ggplot2::theme_classic(base_size=16)
   
   # Add optional titles and labels
   if (!is.null(title)){
     p <- p + ggplot2::labs(title=title)
   }
   if (!is.null(subtitle)){
     p <- p + ggplot2::labs(subtitle=subtitle)
   }
   if (!is.null(caption)){
     p <- p + ggplot2::labs(caption=caption)
   }
   
   # Use a user-defined max y value (for comparison across models)
   if (!is.null(ylim)){
     p <- p + ggplot2::coord_cartesian(ylim = ylim)
   }
   
  
   # p
   
  
   
  # plotOut <- SpecificSettingOfPlotDays(nameOfcolumn,plotOut)
  return(p)
}


# myPlot<- returnPlot(dat, 2.5,.5,.5,.7,2,1,.3)
# myPlot




 


