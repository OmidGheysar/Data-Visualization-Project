
<<<<<<< HEAD
# lets get start working on the debugging 
=======
# lets get start working on the debuging 
dat <- readRDS("05_22.rds")
output<- select100Scenarios(dat, 2.5,.5,.5,.7,2,1,.3)

output<- select100Scenarios(dat, 3,.25,1,.6,4,1,.8)
# ouptColumn <- "Rt"
yAxsis <- "Rt"


yAxsis <- ouptColumn
results <- output%>% 
  select(day,yAxsis) %>% 
  group_by(day) %>%
  summarise(
    Q_05 = quantile(get(yAxsis), 0.05, na.rm=TRUE),
    Q_25 = quantile(get(yAxsis), 0.25, na.rm=TRUE),
    Rt_median = median(get(yAxsis), na.rm=TRUE),
    Q_75 = quantile(get(yAxsis), 0.75, na.rm=TRUE),
    Q_95 = quantile(get(yAxsis), 0.95, na.rm=TRUE)
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
                 ymax=Q_95),
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


p

ggplotly(p)


# =========================================================
# app trace
dat <- readRDS("05_22.rds")
selectScenarios<- function(dat,
                           R,
                           p.sym,
                           sd_contact) {
  
  scenarios<-dat %>% filter(R0==R &
                              p.trace==0&
                              # p.trace_app==p.trace_ap&
                              p.symp== p.sym&
                              iso_delay_traced_max==2&
                              # iso_delay_untraced_sd_max==iso_delay_untraced&
                              sd_contact_rate1==sd_contact) %>% filter(day==31) %>%  
    select(p.trace_app,iso_delay_untraced_sd_max,"day":"Rt") 
  return(scenarios)
}
results<- selectScenarios(dat, 3,.7,.3)
a <- results %>% filter(p.trace_app==.75)%>% filter(iso_delay_untraced_sd_max==1)
b <- results %>% filter(p.trace_app==.75)%>% filter(iso_delay_untraced_sd_max==5)
a <- quantile(a$Rt, .5,na.rm=TRUE)
b <- quantile(b$Rt, .5,na.rm=TRUE)
a
b
# ========================================================
















>>>>>>> 5ee7c8bd42ce80d49b0e8aad5a6360ea5c9951a7

