returnPlotTowScenarions <- function(dat,
                                    inputR012,
                                    inputp.trace12,
                                    inputp.trace_app12,
                                    inputp.symp12,
                                    inputiso_delay_traced_max12,
                                    inputiso_delay_untraced_sd_max12,
                                    inputsd_contact_rate112,
                                    inputR023,
                                    inputp.trace23,
                                    inputp.trace_app23,
                                    inputp.symp23,
                                    inputiso_delay_traced_max23,
                                    inputiso_delay_untraced_sd_max23,
                                    inputsd_contact_rate123     
) {
  
  scenarios1<- select100Scenarios(dat,
                                  inputR012,
                                  inputp.trace12,
                                  inputp.trace_app12,
                                  inputp.symp12,
                                  inputiso_delay_traced_max12,
                                  inputiso_delay_untraced_sd_max12,
                                  inputsd_contact_rate112)
  
  scenarios2<- select100Scenarios(dat,
                                  inputR023,
                                  inputp.trace23,
                                  inputp.trace_app23,
                                  inputp.symp23,
                                  inputiso_delay_traced_max23,
                                  inputiso_delay_untraced_sd_max23,
                                  inputsd_contact_rate123)
  yAxsis <- "Rt"
  
  
  results1 <- scenarios1%>%
    select(day,yAxsis) %>%
    group_by(day) %>%
    summarise(
      Q_05 = quantile(get(yAxsis), 0.05, na.rm=TRUE),
      Q_25 = quantile(get(yAxsis), 0.25, na.rm=TRUE),
      Rt_median = median(get(yAxsis), na.rm=TRUE),
      Q_75 = quantile(get(yAxsis), 0.75, na.rm=TRUE),
      Q_90 = quantile(get(yAxsis), 0.90, na.rm=TRUE)
    ) %>% ungroup()
  
  paired.cols <- RColorBrewer::brewer.pal(12, "Paired")
  p <- ggplot2::ggplot(results1, ggplot2::aes(x=day)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(y=Rt_median,
                   ymin=Q_05,
                   ymax=Q_90),
      fill=paired.cols[7],
      alpha=0.2
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(y=Rt_median,
                   ymin=Q_25,
                   ymax=Q_75),
      fill=paired.cols[8],
      alpha=0.3
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
    )
  
  
  results2 <- scenarios2%>%
    select(day,yAxsis) %>%
    group_by(day) %>%
    summarise(
      Q_05 = quantile(get(yAxsis), 0.05, na.rm=TRUE),
      Q_25 = quantile(get(yAxsis), 0.25, na.rm=TRUE),
      Rt_median = median(get(yAxsis), na.rm=TRUE),
      Q_75 = quantile(get(yAxsis), 0.75, na.rm=TRUE),
      Q_90 = quantile(get(yAxsis), 0.90, na.rm=TRUE)
    ) %>% ungroup()
  
  p <- p+ geom_ribbon(aes(y=results2$Rt_median,
                          ymin=results2$Q_05,
                          ymax=results2$Q_90),
                      fill=paired.cols[2],
                      alpha=0.2
  )+
    ggplot2::geom_ribbon(
      ggplot2::aes(y=results2$Rt_median,
                   ymin=results2$Q_25,
                   ymax=results2$Q_75),
      fill=paired.cols[2],
      alpha=0.3
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y=results2$Rt_median),
      color=paired.cols[2],
      size=1.2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y=results2$Rt_median),
      color=paired.cols[2],
      size=3
    )
  
  
  return(p)
  

} 