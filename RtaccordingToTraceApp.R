source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/uploadRequiredLibraries.R")
uploadRequiredLibraries()

dat <- readRDS("05_22.rds")


selectScenarios <- function(dat,
                                 R,
                                 p.tr,
                                 p.trace_ap,
                                 p.sym,
                                 iso_delay_traced,
                                 iso_delay_untraced,
                                 sd_contact) {
  
  scenarios<-dat %>% filter(R0==R &
                                   p.trace==p.tr&
                                   # p.trace_app==p.trace_ap&
                                   p.symp== p.sym&
                                   iso_delay_traced_max==iso_delay_traced&
                                   iso_delay_untraced_sd_max==iso_delay_untraced&
                                   sd_contact_rate1==sd_contact) %>% filter(day==30) %>%  
    select(p.trace_app,iso_delay_traced_max,"day":"Rt") 
  return(scenarios)
}


aes_x <- "p.trace_app"
aes_y <- "Rt"
# aes_col <- "p.trace_app"
# aes_grp <- "p.trace_app"

results<- selectScenarios(dat, 3,0,.5,.7,2,1,.3)
 
  p <- ggplot(results,
              aes(x=eval(as.name(aes_x)),
                  y=eval(as.name(aes_y))))+
    # color=factor(eval(as.name(aes_col))),
    # group=eval(as.name(aes_grp)))) +
    stat_summary(geom="pointrange",
                 fun.y  = "median",
                 fun.ymin = function(x) quantile(x, .25),
                 fun.ymax = function(x) quantile(x, .75),size=1) +
    stat_summary(geom="line",
                 fun.y = "median",size=1)+ylim(0,2)+
    labs(y="y_label", x="x_label")
  p <- p + geom_hline(yintercept=1,
                      linetype='dotdash',
                      alpha=0.6)
  
  p 
  

  
  
