aes_x <- "p.trace"
aes_y <- "Revenue"
aes_col <- "iso_delay_traced_max"
aes_grp <- "iso_delay_traced_max"
results<- select64000Scenarios(dat, days, R, p.tr, p.trace_ap, p.sym,
                               iso_delay_traced, iso_delay_untraced, sd_contact) 
dat <- readRDS("Newdata.rds")
results<- select64000Scenarios(dat,10 ,3,.5,.5,.7,2,1,.3)


long_results <- results %>% gather(Quarter, Revenue, Rt_Q_05:Rt_Q_95)



p <- ggplot(long_results,
            aes(x=eval(as.name(aes_x)),
                y=eval(as.name(aes_y)),
                color=factor(eval(as.name(aes_col))),
                group=eval(as.name(aes_grp)))) +
  stat_summary(geom="pointrange",
               fun.y  = "median",
               fun.ymin = function(x) min(x),
               fun.ymax = function(x) max(x),size=2) +
  stat_summary(geom="line",
               fun.y = "median",size=1)+ylim(0,2)+
  labs(y="Reproductive Number", x="",
       color="")
p <- p + geom_hline(yintercept=1,
                    linetype='dotdash',
                    alpha=0.6)
p <- p + labs(title="Colors show delay to isolation for traced cases (days)")
p <- ggplotly(p)
x <- list(
  title = "Fraction of cases manually traced"
)
y <- list(
  title = "Reproductive Number"
)

p



