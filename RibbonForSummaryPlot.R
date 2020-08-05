
aes_x <- "p.trace"
aes_y <- "Rt_Q_50"
dat <- readRDS("Newdata.rds")
# I should be careful about which one of select64000Scenarios as there are at least 3 of them
results<- select64000Scenarios(dat,10 ,3,.5,.5,.7,2,1,.3)
paired.cols <- RColorBrewer::brewer.pal(12, "Paired")




outputs1 <- results %>% filter(iso_delay_traced_max==1)
outputs2 <- results %>% filter(iso_delay_traced_max==2)
outputs3 <- results %>% filter(iso_delay_traced_max==3)
outputs4 <- results %>% filter(iso_delay_traced_max==4)


p <- ggplot(outputs1 ,aes(x=eval(as.name(aes_x)), y=Rt_Q_50))

p <- p+ geom_ribbon(aes(ymin=Rt_Q_25,ymax=Rt_Q_75),fill=paired.cols[8],alpha=0.4)
p <- p+ geom_ribbon(aes(ymin=outputs2$Rt_Q_25,ymax=outputs2$Rt_Q_75), fill=paired.cols[2],alpha=0.4)
p <- p+ geom_ribbon(aes(ymin=outputs3$Rt_Q_25,ymax=outputs3$Rt_Q_75),fill=paired.cols[6],alpha=0.4)
p <- p+ geom_ribbon(aes(ymin=outputs4$Rt_Q_25,ymax=outputs4$Rt_Q_75),fill=paired.cols[11],alpha=0.5)

p <- p+geom_line(size = 2,color = paired.cols[8])
p <- p+geom_line(aes(x=outputs2$p.trace, y=outputs2$Rt_Q_50),size = 2,color = paired.cols[2])
p <- p+geom_line(aes(x=outputs3$p.trace, y=outputs3$Rt_Q_50),size = 2,color = paired.cols[6])
p <- p+geom_line(aes(x=outputs4$p.trace, y=outputs4$Rt_Q_50),size = 2,color = paired.cols[11])

p <- p+geom_point(shape = 21, colour = paired.cols[8], fill = "white", size = 3, stroke = 3)
p <- p+geom_point(aes(x=outputs2$p.trace, y=outputs2$Rt_Q_50),
                  shape = 21, colour = paired.cols[2], fill = "white", size = 3, stroke = 3)
                                                                                   
p <- p+geom_point(aes(x=outputs3$p.trace, y=outputs3$Rt_Q_50),
                  shape = 21, colour = paired.cols[6], fill = "white", size = 3, stroke = 3)
p <- p+geom_point(aes(x=outputs4$p.trace, y=outputs4$Rt_Q_50),
                  shape = 21, colour = paired.cols[11], fill = "white", size = 3, stroke = 3)
p

ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 5)
