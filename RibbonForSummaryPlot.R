
aes_x <- "p.trace"
aes_y <- "Rt_Q_50"
dat <- readRDS("Newdata.rds")
# I should be careful about which one of select64000Scenarios as there are at least 3 of them
results<- select64000Scenarios(dat,10 ,3,.5,.5,.7,2,1,.3)
paired.cols <- RColorBrewer::brewer.pal(12, "Paired")




outputs1 <- results %>% filter(p.trace_app==0)
outputs2 <- results %>% filter(p.trace_app==.25)
outputs3 <- results %>% filter(p.trace_app==.50)
outputs4 <- results %>% filter(p.trace_app==.75)
outputs5 <- results %>% filter(p.trace_app==1)




# 
# p <- ggplot(results ,aes(x=p.trace,y=Rt_Q_50,color = factor(iso_delay_traced_max) ))+
#   scale_colour_manual(labels = c("1","2","3","4"),
#                       values = c(paired.cols[8], paired.cols[2],paired.cols[6],paired.cols[11]))
# p <- p + guides(fill=guide_legend(title="New Legend Title"))
# p <- p + scale_fill_discrete(name = "New Legend Title")
# p <- p+geom_line(size = 2)
# p <- p+ labs(color='')
# p <- p + scale_color_discrete(name ="Sex", labels=c("Female", "Male"))
# p
p <- ggplot(outputs1 ,aes(x=eval(as.name(aes_x)), y=Rt_Q_50))
#   scale_colour_manual(labels = c("1","2","3","4"),
#                       values = c(paired.cols[8], paired.cols[2],paired.cols[6],paired.cols[11]))

p <- p+ geom_ribbon(aes(ymin=Rt_Q_25,ymax=Rt_Q_75),fill=paired.cols[8],alpha=0.4)
p <- p+ geom_ribbon(aes(ymin=outputs2$Rt_Q_25,ymax=outputs2$Rt_Q_75), fill=paired.cols[2],alpha=0.4)
p <- p+ geom_ribbon(aes(ymin=outputs3$Rt_Q_25,ymax=outputs3$Rt_Q_75),fill=paired.cols[6],alpha=0.4)
p <- p+ geom_ribbon(aes(ymin=outputs4$Rt_Q_25,ymax=outputs4$Rt_Q_75),fill=paired.cols[11],alpha=0.5)
p <- p+ geom_ribbon(aes(ymin=outputs5$Rt_Q_25,ymax=outputs5$Rt_Q_75),fill=paired.cols[3],alpha=0.4)
# p <-p +scale_color_manual("Line.Color", values=c(red="red",green="green",blue="blue",red="red"),
#                           labels=paste0("Int",1:4))
# p <- ggplot(results ,aes(x=p.trace,y=Rt_Q_50,color = factor(iso_delay_traced_max) ))+
#   scale_colour_manual(labels = c("1","2","3","4"),
#                       values = c(paired.cols[8], paired.cols[2],paired.cols[6],paired.cols[11]))
# p <- p + guides(fill=guide_legend(title="New Legend Title"))
# p <- p + scale_fill_discrete(name = "New Legend Title")
# p <- p+geom_line(size = 2)
# p <- p+ labs(color='')



p <- p+geom_line(size = 2,color = paired.cols[8])
p <- p+geom_line(aes(x=outputs2$p.trace, y=outputs2$Rt_Q_50),size = 2,color = paired.cols[2])
p <- p+geom_line(aes(x=outputs3$p.trace, y=outputs3$Rt_Q_50),size = 2,color = paired.cols[6])
p <- p+geom_line(aes(x=outputs4$p.trace, y=outputs4$Rt_Q_50),size = 2,color = paired.cols[11])
p <- p+geom_line(aes(x=outputs5$p.trace, y=outputs5$Rt_Q_50),size = 2,color = paired.cols[3])

p <- p+geom_point(shape = 21, colour = paired.cols[8], fill = "white", size = 3, stroke = 3)
p <- p+geom_point(aes(x=outputs2$p.trace, y=outputs2$Rt_Q_50),
                  shape = 21, colour = paired.cols[2], fill = "white", size = 3, stroke = 3)
                                                                                   
p <- p+geom_point(aes(x=outputs3$p.trace, y=outputs3$Rt_Q_50),
                  shape = 21, colour = paired.cols[6], fill = "white", size = 3, stroke = 3)
p <- p+geom_point(aes(x=outputs4$p.trace, y=outputs4$Rt_Q_50),
                  shape = 21, colour = paired.cols[11], fill = "white", size = 3, stroke = 3)
p <- p+geom_point(aes(x=outputs5$p.trace, y=outputs5$Rt_Q_50),
                  shape = 21, colour = paired.cols[3], fill = "white", size = 3, stroke = 3)

# col <- c("Sepal Width" , "Petal Length", 
#             "Petal Width","Petal Width")


p <- p+theme_bw()

annotation <- data.frame(
  x= c(0.1,0.1,0.1,0.1,0.1),
  y = c(.5,.45,.40,.35,.30),
  label = c("1 day delay","2 days delay","3 days delay","4 days delay","5 days delay")
)

p <- p + geom_text(data=annotation, aes( x=x, y=y, label=label),                 , 
              color=c(paired.cols[8],paired.cols[2],paired.cols[6],"yellow",paired.cols[3]), 
              size=4 , angle=0, fontface="bold" )
 p <- p+annotate("point", x = .01, y = .5, colour = paired.cols[8],size = 4)
 p <- p+annotate("point", x = .01, y = .45, colour = paired.cols[2],size = 4)
 p <- p+annotate("point", x = .01, y = .40, colour = paired.cols[6],size = 4)
 p <- p+annotate("point", x = .01, y = .35, colour = "yellow",size = 4)
 p <- p+annotate("point", x = .01, y = .30, colour = paired.cols[3],size = 4)
 p
