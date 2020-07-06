

x <- runif(100)
y <- runif(100)
df <- data.frame(c("X","Y"),x,y)
 
 plot(x,y)
 ggplot(data=df, aes(x,y))+geom_line()
 
 dat <- readRDS("05_22.rds")