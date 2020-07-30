library(ggplot2)
library(plotly)
p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
p2 <- ggplot(iris, aes(Sepal.Length, Petal.Length)) + geom_point()
p1
p3 <- gridExtra::grid.arrange(p1, p2, ncol = 1)

p3

install.packages(c("reshape", "plyr"))
require(plyr)
uninstall.packages(c("reshape", "plyr"))
install.packages(c("reshape", "plyr"))