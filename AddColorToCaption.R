library(ggplot2)
library(ggtext)
library(dplyr)



plotMaker1 <- function(){
  Hashtag <- c("#python", "#python", "#python", "#python", "#rstats", "#rstats", "#rstats", "#rstats")
  Category <- c("FiveLikes", "FiveRTs", "HasURL", "HasMedia", "FiveLikes", "FiveRTs", "HasURL", "HasMedia") 
  NumTweets <- c(179, 74, 604, 288, 428, 173, 592, 293)
  graph_data <- data.frame(Hashtag, Category, NumTweets, stringsAsFactors = FALSE)
  
  
  str(graph_data)
  
  
  my_chart <- ggplot(graph_data, aes(x=Category, y=NumTweets, fill= Hashtag)) + 
    geom_col(position="dodge", alpha = 0.9) +
    theme_minimal() +
    xlab("") +
    ylab("") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))
  my_chart + 
    labs(title = "#python and #rstats: Comparing 1,000 random tweets")
  
  my_chart +
    labs(
      title = "<span style='color:#ff8c00'>#python</span> and 
    <span style='color:#346299'>#rstats</span>: Comparing 1,000 random tweets"
    ) +
    theme(
      plot.title = element_markdown()
    )
}
