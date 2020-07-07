
library(ggplot2)
df <- data.frame("time" = c(1, 2, 3, 2),
                 "x" = c(1, 1.03,1.03,1.06),
                "x.upper" = c(.91,.92,.95,.90), 
                "x.lower" = c(1.11,1.13,1.17,1.13))

library(ggplot2)
ggplot(data = df,aes(time,x))+
  geom_line(aes(y = x.upper), colour = 'red') +
  geom_line(aes(y = x.lower), colour = 'blue')+
  geom_line()

ggplot(data = df,aes(time,x))+
  geom_ribbon(aes(x=time, ymax=x.upper, ymin=x.lower), fill="pink", alpha=.5) +
  geom_line(aes(y = x.upper), colour = 'red') +
  geom_line(aes(y = x.lower), colour = 'blue')+
  geom_line()