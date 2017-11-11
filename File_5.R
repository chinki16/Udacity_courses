library(ggplot2)
data=diamonds
str(data)
data$color
?diamonds
ggplot(aes(x=price),data=diamonds)+geom_histogram()
summary(data$price)
