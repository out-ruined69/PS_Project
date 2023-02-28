#METODA 1

#remove.packages("ggplot2")

#install ggplot2
#install.packages("ggplot2")


#install.packages('ggplot2')
library(ggplot2)
r <- 4
n <- 100
#se genereaza aleator coarda 

#se genereaza un unghi cu o masura random
unghi1 <- runif(n, 0, 2 * pi)
#se gasesc punctele corespunzatoare acelui unghi
x1 <- r * cos(unghi1)
y1 <- r * sin(unghi1)

unghi2 <- runif(n, 0, 2 * pi)
x2 <- r * cos(unghi2)
y2 <- r * sin(unghi2)

# Se calculeaza lungimea corzii folosind formula
lung_coarda <- sqrt((x1 - x2)^2 + (y1 - y2)^2)

# Calculam cate corzi
nr_corzi <- sum(lung_coarda >  r * sqrt(3))

# Calculam probabilitatea 
prob <- nr_corzi / n

print(prob)
#se face graful
df <- data.frame(x1, y1, x2, y2)
ggplot(df) +
  annotate("path",x=0+r*cos(seq(0,2*pi,length.out=100)),y=0+r*sin(seq(0,2*pi,length.out=100)), color="red", size=1,5)+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color = "blue", size = 0.5) +
  geom_segment(aes(x = 0, y = r, yend = -r/2, xend = r*(sqrt(3)/2)), color = "purple", size = 1.5) +
  geom_segment(aes(x = 0, y = r, yend = -r/2, xend = -r*(sqrt(3)/2)), color = "purple", size = 1.5) +
  geom_segment(aes(y = -r/2, x = -r*(sqrt(3)/2), yend = -r/2, xend = r*(sqrt(3)/2)), color = "purple", size = 1.5) +
  xlim(-r, r) + ylim(-r, r) +
  ggtitle("Metoda 1",prob) +
  xlab("X") + ylab("Y")


