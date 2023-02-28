# Define the radius of the circle
r <- 5
n <- 1000
library(ggplot2)
#se creeaza o raza random
unghi1 <- runif(n, 0, 2 * pi)
#se gasesc punctele corespunzatoare acelui unghi
r1 <- r * cos(unghi1)
r2 <- r * sin(unghi1)



c1 <- ifelse(r1 > 0,  runif(n,0,r1), runif(n,r1,0))




print(c1)
print(c2)

c2 <- (r2 * c1)/r1

nr_corzi <- sum(c1*c1 + c2*c2 >  (r/2)*(r/2))
print(nr_corzi)
prob <- nr_corzi / n

print(prob)

df <- data.frame(r1,r2)
ggplot(df) +
  geom_segment(aes(x = r1, y = r2, xend = 0, yend = 0), color = "blue", size = 0.5) +
  geom_point(aes(x=c1, y=c2), data=df, size=2, shape=1, color="red", fill = "red") +
  annotate("path",x=0+r*cos(seq(0,2*pi,length.out=100)),y=0+r*sin(seq(0,2*pi,length.out=100)), color="red", size=1,5)+
  geom_segment(aes(x = 0, y = r, yend = -r/2, xend = r*(sqrt(3)/2)), color = "purple", size = 1.5) +
  geom_segment(aes(x = 0, y = r, yend = -r/2, xend = -r*(sqrt(3)/2)), color = "purple", size = 1.5) +
  geom_segment(aes(y = -r/2, x = -r*(sqrt(3)/2), yend = -r/2, xend = r*(sqrt(3)/2)), color = "purple", size = 1.5) +
  xlim(-r, r) + ylim(-r, r) +
  ggtitle("Metoda 2 (Am generat doar centrele corzilor pentru simplitate)",prob) +
  xlab("X") + ylab("Y")