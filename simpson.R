# Date de intrare 
x1 <- c(1, 2, 3, 4)
y1 <- c(10, 20, 30, 40)
x2 <- c(1, 2, 3, 4)
y2 <- c(40, 30, 20, 10)

ui <- fluidPage(
  plotOutput("scatterplot")
)

server <- function(input, output) {
  
  
  output$scatterplot <- renderPlot({
    plot(x1, y1, xlab = "X", ylab = "Y", main = "Individual Trends", pch = 19)
    points(x2, y2, col = "red", pch = 19)
    
    x <- c(x1, x2)
    y <- c(y1, y2)
    fit <- lm(y ~ x)
    abline(fit, col = "blue")
    
    legend("topright", inset = c(- 0.4, 0),                   
           legend = c("Group 1","Group 2"),
           pch = 1:2,
           col = 1:2) 
    #legend("topleft", legend = c("Data 1", "Data 2", "Combined Trend"),
    #      col = c("black", "red", "blue"), pch = 19)
  })
  
}

shinyApp(ui, server)