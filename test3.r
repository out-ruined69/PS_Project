library(shiny)
library(ggplot2)

# METODA 1

# UI definition
ui <- fluidPage(
  numericInput("r", "Radius:",1),
  numericInput("n", "Nr experimente:",10),
  plotOutput("plot")
)

# Server definition
server <- function(input, output) {
  observe({
    r <- input$r
    n <- input$n
    
    mx <- runif(n,-r,r)
    my <- runif(n,-sqrt(r*r-mx*mx),sqrt(r*r-mx*mx))
    
    nr_corzi <- sum((mx*mx+my*my)<(r/2)*(r/2))
    
    # Calculam probabilitatea 
    prob <- nr_corzi / n
    
    #print(prob)
    
    df <- data.frame(mx, my)
    g <- ggplot(df) +
      #geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color = "blue", size = 0.5) +
      geom_point(aes(x=mx, y=my), data=df, size=2, shape=1, color="red", fill = "red") +
      annotate("path",x=0+r*cos(seq(0,2*pi,length.out=100)),y=0+r*sin(seq(0,2*pi,length.out=100)), color="red", size=1,5)+
      annotate("path",x=0+(r/2)*cos(seq(0,2*pi,length.out=100)),y=0+(r/2)*sin(seq(0,2*pi,length.out=100)), color="pink", size=1,5)+
      geom_segment(aes(x = 0, y = r, yend = -r/2, xend = r*(sqrt(3)/2)), color = "purple", size = 1.5) +
      geom_segment(aes(x = 0, y = r, yend = -r/2, xend = -r*(sqrt(3)/2)), color = "purple", size = 1.5) +
      geom_segment(aes(y = -r/2, x = -r*(sqrt(3)/2), yend = -r/2, xend = r*(sqrt(3)/2)), color = "purple", size = 1.5) +
      xlim(-r, r) + ylim(-r, r) +
      ggtitle("Metoda 3", prob) +
      xlab("X") + ylab("Y")
    
    output$plot <- renderPlot({ g }, height = 600, width = 600)
  })
}

# Run the Shiny app
shinyApp(ui, server)