library(shiny)
library(ggplot2)

# METODA 1

# UI definition

#install.packages("rmarkdown")



ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(h1("Paradoxul lui Bertrand")),
    mainPanel(
      br(),
      p("Enunțul Paradoxului Lui Bertrand este următorul:

Fie un triunghi echilateral înscris într-un cerc. Presupunem că o coardă din interiorul cercului este aleasă aleator. Care este probabilitatea ca coarda aleasă sa fie mai mare decât o latură a triunghiului?

Bertarnd oferă trei metode de rezolvare, toate diferite:
"),
      p("1.Metoda capetelor aleatoare:
		Se aleg, aleator, două puncte de pe circumferința cercului și se trasează coarda care le unește. Observăm că dacă capătul de final al corzii se află pe arcul trasat de latura triunghiului opusă vârfului, coarda este mai mai mare decât o latură a triunghiului. Cum lungimea unui arc de cerc este o treime din circumferinta cercului (triunghi echilateral) , probabilitatea ca o coardă aleasă să fie mai lungă decât o latură a triunghiului este ⅓."),
      p("2. Metoda razei aleatoare
		Se alege o rază a cercului. Din rază, se alege un punct aleator și coarda care trece perpendicular prin acesta. Coarda este mai lungă decât latura triunghiului dacă punctul ales se află în jumătatea mai apropiată de centrul cercului. Latura triunghiului împarte raza în două, deci probabilitea în acest caz devine ½.
"),
      p("3. Metoda punctului de mijloc:
      Se construieste un al doilea cerc inscris in triunghiul echilateral, care are raza jumatate din raza cercului mare. Se alege un punct oarecare din cercul mare si i se construieste coarda de care apartine. Observam ca daca centrul corzii se afla in interiorul cercului mai mic, coarda este mai mare decat latura triunghiului. Astfel, probabilitatea ca o coarda aleasa aleatoriu sa fie mai lunga decat o latura a triunghiului este 1/4 (aria triunghiului mare este de patru ori mai mare decat aria triunghiului mic)")
    )
  ),

  numericInput("r", "Radius:",1),
  numericInput("n", "Nr experimente:",10),
  plotOutput("plot1"),
  br(),
  br(),  br(),
  br(),  br(),
  br(),  br(),
  br(),  br(),
  br(),  br(),
  br(),
  plotOutput("plot2"),
  br(),
  br(),  br(),
  br(),  br(),
  br(),  br(),
  br(),  br(),
  br(),  br(),
  br(),
  plotOutput("plot3")
)

# Server definition
server <- function(input, output) {
  observe({
    r <- input$r
    n <- input$n
    
    unghi1 <- runif(n, 0, 2 * pi)
    x1 <- r * cos(unghi1)
    y1 <- r * sin(unghi1)
    
    unghi2 <- runif(n, 0, 2 * pi)
    x2 <- r * cos(unghi2)
    y2 <- r * sin(unghi2)
    
    lung_coarda <- sqrt((x1 - x2)^2 + (y1 - y2)^2)
    
    nr_corzi <- sum(lung_coarda >  r * sqrt(3))
    
    prob <- nr_corzi / n
    
    df <- data.frame(x1, y1, x2, y2)
    g <- ggplot(df) +
      annotate("path",x=0+r*cos(seq(0,2*pi,length.out=100)),y=0+r*sin(seq(0,2*pi,length.out=100)), color="red", size=1,5)+
      geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color = "blue", size = 0.5) +
      geom_segment(aes(x = 0, y = r, yend = -r/2, xend = r*(sqrt(3)/2)), color = "purple", size = 1.5) +
      geom_segment(aes(x = 0, y = r, yend = -r/2, xend = -r*(sqrt(3)/2)), color = "purple", size = 1.5) +
      geom_segment(aes(y = -r/2, x = -r*(sqrt(3)/2), yend = -r/2, xend = r*(sqrt(3)/2)), color = "purple", size = 1.5) +
      xlim(-r, r) + ylim(-r, r) +
      ggtitle(paste0("Metoda 1: ", prob, " (Teoretic 1/3)")) +
      xlab("X") + ylab("Y")
    
    r <- input$r
    n <- input$n
    
    #se creeaza o raza random
    unghi1 <- runif(n, 0, 2 * pi)
    #se gasesc punctele corespunzatoare acelui unghi
    r1 <- r * cos(unghi1)
    r2 <- r * sin(unghi1)
  
    
    c1 <- ifelse(r1 > 0,  runif(n,0,r1), runif(n,r1,0))
    
    c2 <- (r2 * c1)/r1
    
    nr_corzi <- sum(c1*c1 + c2*c2 >  (r/2)*(r/2))
    # print(nr_corzi)
    prob <- nr_corzi / n
    
    #print(prob)
    
    df <- data.frame(r1,r2)
    g2 <- ggplot(df) +
      geom_segment(aes(x = r1, y = r2, xend = 0, yend = 0), color = "blue", size = 0.5) +
      geom_point(aes(x=c1, y=c2), data=df, size=2, shape=1, color="red", fill = "red") +
      annotate("path",x=0+r*cos(seq(0,2*pi,length.out=100)),y=0+r*sin(seq(0,2*pi,length.out=100)), color="red", size=1,5)+
      geom_segment(aes(x = 0, y = r, yend = -r/2, xend = r*(sqrt(3)/2)), color = "purple", size = 1.5) +
      geom_segment(aes(x = 0, y = r, yend = -r/2, xend = -r*(sqrt(3)/2)), color = "purple", size = 1.5) +
      geom_segment(aes(y = -r/2, x = -r*(sqrt(3)/2), yend = -r/2, xend = r*(sqrt(3)/2)), color = "purple", size = 1.5) +
      xlim(-r, r) + ylim(-r, r) +
      ggtitle(paste0("Metoda 2 (Am generat doar centrele corzilor pentru simplitate): ",prob, " (Teoretic 1/2)")) +
      xlab("X") + ylab("Y")
    
    
    r <- input$r
    n <- input$n
    
    mx <- runif(n,-r,r)
    my <- runif(n,-sqrt(r*r-mx*mx),sqrt(r*r-mx*mx))
    
    nr_corzi <- sum((mx*mx+my*my)<(r/2)*(r/2))
    
    # Calculam probabilitatea 
    prob <- nr_corzi / n
    
    #print(prob)
    
    df <- data.frame(mx, my)
    g3 <- ggplot(df) +
      #geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color = "blue", size = 0.5) +
      geom_point(aes(x=mx, y=my), data=df, size=2, shape=1, color="red", fill = "red") +
      annotate("path",x=0+r*cos(seq(0,2*pi,length.out=100)),y=0+r*sin(seq(0,2*pi,length.out=100)), color="red", size=1,5)+
      annotate("path",x=0+(r/2)*cos(seq(0,2*pi,length.out=100)),y=0+(r/2)*sin(seq(0,2*pi,length.out=100)), color="pink", size=1,5)+
      geom_segment(aes(x = 0, y = r, yend = -r/2, xend = r*(sqrt(3)/2)), color = "purple", size = 1.5) +
      geom_segment(aes(x = 0, y = r, yend = -r/2, xend = -r*(sqrt(3)/2)), color = "purple", size = 1.5) +
      geom_segment(aes(y = -r/2, x = -r*(sqrt(3)/2), yend = -r/2, xend = r*(sqrt(3)/2)), color = "purple", size = 1.5) +
      xlim(-r, r) + ylim(-r, r) +
      ggtitle(paste0("Metoda 3: ", prob," (Teoretic 1/4)")) +
      xlab("X") + ylab("Y")
    
    output$plot1 <- renderPlot({ g }, height = 600, width = 600)
    output$plot2 <- renderPlot({ g2 }, height = 600, width = 600)
    output$plot3 <- renderPlot({ g3 }, height = 600, width = 600)
    
    

  })
}

# Run the Shiny app
shinyApp(ui, server)