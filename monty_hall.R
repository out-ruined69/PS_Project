library(shiny)
library(ggplot2)

monty_hall <- function(n_sim, player_strategy) {
  wins <- 0
  for (i in 1:n_sim) {
    doors <- c(1, 2, 3)
    prize_door <- sample(doors, 1)
    player_choice <- sample(doors, 1)
    if (player_strategy == "switch") {
      if(player_choice != prize_door)
        wins <- wins + 1
    }
    else
      wins <- wins + (player_choice == prize_door)
  }
  return(wins/n_sim)
}

ui <- fluidPage(
  titlePanel("Simulare Monty Hall"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_sim", "Numar de simulari : ", 
                  min = 100, max = 1000000, value = 100000, step = 100),
      radioButtons("strategy", "Strategie:", 
                   choices = c("Stay" = "stay", "Switch" = "switch"), 
                   selected = "switch")
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

server <- function(input, output) {
  output$result <- renderText({
    result <- monty_hall(input$n_sim, input$strategy)
    paste("Probabilitatea de castig cu strategia '", input$strategy, "' :", result)
  })
}

shinyApp(ui = ui, server = server)