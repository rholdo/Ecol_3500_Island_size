# Island extinction simulator
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(cowplot)

ext <- function(area){
  SP <- 10
  A <- area # Area
  d <- round(sqrt(A)) # Side of a square island
  maxT <- 100
  H <- matrix(round(runif(d ^ 2, min = 1, max = SP)), nrow = d, ncol = d)
  abun <- matrix(0, nrow = maxT, ncol = SP) # Store species abundances over time
  alpha <- data.frame(Time = numeric(maxT), alpha = numeric(maxT))
  for (i in 1:maxT){
    # Sample population
    sam <- sample(H, replace = TRUE)
    sam <- H
    # Randomly refill 10% of spaces based on sample
    size <- round(A / 10)
    rows <- round(runif(size, min = 1, max = d))
    cols <- round(runif(size, min = 1, max = d))
    newsp <- sample(sam, size, replace = TRUE)
    for (j in 1:size){
      H[rows[j],cols[j]] <- newsp[j]
    }
    for (j in 1:SP){
      abun[i,j] <- sum(H == j)
    }
    alpha$Time[i] <- i
    alpha$alpha[i] <- sum(abun[i,] > 0)
  }
  abun <- data.frame(abun)
  names(abun) <- paste0('SP',1:SP)
  abun.l <- abun %>% pivot_longer(
    cols = everything(),
    names_to = 'SP',
    values_to = 'N'
  )
  abun.l$Time <- rep(1:maxT, each = SP)
  return(list(abund = abun.l, rich = alpha))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Island extinction"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        sliderInput("area", "Island Area:",
                    min = 50, max = 500, value = 500),
        actionButton("run", "Project")
      ),
      mainPanel(
        plotOutput("extPlot")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$run, {
    ret <- ext(input$area)
    abun <- ret$abund
    rich <- ret$rich
    output$extPlot <- renderPlot({
      plot1 <- ggplot(abun) + geom_line(aes(x=Time, y=N, group = SP, color = SP)) +
        theme_bw() +
        theme(legend.position = "none") +
        labs(x = 'Time', 
             y = 'Pop. size')
      plot2 <- ggplot(rich) + geom_line(aes(x=Time, y=alpha)) +
        theme_bw() +
        ylim(0,SP) +
        labs(x = 'Time', 
             y = 'Species richness')
      prow <- plot_grid(
        plot1,
        plot2,
        align = 'vh',
        labels = NULL,
        hjust = 0,
        nrow = 2
      )
      prow
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)