library(shiny)
library(ggplot2)
library(dplyr)
library(stargazer)
library(DT)

ui <- navbarPage(
  "NBA Player Stats Analysis",
  tabPanel("Introduction",
    sidebarLayout(
      position = "right",
      sidebarPanel(
        img(src = "Giannis.jpeg", height = "100%", width = "100%", style = "object-fit: contain;")
      ),
      mainPanel(
        h1("Purpose:"),
        h5("This application explores and visualizes NBA player performance based on various statistical variables. Using advanced analytics and historical data from the 2023–2024 season, we aim to assess how different factors influence scoring outcomes. The analysis helps estimate future performance under a range of parameters using pre-existing player statistics."),
        br(),
        hr(),
        br(),
        h1("Data Section:"),
        p("The data for this project comes from an NBA player performance dataset, which includes statistics from the 2023–2024 regular season. This data was sourced from Kaggle, a widely recognized and reliable repository for basketball data and player metrics."),
        br(),
        p("The dataset includes performance data on many players, with each row representing an individual player's season averages. For this project, we focus on a subset of a few players and a sample of key performance variables. The selected variables represent a combination of offensive, defensive, and efficiency metrics, which help us evaluate player impact and productivity. "),
        br(),
        h4("Dataset:"),
        DTOutput("dataTable"), #displays data table
        br()
      )
    )
  ),
  
  # Main Analysis Tab
  tabPanel("Main Analysis",
    sidebarLayout(
      sidebarPanel(
        helpText("Explore how various stats affect player points."),
        selectInput("xvar", "Select X variable:",
                    choices = c("Minutes Played" = "MP",
                                "Shots Attempted" = "FGA",
                                "Three-Point Attempts" = "X3PA",
                                "Three-Point %" = "X3P.",
                                "Assists" = "AST",
                                "Rebounds" = "TRB"))
      ),
      mainPanel(
        plotOutput("scatterPlot"),
        verbatimTextOutput("regressionOutput"),
        verbatimTextOutput("columnNames")
      )
    )
  ),
  
  # Data Overview Tab
  tabPanel("Data Overview",
    mainPanel(
      h3("Dataset Information"),
      verbatimTextOutput("dataSummary"),
      h3("Column Names"),
      verbatimTextOutput("columnNames2")
    )
  ),
  
  # About Tab
  tabPanel("About",
    mainPanel(
        h4("By Nicholas Wong and Brian Bhola"),
        h4("Dataset:"),
        tags$a(href = "https://www.kaggle.com/datasets/vivovinco/2023-2024-nba-player-stats", 
        target = "_blank",
            "NBA Player Data 2023-2024 Season")
      
    )
  )
)

server <- function(input, output) {
  data <- reactive({ #stores csv in data()
    df <- read.csv("nba.csv", sep = ";", fileEncoding = "UTF-8") %>%
      distinct(Player, .keep_all = TRUE)

    colnames(df)[which(names(df) == "3PA")] <- "X3PA"
    df
  })

  output$scatterPlot <- renderPlot({
    df <- data()
    
    # Create a mapping of variable names to their labels
    var_labels <- c(
      "MP" = "Minutes Played per Game",
      "FGA" = "Field Goal Attempts per Game",
      "X3PA" = "Three-Point Attempts per Game",
      "X3P." = "Three-Point %",
      "AST" = "Assists per Game",
      "TRB" = "Total Rebounds per Game"
    )
    
    ggplot(df, aes_string(x = input$xvar, y = "PTS")) +
      geom_point(color = "steelblue") +
      labs(title = paste("Points vs", var_labels[input$xvar]),
           x = var_labels[input$xvar], y = "Points per Game") +
      theme_minimal()
  })

  output$columnNames <- renderPrint({
    df <- data()
    colnames(df)
  })

  output$columnNames2 <- renderPrint({
    df <- data()
    colnames(df)
  })

  output$dataSummary <- renderPrint({
    df <- data()
    summary(df)
  })

  output$regressionOutput <- renderPrint({
    df <- data()
    model <- lm(PTS ~ MP + FGA + FG + X3PA + `X3P.` + AST + TRB, data = df)
    summary(model)
  })

  output$dataTable <- renderDT({
    datatable(data(),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                scrollY = "400px"
              ))
  })
}

shinyApp(ui = ui, server = server)
