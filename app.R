library(shiny)
library(ggplot2)
library(dplyr)
library(stargazer)
library(DT)
library(tidyr)

ui <- navbarPage(
  "NBA Player Stats Analysis",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tags$div(
    style = "position: absolute; right: 20px; top: 10px; z-index: 1000;",
    tags$a(
      href = "https://github.com/niwong03/bus110",
      target = "_blank",
      tags$img(
        src = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png",
        style = "width: 30px; height: 30px;"
      )
    )
  ),

  tabPanel("Introduction",
    sidebarLayout(
      position = "right",
      sidebarPanel(
        img(src = "Giannis.jpeg", height = "100%", width = "100%", style = "object-fit: contain;")      ),
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
        br(),
        plotOutput("stackedHistogram"), #new stacked histogram
        br(),
        h1("Description of Variables:"),
        tags$ul(
            tags$li(tags$b("MP"), "(Minutes Played per game) – Measures how long a player is on the court, representing overall contribution time"),
            br(),
            tags$li(tags$b("PTS"), " (Points per game) – Reflects scoring ability"),
            br(),
            tags$li(tags$b("TRB"), " (Total Rebounds per game) – Captures rebounding performance, both offensive and defensive"),
            br(),
            tags$li(tags$b("AST"), " (Assists per game) – Measures playmaking and team involvement"),
            br(),
            tags$li(tags$b("FG%"), " (Field Goal Percentage) – Indicates shooting efficiency through average percentage of shots made per game"),
            br(),
            tags$li(tags$b("TOV"), "(Turnovers per game) – Reflects average number of times a player turns the ball over per game"),
            br(),
            tags$li(tags$b("X3PA"), " (Three-Point Attempts per game) – Indicates the number of three-point shots a player attempts per game"),
            br(),
            tags$li(tags$b("X3P."), " (Three-Point Percentage) – Shows the average percentage of three point attempts made per game"),
            br(),
            tags$li(tags$b("FGA"), " (Field Goal Attempts per game) – Represents the average number of shots a player takes per game")
        )
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
                                "Rebounds" = "TRB",
                                "Field Goals" = "FG"))
      ),
      mainPanel(
        plotOutput("scatterPlot"),
        plotOutput("histogramPlot"),
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
      "TRB" = "Total Rebounds per Game",
      "FG" = "Field Goals per Game"
    )
    
    ggplot(df, aes_string(x = input$xvar, y = "PTS")) +
      geom_point(color = "steelblue") +
      labs(title = paste("Points vs", var_labels[input$xvar]),
           x = var_labels[input$xvar], y = "Points per Game") +
      theme_minimal()
  })

  output$histogramPlot <- renderPlot({
    df <- data()
    var <- input$xvar
    ggplot(df, aes_string(x = var)) +
      geom_histogram(fill = "steelblue", color = "black", bins = 20) +
      labs(
        title = paste("Histogram of", var),
        x = var,
        y = "Frequency"
      ) +
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

  output$stackedHistogram <- renderPlot({
    df <- data()
    
    # Create a long format dataset for all variables
    plot_data <- df %>%
      select(MP, FGA, X3PA, `X3P.`, AST, TRB, FG) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Statistic",
        values_to = "Value"
      )
    
    var_labels <- c(
      "MP" = "Minutes Played per Game",
      "FGA" = "Field Goal Attempts per Game",
      "X3PA" = "Three-Point Attempts per Game",
      "X3P." = "Three-Point %",
      "AST" = "Assists per Game",
      "TRB" = "Total Rebounds per Game",
      "FG" = "Field Goals per Game"
    )
    
    ggplot(plot_data, aes(x = Value, fill = Statistic)) +
      geom_histogram(position = "identity", color = "black", bins = 10, alpha = 0.5) +
      labs(
        title = "Distribution of All Statistical Categories",
        x = "Value",
        y = "Number of Players",
        fill = "Statistical Category"
      ) +
      scale_fill_discrete(labels = var_labels) +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5)
      )
  })
}

shinyApp(ui = ui, server = server)
