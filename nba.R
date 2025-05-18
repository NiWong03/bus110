# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the data
nba_data <- read.csv("nba.csv", sep = ";", header = TRUE, fileEncoding = "UTF-8")

nba_df <- nba_data %>% #deletes duplicate players
  distinct(Player, .keep_all = TRUE)
colnames(nba_df)


ggplot(nba_df, aes(x = MP, y = PTS)) +  # Assuming 'Points' is the column for points scored
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Points vs Minutes Played",
       x = "Minutes Played (MP)",
       y = "Points Scored") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(nba_df, aes(x = FGA, y = PTS)) + 
  geom_point(color = "red") +
  labs(title = "Scatter Plot of Points vs Shots Attempted",
       x = "Shots Attempted",
       y = "Points Scored") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(nba_df, aes(x = `X3PA`, y = PTS)) +  # Use backticks for column names with special characters
  geom_point(color = "green") +
  labs(title = "Scatter Plot of Points vs Three-Point Attempts",
       x = "Three-Point Attempts (3PA)",
       y = "Points Scored (PTS)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(nba_df, aes(x = `X3P.`, y = PTS)) + 
  geom_point(color = "orange") +
  labs(title = "Scatter Plot of Points vs Three-Point Percentage",
       x = "Three-Point Percentage (3P%)",
       y = "Points Scored (PTS)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(nba_df, aes(x = AST, y = PTS)) + 
  geom_point(color = "#bd213b") +
  labs(title = "Scatter Plot of Points vs Assists",
       x = "Assists",
       y = "Points Scored (PTS)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(nba_df, aes(x = TRB, y = PTS)) + 
  geom_point(color = "#123393") +
  labs(title = "Scatter Plot of Points vs Rebounding",
       x = "Rebounding",
       y = "Points Scored (PTS)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



