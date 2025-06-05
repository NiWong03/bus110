install.packages("psych")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("corrplot")
install.packages("car")
install.packages("ggpubr")
install.packages("lmtest")
install.packages("sandwich")
install.packages("kableExtra")
install.packages("moments")
install.packages("ecm")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(stargazer)
library(psych)
library(corrplot)
library(car)
library(readxl)
library(lmtest)
library(sandwich)
library(ggpubr)
library(haven)
library(ecm)
library(moments)
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


model <- lm(PTS ~ MP + FGA + `FG.` + X3PA + `X3P.` + AST + TRB, data = nba_df)

summary(model)

stargazer(model, type = "text", title = "Regression Results", digits = 3)



# Create diagnostic plots for the regression model
par(mfrow = c(2,2))  # Set up a 2x2 grid for plots
plot(model, 
     main = "Regression Diagnostic Plots",
     pch = 16,        # Use filled circles for points
     col = "blue")    # Use blue color for points

# Reset the plotting parameters
par(mfrow = c(1,1))

# Create a plot of actual vs predicted values
predicted_values <- predict(model)
actual_values <- nba_df$PTS

plot(actual_values, predicted_values,
     main = "Actual vs Predicted Values",
     xlab = "Actual Points",
     ylab = "Predicted Points",
     pch = 16,
     col = "blue")
abline(0, 1, col = "red", lwd = 2)  # Add a 45-degree line

# Create a clean regression table for your write-up
stargazer(model, type = "text", title = "Regression Results", digits = 3)

# Keep only the columns of interest
nba_stats <- nba_df %>%
  select(MP, PTS, TRB, AST, FG., TOV, X3PA, X3P., FGA)

# Rename confusing columns for clarity (optional but helpful)
nba_stats <- nba_stats %>%
  rename(
    `FG%` = FG.,
    `3PA` = X3PA,
    `3P%` = X3P.,
    `FG Attempts` = FGA
  )

# Generate descriptive statistics
nba_summary <- describe(nba_stats)

# View result
print(nba_summary[, c("n", "mean", "median", "sd", "min", "max")])

# Create correlation matrix for selected variables
correlation_vars <- nba_df %>%
  select(MP, FGA, `FG.`, X3PA, `X3P.`, AST, TRB, PTS)

# Calculate correlation matrix
cor_matrix <- cor(correlation_vars, use = "complete.obs")

# Create correlation heatmap
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE,
         title = "Correlation Heatmap of NBA Statistics",
         mar = c(0,0,2,0))

#  Linearity Check
plot (fitted.values(model), residuals(model))
abline(0, 0)

# Independence test
dwtest(model)
res <- resid(model)
acf(res)
#  VIF 
vif(model)

# Breusch-Pagan test
bp_test <- bptest(model)
print(bp_test)

# Normality test
# Create a data frame with residuals
residuals_df <- data.frame(residuals = residuals(model))

# Create histogram with normal curve
hist_plot <- ggplot(residuals_df, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30,
                 fill = "lightblue",
                 color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(residuals_df$residuals), 
                           sd = sd(residuals_df$residuals)),
                color = "red",
                size = 1) +
  labs(title = "Histogram of Residuals with Normal Curve",
       x = "Residuals",
       y = "Density") +
  theme_minimal()

# Create Q-Q plot
qq_plot <- ggplot(residuals_df, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

hist_plot
qq_plot 

# Perform Shapiro-Wilk test for normality
shapiro.test(res)

simple_reg <- lm(PTS ~ FGA, data = nba_df)

summary(simple_reg)
plot(fitted(simple_reg), resid(simple_reg))
abline(0,0)
coeftest(simple_reg, vcov = vcovHC(simple_reg, type = 'HC0'))
