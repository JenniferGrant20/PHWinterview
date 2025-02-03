
# PHW interview preparation
# Script 3
# Forecasting the next 5 years of expenditure
# Jennifer Grant
# 29/01/25

################################################################################
# Load packages

library(GGally)

################################################################################
# Linear regression 

# Extract only the annual totals from datalong and store in a new data frame
df <- datalong %>% filter(X.1 == X.2 & X.1 == "Total") %>%
  select(-c(X.1, X.2))

# Create data frame of next 5 years to use for predictions
newdata <- data.frame(year = c(2023, 2024, 2025, 2026, 2027))

# Slide 14 graph
# Create correlation matrix for year and expenditure
ggpairs(data = df) + 
  theme_bw() +
  ggtitle("Correlation Matrix of Annual Total Expenditure Data",
          subtitle = "From 2009-10 to 2022-23") +
  theme(text = element_text(size = 20))

# Linear regression model
model <- lm(expenditure ~ year, data = df)
summary(model)

# Unused graph
# Plot histogram of residuals
ggplot(data = df, aes(model$residuals)) +
  geom_histogram(fill = "steelblue", binwidth = 30) +
  theme_minimal() +
  theme(axis.line.x = element_line(),
        axis.line.y = element_line()) +
  ggtitle("Histogram for Model Residuals")

# Slide 15 graph
# Plot linear model using historical data
ggplot(data = df, aes(x = year, y = (expenditure/1000))) +
  geom_point(size = 2.5) +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line(),
        text = element_text(size = 18)) +
  ggtitle("Linear Model Fitted to Data") +
  xlab("Financial Year Beginning") +
  ylab("Expenditure (£ billion)") +
  ylim(0, 11)

# Use model and newdata to predict next 5 years of expenditure
prediction <- predict(model, newdata, interval = "confidence")
prediction <- data.frame(year = c(2023, 2024, 2025, 2026, 2027), prediction) %>%
  rename('expenditure' = 'fit')
#print(prediction)
df <- rbind(df, prediction[,1:2]) # add next 5 years to historical data

# Slide 16 graph
# Plot the forecasted data with the historical data
ggplot(data = df, aes(x = year, y = (expenditure/1000))) +
  geom_point(size = 2.5) +
  stat_smooth(method = "lm", col = "dodgerblue", alpha = 0.3) +
  geom_errorbar(data = prediction, 
                mapping = aes(ymin=(lwr/1000), ymax=(upr/1000), x = year), 
                width=.08, colour = "black") +
  theme_classic() +
  ylim(0, 11) +
  ylab("Expenditure (£ billion)") +
  xlab("Financial Year Beginning") +
  theme(text = element_text(size = 18)) +
  ggtitle("Historical and Forecasted NHS Wales Expenditure")

