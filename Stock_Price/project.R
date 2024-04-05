library(readr)
library(janitor)
library(tidyverse)
library(dplyr)

data <- read_csv("Downloads/stocks.csv")
summary(data)
data <- data %>% clean_names()
glimpse(data)

# How Many Observation Stock Companies ??
# Only 4 : Apple, Microsoft, Netflix, Google
company <- data %>%
          group_by(ticker) %>%
          distinct(ticker) 


# Visualization
ggplot(data, aes(x=date, y=volume, color=ticker)) +
    geom_line() +
    labs(title = "Stock Volume Over Time", x = "Date", y = "Volume") +
    theme_minimal()

ggplot(data, aes(x=date, y=open, color=ticker)) +
  geom_line() +
  labs(title = "Stock Opening Price Over Time", x = "Date", y = "Opening Price") +
  theme_minimal()

ggplot(data, aes(x=date, y=close, color=ticker)) +
  geom_line() +
  labs(title = "Stock Closing Price Over Time", x = "Date", y = "Closing Price") +
  theme_minimal()

ggplot(data, aes(x=open, y=close, color=ticker)) +
  geom_line() +
  labs(title = "Opening vs. Closing Stock Price", x = "Opening Price", y = "Closing Price") +
  theme_minimal()

ggplot(data, aes(x=low, y=high, color=ticker)) +
  geom_line() +
  labs(title = "High vs. Low Stock Price", x = "Low Price", y = "High Price") +
  theme_minimal()

# Sorting the data by the date
data <- data %>%
        arrange(ticker, date)

# Calculate daily_return
# Daily Return=(Today’s Price/ Yesterday’s Price) − 1
stock_return <- data %>%
                group_by(ticker) %>%
                mutate(daily_return = (close/lag(close))-1)

# Converting Date to Week and Month
stock_return <- stock_return %>%
        mutate(week = lubridate::week(date))
stock_return <- stock_return %>%
  mutate(month = lubridate::month(date))


# Calculate Weekly return
stock_return <- stock_return %>%
                  group_by(ticker, week) %>%
                  mutate(weekly_return = prod(1 + daily_return) -1)

# Visualizing Weekly Return
ggplot(stock_return, aes(x=week, y=weekly_return, color=ticker)) +
    geom_line() +
    labs(title = "Weekly Returns for Each Stock", x = "Week", y = "Weekly Return") +
    theme_minimal() 
    
# Calculate Monthly Return
stock_return <- stock_return %>%
                  group_by(ticker, month) %>%
                  mutate(monthly_return = prod(1 + daily_return) -1)

# Visualizing Monthly Return
ggplot(stock_return, aes(x=month, y=monthly_return, color=ticker)) +
    geom_line() +
    labs(title = "Monthly Returns for Each Stock", x = "Month", y = "Monthly Return") +
    theme_minimal() 

# Filter Average Price of Each Stock
selected_stock <- data %>%
                group_by(ticker) %>%
                summarise(avg_price = mean(close)) %>%
                filter(avg_price > 100)

# Calculationg volatility of each stock
# Low volatility is often associated with a lower level of risk and uncertainty
#High volatility is often associated with a higher level of risk and uncertainty
volatility <- data %>%
              group_by(ticker) %>%
              summarise(volatility = sd(close)) %>%
              arrange(desc(volatility))

# Calculate Support and Resistance Level
support_resistance_level <- data %>%
                          group_by(ticker) %>%
                          summarise(support_level = min(low), resistance_level= max(high))

# Calculate Support and Resistance Level for Apple
ggplot(data %>% filter(ticker == "AAPL"), aes(x = date)) +
  geom_line(aes(y = close), color = "blue") +
  geom_hline(data = support_resistance_level %>% filter(ticker == "AAPL"), aes(yintercept = support_level), linetype = "dashed", color = "green") +
  geom_hline(data = support_resistance_level %>% filter(ticker == "AAPL"), aes(yintercept = resistance_level), linetype = "dashed", color = "red") +
  labs(title = "Support and Resistance Levels for AAPL", x = "Date", y = "Price") +
  theme_minimal()


# Calculate Trend Direction
trend_direction <- ifelse(data$high > data$low, "Upward", ifelse(data$high < data$low, "Downward", "Sideways"))


# Group by stock name and filter for the latest date within each group
latest_date_data <- data %>%
                    group_by(ticker) %>%
                    filter(date == max(date))

# CLOSING PRICE
ggplot(data, aes(x = date, y = close)) +
  geom_line(color = "blue") +
  labs(title = "Trend Analysis", x = "Date", y = "Price") +
  theme_minimal() +
  facet_wrap(~ ticker)

# OPENING PRICE
ggplot(data, aes(x = date, y = open)) +
  geom_line(color = "blue") +
  labs(title = "Trend Analysis", x = "Date", y = "Price") +
  theme_minimal() +
  facet_wrap(~ ticker)

# OPENING vs. CLOSING PRICE
ggplot(data, aes(x = date, y = close)) +
  geom_line(color = "blue") +
  geom_segment(aes(x = date, xend = date, y = open, yend = close), color = "black") +
  facet_wrap(~ ticker) +
  labs(title = "Trend Analysis", x = "Date", y = "Price") +
  theme_minimal()

# SAME SAME
ggplot(data, aes(x = date, y = close)) +
  geom_line(aes(color = "Closing Price"), size = 1) + # Specify color and legend label for closing price
  geom_segment(aes(x = date, xend = date, y = open, yend = close, color = "Opening Price"), size = 1) + # Specify color and legend label for opening price
  facet_wrap(~ ticker) +
  labs(title = "Trend Analysis", x = "Date", y = "Price", color = "Price Type") + # Add color legend
  scale_color_manual(values = c("Opening Price" = "black", "Closing Price" = "blue")) + # Specify colors for legend
  theme_minimal()
  


