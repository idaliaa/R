library(readr)
library(dplyr)
library(tidyverse)
library(janitor)
library(Hmisc)

data <- read_csv("Downloads/Germany_largest_companies.csv")
data <- data %>% clean_names()
data$rank_in_germany <- order(data$global_rank)
describe(data)

# Find the most 10 ten from company sales => VOLKSWAGEN GROUP
top_10_sales <- data %>%
            group_by(sales_billion, company) %>%
            summarise(sales_billion = sum(sales_billion)) %>%
            arrange(desc(sales_billion)) %>%
            head(n=10)

# Visualization
ggplot(top_10_sales, aes(x=company, sales_billion)) +
  geom_bar(stat="identity", color="grey", fill="darkblue", width = 0.7) +
  labs(x="Company", y="Billion", title="Company With Top Sales") +
  geom_text(aes(label=sales_billion), vjust=-0.5, color="black", size=3)
  

# Which Company has the most Profits => VOLKSWAGEN GROUP
most_profits <- data %>%
                group_by(profits_billion, company) %>%
                summarise(profits_billion = sum(profits_billion)) %>%
                arrange(desc(profits_billion))  %>%
                head(n=10)

# Visualization
ggplot(most_profits, aes(x=company, profits_billion)) +
  geom_bar(stat="identity", color="grey", fill="darkgreen", width = 0.7) +
  labs(x="Company", y="Billion", title="Company With Top Profits") +
  geom_text(aes(label=profits_billion), vjust=-0.5, color="black", size=3)


# Which Company has the most assets => DEUTSCHE BANK
most_assets <- data %>%
              group_by(assets_billion, company) %>%
              summarise(assets_billion = sum(assets_billion)) %>%
              arrange(desc(assets_billion))  %>%
              head(n=10)

# Visualization
ggplot(most_assets, aes(x=company, assets_billion)) +
  geom_bar(stat="identity", color="grey", fill="darkblue", width = 0.7) +
  labs(x="Company", y="Billion", title="Company With Top Assets") +
  geom_text(aes(label=assets_billion), vjust=-0.5, color="black", size=3)

# Which Company has the most market value ? => SAP
market_value <- data %>%
              group_by(market_value_billion, company) %>%
              summarise(market_value_billion = sum(market_value_billion)) %>%
              arrange(desc(market_value_billion))  %>%
              head(n=10)
                  
# Visualization
ggplot(market_value, aes(x=company, market_value_billion)) +
  geom_bar(stat="identity", color="grey", fill="darkgreen", width = 0.7) +
  labs(x="Company", y="Billion", title="Company With The Most Market Value") +
  geom_text(aes(label=market_value_billion), vjust=-0.5, color="black", size=3)

# Data of all top values
top_values <- cbind(top_10_sales, most_profits, most_assets, market_value)
#colnames(top_values)[2] <- "company"
column_to_rename <- c(2, 4, 6, 8)
for(i in column_to_rename) {
  colnames(top_values)[i] <- "company"
}

###### profitability ratio ######
# Calculate Profit Margin
# Margin = (Profit/Sales)*100
data$profit_margin <- (data$profits_billion / data$sales_billion) * 100

# Calculate return on Assets (ROA)
data$roa <- (data$profits_billion / data$assets_billion) * 100

# Select only numeric column
numeric_data <- data[, sapply(data, is.numeric)]
# Calculate correlation matrix
correlation_matrix <- cor(numeric_data)

# Visualization: Plot as heatmap
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        symm = TRUE,
        margins = c(5, 5))











