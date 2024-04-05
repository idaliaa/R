###### Visualizing Sales Trends Over Time #######


# Melt the data frame to long format for long partner
sales_data_long <- reshape2::melt(data, id.vars="Product_Code", variable.name="Week", value.name="Sales")

# convert week column to numeric (remove W präfix)
sales_data_long$Week <- as.numeric(gsub("W", "", sales_data_long$Week))

# Plot sales trends over time
ggplot(sales_data_long, aes(x=Week, y=Sales, color=Week)) +
  geom_line() +
  labs(title="Sales Trends Over 51 Weeks", x="Week", y="Sales") +
  theme_minimal()

ggplot(sales_data_long, aes(x=Week, y=Sales, color=Week)) +
  geom_line() +
  labs(title="Sales Trends Over 51 Weeks", x="Week", y="Sales") +
  theme_minimal()


# calculate summary statistics for sales values
summary_sales <- summary(sales_data_long$Sales)
print(summary_sales)

# calculate total sales for each products in all weeks or in one year
total_sales <- sum(sales_data_long$Sales)
print(sum_sales)

# identity top selling products based on total sales


