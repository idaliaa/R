library(readr)
library(tidyverse)
library(janitor)
library(dplyr)
library(knitr)
library(caret)

data <- read_csv("Downloads/supermarket_sales.csv")
data <- data |> clean_names()
#revenue = quantity * selling_price
data$revenue <- (data$quantity * data$unit_price)

# why in this data revenue == cogs ???
# new gross margin 
data$gross_margin_percentage <- (data$revenue / data$gross_income)*100



# who has a member as a customer? => Female
count(data, customer_type, gender) #displaying customer type and the gender

# which product does female mostly buy ?
bougth_product_female <- data %>%
                filter(gender == "Female") %>%
                group_by(product_line) %>%
                summarise(total = sum(quantity > mean(quantity))) %>%
                arrange(desc(total))


# which product does male mostly buy ?
bougth_product_male <- data %>%
                filter(gender == "Male") %>%
                group_by(product_line) %>%
                summarise(total = sum(quantity > mean(quantity))) %>%
                arrange(desc(total))

# check average of all selling product
mean(data$quantity) #5.51
                

# total each product distributed in each city
distributed_product <- data %>%
                      group_by(city) %>%
                      count(product_line)
# Visualization
ggplot(distributed_product, aes(x=n, y=product_line, color=city)) +
    geom_point() +
    labs(x="Total Product", y="Product", title="Distributed Product") 
    #theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) 
  
  
# total ditributed product from each branch
distributed_product_branch <- data %>%
                              group_by(branch) %>%
                              count(product_line)
  
# Visualization
ggplot(distributed_product_branch, aes(x=product_line, y=n, color=branch)) +
    geom_point() +
    labs(x="Product", y="Total Product", title="Distributed Product by Branch") +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) 


# to which city is the most product sell ? 
sell_product <- data %>%
                filter(quantity == 10) %>%
                group_by(city) %>%
                count(product_line)
            
# Visualization
ggplot(sell_product, aes(x=product_line, y=n, color=city)) +
    geom_point() +
    labs(x="Product", y="Total", title="Most Sell Product") +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) 


#### bigger Gross Margin : more profit, smaller gross margin  : smaller profit
# margin 2000

# which product has the most rating ?
most_rating <- data %>%
                filter(rating > 8.0) %>%
                group_by(product_line) %>%
                count()
          
# Visualize the distribution of sales over time (daily, weekly, monthly) to identify trends and seasonality
season_trend <- select(data, date, product_line, quantity, invoice_id, rating)
season_trend$date <- as.Date(season_trend$date, tryFormats = c("%m/%d/%Y")) 

# Extract the year, month, and day of the week from the date column
season_trend$month <- format(season_trend$date, "%m")
season_trend$year <- format(season_trend$date, "%Y")
season_trend$day_of_week <- format(season_trend$date, "%A")

# Aggregate sales data by day, week, and month
# Daily sales distribution
sales_daily <- season_trend %>%
  group_by(date) %>%
  summarize(total_sales = sum(quantity))

# Daily sales Visualization
ggplot(sales_daily, aes(x=date, y=total_sales)) +
    geom_line() +
    labs("x=Date", y="Total Sales", title="Daily Sales Distribution")
    
# Weekly Sales Distribution
sales_weekly <- season_trend %>%
  group_by(year, week = format(date, "%V")) %>%
  summarize(total_sales = sum(quantity))

# Weekly Sales Distribution
ggplot(sales_weekly, aes(x=as.Date(paste(year, week, "1", sep="-"), format="%Y-%U-%u"), y=total_sales)) +
    geom_line() +
    labs("x=Week", y="Total Sales", title="Weekly Sales Distribution")

# Monthly Sales Distribution
sales_monthly <- season_trend %>%
  group_by(year, month) %>%
  summarize(total_sales = sum(quantity))

# Monthly Sales Distribution
#represent the first day of each month paste(year,month, "1", sep="-")
ggplot(sales_monthly, aes(x=as.Date(paste(year, month, "1", sep="-"), format="%Y-%m-%d"), y=total_sales)) +
    geom_line() +
    labs("x=Month", y="Total Sales", title="Monthly Sales Distribution")


  
#### Predict the Value of individual customer based on their historical purchase behavior ####
# Using metrics, Mean Absolute Error MAE, Root Mean Square Error RMSE

# Aggregate Data by Customer
customer_data <- season_trend %>%
                group_by(invoice_id) %>%
                summarize(total_purchase_ammount = sum(quantity),
                          frequency = n(),
                          recency = as.numeric(max(date)-min(date)),
                          avg_purchase_amount = mean(quantity))

# Split data into Training and Test Set
set.seed(123) #for reproducibility
#createDataPartition from caret Package
#p=0.8 80% data will be used for training and remaining 20% for testing
#list=FALSE function returns a vector of indices
train_index <- createDataPartition(customer_data$total_purchase_ammount, p=0.8, list=FALSE)
train_data <- customer_data[train_index, ]
test_data <- customer_data[-train_index, ]

# Train Predictive Model (Linear Regression)
model <-lm(total_purchase_ammount ~ frequency + recency + avg_purchase_amount, data=train_data)

# Make Prediction
predictions <- predict(model, newdata = test_data)

# Evaluate MODEL Performance
mae <- mean(abs(predictions - test_data$total_purchase_ammount)) 
rmse <- sqrt(mean((predictions - test_data$total_purchase_ammount)^2)) 

# Print Evaluation Metrics
cat("Mean Absolute Error (MAE): ", mae, "\n") #mae: 1.060904575e-14 close to 0. the predictive model is performing well
cat("Root Mean Squared Error (RMSE): ", rmse, "\n") #rmse: 1.228854721e-14  close to 0. and suitable for making accurate predictions

# Prediction Visualization
plot(test_data$total_purchase_ammount, predictions, 
     main="Predicted vs Actual",
     xlab="Actual Total Purchase Ammount",
     ylab="Predicted Total Purchase Ammount")
abline(0, 1, col="red")








