###### EXPLORATION DATA #######

# importing and read the data
library(readr)
data <- read_csv("Downloads/Sales_Transactions_Dataset_Weekly.csv")
library(tidyverse)
library(janitor)
#normalized ? the range between the min and max sales amount ???!

# read the sales statistic from W1 to W52
# select column 0 to 52
week_data <- data[,0:53]


# which week has the most 

# change the order of the week, start from W1 to W52
# creating a vector the original week column, start from W0
original_week_columns <- paste0("W", 0:51)
#creating a vector new week column, start from W1
new_week_columns <- paste0("W", 1:52)
# rename week column
names(data)[match(original_week_columns, names(data))] <- new_week_columns 


# change normalized from 1 to 52
original_normalized <- paste("Normalized", 0:51)
new_normalized <- paste("Normalized", 1:52)
names(data)[match(original_normalized, names(data))] <- new_normalized

# check missing values
missing_value <- is.na(data)
print(missing_value) # no missing values


################# NORMALIZED #################
first_column <- data[,1] # extract the first column
col56_to107 <- c(data[, 56:107]) # extract the column between 56 to 107
normalized <- cbind(first_column, col56_to107) # combined first and the normalized column


# count the first row without the first column
sum(normalized[1,-1])

# count the first column
sum(normalized[,2])


# calculate total normalized product for each product from W1 to W52
total_normalized_per_product <- colSums(week_data[,-1])
print(total_normalized_per_product)

# make data frame as a Week and Total Normalized for each Week
total_normalized_df <- data.frame((Week=names(total_normalized_per_product)), Total_Normalized=total_normalized_per_product, stringsAsFactors = FALSE)
print(total_normalized_df)

# displaying the total normalized as histogram 
ggplot(total_normalized_df, aes(x=factor(Week, levels = Week), y=Total_Normalized)) +
  geom_point() +
  labs(x="Week", y="Total Normalized", title="Total Sales in a Year") +
  theme(axis.text.x= element_text(angle=90, vjust=0.5, hjust=1)) + # rotate x-axis labels for better quality
  geom_smooth(method="lm")

ggplot(total_normalized_df, aes(x=factor(Week, levels = Week), y=Total_Normalized, group=1)) +
  geom_line() +
  labs(x="Week", y="Total Normalized", title="Total Sales in a Year") +
  theme(axis.text.x= element_text(angle=90, vjust=0.5, hjust=1)) + # rotate x-axis labels for better quality
  geom_smooth(method="lm") +
  geom_hline(yintercept = median(total_normalized_df$Total_Normalized), color="red") +
  geom_hline(yintercept = mean(total_normalized_df$Total_Normalized), color="green")
  
# check median
median(total_normalized_df$Total_Normalized) # 7213

# check mean
mean(total_normalized_df$Total_Normalized) # 7217.058
  
###### WEEK DATA ######

# calculate total sales for each product from W1 to W52
total_sales_per_product <- colSums(week_data[,-1])
print(total_sales_per_product)

# make data frame as a Week and Total Sales for each Week
total_sales_df <- data.frame((Week=names(total_sales_per_product)), Total_Sales=total_sales_per_product, stringsAsFactors = FALSE)
print(total_sales_df)

# displaying the total sales as histogram 
ggplot(total_sales_df, aes(x=factor(Week, levels = Week), y=Total_Sales)) +
  geom_bar(stat="identity") +
  labs(x="Week", y="Total Sales", title="Total Sales in a Year") +
  theme(axis.text.x= element_text(angle=90, vjust=0.5, hjust=1)) # rotate x-axis labels for better quality


total_sales_W1 <- total_sales_df[1, ] # sales from W1
print(total_sales_W1) # 11275 total sales
total_sales_W25 <- total_sales_df[25, ]  # sales from W25
print(total_sales_W25) # 11489 total sales. the most total is in W25

