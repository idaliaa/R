library(readr)
library(dplyr)
library(tidyverse)
library(janitor)
library(ggplot2)

# Reading & Cleaning
data <- read_csv("Downloads/1000 companies list and rating.csv")
data <- data |> clean_names()
summary(data)
sum(is.na(data)) #Total NA : 8458
data$critically_rated_for <- ifelse(is.na(data$critically_rated_for), "Unknown", data$critically_rated_for) #change NA into Unknown
data$x1 <- data$x1+1
total_company <- count(distinct(data, company)) #Total: 9112, rest: Unknown
glimpse(data)

# How Many Type of Company?
total_company_type <- data %>%
                      group_by(type) %>%
                      summarise(total = n())
# Visualization
ggplot(total_company_type, aes(y=type, x=total)) +
    geom_bar(stat="identity", fill="skyblue", color="black", width=0.9) +
    labs(y="Company Type", x="Total", title="Number of Company Type") +
    geom_text(aes(label= total), hjust= -0.5, color="black", size=3)

# The Best Rating Company. Top 15
best_rating_company <- data %>%
                      group_by(company) %>%
                      summarise(avg_rating = mean(rating)) %>%
                      arrange(desc(avg_rating)) %>%
                      slice_head(n=15)

# Display Best Rating Company The Good and Bad Review
#### Not that relevant ? less reviews than other company 
company_rating <- data %>%
                  semi_join(best_rating_company, by = "company") %>%
                  arrange(desc(best_rating_company)) %>%
                  group_by(company, highly_rated_for, critically_rated_for)


# almost same with above ~ not relevant
most_valued_rating <- data %>%
    group_by(company, highly_rated_for) %>%
    count(total=n()) %>%
    arrange(desc(total)) %>%
    slice_head(n = 1) %>% #select first row of each company
    ungroup() %>%#no longer have grouping information, allowing you to perform further operations without being affected by the previous grouping
    select(company, most_hightly_rated_for = highly_rated_for)
  
  
  
# the most frequent value from hightly_rated_for
most_outstanding_value <- names(sort(table(data$highly_rated_for), decreasing = TRUE))[1:5]

# create data frame with count of each value in the highly_rated_for
value_count <- table(data$highly_rated_for)
df <- data.frame(highly_rated_for = names(value_count), count=as.numeric(value_count))

# filter the data frame to include only top 5 most frequent values
top_5_values <- df[df$highly_rated_for %in% most_outstanding_value, ]

# Visualization
ggplot(top_5_values, aes(x=highly_rated_for, y=count)) +
  geom_bar(stat="identity", fill="skyblue", color="black", width = 0.5) +
  labs(x="Highly Rated For", y="Frequency", title="Histogram of Top 5 Highly Rated Value") +
  geom_text(aes(label = count), vjust=-0.5, color="black", size=3) #add exact number labels 

# How About BIG 4 Company ???! Deloitte, KPMG, EY, PwC
big_four_company <- data %>%
                    filter(company=="Deloitte" | company =="KPMG India" | company=="Ernst & Young" | company=="PwC") %>%
                    group_by(company, rating, type) %>%
                    summarise()

# Visualization
### Deloitte has the most rating
ggplot(big_four_company, aes(x=company, y=rating)) +
    geom_bar(stat="identity", fill="darkgreen", color="black", width = 0.6) +
    geom_text(aes(label=rating), vjust=-0.5, color="black", size=3) +
    labs(x="Company", y="Rating", title="BIG 4 Company")


# Display all Management Consulting Company
management_consulting_company <- data %>%
                                filter(type == "Management Consulting") %>%
                                group_by(company, type) %>%
                                count()

 
# What is the most reviewed Company and How Old is that ? 
# Most Reviewed == More People worked there ?
# Displaying Top 10 Reviewed Company
most_reviewed_company <- data %>%
  group_by(company, reviewers) %>%
  head(15)
                        
# The Top 10 Oldest and Youngest Company
# I think the data from age column is irrelevant, Checking on the Website, said something else
data <- data %>%
  mutate(age_numeric = as.numeric(gsub("[^0-9]", "", age)))

oldest_company <- data %>%
                filter(!is.na(age_numeric)) %>%
                arrange(desc(age_numeric))
                


# Why 2024 years old ?
"2024 years old" %in% data$age # TRUE 
# checking
checking_age <- data %>%
                filter(age == " years old") %>%
                group_by(company, age) 





