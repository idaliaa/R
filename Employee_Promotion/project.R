library(readr)
library(dplyr)
library(janitor)
library(tidyverse)
library(ggstats)

test <- read_csv("Downloads/test.csv")
train <- read_csv("Downloads/train.csv")
spec(test)
spec(train)
train <- train %>% clean_names()
test <- test %>% clean_names()
head(test)

#Cleaning Data
typeof(train)
class(train)
str(train)

glimpse(test)
glimpse(train)

# Checking Missing Values
sum(is.na(train)) #6533
sum(is.na(test)) #2846

# Create a new df without na
test <- na.omit(test)
train <- na.omit(train)

# These column wont show in train data
train$region <- NULL
train$awards_won <- NULL
train$employee_id <- NULL

# These column wont show in test data
test$region <- NULL
test$awards_won <- NULL
test$employee_id <- NULL

train <- train %>% mutate(gender = recode(gender, 'f' = 'Female', 'm' = 'Male'))
test <- test %>% mutate(gender = recode(gender, 'f' = 'Female', 'm' = 'Male'))

# Descriptive TRAIN of Analysis AGE & LENGTH OF SERVICE & TRAINING SCORE
mean(train$age) #35.58944
mean(train$length_of_service) #6.31157
median(train$age) #34
median(train$length_of_service) #5
max(train$age) #60
max(train$length_of_service) #37
min(train$age) #20
min(train$length_of_service) #1
mean(train$avg_training_score) #63.60331
median(train$avg_training_score) #60
max(train$avg_training_score) #99
min(train$avg_training_score) #39

summary(train$age)
summary(train$length_of_service)
summary(train$avg_training_score)


# Descriptive TEST of Analysis AGE & LENGTH OF SERVICE & TRAINING SCORE
mean(test$age) #35.59109
mean(test$length_of_service) #6.261732
median(test$age) #34
median(test$length_of_service) #5
max(test$age) #60
max(test$length_of_service) #34
min(test$age) #20
min(test$length_of_service) #1
mean(test$avg_training_score) #63.4335
median(test$avg_training_score) #60
max(test$avg_training_score) #99
min(test$avg_training_score) #39

summary(test$age)
summary(test$length_of_service)
summary(test$avg_training_score)

# Number of Employe by Department
table(train$department)
table(train$gender)

# Visualization
ggplot(train, aes(x=gender, y=is_promoted, fill=gender)) +
        geom_bar(stat="identity") +
        labs(x="Gender", y="Is Promoted", title="Promotion Gender Distribution") 

ggplot(train, aes(x=department, fill=education, by=department)) +
  geom_bar(position="fill") +
  labs(x="Department", title="Education Distribution", y="Total") +
  theme_minimal()

ggplot(train, aes(x=recruitment_channel, y=age, fill=recruitment_channel, mark=is_promoted)) +
  geom_col() + (scale_y_continuous(labels= scales::percent)) +
  labs(x="Reqruitment Channel", y="Percent", title="Recruitment Channel Distribution") +
  theme_minimal()


ggplot(train, aes(x=age, y=is_promoted, fill=age)) +
  geom_col() +
  labs(title="Promotion By Age Distribution ", x="Age", y="Is Promoted") +
  theme_minimal()

ggplot(train) +
  geom_point(mapping = aes(x=length_of_service, y=age, color=department)) +
  labs(title="Time in The Company By The Age and Department")

ggplot(train, aes(x=length_of_service, y=avg_training_score, fill=gender)) +
  geom_col() +
  labs(title="Length of Service of Average Training Score", x="Length of Service", y="Avg of Training Score")


ggplot(train, aes(x=avg_training_score, y=previous_year_rating, fill=gender)) +
  geom_col() +
  labs(title="Previous Year Rating for Average Training Score")

ggplot(train, aes(x=age, y=no_of_trainings, fill=gender)) +
  geom_col() +
  labs(title="Age for Number of No Training")
  
ggplot(train, aes(x=department, fill=gender, by=department)) +
  geom_bar(position="fill") + scale_y_continuous(labels= scales::percent) + # Use scales::percent function
  labs(title="Correlation between Department and Percent of Employees", x ="Department", y = "Percent") +
  theme_minimal()






