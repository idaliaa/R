library(readr)
library(janitor)
library(tidyverse)
library(dplyr)
library(psych) #describe() function

# DATA EXPLORATION
data <- read_csv("Downloads/National Universities Rankings.csv")
data <- data %>% clean_names() 
summary(data)
describe(data)
# Missing Value
missing_value <- sum(is.na(data)) #98 NA
rows_with_na <- which(is.na(data), arr.ind = TRUE) #display all rows with na
na_rows <- is.na(data$in_state)
result <- data[na_rows, ]
data$in_state <- as.character(data$in_state) #converting into character
data$in_state[is.na(data$in_state)] <- "NULL"


# Which Universities has the most top-rank ?
top_uni_rank <- data %>%
                group_by(name) %>%
                summarise(rank = mean(rank))  %>%
                arrange(rank) %>%
                slice_head(n=10)


# Make a new city and state-column
data <- data %>%
        separate(location, into = c("city", "state"), sep=", ", remove = FALSE)

# In which city in USA is the top 10 Universities ?
city_university <- data %>%
                  filter(rank <= 10) %>%
                  group_by(rank, name, city) %>%
                  summarise(rank = mean(rank)) %>%
                  arrange(rank) %>%
                  slice(1:10)
                  
                
# Visualization
ggplot(city_university, aes(x=rank, y=reorder(name, rank))) +
  geom_bar(stat="identity", width = 0.6, color="black", fill="darkblue") +
  labs(x="Universities", y="Rank", title="Top 10 Universities") +
  scale_x_continuous(breaks = seq(0, 10, by = 1))
  

# plotting the most top-ranked universities by the states
top_uni_by_state <- data %>%
                    group_by(state) %>%
                    summarise(rank = mean(rank)) %>%
                    arrange(rank) %>%
                    slice(1:10) 

# Which state are in the Top 20 with the Most Top-Ranked Universities?
state_with_top_uni <- data %>%
                      filter(rank <= 20) %>%
                      group_by(state) %>%
                      count(state)
    
# Visualization
ggplot(state_with_top_uni, aes(x=state, y=n)) +
    geom_bar(stat="identity", fill="lightblue", color="blue") +
    labs(x="State", y="Total", title="Top State Universities")

#data is already arrange according to the rank head and tail
top_twenty_uni <- data %>%
                  filter(rank <= 20) %>%
                  group_by(rank, name)

last_bottom_uni <- data %>%
                  group_by(rank, name) %>%
                  tail(n= 20)


# tuition_and_fees as numeric column
data$tuition_and_fees_numeric <- as.numeric(gsub("[\\$,]", "", data$tuition_and_fees))

# Which Uni hast the most expensive tuition fees and the least expensive
most_expensive_uni <- data %>%
                      group_by(tuition_and_fees_numeric, name) %>%
                      summarise(tuition_and_fees_numeric = mean(tuition_and_fees_numeric)) %>%
                      arrange(desc(tuition_and_fees_numeric)) %>%
                      head(n = 15)

# Visualization. arrange by the tuition fees
ggplot(most_expensive_uni, aes(x=tuition_and_fees_numeric, y=reorder(name, -tuition_and_fees_numeric))) + #the y-axis (vertical axis) of the plot will display the universities' names, reordered based on their corresponding tuition fees 
    geom_bar(stat="identity", color="black", fill="darkred") +
    labs(y="Universities", x="Tuition Fees", title="The most expensive Universities") +
    geom_text(aes(label=tuition_and_fees_numeric), hjust=-0.5, color="black", size=3) 
  
# Which Uni has the most Undergrad Enrollment ?                      
undergrad_enrollment <- data %>%
                        group_by(undergrad_enrollment, name, tuition_and_fees) %>%
                        arrange(desc(undergrad_enrollment)) %>%
                        head(n=20)

# Visualization
ggplot(undergrad_enrollment, aes(x=undergrad_enrollment, y=reorder(name, undergrad_enrollment))) + #y=reorder(name, -undergrad_enrollment): top-down ordered
    geom_bar(stat="identity", color="black", fill="lightblue") +
    labs(x="Total Enrollment", y="Universities", title="Undergrad Enrollment") +
    geom_text(aes(label=undergrad_enrollment), hjust=-0.5, color="black", size=3)

# Make the In Stat numeric
data$in_state_numeric <- as.numeric(gsub("[^0-9.]", "", data$in_state)) #remove all non-numeric characters except for NA values

# The Difference Between In Stat and Tuition Fee from Out Stat
tuition_and_instat <- data %>%
                      group_by(tuition_and_fees_numeric, in_state_numeric, name)  %>%
                      summarise(tuition_and_fees_numeric = mean(tuition_and_fees_numeric))
                      
# Visualization
ggplot(tuition_and_instat, aes(x=tuition_and_fees_numeric)) +
  geom_histogram(color="black", fill="darkred", bins=10) +
  labs(x="Tuition Fees", y="Total", y="Tuition Fees", title="Tuition Fees") +
  stat_bin(geom = "text", aes(label = ..count..), vjust = -0.5, color = "black", size = 3) 

ggplot(tuition_and_instat, aes(x=in_state_numeric)) +
  geom_histogram(color="black", fill="darkgreen", bins=10) +
  labs(x="Tuition Fees", y="Total", y="Total", title="In State Price") 
 













