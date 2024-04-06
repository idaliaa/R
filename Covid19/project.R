# import and read data
rm(list=ls()) # removes all variable stored previously
library(Hmisc)
library(dplyr)
library(tidyverse)

data <- read_csv("Downloads/Covid_Data.csv")
describe(data) #Hmisc command, describing data

# cleaned up death column. death = 1
data$new_death <- as.integer(data$death != 0)
# death rate
sum(data$new_death) / nrow(data)


# AGE
# claim : people who die are older
dead = subset(data, new_death == 1)
alive = subset(data, new_death == 0)
mean(dead$age, na.rm = TRUE) # na.rm remove every entry that unknown NA
mean(alive$age, na.rm = TRUE)

# the different is 20 years.
# is this statiscally significant ?
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.95)

# normally, if p-value < 0.05, we reject null hypothesis
# here p-value ~ 0, so we reject the null hypothesis and
# conclude that this is statiscally significant

# GENDER
# claim : gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$new_death, na.rm = TRUE) # 8.4%
mean(women$new_death, na.rm = TRUE) # 3.7%

# is this statiscally significant ?
t.test(men$new_death, women$new_death, alternative = "two.sided", conf.level = 0.99)
# 99% confidence : men have from 0.8% to 8.8% higher chance of dying
# p-value = 0.002 < 0.05, so this is statistically significant
# statistically significant : the fact that men have higher death rate than women in the sample
# and that is representative of the population

#### Case in Country and Where it's locate ####
# Separate into Country and City based on matching countries
case_and_location <- case_and_location %>%
                    mutate(country = ifelse(location %in% country, location, NA),
                           city = ifelse(location %in% country, NA, location))

# Total Case in Each Countries 
country_case <- data %>%
                group_by(country) %>%
                summarise(case_in_country = sum(case_in_country, na.rm = TRUE))
  
# Visualization from total case in countries
ggplot(country_case, aes(x=case_in_country, y=country)) +
  geom_bar(stat = "identity", color="black", fill="darkblue") +
  labs(x="Total Case", y="Country", title="Total Case in a Country") +
  geom_text(aes(label=case_in_country), hjust=-0.5, color="black", size=3)

# Total Case in Each Cities
city_case <- data %>%
            filter(location != country) %>% #filter the city from country
            filter(case_in_country > 0) %>% #removing 0 case
            group_by(location) %>%
            summarise(case_in_country = sum(case_in_country, na.rm = TRUE))
  
# Visualization from total case in cities
ggplot(city_case, aes(y=case_in_country, x=location)) +
  geom_bar(stat = "identity", color="black", fill="darkgreen", width = 0.7) +
  labs(y="Total Case", x="City", title="Total Case in a Cities") +
  geom_text(aes(label=case_in_country), vjust=-0.5, color="black", size=2) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))


#### Does the age has big effect by death case ? ##### 
# Death if == 1 and Alive == 0 . SEE ABOVE
# Define the age ranges
age_breaks <- seq(0, 100, by=10) #define age breaks from 0 to 100 with a step of 10

# Group the death by age ranges
# Actual number of death = death + unrecovered 
death_age <- data %>%
    mutate(age_range = cut(age, breaks=age_breaks, labels=paste0(age_breaks[-length(age_breaks)], "-", age_breaks[-1]))) %>%
    filter(age_range != "NA") %>% 
    group_by(age_range) %>%
    summarise(total_death = sum(new_death == 1 | recovered == 1, na.rm = TRUE)) #dead == 1 and alive == 0

#  ranges by the lower bound of each range
# death_age <- death_age[order(as.numeric(gsub(".*-([0-9]+)$", "\\1", death_age$age_range))), ]

# Visualization 
ggplot(death_age, aes(x=age_range, y=total_death)) +
    geom_bar(stat="identity", color="black", fill="darkblue") +
    labs(x="Age Range", y="Total Death", title="Total Death by The Age Range") 
    


#### What is the most Symptom Causing Death by Corona ? ####
symptomps <- data %>%
            count(symptom)

#### How Many people are recovered from the Corona Virus ####
# Recover == 0
# Not Recover == 1
recover <- data %>%
          mutate(recovered = ifelse (recovered != "0" & recovered != "1", "0", recovered)) %>%
          count(recovered)

# Visualization
ggplot(recover, aes(x=recovered, y=n)) +
    geom_bar(stat="identity", color="black", fill="darkgreen", width = 0.5) +
    labs(x="Recovered", y="Total", title="Total Recovered Cases from Covid 19") +
    geom_text(aes(label=n), vjust=-0.5, color="black", size=3)






