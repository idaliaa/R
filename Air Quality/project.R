library(readr)
library(tidyverse)
library(janitor)
library(dplyr)
library(Hmisc)

data <- read_csv("Downloads/Data/AirQuality Dataset.csv") #read csv
data <- data |> clean_names() #cleaning names
describe(data) #describe data
data <- data %>% rename("year_2023" = "x2023") #renaming the year


# A city with Country China, China
china_2 <- data %>%
  filter(country=="China, China") 

# Remove China, China, Change to China
data <- data %>% 
        mutate(country = str_replace(country, "China, China", "China"))


# How many city in each country ? What is the most poluted country ?
city_country <- data %>%
                group_by(country) %>%
                count(country) %>%
                arrange(desc(n))
                
# Visualization
ggplot(city_country, aes(x=n, y=reorder(country, n))) +
  geom_bar(stat = "identity", width = 0.6, color="black", fill="darkgreen") +
  labs(x= "Total Cities", y="Countries", title= "Country with The Most Poluted Air in The World") +
  geom_text(aes(label=n), hjust=-0.5, color="black", size=3)

# Average Poluted Air in India in a Year
avg_poluted_air_india <- data %>%
                        filter (country == "India") %>%
                        group_by(country) %>%
                        summarise(avg_number = sum(jan+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec, na.rm = TRUE)/12)
  
# Checking Out the Data
avg_in_Begusarai <- data %>%
                    summarise(avg = sum(data[1, 5:16])/12) # Avg in Begusarai doesn't match ??
avg_in_Guwahati <- data %>%
                  summarise(avg = sum(data[2, 5:16])/12)

# Make new averaged poluted data for year 2023
data$new_avg_column <- round (rowMeans(data[, 5:16], na.rm = TRUE), 1)

# Comparing the Old Avg vs The New Avg
compare_average <- data %>%
                  group_by(city, year_2023, new_avg_column) %>%
                  summarise(different = year_2023 - new_avg_column)

# Count How Many Different Data                  
total_different <- compare_average  %>%
                    group_by(different) %>%
                    count(different)

# AVG Poluted Air in India in January
monthly_averaged_india <- data %>%
                          filter(country == "India")  %>%
                          summarise(avg = mean(jan, na.rm = TRUE))
 
# AVG Poluted Air Each Country in January. Total Country = 38
monthly_avg_by_country <- data %>%
                      group_by(country) %>%
                      summarise(avg_jan = round(mean(jan, na.rm = TRUE), 1))

# AVG Poluted Air Each Country each Month
monthly_average <- data %>%
                group_by(country) %>%
                summarise(across(starts_with("jan"):starts_with("dec"), ~round(mean(., na.rm = TRUE), 1)))

# Reshape data to long format
monthly_average_long <- tidyr::pivot_longer(monthly_average, 
                                            cols = -country,
                                            names_to = "month",
                                            values_to = "avg_value"
                                            )
# Remove rows with missing values
# monthly_average_long <- na.omit(monthly_average_long)

# Convert Month to Factor
monthly_average_long$month <- factor(monthly_average_long$month, levels = unique(monthly_average_long$month))
  
monthly_median <- monthly_average_long %>%
                  group_by(country) %>%
                  summarise(median = median(avg_value, na.rm = TRUE))

# AVG populated air in the world year 2023
overall_mean <- mean(monthly_average_long$avg_value, na.rm = TRUE)

# Visualization
ggplot(monthly_average_long, aes(x=month, y=avg_value, color=country, group=country)) +
    geom_line() +
    geom_point() +
    labs(x="Month", y="Average Value", title="Monthly Average Populated Air") +
    theme_minimal() +
    facet_wrap(~ country, scales = "free_y", ncol=6) +
    geom_hline(yintercept = overall_mean, linetype = "dashed") + # Add overall mean
    guides(color = FALSE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))








  


