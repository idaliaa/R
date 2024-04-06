###### STUDENT FLORISTIC DATA ANALYSIS ######
# Question: were the C-values of the 4 transects different from one another ?


# importing the library
library(tidyverse)
library(here)
library(readxl)
library(janitor)

# reading the file from the path
file1 <- here("Downloads/student floristic quality data", "lfc_fqa_team1.xlsx")
file2 <- here("Downloads/student floristic quality data", "lfc_fqa_team2.xlsx")
file3 <- here("Downloads/student floristic quality data", "lfc_fqa_team3.xlsx")

# reading excel file in R
team1 <- read_excel(file1)
team2 <- read_excel(file2)
team3 <- read_excel(file3)

# combine team1 and team2 and team3 in a rows data frame
fqa_data <- rbind(team1, team2, team3)
View(fqa_data)

# "|>" : pipe operator : %>% 
# fqa_data <- filter(fqa_data, !is.na(species_name))
fqa_data <- fqa_data |>
    # filter() from dplyr package : to filter rows from data
    # filter rows where species_name is not NA
    filter(!is.na(species_name)) |> 
    # fill missing values in column to plot number
    fill(team:plot_number) |>
    # convert col names to lowercase and replaces space with underscores, remove spec. character
    clean_names()
  

fqa_data_tidier <- fqa_data |>
  # mutate() : create or modify new col in a data frame
    mutate(c = case_when(c == "NN" ~ "nn", # change from NN to nn
                         c == "NA" ~ NA_character_, # change NA from logical to NA character
                         TRUE ~ c # keeps the original value from the data
                         ))

fqa_data_tidier |> count(c) # count_c <- count(fqa_data_tidier, c)


fqa_data_tidier <- fqa_data_tidier |>
      # converts the values in column c to numeric data type
      mutate(c = as.numeric(c)) |>
        # remove rows where c = NA(missing), Not Available
        filter(!is.na(c))
      
ggplot(fqa_data_tidier, aes(x=c)) +
      geom_bar() +
      scale_x_continuous(breaks = seq(from=0, to=10, by=1)) + #customize the x-axis
      theme_minimal() + #remove gridline and background colors, only essential elements of the plot
      labs(x="c", y="Total", title="Total c")
  
# how c values compare across sites ?
fqa_data_tidier |> count(transect_name) #count(fqa_data_tidier, transect_name) 

 
fqa_data_tidier <- fqa_data_tidier |>
  # modify the name from Prarie to Prairie etc.
  mutate(transect_name = case_when(transect_name == "Prarie" ~ "Prairie",
                                   transect_name == "Reverie Prairie" ~ "Prairie",
                                   transect_name == "Savannah" ~ "Savanna",
                                   transect_name == "Shooting Star Ravine" ~ "Ravine",
                                   transect_name == "Shooting Star Savanna" ~ "Savanna",
                                   transect_name == "Wetland Restoration" ~ "Wetland",
                                   transect_name == "Wetlands" ~ "Wetland",
                                    TRUE ~ .data$transect_name
                                    ))

fqa_data_tidier |> count(transect_name)

# group the data by transect_name
fqa_data_tidier |> group_by(transect_name) |> 
                  summarize(mean_c = mean(c), #mean value of c column within each group
                            sd_c = sd(c), #standard deviation of c column within each group
                            count = n()) |> #number of observation within each group
                  arrange(-mean_c) #arrange the summarized data in descending order (-) based mean_c

# Visualization of Transect Name vs. C
ggplot(fqa_data_tidier, aes(x= transect_name, y = c)) +
    geom_boxplot() + #line inside-box = median or q2, top line-box = q3, bottom line-box = 1q
    geom_jitter(width = 0.2) +
    labs(x="Transect Name", y="c", title="Transect Name vs. c") +
    theme_minimal()

# Visualization of Transect Name vs c by name
ggplot(fqa_data_tidier, aes(x= transect_name, y = c, color=transect_name)) +
    geom_boxplot()+
    theme_minimal()
    
    
model <- aov(c ~ transect_name, data=fqa_data_tidier) #the ANOVA test by aov()
summary(model)








  
  
  


