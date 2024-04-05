library(readr)
library(tidyverse)
library(janitor) # cleaning names
library(tidyr) # renaming names
library(pander) # left align

box_match <- read_csv("Downloads/MLBB/Games of the Future 2024 - BoxMatch.csv")
team_mlbb <- read_csv("Downloads/MLBB/Games of the Future 2024 - Team.csv")
battle_spell <- read_csv("Downloads/MLBB/Games of the Future 2024 Battle Spell.csv")
item_stats <- read_csv("Downloads/MLBB/Games of the Future 2024 Item_Stats.csv")
jungle_and_roam <- read_csv("Downloads/MLBB/Games of the Future 2024 Jungle And Roam.csv")

summary(box_match)

############ TEAM_MLBB #############

# check if there is a missing value ? => No missing values
missing_value <- is.na(team_mlbb)
print(missing_value)

# cleaning name
team_mlbb <- team_mlbb |> clean_names() 

# renaming name
team_mlbb <- team_mlbb %>% rename(team_country = team_contry)

# how many team MLBB total ? => 16 Teams
team_mlbb |> count(team)

# from which country are the most player MLBB ? => Philippines: 32 
team_mlbb |> count(country)

# Visualization between team and team_country
ggplot(team_mlbb, aes(x=team, y=team_country, color=country)) +
  geom_point() +
  labs(x="Team", y="Team Country", title="MLBB Team") +
  theme(axis.text.x= element_text(angle=90, vjust=0.5, hjust=1))
  
# Visualization between team country and country where the player come from
# some player from different country play for another country
ggplot(team_mlbb, aes(x=team_country, y=country, color=country)) +
  geom_point() +
  labs(x="Team", y="Country", title="National Teams vs. Countries of Origin") +
  theme(axis.text.x= element_text(angle=90, vjust=0.5, hjust=1))


######### BOX_MATCH ############
# cleaning names
box_match <- box_match |> clean_names()

box_match$match <- format(as.character(box_match$match), justify = "none")
box_match$mvp <- format(as.character(box_match$mvp), justify = "none")
box_match$k <- format(as.character(box_match$k), justify = "none")
box_match$d <- format(as.character(box_match$d), justify = "none")
box_match$a <- format(as.character(box_match$a), justify = "none")
box_match$gold <- format(as.character(box_match$gold), justify = "none")
box_match$rating <- format(as.character(box_match$rating), justify = "none")
box_match$level <- format(as.character(box_match$level), justify = "none")

# checking missing values
missing_value <- is.na(box_match)
sum(missing_value)

# change. Nan to NaN
box_match <- box_match |> 
        mutate(medal = case_when(medal =="Nan" ~ "NaN",
                                 TRUE ~ medal
                                 ))

# Group Stage
# How many Match each teams in a group stage ?

# in a group stage with 2 matches. match1 and match2 => 10 Teams plays
group_stage <- box_match %>% filter(stage == "Group Stage") %>% distinct(match, team) #filter teams play in group stage
count(group_stage) #count the matches in group stage
playoff_roundone <- box_match %>% filter(stage == "Playoffs Round 1") %>% distinct(match, team) #filter teams play in round one
count(playoff_roundone) #count the matches in round one
playyoff_quarterfinal <- box_match %>% filter(stage == "Playoffs Quarterfinals") %>% distinct(match, team) #filter teams play in quarterfinal
count(playyoff_quarterfinal) #count the matches in quarterfinal playoff
playyoff_semifinal <- box_match %>% filter(stage == "Playoffs Semifinals") %>% distinct(match, team) #filter teams play in semifinal
count(playyoff_semifinal) #count the matches in semifinal playoff
playyoff_thirdplace <- box_match %>% filter(stage == "Playoffs Third Place") %>% distinct(match, team) #filter teams play in third place
playyoff_grandfinal <- box_match %>% filter(stage == "Playoffs Grand Final") %>% distinct(match, team) #filter teams play in grand final


# selecting which team in grand final
grandfinal_team <- box_match %>% 
                  filter(stage == "Playoffs Grand Final") %>% 
                  select(team) %>% distinct()

# the most banned ? the most banned from each team ? the most pick ? mvp ?
# the most banned hero 
most_ban_hero <- box_match |> 
                group_by(ban) %>%
                count(ban) %>% 
                filter(n == max(n)) %>%
                select(ban, n)

# Visualization of the most banned hero
ggplot(most_ban_hero, aes(x= ban, y=n)) +
  geom_col(fill="darkgrey", color="black") +
  labs(x="Hero", y="total", title="Heroes Banned") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_y_continuous(breaks = seq(from=0, to=70, by=5))
  
# the most banned from team rrq
most_ban_hero_rrq <- box_match %>% 
                    filter(team == "RRQ Hoshi") %>%
                    count(ban) %>% 
                    filter(n == max(n)) %>%
                    select(ban, n)
  
# the most banned hero from each teams
most_ban_hero_each_team <- box_match %>% 
                          group_by(team) %>% 
                          count(ban) %>% 
                          filter(n == max(n)) %>%
                          select(ban, n)

# Visualize the most banned hero from each team
ggplot(most_ban_hero_each_team, aes(x=ban, y=n, color=team)) +
  geom_point() +
  labs(x="Hero", y="Total", title="Most Banened Hero")
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_y_continuous(breaks = seq(from=0, to=9, by=1))

# the most pick hero
most_pick_hero <- box_match %>% 
                  count(pick)

# Visualize most picked hero
ggplot(most_pick_hero, aes(x=pick, y =n)) +
  geom_point(fill="darkblue") +
  labs(x="Heroes", y="Total", title="Most Picked Heroes") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_y_continuous(breaks = seq(from=0, to=60, by=5))

# the most pick hero from each team
most_pick_hero_each_team <- box_match %>% 
                            group_by(team) %>%
                            count(pick) %>% 
                            filter(n == max(n)) %>%
                            select(pick, n)
                  
# Visualize most picked hero from each team
ggplot(most_pick_hero_each_team, aes(x=pick, y =n, color=team)) +
  geom_point() +
  labs(x="Heroes", y="Total", title="Most Picked Heroes from Each Teams") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_y_continuous(breaks = seq(from=0, to=10, by=1))

# which player has the most mvp?
# mvp player has 1, and non mvp 0
# mvp is always the winner
mvp_player <- box_match %>%
              filter(mvp == "1") %>%
              group_by(players, team) %>% 
              summarise(mvp_count = n())


# Visualization from mvp player
ggplot(mvp_player, aes(x=players, y= mvp_count, color=team)) +
  geom_point() +
  labs(x="Players", y="Total MVP", title="Total MVP Players") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) +
  scale_y_continuous(breaks = seq(from=0, to=6, by=1))
  
# 4 games for grand final and AP Bren wins 3 1 for Onic
mvp_grand_final <- box_match %>%
                  filter(stage == "Playoffs Grand Final" & mvp == "1") %>%
                  group_by(players, team, k, d, a, rating) %>%
                  summarise(mvp_count = n())


# picked hero that used when the game win
top_heroes <- box_match %>%
              filter(win == "Win" & pick != "Lose" & mvp == "1") %>%
              group_by(pick, mvp) %>%
              summarise() %>%
              select(pick)
                   





