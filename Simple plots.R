library(tidyverse)
position_levels <- c("F", "M/F", "M", "D/M", "D", "GK")

salaries <- read_csv("MLS salaries 2017.csv", col_types = cols(pos = col_factor(levels = position_levels, ordered = TRUE)))

# Get rid of LAFC and players with no teams
salaries_sub <- filter(salaries, !(club == "LAFC" | is.na(club)))

# Plot by position
ggplot(salaries_sub) +
  geom_point(aes(x = club, y = guaranteed/1000000, color = pos), alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = NULL, y = "Guaranteed salary in millions")

# Faceted by position
ggplot(salaries_sub) +
  geom_point(aes(x = club, y = guaranteed/1000000, color = pos), alpha = 0.8) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "2017 MLS Guaranteed Salaries",
       x = NULL, y = "Guaranteed salary in millions",
       color = "Position") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ pos)

# Geom_point with labels of those with salaries over 2 million
library(ggrepel)
high_salaries <- filter(salaries_sub, guaranteed >= 2000000)

# By club with high earners
ggplot(salaries_sub) +
  geom_point(aes(x = club, y = guaranteed/1000000, color = pos), alpha = 0.8, size = 2) +
  geom_label_repel(aes(x = club, y = guaranteed/1000000, label = last_name), data = high_salaries) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "2017 MLS Guaranteed Salaries",
       x = NULL, y = "Guaranteed salary in millions",
       color = "Position") +
  theme(plot.title = element_text(hjust = 0.5))

# Geom_point with points of those with salaries under 1 million
low_salaries <- filter(salaries_sub, guaranteed < 1000000)

ggplot(low_salaries) +
  geom_point(aes(x = club, y = guaranteed/10000, color = pos), alpha = 0.8, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "2017 MLS Guaranteed Salaries under One Million Dollars",
       x = NULL, y = "Guaranteed salary in Ten Thousands",
       color = "Position") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(low_salaries) +
  geom_boxplot(aes(x = club, y = guaranteed/10000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "2017 MLS Guaranteed Salaries under One Million Dollars",
       x = NULL, y = "Guaranteed salary in Ten Thousands",
       color = "Position") +
  theme(plot.title = element_text(hjust = 0.5))

# Barchart by position
ggplot(salaries_sub) +
  geom_bar(aes(x = pos, y = guaranteed/1000000, fill = club), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = NULL, y = "Guaranteed salary in millions")

ggplot(salaries_sub) +
  geom_bar(aes(x = pos, y = guaranteed/1000000, fill = club), stat = "identity", position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = NULL, y = "Guaranteed salary in millions")

ggplot(data = salaries_sub, mapping = aes(x = pos, y = guaranteed/1000000, color = club)) +
  geom_bar(fill = NA, stat = "identity", position = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = NULL, y = "Guaranteed salary in millions")

# Barchart by club with color for position
ggplot(salaries_sub) +
  geom_bar(aes(x = club, y = guaranteed/1000000, fill = pos), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "2017 MLS Guaranteed Salaries by Position",
         caption = "Data from MLS Players Union",
         x = NULL, y = "Guaranteed salary in millions",
         fill = "Position") +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot by position
ggplot(salaries_sub) +
  geom_boxplot(aes(x = pos, y = guaranteed/1000000)) +
  labs(x = NULL, y = "Guaranteed salary in millions")

# Barchart by player
library(RColorBrewer)
pal <- brewer.pal(9, "OrRd")

### Bar charts by Salaries of individual players ###
salaries_ordered <- (arrange(salaries_sub, guaranteed))
library(RColorBrewer)

ggplot(salaries_ordered) +
  geom_bar(aes(x = reorder(club, guaranteed, FUN = sum), y = guaranteed/1000000, fill = guaranteed/1000000), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "2017 MLS Guaranteed Salaries by Players",
       caption = "Data from MLS Players Union",
       x = NULL, y = "Guaranteed salary in millions",
       fill = "Player Salaries\nin millions") +
  theme(plot.title = element_text(hjust = 0.5))

# Ratio by players
ggplot(salaries_ordered) +
  geom_bar(aes(x = reorder(club, guaranteed, FUN = sum), y = guaranteed/1000000, fill = guaranteed/1000000), stat = "identity", position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "2017 MLS Guaranteed Salaries by Position",
       caption = "Data from MLS Players Union",
       x = NULL, y = "Guaranteed salary in millions",
       fill = "Player Salaries\nin millions") +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())

# Reorder by total salary
ggplot(salaries_sub) +
  geom_bar(aes(x = reorder(club, guaranteed, FUN = sum), y = guaranteed/1000000, fill = pos), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "2017 MLS Guaranteed Salaries by Position",
       caption = "Data from MLS Players Union",
       x = NULL, y = "Guaranteed salary in millions",
       fill = "Position") +
  theme(plot.title = element_text(hjust = 0.5))

# Ratio
ggplot(salaries_sub) +
  geom_bar(aes(x = club, y = guaranteed/1000000, fill = pos), stat = "identity", position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Ratio of Pay Roll by Position for Each MLS Team",
       caption = "Data from MLS Players Union",
       x = NULL, y = "Salary",
       fill = "Position") +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank())

# Dodge
ggplot(salaries_sub) +
  geom_bar(aes(x = club, y = guaranteed/1000000, fill = pos), stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Ratio of Pay Roll by Position",
       caption = "Data from MLS Players Union",
       x = NULL, y = "Guaranteed salary in millions",
       fill = "Position")

### Data Mutations ###
team_salaries <- salaries_sub %>%
  group_by(club) %>%
  summarise(total = sum(guaranteed)) %>% 
  arrange(desc(total))

salary_mean <- salaries_sub %>%
  group_by(club) %>%
  summarise(mn = mean(guaranteed))


# Reorder by total salary
ggplot() +
  geom_line(data = salary_mean, aes(x = club, y = mean)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "2017 MLS Guaranteed Salaries by Position",
       caption = "Data from MLS Players Union",
       x = NULL, y = "Guaranteed salary in millions",
       fill = "Position") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(salary_mean) +
  geom_point(aes(x = club, y = mn))


# https://datascienceplus.com/visualizing-mls-player-salaries/
# Cannot quite get it to work
ggplot(salaries_sub, aes(x = club, y = guaranteed/1000000, fill = guaranteed)) +
  geom_bar(stat = 'identity') +
  labs(caption = "Data from MLS Players Union",
       x = NULL, y = "Guaranteed salary in millions") +
  coord_flip() +
  geom_text(data = filter(salaries_sub, guaranteed >= 2000000), aes(label = last_name), position = position_stack(vjust = 1))


