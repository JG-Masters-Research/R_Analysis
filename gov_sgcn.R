library(ggplot2)
library(dplyr)
library(here)
library(tidyr)
library(gapminder)
library(readr)
library(forcats)

#access file from github

file1 <- "https://raw.githubusercontent.com/JG-Masters-Research/R_Analysis/refs/heads/main/connections_list.csv"
connections <- read_csv(file1)
print(head(connections))

#specify data

government <- connections %>% 
  filter(type %in% c("state", "federal", "indigenous", "local"))

government

#number of filtered agencies per state

gov_agencies <- government %>% 
  group_by(state) %>% 
  summarize(total_agencies = n()) %>% 
  ungroup()

gov_agencies

#add SGCN data

file2 <- "https://raw.githubusercontent.com/JG-Masters-Research/R_Analysis/refs/heads/main/species_participants.csv"
sgcn <- read_csv(file2)
print(head(sgcn))

#combine data to only include SGCN column

gov_sgcn <- gov_agencies %>% 
  left_join(sgcn %>% select(state, SGCN), by = "state")
#create plot with new table

gov_sgcn_plot <- ggplot(gov_sgcn, aes(x = fct_reorder(state, SGCN), y = total_agencies)) +
  geom_point(color = "steelblue", fill = "green", shape = 21, alpha = 1) +
  geom_point(aes(size = SGCN)) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
  labs(
    title = "Government Participants",
    subtitle = "Ordered by SGCN",
    x = "State",
    y = "Government Agency Participants",
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gov_sgcn_plot
