library(ggplot2)
library(dplyr)
library(here)
library(tidyr)
library(gapminder)
library(readr)
library(forcats)
library(googledrive)

#access file from Google Drive
drive_auth() #press 1 to access new Google Drive
#login: jgmastersresearchhub@gmail.com
#password: 3@syP@5sw0rd

file1 <- drive_get("connections_list.csv")
connections <- drive_read_string(file1) %>% 
  read_csv()
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

file2 <- drive_get("species_participants.csv")
sgcn <- drive_read_string(file2) %>% 
  read_csv()
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


