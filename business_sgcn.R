library(ggplot2)
library(dplyr)
library(here)
library(tidyr)
library(gapminder)
library(readr)
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

game_business <- connections %>% 
  filter(type %in% c("ngo-consumptive", "business", "utility"))

#number of filtered agencies per state

businesses <- game_business %>% 
  group_by(state) %>% 
  summarize(total_agencies = n()) %>% 
  ungroup()

businesses

#add SGCN data

file2 <- drive_get("species_participants.csv")
sgcn <- drive_read_string(file2) %>% 
  read_csv()
print(head(sgcn))

#combine data to only include SGCN column

business_sgcn <- businesses %>% 
  left_join(sgcn %>% select(state, SGCN), by = "state")

#create plot with new table

business_plot <- ggplot(business_sgcn, aes(x = fct_reorder(state, SGCN), y = total_agencies)) +
  geom_point(color = "steelblue", fill = "green", shape = 21, alpha = 1) +
  geom_point(aes(size = SGCN)) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
  labs(
    title = "Participants with Financial Interest",
    subtitle = "Ordered by SGCN",
    x = "State",
    y = "Business Participants",
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
business_plot
