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

file2 <- drive_get("species_participants.csv")
sgcn <- drive_read_string(file2) %>% 
  read_csv()
print(head(sgcn))

#layer 1

participants <- ggplot(sgcn, aes(x = fct_reorder(state, SGCN), y = participants)) +
  geom_point(color = "steelblue", fill = "green", shape = 21, alpha = 1) +
  geom_point(aes(size = SGCN)) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
  labs(
    title = "Unique Participants vs SGCN",
    subtitle = "Stated Participants in each SWAP",
    x = "State",
    y = "Unique Participants",
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

participants
