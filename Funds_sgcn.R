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

file3 <- drive_get("Funds_per_Species.csv")
funds <- drive_read_string(file3) %>% 
  read_csv()
print(head(funds))

funding <- ggplot(funds, aes(x = fct_reorder(State, SGCN), y = Funds)) +
  geom_point(color = "steelblue", fill = "green", shape = 21, alpha = 1) +
  geom_point(aes(size = SGCN)) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
  labs(
    title = "Annual Funds Compared to Number of SGCN",
    subtitle = "Ordered by SGCN, from 2015 SWAPs",
    x = "State",
    y = "Funds Distributed in 2016",
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

funding
