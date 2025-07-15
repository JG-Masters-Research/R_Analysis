library(ggplot2)
library(dplyr)
library(here)
library(tidyr)
library(gapminder)
library(readr)
library(forcats)

file2 <- "https://raw.githubusercontent.com/JG-Masters-Research/R_Analysis/refs/heads/main/species_participants.csv"
sgcn <- read_csv(file2)
print(head(sgcn))

#create plot

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
