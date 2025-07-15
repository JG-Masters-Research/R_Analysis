library(ggplot2)
library(dplyr)
library(here)
library(tidyr)
library(gapminder)
library(readr)

#access data from github
file3 <- "https://raw.githubusercontent.com/JG-Masters-Research/R_Analysis/refs/heads/main/Funds_per_Species.csv?token=GHSAT0AAAAAADHNDBUSD56MMQPKP5E6FHY42DWQSHA"
funds <- read_csv(file3)
print(head(funds))

#create plot with trendline
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
