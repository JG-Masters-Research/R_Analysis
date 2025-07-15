library(ggplot2)
library(dplyr)
library(here)
library(tidyr)
library(gapminder)
library(readr)
library(forcats)

#access file from github

file1 <- "https://raw.githubusercontent.com/JG-Masters-Research/R_Analysis/refs/heads/main/connections_list.csv?token=GHSAT0AAAAAADHNDBUTDU2PWAOSMWXGMRCQ2DWQUAA"
connections <- read_csv(file1)
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

file2 <- "https://raw.githubusercontent.com/JG-Masters-Research/R_Analysis/refs/heads/main/species_participants.csv?token=GHSAT0AAAAAADHNDBUSVSEO5ILGE3B2LSLI2DWQVCA"
sgcn <- read_csv(file2)
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
