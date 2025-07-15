library(ggplot2)
library(dplyr)
library(here)
library(tidyr)
library(gapminder)
library(readr)
library(forcats)

#access file from github

file1 <- "https://raw.githubusercontent.com/JG-Masters-Research/R_Analysis/refs/heads/main/connections_list.csv?token=GHSAT0AAAAAADHNDBUSBYNEWZYRUVPHMVLE2DWQX3Q"
connections <- read_csv(file1)
print(head(connections))

#specify data

nongame_school <- connections %>% 
  filter(type %in% c("ngo-nongame" , "school" , "partnership" , "committee-nongame"))

nongame_school

#number of filtered agencies per state

agencies <- nongame_school %>% 
  group_by(state) %>% 
  summarize(total_agencies = n()) %>% 
  ungroup()

agencies

#add SGCN data

file2 <- "https://raw.githubusercontent.com/JG-Masters-Research/R_Analysis/refs/heads/main/species_participants.csv?token=GHSAT0AAAAAADHNDBUSCSX5ABDKEH7JSCJ42DWQXXQ"
sgcn <- read_csv(file2)
print(head(sgcn))

#combine data to only include SGCN column

agencies_sgcn <- agencies %>% 
  left_join(sgcn %>% select(state, SGCN), by = "state")

#create plot with new table

nongame_school_plot <- ggplot(agencies_sgcn, aes(x = fct_reorder(state, SGCN), y = total_agencies)) +
  geom_point(color = "steelblue", fill = "green", shape = 21, alpha = 1) +
  geom_point(aes(size = SGCN)) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
  labs(
    title = "Nongame and School Participants",
    subtitle = "Determining factors of higher numbers",
    x = "State",
    y = "Nongame NGO and School Participants",
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

nongame_school_plot
