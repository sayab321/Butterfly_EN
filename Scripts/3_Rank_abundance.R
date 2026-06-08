############### 
# Rank-Abundance Plot (Land Use type)
############### 
library(readxl); library(tidyverse); library(ggthemes)

#Load Data
data <- read_excel("~/Desktop/Analysis/1_Land_wise_data.xlsx")

# transform data to "Long" format and calculate ranks
rank_data <- data %>%
  pivot_longer(cols = -Species, names_to = "Site", values_to = "Abundance") %>%
  filter(Abundance > 0) %>%
  group_by(Site) %>%
  arrange(Site, desc(Abundance)) %>%
  mutate(Rank = row_number()) %>%
  ungroup()

# color-blind palette (matching PCoA)
cb_palette <- c("#E69F00", "#009E73", "#0072B2")

#Create the Plot
p_rank <- ggplot(rank_data, aes(x = Rank, y = Abundance, color = Site, group = Site)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 2) +
  scale_y_log10() + # Standard for Rank-Abundance to see rare species
  labs(title = "Species Rank-Abundance Curve",
       x = "Species rank",
       y = "Abundance (Log10 scale)") +
  theme_few() +
  theme(text = element_text(family = "serif", size = 12, colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
  scale_color_manual(values = cb_palette)

print(p_rank)
