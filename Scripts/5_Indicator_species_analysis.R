##########################################################
# Indicator Species Analysis - Land Use & Habitat Associations
##########################################################

# Load Libraries & Data
library(readxl); library(indicspecies); library(tidyverse); library(writexl)

# data
glm_full <- read_excel("~/Desktop/Analysis/5_Ind_data.xlsx")

# prepare data
# Columns 3 to 14 are May(C,F,S), June(C,F,S), July(C,F,S), August(C,F,S)
species_names <- glm_full$Species
data_for_isa <- as.data.frame(t(glm_full[, 3:14]))
colnames(data_for_isa) <- species_names

# Define the Landscape groups
landscape_groups <- rep(c("Cropland", "Forest", "Settlement"), times = 4)

# Run Indicator Analysis
set.seed(123)
indval_landuse <- multipatt(data_for_isa, landscape_groups, 
                            func = "IndVal.g", 
                            control = how(nperm = 999))

# Process Results (Inclusion of Marginal Significance p < 0.1)
res_df <- as.data.frame(indval_landuse$sign)
res_df$Species <- rownames(res_df)

# Create descriptive habitat labels and significance stars
top_indicators <- res_df %>%
  filter(p.value < 0.1) %>%  # Capture top indicators including marginal ones
  mutate(
    Habitat = case_when(
      s.Cropland == 1 & s.Forest == 0 & s.Settlement == 0 ~ "Cropland",
      s.Cropland == 0 & s.Forest == 1 & s.Settlement == 0 ~ "Forest",
      s.Cropland == 0 & s.Forest == 0 & s.Settlement == 1 ~ "Settlement",
      s.Cropland == 1 & s.Forest == 1 & s.Settlement == 0 ~ "Cropland+Forest",
      s.Cropland == 1 & s.Forest == 0 & s.Settlement == 1 ~ "Cropland+Settlement",
      s.Cropland == 0 & s.Forest == 1 & s.Settlement == 1 ~ "Forest+Settlement",
      TRUE ~ "Generalist"
    ),
    Significance = case_when(
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1  ~ ".",
      TRUE           ~ ""
    ),
    # Combine species name with significance stars for the plot
    SpeciesLabel = paste0(Species, Significance)
  ) %>%
  arrange(desc(stat)) %>%  # Rank by indicator strength
  head(10)                 # Keep Top 10 for the main figure

# plot
# Define consistent colors for your land-use types
habitat_colors <- c(
  "Forest" = "#009E73", 
  "Cropland" = "#E69F00", 
  "Settlement" = "#0072B2", 
  "Cropland+Forest" = "#56B4E9", 
  "Cropland+Settlement" = "#CC79A7"
)

ggplot(top_indicators, aes(x = reorder(SpeciesLabel, stat), y = stat, fill = Habitat)) +
  geom_col(width = 0.7, color = "black", size = 0.2) +
  geom_text(aes(label = round(stat, 2)), hjust = -0.2, size = 3.5, family = "serif") +
  coord_flip() +
  scale_fill_manual(values = habitat_colors) +
  labs(
    x = "Indicator Species (Stars indicate significance)", 
    y = "Indicator Value (IndVal)", 
    title = "Top 10 Butterfly Indicators by Landscape Association",
    fill = "Habitat Association"
  ) +
  theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    axis.text.y = element_text(face = "italic", color = "black"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

# Save Final Output
write.csv(top_indicators, "~/Desktop/Analysis/Final_Indicator_Analysis_Detailed.csv", row.names = FALSE)

#months #no significant results