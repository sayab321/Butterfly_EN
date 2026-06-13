###################
#Monthly diversity
###################
library(iNEXT); library(ggthemes); library(ggplot2); library(readxl); library(patchwork); library(tidyverse); library(UpSetR); library(writexl)

#site wise 
#data <- read_excel("~/Desktop/Analysis/6_1_Forest_month_data.xlsx") #forest
#data <- read_excel("~/Desktop/Analysis/6_2_Cropland_month_data.xlsx") #cropland
#data <- read_excel("~/Desktop/Analysis/6_3_Settlement_month_data.xlsx") #settlement

# Extract the species column and convert it to a character vector
species_names <- as.character(data$Species)

# Extract the counts for each month
counts_months <- data[, c("May", "June", "July", "August")]

# Create a matrix with species counts
data_matrix <- as.matrix(counts_months)

# Check for missing values and handle them if necessary
if (any(is.na(data_matrix))) {
  data_matrix <- na.omit(data_matrix)}

set.seed(123)

# Run iNEXT function
inext_month <- iNEXT(data_matrix, q = 0, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# output from iNEXT
g1 <- ggiNEXT(inext_month, type = 1)

#cb_palette <- c("#1A85FF", "#D41159", "#FFC20A", "#4B0092")
cb_palette <- c("#0057E9", "#87E911", "#F2CA19", "#FF00BD")

#xlim(c(0,500) for Bhadrakali and Shantiban
g1 <- g1 + theme_few() + xlim(c(0,200)) +
  theme(plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
        axis.title.x = element_text(size = 12, family = "serif"),
        axis.title.y = element_text(size = 12, family = "serif"), 
        axis.text.x = element_text(size = 12, family = "serif"), 
        axis.text.y = element_text(size = 12, family = "serif"),
        legend.text = element_text(size = 12, family = "serif"),
        legend.title = element_text(size = 12, family = "serif") # If there is a legend title
  ) + labs(x = "Number of individuals", y = "Species richness", title = "(a)") + 
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +   theme(legend.position = "none")
print(g1)

g2 <- ggiNEXT(inext_month, type = 3)

g2 <- g2 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
  lgend.text = element_text(size = 9, family = "serif"),
) + labs(x = "Sample coverage", y = "Species richness", title = "(b)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +theme(
    legend.position = c(0.21, 0.56),
    legend.background = element_blank())
print(g2)

#Shannon diversity #q=1
inext_month1 <- iNEXT(data_matrix, q = 1, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# Create plot
g3 <- ggiNEXT(inext_month1, type = 3)  # Shannon diversity plot

g3 <- g3 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
) + labs(x = "Sample coverage", y = "exp(Shannon)", title = "(c)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) + 
  theme(legend.position = "none")
print(g3)

#Simpson diversity
# Run iNEXT function for Simpson diversity (q = 2)
inext_month2 <- iNEXT(data_matrix, q = 2, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# Create plot
g4 <- ggiNEXT(inext_month2, type = 3)  # Simpson diversity plot

g4 <- g4 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
) + labs(x = "Sample coverage", y = "1/Simpson", title = "(d)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +
  theme(legend.position = "none")

# Print or display the plots
print(g4)

# Combine the three plots in a single row
combined_plot <- g1+ g2 + g3 + g4 + plot_layout(ncol = 2, nrow = 2)

# Print the combined plot
print(combined_plot)

#########################################################
#combined monthly diversity #combine three sites
###########################################################

#load data
forest <- read_excel("~/Desktop/Analysis/6_1_Forest_month_data.xlsx")
cropland <- read_excel("~/Desktop/Analysis/6_2_Cropland_month_data.xlsx")
settlement <- read_excel("~/Desktop/Analysis/6_3_Settlement_month_data.xlsx")

# Combine the three datasets
combined_data <- bind_rows(forest, cropland, settlement) %>%
  group_by(Species) %>% summarise(across(c("May", "June", "July", "August"), 
                   ~ sum(.x, na.rm = TRUE))) %>% ungroup()

#write_xlsx(combined_data, "~/Desktop/Analysis/6_Combined_month_data.xlsx")

# Extract the species column and convert it to a character vector
species_names <- as.character(combined_data$Species)

# Extract the counts for each season
counts_months <- combined_data[, c("May", "June", "July", "August")]

# Create a matrix with species counts
data_matrix <- as.matrix(counts_months)

# Check for missing values and handle them if necessary
if (any(is.na(data_matrix))) {
  data_matrix <- na.omit(data_matrix)}

set.seed(123)

# Run iNEXT function
combined_month <- iNEXT(data_matrix, q = 0, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# Assuming banpale_season is the output from iNEXT
g1 <- ggiNEXT(combined_month, type = 1)

cb_palette <- c("#0057E9", "#87E911", "#F2CA19", "#FF00BD")

g1 <- g1 + theme_few() + xlim(c(0,1000)) +
  theme(plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
        axis.title.x = element_text(size = 12, family = "serif"),
        axis.title.y = element_text(size = 12, family = "serif"), 
        axis.text.x = element_text(size = 12, family = "serif"), 
        axis.text.y = element_text(size = 12, family = "serif"),
        legend.text = element_text(size = 12, family = "serif"),
        legend.title = element_text(size = 12, family = "serif") # If there is a legend title
  ) + labs(x = "Number of individuals", y = "Species richness", title = "(a)") + 
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +   theme(legend.position = "none")
print(g1)

g2 <- ggiNEXT(combined_month, type = 3)

g2 <- g2 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
  lgend.text = element_text(size = 9, family = "serif"),
) + labs(x = "Sample coverage", y = "Species richness", title = "(b)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +theme(
    legend.position = c(0.21, 0.56),
    legend.background = element_blank())
print(g2)

#Shannon diversity #q=1
combined_month1 <- iNEXT(data_matrix, q = 1, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# Create plot
g3 <- ggiNEXT(combined_month1, type = 3)  # Shannon diversity plot

g3 <- g3 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
) + labs(x = "Sample coverage", y = "exp(Shannon)", title = "(c)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) + 
  theme(legend.position = "none")
print(g3)

#Simpson diversity
# Run iNEXT function for Simpson diversity (q = 2)
combined_month2 <- iNEXT(data_matrix, q = 2, datatype = "abundance", size = NULL, endpoint = NULL, knots = 40, se = TRUE, conf = 0.95, nboot = 200)

# Create plot
g4 <- ggiNEXT(combined_month2, type = 3)  # Simpson diversity plot

g4 <- g4 + theme_few() + theme(
  plot.title = element_text(size = 12, family = "serif", face = "plain", hjust = 0.5),  # Centered title
  axis.title.x = element_text(size = 12, family = "serif"),
  axis.title.y = element_text(size = 12, family = "serif"), 
  axis.text.x = element_text(size = 12, family = "serif"), 
  axis.text.y = element_text(size = 12, family = "serif"),
) + labs(x = "Sample coverage", y = "1/Simpson", title = "(d)") +
  scale_colour_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +
  theme(legend.position = "none")
print(g4)

# Combine the three plots in a single row
combined_plot <- g1+ g2 + g3 + g4 + plot_layout(ncol = 2, nrow = 2)

# Print the combined plot
print(combined_plot)

#
####standarize
# Standardize to 95% coverage to match your best-sampled month (May)
temporal_standardarize <- estimateD(data_matrix, datatype = "abundance", base = "coverage", level = NULL)
print(temporal_standardarize)
write.csv(temporal_standardarize, "~/Desktop/Analysis/Standardized_Hill_Diversity_Monthly.csv", row.names = FALSE)

############################
#####UpsetPlot
##########################

# prepare data
binary_data <- as.data.frame(combined_data)
forest_cols <- c("May", "June", "July", "August")
binary_data[forest_cols] <- lapply(binary_data[forest_cols], function(x) ifelse(x > 0, 1, 0))

# Set the global font family to serif
par(family = "serif")

# plot
upset(binary_data, sets = forest_cols, keep.order = TRUE,               
      sets.bar.color = c("#0057E9", "#87E911", "#F2CA19", "#FF00BD"),
      mainbar.y.label = "Shared species intersections",   
      sets.x.label = "Species count",   
      main.bar.color = "grey25", order.by = "freq", empty.intersections = "on",
      text.scale = c(1.5, 1.2, 1.5, 1.2, 1.5, 1.2))
