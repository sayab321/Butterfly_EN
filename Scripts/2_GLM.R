###########################
#GLMs analysis
###########################
#libraries
library(tidyverse); library(readxl); library(car); library(emmeans); library(DHARMa)

# load data
data <- read_excel("~/Desktop/Pratiksha/Analysis/2_GLM.xlsx")

data_clean <- data %>% separate(Assemblage, into = c("Month", "Habitat"), sep = "_") %>% 
  mutate(Month = as.factor(Month), 
    Habitat = as.factor(Habitat))

# Ensure Forest is reference baseline group
data_clean$Habitat <- relevel(data_clean$Habitat, ref = "Forest")

# MODEL 1: Richness
glm_rich <- glm(Richness ~ Habitat + Month, data = data_clean, family = Gamma(link = "log"))

# Get Significance Tables
print(Anova(glm_rich, type = "II", test.statistic = "F"))
print(summary(glm_rich))

# Post-hoc pairwise comparisons
h_comp_rich <- emmeans(glm_rich, pairwise ~ Habitat, type = "response")
print(h_comp_rich)

# Diagnostics
res_rich <- simulateResiduals(glm_rich)
plot(res_rich)

# MODEL 2: Shannon
glm_shannon <- glm(Shannon ~ Habitat + Month, data = data_clean, family = Gamma(link = "log"))

# Get Significance Tables 
print(Anova(glm_shannon, type = "II", test.statistic = "F"))
print(summary(glm_shannon))

# Post-hoc pairwise comparisons
h_comp_shannon <- emmeans(glm_shannon, pairwise ~ Habitat, type = "response")
print(h_comp_shannon)

# Diagnostics
res_shannon <- simulateResiduals(glm_shannon)
plot(res_shannon)

# MODEL 3: Simpson
glm_simpson <- glm(Simpson ~ Habitat + Month, data = data_clean, family = Gamma(link = "log"))

# Get Significance Tables
print(Anova(glm_simpson, type = "II", test.statistic = "F"))
print(summary(glm_simpson))

# Post-hoc pairwise comparisons
h_comp_simpson <- emmeans(glm_simpson, pairwise ~ Habitat, type = "response")
print(h_comp_simpson)

# Diagnostics
res_simpson <- simulateResiduals(glm_simpson)
plot(res_simpson)

###plot
# prepare data
data_clean <- data_clean %>%
  mutate(Month = factor(Month, levels = c("May", "June", "July", "August")),
    Habitat = factor(Habitat, levels = c("Forest", "Cropland", "Settlement")))

# color
land_palette <- c("Cropland" = "#E69F00", "Forest" = "#009E73", "Settlement" = "#0072B2")
month_palette <- c("May" = "#0057E9", "June" = "#87E911", "July" = "#F2CA19", "August" = "#FF00BD")

# Theme
theme_pub <- theme_minimal() + 
  theme(panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none")

# Habitat plot
h1 <- ggplot(data_clean, aes(x = Habitat, y = Richness, fill = Habitat)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  scale_fill_manual(values = land_palette) +
  labs(title = "Species Richness", y = "Count", x = NULL) + theme_pub

h2 <- ggplot(data_clean, aes(x = Habitat, y = Shannon, fill = Habitat)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  scale_fill_manual(values = land_palette) +
  labs(title = "Shannon Diversity", y = "Index Value", x = "Habitat") + theme_pub

h3 <- ggplot(data_clean, aes(x = Habitat, y = Simpson, fill = Habitat)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  scale_fill_manual(values = land_palette) +
  labs(title = "Simpson Diversity", y = "Index Value", x = NULL) + theme_pub

# Month plot
m1 <- ggplot(data_clean, aes(x = Month, y = Richness, fill = Month)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  scale_fill_manual(values = month_palette) +
  labs(title = "", y = "Count", x = NULL) + theme_pub

m2 <- ggplot(data_clean, aes(x = Month, y = Shannon, fill = Month)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  scale_fill_manual(values = month_palette) +
  labs(title = "", y = "Index Value", x = "Month") + theme_pub

m3 <- ggplot(data_clean, aes(x = Month, y = Simpson, fill = Month)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  scale_fill_manual(values = month_palette) +
  labs(title = "", y = "Index Value", x = NULL) + theme_pub

# combine
final_plot <- (h1 | h2 | h3) / (m1 | m2 | m3) + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(face = "bold"))

# View result
print(final_plot)
