############### 
# PCoA, Heatmap, and Statistics #site #month
############### 
rm(list = ls())

# Load libraries
library(tidyverse); library(vegan); library(ggpubr); library(ggthemes); library(ggplot2); library(readxl); library(patchwork); library(reshape2); library(pairwiseAdonis)

# Load Data
df <- read_excel("~/Desktop/Analysis/3_PCoA_site.xlsx", .name_repair = "unique")
df_dist <- vegdist(df[, -1], method = "bray")

# Statistics: PERMANOVA, Pairwise, and Dispersion
perm <- adonis2(df_dist ~ Site, data = df)
p_val <- perm$`Pr(>F)`[1]
pairwise_Site <- pairwise.adonis(df_dist, factors = df$Site)
dispersion_test <- betadisper(df_dist, df$Site)
disp_p <- anova(dispersion_test)$`Pr(>F)`[1]

# Save Statistics
write.csv(as.data.frame(perm), "~/Desktop/Analysis/1_PERMANOVA_Site.csv")
write.csv(pairwise_Site, "~/Desktop/Analysis/2_PERMANOVA_Pairwise_Site.csv")
write.csv(as.data.frame(anova(dispersion_test)), "~/Desktop/Analysis/3_Dispersion_Check_site.csv")

# Run PCoA & Prepare Plot Data
pcoa_res <- wcmdscale(df_dist, eig = TRUE)
eig_percent <- round(100 * pcoa_res$eig / sum(pcoa_res$eig), 1)
pcoa_scores <- as.data.frame(pcoa_res$points)
colnames(pcoa_scores) <- c("PCoA1", "PCoA2")
plot_data <- cbind(pcoa_scores, df)

# Calculate centroids for 'spiders'
centroids <- aggregate(cbind(PCoA1, PCoA2) ~ Site, data = plot_data, FUN = mean)
colnames(centroids) <- c("Site", "mPCoA1", "mPCoA2")
plot_data <- merge(plot_data, centroids, by = "Site")

# PART A: PCoA Plot
cb_palette <- c("#E69F00", "#009E73", "#0072B2")
#cb_palette <- c("#0057E9", "#87E911", "#F2CA19", "#FF00BD")
p1 <- ggplot(plot_data, aes(x = PCoA1, y = PCoA2, color = Site)) + 
  stat_ellipse(aes(fill = Site), geom = "polygon", alpha = 0.1, level = 0.95) + 
  geom_segment(aes(xend = mPCoA1, yend = mPCoA2), alpha = 0.2) + 
  geom_point(size = 3) + 
  geom_point(aes(x = mPCoA1, y = mPCoA2), size = 5, shape = 18) + 
  annotate("text", x = -Inf, y = Inf, label = paste0("PERMANOVA p = ", p_val), 
           family = "serif", fontface = "italic", size = 4, hjust = -0.1, vjust = 1.5) + 
  labs(title = "(a)", 
       x = paste0("PCoA1 (", eig_percent[1], "%)"), 
       y = paste0("PCoA2 (", eig_percent[2], "%)")) + 
  coord_fixed() + theme_bw() + 
  theme(text = element_text(family = "serif", size = 12, colour = "black"), 
        plot.title = element_text(hjust = 0.5), legend.position = "bottom", # Moved to bottom for better column fit
        legend.title = element_blank(), panel.grid = element_blank(), 
        axis.text = element_text(colour = "black")) + 
  scale_fill_manual(values = cb_palette) + scale_colour_manual(values = cb_palette)
p1 <- p1 + guides(color = guide_legend(override.aes = list(shape = 16, size = 4)))

# PART B: Bray-Curtis Heatmap (Greyscale Version)
Site_means <- aggregate(df[, -1], by = list(Site = df$Site), FUN = mean)
dist_matrix <- as.matrix(vegdist(Site_means[, -1], method = "bray"))
rownames(dist_matrix) <- colnames(dist_matrix) <- Site_means$Site

p2 <- ggplot(melt(dist_matrix), aes(Var1, Var2, fill = value)) + 
  geom_tile(color = "white", size = 0.5) + 
  # Using 'Greys' for a smooth, low-contrast, and professional look
  scale_fill_distiller(palette = "Greys", direction = 1, name = "Bray–Curtis") + 
  geom_text(aes(label = sprintf("%.2f", value)), 
            family = "serif", size = 4, 
            # This logic makes text white on dark cells and black on light cells
            color = ifelse(melt(dist_matrix)$value > 0.4, "white", "black")) + 
  labs(title = "(b)", x = NULL, y = NULL) + 
  theme_minimal() + 
  theme(text = element_text(family = "serif", size = 12, colour = "black"), 
        plot.title = element_text(hjust = 0.5), 
        panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))

# Combine and Display side-by-side
combined_final <- p1 + p2 + plot_layout(ncol = 2, widths = c(1.4, 1))
print(combined_final)


# SIMPER Analysis
simper_Site <- simper(df[, -1], group = df$Site, permutations = 999)
simper_summary <- summary(simper_Site)
extract_top5 <- function(comp) { 
  df_sim <- as.data.frame(simper_summary[[comp]]) 
  df_sim$Species <- rownames(df_sim) 
  df_sim$Comparison <- comp 
  head(select(df_sim, Comparison, Species, average, cumsum, p), 5) 
}
final_simper <- bind_rows(lapply(names(simper_summary), extract_top5))
write_csv(final_simper, "~/Desktop/Analysis/4_Simper_Site_Top5.csv")


##################
###Months
# Load Data
df <- read_excel("~/Desktop/Analysis/4_PCoA_month.xlsx", .name_repair = "unique")
df_dist <- vegdist(df[, -1], method = "bray")

# Statistics: PERMANOVA, Pairwise, and Dispersion
perm <- adonis2(df_dist ~ Month, data = df)
p_val <- perm$`Pr(>F)`[1]
pairwise_Month <- pairwise.adonis(df_dist, factors = df$Month)
dispersion_test <- betadisper(df_dist, df$Month)
disp_p <- anova(dispersion_test)$`Pr(>F)`[1]

# Save Statistics
write.csv(as.data.frame(perm), "~/Desktop/Analysis/1_PERMANOVA_Month.csv")
write.csv(pairwise_Month, "~/Desktop/Analysis/2_PERMANOVA_Pairwise_month.csv")
write.csv(as.data.frame(anova(dispersion_test)), "~/Desktop/Analysis/3_Dispersion_Check_month.csv")

# Run PCoA & Prepare Plot Data
pcoa_res <- wcmdscale(df_dist, eig = TRUE)
eig_percent <- round(100 * pcoa_res$eig / sum(pcoa_res$eig), 1)
pcoa_scores <- as.data.frame(pcoa_res$points)
colnames(pcoa_scores) <- c("PCoA1", "PCoA2")
plot_data <- cbind(pcoa_scores, df)

# Calculate centroids for 'spiders'
centroids <- aggregate(cbind(PCoA1, PCoA2) ~ Month, data = plot_data, FUN = mean)
colnames(centroids) <- c("Month", "mPCoA1", "mPCoA2")
plot_data <- merge(plot_data, centroids, by = "Month")

# PART A: PCoA Plot
cb_palette <- c("#0057E9", "#87E911", "#F2CA19", "#FF00BD")
p1 <- ggplot(plot_data, aes(x = PCoA1, y = PCoA2, color = Month)) +
  # Use this instead of stat_ellipse for small sample sizes (n=3)
  ggpubr::stat_chull(aes(fill = Month), geom = "polygon", alpha = 0.1) + 
  
  # Keep the rest exactly the same
  geom_segment(aes(xend = mPCoA1, yend = mPCoA2), alpha = 0.2) + 
  geom_point(size = 3, shape = 16) + 
  geom_point(aes(x = mPCoA1, y = mPCoA2), size = 5, shape = 18) + 
  
  annotate("text", x = -Inf, y = Inf, label = paste0("PERMANOVA p = ", p_val), 
           family = "serif", fontface = "italic", size = 4, hjust = -0.1, vjust = 1.5) + 
  labs(title = "(a)", 
       x = paste0("PCoA1 (", eig_percent[1], "%)"), 
       y = paste0("PCoA2 (", eig_percent[2], "%)")) + 
  coord_fixed() + theme_bw() + 
  theme(text = element_text(family = "serif", size = 12, colour = "black"), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid = element_blank(), 
        axis.text = element_text(colour = "black")) + 
  scale_fill_manual(values = cb_palette) + 
  scale_colour_manual(values = cb_palette)
p1 <- p1 + guides(color = guide_legend(override.aes = list(shape = 16, size = 4)))

# PART B: Bray-Curtis Heatmap (Greyscale Version)
Month_means <- aggregate(df[, -1], by = list(Month = df$Month), FUN = mean)
dist_matrix <- as.matrix(vegdist(Month_means[, -1], method = "bray"))
rownames(dist_matrix) <- colnames(dist_matrix) <- Month_means$Month

p2 <- ggplot(melt(dist_matrix), aes(Var1, Var2, fill = value)) + 
  geom_tile(color = "white", size = 0.5) + 
  # Using 'Greys' for a smooth, low-contrast, and professional look
  scale_fill_distiller(palette = "Greys", direction = 1, name = "Bray–Curtis") + 
  geom_text(aes(label = sprintf("%.2f", value)), 
            family = "serif", size = 4, 
            # This logic makes text white on dark cells and black on light cells
            color = ifelse(melt(dist_matrix)$value > 0.4, "white", "black")) + 
  labs(title = "(b)", x = NULL, y = NULL) + 
  theme_minimal() + 
  theme(text = element_text(family = "serif", size = 12, colour = "black"), 
        plot.title = element_text(hjust = 0.5), 
        panel.grid = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))

# Combine and Display side-by-side
combined_final <- p1 + p2 + plot_layout(ncol = 2, widths = c(1.4, 1))
print(combined_final)

# SIMPER Analysis
simper_Month <- simper(df[, -1], group = df$Month, permutations = 999)
simper_summary <- summary(simper_Month)
extract_top5 <- function(comp) { 
  df_sim <- as.data.frame(simper_summary[[comp]]) 
  df_sim$Species <- rownames(df_sim) 
  df_sim$Comparison <- comp 
  head(select(df_sim, Comparison, Species, average, cumsum, p), 5) 
}
final_simper <- bind_rows(lapply(names(simper_summary), extract_top5))
write_csv(final_simper, "~/Desktop/Analysis/4_Simper_Month_Top5_month.csv")
