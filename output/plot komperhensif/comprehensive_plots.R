# ===============================================================================
# COMPREHENSIVE PLOTS FOR MEDICINAL PLANTS CONSERVATION RESEARCH
# Pteropus vampyrus Habitat - Situ Lengkong Panjalu Nature Reserve
# ===============================================================================

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(RColorBrewer)
library(gridExtra)
library(corrplot)
library(ggpubr)
library(scales)
library(cowplot)

# Set theme for all plots
theme_set(theme_minimal() + 
  theme(
    text = element_text(family = "Arial", size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ))

# ===============================================================================
# DATA PREPARATION
# ===============================================================================

# Species data
species_data <- data.frame(
  Species = c("Dysoxylum densiflorum", "Gordonia excelsa", "Dysoxylum parasiticum",
              "Sterculia coccinea", "Rhodamnia cinerea", "Endiandra rubescens",
              "Actinodaphne sp.", "Podocarpus blumei", "Magnolia champaca", 
              "Artocarpus elasticus"),
  Local_Name = c("Ki Haji", "Ki Sapi", "Pisitan Monyet", "Hantap Helang",
                 "Ki Besi", "Ki Terong", "Huru Kembang", "Ki Bima",
                 "Campaka", "Teureup"),
  Family = c("Meliaceae", "Theaceae", "Meliaceae", "Sterculiaceae",
             "Myrtaceae", "Lauraceae", "Lauraceae", "Podocarpaceae",
             "Magnoliaceae", "Moraceae"),
  Individuals = c(65, 35, 12, 8, 5, 8, 7, 3, 4, 2),
  DBH_mean = c(24.5, 28.3, 18.7, 22.1, 16.8, 19.4, 17.2, 31.5, 26.8, 35.2),
  IVI = c(116.56, 64.72, 22.85, 17.32, 16.23, 16.19, 15.46, 12.83, 9.91, 7.92),
  Conservation_Status = c("LC", "LC", "VU", "EN", "EN", "EN", "EN", "CR", "CR", "CR"),
  Priority_Score = c(0.640, 0.527, 0.607, 0.568, 0.496, 0.495, 0.494, 0.567, 0.630, 0.660),
  Bioactive_Compounds = c(15, 8, 12, 6, 4, 5, 3, 7, 18, 23),
  Economic_Value_Million = c(1156, 623, 267, 178, 89, 156, 134, 268, 358, 179),
  Threat_Level = c("Low", "Low", "High", "Medium", "Medium", "Medium", "Medium", "High", "Extreme", "Extreme"),
  Viability_Status = c("Viable", "Approaching MVP", "Critically Low", "Critically Low", 
                      "Non-viable", "Non-viable", "Non-viable", "Non-viable", "Non-viable", "Non-viable"),
  Use_Reports = c(13, 6, 8, 4, 3, 5, 2, 4, 9, 11),
  Frequency_Percent = c(50, 25, 8.33, 8.33, 8.33, 8.33, 8.33, 8.33, 8.33, 8.33)
)

# Family composition data
family_data <- data.frame(
  Family = c("Meliaceae", "Theaceae", "Lauraceae", "Sterculiaceae", 
             "Myrtaceae", "Magnoliaceae", "Podocarpaceae", "Moraceae"),
  Individuals = c(77, 35, 15, 8, 5, 4, 3, 2),
  Percentage = c(51.7, 23.5, 10.1, 5.4, 3.4, 2.7, 2.0, 1.3)
)

# Diameter class distribution
diameter_data <- data.frame(
  Class = c("10-20", "20-30", "30-40", "40-50", ">50"),
  Individuals = c(89, 35, 18, 5, 2),
  Percentage = c(59.7, 23.5, 12.1, 3.4, 1.3)
)

# Conservation status distribution
conservation_data <- data.frame(
  Status = c("Critically Endangered", "Endangered", "Vulnerable", "Least Concern"),
  Count = c(3, 4, 1, 2),
  Percentage = c(30, 40, 10, 20)
)

# Bioactive compounds by chemical class
compounds_data <- data.frame(
  Class = c("Alkaloids", "Flavonoids", "Terpenoids", "Steroids", "Phenols", "Saponins", "Others"),
  Count = c(34, 28, 25, 19, 18, 15, 17),
  Percentage = c(21.8, 17.9, 16.0, 12.2, 11.5, 9.6, 10.9)
)

# Ethnobotanical use categories
ethnobotany_data <- data.frame(
  Category = c("Digestive disorders", "Skin diseases", "Respiratory disorders", 
               "Fever", "Cancer/Tumors", "Wounds", "Malaria", "Inflammation",
               "Diabetes", "Hypertension", "Kidney disorders", "Others"),
  Use_Reports = c(23, 18, 15, 12, 11, 9, 8, 7, 6, 5, 4, 12),
  ICF = c(0.87, 0.82, 0.79, 0.74, 0.71, 0.68, 0.65, 0.62, 0.58, 0.55, 0.52, 0.45)
)

# ===============================================================================
# PLOT 1: SPECIES COMPOSITION AND DOMINANCE
# ===============================================================================

# A. Family composition pie chart
p1a <- ggplot(family_data, aes(x = "", y = Percentage, fill = Family)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 0.5) +
  coord_polar("y", start = 0) +
  scale_fill_viridis_d(option = "plasma", name = "Family") +
  labs(title = "Family Composition",
       subtitle = "Distribution of individuals by family") +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "right",
    legend.text = element_text(size = 8)
  ) +
  geom_text(aes(label = paste0(Family, "\n", Percentage, "%")), 
            position = position_stack(vjust = 0.5), size = 2.5, color = "white", fontface = "bold")

# B. Species abundance bar plot
species_data$Species_Short <- c("D. densiflorum", "G. excelsa", "D. parasiticum",
                               "S. coccinea", "R. cinerea", "E. rubescens",
                               "Actinodaphne sp.", "P. blumei", "M. champaca", "A. elasticus")

p1b <- ggplot(species_data, aes(x = reorder(Species_Short, Individuals), y = Individuals, 
                               fill = Conservation_Status)) +
  geom_col(color = "black", size = 0.3) +
  scale_fill_manual(values = c("CR" = "#d73027", "EN" = "#fc8d59", "VU" = "#fee08b", "LC" = "#91cf60"),
                    name = "Conservation\nStatus",
                    labels = c("CR" = "Critically Endangered", "EN" = "Endangered", 
                              "VU" = "Vulnerable", "LC" = "Least Concern")) +
  labs(title = "Species Abundance and Conservation Status",
       subtitle = "Number of individuals per species",
       x = "Species", y = "Number of Individuals") +
  coord_flip() +
  theme(axis.text.y = element_text(face = "italic", size = 9))

# ===============================================================================
# PLOT 2: CONSERVATION PRIORITY AND THREAT ANALYSIS
# ===============================================================================

# A. Priority score vs population size
p2a <- ggplot(species_data, aes(x = Individuals, y = Priority_Score, 
                               color = Conservation_Status, size = Bioactive_Compounds)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = c("CR" = "#d73027", "EN" = "#fc8d59", "VU" = "#fee08b", "LC" = "#91cf60"),
                     name = "Conservation\nStatus") +
  scale_size_continuous(range = c(3, 8), name = "Bioactive\nCompounds") +
  labs(title = "Conservation Priority Analysis",
       subtitle = "Priority score vs population size",
       x = "Population Size (individuals)", y = "Priority Score") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", alpha = 0.3) +
  ggrepel::geom_text_repel(aes(label = Species_Short), size = 3, 
                          box.padding = 0.5, point.padding = 0.3)

# B. Threat level distribution
threat_summary <- species_data %>%
  count(Threat_Level) %>%
  mutate(Percentage = n/sum(n)*100)

p2b <- ggplot(threat_summary, aes(x = factor(Threat_Level, levels = c("Low", "Medium", "High", "Extreme")), 
                                 y = n, fill = Threat_Level)) +
  geom_col(color = "black", size = 0.3) +
  scale_fill_manual(values = c("Low" = "#91cf60", "Medium" = "#fee08b", 
                              "High" = "#fc8d59", "Extreme" = "#d73027"),
                    name = "Threat Level") +
  labs(title = "Threat Level Distribution",
       subtitle = "Number of species by threat category",
       x = "Threat Level", y = "Number of Species") +
  geom_text(aes(label = paste0(n, " species\n(", round(Percentage, 1), "%)")), 
            vjust = -0.5, size = 3, fontface = "bold")

# ===============================================================================
# PLOT 3: PHARMACOLOGICAL AND ETHNOBOTANICAL ANALYSIS
# ===============================================================================

# A. Bioactive compounds by species
p3a <- ggplot(species_data, aes(x = reorder(Species_Short, Bioactive_Compounds), 
                               y = Bioactive_Compounds, fill = Priority_Score)) +
  geom_col(color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "plasma", name = "Priority\nScore") +
  labs(title = "Bioactive Compounds by Species",
       subtitle = "Number of identified bioactive compounds",
       x = "Species", y = "Number of Bioactive Compounds") +
  coord_flip() +
  theme(axis.text.y = element_text(face = "italic", size = 9))

# B. Chemical compound classes
p3b <- ggplot(compounds_data, aes(x = reorder(Class, Count), y = Count, fill = Class)) +
  geom_col(color = "black", size = 0.3) +
  scale_fill_viridis_d(option = "viridis", name = "Chemical Class") +
  labs(title = "Distribution of Bioactive Compounds",
       subtitle = "By chemical class",
       x = "Chemical Class", y = "Number of Compounds") +
  coord_flip() +
  geom_text(aes(label = paste0(Count, " (", Percentage, "%)")), 
            hjust = -0.1, size = 3, fontface = "bold")

# ===============================================================================
# PLOT 4: ECONOMIC VALUATION AND VIABILITY
# ===============================================================================

# A. Economic value vs conservation investment
species_data$BCR <- c(3.42, 2.15, 2.78, 1.89, 1.45, 1.67, 1.52, 2.23, 2.67, 2.89)

p4a <- ggplot(species_data, aes(x = Economic_Value_Million, y = BCR, 
                               color = Viability_Status, size = Individuals)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = c("Viable" = "#2166ac", "Approaching MVP" = "#5aae61", 
                               "Critically Low" = "#fee08b", "Non-viable" = "#d73027"),
                     name = "Viability\nStatus") +
  scale_size_continuous(range = c(3, 8), name = "Population\nSize") +
  labs(title = "Economic Viability Analysis",
       subtitle = "Economic value vs benefit-cost ratio",
       x = "Economic Value (Million Rupiah)", y = "Benefit-Cost Ratio") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
  ggrepel::geom_text_repel(aes(label = Species_Short), size = 3)

# B. Population viability status
viability_summary <- species_data %>%
  count(Viability_Status) %>%
  mutate(Percentage = n/sum(n)*100)

p4b <- ggplot(viability_summary, aes(x = factor(Viability_Status, 
                                               levels = c("Viable", "Approaching MVP", "Critically Low", "Non-viable")), 
                                    y = n, fill = Viability_Status)) +
  geom_col(color = "black", size = 0.3) +
  scale_fill_manual(values = c("Viable" = "#2166ac", "Approaching MVP" = "#5aae61", 
                              "Critically Low" = "#fee08b", "Non-viable" = "#d73027"),
                    name = "Viability Status") +
  labs(title = "Population Viability Assessment",
       subtitle = "Distribution of species by viability status",
       x = "Viability Status", y = "Number of Species") +
  geom_text(aes(label = paste0(n, " species\n(", round(Percentage, 1), "%)")), 
            vjust = -0.5, size = 3, fontface = "bold") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ===============================================================================
# PLOT 5: CORRELATION MATRIX AND ECOLOGICAL RELATIONSHIPS
# ===============================================================================

# Prepare correlation data
cor_data <- species_data %>%
  select(Individuals, IVI, Priority_Score, Bioactive_Compounds, 
         Economic_Value_Million, Use_Reports, DBH_mean) %>%
  rename(
    "Population Size" = Individuals,
    "Importance Value" = IVI,
    "Priority Score" = Priority_Score,
    "Bioactive Compounds" = Bioactive_Compounds,
    "Economic Value" = Economic_Value_Million,
    "Traditional Uses" = Use_Reports,
    "Mean DBH" = DBH_mean
  )

# Calculate correlation matrix
cor_matrix <- cor(cor_data, use = "complete.obs")

# Create correlation plot
p5 <- corrplot(cor_matrix, method = "color", type = "upper", 
               order = "hclust", tl.cex = 0.8, tl.col = "black",
               col = colorRampPalette(c("#d73027", "white", "#2166ac"))(100),
               addCoef.col = "black", number.cex = 0.7,
               title = "Correlation Matrix of Ecological and Conservation Variables",
               mar = c(0,0,2,0))

# ===============================================================================
# PLOT 6: ETHNOBOTANICAL USE PATTERNS
# ===============================================================================

# A. Use categories and consensus
p6a <- ggplot(ethnobotany_data, aes(x = reorder(Category, Use_Reports), y = Use_Reports, fill = ICF)) +
  geom_col(color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "plasma", name = "Informant\nConsensus\nFactor") +
  labs(title = "Traditional Use Categories",
       subtitle = "Use reports and informant consensus",
       x = "Use Category", y = "Number of Use Reports") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 9))

# B. Species vs traditional uses
p6b <- ggplot(species_data, aes(x = reorder(Species_Short, Use_Reports), y = Use_Reports, 
                               fill = Bioactive_Compounds)) +
  geom_col(color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "viridis", name = "Bioactive\nCompounds") +
  labs(title = "Traditional Use Reports by Species",
       subtitle = "Correlation with bioactive compounds",
       x = "Species", y = "Number of Use Reports") +
  coord_flip() +
  theme(axis.text.y = element_text(face = "italic", size = 9))

# ===============================================================================
# COMBINE PLOTS INTO COMPREHENSIVE FIGURE
# ===============================================================================

# Create multi-panel figure
combined_plot <- plot_grid(
  plot_grid(p1a, p1b, ncol = 2, labels = c("A", "B"), rel_widths = c(1, 1.2)),
  plot_grid(p2a, p2b, ncol = 2, labels = c("C", "D")),
  plot_grid(p3a, p3b, ncol = 2, labels = c("E", "F")),
  plot_grid(p4a, p4b, ncol = 2, labels = c("G", "H")),
  plot_grid(p6a, p6b, ncol = 2, labels = c("I", "J")),
  ncol = 1, rel_heights = c(1, 1, 1, 1, 1)
)

# Save the comprehensive plot
ggsave("comprehensive_medicinal_plants_analysis.png", combined_plot, 
       width = 16, height = 20, dpi = 300, bg = "white")

# Save individual plots
ggsave("family_composition.png", p1a, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("species_abundance.png", p1b, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("priority_analysis.png", p2a, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("threat_distribution.png", p2b, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("bioactive_compounds.png", p3a, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("chemical_classes.png", p3b, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("economic_analysis.png", p4a, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("viability_status.png", p4b, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("ethnobotany_categories.png", p6a, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("species_traditional_uses.png", p6b, width = 10, height = 6, dpi = 300, bg = "white")

# Save correlation plot separately
png("correlation_matrix.png", width = 10, height = 8, units = "in", res = 300)
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 1.2, tl.col = "black",
         col = colorRampPalette(c("#d73027", "white", "#2166ac"))(100),
         addCoef.col = "black", number.cex = 1,
         title = "Correlation Matrix of Ecological and Conservation Variables",
         mar = c(0,0,3,0))
dev.off()

# Print summary statistics
cat("=== COMPREHENSIVE ANALYSIS SUMMARY ===\n")
cat("Total species analyzed:", nrow(species_data), "\n")
cat("Total individuals recorded:", sum(species_data$Individuals), "\n")
cat("Species requiring conservation attention:", sum(species_data$Conservation_Status != "LC"), "\n")
cat("Average priority score:", round(mean(species_data$Priority_Score), 3), "\n")
cat("Total bioactive compounds identified:", sum(species_data$Bioactive_Compounds), "\n")
cat("Total economic value (Million Rupiah):", sum(species_data$Economic_Value_Million), "\n")
cat("Average benefit-cost ratio:", round(mean(species_data$BCR), 2), "\n")

cat("\n=== PLOTS GENERATED ===\n")
cat("1. comprehensive_medicinal_plants_analysis.png - Main multi-panel figure\n")
cat("2. Individual component plots (10 files)\n")
cat("3. correlation_matrix.png - Correlation analysis\n")
cat("\nAll plots saved in high resolution (300 DPI) for publication.\n")
