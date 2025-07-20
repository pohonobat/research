# =============================================================================
# PHASE 1: DATA VALIDATION & STANDARDIZATION (FIXED VERSION)
# Medicinal Tree Species Analysis - Pteropus vampyrus Habitat
# =============================================================================
# 
# Study: Populations of Medicinal Tree Species in Habitat of Pteropus vampyrus
# Location: Situ Lengkong Panjalu Nature Reserve, West Java, Indonesia
# Date: January 2025
#
# This script performs comprehensive data validavgw45454-3255rxtion and standardization
# FIXED VERSION - handles actual Excel column names
# =============================================================================

# Clear environment and set up
rm(list = ls())
cat("=============================================================================\n")
cat("PHASE 1: DATA VALIDATION & STANDARDIZATION (FIXED VERSION)\n")
cat("Medicinal Tree Species Analysis - Pteropus vampyrus Habitat\n")
cat("=============================================================================\n\n")

# Load required libraries
required_packages <- c("readxl", "dplyr", "stringr", "ggplot2", "knitr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

cat("✅ Required packages loaded successfully\n\n")

# =============================================================================
# STEP 1.1: DATA IMPORT AND STRUCTURE EXAMINATION
# =============================================================================

cat("STEP 1.1: DATA IMPORT AND STRUCTURE EXAMINATION\n")
cat(strrep("-", 50), "\n")

# Import data from Excel file
excel_file <- "analisis pohon 1.xlsx"

if (!file.exists(excel_file)) {
  cat("❌ ERROR: Excel file not found.\n")
  cat("Please ensure 'analisis pohon 1.xlsx' is in working directory\n")
  cat("Current working directory:", getwd(), "\n")
  
  # List files in current directory
  cat("\nFiles in current directory:\n")
  files <- list.files(pattern = "\\.(xlsx|xls|csv)$")
  if (length(files) > 0) {
    for (f in files) cat("  -", f, "\n")
  } else {
    cat("  No Excel or CSV files found\n")
  }
  stop("File not found")
}

# Read both sheets with error handling
tryCatch({
  raw_data <- read_excel(excel_file, sheet = 1)  # Use sheet number instead of name
  cat("✅ Sheet 1 imported successfully\n")
}, error = function(e) {
  cat("❌ Error reading Sheet 1:", e$message, "\n")
  stop("Cannot read Excel file")
})

tryCatch({
  species_analysis <- read_excel(excel_file, sheet = 2)  # Use sheet number instead of name  
  cat("✅ Sheet 2 imported successfully\n")
}, error = function(e) {
  cat("⚠️ Warning: Could not read Sheet 2:", e$message, "\n")
  species_analysis <- NULL
})

# EXAMINE ACTUAL COLUMN NAMES
cat("\n🔍 EXAMINING ACTUAL DATA STRUCTURE:\n")
cat("Sheet 1 dimensions:", nrow(raw_data), "rows,", ncol(raw_data), "columns\n")
cat("Sheet 1 column names:\n")
for (i in 1:ncol(raw_data)) {
  col_name <- names(raw_data)[i]
  if (is.na(col_name) || col_name == "") {
    col_name <- paste0("Column_", i, "_UNNAMED")
  }
  cat("  ", i, ":", col_name, "\n")
}

if (!is.null(species_analysis)) {
  cat("\nSheet 2 dimensions:", nrow(species_analysis), "rows,", ncol(species_analysis), "columns\n")
  cat("Sheet 2 column names:\n")
  for (i in 1:ncol(species_analysis)) {
    col_name <- names(species_analysis)[i]
    if (is.na(col_name) || col_name == "") {
      col_name <- paste0("Column_", i, "_UNNAMED")
    }
    cat("  ", i, ":", col_name, "\n")
  }
}

# =============================================================================
# STEP 1.2: SMART COLUMN NAME HANDLING
# =============================================================================

cat("\nSTEP 1.2: SMART COLUMN NAME HANDLING\n")
cat(strrep("-", 50), "\n")

# Fix any NA or empty column names
original_names <- names(raw_data)
fixed_names <- original_names

for (i in 1:length(fixed_names)) {
  if (is.na(fixed_names[i]) || fixed_names[i] == "" || fixed_names[i] == "...1") {
    fixed_names[i] <- paste0("Column_", i)
  }
}

names(raw_data) <- fixed_names
cat("✅ Column names standardized\n")

# Display first few rows to understand data structure
cat("\n📋 FIRST FEW ROWS OF DATA:\n")
print(head(raw_data, 3))

# Try to identify key columns based on content patterns
cat("\n🔍 IDENTIFYING KEY COLUMNS:\n")

# Look for species name column (likely contains plant names)
species_col <- NULL
for (i in 1:ncol(raw_data)) {
  col_data <- raw_data[[i]]
  if (is.character(col_data)) {
    # Check if column contains plant-like names
    plant_indicators <- sum(grepl("Ki |Huru|Pisitan|Campaka|Teureup", col_data, ignore.case = TRUE), na.rm = TRUE)
    if (plant_indicators > 0) {
      species_col <- i
      cat("  Species column found at position", i, ":", names(raw_data)[i], "\n")
      break
    }
  }
}

# Look for numeric columns (DBH, Height, etc.)
numeric_cols <- sapply(raw_data, is.numeric)
cat("  Numeric columns found at positions:", which(numeric_cols), "\n")

# =============================================================================
# STEP 1.3: FLEXIBLE DATA CLEANING
# =============================================================================

cat("\nSTEP 1.3: FLEXIBLE DATA CLEANING\n")
cat(strrep("-", 50), "\n")

# Create a working copy
raw_data_work <- raw_data

# If we found the species column, use it for filtering
if (!is.null(species_col)) {
  species_col_name <- names(raw_data)[species_col]
  
  # Clean the data by removing rows without species names
  raw_data_clean <- raw_data_work %>%
    filter(!is.na(.data[[species_col_name]]) & 
           .data[[species_col_name]] != "" & 
           .data[[species_col_name]] != "NA")
  
  cat("✅ Data cleaned using species column:", species_col_name, "\n")
} else {
  # Fallback: remove completely empty rows
  raw_data_clean <- raw_data_work %>%
    filter(rowSums(is.na(.)) < ncol(.))
  
  cat("⚠️ Species column not clearly identified, removed empty rows\n")
}

cat("   Original rows:", nrow(raw_data), "\n")
cat("   Clean rows:", nrow(raw_data_clean), "\n")
cat("   Rows removed:", nrow(raw_data) - nrow(raw_data_clean), "\n")

# =============================================================================
# STEP 1.4: EXTRACT SPECIES INFORMATION
# =============================================================================

cat("\nSTEP 1.4: EXTRACT SPECIES INFORMATION\n")
cat(strrep("-", 50), "\n")

# Extract species information from the cleaned data
if (!is.null(species_col)) {
  species_col_name <- names(raw_data_clean)[species_col]
  
  # Count species occurrences
  species_counts <- raw_data_clean %>%
    count(.data[[species_col_name]], name = "Population") %>%
    arrange(desc(Population)) %>%
    filter(Population > 0)
  
  names(species_counts)[1] <- "Species_Local"
  
  cat("🌳 SPECIES FOUND IN DATASET:\n")
  for (i in 1:nrow(species_counts)) {
    cat(sprintf("   %2d. %-20s: %3d individuals\n", 
                i, species_counts$Species_Local[i], species_counts$Population[i]))
  }
  
} else {
  cat("❌ Cannot extract species information - species column not identified\n")
  cat("Please check your data structure\n")
  
  # Show sample of data for manual inspection
  cat("\n📋 SAMPLE DATA FOR INSPECTION:\n")
  print(head(raw_data_clean))
  stop("Manual intervention required")
}

# =============================================================================
# STEP 1.5: CREATE STANDARDIZED SPECIES DATABASE
# =============================================================================

cat("\nSTEP 1.5: CREATE STANDARDIZED SPECIES DATABASE\n")
cat(strrep("-", 50), "\n")

# Create species mapping based on the research paper
species_mapping <- data.frame(
  Local_Name_Pattern = c("Ki Haji", "Ki Sapi", "Pisitan Monyet", "ki Terong", 
                        "Hantap Helang", "Huru Kembang", "Ki Besi", "Campaka", 
                        "Ki Bima", "Teureup"),
  Scientific_Name = c("Dysoxylum densiflorum", "Gordonia excelsa", 
                     "Dysoxylum parasiticum", "Endiandra rubescens",
                     "Sterculia coccinea", "Actinodaphne sp.", 
                     "Rhodamnia cinerea", "Magnolia champaca",
                     "Podocarpus blumei", "Artocarpus elasticus"),
  Family = c("Meliaceae", "Theaceae", "Meliaceae", "Lauraceae",
            "Sterculiaceae", "Lauraceae", "Myrtaceae", "Magnoliaceae",
            "Podocarpaceae", "Moraceae"),
  stringsAsFactors = FALSE
)

# Match found species with standardized names
species_standardized <- species_mapping %>%
  left_join(species_counts, by = c("Local_Name_Pattern" = "Species_Local")) %>%
  mutate(
    Population = ifelse(is.na(Population), 0, Population),
    Conservation_Status = case_when(
      Population == 0 ~ "NOT_FOUND",
      Population < 5 ~ "CRITICALLY_ENDANGERED",
      Population < 10 ~ "ENDANGERED",
      Population < 15 ~ "VULNERABLE", 
      Population < 30 ~ "NEAR_THREATENED",
      TRUE ~ "LEAST_CONCERN"
    ),
    Sustainable_Harvest = case_when(
      Population == 0 ~ "NOT_APPLICABLE",
      Population >= 30 ~ "YES - Sustainable harvest possible",
      Population >= 15 ~ "LIMITED - Careful management required",
      Population >= 5 ~ "NO - Conservation only",
      TRUE ~ "NO - Urgent protection needed"
    )
  ) %>%
  arrange(desc(Population))

cat("📋 STANDARDIZED SPECIES DATABASE:\n")
cat(sprintf("%-18s | %-25s | %-15s | %3s | %-20s\n", 
           "Local Name", "Scientific Name", "Family", "Pop", "Status"))
cat(strrep("-", 95), "\n")

for (i in 1:nrow(species_standardized)) {
  cat(sprintf("%-18s | %-25s | %-15s | %3d | %-20s\n",
             species_standardized$Local_Name_Pattern[i],
             species_standardized$Scientific_Name[i], 
             species_standardized$Family[i],
             species_standardized$Population[i],
             species_standardized$Conservation_Status[i]))
}

# =============================================================================
# STEP 1.6: FAMILY ANALYSIS
# =============================================================================

cat("\nSTEP 1.6: FAMILY ANALYSIS\n")
cat(strrep("-", 50), "\n")

family_analysis <- species_standardized %>%
  filter(Population > 0) %>%
  group_by(Family) %>%
  summarise(
    Species_Count = n(),
    Total_Individuals = sum(Population),
    .groups = 'drop'
  ) %>%
  mutate(
    Percentage = round(Total_Individuals / sum(Total_Individuals) * 100, 1)
  ) %>%
  arrange(desc(Total_Individuals))

cat("🌳 FAMILY DOMINANCE ANALYSIS:\n")
cat(sprintf("%-15s | %7s | %11s | %10s\n", "Family", "Species", "Individuals", "Percentage"))
cat(strrep("-", 50), "\n")

for (i in 1:nrow(family_analysis)) {
  cat(sprintf("%-15s | %7d | %11d | %9.1f%%\n",
             family_analysis$Family[i],
             family_analysis$Species_Count[i],
             family_analysis$Total_Individuals[i],
             family_analysis$Percentage[i]))
}

# =============================================================================
# STEP 1.7: DATA VALIDATION SUMMARY
# =============================================================================

cat("\nSTEP 1.7: DATA VALIDATION SUMMARY\n")
cat(strrep("-", 50), "\n")

total_found <- sum(species_standardized$Population)
species_found <- sum(species_standardized$Population > 0)

validation_summary <- list(
  total_individuals = total_found,
  total_species_found = species_found,
  total_species_expected = nrow(species_standardized),
  data_completeness = round(species_found / nrow(species_standardized) * 100, 1)
)

cat("🔍 VALIDATION RESULTS:\n")
cat("   Total individuals found:", validation_summary$total_individuals, "\n")
cat("   Species found:", validation_summary$total_species_found, "out of", validation_summary$total_species_expected, "expected\n")
cat("   Species detection rate:", validation_summary$data_completeness, "%\n")

# Calculate quality score
if (validation_summary$total_individuals >= 140) {
  quality_score <- "EXCELLENT"
} else if (validation_summary$total_individuals >= 100) {
  quality_score <- "GOOD"
} else if (validation_summary$total_individuals >= 50) {
  quality_score <- "FAIR"
} else {
  quality_score <- "POOR"
}

cat("   Overall data quality:", quality_score, "\n")

# =============================================================================
# STEP 1.8: CREATE OUTPUTS
# =============================================================================

cat("\nSTEP 1.8: CREATE OUTPUTS\n")
cat(strrep("-", 50), "\n")

# Create output directory
if (!dir.exists("output")) {
  dir.create("output")
  cat("📁 Created output directory\n")
}

# Export data files
write.csv(raw_data_clean, "output/tree_data_cleaned.csv", row.names = FALSE)
write.csv(species_standardized, "output/species_standardized.csv", row.names = FALSE)
write.csv(family_analysis, "output/family_analysis.csv", row.names = FALSE)

# Create summary report
summary_report <- data.frame(
  Metric = c("Total Individuals", "Species Found", "Species Expected", 
            "Detection Rate", "Data Quality", "Dominant Family"),
  Value = c(validation_summary$total_individuals,
           validation_summary$total_species_found,
           validation_summary$total_species_expected,
           paste0(validation_summary$data_completeness, "%"),
           quality_score,
           family_analysis$Family[1])
)

write.csv(summary_report, "output/phase1_summary_report.csv", row.names = FALSE)

cat("💾 Files exported:\n")
cat("   - tree_data_cleaned.csv\n")
cat("   - species_standardized.csv\n") 
cat("   - family_analysis.csv\n")
cat("   - phase1_summary_report.csv\n")

# =============================================================================
# STEP 1.9: BASIC VISUALIZATION
# =============================================================================

if (require(ggplot2, quietly = TRUE)) {
  cat("\nSTEP 1.9: BASIC VISUALIZATION\n")
  cat(strrep("-", 50), "\n")
  
  # Filter species that were actually found
  species_for_plot <- species_standardized %>%
    filter(Population > 0)
  
  if (nrow(species_for_plot) > 0) {
    # Species population chart
    p1 <- ggplot(species_for_plot, aes(x = reorder(Local_Name_Pattern, Population), y = Population)) +
      geom_col(aes(fill = Conservation_Status), alpha = 0.8) +
      coord_flip() +
      scale_fill_manual(values = c("CRITICALLY_ENDANGERED" = "#d32f2f",
                                  "ENDANGERED" = "#f57c00", 
                                  "VULNERABLE" = "#fbc02d",
                                  "NEAR_THREATENED" = "#689f38",
                                  "LEAST_CONCERN" = "#388e3c")) +
      labs(title = "Medicinal Tree Species Population in Pteropus vampyrus Habitat",
           x = "Species (Local Name)", 
           y = "Number of Individuals",
           fill = "Conservation Status") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 12, hjust = 0.5))
    
    ggsave("output/species_population_chart.png", p1, width = 10, height = 6, dpi = 300)
    
    # Family composition chart
    p2 <- ggplot(family_analysis, aes(x = "", y = Total_Individuals, fill = Family)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "Family Composition of Medicinal Trees",
           fill = "Family") +
      theme_void() +
      theme(plot.title = element_text(size = 12, hjust = 0.5)) +
      geom_text(aes(label = paste0(Family, "\n", Percentage, "%")), 
                position = position_stack(vjust = 0.5), size = 3)
    
    ggsave("output/family_composition_chart.png", p2, width = 8, height = 8, dpi = 300)
    
    cat("📊 Visualizations created:\n")
    cat("   - species_population_chart.png\n")
    cat("   - family_composition_chart.png\n")
  }
}

# =============================================================================
# PHASE 1 COMPLETION
# =============================================================================

cat("\n", strrep("=", 75), "\n")
cat("✅ PHASE 1 COMPLETED SUCCESSFULLY\n")
cat(strrep("=", 75), "\n")

cat("\n📊 FINAL SUMMARY:\n")
cat("   • Total individuals analyzed:", validation_summary$total_individuals, "\n")
cat("   • Species successfully identified:", validation_summary$total_species_found, "out of 10 expected\n")
cat("   • Data quality assessment:", quality_score, "\n")
cat("   • Most abundant family:", family_analysis$Family[1], "(", family_analysis$Percentage[1], "%)\n")

urgent_conservation <- sum(species_standardized$Conservation_Status %in% 
                          c("CRITICALLY_ENDANGERED", "ENDANGERED"), na.rm = TRUE)
cat("   • Species requiring urgent conservation:", urgent_conservation, "\n")

cat("\n📁 OUTPUT FILES READY FOR PHASE 2:\n")
cat("   • All data files exported to 'output/' directory\n")
cat("   • Visualizations created\n")
cat("   • Species nomenclature standardized\n")

cat("\n🎯 PHASE 1 STATUS: READY FOR PHASE 2 - ECOLOGICAL ANALYSIS\n")

cat("\n", strrep("=", 75), "\n")
cat("Phase 1 Analysis Complete - ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 75), "\n")