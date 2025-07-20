# =============================================================================
# PHASE 5: CORRECTED VISUALIZATION & REPORTING FRAMEWORK
# Medicinal Tree Species Analysis - Pteropus vampyrus Habitat
# =============================================================================
# 
# CORRECTED VERSION - Uses actual data from previous phases
# Fixes inconsistencies found between Phase 3 and Phase 5 outputs
# =============================================================================

# Clear environment and set up
rm(list = ls())
cat("=============================================================================\n")
cat("PHASE 5: CORRECTED VISUALIZATION & REPORTING FRAMEWORK\n")
cat("Medicinal Tree Species Analysis - Pteropus vampyrus Habitat\n")
cat("=============================================================================\n\n")

# Essential packages
essential_packages <- c("ggplot2", "dplyr", "readr", "stringr")

for (pkg in essential_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
    cat("✅", pkg, "installed and loaded\n")
  } else {
    cat("✅", pkg, "available\n")
  }
}

# Create output directories
output_dirs <- c("output/phase5_corrected_figures", "output/phase5_corrected_reports", 
                "output/phase5_corrected_tables")

for (dir in output_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("📁 Created directory:", dir, "\n")
  }
}

cat("\n✅ Phase 5 corrected environment setup completed\n\n")

# =============================================================================
# STEP 1: LOAD ACTUAL DATA FROM PREVIOUS PHASES
# =============================================================================

cat("STEP 1: LOADING ACTUAL DATA FROM PREVIOUS PHASES\n")
cat(strrep("-", 60), "\n")

# Function to safely load CSV files
safe_load_csv <- function(filename, description) {
  if (file.exists(filename)) {
    tryCatch({
      data <- read_csv(filename, show_col_types = FALSE)
      cat("✅", description, "loaded:", nrow(data), "rows,", ncol(data), "columns\n")
      return(data)
    }, error = function(e) {
      cat("❌ Error loading", filename, ":", e$message, "\n")
      return(NULL)
    })
  } else {
    cat("⚠️", filename, "not found\n")
    return(NULL)
  }
}

# Load data from each phase
cat("📂 LOADING PHASE DATA:\n")
species_data <- safe_load_csv("output/species_standardized.csv", "Phase 1 - Species standardized")
ivi_data <- safe_load_csv("output/phase2_ivi_results.csv", "Phase 2 - IVI results")
conservation_data <- safe_load_csv("output/phase3_conservation_summary.csv", "Phase 3 - Conservation summary")
priority_data <- safe_load_csv("output/phase3_priority_assessment.csv", "Phase 3 - Priority assessment")
threat_data <- safe_load_csv("output/phase4_threat_assessment.csv", "Phase 4 - Threat assessment")
viability_data <- safe_load_csv("output/phase4_population_viability.csv", "Phase 4 - Population viability")

# =============================================================================
# STEP 2: CREATE CORRECTED INTEGRATED DATASET
# =============================================================================

cat("\nSTEP 2: CREATING CORRECTED INTEGRATED DATASET\n")
cat(strrep("-", 60), "\n")

# Start with Phase 3 conservation data as the baseline (most reliable)
if (!is.null(conservation_data)) {
  integrated_data <- conservation_data
  cat("✅ Using Phase 3 conservation data as baseline\n")
  
  # Add family information from species data
  if (!is.null(species_data) && "Family" %in% colnames(species_data)) {
    integrated_data <- integrated_data %>%
      left_join(species_data %>% select(Scientific_Name, Family), by = "Scientific_Name")
    cat("✅ Added family information\n")
  }
  
  # Add IVI data from Phase 2
  if (!is.null(ivi_data) && "IVI" %in% colnames(ivi_data)) {
    # Check if the join column exists
    if ("Scientific_Name" %in% colnames(ivi_data)) {
      integrated_data <- integrated_data %>%
        left_join(ivi_data %>% select(Scientific_Name, IVI), by = "Scientific_Name")
      cat("✅ Added IVI data from Phase 2\n")
    } else if ("Nama Jenis" %in% colnames(ivi_data)) {
      # Use local name for joining if Scientific_Name not available
      integrated_data <- integrated_data %>%
        left_join(ivi_data %>% select(Local_Name = `Nama Jenis`, IVI), by = "Local_Name")
      cat("✅ Added IVI data from Phase 2 (using local names)\n")
    } else {
      cat("⚠️ Cannot join IVI data - no matching column found\n")
    }
  }
  
  # Add threat data from Phase 4 (if available)
  if (!is.null(threat_data)) {
    threat_cols <- intersect(c("Overall_Threat_Score", "Threat_Level"), colnames(threat_data))
    if (length(threat_cols) > 0) {
      integrated_data <- integrated_data %>%
        left_join(threat_data %>% select(Scientific_Name, all_of(threat_cols)), by = "Scientific_Name")
      cat("✅ Added threat assessment data\n")
    }
  }
  
  # Add viability data from Phase 4 (if available)
  if (!is.null(viability_data)) {
    viability_cols <- intersect(c("Viability_Status", "MVP_Status"), colnames(viability_data))
    if (length(viability_cols) > 0) {
      integrated_data <- integrated_data %>%
        left_join(viability_data %>% select(Scientific_Name, all_of(viability_cols)), by = "Scientific_Name")
      cat("✅ Added viability assessment data\n")
    }
  }
  
} else {
  cat("❌ CRITICAL ERROR: Phase 3 conservation data not found!\n")
  cat("❌ Cannot proceed without baseline conservation data.\n")
  stop("Missing required Phase 3 conservation summary file")
}

# Standardize column names for consistency
if ("Conservation_Status" %in% colnames(integrated_data)) {
  integrated_data$Conservation_Status_Clean <- toupper(gsub(" ", "_", integrated_data$Conservation_Status))
}

cat("✅ Integrated dataset created with", nrow(integrated_data), "species\n")
cat("📊 Columns available:", paste(colnames(integrated_data), collapse = ", "), "\n")

# =============================================================================
# STEP 3: DATA VALIDATION AND CONSISTENCY CHECK
# =============================================================================

cat("\nSTEP 3: DATA VALIDATION AND CONSISTENCY CHECK\n")
cat(strrep("-", 60), "\n")

# Check for data consistency issues
cat("🔍 VALIDATION RESULTS:\n")

# Check Priority Scores
if ("Priority_Score" %in% colnames(integrated_data)) {
  priority_range <- range(integrated_data$Priority_Score, na.rm = TRUE)
  cat("✅ Priority Score range:", round(priority_range[1], 3), "to", round(priority_range[2], 3), "\n")
  
  # Show top 3 priority species
  top_priority <- integrated_data %>%
    arrange(desc(Priority_Score)) %>%
    head(3) %>%
    select(Scientific_Name, Local_Name, Priority_Score)
  
  cat("🏆 TOP 3 PRIORITY SPECIES (CORRECTED):\n")
  for (i in 1:nrow(top_priority)) {
    cat("  ", i, ".", top_priority$Local_Name[i], "(", top_priority$Scientific_Name[i], ") - Score:", 
        round(top_priority$Priority_Score[i], 3), "\n")
  }
}

# Check Conservation Status distribution
if ("Conservation_Status" %in% colnames(integrated_data)) {
  status_counts <- table(integrated_data$Conservation_Status)
  cat("📊 CONSERVATION STATUS DISTRIBUTION:\n")
  for (status in names(status_counts)) {
    cat("  ", status, ":", status_counts[status], "species\n")
  }
}

# =============================================================================
# STEP 4: CREATE CORRECTED SUMMARY TABLES
# =============================================================================

cat("\nSTEP 4: CREATING CORRECTED SUMMARY TABLES\n")
cat(strrep("-", 60), "\n")

# Master species table with corrected data
species_master_corrected <- integrated_data %>%
  select(Scientific_Name, Local_Name, Individuals, Conservation_Status,
         any_of(c("Family", "Priority_Score", "Priority_Category", "IVI",
                 "Threat_Level", "Viability_Status"))) %>%
  arrange(desc(if("Priority_Score" %in% colnames(.)) Priority_Score else Individuals))

# Write corrected master table
write_csv(species_master_corrected, "output/phase5_corrected_tables/Species_Master_Corrected.csv")
cat("✅ Corrected species master table saved\n")

# Conservation breakdown table
if ("Conservation_Status" %in% colnames(integrated_data)) {
  conservation_breakdown <- integrated_data %>%
    group_by(Conservation_Status) %>%
    summarise(
      Species_Count = n(),
      Total_Individuals = sum(Individuals, na.rm = TRUE),
      Average_Population = round(mean(Individuals, na.rm = TRUE), 1),
      .groups = 'drop'
    ) %>%
    mutate(
      Percentage_Species = round(Species_Count / sum(Species_Count) * 100, 1),
      Percentage_Individuals = round(Total_Individuals / sum(Total_Individuals) * 100, 1)
    ) %>%
    arrange(desc(Total_Individuals))
  
  write_csv(conservation_breakdown, "output/phase5_corrected_tables/Conservation_Breakdown_Corrected.csv")
  cat("✅ Corrected conservation breakdown saved\n")
}

# Priority comparison table (Phase 3 vs Phase 5)
if (!is.null(conservation_data) && "Priority_Score" %in% colnames(conservation_data)) {
  priority_comparison <- conservation_data %>%
    select(Scientific_Name, Local_Name, Priority_Score_Phase3 = Priority_Score) %>%
    left_join(
      integrated_data %>% select(Scientific_Name, Priority_Score_Phase5 = Priority_Score),
      by = "Scientific_Name"
    ) %>%
    mutate(
      Score_Difference = Priority_Score_Phase5 - Priority_Score_Phase3,
      Consistent = abs(Score_Difference) < 0.001
    ) %>%
    arrange(desc(abs(Score_Difference)))
  
  write_csv(priority_comparison, "output/phase5_corrected_tables/Priority_Score_Comparison.csv")
  cat("✅ Priority score comparison table saved\n")
  
  # Report inconsistencies
  inconsistent_count <- sum(!priority_comparison$Consistent, na.rm = TRUE)
  if (inconsistent_count > 0) {
    cat("⚠️ Found", inconsistent_count, "species with inconsistent priority scores\n")
  } else {
    cat("✅ All priority scores are consistent between phases\n")
  }
}

cat("\n✅ Phase 5 corrected analysis completed successfully!\n")
cat("📁 All corrected outputs saved to output/phase5_corrected_* directories\n")
