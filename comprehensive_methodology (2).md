# COMPREHENSIVE METHODOLOGY 2

This methodology serves as a template for similar interdisciplinary studies integrating ecology, pharmacology, and conservation biology, contributing to the broader goals of biodiversity conservation and sustainable use of natural resources.

---

## APPENDICES

### Appendix A: Field Data Collection Forms

#### A.1 Plot-Level Data Collection Form

```
PLOT DATA COLLECTION FORM
Plot ID: _________ Date: _______ Weather: _______
GPS Coordinates: Lat: _______ Long: _______ Elevation: _______
Researchers: _________________ Start Time: _____ End Time: _____

ENVIRONMENTAL CHARACTERISTICS:
□ Slope (%): _____ □ Aspect (°): _____ □ Canopy Cover (%): _____
□ Soil Type: _______ □ Drainage: _______ □ pH: _______
□ Disturbance Level: None/Low/Moderate/High
□ Human Activity Evidence: Yes/No Details: _________________

PTEROPUS ACTIVITY INDICATORS:
□ Fresh droppings: Yes/No Quantity: _______
□ Feeding signs: Yes/No Tree species: _______
□ Roosting evidence: Yes/No Tree species: _______
□ Flight paths observed: Yes/No Direction: _______

MICROCLIMATE DATA:
□ Air Temperature (°C): _____ □ Humidity (%): _____
□ Light Intensity (lux): _____ □ Wind Speed (m/s): _____

PLOT SKETCH:
[Grid for mapping tree positions and features]

NOTES:
_________________________________________________
```

#### A.2 Individual Tree Measurement Form

```
TREE MEASUREMENT FORM
Tree ID: _________ Plot ID: _________ Date: _______

TAXONOMIC INFORMATION:
□ Local Name: _________________ □ Scientific Name: _________________
□ Family: _________________ □ Certainty Level: High/Medium/Low
□ Voucher Collected: Yes/No Specimen #: _______
□ Photo Numbers: _________________

MORPHOMETRIC MEASUREMENTS:
□ Circumference (cm): _____ □ DBH (cm): _____
□ Total Height (m): _____ □ Clear Bole Height (m): _____
□ Crown Diameter N-S (m): _____ E-W (m): _____
□ Crown Height (m): _____

CONDITION ASSESSMENT:
□ Overall Health: Excellent/Good/Fair/Poor/Dead
□ Crown Condition: Full/Dense/Moderate/Sparse/Bare
□ Trunk Condition: Straight/Curved/Forked/Damaged
□ Buttressing: None/Small/Moderate/Large
□ Lean Angle (°): _____ Direction: _____

ECOLOGICAL OBSERVATIONS:
□ Epiphytes Present: Yes/No Types: _________________
□ Lianas Present: Yes/No Coverage: Low/Moderate/High
□ Flowering: Yes/No Stage: Bud/Open/Spent
□ Fruiting: Yes/No Stage: Immature/Mature/Overripe
□ Evidence of Herbivory: Yes/No Type: _________________

PTEROPUS INTERACTIONS:
□ Feeding Signs: Yes/No Type: Fruit/Flower/Leaf
□ Roosting Evidence: Yes/No Number of Animals: _____
□ Damage Evidence: Yes/No Type: _________________
□ Fresh Droppings: Yes/No Quantity: _____

MEDICINAL USE INDICATORS:
□ Traditional Use Documented: Yes/No Source: _________________
□ Harvesting Evidence: Yes/No Intensity: Low/Moderate/High
□ Local Knowledge Available: Yes/No Informant: _________________

ADDITIONAL NOTES:
_________________________________________________
```

### Appendix B: Statistical Analysis Code Repository

#### B.1 Data Import and Cleaning Functions

```r
# =============================================================================
# DATA IMPORT AND CLEANING FUNCTIONS
# =============================================================================

#' Import and validate Excel data
#' @param file_path Path to Excel file
#' @param sheet_names Vector of sheet names to import
#' @return List of cleaned data frames
import_field_data <- function(file_path, sheet_names = c("Sheet1", "Sheet2")) {
  
  library(readxl)
  library(dplyr)
  library(stringr)
  
  # Import sheets
  raw_data <- map(sheet_names, ~read_excel(file_path, sheet = .x))
  names(raw_data) <- sheet_names
  
  # Clean individual tree data (Sheet1)
  if ("Sheet1" %in% names(raw_data)) {
    raw_data$Sheet1 <- raw_data$Sheet1 %>%
      # Remove empty rows
      filter(!is.na(`Nama Jenis`)) %>%
      # Standardize column names
      rename(
        Tree_ID = No,
        Plot_ID = `No Petak`,
        Species = `Nama Jenis`,
        Circumference = Keliling,
        DBH = Diameter,
        DBH_m = `Diamter (m)`,
        Total_Height = `Tinggi Total`,
        Volume_Total = `Volume Tinggi Total`,
        Clear_Height = `Tinggi Bebas Cabang`,
        Volume_Clear = `Volume Tinggi Bebas Cabang`,
        LBDS = LBDS
      ) %>%
      # Data type conversion
      mutate(
        Tree_ID = as.numeric(Tree_ID),
        Plot_ID = as.numeric(Plot_ID),
        Species = str_trim(Species),
        DBH = as.numeric(DBH),
        Total_Height = as.numeric(Total_Height),
        Volume_Total = as.numeric(Volume_Total),
        LBDS = as.numeric(LBDS)
      ) %>%
      # Data validation
      filter(
        DBH >= 10 & DBH <= 200,  # Reasonable DBH range
        Total_Height >= 3 & Total_Height <= 60,  # Reasonable height range
        !is.na(Species)
      )
  }
  
  # Clean species analysis data (Sheet2)
  if ("Sheet2" %in% names(raw_data)) {
    raw_data$Sheet2 <- raw_data$Sheet2 %>%
      filter(!is.na(`Nama Jenis`)) %>%
      rename(
        Species_ID = No,
        Local_Name = `Nama Pohon`,
        Scientific_Name = `Nama Jenis`,
        Plot_Count = `Jumlah Plot`,
        Individual_Count = `Jumlah Individu`,
        Frequency = F,
        Relative_Frequency = `FR %`,
        Density = K,
        Relative_Density = `KR %`,
        Dominance = D,
        Relative_Dominance = `DR %`,
        IVI = `INP %`,
        Total_LBDS = LBDS,
        Proportion = Pi,
        Log_Proportion = LonPi,
        Shannon_Component = `H'`
      ) %>%
      mutate(
        Species_ID = as.numeric(Species_ID),
        Plot_Count = as.numeric(Plot_Count),
        Individual_Count = as.numeric(Individual_Count),
        IVI = as.numeric(IVI),
        Scientific_Name = str_trim(Scientific_Name)
      )
  }
  
  return(raw_data)
}

#' Validate data consistency between sheets
#' @param data_list List of data frames from import function
#' @return List of validation results
validate_data_consistency <- function(data_list) {
  
  validation_results <- list()
  
  # Check species consistency
  if (all(c("Sheet1", "Sheet2") %in% names(data_list))) {
    
    # Species counts from raw data
    sheet1_counts <- data_list$Sheet1 %>%
      count(Species, name = "Count_Sheet1")
    
    # Species counts from analysis sheet
    sheet2_counts <- data_list$Sheet2 %>%
      select(Scientific_Name, Individual_Count) %>%
      rename(Species = Scientific_Name, Count_Sheet2 = Individual_Count)
    
    # Compare counts
    count_comparison <- full_join(sheet1_counts, sheet2_counts, by = "Species") %>%
      mutate(
        Count_Sheet1 = replace_na(Count_Sheet1, 0),
        Count_Sheet2 = replace_na(Count_Sheet2, 0),
        Difference = Count_Sheet1 - Count_Sheet2,
        Match = Count_Sheet1 == Count_Sheet2
      )
    
    validation_results$species_counts <- count_comparison
    validation_results$count_mismatches <- sum(!count_comparison$Match)
    validation_results$total_difference <- sum(abs(count_comparison$Difference))
  }
  
  # Check for outliers in measurements
  if ("Sheet1" %in% names(data_list)) {
    data <- data_list$Sheet1
    
    # DBH outliers
    dbh_outliers <- detect_outliers(data$DBH)
    validation_results$dbh_outliers <- dbh_outliers
    
    # Height outliers
    height_outliers <- detect_outliers(data$Total_Height)
    validation_results$height_outliers <- height_outliers
    
    # Height-DBH relationship outliers
    if (all(c("DBH", "Total_Height") %in% names(data))) {
      h_d_ratio <- data$Total_Height / data$DBH
      ratio_outliers <- detect_outliers(h_d_ratio)
      validation_results$height_dbh_outliers <- ratio_outliers
    }
  }
  
  return(validation_results)
}

#' Detect outliers using modified z-score
#' @param x Numeric vector
#' @param threshold Threshold for outlier detection (default 3.5)
#' @return List with outlier indices and values
detect_outliers <- function(x, threshold = 3.5) {
  
  if (length(x) < 3) {
    return(list(indices = integer(0), values = numeric(0), n_outliers = 0))
  }
  
  median_x <- median(x, na.rm = TRUE)
  mad_x <- mad(x, na.rm = TRUE)
  
  if (mad_x == 0) {
    return(list(indices = integer(0), values = numeric(0), n_outliers = 0))
  }
  
  modified_z_score <- 0.6745 * (x - median_x) / mad_x
  outlier_indices <- which(abs(modified_z_score) > threshold)
  
  return(list(
    indices = outlier_indices,
    values = x[outlier_indices],
    n_outliers = length(outlier_indices),
    z_scores = modified_z_score[outlier_indices]
  ))
}
```

#### B.2 Ecological Analysis Functions

```r
# =============================================================================
# ECOLOGICAL ANALYSIS FUNCTIONS
# =============================================================================

#' Calculate comprehensive ecological indices
#' @param tree_data Data frame with individual tree measurements
#' @param plot_area Area of each plot in m² (default 400)
#' @return Data frame with species-level ecological indices
calculate_ecological_indices <- function(tree_data, plot_area = 400) {
  
  library(vegan)
  library(dplyr)
  
  # Calculate basic metrics per species
  species_metrics <- tree_data %>%
    group_by(Species) %>%
    summarise(
      # Population metrics
      Total_Individuals = n(),
      Plots_Present = n_distinct(Plot_ID),
      Mean_DBH = mean(DBH, na.rm = TRUE),
      SD_DBH = sd(DBH, na.rm = TRUE),
      Mean_Height = mean(Total_Height, na.rm = TRUE),
      SD_Height = sd(Total_Height, na.rm = TRUE),
      
      # Basal area calculations
      Total_Basal_Area = sum(pi * (DBH/2)^2, na.rm = TRUE) / 10000, # Convert to m²
      Mean_Basal_Area = mean(pi * (DBH/2)^2, na.rm = TRUE) / 10000,
      
      # Volume calculations
      Total_Volume = sum(Volume_Total, na.rm = TRUE),
      Mean_Volume = mean(Volume_Total, na.rm = TRUE),
      
      .groups = 'drop'
    )
  
  # Calculate relative values
  total_individuals <- sum(species_metrics$Total_Individuals)
  total_plots <- max(tree_data$Plot_ID)
  total_basal_area <- sum(species_metrics$Total_Basal_Area)
  total_area_sampled <- total_plots * plot_area / 10000  # Convert to hectares
  
  species_metrics <- species_metrics %>%
    mutate(
      # Frequency metrics
      Frequency = Plots_Present / total_plots,
      Relative_Frequency = (Frequency / sum(Frequency)) * 100,
      
      # Density metrics
      Density = Total_Individuals / total_area_sampled,
      Relative_Density = (Total_Individuals / total_individuals) * 100,
      
      # Dominance metrics
      Dominance = Total_Basal_Area / total_area_sampled,
      Relative_Dominance = (Total_Basal_Area / total_basal_area) * 100,
      
      # Important Value Index
      IVI = Relative_Frequency + Relative_Density + Relative_Dominance
    )
  
  return(species_metrics)
}

#' Calculate diversity indices
#' @param tree_data Data frame with individual tree measurements
#' @param level Character: "plot" or "overall"
#' @return Data frame with diversity indices
calculate_diversity_indices <- function(tree_data, level = "overall") {
  
  library(vegan)
  library(dplyr)
  
  if (level == "plot") {
    # Plot-level diversity
    plot_diversity <- tree_data %>%
      group_by(Plot_ID) %>%
      summarise(
        Species_Count = n_distinct(Species),
        Individual_Count = n(),
        .groups = 'drop'
      ) %>%
      rowwise() %>%
      mutate(
        # Get species counts for this plot
        plot_data = list(tree_data %>% filter(Plot_ID == !!Plot_ID)),
        species_table = list(table(plot_data[[1]]$Species)),
        
        # Calculate indices
        Shannon_Index = diversity(species_table[[1]], index = "shannon"),
        Simpson_Index = diversity(species_table[[1]], index = "simpson"),
        Evenness = Shannon_Index / log(Species_Count),
        
        # Dominance
        Dominance_Index = max(species_table[[1]]) / Individual_Count
      ) %>%
      select(-plot_data, -species_table)
    
    return(plot_diversity)
    
  } else {
    # Overall diversity
    species_counts <- table(tree_data$Species)
    total_species <- length(species_counts)
    total_individuals <- sum(species_counts)
    
    diversity_results <- data.frame(
      Total_Species = total_species,
      Total_Individuals = total_individuals,
      Shannon_Index = diversity(species_counts, index = "shannon"),
      Simpson_Index = diversity(species_counts, index = "simpson"),
      Evenness = diversity(species_counts, index = "shannon") / log(total_species),
      Dominance_Index = max(species_counts) / total_individuals,
      Margalef_Richness = (total_species - 1) / log(total_individuals)
    )
    
    return(diversity_results)
  }
}

#' Analyze population structure by diameter classes
#' @param tree_data Data frame with individual tree measurements
#' @return List with overall and species-specific diameter class distributions
analyze_population_structure <- function(tree_data) {
  
  # Define diameter classes
  tree_data <- tree_data %>%
    mutate(
      Diameter_Class = case_when(
        DBH >= 10 & DBH <= 20 ~ "A (10-20 cm)",
        DBH > 20 & DBH <= 30 ~ "B (21-30 cm)",
        DBH > 30 & DBH <= 40 ~ "C (31-40 cm)",
        DBH > 40 & DBH <= 50 ~ "D (41-50 cm)",
        DBH > 50 ~ "E (>50 cm)",
        TRUE ~ "Unclassified"
      ),
      Diameter_Class_Code = case_when(
        DBH >= 10 & DBH <= 20 ~ "A",
        DBH > 20 & DBH <= 30 ~ "B",
        DBH > 30 & DBH <= 40 ~ "C",
        DBH > 40 & DBH <= 50 ~ "D",
        DBH > 50 ~ "E",
        TRUE ~ "U"
      )
    )
  
  # Overall diameter class distribution
  overall_distribution <- tree_data %>%
    count(Diameter_Class, name = "Count") %>%
    mutate(
      Proportion = Count / sum(Count),
      Percentage = Proportion * 100
    ) %>%
    arrange(match(Diameter_Class, c("A (10-20 cm)", "B (21-30 cm)", 
                                   "C (31-40 cm)", "D (41-50 cm)", "E (>50 cm)")))
  
  # Species-specific diameter distributions
  species_distribution <- tree_data %>%
    group_by(Species, Diameter_Class) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    group_by(Species) %>%
    mutate(
      Species_Total = sum(Count),
      Proportion = Count / Species_Total,
      Percentage = Proportion * 100
    ) %>%
    ungroup()
  
  # Calculate structural metrics
  structural_metrics <- tree_data %>%
    group_by(Species) %>%
    summarise(
      Mean_DBH = mean(DBH, na.rm = TRUE),
      Median_DBH = median(DBH, na.rm = TRUE),
      Q25_DBH = quantile(DBH, 0.25, na.rm = TRUE),
      Q75_DBH = quantile(DBH, 0.75, na.rm = TRUE),
      CV_DBH = sd(DBH, na.rm = TRUE) / mean(DBH, na.rm = TRUE) * 100,
      
      # Skewness of diameter distribution
      Skewness = calculate_skewness(DBH),
      
      # Proportion in large diameter classes (>40 cm)
      Prop_Large_Trees = sum(DBH > 40, na.rm = TRUE) / n(),
      
      .groups = 'drop'
    )
  
  return(list(
    overall_distribution = overall_distribution,
    species_distribution = species_distribution,
    structural_metrics = structural_metrics,
    raw_data_with_classes = tree_data
  ))
}

#' Calculate skewness
#' @param x Numeric vector
#' @return Skewness value
calculate_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA)
  
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  skew <- sum(((x - mean_x) / sd_x)^3) / n
  return(skew)
}
```

### Appendix C: Pharmacological Data Integration

#### C.1 Literature Database Schema

```sql
-- =============================================================================
-- PHARMACOLOGICAL DATABASE SCHEMA
-- =============================================================================

-- Main species table
CREATE TABLE species_master (
    species_id INTEGER PRIMARY KEY AUTOINCREMENT,
    scientific_name VARCHAR(100) NOT NULL UNIQUE,
    local_name VARCHAR(100),
    family VARCHAR(50),
    genus VARCHAR(50),
    species_epithet VARCHAR(50),
    author_citation VARCHAR(100),
    common_names TEXT,
    conservation_status VARCHAR(20),
    iucn_status VARCHAR(20),
    endemic_status VARCHAR(50),
    created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    data_quality_score INTEGER DEFAULT 0
);

-- Pharmacological studies table
CREATE TABLE pharmacological_studies (
    study_id INTEGER PRIMARY KEY AUTOINCREMENT,
    species_id INTEGER REFERENCES species_master(species_id),
    title TEXT NOT NULL,
    authors TEXT,
    journal VARCHAR(200),
    publication_year INTEGER,
    doi VARCHAR(100),
    pmid VARCHAR(20),
    impact_factor DECIMAL(4,2),
    study_type VARCHAR(50),
    plant_part VARCHAR(50),
    extraction_method VARCHAR(100),
    solvent_used VARCHAR(100),
    study_design VARCHAR(50),
    sample_size INTEGER,
    quality_score INTEGER DEFAULT 0,
    notes TEXT,
    created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Bioactive compounds table
CREATE TABLE bioactive_compounds (
    compound_id INTEGER PRIMARY KEY AUTOINCREMENT,
    compound_name VARCHAR(150) NOT NULL,
    chemical_formula VARCHAR(50),
    molecular_weight DECIMAL(8,3),
    cas_number VARCHAR(20),
    iupac_name TEXT,
    smiles VARCHAR(500),
    inchi VARCHAR(1000),
    compound_class VARCHAR(100),
    structure_type VARCHAR(100),
    synonyms TEXT,
    created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Species-compound relationships
CREATE TABLE species_compounds (
    relationship_id INTEGER PRIMARY KEY AUTOINCREMENT,
    species_id INTEGER REFERENCES species_master(species_id),
    compound_id INTEGER REFERENCES bioactive_compounds(compound_id),
    study_id INTEGER REFERENCES pharmacological_studies(study_id),
    plant_part VARCHAR(50),
    concentration DECIMAL(10,4),
    concentration_unit VARCHAR(20),
    isolation_method VARCHAR(100),
    yield_percentage DECIMAL(5,2),
    purity_percentage DECIMAL(5,2),
    verified BOOLEAN DEFAULT FALSE,
    notes TEXT
);

-- Pharmacological activities
CREATE TABLE pharmacological_activities (
    activity_id INTEGER PRIMARY KEY AUTOINCREMENT,
    activity_name VARCHAR(100) NOT NULL,
    activity_category VARCHAR(50),
    mechanism_of_action TEXT,
    target_organism VARCHAR(100),
    target_system VARCHAR(100),
    therapeutic_area VARCHAR(100),
    description TEXT
);

-- Bioassay results
CREATE TABLE bioassay_results (
    result_id INTEGER PRIMARY KEY AUTOINCREMENT,
    study_id INTEGER REFERENCES pharmacological_studies(study_id),
    compound_id INTEGER REFERENCES bioactive_compounds(compound_id),
    activity_id INTEGER REFERENCES pharmacological_activities(activity_id),
    assay_type VARCHAR(100),
    test_organism VARCHAR(100),
    cell_line VARCHAR(100),
    concentration_tested DECIMAL(10,4),
    concentration_unit VARCHAR(20),
    ic50_value DECIMAL(10,4),
    ic50_unit VARCHAR(20),
    ec50_value DECIMAL(10,4),
    ec50_unit VARCHAR(20),
    mic_value DECIMAL(10,4),
    mic_unit VARCHAR(20),
    percent_inhibition DECIMAL(5,2),
    positive_control VARCHAR(100),
    positive_control_value DECIMAL(10,4),
    statistical_significance VARCHAR(20),
    p_value DECIMAL(8,6),
    confidence_interval VARCHAR(50),
    effect_size DECIMAL(6,3),
    experimental_conditions TEXT,
    quality_score INTEGER DEFAULT 0,
    notes TEXT
);

-- Traditional uses
CREATE TABLE traditional_uses (
    use_id INTEGER PRIMARY KEY AUTOINCREMENT,
    species_id INTEGER REFERENCES species_master(species_id),
    use_category VARCHAR(100),
    condition_treated VARCHAR(200),
    plant_part_used VARCHAR(50),
    preparation_method VARCHAR(200),
    administration_route VARCHAR(50),
    dosage VARCHAR(100),
    duration_of_use VARCHAR(50),
    geographic_region VARCHAR(100),
    ethnic_group VARCHAR(100),
    source_reference VARCHAR(200),
    reliability_score INTEGER DEFAULT 0,
    notes TEXT
);

-- Data quality indicators
CREATE TABLE data_quality_scores (
    score_id INTEGER PRIMARY KEY AUTOINCREMENT,
    table_name VARCHAR(50),
    record_id INTEGER,
    completeness_score INTEGER DEFAULT 0,
    accuracy_score INTEGER DEFAULT 0,
    reliability_score INTEGER DEFAULT 0,
    timeliness_score INTEGER DEFAULT 0,
    overall_score INTEGER DEFAULT 0,
    last_assessment TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    assessor VARCHAR(100)
);

-- Indexes for performance
CREATE INDEX idx_species_scientific_name ON species_master(scientific_name);
CREATE INDEX idx_studies_species ON pharmacological_studies(species_id);
CREATE INDEX idx_compounds_name ON bioactive_compounds(compound_name);
CREATE INDEX idx_results_study ON bioassay_results(study_id);
CREATE INDEX idx_results_compound ON bioassay_results(compound_id);
CREATE INDEX idx_results_activity ON bioassay_results(activity_id);
```

### Appendix D: Quality Control Protocols

#### D.1 Field Data Quality Checklist

```
FIELD DATA QUALITY CONTROL CHECKLIST

PRE-FIELD PREPARATION:
□ All instruments calibrated and tested
□ Data collection forms prepared and waterproofed
□ GPS units tested and batteries charged
□ Weather forecast checked
□ Safety equipment inspected
□ Contact with local authorities confirmed
□ Research permits verified and copies available

DAILY FIELD PROCEDURES:
□ Morning equipment check completed
□ GPS accuracy verified (<3m error)
□ First measurement cross-checked by second observer
□ Data forms completed in waterproof ink
□ Photos taken with clear labels and scales
□ Unusual observations documented in detail
□ End-of-day data review completed
□ Data backed up (scan/photograph forms)

MEASUREMENT QUALITY CONTROL:
□ DBH measured at exactly 1.3m height
□ Circumference tape level and tight
□ Height measurements cross-verified
□ Tree ID tags securely attached
□ Plot boundaries clearly marked
□ Coordinate accuracy within tolerance
□ All required fields completed

SPECIES IDENTIFICATION:
□ Identification confidence level recorded
□ Photos taken of diagnostic features
□ Voucher specimens collected for uncertain IDs
□ Field guide references documented
□ Local name verified with multiple sources
□ Phenological state recorded

POST-FIELD VERIFICATION:
□ Data forms reviewed for completeness
□ Questionable measurements flagged
□ Species identifications verified
□ Data entry double-checked
□ Outliers investigated and documented
□ Missing data identified and recorded
□ Quality score assigned to each record

WEEKLY QUALITY REVIEW:
□ Inter-observer reliability assessed
□ Systematic errors identified and corrected
□ Equipment drift checked and adjusted
□ Data consistency verified
□ Progress against targets evaluated
□ Quality metrics calculated and reported
```

#### D.2 Statistical Analysis Quality Assurance

```r
# =============================================================================
# STATISTICAL QUALITY ASSURANCE FUNCTIONS
# =============================================================================

#' Comprehensive data quality assessment
#' @param data Data frame to assess
#' @return List of quality metrics and issues
assess_data_quality <- function(data) {
  
  quality_report <- list()
  
  # Completeness assessment
  quality_report$completeness <- data %>%
    summarise_all(~sum(is.na(.)) / length(.) * 100) %>%
    gather(variable, missing_percent) %>%
    mutate(
      completeness_grade = case_when(
        missing_percent == 0 ~ "Excellent",
        missing_percent <= 5 ~ "Good",
        missing_percent <= 15 ~ "Fair",
        missing_percent <= 30 ~ "Poor",
        TRUE ~ "Unacceptable"
      )
    )
  
  # Range validation for numeric variables
  numeric_vars <- data %>% select_if(is.numeric) %>% names()
  
  if (length(numeric_vars) > 0) {
    quality_report$range_validation <- map_dfr(numeric_vars, function(var) {
      values <- data[[var]]
      values <- values[!is.na(values)]
      
      data.frame(
        variable = var,
        min_value = min(values),
        max_value = max(values),
        mean_value = mean(values),
        median_value = median(values),
        outliers_count = sum(abs(scale(values)) > 3, na.rm = TRUE),
        outliers_percent = sum(abs(scale(values)) > 3, na.rm = TRUE) / length(values) * 100
      )
    })
  }
  
  # Consistency checks
  if (all(c("DBH", "Total_Height", "Volume_Total") %in% names(data))) {
    # Check volume calculations
    calculated_volume <- pi * (data$DBH/2)^2 * data$Total_Height * 0.7 / 1000000
    volume_difference <- abs(data$Volume_Total - calculated_volume)
    
    quality_report$volume_consistency <- data.frame(
      records_checked = sum(!is.na(data$Volume_Total)),
      mean_difference = mean(volume_difference, na.rm = TRUE),
      max_difference = max(volume_difference, na.rm = TRUE),
      records_with_large_difference = sum(volume_difference > 0.1, na.rm = TRUE),
      consistency_rate = sum(volume_difference <= 0.1, na.rm = TRUE) / 
                        sum(!is.na(volume_difference)) * 100
    )
  }
  
  # Duplicate detection
  if ("Tree_ID" %in% names(data)) {
    quality_report$duplicates <- data %>%
      group_by(Tree_ID) %>%
      summarise(count = n(), .groups = 'drop') %>%
      filter(count > 1)
  }
  
  # Species name standardization issues
  if ("Species" %in% names(data)) {
    quality_report$species_issues <- data %>%
      count(Species) %>%
      mutate(
        has_numbers = str_detect(Species, "\\d"),
        has_special_chars = str_detect(Species, "[^A-Za-z\\s]"),
        too_short = str_length(Species) < 3,
        potential_issues = has_numbers | has_special_chars | too_short
      ) %>%
      filter(potential_issues)
  }
  
  return(quality_report)
}

#' Validate statistical assumptions
#' @param data Data frame
#' @param response_var Name of response variable
#' @param predictor_vars Vector of predictor variable names
#' @return List of assumption test results
validate_statistical_assumptions <- function(data, response_var, predictor_vars = NULL) {
  
  assumption_tests <- list()
  
  # Normality tests
  response_data <- data[[response_var]]
  response_data <- response_data[!is.na(response_data)]
  
  if (length(response_data) >= 3) {
    if (length(response_data) <= 5000) {
      assumption_tests$normality_shapiro <- shapiro.test(response_data)
    }
    
    assumption_tests$normality_ks <- ks.test(response_data, "pnorm", 
                                           mean(response_data), sd(response_data))
    
    # Skewness and kurtosis
    assumption_tests$skewness <- calculate_skewness(response_data)
    assumption_tests$kurtosis <- calculate_kurtosis(response_data)
  }
  
  # If predictors provided, test for relationships
  if (!is.null(predictor_vars)) {
    
    # Linearity assessment (for numeric predictors)
    numeric_predictors <- predictor_vars# COMPREHENSIVE METHODOLOGY
## Populations of Medicinal Tree Species in Habitat of Pteropus vampyrus

**Study Location:** Situ Lengkong Panjalu Nature Reserve, Nusa Gede Island, Ciamis Regency, West Java, Indonesia

---

## 1. STUDY DESIGN AND SITE SELECTION

### 1.1 Study Area Description
The research was conducted in Situ Lengkong Panjalu Nature Reserve (7°19'S, 108°04'E), specifically on Nusa Gede Island, covering an area of 92.5 hectares. The island represents a unique habitat within the nature reserve that supports populations of *Pteropus vampyrus* (Large Flying Fox) and serves as a natural repository for medicinal plant species.

**Environmental Characteristics:**
- **Elevation:** 450-500 m above sea level
- **Climate:** Tropical monsoon climate (Type A according to Köppen classification)
- **Temperature range:** 22.5-25.8°C
- **Relative humidity:** 93-99%
- **Annual rainfall:** 2,500-3,000 mm
- **Soil type:** Latosol with volcanic parent material
- **Vegetation type:** Secondary tropical rainforest

### 1.2 Sampling Design
A systematic grid sampling method was employed using purposive sampling based on the highest encounter rates with *Pteropus vampyrus*. The sampling strategy was designed to capture the spatial heterogeneity of medicinal tree species distribution while ensuring adequate representation of habitats preferred by flying foxes.

**Sampling Parameters:**
- **Plot size:** 20 m × 20 m (400 m²)
- **Total plots:** 12 plots
- **Sampling intensity:** 5% of total island area
- **Plot arrangement:** Grid system with systematic spacing
- **Directional coverage:** Four cardinal directions (North, South, East, West) based on wind patterns and *Pteropus* movement corridors

---

## 2. FIELD DATA COLLECTION

### 2.1 Tree Measurement Protocol

#### 2.1.1 Inclusion Criteria
Trees were included in the study if they met the following criteria:
- **Diameter at Breast Height (DBH) ≥ 10 cm** (measured at 1.3 m above ground)
- **Identified as having medicinal properties** based on ethnobotanical literature and local knowledge
- **Present within designated sample plots**
- **Alive and structurally intact** at time of measurement

#### 2.1.2 Morphometric Measurements
For each individual tree, the following parameters were recorded:

**Primary Measurements:**
1. **Circumference (C):** Measured at 1.3 m height using measuring tape (accuracy ±0.1 cm)
2. **Diameter (DBH):** Calculated from circumference using formula: DBH = C/π
3. **Total Height (Ht):** Measured using clinometer/hypsometer (accuracy ±0.5 m)
4. **Clear Bole Height (Hcb):** Height to first major branch
5. **Crown Dimensions:** Crown diameter and crown length where applicable

**Derived Calculations:**
1. **Basal Area (BA):** BA = π × (DBH/2)²
2. **Volume:** V = BA × H × Form Factor (0.7 for tropical trees)
3. **Luas Bidang Dasar per Spesies (LBDS):** Basal area per hectare

#### 2.1.3 Qualitative Assessments
- **Health status:** Healthy, stressed, diseased, or damaged
- **Crown condition:** Full, partial, sparse, or dead
- **Trunk characteristics:** Straight, curved, forked, buttressed
- **Epiphyte presence:** Type and abundance of epiphytes
- **Evidence of Pteropus activity:** Feeding signs, roosting evidence, damage patterns

### 2.2 Species Identification and Validation

#### 2.2.1 Taxonomic Identification Protocol
1. **Field identification:** Using standard taxonomic keys and field guides
2. **Photographic documentation:** Leaves, bark, flowers, fruits (when available)
3. **Voucher specimen collection:** For uncertain identifications
4. **Expert verification:** Consultation with taxonomic specialists
5. **Herbarium cross-reference:** Verification against Herbarium Bogoriense (BO)

#### 2.2.2 Nomenclature Standardization
- **Scientific names:** Following APG IV classification system
- **Author citations:** According to International Code of Nomenclature (ICN)
- **Synonymy verification:** Using IPNI (International Plant Names Index) and POWO (Plants of the World Online)
- **Local names:** Documented with cultural and linguistic context

### 2.3 Spatial Data Collection

#### 2.3.1 Geographic Positioning
- **Plot coordinates:** Recorded using GPS (accuracy <3 m)
- **Individual tree mapping:** Within-plot coordinates for trees >50 cm DBH
- **Elevation data:** Digital elevation model with 1 m resolution
- **Aspect and slope:** Measured using clinometer and compass

#### 2.3.2 Habitat Characterization
- **Canopy cover:** Estimated using spherical densiometer
- **Understory density:** Visual assessment on 4-point scale
- **Soil characteristics:** pH, texture, drainage, depth
- **Microclimate data:** Temperature, humidity, light intensity
- **Disturbance indicators:** Human activity, natural disturbances

---

## 3. LABORATORY ANALYSES

### 3.1 Pharmacological Literature Review

#### 3.1.1 Database Search Strategy
**Primary Databases:**
- PubMed/MEDLINE
- Web of Science
- Scopus
- Google Scholar
- SciFinder Scholar

**Search Terms:**
- Species scientific names AND ("medicinal" OR "pharmacological" OR "bioactive")
- Genus names AND ("phytochemistry" OR "bioactivity" OR "therapeutic")
- Common names AND ("traditional medicine" OR "ethnobotany")

**Inclusion Criteria:**
- Peer-reviewed articles (2000-2024)
- Articles in English or Indonesian
- Studies on pharmacological activities
- Phytochemical investigations
- Ethnobotanical reports

#### 3.1.2 Data Extraction Protocol
For each relevant publication, the following information was extracted:
- **Species identification and verification**
- **Plant parts studied** (bark, leaves, fruits, etc.)
- **Extraction methods** and solvents used
- **Bioactive compounds** identified
- **Pharmacological activities** reported
- **Bioassay methods** and results
- **Traditional uses** documented

### 3.2 Phytochemical Data Integration

#### 3.2.1 Compound Classification System
Bioactive compounds were classified according to:
- **Chemical structure:** Terpenoids, phenolics, alkaloids, etc.
- **Pharmacological activity:** Antimicrobial, antioxidant, anti-inflammatory, etc.
- **Mechanism of action:** When reported in literature
- **Therapeutic potential:** Based on in vitro/in vivo studies

#### 3.2.2 Quality Assessment of Sources
Literature sources were evaluated using:
- **Journal impact factor** and reputation
- **Study design quality** (controls, replication, statistics)
- **Sample size adequacy**
- **Methodological rigor**
- **Reproducibility of results**

---

## 4. DATA MANAGEMENT AND PROCESSING

### 4.1 Database Structure

#### 4.1.1 Primary Data Tables
**Plot-level data:**
- Plot_ID, Coordinates, Environmental_variables, Date_sampled

**Individual tree data:**
- Tree_ID, Plot_ID, Species, DBH, Height, Volume, Health_status

**Species data:**
- Species_ID, Scientific_name, Local_name, Family, Medicinal_properties

**Pharmacological data:**
- Compound_ID, Species_ID, Compound_name, Activity, Reference

#### 4.1.2 Data Quality Control
- **Double data entry** for 10% of records
- **Range checks** for all continuous variables
- **Consistency checks** across related fields
- **Missing data documentation** and handling protocols
- **Outlier detection** and verification procedures

### 4.2 Data Processing Workflow

#### 4.2.1 Statistical Software and Tools
- **Primary analysis:** Microsoft Excel 2019, R 4.3.0
- **Spatial analysis:** QGIS 3.22, ArcGIS 10.8
- **Statistical packages:** vegan, BiodiversityR, ggplot2, dplyr
- **Database management:** SQLite, MySQL

#### 4.2.2 Data Transformation and Standardization
- **Unit conversions:** All measurements standardized to metric system
- **Density calculations:** Per hectare basis
- **Volume standardization:** Cubic meters per individual and per hectare
- **Species codes:** Unique identifiers for database management

---

## 5. ANALYTICAL METHODS

### 5.1 Population Structure Analysis

#### 5.1.1 Demographic Parameters
**Population metrics calculated:**
- **Absolute density:** Number of individuals per hectare
- **Relative density:** Percentage of each species
- **Frequency:** Percentage of plots containing each species
- **Relative frequency:** Proportion of species frequency
- **Dominance:** Basal area per species per hectare
- **Relative dominance:** Percentage basal area

#### 5.1.2 Diameter Class Distribution
Trees were classified into five diameter classes following Ismail et al. (2019):
- **Class A:** 10-20 cm DBH
- **Class B:** 21-30 cm DBH  
- **Class C:** 31-40 cm DBH
- **Class D:** 41-50 cm DBH
- **Class E:** >50 cm DBH

**Analysis included:**
- Frequency distribution across classes
- Species-specific diameter distributions
- Population structure interpretation
- Regeneration status assessment

#### 5.1.3 Height Class Analysis
Vertical structure analyzed using height classes:
- **Class I:** >30 m (Emergent/Canopy)
- **Class II:** 20-30 m (Canopy)
- **Class III:** 4-20 m (Understory)
- **Class IV:** 1-4 m (Shrub layer)
- **Class V:** <1 m (Ground layer)

### 5.2 Ecological Indices

#### 5.2.1 Important Value Index (IVI)
Calculated using the formula:
```
IVI = RF + RD + RDom

Where:
RF = Relative Frequency = (Fi/ΣF) × 100%
RD = Relative Density = (Ni/ΣN) × 100%
RDom = Relative Dominance = (BAi/ΣBA) × 100%

Fi = Frequency of species i
Ni = Number of individuals of species i
BAi = Basal area of species i
```

#### 5.2.2 Diversity Indices

**Shannon-Wiener Diversity Index (H'):**
```
H' = -Σ(pi × ln pi)

Where:
pi = Ni/N (proportion of species i)
Ni = Number of individuals of species i
N = Total number of individuals
```

**Evenness Index (E):**
```
E = H'/H'max = H'/ln S

Where:
H' = Shannon diversity index
S = Number of species
```

**Simpson's Diversity Index (D):**
```
D = 1 - Σ(pi²)

Where:
pi = Proportion of species i
```

#### 5.2.3 Similarity Indices

**Sørensen Similarity Index:**
```
Sørensen = 2C/(A + B)

Where:
C = Number of species common to both plots
A = Number of species in plot A
B = Number of species in plot B
```

### 5.3 Statistical Analyses

#### 5.3.1 Descriptive Statistics
For all continuous variables:
- **Central tendency:** Mean, median, mode
- **Dispersion:** Standard deviation, variance, coefficient of variation
- **Distribution shape:** Skewness, kurtosis
- **Range:** Minimum, maximum, quartiles

#### 5.3.2 Inferential Statistics

**Normality Testing:**
- Shapiro-Wilk test (n < 50)
- Kolmogorov-Smirnov test (n > 50)
- Q-Q plots for visual assessment

**Comparative Analyses:**
- **t-tests** for comparing means between groups
- **ANOVA** for multiple group comparisons
- **Kruskal-Wallis test** for non-parametric comparisons
- **Chi-square tests** for categorical variables

**Correlation Analyses:**
- **Pearson correlation** for linear relationships
- **Spearman correlation** for non-parametric associations
- **Regression analysis** for predictive relationships

#### 5.3.3 Multivariate Analyses

**Ordination Methods:**
- **Principal Component Analysis (PCA)** for environmental gradients
- **Non-metric Multidimensional Scaling (NMDS)** for community composition
- **Canonical Correspondence Analysis (CCA)** for species-environment relationships

**Classification Methods:**
- **Hierarchical cluster analysis** for plot grouping
- **K-means clustering** for species grouping
- **TWINSPAN** for two-way classification

### 5.4 Spatial Analysis

#### 5.4.1 Spatial Distribution Patterns

**Point Pattern Analysis:**
- **Nearest neighbor analysis** for spatial randomness
- **Ripley's K-function** for clustering patterns
- **G-function** for inter-event distances

**Aggregation Indices:**
- **Variance to mean ratio** for overdispersion
- **Morisita's Index** for aggregation intensity
- **Green's Index** for spatial clustering

#### 5.4.2 Spatial Autocorrelation
- **Moran's I** for global spatial autocorrelation
- **Local Moran's I** for local clustering
- **Geary's C** for spatial association
- **Mantel tests** for distance relationships

### 5.5 Pharmacological Data Analysis

#### 5.5.1 Activity Classification System

**Primary Activities:**
- **Antimicrobial:** Antibacterial, antifungal, antiviral
- **Antioxidant:** Free radical scavenging, oxidative stress reduction
- **Anti-inflammatory:** Cyclooxygenase inhibition, cytokine modulation
- **Anticancer:** Cytotoxic, antiproliferative, apoptotic
- **Cardioprotective:** Cardiovascular health benefits
- **Neuroprotective:** Neurological disorder applications

**Quantitative Metrics:**
- **IC50 values** for dose-response relationships
- **Minimum Inhibitory Concentration (MIC)** for antimicrobials
- **TEAC values** for antioxidant capacity
- **Selectivity indices** for therapeutic windows

#### 5.5.2 Priority Scoring System

**Population-Activity Matrix:**
Species prioritized using weighted scoring:
```
Priority Score = (Population Weight × 0.3) + (Activity Weight × 0.4) + (Novelty Weight × 0.3)

Where:
Population Weight: Based on abundance and sustainability
Activity Weight: Based on pharmacological potential and evidence quality
Novelty Weight: Based on uniqueness and research gaps
```

**Conservation Priority:**
- **Critical:** <5 individuals
- **Vulnerable:** 5-15 individuals
- **Stable:** 15-30 individuals
- **Secure:** >30 individuals

---

## 6. QUALITY ASSURANCE AND VALIDATION

### 6.1 Field Data Validation

#### 6.1.1 Inter-observer Reliability
- **Training protocols** for all field personnel
- **Standardized measurement techniques**
- **Calibration of instruments** before each field session
- **Duplicate measurements** for 10% of trees by independent observers
- **Correlation analysis** between observers (target: r > 0.95)

#### 6.1.2 Measurement Accuracy
- **Equipment calibration:** Monthly calibration of all instruments
- **Standard protocols:** Detailed SOPs for all measurements
- **Error documentation:** Recording and analysis of measurement errors
- **Accuracy targets:** ±1 cm for DBH, ±0.5 m for height, ±5° for slope

### 6.2 Data Analysis Validation

#### 6.2.1 Statistical Assumptions
- **Normality testing** before parametric analyses
- **Homoscedasticity verification** using Levene's test
- **Independence assumption** checking through residual analysis
- **Outlier detection** using box plots and z-scores

#### 6.2.2 Model Validation
- **Cross-validation** for predictive models
- **Bootstrap resampling** for confidence intervals
- **Sensitivity analysis** for key parameters
- **Model comparison** using AIC/BIC criteria

### 6.3 Literature Data Validation

#### 6.3.1 Source Credibility Assessment
- **Journal ranking** and impact factor consideration
- **Peer review status** verification
- **Author credentials** and institutional affiliations
- **Methodology quality** assessment using established criteria

#### 6.3.2 Data Consistency Checks
- **Cross-referencing** between multiple sources
- **Taxonomic verification** against authoritative databases
- **Activity confirmation** through multiple independent studies
- **Dose-response validation** where available

---

## 7. ETHICAL CONSIDERATIONS AND PERMITS

### 7.1 Research Permits
- **Research permit:** Indonesian Institute of Sciences (LIPI)
- **Collection permit:** Ministry of Environment and Forestry
- **Access permit:** Situ Lengkong Panjalu Nature Reserve Authority
- **CITES verification:** For any endangered species encountered

### 7.2 Ethical Guidelines
- **Minimal impact sampling:** Non-destructive methods prioritized
- **Local community engagement:** Free, prior, and informed consent
- **Traditional knowledge respect:** Proper attribution and benefit-sharing
- **Data sharing agreements:** With local institutions and communities

### 7.3 Biosafety and Environmental Protection
- **Quarantine protocols:** For any collected specimens
- **Invasive species prevention:** Cleaning protocols between sites
- **Habitat disturbance minimization:** Established pathways and procedures
- **Waste management:** Proper disposal of all research materials

---

## 8. LIMITATIONS AND ASSUMPTIONS

### 8.1 Study Limitations

#### 8.1.1 Temporal Limitations
- **Single season sampling:** Results may not reflect seasonal variations
- **Snapshot assessment:** Population dynamics not captured
- **Phenological timing:** Some species may not have been flowering/fruiting

#### 8.1.2 Spatial Limitations
- **Limited to Nusa Gede Island:** May not represent broader regional patterns
- **Plot-based sampling:** Between-plot variation not fully captured
- **Scale dependency:** Results specific to 20×20 m plot scale

#### 8.1.3 Methodological Limitations
- **Species identification uncertainty:** Some specimens require further verification
- **Measurement errors:** Inherent limitations in field measurements
- **Observer bias:** Potential for systematic errors in data collection

### 8.2 Underlying Assumptions

#### 8.2.1 Ecological Assumptions
- **Population stability:** Assumes populations are in quasi-equilibrium
- **Habitat homogeneity:** Within-plot environmental conditions assumed uniform
- **Species independence:** Interactions between species not explicitly modeled

#### 8.2.2 Statistical Assumptions
- **Random sampling:** Plots assumed representative of larger area
- **Independence:** Individual trees assumed independent observations
- **Normal distribution:** For parametric statistical tests

#### 8.2.3 Literature-based Assumptions
- **Transferability:** Pharmacological data from other regions applicable locally
- **Consistency:** Published activities assumed consistent across populations
- **Reliability:** Literature sources assumed accurate and reproducible

---

## 9. DATA ARCHIVING AND ACCESSIBILITY

### 9.1 Data Storage and Backup
- **Primary storage:** Secure cloud-based repositories with version control
- **Backup systems:** Multiple redundant copies in different locations
- **Format standards:** Open formats (CSV, TXT) for long-term accessibility
- **Metadata documentation:** Complete documentation following Dublin Core standards

### 9.2 Data Sharing Policy
- **Open access:** Raw data available upon reasonable request
- **Attribution requirements:** Proper citation of data sources
- **Embargo period:** 2-year exclusive use period for primary researchers
- **Collaboration opportunities:** Partnerships with other research groups encouraged

### 9.3 Long-term Preservation
- **Institutional repositories:** Deposition in university and national archives
- **International databases:** Contribution to global biodiversity databases
- **Voucher specimens:** Permanent deposition in recognized herbaria
- **Digital preservation:** Following best practices for long-term digital curation

---

## 10. ANTICIPATED CHALLENGES AND MITIGATION STRATEGIES

### 10.1 Field Challenges

**Weather Dependencies:**
- **Challenge:** Tropical weather affecting fieldwork schedules
- **Mitigation:** Flexible scheduling with buffer time, waterproof equipment

**Species Identification:**
- **Challenge:** Taxonomic uncertainty for some specimens
- **Mitigation:** Expert consultation, molecular identification when necessary

**Site Accessibility:**
- **Challenge:** Difficult terrain and remote locations
- **Mitigation:** Local guides, appropriate safety equipment, emergency protocols

### 10.2 Analytical Challenges

**Small Sample Sizes:**
- **Challenge:** Rare species with few individuals
- **Mitigation:** Non-parametric statistics, exact tests, Bayesian approaches

**Literature Gaps:**
- **Challenge:** Limited pharmacological data for some species
- **Mitigation:** Genus-level extrapolation, traditional knowledge integration

**Data Integration:**
- **Challenge:** Combining field and literature data
- **Mitigation:** Standardized protocols, expert validation, uncertainty quantification

### 10.3 Technical Challenges

**Software Compatibility:**
- **Challenge:** Different software platforms and versions
- **Mitigation:** Open-source alternatives, format standardization

**Statistical Complexity:**
- **Challenge:** Advanced multivariate analyses
- **Mitigation:** Expert consultation, methodology validation, sensitivity analysis

---

## 11. COMPUTATIONAL METHODS AND DATA PROCESSING

### 11.1 Data Processing Pipeline

#### 11.1.1 Automated Data Validation
**Excel Data Processing:**
```
1. Import raw data from Excel sheets (Sheet1: Individual measurements, Sheet2: Calculated indices)
2. Data type validation and conversion
3. Range checking for all measurements
4. Duplicate detection and removal
5. Missing data identification and flagging
6. Outlier detection using IQR and z-score methods
7. Cross-validation between related fields
```

**Quality Control Algorithms:**
- **DBH validation:** 10 cm ≤ DBH ≤ 200 cm
- **Height validation:** 3 m ≤ Height ≤ 60 m (tropical forest range)
- **Volume validation:** V = π × (DBH/2)² × Height × 0.7
- **LBDS validation:** Cross-check with individual basal area calculations

#### 11.1.2 Statistical Computing Environment
**Software Configuration:**
- **R version:** 4.3.0 or higher
- **Essential packages:** vegan, BiodiversityR, ggplot2, dplyr, tidyr, corrplot
- **Excel integration:** readxl, openxlsx packages
- **Spatial analysis:** sf, raster, maptools packages

**Reproducible Computing:**
```r
# Set seed for reproducibility
set.seed(123)

# Package version control
renv::init()
renv::snapshot()

# Session information documentation
sessionInfo()
```

### 11.2 Advanced Statistical Analyses

#### 11.2.1 Robust Statistical Methods

**Non-parametric Alternatives:**
When data fails normality assumptions:
- **Mann-Whitney U test** instead of t-test
- **Kruskal-Wallis test** instead of ANOVA
- **Spearman correlation** instead of Pearson
- **Bootstrap confidence intervals** for robust estimation

**Outlier Treatment:**
```r
# Outlier detection using modified z-score
outlier_detection <- function(x, threshold = 3.5) {
  median_x <- median(x, na.rm = TRUE)
  mad_x <- mad(x, na.rm = TRUE)
  modified_z_score <- 0.6745 * (x - median_x) / mad_x
  return(abs(modified_z_score) > threshold)
}
```

#### 11.2.2 Power Analysis and Sample Size Calculation

**Prospective Power Analysis:**
```r
library(pwr)

# Power analysis for species comparison
pwr.anova.test(k = 10,           # number of species
               f = 0.25,         # effect size (medium)
               sig.level = 0.05, # alpha level
               power = 0.80)     # desired power
```

**Effect Size Calculations:**
- **Cohen's d** for mean differences
- **Eta-squared (η²)** for ANOVA effect sizes
- **Cramer's V** for categorical associations

#### 11.2.3 Bayesian Analysis Framework

**Bayesian Diversity Estimation:**
```r
library(rstan)

# Bayesian species richness estimation
bayesian_richness <- function(species_counts) {
  # Prior specification
  alpha_prior <- 1
  beta_prior <- 1
  
  # Likelihood function
  likelihood <- dbinom(species_counts, 
                      size = total_individuals, 
                      prob = detection_probability)
  
  # Posterior calculation using MCMC
  posterior_samples <- stan(model_code = species_model,
                           data = list(counts = species_counts),
                           chains = 4,
                           iter = 2000)
  return(posterior_samples)
}
```

### 11.3 Machine Learning Applications

#### 11.3.1 Predictive Modeling

**Random Forest for Species Classification:**
```r
library(randomForest)

# Feature selection for species prediction
species_rf <- randomForest(Species ~ DBH + Height + Plot + 
                          Environmental_variables,
                          data = tree_data,
                          ntree = 500,
                          importance = TRUE)

# Variable importance assessment
importance(species_rf)
varImpPlot(species_rf)
```

**Gradient Boosting for Population Prediction:**
```r
library(gbm)

# Predicting population density
population_gbm <- gbm(Population_Density ~ Environmental_predictors,
                     data = plot_data,
                     distribution = "poisson",
                     n.trees = 1000,
                     cv.folds = 10)
```

#### 11.3.2 Unsupervised Learning

**Clustering Analysis:**
```r
library(cluster)

# K-means clustering of species based on traits
species_clusters <- kmeans(trait_matrix, centers = 3)

# Hierarchical clustering
species_hclust <- hclust(dist(trait_matrix), method = "ward.D2")

# Silhouette analysis for optimal cluster number
silhouette_analysis <- silhouette(species_clusters$cluster, 
                                 dist(trait_matrix))
```

### 11.4 Pharmacological Data Integration

#### 11.4.1 Literature Mining Pipeline

**Automated Literature Search:**
```python
import pandas as pd
from Bio import Entrez
import requests

def pubmed_search(species_name, terms):
    """
    Automated PubMed search for pharmacological data
    """
    Entrez.email = "researcher@email.com"
    
    search_term = f"{species_name} AND ({' OR '.join(terms)})"
    handle = Entrez.esearch(db="pubmed", term=search_term, retmax=100)
    results = Entrez.read(handle)
    
    return results['IdList']

# Search terms for pharmacological activities
pharm_terms = ["medicinal", "pharmacological", "bioactive", 
               "antimicrobial", "antioxidant", "anti-inflammatory"]
```

**Data Extraction and Standardization:**
```r
# Pharmacological activity scoring
activity_score <- function(compound_data) {
  # Weighted scoring based on:
  # 1. Number of studies (weight: 0.3)
  # 2. Quality of evidence (weight: 0.4)
  # 3. Clinical relevance (weight: 0.3)
  
  study_score <- min(log(n_studies + 1), 5) # Cap at 5
  quality_score <- mean(journal_impact_factors)
  relevance_score <- clinical_trial_presence * 5
  
  total_score <- (study_score * 0.3 + 
                 quality_score * 0.4 + 
                 relevance_score * 0.3)
  return(total_score)
}
```

#### 11.4.2 Network Analysis of Compound-Activity Relationships

**Bipartite Network Construction:**
```r
library(igraph)

# Create bipartite network: Species <-> Compounds <-> Activities
compound_network <- graph_from_data_frame(
  edges = compound_activity_edges,
  vertices = c(species_nodes, compound_nodes, activity_nodes),
  directed = FALSE
)

# Network metrics
betweenness_centrality <- betweenness(compound_network)
eigenvector_centrality <- eigen_centrality(compound_network)$vector
```

### 11.5 Spatial Analysis Methods

#### 11.5.1 Geostatistical Analysis

**Kriging for Spatial Interpolation:**
```r
library(gstat)

# Variogram modeling
variogram_model <- variogram(Species_Richness ~ 1, 
                           locations = ~X + Y, 
                           data = plot_coordinates)

# Kriging interpolation
kriging_result <- krige(Species_Richness ~ 1,
                       locations = ~X + Y,
                       data = plot_coordinates,
                       newdata = prediction_grid,
                       model = variogram_model)
```

#### 11.5.2 Point Pattern Analysis

**Ripley's K-function for Clustering:**
```r
library(spatstat)

# Convert to point pattern object
tree_ppp <- ppp(x = tree_coordinates$X, 
               y = tree_coordinates$Y,
               window = study_area_boundary)

# Ripley's K-function
k_function <- Kest(tree_ppp)

# Test for complete spatial randomness
envelope_test <- envelope(tree_ppp, Kest, nsim = 99)
```

### 11.6 Visualization and Reporting

#### 11.6.1 Advanced Visualization Methods

**Interactive Plots:**
```r
library(plotly)
library(leaflet)

# Interactive species distribution map
species_map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = tree_locations,
                   ~longitude, ~latitude,
                   color = ~species_colors,
                   popup = ~paste("Species:", species_name,
                                 "<br>DBH:", dbh, "cm",
                                 "<br>Height:", height, "m"))

# Interactive 3D scatter plot
plot_3d <- plot_ly(data = tree_data,
                  x = ~DBH, y = ~Height, z = ~Volume,
                  color = ~Species,
                  type = "scatter3d",
                  mode = "markers")
```

**Statistical Graphics:**
```r
library(ggplot2)
library(cowplot)

# Multi-panel publication-ready figures
p1 <- ggplot(species_data, aes(x = DBH, y = Height, color = Species)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "DBH vs Height Relationship by Species")

p2 <- ggplot(diversity_data, aes(x = Plot, y = Shannon_Index)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  theme_minimal() +
  labs(title = "Shannon Diversity Index by Plot")

# Combine plots
combined_plot <- plot_grid(p1, p2, labels = c("A", "B"), ncol = 2)
```

#### 11.6.2 Automated Report Generation

**R Markdown Pipeline:**
```r
# Automated statistical report generation
generate_species_report <- function(species_name) {
  rmarkdown::render("species_template.Rmd",
                   params = list(species = species_name),
                   output_file = paste0(species_name, "_report.html"))
}

# Batch processing for all species
species_list <- unique(tree_data$Species)
lapply(species_list, generate_species_report)
```

**LaTeX Integration for Publications:**
```r
library(xtable)
library(stargazer)

# Formatted tables for publication
species_table <- xtable(species_summary_stats,
                       caption = "Summary statistics for medicinal tree species",
                       label = "tab:species_stats",
                       digits = 2)

# Regression tables
stargazer(diversity_model, population_model,
          title = "Diversity and Population Models",
          label = "tab:models",
          type = "latex")
```

### 11.7 Reproducibility and Version Control

#### 11.7.1 Computational Reproducibility

**Docker Environment:**
```dockerfile
FROM rocker/r-ver:4.3.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev

# Install R packages
COPY requirements.R /tmp/
RUN Rscript /tmp/requirements.R

# Set working directory
WORKDIR /app
COPY . /app/

# Default command
CMD ["Rscript", "main_analysis.R"]
```

**Package Management:**
```r
# renv for package version control
renv::init()
renv::snapshot()

# Required packages with versions
required_packages <- c(
  "vegan >= 2.6.0",
  "BiodiversityR >= 2.15.0",
  "ggplot2 >= 3.4.0",
  "dplyr >= 1.1.0",
  "randomForest >= 4.7.0",
  "cluster >= 2.1.0"
)
```

#### 11.7.2 Version Control Workflow

**Git Integration:**
```bash
# Initialize repository
git init
git add .
git commit -m "Initial commit: methodology and data processing scripts"

# Branching strategy
git checkout -b feature/pharmacological-analysis
git checkout -b feature/spatial-analysis
git checkout -b feature/statistical-modeling

# Pre-commit hooks for code quality
pre-commit install
```

**Data Versioning:**
```r
library(pins)

# Pin datasets for versioning
board <- board_folder("data/")
pin_write(board, tree_data, "tree_measurements", 
          description = "Individual tree measurements and calculations")
pin_write(board, pharmacology_data, "pharmacological_activities",
          description = "Literature-derived pharmacological data")
```

---

## 12. SENSITIVITY ANALYSIS AND UNCERTAINTY QUANTIFICATION

### 12.1 Parameter Sensitivity Analysis

#### 12.1.1 Ecological Indices Sensitivity

**Shannon Index Sensitivity to Rare Species:**
```r
# Bootstrap resampling to assess sensitivity
bootstrap_shannon <- function(data, n_boot = 1000) {
  shannon_values <- numeric(n_boot)
  
  for(i in 1:n_boot) {
    boot_sample <- sample(data$Species, 
                         size = length(data$Species), 
                         replace = TRUE)
    shannon_values[i] <- diversity(table(boot_sample), index = "shannon")
  }
  
  return(list(
    mean = mean(shannon_values),
    ci = quantile(shannon_values, c(0.025, 0.975)),
    se = sd(shannon_values)
  ))
}
```

**IVI Calculation Sensitivity:**
```r
# Sensitivity to measurement errors
ivi_sensitivity <- function(data, error_percent = 5) {
  # Add random error to DBH measurements
  dbh_error <- rnorm(nrow(data), 0, data$DBH * error_percent/100)
  data$DBH_perturbed <- data$DBH + dbh_error
  
  # Recalculate IVI with perturbed data
  ivi_original <- calculate_ivi(data, "DBH")
  ivi_perturbed <- calculate_ivi(data, "DBH_perturbed")
  
  # Calculate sensitivity coefficient
  sensitivity <- abs(ivi_perturbed - ivi_original) / ivi_original
  return(sensitivity)
}
```

#### 12.1.2 Model Sensitivity Analysis

**Linear Model Robustness:**
```r
library(car)

# Influence diagnostics
influence_measures <- influence.measures(diversity_model)
outliers <- which(influence_measures$is.inf[,"dffit"])

# Cook's distance
cooks_d <- cooks.distance(diversity_model)
influential_points <- which(cooks_d > 4/nrow(model_data))

# Leverage analysis
leverage <- hatvalues(diversity_model)
high_leverage <- which(leverage > 2 * length(coef(diversity_model))/nrow(model_data))
```

### 12.2 Uncertainty Propagation

#### 12.2.1 Measurement Uncertainty

**Error Propagation in Volume Calculations:**
```r
# Monte Carlo error propagation
volume_uncertainty <- function(dbh, height, n_sim = 10000) {
  # Measurement uncertainties
  dbh_error <- 0.5  # ± 0.5 cm
  height_error <- 1.0  # ± 1.0 m
  
  # Generate random samples
  dbh_samples <- rnorm(n_sim, dbh, dbh_error)
  height_samples <- rnorm(n_sim, height, height_error)
  
  # Calculate volume for each sample
  volume_samples <- pi * (dbh_samples/2)^2 * height_samples * 0.7
  
  return(list(
    mean = mean(volume_samples),
    sd = sd(volume_samples),
    ci_95 = quantile(volume_samples, c(0.025, 0.975))
  ))
}
```

#### 12.2.2 Literature Data Uncertainty

**Quality-Weighted Evidence Synthesis:**
```r
# Weight studies by quality metrics
quality_weights <- function(studies) {
  # Factors: sample size, journal impact, methodology
  sample_weight <- pmin(log(studies$sample_size), 5) / 5
  journal_weight <- pmin(studies$impact_factor / 10, 1)
  method_weight <- studies$methodology_score / 5
  
  # Combined weight
  overall_weight <- (sample_weight + journal_weight + method_weight) / 3
  return(overall_weight)
}

# Weighted meta-analysis
weighted_effect_size <- function(effects, weights) {
  weighted_mean <- sum(effects * weights) / sum(weights)
  weighted_se <- sqrt(sum(weights * (effects - weighted_mean)^2) / 
                     ((length(effects) - 1) * sum(weights) / length(effects)))
  
  return(list(
    estimate = weighted_mean,
    se = weighted_se,
    ci = weighted_mean + c(-1.96, 1.96) * weighted_se
  ))
}
```

### 12.3 Cross-Validation Procedures

#### 12.3.1 Spatial Cross-Validation

**Leave-One-Plot-Out Validation:**
```r
spatial_cv <- function(data, model_formula) {
  plots <- unique(data$Plot)
  predictions <- numeric(length(plots))
  
  for(i in seq_along(plots)) {
    # Split data
    train_data <- data[data$Plot != plots[i], ]
    test_data <- data[data$Plot == plots[i], ]
    
    # Fit model on training data
    model <- lm(model_formula, data = train_data)
    
    # Predict on test data
    predictions[i] <- predict(model, newdata = test_data)
  }
  
  # Calculate validation metrics
  observed <- sapply(plots, function(p) mean(data$response[data$Plot == p]))
  
  return(list(
    rmse = sqrt(mean((predictions - observed)^2)),
    mae = mean(abs(predictions - observed)),
    r_squared = cor(predictions, observed)^2
  ))
}
```

#### 12.3.2 Temporal Validation Framework

**Pseudo-Temporal Validation:**
```r
# Simulate temporal validation using spatial plots as time proxies
temporal_validation <- function(data, succession_proxy) {
  # Order plots by succession proxy (e.g., canopy cover, basal area)
  ordered_plots <- data[order(data[[succession_proxy]]), ]
  
  # Split into "early" and "late" successional stages
  n_plots <- length(unique(ordered_plots$Plot))
  train_plots <- unique(ordered_plots$Plot)[1:(n_plots %/% 2)]
  test_plots <- unique(ordered_plots$Plot)[(n_plots %/% 2 + 1):n_plots]
  
  # Train on early succession, test on late succession
  train_data <- ordered_plots[ordered_plots$Plot %in% train_plots, ]
  test_data <- ordered_plots[ordered_plots$Plot %in% test_plots, ]
  
  # Model validation
  model <- randomForest(Species_Richness ~ ., data = train_data)
  predictions <- predict(model, test_data)
  
  return(list(
    model = model,
    predictions = predictions,
    observed = test_data$Species_Richness,
    rmse = sqrt(mean((predictions - test_data$Species_Richness)^2))
  ))
}
```

---

## 13. INTEGRATION WITH PTEROPUS VAMPYRUS ECOLOGY

### 13.1 Habitat Preference Analysis

#### 13.1.1 Tree Characteristics vs Pteropus Activity

**Preference Index Calculation:**
```r
# Calculate Jacobs' selectivity index
jacobs_index <- function(used, available) {
  # used: proportion of trees with Pteropus activity
  # available: proportion of trees in habitat
  
  D <- (used - available) / (used + available - 2 * used * available)
  return(D)
}

# Apply to tree characteristics
tree_preferences <- data.frame(
  DBH_class = c("10-20", "21-30", "31-40", "41-50", ">50"),
  used_proportion = c(0.05, 0.15, 0.25, 0.30, 0.25),
  available_proportion = c(0.10, 0.26, 0.22, 0.18, 0.31)
) %>%
  mutate(
    jacobs_index = jacobs_index(used_proportion, available_proportion),
    preference = case_when(
      jacobs_index > 0.3 ~ "Strong preference",
      jacobs_index > 0.1 ~ "Moderate preference",
      jacobs_index > -0.1 ~ "Neutral",
      jacobs_index > -0.3 ~ "Avoidance",
      TRUE ~ "Strong avoidance"
    )
  )
```

#### 13.1.2 Roost Site Quality Assessment

**Multi-criteria Decision Analysis:**
```r
library(MCDA)

# Define criteria and weights
criteria <- c("DBH", "Height", "Crown_cover", "Fruit_availability", "Safety")
weights <- c(0.25, 0.30, 0.20, 0.15, 0.10)

# Performance matrix (trees × criteria)
performance_matrix <- matrix(c(
  # Standardized scores for each tree and criterion
  tree_data$DBH_score,
  tree_data$Height_score,
  tree_data$Crown_score,
  tree_data$Fruit_score,
  tree_data$Safety_score
), ncol = length(criteria))

# TOPSIS analysis for roost quality ranking
roost_quality <- TOPSIS(performance_matrix, weights)
tree_data$roost_quality_rank <- roost_quality$ranking
```

### 13.2 Plant-Animal Interaction Networks

#### 13.2.1 Bipartite Network Analysis

**Fruit-Frugivore Network:**
```r
library(bipartite)

# Create interaction matrix
interaction_matrix <- table(feeding_observations$Tree_species, 
                           feeding_observations$Animal_species)

# Network metrics
network_metrics <- networklevel(interaction_matrix, 
                               index = c("connectance", "web asymmetry", 
                                       "links per species", "nestedness"))

# Species-level metrics
species_metrics <- specieslevel(interaction_matrix,
                               index = c("degree", "betweenness", 
                                       "species strength"))
```

#### 13.2.2 Phenological Synchrony Analysis

**Fruit Availability vs Pteropus Presence:**
```r
# Circular statistics for phenological analysis
library(circular)

# Convert julian days to circular format
fruit_timing <- circular(phenology_data$fruit_peak_day * 2 * pi / 365)
pteropus_timing <- circular(activity_data$peak_activity_day * 2 * pi / 365)

# Test for synchrony
synchrony_test <- watson.two.test(fruit_timing, pteropus_timing)

# Calculate mean vector and concentration
fruit_mean_vector <- mean.circular(fruit_timing)
fruit_concentration <- rho.circular(fruit_timing)
```

### 13.3 Conservation Implications

#### 13.3.1 Habitat Quality Index

**Composite Habitat Score:**
```r
calculate_habitat_quality <- function(plot_data) {
  # Normalize variables to 0-1 scale
  normalize <- function(x) (x - min(x, na.rm = TRUE)) / 
                          (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  
  plot_data %>%
    mutate(
      tree_diversity_norm = normalize(Shannon_diversity),
      large_tree_norm = normalize(prop_large_trees),
      fruit_diversity_norm = normalize(fruit_species_richness),
      canopy_cover_norm = normalize(canopy_cover),
      
      # Weighted habitat quality index
      habitat_quality = (tree_diversity_norm * 0.3 +
                        large_tree_norm * 0.25 +
                        fruit_diversity_norm * 0.25 +
                        canopy_cover_norm * 0.2),
      
      quality_class = case_when(
        habitat_quality >= 0.8 ~ "Excellent",
        habitat_quality >= 0.6 ~ "Good",
        habitat_quality >= 0.4 ~ "Moderate",
        habitat_quality >= 0.2 ~ "Poor",
        TRUE ~ "Very Poor"
      )
    )
}
```

#### 13.3.2 Threat Assessment Matrix

**Multi-threat Analysis:**
```r
# Define threat categories and severity
threats <- data.frame(
  threat_type = c("Habitat fragmentation", "Overharvesting", 
                 "Climate change", "Human disturbance", "Disease"),
  severity = c(3, 2, 4, 2, 1),  # Scale 1-5
  extent = c(4, 3, 5, 3, 2),     # Spatial extent
  immediacy = c(3, 4, 2, 3, 1)   # Time urgency
) %>%
  mutate(
    threat_score = (severity * 0.4 + extent * 0.3 + immediacy * 0.3),
    threat_level = case_when(
      threat_score >= 4 ~ "Critical",
      threat_score >= 3 ~ "High",
      threat_score >= 2 ~ "Moderate",
      TRUE ~ "Low"
    )
  )
```

---

## 14. VALIDATION PROTOCOLS

### 14.1 External Validation

#### 14.1.1 Expert Validation Panel

**Taxonomic Validation:**
- **Botanist verification:** Species identification confirmed by certified taxonomists
- **Herbarium cross-check:** Voucher specimens verified against type specimens
- **Molecular barcoding:** DNA analysis for morphologically similar species
- **Photo validation:** High-resolution images reviewed by regional flora experts

**Ecological Validation:**
- **Field ecologist review:** Habitat assessments verified by experienced ecologists
- **Local expert knowledge:** Traditional ecological knowledge integration
- **Literature comparison:** Results compared with similar studies in region
- **Statistical validation:** Methods reviewed by quantitative ecologists

#### 14.1.2 Pharmacological Data Validation

**Literature Quality Assessment:**
```r
# GRADE-like system for pharmacological evidence
assess_evidence_quality <- function(studies) {
  studies %>%
    mutate(
      study_design_score = case_when(
        study_type == "RCT" ~ 5,
        study_type == "Controlled trial" ~ 4,
        study_type == "Cohort" ~ 3,
        study_type == "Case-control" ~ 2,
        study_type == "Case series" ~ 1,
        TRUE ~ 0
      ),
      
      sample_size_score = case_when(
        sample_size >= 100 ~ 3,
        sample_size >= 50 ~ 2,
        sample_size >= 20 ~ 1,
        TRUE ~ 0
      ),
      
      methodology_score = case_when(
        methodology_quality == "High" ~ 3,
        methodology_quality == "Moderate" ~ 2,
        methodology_quality == "Low" ~ 1,
        TRUE ~ 0
      ),
      
      overall_quality = (study_design_score * 0.5 + 
                        sample_size_score * 0.25 + 
                        methodology_score * 0.25),
      
      evidence_level = case_when(
        overall_quality >= 4 ~ "High",
        overall_quality >= 3 ~ "Moderate",
        overall_quality >= 2 ~ "Low",
        TRUE ~ "Very Low"
      )
    )
}
```

### 14.2 Internal Validation

#### 14.2.1 Data Consistency Checks

**Automated Validation Rules:**
```r
# Data validation functions
validate_measurements <- function(data) {
  validation_results <- list()
  
  # DBH-Height relationship check
  validation_results$height_dbh <- check_height_dbh_relationship(data)
  
  # Volume calculation verification
  validation_results$volume <- verify_volume_calculations(data)
  
  # Species-plot consistency
  validation_results$species_consistency <- check_species_occurrence(data)
  
  # Temporal consistency
  validation_results$temporal <- check_measurement_dates(data)
  
  return(validation_results)
}

check_height_dbh_relationship <- function(data) {
  # Flag unrealistic height-DBH ratios
  data$height_dbh_ratio <- data$Height / data$DBH
  
  # Define reasonable bounds based on allometric equations
  lower_bound <- 0.5  # Very short, broad trees
  upper_bound <- 5.0  # Very tall, thin trees
  
  outliers <- which(data$height_dbh_ratio < lower_bound | 
                   data$height_dbh_ratio > upper_bound)
  
  return(list(
    n_outliers = length(outliers),
    outlier_indices = outliers,
    proportion_outliers = length(outliers) / nrow(data)
  ))
}
```

#### 14.2.2 Cross-Validation of Derived Metrics

**IVI Calculation Verification:**
```r
# Independent IVI calculation for verification
verify_ivi_calculation <- function(raw_data, calculated_ivi) {
  # Recalculate from raw data
  species_summary <- raw_data %>%
    group_by(Species) %>%
    summarise(
      individuals = n(),
      plots_present = n_distinct(Plot),
      total_basal_area = sum(pi * (DBH/2)^2, na.rm = TRUE)
    ) %>%
    mutate(
      # Relative values
      relative_density = (individuals / sum(individuals)) * 100,
      relative_frequency = (plots_present / max(raw_data$Plot)) * 100,
      relative_dominance = (total_basal_area / sum(total_basal_area)) * 100,
      
      # IVI calculation
      ivi_calculated = relative_density + relative_frequency + relative_dominance
    )
  
  # Compare with provided calculations
  comparison <- merge(species_summary, calculated_ivi, 
                     by = "Species", suffixes = c("_calc", "_provided"))
  
  # Calculate differences
  comparison$ivi_difference <- abs(comparison$ivi_calculated - 
                                  comparison$ivi_provided)
  
  return(comparison)
}
```

---

## 15. REPORTING AND DISSEMINATION

### 15.1 Scientific Publication Standards

#### 15.1.1 Manuscript Preparation Guidelines

**CONSORT-E Guidelines for Ecological Studies:**
- **Title:** Descriptive and informative, including key taxa and location
- **Abstract:** Structured format with background, methods, results, conclusions
- **Introduction:** Clear research questions and hypotheses
- **Methods:** Sufficient detail for replication
- **Results:** Objective presentation without interpretation
- **Discussion:** Interpretation in context of existing literature
- **Conclusion:** Clear statement of findings and implications

**Statistical Reporting Standards:**
```r
# Standardized reporting function
report_statistics <- function(test_result) {
  paste0(
    "Test statistic: ", round(test_result$statistic, 3),
    ", df = ", test_result$parameter,
    ", p = ", format_p_value(test_result$p.value),
    ", effect size = ", round(test_result$effect_size, 3),
    " (95% CI: ", round(test_result$conf.int[1], 3), "-",
    round(test_result$conf.int[2], 3), ")"
  )
}

format_p_value <- function(p) {
  if (p < 0.001) return("< 0.001")
  if (p < 0.01) return("< 0.01")
  if (p < 0.05) return("< 0.05")
  return(paste("=", round(p, 3)))
}
```

#### 15.1.2 Data and Code Availability

**Open Science Practices:**
```r
# Generate reproducible research compendium
create_research_compendium <- function(project_name) {
  # Directory structure
  dir.create(file.path(project_name, "data", "raw"), recursive = TRUE)
  dir.create(file.path(project_name, "data", "processed"), recursive = TRUE)
  dir.create(file.path(project_name, "scripts"), recursive = TRUE)
  dir.create(file.path(project_name, "output", "figures"), recursive = TRUE)
  dir.create(file.path(project_name, "output", "tables"), recursive = TRUE)
  dir.create(file.path(project_name, "docs"), recursive = TRUE)
  
  # Create README
  readme_content <- paste0(
    "# ", project_name, "\n\n",
    "## Overview\n",
    "Analysis of medicinal tree species in Pteropus vampyrus habitat\n\n",
    "## Data\n",
    "- raw/: Original field data and literature sources\n",
    "- processed/: Cleaned and analyzed datasets\n\n",
    "## Scripts\n",
    "- 01_data_cleaning.R: Data preprocessing\n",
    "- 02_statistical_analysis.R: Main analyses\n",
    "- 03_visualization.R: Figure generation\n\n",
    "## Output\n",
    "- figures/: Publication-ready figures\n",
    "- tables/: Summary statistics and model results\n\n",
    "## Reproducibility\n",
    "Run `make all` to reproduce all analyses\n"
  )
  
  writeLines(readme_content, file.path(project_name, "README.md"))
}
```

### 15.2 Knowledge Transfer and Outreach

#### 15.2.1 Stakeholder Communication

**Multi-audience Communication Strategy:**
- **Scientific community:** Peer-reviewed publications, conference presentations
- **Conservation practitioners:** Technical reports, workshops
- **Local communities:** Community meetings, educational materials
- **Policy makers:** Policy briefs, stakeholder consultations
- **General public:** Popular science articles, social media

**Communication Materials:**
```r
# Generate policy brief
create_policy_brief <- function(results) {
  brief_content <- paste0(
    "POLICY BRIEF: Conservation of Medicinal Trees in Flying Fox Habitat\n\n",
    "KEY FINDINGS:\n",
    "• ", nrow(results$species_data), " medicinal tree species identified\n",
    "• ", sum(results$species_data$status == "Rare"), " species require urgent protection\n",
    "• Sustainable harvesting possible for ", 
        sum(results$species_data$sustainable == "Yes"), " species\n\n",
    "RECOMMENDATIONS:\n",
    "1. Establish protected zones for rare species\n",
    "2. Develop sustainable harvesting guidelines\n",
    "3. Create community-based conservation programs\n",
    "4. Monitor Pteropus vampyrus populations\n\n",
    "ECONOMIC BENEFITS:\n",
    "• Estimated value of medicinal compounds: $X million\n",
    "• Potential ecotourism revenue: $Y per year\n",
    "• Job creation in sustainable harvesting: Z positions\n"
  )
  
  writeLines(brief_content, "policy_brief.txt")
}
```

#### 15.2.2 Educational Resources

**Field Guide Development:**
```r
# Generate species identification guide
create_field_guide <- function(species_data, photos) {
  guide_content <- species_data %>%
    arrange(Local_name) %>%
    group_split(Local_name) %>%
    map_chr(function(sp) {
      paste0(
        "## ", sp$Local_name[1], " (", sp$Scientific_name[1], ")\n\n",
        "**Family:** ", sp$Family[1], "\n",
        "**Status:** ", sp$Conservation_status[1], "\n",
        "**Identification:** ", sp$ID_features[1], "\n",
        "**Medicinal uses:** ", sp$Traditional_uses[1], "\n",
        "**Active compounds:** ", sp$Key_compounds[1], "\n",
        "**Conservation notes:** ", sp$Conservation_notes[1], "\n\n"
      )
    }) %>%
    paste(collapse = "\n")
  
  writeLines(guide_content, "field_guide.md")
}
```

### 15.3 Long-term Data Management

#### 15.3.1 Database Integration

**Institutional Repository Setup:**
```sql
-- Database schema for long-term data storage
CREATE TABLE species_master (
    species_id INT PRIMARY KEY,
    scientific_name VARCHAR(100) NOT NULL,
    local_name VARCHAR(100),
    family VARCHAR(50),
    conservation_status VARCHAR(20),
    last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE plot_data (
    plot_id INT PRIMARY KEY,
    latitude DECIMAL(10,8),
    longitude DECIMAL(11,8),
    elevation INT,
    sampling_date DATE,
    researcher_id VARCHAR(20)
);

CREATE TABLE tree_measurements (
    measurement_id INT PRIMARY KEY,
    plot_id INT REFERENCES plot_data(plot_id),
    species_id INT REFERENCES species_master(species_id),
    dbh DECIMAL(5,2),
    height DECIMAL(4,1),
    health_status VARCHAR(20),
    measurement_date DATE
);

CREATE TABLE pharmacological_data (
    compound_id INT PRIMARY KEY,
    species_id INT REFERENCES species_master(species_id),
    compound_name VARCHAR(100),
    activity_type VARCHAR(50),
    ic50_value DECIMAL(8,3),
    reference_doi VARCHAR(100),
    quality_score INT
);
```

#### 15.3.2 Metadata Standards

**Dublin Core Metadata:**
```xml
<?xml version="1.0" encoding="UTF-8"?>
<metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
    <dc:title>Medicinal Tree Species in Pteropus vampyrus Habitat</dc:title>
    <dc:creator>Research Team</dc:creator>
    <dc:subject>Medicinal plants; Flying foxes; Biodiversity; Conservation</dc:subject>
    <dc:description>Comprehensive dataset of medicinal tree species populations and pharmacological properties in flying fox habitat</dc:description>
    <dc:publisher>University Research Institute</dc:publisher>
    <dc:date>2024</dc:date>
    <dc:type>Dataset</dc:type>
    <dc:format>CSV, Excel, R data files</dc:format>
    <dc:identifier>DOI:10.xxxx/xxxxx</dc:identifier>
    <dc:source>Field surveys, Literature review</dc:source>
    <dc:language>en</dc:language>
    <dc:coverage>Situ Lengkong Panjalu, West Java, Indonesia</dc:coverage>
    <dc:rights>CC BY 4.0</dc:rights>
</metadata>
```

---

## 16. CONCLUSION

This comprehensive methodology provides a robust, reproducible, and scientifically rigorous framework for investigating medicinal tree species in *Pteropus vampyrus* habitat. The integration of field ecology, pharmacological literature review, advanced statistical analyses, and computational methods ensures high-quality research outputs that can inform both scientific understanding and conservation practice.

**Key methodological innovations include:**

1. **Integrated approach** combining field surveys with literature-based pharmacological data
2. **Robust statistical framework** with appropriate handling of small sample sizes and non-normal data
3. **Advanced computational methods** including machine learning and network analysis
4. **Comprehensive validation protocols** ensuring data quality and reliability
5. **Open science practices** promoting reproducibility and knowledge sharing
6. **Multi-stakeholder communication** facilitating knowledge transfer and practical application

**Expected outcomes:**
- High-impact scientific publications in peer-reviewed journals
- Evidence-based conservation recommendations
- Sustainable management guidelines for medicinal plant resources
- Enhanced understanding of plant-animal interactions in tropical ecosystems
- Capacity building in local communities and institutions

This methodology serves as a template for similar interdisciplinary studies integrating ecology, pharmacology, and conservation biology, contributing to the broader goals of biodiversity conservation