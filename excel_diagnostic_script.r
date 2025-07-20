# =============================================================================
# EXCEL FILE DIAGNOSTIC SCRIPT
# Quick examination of Excel file structure
# =============================================================================

# Load required library
library(readxl)

# Clear console
cat("\n", strrep("=", 60), "\n")
cat("EXCEL FILE DIAGNOSTIC TOOL\n")
cat(strrep("=", 60), "\n\n")

# Check if file exists
excel_file <- "analisis pohon 1.xlsx"

if (!file.exists(excel_file)) {
  cat("❌ ERROR: File 'analisis pohon 1.xlsx' not found!\n")
  cat("Current working directory:", getwd(), "\n\n")
  
  cat("📁 Files in current directory:\n")
  all_files <- list.files()
  if (length(all_files) > 0) {
    for (f in all_files) {
      cat("   -", f, "\n")
    }
  } else {
    cat("   (No files found)\n")
  }
  
  cat("\n📋 Excel/CSV files specifically:\n")
  excel_files <- list.files(pattern = "\\.(xlsx|xls|csv)$", ignore.case = TRUE)
  if (length(excel_files) > 0) {
    for (f in excel_files) {
      cat("   -", f, "\n")
    }
  } else {
    cat("   (No Excel/CSV files found)\n")
  }
  
  stop("Please ensure the Excel file is in the correct directory")
}

cat("✅ File found:", excel_file, "\n\n")

# Get sheet information
tryCatch({
  sheet_names <- excel_sheets(excel_file)
  cat("📊 SHEET INFORMATION:\n")
  cat("   Number of sheets:", length(sheet_names), "\n")
  for (i in 1:length(sheet_names)) {
    cat("   Sheet", i, ":", sheet_names[i], "\n")
  }
  cat("\n")
}, error = function(e) {
  cat("❌ Error reading sheet names:", e$message, "\n")
  stop("Cannot access Excel file")
})

# Examine each sheet
for (i in 1:length(sheet_names)) {
  cat(strrep("-", 50), "\n")
  cat("EXAMINING SHEET", i, ":", sheet_names[i], "\n")
  cat(strrep("-", 50), "\n")
  
  tryCatch({
    # Read the sheet
    sheet_data <- read_excel(excel_file, sheet = i)
    
    cat("📏 DIMENSIONS:\n")
    cat("   Rows:", nrow(sheet_data), "\n")
    cat("   Columns:", ncol(sheet_data), "\n\n")
    
    cat("📋 COLUMN NAMES:\n")
    for (j in 1:ncol(sheet_data)) {
      col_name <- names(sheet_data)[j]
      if (is.na(col_name) || col_name == "" || col_name == "...1") {
        col_name <- paste0("[UNNAMED_", j, "]")
      }
      cat("   ", j, ":", col_name, "\n")
    }
    
    cat("\n🔍 FIRST 3 ROWS:\n")
    print(head(sheet_data, 3))
    
    cat("\n📊 COLUMN TYPES:\n")
    col_types <- sapply(sheet_data, class)
    for (j in 1:length(col_types)) {
      cat("   ", names(sheet_data)[j], ":", col_types[j], "\n")
    }
    
    # Check for potential species column
    cat("\n🌳 SPECIES DETECTION:\n")
    species_found <- FALSE
    for (j in 1:ncol(sheet_data)) {
      if (is.character(sheet_data[[j]])) {
        # Look for plant names
        plant_patterns <- c("Ki ", "Huru", "Pisitan", "Campaka", "Teureup", "Hantap")
        matches <- sum(sapply(plant_patterns, function(p) 
          sum(grepl(p, sheet_data[[j]], ignore.case = TRUE, na.rm = TRUE))))
        
        if (matches > 0) {
          cat("   Potential species column found: Column", j, "-", names(sheet_data)[j], "\n")
          cat("   Plant name patterns found:", matches, "\n")
          species_found <- TRUE
          
          # Show unique values
          unique_vals <- unique(sheet_data[[j]][!is.na(sheet_data[[j]])])
          cat("   Unique values in this column:\n")
          for (val in unique_vals[1:min(10, length(unique_vals))]) {
            cat("     -", val, "\n")
          }
          if (length(unique_vals) > 10) {
            cat("     ... and", length(unique_vals) - 10, "more\n")
          }
        }
      }
    }
    
    if (!species_found) {
      cat("   ⚠️ No obvious species column detected\n")
    }
    
    cat("\n")
    
  }, error = function(e) {
    cat("❌ Error reading sheet", i, ":", e$message, "\n\n")
  })
}

cat(strrep("=", 60), "\n")
cat("DIAGNOSTIC COMPLETE\n")
cat("Now you can run the main Phase 1 script\n")
cat(strrep("=", 60), "\n")