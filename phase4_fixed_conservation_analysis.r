# =============================================================================
# PHASE 4: CONSERVATION ANALYSIS (FIXED SIMPLE VERSION)
# Medicinal Tree Species Analysis - Pteropus vampyrus Habitat
# =============================================================================

# Clear environment and set up
rm(list = ls())
cat("=============================================================================\n")
cat("PHASE 4: CONSERVATION ANALYSIS (FIXED VERSION)\n")
cat("Medicinal Tree Species Analysis - Pteropus vampyrus Habitat\n")
cat("=============================================================================\n\n")

# =============================================================================
# STEP 4.0: PACKAGE SETUP
# =============================================================================

cat("STEP 4.0: PACKAGE SETUP\n")
cat(strrep("-", 50), "\n")

# Essential packages only
essential_packages <- c("readr", "dplyr", "ggplot2")

for (pkg in essential_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
    cat("✅", pkg, "installed\n")
  } else {
    cat("✅", pkg, "available\n")
  }
}

cat("\n✅ Package setup completed\n\n")

# =============================================================================
# STEP 4.1: LOAD PHASE 3 DATA
# =============================================================================

cat("STEP 4.1: LOAD PHASE 3 DATA\n")
cat(strrep("-", 50), "\n")

# Load required data
tryCatch({
  priority_data <- read_csv("output/phase3_priority_assessment.csv", show_col_types = FALSE)
  
  cat("✅ Phase 3 data loaded successfully:\n")
  cat("   - Priority assessment:", nrow(priority_data), "species\n\n")
  
}, error = function(e) {
  cat("❌ ERROR loading Phase 3 data:", e$message, "\n")
  stop("Cannot proceed without Phase 3 data")
})

# =============================================================================
# STEP 4.2: THREAT ASSESSMENT
# =============================================================================

cat("STEP 4.2: THREAT ASSESSMENT\n")
cat(strrep("-", 50), "\n")

# Simple threat assessment
threat_data <- priority_data %>%
  mutate(
    # Population vulnerability (0-5 scale)
    Population_Vulnerability = case_when(
      Individuals == 0 ~ 5,
      Individuals < 5 ~ 5,
      Individuals < 10 ~ 4,
      Individuals < 20 ~ 3,
      Individuals < 50 ~ 2,
      TRUE ~ 1
    ),
    
    # Habitat threat (based on priority category)
    Habitat_Threat = case_when(
      Priority_Category == "CRITICAL" ~ 5,
      Priority_Category == "HIGH" ~ 4,
      Priority_Category == "MEDIUM" ~ 3,
      Priority_Category == "LOW" ~ 2,
      TRUE ~ 1
    ),
    
    # Overharvesting risk
    Harvest_Risk = case_when(
      Individuals < 10 & Priority_Category %in% c("CRITICAL", "HIGH") ~ 5,
      Individuals < 20 & Priority_Category %in% c("CRITICAL", "HIGH") ~ 4,
      Individuals < 30 ~ 3,
      TRUE ~ 2
    ),
    
    # Overall threat score
    Overall_Threat_Score = (Population_Vulnerability * 0.4 + 
                           Habitat_Threat * 0.35 + 
                           Harvest_Risk * 0.25),
    
    # Threat level
    Threat_Level = case_when(
      Overall_Threat_Score >= 4.5 ~ "EXTREME",
      Overall_Threat_Score >= 3.5 ~ "HIGH",
      Overall_Threat_Score >= 2.5 ~ "MEDIUM",
      Overall_Threat_Score >= 1.5 ~ "LOW",
      TRUE ~ "MINIMAL"
    )
  ) %>%
  arrange(desc(Overall_Threat_Score))

cat("🚨 THREAT ASSESSMENT COMPLETED:\n")
cat("\n📊 THREAT LEVEL DISTRIBUTION:\n")

threat_summary <- threat_data %>% count(Threat_Level)
for (i in 1:nrow(threat_summary)) {
  cat("   ", threat_summary$Threat_Level[i], ":", threat_summary$n[i], "species\n")
}

cat("\n🔥 TOP 5 MOST THREATENED SPECIES:\n")
cat(sprintf("%-15s | %8s | %8s | %-10s\n", "Local Name", "Pop", "Threat", "Level"))
cat(strrep("-", 50), "\n")

top_threats <- head(threat_data, 5)
for (i in 1:nrow(top_threats)) {
  cat(sprintf("%-15s | %8d | %8.2f | %-10s\n",
             top_threats$Local_Name[i],
             top_threats$Individuals[i],
             top_threats$Overall_Threat_Score[i],
             top_threats$Threat_Level[i]))
}

# =============================================================================
# STEP 4.3: POPULATION VIABILITY ANALYSIS
# =============================================================================

cat("\nSTEP 4.3: POPULATION VIABILITY ANALYSIS\n")
cat(strrep("-", 50), "\n")

# Simple PVA
pva_data <- threat_data %>%
  mutate(
    # Growth rate estimate
    Growth_Rate = case_when(
      Individuals >= 30 ~ 0.05,
      Individuals >= 15 ~ 0.03,
      Individuals >= 5 ~ 0.01,
      TRUE ~ -0.02
    ),
    
    # Minimum viable population
    MVP_Estimate = case_when(
      Threat_Level %in% c("EXTREME", "HIGH") ~ 100,
      Threat_Level == "MEDIUM" ~ 75,
      TRUE ~ 50
    ),
    
    # Time to extinction (simplified)
    Time_to_Extinction = case_when(
      Individuals == 0 ~ 0,
      Growth_Rate <= 0 ~ pmax(10, 50 / (abs(Growth_Rate) * 10 + 1)),
      Individuals < 10 ~ 50,
      TRUE ~ 100
    ),
    
    # Viability status
    Viability_Status = case_when(
      Individuals == 0 ~ "EXTINCT",
      Individuals < MVP_Estimate * 0.1 ~ "NON-VIABLE",
      Individuals < MVP_Estimate * 0.3 ~ "CRITICALLY LOW",
      Individuals < MVP_Estimate * 0.6 ~ "BELOW MVP",
      Individuals < MVP_Estimate ~ "APPROACHING MVP",
      TRUE ~ "VIABLE"
    ),
    
    # Conservation urgency
    Conservation_Urgency = case_when(
      Time_to_Extinction <= 10 & Individuals > 0 ~ "IMMEDIATE",
      Time_to_Extinction <= 25 ~ "URGENT",
      Viability_Status %in% c("NON-VIABLE", "CRITICALLY LOW") ~ "HIGH",
      TRUE ~ "MODERATE"
    )
  )

cat("📈 POPULATION VIABILITY ANALYSIS COMPLETED:\n")

viability_summary <- pva_data %>% count(Viability_Status)
cat("\n📊 VIABILITY STATUS:\n")
for (i in 1:nrow(viability_summary)) {
  cat("   ", viability_summary$Viability_Status[i], ":", viability_summary$n[i], "species\n")
}

urgency_summary <- pva_data %>% count(Conservation_Urgency)
cat("\n⏰ CONSERVATION URGENCY:\n")
for (i in 1:nrow(urgency_summary)) {
  cat("   ", urgency_summary$Conservation_Urgency[i], ":", urgency_summary$n[i], "species\n")
}

# =============================================================================
# STEP 4.4: CONSERVATION ACTION PLANS
# =============================================================================

cat("\nSTEP 4.4: CONSERVATION ACTION PLANS\n")
cat(strrep("-", 50), "\n")

# Develop action plans
action_plans <- pva_data %>%
  mutate(
    # Implementation priority
    Implementation_Priority = case_when(
      Conservation_Urgency == "IMMEDIATE" ~ 1,
      Conservation_Urgency == "URGENT" ~ 2,
      Threat_Level == "EXTREME" ~ 2,
      Conservation_Urgency == "HIGH" ~ 3,
      TRUE ~ 4
    ),
    
    # Immediate actions
    Immediate_Actions = case_when(
      Conservation_Urgency == "IMMEDIATE" ~ "Emergency collection, ex-situ conservation",
      Conservation_Urgency == "URGENT" ~ "Intensive monitoring, habitat protection",
      Threat_Level == "EXTREME" ~ "Population reinforcement, threat mitigation",
      TRUE ~ "Regular monitoring, basic protection"
    ),
    
    # Monitoring frequency
    Monitoring_Frequency = case_when(
      Implementation_Priority == 1 ~ "Monthly",
      Implementation_Priority == 2 ~ "Quarterly",
      Implementation_Priority == 3 ~ "Bi-annual",
      TRUE ~ "Annual"
    )
  ) %>%
  arrange(Implementation_Priority, desc(Overall_Threat_Score))

cat("📋 CONSERVATION ACTION PLANS DEVELOPED:\n")

priority_summary <- action_plans %>% count(Implementation_Priority)
cat("   Implementation priorities:\n")
for (i in 1:nrow(priority_summary)) {
  priority_name <- case_when(
    priority_summary$Implementation_Priority[i] == 1 ~ "IMMEDIATE",
    priority_summary$Implementation_Priority[i] == 2 ~ "URGENT", 
    priority_summary$Implementation_Priority[i] == 3 ~ "HIGH",
    TRUE ~ "MODERATE"
  )
  cat("     Priority", priority_summary$Implementation_Priority[i], "(", priority_name, "):", 
      priority_summary$n[i], "species\n")
}

# =============================================================================
# STEP 4.5: ECONOMIC ANALYSIS
# =============================================================================

cat("\nSTEP 4.5: ECONOMIC ANALYSIS\n")
cat(strrep("-", 50), "\n")

# Simple economic valuation
economic_data <- action_plans %>%
  mutate(
    # Value per individual (USD)
    Value_Per_Individual = case_when(
      Evidence_Quality >= 4 & Therapeutic_Potential >= 4 ~ 500,
      Evidence_Quality >= 3 & Therapeutic_Potential >= 3 ~ 300,
      TRUE ~ 150
    ),
    
    # Population value
    Population_Value = Value_Per_Individual * Individuals,
    
    # Annual conservation cost
    Conservation_Cost = case_when(
      Implementation_Priority == 1 ~ 15000,
      Implementation_Priority == 2 ~ 10000,
      Implementation_Priority == 3 ~ 5000,
      TRUE ~ 2000
    ),
    
    # Benefit-cost ratio (10-year timeframe)
    BCR_10year = Population_Value / (Conservation_Cost * 10),
    
    # Economic priority
    Economic_Priority = case_when(
      BCR_10year >= 3 ~ "HIGH",
      BCR_10year >= 1.5 ~ "MEDIUM",
      BCR_10year >= 0.8 ~ "LOW",
      TRUE ~ "VERY LOW"
    )
  )

cat("💰 ECONOMIC ANALYSIS COMPLETED:\n")

total_value <- sum(economic_data$Population_Value)
total_cost <- sum(economic_data$Conservation_Cost)
cat("   Total population value: $", format(total_value, big.mark = ","), "\n")
cat("   Total annual cost: $", format(total_cost, big.mark = ","), "\n")
cat("   Overall BCR:", round(total_value / (total_cost * 10), 2), "\n")

# =============================================================================
# STEP 4.6: EXPORT RESULTS
# =============================================================================

cat("\nSTEP 4.6: EXPORT RESULTS\n")
cat(strrep("-", 50), "\n")

# Export main results
write_csv(threat_data, "output/phase4_threat_assessment.csv")
write_csv(pva_data, "output/phase4_population_viability.csv") 
write_csv(action_plans, "output/phase4_conservation_action_plans.csv")
write_csv(economic_data, "output/phase4_economic_analysis.csv")

# Summary tables
conservation_summary <- action_plans %>%
  select(Scientific_Name, Local_Name, Individuals, Threat_Level, 
         Viability_Status, Conservation_Urgency, Implementation_Priority) %>%
  arrange(Implementation_Priority)

economic_summary <- economic_data %>%
  select(Scientific_Name, Local_Name, Population_Value, Conservation_Cost, 
         BCR_10year, Economic_Priority) %>%
  arrange(desc(BCR_10year))

write_csv(conservation_summary, "output/phase4_conservation_summary.csv")
write_csv(economic_summary, "output/phase4_economic_summary.csv")

cat("💾 Phase 4 results exported:\n")
cat("   - phase4_threat_assessment.csv\n")
cat("   - phase4_population_viability.csv\n")
cat("   - phase4_conservation_action_plans.csv\n")
cat("   - phase4_economic_analysis.csv\n")
cat("   - phase4_conservation_summary.csv\n")
cat("   - phase4_economic_summary.csv\n")

# =============================================================================
# STEP 4.7: CREATE SIMPLE VISUALIZATIONS  
# =============================================================================

cat("\nSTEP 4.7: CREATE VISUALIZATIONS\n")
cat(strrep("-", 50), "\n")

# 1. Threat vs Population
tryCatch({
  p1 <- ggplot(threat_data, aes(x = Individuals + 1, y = Overall_Threat_Score)) +
    geom_point(aes(color = Threat_Level), size = 3, alpha = 0.7) +
    geom_text(aes(label = Local_Name), hjust = 1.1, size = 3) +
    scale_color_manual(values = c("EXTREME" = "red", "HIGH" = "orange",
                                 "MEDIUM" = "yellow", "LOW" = "lightgreen", 
                                 "MINIMAL" = "green")) +
    scale_x_log10() +
    labs(title = "Threat Assessment Matrix", 
         x = "Population Size (log)", y = "Threat Score") +
    theme_minimal()
  
  ggsave("output/phase4_threat_matrix.png", p1, width = 10, height = 6, dpi = 300)
  cat("   ✅ phase4_threat_matrix.png created\n")
}, error = function(e) {
  cat("   ⚠️ Error creating threat chart\n")
})

# 2. Economic Analysis
tryCatch({
  p2 <- ggplot(economic_data, aes(x = Conservation_Cost, y = Population_Value)) +
    geom_point(aes(color = Economic_Priority), size = 3, alpha = 0.7) +
    geom_text(aes(label = Local_Name), hjust = 1.1, size = 3) +
    scale_color_manual(values = c("HIGH" = "green", "MEDIUM" = "yellow",
                                 "LOW" = "orange", "VERY LOW" = "red")) +
    labs(title = "Economic Cost-Benefit Analysis",
         x = "Annual Conservation Cost ($)", y = "Population Value ($)") +
    theme_minimal()
  
  ggsave("output/phase4_economic_analysis.png", p2, width = 10, height = 6, dpi = 300)
  cat("   ✅ phase4_economic_analysis.png created\n")
}, error = function(e) {
  cat("   ⚠️ Error creating economic chart\n")
})

# =============================================================================
# STEP 4.8: FINAL SUMMARY
# =============================================================================

cat("\nSTEP 4.8: FINAL SUMMARY\n")
cat(strrep("-", 50), "\n")

# Create final summary
final_summary <- data.frame(
  Metric = c("Total Species Analyzed", "Species with Extreme Threats", "Non-Viable Populations",
            "Species Requiring Immediate Action", "Total Economic Value", "Total Conservation Cost",
            "High Economic Priority Species"),
  Value = c(nrow(threat_data),
           sum(threat_data$Threat_Level == "EXTREME"),
           sum(pva_data$Viability_Status == "NON-VIABLE"),
           sum(action_plans$Implementation_Priority == 1),
           paste0("$", format(total_value, big.mark = ",")),
           paste0("$", format(total_cost, big.mark = ",")),
           sum(economic_data$Economic_Priority == "HIGH"))
)

write_csv(final_summary, "output/phase4_final_summary.csv")

cat("📋 PHASE 4 FINAL SUMMARY:\n")
for (i in 1:nrow(final_summary)) {
  cat("   ", final_summary$Metric[i], ":", final_summary$Value[i], "\n")
}

# =============================================================================
# PHASE 4 COMPLETION
# =============================================================================

cat("\n", strrep("=", 75), "\n")
cat("✅ PHASE 4 COMPLETED SUCCESSFULLY\n")
cat(strrep("=", 75), "\n")

cat("\n🎯 KEY ACHIEVEMENTS:\n")
cat("   • Threat assessment for", nrow(threat_data), "species\n")
cat("   • Population viability analysis completed\n")
cat("   • Conservation action plans developed\n")
cat("   • Economic cost-benefit analysis finished\n")
cat("   • Implementation priorities established\n")

cat("\n📊 CONSERVATION PRIORITIES:\n")
cat("   • Extreme threat species:", sum(threat_data$Threat_Level == "EXTREME"), "\n")
cat("   • Immediate action needed:", sum(action_plans$Implementation_Priority == 1), "\n")
cat("   • High economic priority:", sum(economic_data$Economic_Priority == "HIGH"), "\n")

cat("\n🌟 RESEARCH PIPELINE COMPLETE:\n")
cat("   ✅ Phase 1: Data Validation & Standardization\n")
cat("   ✅ Phase 2: Ecological Analysis & Population Structure\n")
cat("   ✅ Phase 3: Pharmacological Integration & Priority Assessment\n")
cat("   ✅ Phase 4: Conservation Analysis & Implementation Framework\n")

cat("\n🚀 READY FOR IMPLEMENTATION!\n")
cat("Conservation framework complete and ready for action\n")

cat("\n", strrep("=", 75), "\n")
cat("Phase 4 Analysis Complete - ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 75), "\n")