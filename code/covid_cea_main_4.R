# Calculate the total QALYs lost and total cost under no mittigation (cmmid) and observed scenarios

# V4 Uses a single spreadsheet for all inputs

library(readxl)
library(writexl)


# Function to run decision tree model
source("code/generate_costs_qalys_3.R")

country_names <- c("UK", "Ireland", "Spain", "Germany", "Sweden")

# Global input parameters and settings. Includes costs and QALYs for decision tree model
global_inputs <- as.data.frame(read_excel("data/covid_cea_input_data.xlsx", sheet = "global_inputs"))
rownames(global_inputs) <- global_inputs[, "Parameter"]

# OECD PPP 2019 accessed 5th October 2020
# https://data.oecd.org/conversion/purchasing-power-parities-ppp.htm 
dollar_to_gbp <- global_inputs["dollar_to_gdp", "Value"] # 0.680
# Components of costs and QALYs to calculate
subcalculation_names <- c("all_impacts", "cases_only", "hospitalisation_only", "death_only")

# Standardised mortality ratio
smr <- global_inputs["smr", "Value"] #0.111*1.6+(1-0.111)*1
# Discounting
discount_rate <- global_inputs["discount_rate", "Value"]

# Quality of life modifier for above standardised mortality ratio
qCM <- global_inputs["qCM", "Value"]


# UK proportion female (may have to use this for all countries)
prop_female <- global_inputs["prop_female", "Value"]

# QALY inputs


# Reported cases
cases_raw <- as.data.frame(read_excel("data/covid_cea_input_data.xlsx", sheet = "total_cases"))
cases_mitigation <- cases_raw[, "Cases by July 20th"]
names(cases_mitigation) <- cases_raw[, "Country"]

# Reported cases
hospitalisations_raw <- as.data.frame(read_excel("data/covid_cea_input_data.xlsx", sheet = "total_hospitalisations"))
hospitalisations_mitigation <- hospitalisations_raw[, "Hospitalisations"]
hospitalisations_mitigation <- as.numeric(unlist(lapply(hospitalisations_mitigation, toString)))
names(hospitalisations_mitigation) <- hospitalisations_raw[, "...1"]

# Reported deaths
deaths_raw <- as.data.frame(read_excel("data/covid_cea_input_data.xlsx", sheet = "total_deaths"))
deaths_mitigation <- deaths_raw[, "Total covid deaths"]
names(deaths_mitigation) <- deaths_raw[, "Country"]

# Set Sweden's hospitalisations to be cases * ratio observed across all other countries

observed_hospitalisation_ratio <- sum(hospitalisations_mitigation, na.rm = TRUE) / sum(cases_mitigation[country_names])
hospitalisations_mitigation["Sweden"] <- observed_hospitalisation_ratio * cases_mitigation["Sweden"]
country_level_hospitalisation_ratio <- hospitalisations_mitigation / cases_mitigation[country_names]

dQALYS_lost_table <- matrix(nrow = 4, ncol = 5)
rownames(dQALYS_lost_table) <- c("Mitigation", "A", "B", "C")

# ONS lifetables for males and females in UK
lifetable_male_uk <- as.data.frame(read_excel("data/covid_cea_input_data.xlsx", sheet = "uk_lifetable_male"))
lifetable_female_uk <- as.data.frame(read_excel("data/covid_cea_input_data.xlsx", sheet = "uk_lifetable_female"))
# WHO data Probability of dying between year x and x+1 for each gender and each country
who_lifetables_qx <- as.data.frame(read_excel("data/covid_cea_input_data.xlsx", sheet = "who_lifetables"))

# UK relevant QOL data but this will be applied to all countries
qol_norm_data <- as.data.frame(read_excel("data/covid_cea_input_data.xlsx", sheet = "qol_norm_data"))

# Age at death distribution
age_death_distribution <- as.data.frame(read_excel("data/covid_cea_input_data.xlsx", sheet = "age_death"))
# remove the row with totals
age_death_distribution <- age_death_distribution[-11,]




for(cmmid_simulation in c("A", "B", "C")) {

  source("code/calculate_qalys_4.R")
  dQALYS_lost_table["Mitigation", ] <- dQALYs_lost
  dQALYS_lost_table[cmmid_simulation, ] <- dQALYs_lost_cmmid

  covid_cea_table <- list()
  

  # Deaths modelled by the cmmid no mitigation scenario
  cmmid_deaths <- t(cmmid_raw[, grep("Deaths", colnames(cmmid_raw))])
  colnames(cmmid_deaths) <- c(unlist(lapply(cmmid_raw[, "Country"], toString)))
  cmmid_deaths <- colSums(cmmid_deaths)
  
  # Hospitalisations modelled by the cmmid no mitigation scenario
  cmmid_mean_hospital_stay <- global_inputs["cmmid_mean_hospital_stay", "Value"]
  cmmid_mean_icu_stay <- global_inputs["cmmid_mean_icu_stay", "Value"]
  cmmid_hospitalisations <- cmmid_raw[, "CMMID Non-ICU bed days"] / cmmid_mean_hospital_stay
  cmmid_icu <- cmmid_raw[, "CMMID Non-ICU bed days"] / cmmid_mean_icu_stay
  names(cmmid_hospitalisations) <- names(cmmid_icu) <- c(unlist(lapply(cmmid_raw[, "Country"], toString)))
  
  # Cases modelled by the cmmid no mitigation scenario
  cmmid_cases <- cmmid_raw[, "CMMID Total cases 20 July"]
  names(cmmid_cases) <- c(unlist(lapply(cmmid_raw[, "Country"], toString)))
  
  # No mitigation scenario entirely follows cmmid projections
  deaths_nomitigation <- cmmid_deaths
  hospitalisations_nomitigation <- cmmid_hospitalisations
  icu_nomitigation <- cmmid_icu
  cases_nomitigation <- cmmid_cases
  
  # Use cmmid model to estimate proportion hospitalised
  cmmid_hospitalisation_ratio <- (cmmid_hospitalisations + cmmid_icu)/cmmid_cases
  
  # For comparison include GPD impact
  imf_gdp_loss <- as.data.frame(read_excel("data/covid_cea_input_data.xlsx", sheet = "gdp_impact"))
  population_size <- imf_gdp_loss[, c("Country", "Population size")]
  imf_gdp_loss <- imf_gdp_loss[, c("Country", "IMF GDP loss")]
  rownames(imf_gdp_loss) <- rownames(population_size) <- imf_gdp_loss[, "Country"]
  imf_gdp_loss[, "IMF GDP loss"] <- imf_gdp_loss[, "IMF GDP loss"] * dollar_to_gbp
  # Remove Korea
  imf_gdp_loss <- imf_gdp_loss[-which(rownames(imf_gdp_loss) == "Korea"), ]
  population_size <- population_size[-which(rownames(population_size) == "Korea"), ]
  
  for(subcalculation_name in subcalculation_names) {
    covid_cea_table[[subcalculation_name]] <- matrix(NA, nrow = length(country_names), ncol = 13)
    colnames(covid_cea_table[[subcalculation_name]]) <- c("dQALYs lost mitigation", "Costs mitigation", "dQALYs lost no mitigation", "Costs no mitigation",
                                                          "Incremental QALYs", "Incremental Costs", "ICER", 
                                                          "Incremental net benefit at ?20k (?billion)", "Incremental net benefit at ?30k (?billion)", 
                                                          "GDP Loss (?billion)",
                                                          "Incremental net benefit at ?20k PP", "Incremental net benefit at ?30k PP", 
                                                          "GDP Loss PP")
    rownames(covid_cea_table[[subcalculation_name]]) <- country_names
    
    for(country_name in country_names) {
      temp <- generate_costs_qalys(n_deaths = deaths_mitigation[country_name] , n_cases = cases_mitigation[country_name], 
                                   n_hospitalisations = hospitalisations_mitigation[country_name],
                                   dQALYs_lost = dQALYs_lost_cmmid,
                                   subcalculation_name = subcalculation_name)
      
      covid_cea_table[[subcalculation_name]][country_name, c("dQALYs lost mitigation", "Costs mitigation")] <- 
        c(temp$total_qalyloss, temp$total_cost)
      
      temp <- generate_costs_qalys(n_deaths = deaths_nomitigation[country_name] , n_cases = cases_nomitigation[country_name], 
                                   n_hospitalisations = hospitalisations_nomitigation[country_name],
                                   n_icu = icu_nomitigation[country_name],
                                   dQALYs_lost = dQALYs_lost_cmmid,
                                   subcalculation_name = subcalculation_name)
      
      covid_cea_table[[subcalculation_name]][country_name, c("dQALYs lost no mitigation", "Costs no mitigation")] <- 
        c(temp$total_qalyloss, temp$total_cost)
      
      covid_cea_table[[subcalculation_name]][country_name, "GDP Loss (?billion)"] <- 
        imf_gdp_loss[country_name, "IMF GDP loss"]
      
      # Per person
      covid_cea_table[[subcalculation_name]][country_name, "GDP Loss PP"] <- 
        1000000000 * covid_cea_table[[subcalculation_name]][country_name, "GDP Loss (?billion)"] / 
        population_size[country_name, "Population size"]
    }
    
    # Greater incremental QALYs means greater QALYS saved
    covid_cea_table[[subcalculation_name]][, "Incremental QALYs"] <- covid_cea_table[[subcalculation_name]][,"dQALYs lost mitigation"] -
      covid_cea_table[[subcalculation_name]][,"dQALYs lost no mitigation"]
    # More negative indicates greater savings
    covid_cea_table[[subcalculation_name]][, "Incremental Costs"] <- covid_cea_table[[subcalculation_name]][,"Costs mitigation"] -
      covid_cea_table[[subcalculation_name]][,"Costs no mitigation"]
    
    covid_cea_table[[subcalculation_name]][, "ICER"] <- covid_cea_table[[subcalculation_name]][, "Incremental Costs"] / covid_cea_table[[subcalculation_name]][, "Incremental QALYs"]
    # Total savings with QALYs valued at 20000
    # Compare to economic loss?
    covid_cea_table[[subcalculation_name]][, "Incremental net benefit at ?20k (?billion)"] <- (20000 *  covid_cea_table[[subcalculation_name]][, "Incremental QALYs"] - covid_cea_table[[subcalculation_name]][, "Incremental Costs"]) / 1000000000
    covid_cea_table[[subcalculation_name]][, "Incremental net benefit at ?30k (?billion)"] <- (30000 *  covid_cea_table[[subcalculation_name]][, "Incremental QALYs"] - covid_cea_table[[subcalculation_name]][, "Incremental Costs"]) / 1000000000
    # Per person
    covid_cea_table[[subcalculation_name]][, "Incremental net benefit at ?30k PP"] <- 1000000000 * covid_cea_table[[subcalculation_name]][, "Incremental net benefit at ?30k (?billion)"] / population_size[, "Population size"]
    covid_cea_table[[subcalculation_name]][, "Incremental net benefit at ?20k PP"] <- 1000000000 * covid_cea_table[[subcalculation_name]][, "Incremental net benefit at ?20k (?billion)"] / population_size[, "Population size"]
    
    # Rescale to millions of QALYs and billions of ?
    colnames(covid_cea_table[[subcalculation_name]]) <- c("dQALYs lost mitigation (million)", "Costs mitigation (?billion)", "dQALYs lost no mitigation (million)", "Costs no mitigation (?billion)",
                                                          "Incremental QALYs (million)", "Incremental Costs (?billion)", "ICER", 
                                                          "Incremental net benefit at ?20k (?billion)", "Incremental net benefit at ?30k (?billion)", 
                                                          "GDP Loss (?billion)",
                                                          "Incremental net benefit at ?20k PP", "Incremental net benefit at ?30k PP", 
                                                          "GDP Loss PP")
    
    covid_cea_table[[subcalculation_name]][, c("dQALYs lost mitigation (million)", "dQALYs lost no mitigation (million)",
                                               "Incremental QALYs (million)")] <- covid_cea_table[[subcalculation_name]][, c("dQALYs lost mitigation (million)", "dQALYs lost no mitigation (million)",
                                                                                                                             "Incremental QALYs (million)")] / 1000000
    
    covid_cea_table[[subcalculation_name]][, c("Costs mitigation (?billion)",  "Costs no mitigation (?billion)",
                                               "Incremental Costs (?billion)")] <-  covid_cea_table[[subcalculation_name]][, c("Costs mitigation (?billion)",  "Costs no mitigation (?billion)",
                                                                                                                               "Incremental Costs (?billion)")] / 1000000000
  }
  
  
  write.xlsx("header sheet", file = paste0("results/covid_cea_table_", cmmid_simulation, "_smr", smr, ".xlsx"), sheetName = "header", append = FALSE)  
  for(subcalculation_name in subcalculation_names) {
    write.xlsx(covid_cea_table[[subcalculation_name]], file = paste0("results/covid_cea_table_", cmmid_simulation, "_smr", smr, ".xlsx"), sheetName = subcalculation_name, append = TRUE)  
  }
  
  
} # End loop over cmmid 

# Export the QALYs lost per death under each scenario
colnames(dQALYS_lost_table) <- names(dQALYs_lost)
write.csv(dQALYS_lost_table, file = "results/qalys_lost_per_death.csv")