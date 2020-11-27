# Calculate the total QALYs lost and total cost under no mittigation (cmmid) and observed scenarios

# V4 Uses a single spreadsheet for all inputs

library(readxl)
library(writexl)
library(openxlsx)
library(tidyverse)
library(ggplot2)



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
smr <- 2.0
# Discounting
discount_rate <- global_inputs["discount_rate", "Value"]

# Quality of life modifier for above standardised mortality ratio
qCM <- global_inputs["qCM", "Value"]


# UK proportion female (may have to use this for all countries)
prop_female <- global_inputs["prop_female", "Value"]

# Simulations to run
cmmid_simulations <- c("A", "B", "C")


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
rownames(dQALYS_lost_table) <- c("Mitigation", cmmid_simulations)

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

# Table of cases, hospitalisations, and death under mitigation and each no mitigation scenario
clinical_results <- as.data.frame(matrix(nrow = length(country_names) * 4, ncol = 8))
colnames(clinical_results) <- c("Country", "QALYs lost per death", "Cases", "Hospitalisations",	"Deaths",
                                "Cases PP", "Hospitalisations PP", "Deaths PP")
rownames(clinical_results) <- paste(rep(c("Mitigation", cmmid_simulations), each = length(country_names)), rep(country_names, 4))
clinical_results[, "Country"] <- rep(country_names, 4)

# Mitigation results are not dependent on modelling scenario
clinical_results[paste("Mitigation", country_names), "Cases"] <- cases_mitigation[country_names]
clinical_results[paste("Mitigation", country_names), "Hospitalisations"] <- hospitalisations_mitigation[country_names]
clinical_results[paste("Mitigation", country_names), "Deaths"] <- deaths_mitigation[country_names]

# Table of results for the publication
publication_main_results <- as.data.frame(matrix(nrow = length(cmmid_simulations) * length(country_names), ncol = 6))
colnames(publication_main_results) <- c("Scenario", "Country", "Incremental QALYs (million)", "Incremental Costs (£billion)", "Incremental net benefit at £20k PP", "Incremental net benefit at £30k PP")
publication_main_results[, "Scenario"] <- rep(cmmid_simulations, each = length(country_names))
publication_main_results[, "Country"] <- rep(country_names, length(cmmid_simulations))

publication_cases_results <- publication_hospitalisation_results <- publication_death_results <- publication_main_results


for(cmmid_simulation in cmmid_simulations) {

  source("code/calculate_qalys_4.R")
  clinical_results[paste("Mitigation", country_names), "QALYs lost per death"] <- dQALYs_lost[country_names]
  clinical_results[paste(cmmid_simulation, country_names), "QALYs lost per death"] <- dQALYs_lost_cmmid[country_names]
  
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
  
  # Extract outcomes before applying cost and QALYs
  clinical_results[paste(cmmid_simulation, country_names), "Cases"] <- cases_nomitigation[country_names]
  clinical_results[paste(cmmid_simulation, country_names), "Hospitalisations"] <- hospitalisations_nomitigation[country_names]
  clinical_results[paste(cmmid_simulation, country_names), "Deaths"] <- deaths_nomitigation[country_names]
  
  
  # Use cmmid model to estimate proportion hospitalised
  cmmid_hospitalisation_ratio <- (cmmid_hospitalisations + cmmid_icu)/cmmid_cases
  
  # For comparison include GPD impact
  imf_gdp_loss <- as.data.frame(read_excel("data/covid_cea_input_data.xlsx", sheet = "gdp_impact"))
  population_size <- imf_gdp_loss[, c("Country", "Population size")]
  imf_gdp_loss_all <- imf_gdp_loss[, c("Country", "IMF GDP loss scenario all")]
  imf_gdp_loss <- imf_gdp_loss[, c("Country", "IMF GDP loss")]
  rownames(imf_gdp_loss) <- rownames(imf_gdp_loss_all) <- rownames(population_size) <- imf_gdp_loss[, "Country"]
  imf_gdp_loss[, "IMF GDP loss"] <- imf_gdp_loss[, "IMF GDP loss"] * dollar_to_gbp
  imf_gdp_loss_all[, "IMF GDP loss scenario all"] <- imf_gdp_loss_all[, "IMF GDP loss scenario all"] * dollar_to_gbp
  # Remove Korea
  imf_gdp_loss <- imf_gdp_loss[-which(rownames(imf_gdp_loss) == "Korea"), ]
  population_size <- population_size[-which(rownames(population_size) == "Korea"), ]
  
  for(subcalculation_name in subcalculation_names) {
    covid_cea_table[[subcalculation_name]] <- matrix(NA, nrow = length(country_names), ncol = 15)
    colnames(covid_cea_table[[subcalculation_name]]) <- c("dQALYs lost mitigation", "Costs mitigation", "dQALYs lost no mitigation", "Costs no mitigation",
                                                          "Incremental QALYs", "Incremental Costs", "ICER", 
                                                          "Incremental net benefit at £20k (£billion)", "Incremental net benefit at £30k (£billion)", 
                                                          "GDP Loss (£billion)", "GDP Loss scenario all (£billion)",
                                                          "Incremental net benefit at £20k PP", "Incremental net benefit at £30k PP", 
                                                          "GDP Loss PP (£)", "GDP Loss scenario all PP (£)")
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
      
      covid_cea_table[[subcalculation_name]][country_name, "GDP Loss (£billion)"] <- 
        imf_gdp_loss[country_name, "IMF GDP loss"]
      covid_cea_table[[subcalculation_name]][country_name, "GDP Loss scenario all (£billion)"] <- 
        imf_gdp_loss_all[country_name, "IMF GDP loss scenario all"]
      
      # Per person
      covid_cea_table[[subcalculation_name]][country_name, "GDP Loss PP (£)"] <- 
        1000000000 * covid_cea_table[[subcalculation_name]][country_name, "GDP Loss (£billion)"] / 
        population_size[country_name, "Population size"]
      covid_cea_table[[subcalculation_name]][country_name, "GDP Loss scenario all PP (£)"] <- 
        1000000000 * covid_cea_table[[subcalculation_name]][country_name, "GDP Loss scenario all (£billion)"] / 
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
    # Compare to economic loss£
    covid_cea_table[[subcalculation_name]][, "Incremental net benefit at £20k (£billion)"] <- (20000 *  covid_cea_table[[subcalculation_name]][, "Incremental QALYs"] - covid_cea_table[[subcalculation_name]][, "Incremental Costs"]) / 1000000000
    covid_cea_table[[subcalculation_name]][, "Incremental net benefit at £30k (£billion)"] <- (30000 *  covid_cea_table[[subcalculation_name]][, "Incremental QALYs"] - covid_cea_table[[subcalculation_name]][, "Incremental Costs"]) / 1000000000
    # Per person
    covid_cea_table[[subcalculation_name]][, "Incremental net benefit at £30k PP"] <- 1000000000 * covid_cea_table[[subcalculation_name]][, "Incremental net benefit at £30k (£billion)"] / population_size[, "Population size"]
    covid_cea_table[[subcalculation_name]][, "Incremental net benefit at £20k PP"] <- 1000000000 * covid_cea_table[[subcalculation_name]][, "Incremental net benefit at £20k (£billion)"] / population_size[, "Population size"]
    
    # Rescale to millions of QALYs and billions of £
    colnames(covid_cea_table[[subcalculation_name]]) <- c("dQALYs lost mitigation (million)", "Costs mitigation (£billion)", "dQALYs lost no mitigation (million)", "Costs no mitigation (£billion)",
                                                          "Incremental QALYs (million)", "Incremental Costs (£billion)", "ICER", 
                                                          "Incremental net benefit at £20k (£billion)", "Incremental net benefit at £30k (£billion)", 
                                                          "GDP Loss (£billion)", "GDP Loss scenario all (£billion)",
                                                          "Incremental net benefit at £20k PP", "Incremental net benefit at £30k PP", 
                                                          "GDP Loss PP (£)", "GDP Loss scenario all PP (£)")  
    
    covid_cea_table[[subcalculation_name]][, c("dQALYs lost mitigation (million)", "dQALYs lost no mitigation (million)",
                                               "Incremental QALYs (million)")] <- covid_cea_table[[subcalculation_name]][, c("dQALYs lost mitigation (million)", "dQALYs lost no mitigation (million)",
                                                                                                                             "Incremental QALYs (million)")] / 1000000
    
    covid_cea_table[[subcalculation_name]][, c("Costs mitigation (£billion)",  "Costs no mitigation (£billion)",
                                               "Incremental Costs (£billion)")] <-  covid_cea_table[[subcalculation_name]][, c("Costs mitigation (£billion)",  "Costs no mitigation (£billion)",
                                                                                                                               "Incremental Costs (£billion)")] / 1000000000
  }
  
  
  wb <- createWorkbook()
  
  for(subcalculation_name in subcalculation_names) {
    addWorksheet(wb, subcalculation_name)
    writeData(wb, subcalculation_name, covid_cea_table[[subcalculation_name]], startRow = 1, startCol = 1)
    #write.xlsx(covid_cea_table[[subcalculation_name]], file = paste0("results/covid_cea_table_", cmmid_simulation, "_smr", smr, ".xlsx"), sheetName = subcalculation_name, append = TRUE)  
  }
  saveWorkbook(wb, file = paste0("results/covid_cea_table_", cmmid_simulation, "_smr", smr, ".xlsx"), overwrite = TRUE)    
  
  # Add to the publication results tables
  publication_main_results[publication_main_results[, "Scenario"] == cmmid_simulation, -c(1:2)] <- 
    covid_cea_table[["all_impacts"]][, colnames(publication_main_results)[-c(1:2)]]
  publication_cases_results[publication_main_results[, "Scenario"] == cmmid_simulation, -c(1:2)] <- 
    covid_cea_table[["cases_only"]][, colnames(publication_main_results)[-c(1:2)]]
  publication_hospitalisation_results[publication_main_results[, "Scenario"] == cmmid_simulation, -c(1:2)] <- 
    covid_cea_table[["hospitalisation_only"]][, colnames(publication_main_results)[-c(1:2)]]
  publication_death_results[publication_main_results[, "Scenario"] == cmmid_simulation, -c(1:2)] <- 
    covid_cea_table[["death_only"]][, colnames(publication_main_results)[-c(1:2)]]
  
  
} # End loop over cmmid 

###################################################################################################
# Export results in format needed for publication
write.csv(publication_main_results, file = paste0("results/publication_main_results_", smr, ".csv"))
# Create a table with all CEA results for the publication
# Broken down by types of impact
publication_breakdown_results <- cbind(publication_cases_results, 
                                       publication_hospitalisation_results[, -c(1:2)],
                                       publication_death_results[, -c(1:2)])
write.csv(publication_breakdown_results, file = paste0("results/publication_breakdown_results_", smr, ".csv"))

###################################################################################################
# Table with GDP loss under each scenario and the % difference with the total loss scenario and 
# healthcare net benefit per capita
publication_economic_results <- as.data.frame(matrix(nrow = length(country_names), ncol = 5))
colnames(publication_economic_results) <- c("Country", "GDP Loss PP (£)", "GDP Loss scenario all PP (£)",
                                            "% recouped at £20k/QALY", "% recouped at £30k/QALY")
rownames(publication_economic_results) <- publication_economic_results[, "Country"] <- country_names
publication_economic_results[, c("GDP Loss PP (£)", "GDP Loss scenario all PP (£)")] <- 
  covid_cea_table[["all_impacts"]][,  c("GDP Loss PP (£)", "GDP Loss scenario all PP (£)")]
# Add % difference for each country
for(country_name in country_names) {
  publication_economic_results[country_name, "% recouped at £20k/QALY"] <- paste0(format(100 * publication_main_results[publication_main_results$Scenario == "A" & publication_main_results$Country == country_name,
      "Incremental net benefit at £20k PP"] / publication_economic_results[country_name, "GDP Loss scenario all PP (£)"], digits = 3, nsmall = 1), " (",
    format(100 * publication_main_results[publication_main_results$Scenario == "B" & publication_main_results$Country == country_name,
      "Incremental net benefit at £20k PP"] / publication_economic_results[country_name, "GDP Loss scenario all PP (£)"], digits = 3, nsmall = 1), ", ",
    format(100 * publication_main_results[publication_main_results$Scenario == "C" & publication_main_results$Country == country_name,
      "Incremental net benefit at £20k PP"] / publication_economic_results[country_name, "GDP Loss scenario all PP (£)"], digits = 3, nsmall = 1),")")
  
  publication_economic_results[country_name, "% recouped at £30k/QALY"] <- paste0(format(100 * publication_main_results[publication_main_results$Scenario == "A" & publication_main_results$Country == country_name,
      "Incremental net benefit at £30k PP"] / publication_economic_results[country_name, "GDP Loss scenario all PP (£)"], digits = 3, nsmall = 1), " (",
    format(100 * publication_main_results[publication_main_results$Scenario == "B" & publication_main_results$Country == country_name,
      "Incremental net benefit at £30k PP"] / publication_economic_results[country_name, "GDP Loss scenario all PP (£)"], digits = 3, nsmall = 1), ", ",
    format(100 * publication_main_results[publication_main_results$Scenario == "C" & publication_main_results$Country == country_name,
      "Incremental net benefit at £30k PP"] / publication_economic_results[country_name, "GDP Loss scenario all PP (£)"], digits = 3, nsmall = 1),")")
  
}
write.csv(publication_economic_results, file = paste0("results/publication_economic_results_", smr, ".csv"))


###################################################################################################
# Scale clinical results on outcomes to be per person
clinical_results[, "Cases PP"] <- as.numeric(clinical_results[, "Cases"]) / rep(population_size[, "Population size"], 4)
clinical_results[, "Hospitalisations PP"] <- as.numeric(clinical_results[, "Hospitalisations"]) / rep(population_size[, "Population size"], 4)
clinical_results[, "Deaths PP"] <- as.numeric(clinical_results[, "Deaths"]) / rep(population_size[, "Population size"], 4)

# Export the clinical outcomes
write.csv(clinical_results, file = paste0("results/clinical_results_", smr, ".csv"))

###################################################################################################
# Create incremental results table for events prevented by government response
clinical_results_incremental <- clinical_results
colnames(clinical_results_incremental) <- c("Country", "QALYS lost per death", "Cases prevented", "Hospitalisations prevented",
                                            "Deaths prevented", "Cases prevented PP", "Hospitalisations prevented PP", 
                                            "Deaths prevented PP")
for(cmmid_simulation in cmmid_simulations) {
  # Difference in row value but exclude country and QALYs per death
  clinical_results_incremental[grep(cmmid_simulation, rownames(clinical_results_incremental)), -c(1:2)] <- 
    clinical_results[grep(cmmid_simulation, rownames(clinical_results_incremental)), -c(1:2)] - 
    clinical_results[grep("Mitigation", rownames(clinical_results_incremental)), -c(1:2)]
}
# Remove the mitigation rows from the outcomes prevented table
clinical_results_incremental <- clinical_results_incremental[-grep("Mitigation", rownames(clinical_results_incremental)), ]

# Export the clinical outcomes prevented
write.csv(clinical_results, file = "results/clinical_results_incremental_", smr, ".csv")

###################################################################################################
# Illustrate the clinical outcomes results
pdf(paste("results/outcomes_prevented_pp_", smr, ".pdf"))
par(mfrow = c(2,2))
outcomes <- c("Cases prevented PP", "Hospitalisations prevented PP", "Deaths prevented PP")
outcome_names <- c("Cases", "Hospitalisations", "Deaths")
names(outcome_names) <- outcomes
for(outcome in outcomes) {
  # Create plotting area
  plot(c(0, 0), col = 0, xlim = c(1, 5), ylim = 1.1 * c(0, max(clinical_results_incremental[, outcome])),
       ylab = outcome_names[outcome], xlab = "", xaxt = "n")
  for(country_name in country_names) {
    # Scenario A is base case
    points(y = clinical_results_incremental[grep(country_name, rownames(clinical_results_incremental)), outcome][1],
          x = which(country_names == country_name), lwd = 2)
    # Scenarios B and C are lower and upper limits of R0
    lines(y = clinical_results_incremental[grep(country_name, rownames(clinical_results_incremental)), outcome][2:3],
           x = rep(which(country_names == country_name), 2), lwd = 2)
  }
  # UK lines for scenarios A, B, and C for ease of comparison
  lines(y = clinical_results_incremental[grep("UK", rownames(clinical_results_incremental)), outcome][c(1, 1)],
        x = c(1, 5))
  lines(y = clinical_results_incremental[grep("UK", rownames(clinical_results_incremental)), outcome][c(2, 2)],
        x = c(1, 5), lty = 2)
  lines(y = clinical_results_incremental[grep("UK", rownames(clinical_results_incremental)), outcome][c(3, 3)],
        x = c(1, 5), lty = 2)
  
  axis(side = 1, at = c(1:5), labels = country_names, las = 2)
}
dev.off()

###################################################################################################
##### alternative plot in ggplot
cri_plot <- clinical_results_incremental %>% select(c(1,6,7,8)) %>% mutate(category=gsub(" [A-z]*","",row.names(.))) %>% pivot_longer(2:4,names_to="Outcome",values_to="Value") %>% pivot_wider(names_from=category,values_from=Value)

linevals <- cri_plot %>% filter(Country=="UK")

pdf("results/outcomeplotsGG_smr_", smr, ".pdf",width=9,height=5)
cri_plot %>% ggplot(aes(x=Country,y=A)) + geom_point(size=2) + theme_minimal()  + geom_errorbar(aes(ymax=C,ymin=B),width=0.2) + ylab("Outcomes saved per capita") + geom_hline(data=linevals,aes(yintercept=A)) + geom_hline(data=linevals,aes(yintercept=B),linetype="dashed") + geom_hline(data=linevals,aes(yintercept=C),linetype="dashed")+ facet_wrap(~Outcome,scales="free") + xlab("")
dev.off()

pdf("results/outcomeplotsGG_2_smr", smr, ".pdf",width=9,height=8)
cri_plot %>% ggplot(aes(x=Country,y=A)) + geom_point(size=2) + theme_minimal()  + geom_errorbar(aes(ymax=C,ymin=B),width=0.2) + ylab("Outcomes saved per capita") + geom_hline(data=linevals,aes(yintercept=A)) + geom_hline(data=linevals,aes(yintercept=B),linetype="dashed") + geom_hline(data=linevals,aes(yintercept=C),linetype="dashed")+ facet_wrap(~Outcome) + xlab("")
dev.off()
