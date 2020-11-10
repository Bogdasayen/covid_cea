# Script to calculate discounted QALYs lost due to Covid
# Follows Briggs Excel sheet available here:
# Implemented in R to allow extension to other countries, include QALY lost due to
# non-fatal cases, and to include healthcare costs
# Howard Thom 27-July-2020


require(readxl)




# Extract cmmid modelled age at death for each country
cmmid_raw <- read.xlsx(paste0(baseline_directory,"/data/covid_cea_input_data.xlsx"), sheetName = "cmmid_model")
# Need to specify cmmid_simulation
cmmid_raw <- cmmid_raw[cmmid_raw[, "Simulation"] == cmmid_simulation, ]
# Format to match age_death_country
cmmid_age_death_country <- matrix(nrow = 6, ncol = 6)
colnames(cmmid_age_death_country) <- c("age", unlist(lapply(cmmid_raw[, "Country"], toString)))
# Midpoints of age groups, except 75+ which we (somewhat arbitrarily) set to 80.
cmmid_age_death_country[, "age"] <- c(14/2, (29+15)/2, (44+30)/2, (49+45)/2, (74+60)/2, 80)
cmmid_age_death_country[, -1] <- t(cmmid_raw[, grep("Deaths", colnames(cmmid_raw))][, -7])
# Scale to proportions (this will later be rescaled using the total deaths)
for(i_country in 2:dim(cmmid_age_death_country)[2]) {
  cmmid_age_death_country[, i_country] <- cmmid_age_death_country[, i_country] / sum(cmmid_age_death_country[, i_country])  
}



generate_dQALYs_lost<-function(lifetable_female, lifetable_male, age_death,
                               qol_norm_country) {
  # Use the qx(probability of dying between ages x and x+1)
  # from males and females to calculate life expectancy for cohort
  lifetable_general <- matrix(nrow = dim(lifetable_male)[1], ncol = 6)
  colnames(lifetable_general) <- c( "smrlx", "smrLx", "smrLEx", "smrqLx", "QALE", "dQALY")
  
  # Proportion of cohort surviving to each time
  lifetable_general[1, "smrlx"] <- 100000
  for(i_age in 2:dim(lifetable_general)[1]) {
    lifetable_general[i_age, "smrlx"] <- (1-prop_female) * lifetable_general[i_age - 1, "smrlx"] * exp(log(1 - lifetable_male[i_age - 1, "qx"]) * smr) +
      prop_female * lifetable_general[i_age - 1, "smrlx"] * exp(log(1 - lifetable_female[i_age - 1, "qx"]) * smr)
  }
  
  # Calculate person-years lived between ages x and x+1 
  lifetable_general[, "smrLx"] <- (c(lifetable_general[1 + 1:(dim(lifetable_general)[1]-1), "smrlx"],0) + lifetable_general[, "smrlx"]) / 2
  
  
  # Calculate number of people who survive to each year
  for(i_age in 1:dim(lifetable_general)[1]) {
    lifetable_general[i_age, "smrLEx"] <- sum(lifetable_general[i_age:(dim(lifetable_general)[1]), "smrLx"]) / lifetable_general[i_age, "smrlx"]
  }
  
  # Weight ages by QALYs
  ######################################################################
  for(i_age in 1:dim(lifetable_general)[1]) {
    lifetable_general[i_age, "smrqLx"] <- lifetable_general[i_age, "smrLx"] * 
      qol_norm_country[max(which((i_age - 1) >= qol_norm_country[, "start_age"])), "qol_weight"]
  }
  
  # Calculate number of people who survive to each year
  for(i_age in 1:dim(lifetable_general)[1]) {
    lifetable_general[i_age, "QALE"] <- sum(lifetable_general[i_age:(dim(lifetable_general)[1]), "smrqLx"]) / lifetable_general[i_age, "smrlx"]
    
    lifetable_general[i_age, "dQALY"] <- 
      sum(lifetable_general[i_age:(dim(lifetable_general)[1]), "smrqLx"] *
            (1/discount_rate)^(i_age:(dim(lifetable_general)[1])-i_age)) / lifetable_general[i_age, "smrlx"]
  }
  
  # Calculate weighted average of dQALY using age at death distribution
  # These are QALYs lost per death
  dQALYs <- sum(lifetable_general[age_death[, "age"] + 1, "dQALY"] * age_death[, "proportion"], na.rm = TRUE)
  
  return(dQALYs)
}

# Needs country_names to be specified

dQALYs_lost <- dQALYs_lost_cmmid <- rep(NA, length(country_names))
names(dQALYs_lost) <- names(dQALYs_lost_cmmid) <- country_names

# Calculate discounted QALYs lost per death for each of the countries of interest
for(country_name in country_names) {
  # Calculate actual/reported dQALYs lost per death
  age_death_country <- age_death_distribution[, c("Age.band", "index", country_name)]
  colnames(age_death_country) <- c("Age.band", "age", "proportion")
  age_death_country[, "age"] <- age_death_country[, "age"] + 5
  
  # Country specific inputs
  # Will use this format for the function
  qol_norm_country <- qol_norm_data[, c("Age.band", "index", country_name)]
  colnames(qol_norm_country) <- c("Age.band", "start_age", "qol_weight")
  
  
  
  
  if(country_name == "UK") {
    lifetable_male = lifetable_male_uk
    lifetable_female = lifetable_female_uk
    
  } else {
    # Extract the necessary lifetable data 
    # Convert from factor to numeric
    lifetable_female <- who_lifetables_qx[, c("Age.Group", paste0(country_name,"_female"))]
    lifetable_female[, 2] <- as.numeric(unlist(lapply(lifetable_female[, 2], toString)))
    colnames(lifetable_female)[2] <- "qx"
    lifetable_male <- who_lifetables_qx[, c("Age.Group", paste0(country_name,"_male"))]
    lifetable_male[, 2] <- as.numeric(unlist(lapply(lifetable_male[, 2], toString)))
    colnames(lifetable_male)[2] <- "qx"
    
    # If a country's age at death distribution is not reported use that of the UK
    if(sum(is.na(age_death_country[, "proportion"])) > 2) {
      age_death_country[, "proportion"] <- age_death_distribution[, "UK"]
    }
  }
  
  
  dQALYs_lost[country_name] <- generate_dQALYs_lost(lifetable_female = lifetable_female, 
                       lifetable_male = lifetable_male, age_death = age_death_country,
                       qol_norm_country = qol_norm_country)
  
  # Calculate modelled dQALYs lost under no mitigation scenario cmmid model
  age_death_country <- cmmid_age_death_country[, c("age", country_name)]
  colnames(age_death_country) <- c("age","proportion")
  dQALYs_lost_cmmid[country_name] <- generate_dQALYs_lost(lifetable_female = lifetable_female, 
                                                    lifetable_male = lifetable_male, age_death= age_death_country,
                                                    qol_norm_country = qol_norm_country)
  
}


