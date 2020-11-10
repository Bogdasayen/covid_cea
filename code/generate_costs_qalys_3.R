# Function to estimate costs and QALYs given an input number of
# cases, hospitalizations, ICU stays, and deaths.
# Uses a country specific life expectancy but other cost and QALYs are for the UK
# Simple decision tree

# Needs global_inputs table to be defined

generate_costs_qalys <- function(n_deaths, n_cases, n_hospitalisations, n_icus = NULL,
                                 country_name = "UK", dQALYs_lost, 
                                 subcalculation_name = "base_case") {
  # Model parameters
  cost_hospital_stay <- global_inputs["cost_hospital_stay", "Value"] # 2161
  cost_icu_day <- global_inputs["cost_icu_day", "Value"] #1504.47
  qalyloss_covid_case <- global_inputs["qalyloss_covid_case", "Value"] # -0.001671233
  qalyloss_covid_hospitalisation <- global_inputs["qalyloss_covid_hospitalisation", "Value"] #-0.031
  proportion_icu <- global_inputs["proportion_icu", "Value"] #0.17
  average_stay_icu = global_inputs["average_stay_icu", "Value"] #10.5
  
  if(subcalculation_name == "cases_only") {cost_hospital_stay <- cost_icu_day <- qalyloss_covid_hospitalisation <- 0}
  if(subcalculation_name == "hospitalisation_only") { qalyloss_covid_case <- 0}
  if(subcalculation_name == "death_only") { qalyloss_covid_case <- cost_hospital_stay <- cost_icu_day <- qalyloss_covid_hospitalisation <- 0}
  
  # Calculate ICU stays if not provided
  if(is.null(n_icus)) n_icus = proportion_icu * n_hospitalisations
  
  # QALYs lost for each death, hospitalisation, and case
  total_qalyloss <- -n_deaths * dQALYs_lost[country_name] + 
    n_hospitalisations * qalyloss_covid_hospitalisation + 
    n_cases * qalyloss_covid_case
  # If not counting impact of death
  if(subcalculation_name == "cases_only" | subcalculation_name == "hospitalisation_only") {
    total_qalyloss <-
      n_hospitalisations * qalyloss_covid_hospitalisation + 
      n_cases * qalyloss_covid_case
  }
  
  
  # Total cost per hospitalisation and ICU stay
  total_cost<- n_hospitalisations * cost_hospital_stay +
    n_icus * cost_icu_day * average_stay_icu
  
  return(list("total_qalyloss" = total_qalyloss,
              "total_cost" = total_cost))
  
}

# Example usage
#temp <- generate_costs_qalys(n_deaths = 49001 , n_cases = 295372, n_hospitalisations = 133125,
 #                    dQALYs_lost = dQALYs_lost_cmmid)

