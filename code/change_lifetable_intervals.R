# Script to convert 5 yearly interval lifetables to 1-yearly intervals
# Howard Thom 13-August-2020

baseline_directory <- "C:/Users/Howard/Documents/Bristol/Covid-19/Cost-effectiveness of lockdowns/code"
setwd(baseline_directory)


library(xlsx)

# Lifetables for 5 yearly interval
lifetables_5year <- read.xlsx("WHO_lifetables.xlsx", sheetName = "WHO_all")

# Countries to analyse
country_names <- c("Ireland", "Spain", "Germany", "Sweden")

rownames(lifetables_5year) <- lifetables_5year$Age.Group


# Create a matrix to store 1 year interval lifetable
lifetables_1year <- matrix(nrow = 101, ncol = dim(lifetables_5year)[2])
colnames(lifetables_1year) <- colnames(lifetables_5year)

lifetables_1year[, "Indicator"] <- "qx"

rownames(lifetables_1year) <- lifetables_1year[,"Age.Group"] <- c(0:100)
# Year 0 are the same
for(i in 3:dim(lifetables_5year)[2])lifetables_1year["0", i] <- lifetables_5year[1, i]
# Final 15 years have only one group 85+ years
for(i in 3:dim(lifetables_5year)[2]) {
  lifetables_1year[86:101, i] <- lifetables_5year[" 85+ years", i]/15
}

start_year <- c(1, (1:16) * 5)
end_year <- c(1:17) * 5 - 1

for(i_year_group in 1:length(start_year)) {
  for(i_country_gender in 3:dim(lifetables_5year)[2]) {
    # Split probability evenly over the 5 years
    lifetables_1year[start_year[i_year_group]:end_year[i_year_group] + 1, i_country_gender] <- 
      lifetables_5year[paste0(start_year[i_year_group], "-", end_year[i_year_group], " years"), i_country_gender] / 
      (end_year[i_year_group] - start_year[i_year_group])
  }
}

View(lifetables_1year)

# And export
write.xlsx(lifetables_1year, "WHO_lifetables.xlsx", sheetName = "WHO_all_1year", append = TRUE)
  


  
  
  
  