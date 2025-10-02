vaccine_coverage <- user(0.25) # the coverage we want to achieve
vaccine_start <- user(5)
vaccine_end <- user(10)

##(NB coverage starts at time vaccine_start and reaches 25% at vaccine_end)
### !!!PROVIDED!!! WE ADD vaccine_start AND vaccine_end as 'critical times' in advanced options
convert_coverage_to_rate <- log(1 / (1 - vaccine_coverage)) / (vaccine_end - vaccine_start)
vaccine_rate <- if((t > vaccine_start) && (t < vaccine_end)) convert_coverage_to_rate else 0

initial(not_vaccinated) <- 1
initial(vaccinated) <- 0

deriv(not_vaccinated) <- -vaccine_rate * not_vaccinated
deriv(vaccinated) <- vaccine_rate * not_vaccinated
