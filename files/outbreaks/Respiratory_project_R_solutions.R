################################################################################
##                                                                            ##
##             RESPIRATORY PROJECT: TASKS 1-5 SOLUTIONS                       ##
##                                                                            ##
################################################################################

# Load in libraries
library(tidyverse)

#### TASK 1 - DATA CLEANING ####################################################

## Download SARS_X_Linelist.xlsx file from SharePoint
## Set working directory to folder you have saved data in

## Read in SARS-X Linelist from Excel file:
SARS_X_Linelist <- read_excel("SARS_X_Linelist.xlsx")

## View data and try and understand what each column contains
View(SARS_X_Linelist)
# Check free text arguments for potential typos
unique(SARS_X_Linelist$hospital)
# There are three unique entries referring to Connaught Hospital

# Fix hospital column 
SARS_X_clean <- SARS_X_Linelist |> 
  mutate(clean_hospital=case_when(hospital=="Connaught" ~ "Connaught Hospital",
                                  hospital=="Connaught Hopital" ~ "Connaught Hospital",
                                  !(hospital%in%c("Connaught","Connaught Hopital")) ~ hospital))

## Check patient pipeline dates make sense
# Make sure date format is correct
SARS_X_clean$`date of infection`<- as.Date(SARS_X_clean$`date of infection`, 
                                           format = "%d/%m/%Y")
SARS_X_clean$`date of onset` <- as.Date(SARS_X_clean$`date of onset`,
                                        format = "%d/%m/%Y")
SARS_X_clean$`date of hospitalisation`<- as.Date(SARS_X_clean$`date of hospitalisation`, 
                                                 format = "%d/%m/%Y")
SARS_X_clean$`date of outcome`<- as.Date(SARS_X_clean$`date of outcome`,
                                         format = "%d/%m/%Y")

# Check no date of symptom onset is before date of infection
sum(SARS_X_clean$`date of infection`>SARS_X_clean$`date of onset`, na.rm=TRUE)
# Check no date of hospitalisation is before date of symptom onset 
sum(SARS_X_clean$`date of onset`>
      SARS_X_clean$`date of hospitalisation`, na.rm=TRUE)
# Check no date of outcome is before date of hospitalisation 
sum(SARS_X_clean$`date of hospitalisation`>
      SARS_X_clean$`date of outcome`, na.rm=TRUE)

## There are 200 dates of outcome that are before hospitalisation 
# Set these `date of outcome` entries as NA
SARS_X_clean <- SARS_X_clean |> 
  mutate(clean_date_of_outcome=ifelse(`date of hospitalisation`>`date of outcome`, NA,`date of outcome`))

# Add column for before/after intervention
SARS_X_clean <- SARS_X_clean |> 
  mutate(intervention=ifelse(`date of onset` > as.Date("2025-01-29"),"after","before"))


#### TASK 2 - INCIDENCE ########################################################

## Calculate total number of hospitalisations each day of onset 
SARS_incidence <- SARS_X_clean |> 
  group_by(`date of onset`) |>
  summarise(Hospitalisations = n()) 

# Plot total incidence
total_incidence <- ggplot(data = SARS_incidence)+
  geom_point(aes(x =`date of onset`, y = Hospitalisations))+
  theme_minimal()+
  geom_vline(xintercept = as.Date("2025-01-29"), colour="grey")+
  labs(x="Date of symptom onset", 
       y="Hospitalisation")
total_incidence
ggsave(total_incidence, file="total_incidence.png", width=7, height=5)

## Calculate total number of hospitalisations by day of onset and hospital
SARS_incidence_hospitals <- SARS_X_clean |> 
  group_by(`date of onset`, clean_hospital) |>
  summarise(Hospitalisations=n()) 

# Plot total incidence by hospital
total_incidence_hospital <- ggplot(data = SARS_incidence_hospitals)+
  geom_point(aes(x = `date of onset`, y = Hospitalisations, colour = clean_hospital))+
  theme_minimal()+
  geom_vline(xintercept = as.Date("2025-01-29"), colour="grey")+
  labs(x = "Date of symptom onset", y = "Hospitalisation")+
  facet_wrap( ~ clean_hospital, ncol = 2)+
  theme(legend.position = "none") 
total_incidence_hospital
ggsave(total_incidence_hospital, 
       file="total_incidence_hospital.png", 
       width=7, height=5)


## Calculate total number by day of onset and age group
SARS_incidence_age <- SARS_X_clean |> 
  group_by(`date of onset`, `age group`, intervention) |>
  summarise(Hospitalisations=n()) 

# Plot total incidence by age
total_incidence_age <- ggplot(data = SARS_incidence_age)+
  geom_point(aes(x = `date of onset`, y = Hospitalisations, colour = `age group`))+
  theme_minimal()+
  geom_vline(xintercept = as.Date(2025-01-29))+
  labs(x = "Date of symptom onset", y = "Hospitalisation")
total_incidence_age
ggsave(total_incidence_age, 
       file="total_incidence_age.png", 
       width=7, height=5)

# compare before/after intervetion 
total_incidence_age_intervention <- ggplot(data = SARS_incidence_age)+
  geom_point(aes(x = `date of onset`, y = Hospitalisations, colour = `intervention`))+
  theme_minimal()+
  geom_vline(xintercept = as.Date("2025-01-29"), colour="grey")+
  labs(x = "Date of symptom onset", y = "Hospitalisation")+
  facet_wrap( ~ `age group`)
total_incidence_age_intervention
ggsave(total_incidence_age_intervention, 
       file="total_incidence_age_intervention.png", 
       width=7, height=5)

## Calculate total number by day of onset and underlying health conditions
SARS_incidence_health <- SARS_X_clean |> 
  group_by(`date of onset`, `underlying health condition`) |>
  summarise(Hospitalisations=n()) 

# Plot total incidence by whether patient has an underlying health condition
total_incidence_health <- ggplot(data = SARS_incidence_health)+
  geom_point(aes(x = `date of onset`, y = Hospitalisations, colour = `underlying health condition`))+
  theme_minimal()+
  geom_vline(xintercept = as.Date(2025-01-29))+
  labs(x = "Date of symptom onset", y = "Hospitalisation")
total_incidence_health
ggsave(total_incidence_health, 
       file="total_incidence_health.png", 
       width=7, height=5)


##### TASK 3 - CFR #############################################################
## Plot outcomes
SARS_incidence_deaths <- SARS_X_clean |> 
  group_by(`date of onset`,outcome, intervention) |>
   summarise(Hospitalisations=n()) 

total_incidence_outcome <- ggplot(data = SARS_incidence_deaths)+
  geom_point(aes(x = `date of onset`, y = Hospitalisations, colour = `outcome`))+
  theme_minimal()+
  geom_vline(xintercept = as.Date(2025-01-29))+
  labs(x = "Date of symptom onset", y = "Hospitalisation")
total_incidence_outcome
ggsave(total_incidence_outcome, 
       file="total_incidence_outcome.png", 
       width=7, height=5)

#### CFR
### CFR by intervention 
outcomes_cfr <- SARS_incidence_deaths |>
  group_by(intervention, outcome) |>
  summarise(Hospitalisations=sum(Hospitalisations))

## Crude CFR 
# total (before & after intervention)  
(outcomes_cfr[1,3]+outcomes_cfr[4,3])/
  sum(outcomes_cfr[1:3,3]+
        outcomes_cfr[4:6,3])
## Known outcome
# total (before & after intervention)  
(outcomes_cfr[1,3]+outcomes_cfr[4,3])/
  sum(outcomes_cfr[1:2,3]+
        outcomes_cfr[4:5,3])

## Crude CFR 
# before intervention
outcomes_cfr[4,3]/sum(outcomes_cfr[4:6,3])
# after intervention 
outcomes_cfr[1,3]/sum(outcomes_cfr[1:3,3])

## Calculate CFR from known outcomes
# before intervention
outcomes_cfr[4,3]/sum(outcomes_cfr[4:5,3])
# after intervention 
outcomes_cfr[1,3]/sum(outcomes_cfr[1:2,3])


### CFR by gender 
gender_cfr <- SARS_X_clean |> 
  group_by(gender, outcome) |>
  summarise(Hospitalisations=n()) 

## crude CFR
# female
gender_cfr[1,3]/sum(gender_cfr[1:3,3])
# male
gender_cfr[4,3]/sum(gender_cfr[4:6,3])

## known outcome CFR
# female
gender_cfr[1,3]/sum(gender_cfr[1:2,3])
# male
gender_cfr[4,3]/sum(gender_cfr[4:5,3])


### CFR by age 
age_cfr <- SARS_X_clean |> 
  group_by(`age group`, outcome) |>
  summarise(Hospitalisations=n()) 

## crude CFR
# 14+
age_cfr[1,3]/sum(age_cfr[1:3,3])
# under 14
age_cfr[4,3]/sum(age_cfr[4:6,3])

## known outcome CFR
# 14+
age_cfr[1,3]/sum(age_cfr[1:2,3])
# under 14
age_cfr[4,3]/sum(age_cfr[4:5,3])

  
### CFR by underlying health condition  
health_cfr <- SARS_X_clean |> 
  group_by(`underlying health condition`, outcome) |>
  summarise(Hospitalisations=n()) 

## crude CFR
# no underlying health condition
health_cfr[1,3]/sum(health_cfr[1:3,3])
# has underlying health condition
health_cfr[4,3]/sum(health_cfr[4:6,3])

## known outcome CFR
# no underlying health condition
health_cfr[1,3]/sum(health_cfr[1:2,3])
#has underlying health condition
health_cfr[4,3]/sum(health_cfr[4:5,3])


#### TASK 4 - DELAYS ###########################################################

### Calculate delays (where dates are already given)
SARS_X_delays <- SARS_X_clean |> 
  mutate(onset_to_admission=difftime(`date of hospitalisation`,`date of onset`,
                                     units="days"),
         onset_to_outcome=difftime(clean_date_of_outcome,`date of onset`,
                                   units="days"), 
         hospital_duration=difftime(clean_date_of_outcome,`date of hospitalisation`,
                                    units="days"),
         incubation_period=difftime(`date of onset`,`date of infection`,
                                    units="days")
  )

## Calculate Serial Interval (SI)
# match infector date of onset to relevant case ID
SARS_X_delays$infector_onset <- SARS_X_delays$`date of onset`[
  match(SARS_X_delays$infector, SARS_X_delays$`case ID`)]

# Serial interval = date on symptom onset infectee - date of symptom onset infector 
SARS_X_delays <- SARS_X_delays |> 
  mutate(serial_interval = difftime(`date of onset`,infector_onset, units="days") )

### Plot delay and find mean and standard deviation 
## Plot serial interval 
serial_interval <- ggplot(SARS_X_delays, aes(x = as.numeric(serial_interval))) +
  geom_histogram() +
  labs(
    title = "Distribution of Serial Intervals",
    x = "Serial interval (days)",
    y = "Number of hospitalisations"
  ) +
  theme_minimal()
serial_interval
ggsave(serial_interval, file="serial_interval.png",
       width=7, height=5)
# Calculate mean and standard deviations
mean(as.numeric(SARS_X_delays$serial_interval), na.rm=TRUE)
sd(as.numeric(SARS_X_delays$serial_interval), na.rm=TRUE)


## Plot delay from onset of symptoms to hospital admission 
onset_to_admission <- ggplot(SARS_X_delays, aes(x = as.numeric(onset_to_admission))) +
  geom_histogram() +
  labs(
    title = "Distribution of symptom onset to admission",
    x = "Delay (days)",
    y = "Number of hospitalisations"
  ) +
  theme_minimal()
onset_to_admission
ggsave(onset_to_admission, file="onset_to_admission.png",
       width=7, height=5)
# Calculate mean and standard deviations
mean(as.numeric(SARS_X_delays$onset_to_admission), na.rm=TRUE)
sd(as.numeric(SARS_X_delays$onset_to_admission), na.rm=TRUE)



### Plot delay from onset of symptoms to date of outcome 
onset_to_outcome <- ggplot(SARS_X_delays, aes(x = as.numeric(onset_to_outcome))) +
  geom_histogram() +
  labs(
    title = "Distribution of symptom onset to outcome",
    x = "Delay (days)",
    y = "Number of hospitalisations"
  ) +
  theme_minimal()
onset_to_outcome
ggsave(onset_to_outcome, file="onset_to_outcome.png",
       width=7, height=5)
# Calculate mean and standard deviations
mean(as.numeric(SARS_X_delays$onset_to_outcome), na.rm=TRUE)
sd(as.numeric(SARS_X_delays$onset_to_outcome), na.rm=TRUE)

## Plot delay from onset of symptoms to date of death 
SARS_X_delays_death <- SARS_X_delays |>
  filter(outcome=="Death")
onset_to_outcome <- ggplot(SARS_X_delays_death, 
                           aes(x = as.numeric(onset_to_outcome))) +
  geom_histogram() +
  labs(
    title = "Distribution of symptom onset to death",
    x = "Delay (days)",
    y = "Number of hospitalisations"
  ) +
  theme_minimal()
onset_to_outcome
ggsave(onset_to_outcome, file="onset_to_outcome.png",
       width=7, height=5)
# Calculate mean and standard deviations
mean(as.numeric(SARS_X_delays_death$onset_to_outcome), na.rm=TRUE)
sd(as.numeric(SARS_X_delays_death$onset_to_outcome), na.rm=TRUE)


## Plot delay from onset of symptoms to date of recovered 
SARS_X_delays_recover <- SARS_X_delays |>
  filter(outcome=="Recover")
onset_to_outcome <- ggplot(SARS_X_delays_recover, 
                           aes(x = as.numeric(onset_to_outcome))) +
  geom_histogram() +
  labs(
    title = "Distribution of symptom onset to recovery",
    x = "Delay (days)",
    y = "Number of hospitalisations"
  ) +
  theme_minimal()
onset_to_outcome
ggsave(onset_to_outcome, file="onset_to_outcome.png",
       width=7, height=5)
# Calculate mean and standard deviations
mean(as.numeric(SARS_X_delays_recover$onset_to_outcome), na.rm=TRUE)
sd(as.numeric(SARS_X_delays_recover$onset_to_outcome), na.rm=TRUE)



## Plot delay from onset of symptoms to date of outcome 
hospital_duration <- ggplot(SARS_X_delays, aes(x = as.numeric(hospital_duration))) +
  geom_histogram() +
  labs(
    title = "Distribution of duration of hospital stay",
    x = "Delay (days)",
    y = "Number of hospitalisations"
  ) +
  theme_minimal()
hospital_duration
ggsave(hospital_duration, file="hospital_duration.png",
       width=7, height=5)
# Calculate mean and standard deviations
mean(as.numeric(SARS_X_delays$hospital_duration), na.rm=TRUE)
sd(as.numeric(SARS_X_delays$hospital_duration), na.rm=TRUE)



## Plot delay from date of infection to onset of symptoms (incubation period)
incubation_period <- ggplot(SARS_X_delays, aes(x = as.numeric(incubation_period))) +
  geom_histogram() +
  labs(
    title = "Distribution of incubation period",
    x = "Delay (days)",
    y = "Number of hospitalisations"
  ) +
  theme_minimal()
incubation_period
ggsave(incubation_period, file="incubation_period.png",
       width=7, height=5)
# Calculate mean and standard deviations
mean(as.numeric(SARS_X_delays$incubation_period), na.rm=TRUE)
sd(as.numeric(SARS_X_delays$incubation_period), na.rm=TRUE)




#### TASK 5 - PROJECTIONS ######################################################

### fit log-linear model to incidence 
## filter data for before intervention
# add column for day of outbreak (from first date of symptom onset)
SARS_incidence_before <- filter(SARS_incidence, intervention=="before") |>
  mutate(day=`date of onset`-min(SARS_incidence$`date of onset`)+1)

## fit linear model to log of the counts 
model_log_linear_before <- lm(log(`Hospitalisations`) ~ as.numeric(day),
                              data=SARS_incidence_before)

## get model fit - remember to take exponential of predictions
fitted_before <- as.data.frame(exp(model_log_linear_before$fitted.values)) |>
  mutate(day=SARS_incidence_before$day)
colnames(fitted_before)<-c("Model fit", "day")
# plot model fit
model_fit_before <- ggplot()+
  geom_point(data=SARS_incidence_before, aes(x=day, y=Hospitalisations))+
  geom_line(data=fitted_before, aes(x=day, y=`Model fit` , colour="Model fit"))+
  theme_minimal()+labs(title="Log-linear model before intervention")
model_fit_before
ggsave(model_fit_before, file="model_fit_before.png",
       width=7, height=5)

## Calculate doubling rate (cases are increasing)
# LN(2)/r
r_before <- model_log_linear_before$coefficients[2]
doubling_rate_before <- log(2)/r_before

## Calculate R
# R = (1 + r * incubation_period)*(1 + r * infectious_period)
# from handout infectious period = 5.2
infectious_period <- 5.2
incubation_period <- mean(as.numeric(SARS_X_delays$incubation_period), na.rm=TRUE)
R_before <- (1 + r_before * incubation_period)*(1 + r_before * infectious_period)


## filter data for after intervention
# add column for day of outbreak (from first date of symptom onset)
SARS_incidence_after <- filter(SARS_incidence, intervention=="after") |>
  mutate(day=`date of onset`-min(SARS_incidence$`date of onset`)+1)

model_log_linear_after <- lm(log(`Hospitalisations`) ~ as.numeric(day),
                             data=SARS_incidence_after)

fitted_after <- as.data.frame(exp(model_log_linear_after$fitted.values)) |>
  mutate(day=as.numeric(SARS_incidence_after$day))
colnames(fitted_after)<-c("Model fit", "day")

model_fit_after <- ggplot()+
  geom_point(data=SARS_incidence_after, aes(x=day, y=Hospitalisations))+
  geom_line(data=fitted_after, aes(x=day, y=`Model fit` , colour="Model fit"))+
  theme_minimal()+labs(title="Log-linear model after intervention")
model_fit_after
ggsave(model_fit_after, file="model_fit_before.png",
       width=7, height=5)

## Calculate halving rate (cases are increasing)
# LN(0.5)/r
r_after <- model_log_linear_after$coefficients[2]
halving_time_after <- log(0.5)/r_after


## Calculate R
# R = (1 + r * incubation_period)*(1 + r * infectious_period)
# from handout infectious period = 5.2
R_after <- (1 + r_after * incubation_period)*(1 + r_after * infectious_period)
 

### Projections from after fit 
## Manually calculatin projection
slope <- model_log_linear_after$coefficients[2]
intercept <- model_log_linear_after$coefficients[1]

projections <- data.frame(day = seq(from = max(SARS_incidence_after$day), to = 500))
projections$projections <- exp(intercept + projections$day * slope)

projections_plot_manual <- ggplot()+
  geom_point(data=SARS_incidence_after, aes(x=day, y=Hospitalisations))+
  geom_line(data=fitted_after, aes(x=day, y=`Model fit`, colour="Model fit"))+
  geom_line(data=projections, aes(x=day, y=`projections`, colour="Model projections"))+
  theme_minimal()+labs(title="Log-linear model after intervention")
projections_plot_manual
ggsave(projections_plot_manual, file="projections_plot_manual.png",
       width=7, height=5)

## Calculating projections using predict function 
# Fitted values on the original scale (exp)
projections <- data.frame(day = seq(from = max(SARS_incidence_after$day), to = 500))
projections$projections <- exp(predict(model_log_linear_after,
        newdata = data.frame(day = as.numeric(seq(from = max(SARS_incidence_after$day), to = 500)))))

# Plot observed vs fitted
projections_plot_predict <- ggplot()+
  geom_point(data=SARS_incidence_after, aes(x=day, y=Hospitalisations))+
  geom_line(data=fitted_after, aes(x=day, y=`Model fit` , colour="Model fit"))+
  geom_line(data=projections, aes(x=day, y=`projections` , colour="Model projections"))+
  theme_minimal()+labs(title="Log-linear model after intervention")
projections_plot_predict
ggsave(projections_plot_predict, file="projections_plot_predict.png",
       width=9, height=5)
