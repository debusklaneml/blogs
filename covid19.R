# Trying to get Coronavirus data and model it via latent profile analysis to look at predictors. 

library(tidyverse)
library(janitor)
library(readxl)
library(tidylog)

rami.data <- readr::read_csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv")


sk.patient <- readr::read_csv("https://raw.githubusercontent.com/jihoo-kim/Data-Science-for-COVID-19/master/dataset/Patient/PatientInfo.csv")
options(scipen = 999)
sk.1 <- sk.patient %>%
  mutate(yoa = 2020 - birth_year,
         patient_id = as.numeric(patient_id))

# Basic EDA
skimr::skim(sk.1)
Hmisc::describe(sk.1)


# Clean the data a bit to then push it to MPlus
sk.2 <- sk.1 %>%
  select(patient_id, sex, infection_case, infected_by, confirmed_date, state, yoa) %>%
  mutate(female = if_else(sex == "male", 0, 1),
         infect = if_else(!is.na(infected_by), 1, 0),
         date = as.Date("2020-03-18"),
         days = date - confirmed_date,
         deseaced = if_else(state == "deceased", 1, 0),
         isolated = if_else(state == "isolated", 1, 0),
         released = if_else(state == "released", 1, 0))%>%
  select(-sex, -confirmed_date, -infection_case, -infected_by, -date)
         

