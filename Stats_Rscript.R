# MSTP-BioStats


# Load libraries

library(readr)
library(dplyr)
library(ggplot2)

# Set dataframe as variable 'polls', view, and check distinct IDs

polls <- read_csv('CandidateSummaryAction1.csv')
summary(polls)
View(polls)

n_distinct(polls$can_id)


# Select variables of interest

polls_filtered <- polls %>%
  select(can_id, can_off, can_off_sta, can_off_dis, 
         can_par_aff, can_inc_cha_ope_sea, tot_rec, winner) %>%
  rename(id = can_id, office = can_off, state = can_off_sta,
         district = can_off_dis, party = can_par_aff,
         status = can_inc_cha_ope_sea, funding = tot_rec) %>%
  filter(office == 'H')

summary(polls_filtered)
table(polls_filtered$winner) #there are 1429 house candidates and 435 winners

# 
