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

# Remove NA from district

polls_filtered <- polls_filtered %>% filter(!is.na(district))


# Create df of winners and losers

winners <- polls_filtered %>%
  filter(winner == 'Y') %>%
  mutate(district_state = paste(district, state, sep =' '))

losers <- polls_filtered %>%
  mutate(win = is.na(winner)) %>%
  select(-winner) %>%
  filter(win == TRUE) %>%
  mutate(winner = 'N') %>%
  select(-win) %>%
  mutate(district_state = paste(district, state, sep =' '))


# Determine frequency of participants per race

num_in_race <- table(losers$district_state) %>%
  as.data.frame() %>%
  rename(district_state = Var1) %>%
  mutate(par = Freq + 1) %>%
  mutate(participants = as.numeric(par)) %>%
  select(-Freq, -par)

hist(num_in_race$participants, breaks = c(0:17)) #two participant race most common

two_par <- num_in_race %>%
  filter(participants == 2)

twopar_losers <- left_join(two_par, losers, by = 'district_state') %>%
  filter(district_state != '0 MP') %>%
  filter(district_state != '2 MS', district_state != '3 NE',
         district_state != '38 CA')

twopar_winners <- left_join(two_par, winners, by = 'district_state') %>%
  filter(district_state != '0 MP') %>%
  filter(district_state != '2 MS', district_state != '3 NE',
         district_state != '38 CA')

two_participants_win_lose <- union(twopar_losers, twopar_winners) %>%
  mutate(numfunds = parse_number(funding)) %>%
  mutate(dsfunding = paste(district_state, numfunds, sep = ' ')) %>%
  filter(district_state != '2 MS', district_state != '3 NE',
         district_state != '38 CA')

twopar_losers_funding <- twopar_losers %>%
  select(district_state, funding)

twopar_winners_funding <- twopar_winners %>%
  select(district_state, funding)


funding <- inner_join(twopar_losers_funding, twopar_winners_funding,
                      by = "district_state") %>%
  rename(funds1 = funding.x, funds2 = funding.y) %>%
  mutate(funding1 = parse_number(funds1)) %>%
  mutate(funding2 = parse_number(funds2)) %>%
  transmute(district_state = district_state, funds1 = as.numeric(funding1),
              funds2 = as.numeric(funding2)) %>%
  mutate(maxfunds = ifelse(funds1 > funds2,
                           funds1, funds2)) %>%
  mutate(minfunds = ifelse(funds1 > funds2,
                           funds2, funds1)) %>%
  mutate(dsfunding = paste(district_state, maxfunds, sep = ' ')) %>%
  select(district_state, maxfunds, minfunds)

maxfunds <- funding %>%
  transmute(dsfunding = paste(district_state, maxfunds, sep = ' '))

minfunds <- funding %>%
  transmute(dsfunding = paste(district_state, minfunds, sep = ' '))

maximum_funding_alldata <- maxfunds %>%
  left_join(two_participants_win_lose, by = 'dsfunding') %>%
  mutate(maxfunded = 'yes')

minimum_funding_alldata <- minfunds %>%
  left_join(two_participants_win_lose, by = 'dsfunding') %>%
  mutate(maxfunded = 'no')

all_data_2par_funds_wins <- union(minimum_funding_alldata, 
                                  maximum_funding_alldata) %>%
  select(-dsfunding, -numfunds)

table(all_data_2par_funds_wins$winner, all_data_2par_funds_wins$maxfunded)

chisq.test(all_data_2par_funds_wins$winner, all_data_2par_funds_wins$maxfunded)
