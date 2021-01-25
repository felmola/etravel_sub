### R script for "Fair and Unfair Commercial Practices in a Pit Market:
### A Framed Lab Experiment"
### Author: Rafael Charrris and Felipe Montealegre

# Load Packages
library(tidyverse)
library(stargazer)
library(fastDummies)
library(estimatr)
library(ggpattern)

# Load Data
etravel <- read.csv("D:\\Rafael\\Dropbox\\4 diversificación Open Evidence\\2 Online tourism\\Lab\\Data\\etravel_final.csv", sep = ";")

# Figure 1: Decision to acquire info on competitors
etravel %>%
  filter(role == "seller") %>%
  ggplot(aes(x = as.factor(treatment), y = see_list)) +
  stat_summary(fun = mean,
               geom = "bar",
               color = "black",
               fill = "white") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               position  = position_dodge(width = 0.9),
               width = 0.2
  ) +
  labs(y = "Percentage of Participants", x = "Treatment") + 
  scale_y_continuous(labels = function(x)paste0(x*100, "%")) +
  scale_x_discrete(labels = c("Control", "Commercial\nPractices (CP)",
                              "CP + Formal\nSanction", "CP + Informal\nSanction",
                              "CP +\n Regret"))+
  theme_bw() + 
  ggsave("graphs\\percentage_saw_list.png", dpi = 320)

# Figure 2: Probability of using a commercial practice by condition

## Panel A
etravel %>%
  filter(treatment != 0, com_practice != "5", role == "seller") %>%
  ggplot(aes(x = as.factor(treatment), y = used_com_practice)) + 
  stat_summary(fun = mean,
               geom = "bar",
               color = "black",
               fill = "white") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               position  = position_dodge(width = 0.9),
               width = 0.2
  ) +
  labs(y = "Percentage of Times CP were used", x = "Treatment") + 
  scale_y_continuous(labels = function(x)paste0(x*100, "%")) +
  scale_x_discrete(labels = c("Commercial\nPractices (CP)",
                              "CP + Formal\nSanction", "CP + Informal\nSanction",
                              "CP +\n Regret"))+
  theme_bw() + 
  ggsave("percentage_CP_used.png", dpi = 320)

## Panel B

etravel %>%
  filter(role == "seller", treatment != 0) %>%
  group_by(treatment,com_practice) %>% 
  summarise(n = n(),
            grouped_used_com_practice = sum(used_com_practice)) %>% 
  group_by(treatment) %>% 
  mutate(n_treatment = sum(n),
         com_practice = as.factor(com_practice)) %>%
  summarise(com_practice, prob_com_prac = grouped_used_com_practice/n_treatment) %>%
  filter(com_practice != 0) %>%
  mutate(com_practice = factor(case_when(
    com_practice == 1 ~ "Best Price\nGuarantee",
    com_practice == 2 ~ "Reference Pricing",
    com_practice == 3 ~ "Drip Pricing",
  ))) %>%
  rename(`Commercial Practice`= com_practice) %>%
  ggplot() + 
  geom_bar_pattern(stat = "identity",
                   fill = "white",
                   color = "black",
                   pos = "stack",
                   pattern_density = 0.2,
                   pattern_spacing = 0.025,
                   pattern_aspect_ratio = 1,
                   pattern_fill    = 'black',
                   pattern_colour  = 'black',
                   aes(x = as.factor(treatment),
                       y = prob_com_prac,
                       pattern= `Commercial Practice`)) +
  theme_bw() +
  theme(text = element_text(size=16), legend.position = "bottom")+
  labs(y = "Probability of using a commercial\npractice", x = "Treatment") +
  scale_y_continuous(labels = function(x)paste0(x*100, "%")) + 
  scale_x_discrete(labels = c("Commercial\nPractices (CP)",
                              "CP + Formal\nSanction", "CP + Informal\nSanction",
                              "CP +\n Regret")) + 
  ggsave("graphs\\used_comm_practice_percentages.png", dpi = 320, width = 8, height = 5)

## Panel C
## Excel

# Figure 3: Probability of cheating
## Panel A
etravel %>%
  filter(role == "seller", treatment != 0) %>%
  ggplot(aes(x = as.factor(treatment), y = badpractice)) +
  stat_summary(fun = mean,
               geom = "bar",
               position = "dodge",
               fill = "white",
               color = "black") +
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               position  = position_dodge(width = 0.9),
               width = 0.2) +
  labs(y = "Probability of cheating", x ="Treatment" ) +
  scale_x_discrete(labels = c("Commercial\nPractices (CP)",
                              "CP + Formal\nSanction", "CP + Informal\nSanction",
                              "CP +\n Regret")) + 
  scale_y_continuous(labels = function(x)paste0(x*100, "%")) +
  theme_bw() +
  theme(text = element_text(size=16))+
  ggsave("graphs\\prob_cheating.png", dpi = 320, width = 6 , height = 4.7)

## Panel B

etravel %>%
  filter(role == "seller", treatment != 0) %>%
  group_by(treatment,com_practice) %>% 
  summarise(n = n(),
            grouped_badpractice = sum(badpractice)) %>% 
  group_by(treatment) %>% 
  mutate(n_treatment = sum(n),
         com_practice = as.factor(com_practice)) %>%
  summarise(com_practice, prob_cheating = grouped_badpractice/n_treatment) %>%
  filter(com_practice != 0) %>%
  mutate(com_practice = factor(case_when(
    com_practice == 1 ~ "Best Price\nGuarantee",
    com_practice == 2 ~ "Reference Pricing",
    com_practice == 3 ~ "Drip Pricing",
  ))) %>%
  rename(`Commercial Practice`= com_practice) %>%
  ggplot() + 
  geom_bar_pattern(stat = "identity",
                   fill = "white",
                   color = "black",
                   pos = "stack",
                   pattern_density = 0.2,
                   pattern_spacing = 0.025,
                   pattern_aspect_ratio = 1,
                   pattern_fill    = 'black',
                   pattern_colour  = 'black',
                   aes(x = as.factor(treatment),
                       y = prob_cheating,
                       pattern= `Commercial Practice`)) +
  theme_bw() +
  theme(text = element_text(size=16), legend.position = "bottom") +
  labs(y = "Probability of cheating", x ="Treatment") +
  scale_x_discrete(labels = c("Commercial\nPractices (CP)",
                              "CP + Formal\nSanction", "CP + Informal\nSanction",
                              "CP +\n Regret")) + 
  scale_y_continuous(labels = function(x)paste0(x*100, "%")) + 
  ggsave("graphs\\prob_cheating_percentages.png", dpi = 300, width = 8, height = 5)

## Panel C
##Excel

# Figure 4: Efficiency by experimental condition
## Data Manipulation
### 1. Get the sellers
sellers <- etravel %>%
  filter(role == "seller") %>%
  group_by(sessioncode, round, seller_package) %>% 
  select(participantcode, seller_package, seller_valuation) %>%
  arrange(.by_group = TRUE, seller_valuation)

# Add rank
sellers <- sellers %>%
  group_by(sessioncode, round, seller_package) %>%
  mutate(rank = order(order(sessioncode, round, seller_package, decreasing=TRUE)))

# get buyers
buyers <- etravel %>%
  filter(role == "buyer") %>%
  pivot_longer(names_to = "buyer_package",
               cols = contains("buyer_valuation_pac"), 
               names_pattern = "buyer_valuation_pac(.+)",
               values_to = "buyer_valuations") %>% 
  group_by(sessioncode, round, buyer_package) %>%
  arrange(desc(buyer_valuations), .by_group = TRUE) %>%
  select(id_in_group, country, participantcode, buyer_package, buyer_valuations, treatment)
buyers$buyer_package <- as.numeric(buyers$buyer_package)

# Add rank to buyers
buyers <- buyers %>%
  group_by(sessioncode, country, round, buyer_package, treatment) %>%
  mutate(rank = order(order(sessioncode, round, decreasing=TRUE)))

# Match between sellers and buyers
ef_base <- inner_join(sellers, buyers, 
                      by = c("sessioncode", 
                             "round",
                             "rank",
                             "seller_package" = "buyer_package"))

# Change the name of some variables
ef_final <- ef_base %>% 
  rename(package = seller_package, 
         seller_code = participantcode.x,
         buyer_code = participantcode.y
  )

# Get the total efficency 
ef_final_valuations <- ef_final %>%
  mutate(surplus = buyer_valuations - seller_valuation) %>%
  group_by(sessioncode, round, package, treatment) %>%
  mutate(total_efficency = sum(surplus))

# Aggregate the surplus at the market level
ef_final_grouped <- ef_final_valuations %>%
  group_by(sessioncode, round, treatment) %>%
  summarise(max_efficency = sum(total_efficency))

#### Actual Efficency ######## 
# Get the sellers who sold
sold <- etravel %>%
  filter(my_buyer > 0) %>%
  select(-starts_with("buyer_valuation_pac"), -c(4:5,7:10), 
         -c(will_buy, my_seller,id_in_group, time_spent, paying_round, X, X.1))

# Get the sellers who sold and the buyers who bought
transactions <- left_join(sold, buyers, 
                          by = c("sessioncode", 
                                 "round",
                                 "country",
                                 "treatment",
                                 "my_buyer" = "id_in_group",
                                 "seller_package" = "buyer_package")) %>%
  rename(package = seller_package, 
         seller_code = participantcode.x,
         buyer_code = participantcode.y)

## get the actual surplus and aggregate the surplus at the market level

transactions_efficency <- transactions %>% 
  mutate(surplus = buyer_valuations - seller_valuation) %>%
  group_by(sessioncode,country, round, treatment) %>%
  summarise(aggregated_surplus = sum(surplus))

#### Match the theoretical efficency, and the actual efficency

actual_ef <- left_join(ef_final_grouped, transactions_efficency)

market_efficency <- actual_ef %>%
  mutate(aggregated_surplus/max_efficency*100)

##### Correct efficency ####
# Left the player in the market they make the more efficent. 
corrected_buyers <- ef_final_valuations %>% 
  select(sessioncode,buyer_code, surplus, id_in_group, treatment, country)%>% 
  group_by(sessioncode, round, buyer_code) %>% 
  arrange(buyer_code, .by_group = TRUE) %>%
  mutate(max_surplus = max(surplus)) %>%
  filter(surplus == max_surplus)

# Find the efficency of the system
ef_corrected <- corrected_buyers %>%
  group_by(sessioncode,country, round, treatment) %>%
  select(-max_surplus) %>%
  summarise(max_efficency = sum(surplus))

# Match the new aficency to the previous data set 
actual_ef_corrected <- left_join(ef_corrected, transactions_efficency)

market_efficency_corrected <- actual_ef_corrected %>%
  mutate(aggregated_surplus/max_efficency*100)

market_efficency <- market_efficency %>% 
  rename(max_efficency_no_corrected= max_efficency, 
         total_efficency_not_corrected = `aggregated_surplus/max_efficency * 100`)

market_efficency_corrected <-market_efficency_corrected %>%  
  rename(max_efficency_corrected = max_efficency, 
         total_efficency_corrected= `aggregated_surplus/max_efficency * 100`)
# Get all the data in a data set
all_market <- left_join(market_efficency, market_efficency_corrected) %>%
  #Reorganize data
  select(sessioncode, country, round, treatment, 
         aggregated_surplus, max_efficency_no_corrected, 
         total_efficency_not_corrected, max_efficency_corrected, 
         total_efficency_corrected)
# Plot
ggplot(all_market, aes(x = round, y = total_efficency_corrected))+
  stat_summary(
    fun = mean, 
    geom = "line",
    aes(linetype=as.factor(treatment)),
    size = 1
  ) +
  stat_summary(
    fun = mean, 
    geom = "point",
    aes(shape=as.factor(treatment)),
    size = 2
  ) +
  scale_y_continuous(labels = function(x)paste0(x, "%")) + 
  scale_shape_discrete(name = "Treatment",
                       breaks = c("0","1", "2","3", "4"),
                       labels = c("Control", "Commercial\nPractices (CP)",
                                  "CP +\nFormal\nSanction", "CP +\nInformal\nSanction",
                                  "CP +\nRegret")) + 
  guides(linetype =FALSE) +
  labs(y = "Efficency of the Market", x = "Round")+ 
  theme_bw() + 
  theme(legend.position="bottom", legend.text=element_text(size=6)) +
  ggsave("graphs\\market_efficency_by_round.png", dpi = 320)

# Figure 5


# Table 2: Equilibrium properties of different treatments

# Substract initial endowment
etravel <- etravel %>%
  mutate(payoff = ifelse(country == 0, payoff - 25, payoff - 30))

### Modify audited 
# ID for merge
etravel <- etravel %>%
  mutate(id = row_number())
#Sample 20 
corrected_sellers <- etravel %>%
  filter(country == 0 & treatment == 2 & badpractice == 1) %>%
  sample_n(size = 20) %>%
  mutate(correct_payoff = payoff - 20)
#Merge back
etravel <- etravel %>%
  left_join(corrected_sellers) %>% 
  mutate(payoff = ifelse(!is.na(correct_payoff), correct_payoff, payoff))

# Create net_profit variable
etravel <- etravel %>%
  mutate(net_profit = payoff)

## Mean market price 
etravel <- etravel %>%
  mutate(market = case_when(
    sessioncode == "3b5a4rwf" & round == 1 ~ "1",
    sessioncode == "3b5a4rwf" & round == 2 ~ "2",
    sessioncode == "3b5a4rwf" & round == 3 ~ "3",
    sessioncode == "3b5a4rwf" & round == 4 ~ "4",
    sessioncode == "3b5a4rwf" & round == 5 ~ "5",
    sessioncode == "44kexb5g" & round == 1 ~ "6",
    sessioncode == "44kexb5g" & round == 2 ~ "7",
    sessioncode == "44kexb5g" & round == 3 ~ "8",
    sessioncode == "44kexb5g" & round == 4 ~ "9",
    sessioncode == "44kexb5g" & round == 5 ~ "10",
    sessioncode == "53dvdbpt" & round == 1 ~ "11",
    sessioncode == "53dvdbpt" & round == 2 ~ "12",
    sessioncode == "53dvdbpt" & round == 3 ~ "13",
    sessioncode == "53dvdbpt" & round == 4 ~ "14",
    sessioncode == "53dvdbpt" & round == 5 ~ "15",
    sessioncode == "5ut4i0ln" & round == 1 ~ "16",
    sessioncode == "5ut4i0ln" & round == 2 ~ "17",
    sessioncode == "5ut4i0ln" & round == 3 ~ "18",
    sessioncode == "5ut4i0ln" & round == 4 ~ "19",
    sessioncode == "5ut4i0ln" & round == 5 ~ "20",
    sessioncode == "7rii3hlv" & round == 1 ~ "21",
    sessioncode == "7rii3hlv" & round == 2 ~ "22",
    sessioncode == "7rii3hlv" & round == 3 ~ "23",
    sessioncode == "7rii3hlv" & round == 4 ~ "24",
    sessioncode == "7rii3hlv" & round == 5 ~ "25",
    sessioncode == "7x8tbzhg" & round == 1 ~ "26",
    sessioncode == "7x8tbzhg" & round == 2 ~ "27",
    sessioncode == "7x8tbzhg" & round == 3 ~ "28",
    sessioncode == "7x8tbzhg" & round == 4 ~ "29",
    sessioncode == "7x8tbzhg" & round == 5 ~ "30",
    sessioncode == "ap9thekt" & round == 1 ~ "31",
    sessioncode == "ap9thekt" & round == 2 ~ "32",
    sessioncode == "ap9thekt" & round == 3 ~ "33",
    sessioncode == "ap9thekt" & round == 4 ~ "34",
    sessioncode == "ap9thekt" & round == 5 ~ "35",
    sessioncode == "aqekfl2l" & round == 1 ~ "36",
    sessioncode == "aqekfl2l" & round == 2 ~ "37",
    sessioncode == "aqekfl2l" & round == 3 ~ "38",
    sessioncode == "aqekfl2l" & round == 4 ~ "39",
    sessioncode == "aqekfl2l" & round == 5 ~ "40",
    sessioncode == "eni1zs0w" & round == 1 ~ "41",
    sessioncode == "eni1zs0w" & round == 2 ~ "42",
    sessioncode == "eni1zs0w" & round == 3 ~ "43",
    sessioncode == "eni1zs0w" & round == 4 ~ "44",
    sessioncode == "eni1zs0w" & round == 5 ~ "45",
    sessioncode == "iu41zgjt" & round == 1 ~ "46",
    sessioncode == "iu41zgjt" & round == 2 ~ "47",
    sessioncode == "iu41zgjt" & round == 3 ~ "48",
    sessioncode == "iu41zgjt" & round == 4 ~ "49",
    sessioncode == "iu41zgjt" & round == 5 ~ "50",
    sessioncode == "jhr6la2a" & round == 1 ~ "51",
    sessioncode == "jhr6la2a" & round == 2 ~ "52",
    sessioncode == "jhr6la2a" & round == 3 ~ "53",
    sessioncode == "jhr6la2a" & round == 4 ~ "54",
    sessioncode == "jhr6la2a" & round == 5 ~ "55",
    sessioncode == "m2vvsd7b" & round == 1 ~ "56",
    sessioncode == "m2vvsd7b" & round == 2 ~ "57",
    sessioncode == "m2vvsd7b" & round == 3 ~ "58",
    sessioncode == "m2vvsd7b" & round == 4 ~ "59",
    sessioncode == "m2vvsd7b" & round == 5 ~ "60",
    sessioncode == "md2pr0uf" & round == 1 ~ "61",
    sessioncode == "md2pr0uf" & round == 2 ~ "62",
    sessioncode == "md2pr0uf" & round == 3 ~ "63",
    sessioncode == "md2pr0uf" & round == 4 ~ "64",
    sessioncode == "md2pr0uf" & round == 5 ~ "65",
    sessioncode == "oj51j0x5" & round == 1 ~ "66",
    sessioncode == "oj51j0x5" & round == 2 ~ "67",
    sessioncode == "oj51j0x5" & round == 3 ~ "68",
    sessioncode == "oj51j0x5" & round == 4 ~ "69",
    sessioncode == "oj51j0x5" & round == 5 ~ "70", 
    sessioncode == "pk72mxqz" & round == 1 ~ "71",
    sessioncode == "pk72mxqz" & round == 2 ~ "72",
    sessioncode == "pk72mxqz" & round == 3 ~ "73",
    sessioncode == "pk72mxqz" & round == 4 ~ "74",
    sessioncode == "pk72mxqz" & round == 5 ~ "75",
    sessioncode == "qyugo1nr" & round == 1 ~ "76",
    sessioncode == "qyugo1nr" & round == 2 ~ "77",
    sessioncode == "qyugo1nr" & round == 3 ~ "78",
    sessioncode == "qyugo1nr" & round == 4 ~ "79",
    sessioncode == "qyugo1nr" & round == 5 ~ "80",
    sessioncode == "vfgin3ts" & round == 1 ~ "81",
    sessioncode == "vfgin3ts" & round == 2 ~ "82",
    sessioncode == "vfgin3ts" & round == 3 ~ "83",
    sessioncode == "vfgin3ts" & round == 4 ~ "84",
    sessioncode == "vfgin3ts" & round == 5 ~ "85",
    sessioncode == "yvoauhl8" & round == 1 ~ "86",
    sessioncode == "yvoauhl8" & round == 2 ~ "87",
    sessioncode == "yvoauhl8" & round == 3 ~ "88",
    sessioncode == "yvoauhl8" & round == 4 ~ "89",
    sessioncode == "yvoauhl8" & round == 5 ~ "90",
    sessioncode == "zp0scfoz" & round == 1 ~ "91",
    sessioncode == "zp0scfoz" & round == 2 ~ "92",
    sessioncode == "zp0scfoz" & round == 3 ~ "93",
    sessioncode == "zp0scfoz" & round == 4 ~ "94",
    sessioncode == "zp0scfoz" & round == 5 ~ "95",
    sessioncode == "zwgoijjg" & round == 1 ~ "96",
    sessioncode == "zwgoijjg" & round == 2 ~ "97",
    sessioncode == "zwgoijjg" & round == 3 ~ "98",
    sessioncode == "zwgoijjg" & round == 4 ~ "99",
    sessioncode == "zwgoijjg" & round == 5 ~ "100"
  ))

etravel_mean_market_price <- etravel %>%
  mutate(market = case_when(
    sessioncode == "3b5a4rwf" & round == 1 ~ "1",
    sessioncode == "3b5a4rwf" & round == 2 ~ "2",
    sessioncode == "3b5a4rwf" & round == 3 ~ "3",
    sessioncode == "3b5a4rwf" & round == 4 ~ "4",
    sessioncode == "3b5a4rwf" & round == 5 ~ "5",
    sessioncode == "44kexb5g" & round == 1 ~ "6",
    sessioncode == "44kexb5g" & round == 2 ~ "7",
    sessioncode == "44kexb5g" & round == 3 ~ "8",
    sessioncode == "44kexb5g" & round == 4 ~ "9",
    sessioncode == "44kexb5g" & round == 5 ~ "10",
    sessioncode == "53dvdbpt" & round == 1 ~ "11",
    sessioncode == "53dvdbpt" & round == 2 ~ "12",
    sessioncode == "53dvdbpt" & round == 3 ~ "13",
    sessioncode == "53dvdbpt" & round == 4 ~ "14",
    sessioncode == "53dvdbpt" & round == 5 ~ "15",
    sessioncode == "5ut4i0ln" & round == 1 ~ "16",
    sessioncode == "5ut4i0ln" & round == 2 ~ "17",
    sessioncode == "5ut4i0ln" & round == 3 ~ "18",
    sessioncode == "5ut4i0ln" & round == 4 ~ "19",
    sessioncode == "5ut4i0ln" & round == 5 ~ "20",
    sessioncode == "7rii3hlv" & round == 1 ~ "21",
    sessioncode == "7rii3hlv" & round == 2 ~ "22",
    sessioncode == "7rii3hlv" & round == 3 ~ "23",
    sessioncode == "7rii3hlv" & round == 4 ~ "24",
    sessioncode == "7rii3hlv" & round == 5 ~ "25",
    sessioncode == "7x8tbzhg" & round == 1 ~ "26",
    sessioncode == "7x8tbzhg" & round == 2 ~ "27",
    sessioncode == "7x8tbzhg" & round == 3 ~ "28",
    sessioncode == "7x8tbzhg" & round == 4 ~ "29",
    sessioncode == "7x8tbzhg" & round == 5 ~ "30",
    sessioncode == "ap9thekt" & round == 1 ~ "31",
    sessioncode == "ap9thekt" & round == 2 ~ "32",
    sessioncode == "ap9thekt" & round == 3 ~ "33",
    sessioncode == "ap9thekt" & round == 4 ~ "34",
    sessioncode == "ap9thekt" & round == 5 ~ "35",
    sessioncode == "aqekfl2l" & round == 1 ~ "36",
    sessioncode == "aqekfl2l" & round == 2 ~ "37",
    sessioncode == "aqekfl2l" & round == 3 ~ "38",
    sessioncode == "aqekfl2l" & round == 4 ~ "39",
    sessioncode == "aqekfl2l" & round == 5 ~ "40",
    sessioncode == "eni1zs0w" & round == 1 ~ "41",
    sessioncode == "eni1zs0w" & round == 2 ~ "42",
    sessioncode == "eni1zs0w" & round == 3 ~ "43",
    sessioncode == "eni1zs0w" & round == 4 ~ "44",
    sessioncode == "eni1zs0w" & round == 5 ~ "45",
    sessioncode == "iu41zgjt" & round == 1 ~ "46",
    sessioncode == "iu41zgjt" & round == 2 ~ "47",
    sessioncode == "iu41zgjt" & round == 3 ~ "48",
    sessioncode == "iu41zgjt" & round == 4 ~ "49",
    sessioncode == "iu41zgjt" & round == 5 ~ "50",
    sessioncode == "jhr6la2a" & round == 1 ~ "51",
    sessioncode == "jhr6la2a" & round == 2 ~ "52",
    sessioncode == "jhr6la2a" & round == 3 ~ "53",
    sessioncode == "jhr6la2a" & round == 4 ~ "54",
    sessioncode == "jhr6la2a" & round == 5 ~ "55",
    sessioncode == "m2vvsd7b" & round == 1 ~ "56",
    sessioncode == "m2vvsd7b" & round == 2 ~ "57",
    sessioncode == "m2vvsd7b" & round == 3 ~ "58",
    sessioncode == "m2vvsd7b" & round == 4 ~ "59",
    sessioncode == "m2vvsd7b" & round == 5 ~ "60",
    sessioncode == "md2pr0uf" & round == 1 ~ "61",
    sessioncode == "md2pr0uf" & round == 2 ~ "62",
    sessioncode == "md2pr0uf" & round == 3 ~ "63",
    sessioncode == "md2pr0uf" & round == 4 ~ "64",
    sessioncode == "md2pr0uf" & round == 5 ~ "65",
    sessioncode == "oj51j0x5" & round == 1 ~ "66",
    sessioncode == "oj51j0x5" & round == 2 ~ "67",
    sessioncode == "oj51j0x5" & round == 3 ~ "68",
    sessioncode == "oj51j0x5" & round == 4 ~ "69",
    sessioncode == "oj51j0x5" & round == 5 ~ "70", 
    sessioncode == "pk72mxqz" & round == 1 ~ "71",
    sessioncode == "pk72mxqz" & round == 2 ~ "72",
    sessioncode == "pk72mxqz" & round == 3 ~ "73",
    sessioncode == "pk72mxqz" & round == 4 ~ "74",
    sessioncode == "pk72mxqz" & round == 5 ~ "75",
    sessioncode == "qyugo1nr" & round == 1 ~ "76",
    sessioncode == "qyugo1nr" & round == 2 ~ "77",
    sessioncode == "qyugo1nr" & round == 3 ~ "78",
    sessioncode == "qyugo1nr" & round == 4 ~ "79",
    sessioncode == "qyugo1nr" & round == 5 ~ "80",
    sessioncode == "vfgin3ts" & round == 1 ~ "81",
    sessioncode == "vfgin3ts" & round == 2 ~ "82",
    sessioncode == "vfgin3ts" & round == 3 ~ "83",
    sessioncode == "vfgin3ts" & round == 4 ~ "84",
    sessioncode == "vfgin3ts" & round == 5 ~ "85",
    sessioncode == "yvoauhl8" & round == 1 ~ "86",
    sessioncode == "yvoauhl8" & round == 2 ~ "87",
    sessioncode == "yvoauhl8" & round == 3 ~ "88",
    sessioncode == "yvoauhl8" & round == 4 ~ "89",
    sessioncode == "yvoauhl8" & round == 5 ~ "90",
    sessioncode == "zp0scfoz" & round == 1 ~ "91",
    sessioncode == "zp0scfoz" & round == 2 ~ "92",
    sessioncode == "zp0scfoz" & round == 3 ~ "93",
    sessioncode == "zp0scfoz" & round == 4 ~ "94",
    sessioncode == "zp0scfoz" & round == 5 ~ "95",
    sessioncode == "zwgoijjg" & round == 1 ~ "96",
    sessioncode == "zwgoijjg" & round == 2 ~ "97",
    sessioncode == "zwgoijjg" & round == 3 ~ "98",
    sessioncode == "zwgoijjg" & round == 4 ~ "99",
    sessioncode == "zwgoijjg" & round == 5 ~ "100"
  )) %>% 
  filter(sold == 1) %>%
  group_by(market) %>%
  summarise(mean_market_price = mean(ask_price_fin))

# Get the information from the market to the big data set
etravel <- left_join(etravel, etravel_mean_market_price)

# Get only the distinct values of the market for the regression
etravel_unique <- etravel %>% 
  distinct(market, .keep_all = TRUE)
## reg mean price
reg_mean_price <- etravel_unique %>%
  lm(data = ., mean_market_price ~ as.factor(treatment) + as.factor(round) + country)

## Table 2 regression 1
stargazer(reg_mean_price, 
          out = "table2_price.doc",
          model.names = TRUE,
          dep.var.labels = "Market Mean Price",
          covariate.labels = c("Commercial Practices", 
                               "Formal Sanction", 
                               "Informal Sanction",
                               "Regret"),
          add.lines = list(c("Controls", "Yes")),
          se = starprep(reg_mean_price, se_type = "stata", clusters = etravel_unique$sessioncode),
          omit = c("round", "country"),
          multicolumn = TRUE,
          type = "html",
          keep.stat  = "all")

## Filter sellers only
etravel_seller <- etravel %>%
  filter(role == "seller")

etravel_gross <- etravel_seller %>%
  #Modify payoff to make it the gross profit and save it in a new data frame
  mutate(payoff = ifelse(see_list == 1, payoff + 1, payoff),
         payoff = ifelse(times_reported >= 3 & treatment == 3, payoff + times_reported*2, payoff),
         payoff = ifelse(badpractice == 1 & audited == 1 & treatment == 2, payoff + 20, payoff))

# regression for gross profit
reg_gross_profit <- lm(data = etravel_gross, payoff ~ as.factor(treatment) + as.factor(round) + gender + age + education + country)

## transaction closed
reg_sold <- etravel_seller %>%
  lm(data = ., sold ~ as.factor(treatment) + as.factor(round) + gender + age + education + country)

# regresion net profit 
reg_net_profit<- etravel_seller %>%
  lm(data = ., net_profit ~ as.factor(treatment) + as.factor(round) + gender + age + education + country)

## Table 2 regressions 2,3, and 4
stargazer(reg_sold, reg_gross_profit, reg_net_profit, 
          out = "table2_gross_sold_net.doc",
          model.names = TRUE,
          dep.var.labels = c("Transaction Closed", "Gross Profit", "Net Sellers Profits"),
          covariate.labels = c("Commercial Practices", 
                               "Formal Sanction", 
                               "Informal Sanction",
                               "Regret"),
          add.lines = list(c("Controls", "Yes", "Yes", "Yes")),
          se = starprep(reg_sold, reg_gross_profit, reg_net_profit, se_type = "stata", clusters = etravel_seller$sessioncode),
          omit = c("round", "gender", "age", "income", "civil_status", "country", "online_frequ", "education"),
          multicolumn = TRUE,
          type = "html",
          notes = c("The control for regression (1) is country and the dependent variable is the mean price of the closed transactions per market. We use clustered standar errors by session. The controls for regressions (2), (3), and (4) are round, gender, age, income, civil status, country, online frequency, and education with clustered standard errors by session. Regression (4) takes into account the different penalties that sellers could be affected by: being reported by three or more sellers (informal sanction condition), penalty for cheating if audited (formal sanction condition), and costs of seeing their competitors initial prices (available in all conditions)."),
          notes.align = "l")

