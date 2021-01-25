####==========================================================================####
# 1. Initialization
####==========================================================================###

rm(list=ls())
getwd()
setwd("C:/Users/Felipe M/Dropbox/2_CID/4 diversificación Open Evidence (1)/2 Online tourism/paper/efficency_analysis_etravel")

install.packages("tidyverse")
install.packages("readxl")
install.packages("scales")
install.packages('Rmisc', dependencies = TRUE)
install.packages("haven")
install.packages("Hmisc")

library(tidyverse) # disable safe url in options
library(readxl)
require(forcats)
library(plyr)
library(dplyr)
library(Rmisc) #downlad the zip version
library(scales)
library(haven)
library(Hmisc)

####==========================================================================####
# 2. Database
####==========================================================================###

db <- as_tibble(read_dta(file = "for_table_2.dta"))
gdir <- "C:/Users/Felipe M/Dropbox/2_CID/4 diversificación Open Evidence (1)/2 Online tourism/paper/efficency_analysis_etravel/graphs"

####==========================================================================####
# 3. Graphs (2021-01-18)
####==========================================================================###

ggplot(data=db, aes(x=factor(round), y=mean_market_price, group=factor(treatment))) +
  geom_line(aes(linetype=factor(treatment))) +
  scale_y_continuous(name = "Mean Market Price") + 
  scale_linetype_manual(values=c("solid","longdash","dotted", "dotdash", "dashed"),
                        label = c("Control", "CP",  "CP + \nFormal", "CP + \nInformal", "CP + \nRegret")) +
  ggtitle("Panel B") + 
  labs(x = "Round", linetype = "Legend") + 
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggsave("graphs\\mean_market_price.png", dpi = 300)



ggplot(data=db, aes(x=factor(round), y=tr_sum_transactions_closed, group=factor(treatment))) +
  geom_line(aes(linetype=factor(treatment))) +
  scale_y_continuous(name = "Transactions Closed") + 
  scale_linetype_manual(values=c("solid","longdash","dotted", "dotdash", "dashed"),
                        label = c("Control", "CP",  "CP + \nFormal", "CP + \nInformal", "CP + \nRegret")) +
  ggtitle("Panel A") + 
  labs(x = "Round", linetype = "Legend") + 
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggsave("graphs\\transactions_closed.png", dpi = 300)


ggplot(data=db, aes(x=factor(round), y=tr_gross_profit, group=factor(treatment))) +
  geom_line(aes(linetype=factor(treatment))) + 
  scale_y_continuous(name = "Gross Profit") + 
  scale_linetype_manual(values=c("solid","longdash","dotted", "dotdash", "dashed"),
                        label = c("Control", "CP",  "CP + \nFormal", "CP + \nInformal", "CP + \nRegret")) +
  #ggtitle("Net Profits") + 
  labs(x = "Round", linetype = "Legend") + 
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggsave("graphs\\grossprofit_treatment_round.png", dpi = 300)

  

ggplot(data=db, aes(x=factor(round), y=sd_gp, group=factor(treatment))) +
  geom_line(aes(linetype=factor(treatment))) +
  scale_y_continuous(labels = scales::percent, name = "Gross Profit/Gross Profit Control Condition") + 
  scale_linetype_manual(values=c("solid","longdash","dotted", "dotdash", "dashed"),
                        label = c("Control", "CP",  "CP + \nFormal", "CP + \nInformal", "CP + \nRegret")) +
  ggtitle("Panel C") + 
  labs(x = "Round", linetype = "Legend") + 
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggsave("graphs\\grossprofit_treatment_round_sd.png", dpi = 300)


ggplot(data=db, aes(x=factor(round), y=tr_net_profit, group=factor(treatment))) +
  geom_line(aes(linetype=factor(treatment))) +
  scale_y_continuous(name = "Net Profit") + 
  scale_linetype_manual(values=c("solid","longdash","dotted", "dotdash", "dashed"),
                        label = c("Control", "CP",  "CP + \nFormal", "CP + \nInformal", "CP + \nRegret")) +
  #ggtitle("Net Profits") + 
  labs(x = "Round", linetype = "Legend") + 
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggsave("graphs\\netprofit_treatment_round.png", dpi = 300)

ggplot(data=db, aes(x=factor(round), y=sd_np, group=factor(treatment))) +
  geom_line(aes(linetype=factor(treatment))) +
  scale_y_continuous(labels = scales::percent, name = "Net Profit/Net Profit Control Condition") + 
  scale_linetype_manual(values=c("solid","longdash","dotted", "dotdash", "dashed"),
                        label = c("Control", "CP",  "CP + \nFormal", "CP + \nInformal", "CP + \nRegret")) +
  ggtitle("Panel D") + 
  labs(x = "Round", linetype = "Legend") + 
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  ggsave("graphs\\netprofit_treatment_round_sd.png", dpi = 300)
