
library(dplyr)
library(odbc)
library(DBI)
library(stringr)
library(reshape2)
library(xlsx)
library(config)
library(RPostgreSQL)
options (scipen = 999)
library(RPostgres)

#install.packages("dplyr")
#install.packages("odbc")
#install.packages("DBI")
#install.packages("strings")
#install.packages("reshape2")
#install.packages("config")

# read files
field_targets <- read.csv("Post Processing SummaryV3.0.csv")
library(readx1)
referrals_27to45 <- read_excel ("Pharmacy_volume_hcp_level_JAN_2022-DEC_2022.xlsx")
referrals_27to45 <- referrals_27to45 %>% rename_at('PT_COUNT', 'PT_COUNT_PHARM')
###########################
###### field targets ######
#clean post rep edit field targets
field_targets %>% count (FINAL_CALL_DECK_STATUS)
