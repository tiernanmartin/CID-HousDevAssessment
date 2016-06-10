# PROJECT TITLE HERE

# SETUP: README ----------------------------------------------------------------------------------

### This document creates the data objects that will be analyzed.


# SETUP: FUNCTIONS AND OPTIONS ----------------------------------------------------------------------------------

source("./1_r_scripts/1_setup_functions.R") # load the project functions
setwd("/Users/tiernanmartin/Documents/FW/SCIDpda/CID-HousDevAssessment/")

# LOAD DATA: EXCEL FILE FROM H&D SHINY APP ----------------------------------------------------------------------------------

devPresOpp <- read_csv("./2_inputs/1_raw/DevOp_2016-06-10.csv")

chngPot <- read_csv("./2_inputs/1_raw/Hs_Chng_Pot_2016-06-10.csv")


# EXPLORE DATA:  ----------------------------------------------------------------------------------

# Several different indices were created using different weighting schemes.

# Development Opportunity
devPresOpp %>% 
        select(PROP_NAME,INDEX_D_A,INDEX_D_B,INDEX_PM) %>% 
        mutate(
                FILTER = if_else(is.na(INDEX_D_A)&is.na(INDEX_D_B)&is.na(INDEX_PM),
                       FALSE,TRUE)
               ) %>% 
        filter(FILTER) %>% 
        select(-FILTER) %>% 
        arrange(desc(INDEX_D_A)) %>% 
        View

# Preservation Opportunity
devPresOpp %>% 
        select(PROP_NAME,INDEX_P_A,INDEX_P_B) %>% 
        mutate(
                FILTER = if_else(is.na(INDEX_P_A)&is.na(INDEX_P_B),
                                 FALSE,TRUE)
        ) %>% 
        filter(FILTER) %>% 
        select(-FILTER) %>% 
        arrange(desc(INDEX_P_A)) %>% 
        View

# Change Potential
chngPot %>% 
        filter(!is.na(INDEX_A)) %>% 
        select(NAME,INDEX_A,INDEX_S1_B:INDEX_S3_B,NEW_IDX_B) %>%
        arrange(desc(INDEX_A)) %>% 
        View

