# PROJECT TITLE HERE

# SETUP: README ----------------------------------------------------------------------------------

### This document creates the data objects that will be analyzed.


# SETUP: FUNCTIONS AND OPTIONS ----------------------------------------------------------------------------------

source("./1_r_scripts/1_setup_functions.R") # load the project functions
setwd("/Users/tiernanmartin/Documents/FW/SCIDpda/CID-HousDevAssessment/")

# LOAD DATA: EXCEL FILE FROM H&D SHINY APP ----------------------------------------------------------------------------------

chngPot <- read_csv("./2_inputs/1_raw/Hs_Chng_Pot_2016-06-10.csv")

devPresOpp <- read_csv("./2_inputs/1_raw/DevOp_2016-06-10.csv")

# VIEW DATA:  ----------------------------------------------------------------------------------

# Several different indices were created using different weighting schemes.

# Change Potential
chngPot %>% 
        filter(!is.na(INDEX_A)) %>% 
        select(NAME,INDEX_A,INDEX_S1_B:INDEX_S3_B,NEW_IDX_B) %>%
        arrange(desc(INDEX_A)) %>% 
        View

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



# EXPLORE DATA: Graphs ----------------------------------------------------------------------------------

# Change Potential

cp <- chngPot %>% filter(!is.na(INDEX_A))
        
glimpse(cp)

qplot(data = cp,INDEX_S1_B)


qplot(INDEX_S1_B, data = cp)
qplot(INDEX_S1_B, data = cp, facets = RES_TYPE ~ .)
qplot(RES_TYPE, INDEX_S1_B, data = cp, geom = c("boxplot")) + geom_qq()
qplot(RES_TYPE, INDEX_S1_B, data = cp)
qplot(NAME, data = cp, fill = progress, position = "fill", ylab = "percentage")


HCPScoreByBldg <- {
        make_HCPScoreByBldg <- function(){
                cp1 <- 
                        cp %>% 
                        select(PIN,NAME,CONDITION,RRIO,URM,PRMT_VAL,VLTN_COUNT,OCC_RATE,WILL_SELL,TURNOVER,NEIGHBORS, matches("SCORE_1_")) %>% 
                        mutate(CODENAME = paste("Building",LETTERS[row_number()]))
                cp2 <- 
                        cp1 %>% 
                        select(CODENAME,matches("SCORE_1_")) %>%
                        tidyr::gather(key = CODENAME) %>% 
                        `colnames<-`(c("CODENAME","INDICATOR","SCORE")) %>% 
                        arrange(CODENAME)
                
                NameFctr <- cp2 %>% group_by(CODENAME) %>% summarize(SUM = sum(SCORE,na.rm = T)) %>% arrange(SUM) %>% select(CODENAME) %>% unlist
                IndFctr <- 
                        cp2 %>% 
                        mutate(GOTSCORE = if_else(!is.na(SCORE) & SCORE != 0,1,0)) %>%
                        group_by(INDICATOR) %>% 
                        summarize(COUNT = sum(GOTSCORE)) %>% 
                        arrange(desc(COUNT)) %>% 
                        select(INDICATOR) %>% unlist
                cp3 <-
                        cp2 %>%
                        mutate(CODENAME = factor(CODENAME, levels = ordered(NameFctr)),
                               INDICATOR = factor(INDICATOR, levels = ordered(IndFctr))) %>%
                        mutate(IND_NAME = NA_character_) %>% 
                        mutate(IND_NAME = if_else(INDICATOR == 'SCORE_1_1B','Building Condition',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_2B','RRIO',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_3B','Unreinforced Masonry',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_4B','Permit Value',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_5B','Code Violation',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_6B','Occupancy Rate',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_7B','Owner Willing to Sell',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_8B','Recent Turnover',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_9B','High DevOpp Neighbors',IND_NAME)) %>% 
                        mutate(IND_ORDER = as.numeric(INDICATOR)) %>% 
                        arrange(IND_ORDER) 
                
                cp3_pos <- subset(cp3,SCORE > -1 | is.na(SCORE))
                cp3_neg <- subset(cp3,SCORE <= -1 )
                
                gg1 <- ggplot()
                gg1 <- gg1 + geom_bar(data = cp3_pos,aes(x = ordered(CODENAME), y=SCORE, fill= reorder(IND_NAME,IND_ORDER)),stat="identity")
                gg1 <- gg1 + geom_bar(data = cp3_neg,aes(x = ordered(CODENAME), y=SCORE, fill= reorder(IND_NAME,IND_ORDER)),stat="identity")      
                gg1 <- gg1 + coord_flip()
                gg1 <- gg1 + scale_y_continuous(breaks = seq(-15,60,by = 15),limits = c(-15,60),oob = squish)
                gg1 <- gg1 + theme_classic()
                gg1 <- gg1 + theme(text=element_text(colour = "gray30"))
                # gg1 <- gg1 + theme(axis.text.x=element_blank())
                gg1 <- gg1 + theme(axis.ticks.y=element_blank())
                gg1 <- gg1 + theme(axis.title.y=element_blank())
                gg1 <- gg1 + theme(legend.title=element_blank())
                gg1 <- gg1 + theme(axis.ticks=element_line(colour = "lightgrey"))
                # gg1 <- gg1 + theme(axis.text=element_text(colour = "gray30"))
                gg1 <- gg1 + labs(title = "Housing Change Potential: Score Distribution",
                                  subtitle = "Grouped by Building, Colored by Indicator",
                                  caption = "Note: Permit Value is a negative score")
                gg1 <- gg1 + theme(plot.title=element_text(colour = "black",face = "bold"))
                # gg1 <- gg1 + theme(plot.subtitle=element_text(colour = "gray30"))
                gg1
        }
        plot <- make_HCPScoreByBldg()
        rm(make_HCPScoreByBldg)
        plot
}































