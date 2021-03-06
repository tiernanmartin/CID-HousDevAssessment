# Setup ----
library(useful)  
library(scales)
library(readxl) 
library(stringr)  
library(plyr)
library(knitr)
library(rprojroot) 
library(leaflet)
library(foreign)
library(ggthemes)
library(magrittr)
library(stringr)
library(downloader)
library(webshot)
library(htmltools)
library(gplots)
library(ggmap) 
library(htmlwidgets)
library(readxl) 
library(RColorBrewer)
library(forcats)
library(tidyverse)
library(miscgis)
library(operator.tools) 
library(viridisLite) 
library(sp)
library(sf)
library(googledrive)
options(scipen=999,stringsAsFactors = FALSE)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()

crs_proj <- CRS("+init=epsg:4326") # This project will use WGS 84 projected coordinate system
crs_geog <- CRS("+init=epsg:2285") # Washington State plane CRS

# Load data ----

# The data are stored as csv files, which is one of the format choices that the Shiny interface can output

# Load Change Potential dataframes

chngpot_df <- drive_read(drive_ls("~/Futurewise/YCC/SCIDpda/CID-HousDevAssessment/1-data/2-external",pattern = "Hs_Chng"), read_csv)

# Load Pres/Dev Opportunity dataframes

devpresopp_df <- drive_read(dribble = drive_ls("~/Futurewise/YCC/SCIDpda/CID-HousDevAssessment/1-data/2-external",pattern = "DevOp"),
                            read_fun = read_csv)

# Check out the data ----
# Change Potential
chngpot_df %>% 
        filter(!is.na(INDEX_A)) %>% 
        select(NAME,INDEX_A,INDEX_S1_B:INDEX_S3_B,NEW_IDX_B) %>%
        arrange(desc(INDEX_A)) %>% 
        View

# Development Opportunity
devpresopp_df %>% 
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
devpresopp_df %>% 
        select(PROP_NAME,INDEX_P_A,INDEX_P_B) %>% 
        mutate(
                FILTER = if_else(is.na(INDEX_P_A)&is.na(INDEX_P_B),
                                 FALSE,TRUE)
        ) %>% 
        filter(FILTER) %>% 
        select(-FILTER) %>% 
        arrange(desc(INDEX_P_A)) %>% 
        View

# Change Potential: Plot 1 ----

cp <- chngpot_df

qplot(data = cp,INDEX_S1_B)


qplot(INDEX_S1_B, data = cp)
qplot(INDEX_S1_B, data = cp, facets = RES_TYPE ~ .) 

hcp_by_bldg <- {
        make_HCPScoreByBldg <- function(){
                
                cp <- chngpot_df

                cp1 <- 
                        cp %>% 
                        filter(!is.na(INDEX_A)) %>% 
                        select(NAME, matches("SCORE_1_")) %>% 
                        mutate(CODENAME = paste("Building",make_id(rn = n(),.shuffle = FALSE))) %>% 
                        select(CODENAME,everything())
                cp2 <- 
                        cp1 %>% 
                        select(NAME,CODENAME,matches("SCORE_1_")) %>%
                        tidyr::gather(key = "INDICATOR",value = "SCORE",-NAME,-CODENAME) %>% 
                        arrange(CODENAME)

                cp3 <- 
                        cp2 %>% 
                        mutate(CODENAME_FCT = fct_reorder(factor(CODENAME),SCORE,sum,na.rm = TRUE),
                               NAME_FCT = fct_reorder(factor(NAME),SCORE,sum,na.rm = TRUE),
                               GOTSCORE = if_else(!is.na(SCORE) & SCORE != 0,1,0),
                               IND_NAME = NA_character_,
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_1B','Building Condition',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_2B','RRIO',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_3B','Unreinforced Masonry',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_4B','Permit Value',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_5B','Code Violation',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_6B','Occupancy Rate',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_7B','Owner Willing to Sell',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_8B','Recent Turnover',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_9B','High DevOpp Neighbors',IND_NAME),
                               IND_NAME_FCT = fct_reorder(factor(IND_NAME),GOTSCORE,sum,na.rm = TRUE)) %>% 
                        group_by(CODENAME) %>% 
                        filter(sum(SCORE, na.rm = TRUE) > 0) %>% 
                        ungroup
                        
                
                gg1 <- ggplot(data = cp3, aes(x = CODENAME_FCT, y = SCORE, fill = IND_NAME_FCT))
                gg1 <- gg1 + geom_col(color = "white")
                gg1 <- gg1 + coord_flip()
                gg1 <- gg1 + scale_y_continuous(breaks = seq(-15,60,by = 15),limits = c(-15,60),oob = squish)
                gg1 <- gg1 + scale_fill_hue(h = c(0, 360) + 15, c = 100, l = 70, h.start = 180,
                                            direction = -1, na.value = "grey50")
                gg1 <- gg1 + theme_classic()
                gg1 <- gg1 + theme(text=element_text(colour = "gray30")) 
                gg1 <- gg1 + theme(axis.ticks.y=element_blank())
                gg1 <- gg1 + theme(axis.title.y=element_blank())
                gg1 <- gg1 + theme(legend.title=element_blank())
                gg1 <- gg1 + theme(axis.ticks=element_line(colour = "lightgrey")) 
                gg1 <- gg1 + labs(title = "Housing Change Potential: Score Distribution",
                                  subtitle = "Grouped by Building, Colored by Indicator",
                                  caption = "Note: Permit Value is a negative score")
                gg1 <- gg1 + theme(plot.title=element_text(colour = "black",face = "bold")) 
                gg1
        }
        plot <- make_HCPScoreByBldg()
        rm(make_HCPScoreByBldg)
        plot
}

# Change Potential: Plot 2 ----


hcp_by_rent <- {
        make_hcp_by_rent <- function(){
                
                cp <- chngpot_df
                
                # Note: the YR_BUILT column is mislabeled - it should be "effective year built" (http://www.kingcounty.gov/~/media/Assessor/AreaReports/2010/Commercial/174.ashx)
                
                median_yrbuilt <- median(cp$YR_BUILT,na.rm = TRUE)
                
                yr_built_grps <- c("pre-1975", "1975 or later")
                
                studio_grps <- c( "low", "moderate", "high","no data")
                
                cp1 <- 
                        cp %>% 
                        filter(!is.na(INDEX_A)) %>% 
                        mutate(YR_BUILT_FCT = case_when(
                                YR_BUILT >= median_yrbuilt ~ yr_built_grps[2],
                                TRUE ~ yr_built_grps[1]
                                ) %>% fct_relevel(yr_built_grps),
                               STUDIO_FCT = case_when(
                                       is.na(STUDIO) ~ studio_grps[1],
                                       STUDIO<600 ~ studio_grps[2],
                                       between(STUDIO,600,800) ~ studio_grps[3],
                                       TRUE ~ studio_grps[4]
                               ) %>% fct_relevel(studio_grps)
                               ) %>%
                        select(NAME,matches("STUDIO"),matches("BUILT"),HOUS_UNITS,RES_TYPE,matches("SCORE_1_")) %>% 
                        gather(INDICATOR, SCORE, SCORE_1_1B:SCORE_1_9B) %>% 
                        mutate(SCORE = abs(SCORE),
                               GOTSCORE = if_else(!is.na(SCORE) & SCORE != 0,1,0),
                               IND_NAME = NA_character_,
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_1B','Building Condition',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_2B','RRIO',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_3B','Unreinforced Masonry',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_4B','Permit Value',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_5B','Code Violation',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_6B','Occupancy Rate',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_7B','Owner Willing to Sell',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_8B','Recent Turnover',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_9B','High DevOpp Neighbors',IND_NAME),
                               IND_NAME_FCT = fct_reorder(factor(IND_NAME),GOTSCORE,sum,na.rm = TRUE)) %>% 
                        group_by(YR_BUILT_FCT,IND_NAME_FCT) %>% 
                        summarise(SCORE = sum(SCORE, na.rm = TRUE),
                                  N = paste("n =",n())) 
                
                gg1 <- ggplot(data = cp1, aes(x = YR_BUILT_FCT, y = SCORE, fill = IND_NAME_FCT))       
                gg1 <- gg1 + geom_bar(stat = "identity",position = position_fill(), color = "white")
                gg1 <- gg1 + geom_text(aes(x = YR_BUILT_FCT, y = .05, label = N), color = "white")
                gg1 <- gg1 + scale_y_continuous(labels = percent_format()) 
                gg1 <- gg1 + scale_fill_hue(h = c(0, 360) + 15, c = 100, l = 70, h.start = 180,
                                            direction = -1, na.value = "grey50")
                gg1 <- gg1 + theme_minimal()
                gg1 <- gg1 + theme(text=element_text(colour = "gray30")) 
                gg1 <- gg1 + theme(axis.ticks.y=element_blank())
                gg1 <- gg1 + theme(axis.title.y=element_blank())
                gg1 <- gg1 + theme(legend.title=element_blank())
                gg1 <- gg1 + theme(panel.grid=element_blank()) 
                gg1 
                
                
        }
        plot <- make_hcp_by_rent()
        rm(make_hcp_by_rent)
        plot
}

# Change Potential: Plot 3 ----

hcp_by_vars <- {
        make_hcp_by_vars <- function(){
                
                cp <- chngpot_df
                
                # Check out the possible facet variables
                
                # cp %>% filter(!is.na(INDEX_A)) %>% select(BLDG_SF:RES_TYPE) %>% mutate_if(is_character,factor) %>% skim
                
                
                median_yrbuilt <- median(cp$YR_BUILT,na.rm = TRUE)
                
                yr_built_grps <- c("pre-1975", "1975 or later")
                
                studio_grps <- c( "low", "moderate", "high","no data")
                
                zoning_grps <- c("IDM-75-85", "IDM 75/85-150", "IDR 45/125-240", "IDR/C 125/150-240")
                
                var_lvls <- c("LU_FCT", "YR_BUILT_FCT", "STUDIO_FCT")
                
                var_labels <- c("LAND USE", "EFFECTIVE YEAR BUILT", "STUDIO RENT")
                
                cp2 <- 
                        cp %>% 
                        filter(!is.na(INDEX_A)) %>% 
                        mutate(LU_FCT = case_when(LU %in% "Commercial/Mixed-Use" ~ "Commercial/\nMixed-Use", TRUE ~ LU) %>% factor,
                               YR_BUILT_FCT = case_when(
                                YR_BUILT >= median_yrbuilt ~ yr_built_grps[2],
                                TRUE ~ yr_built_grps[1]
                        ) %>% fct_relevel(yr_built_grps),
                        STUDIO_FCT = case_when(
                                is.na(STUDIO) ~ studio_grps[1],
                                STUDIO<600 ~ studio_grps[2],
                                between(STUDIO,600,800) ~ studio_grps[3],
                                TRUE ~ studio_grps[4]
                        ) %>% fct_relevel(studio_grps)
                        ) %>%
                        select(NAME,LU_FCT,STUDIO_FCT,YR_BUILT_FCT,matches("SCORE_1_")) %>% 
                        gather(INDICATOR, SCORE, SCORE_1_1B:SCORE_1_9B) %>% 
                        gather(VAR_NAME,VAR_LVL,LU_FCT, STUDIO_FCT,YR_BUILT_FCT) %>% 
                        mutate(VAR_NAME = fct_relevel(VAR_NAME, var_lvls) %>% factor(labels = var_labels),
                               VAR_LVL = fct_relevel(VAR_LVL,c(yr_built_grps,studio_grps))) %>%
                        mutate(SCORE = abs(SCORE),
                               GOTSCORE = if_else(!is.na(SCORE) & SCORE != 0,1,0),
                               IND_NAME = NA_character_,
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_1B','Building Condition',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_2B','RRIO',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_3B','Unreinforced Masonry',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_4B','Permit Value',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_5B','Code Violation',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_6B','Occupancy Rate',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_7B','Owner Willing to Sell',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_8B','Recent Turnover',IND_NAME),
                               IND_NAME = if_else(INDICATOR == 'SCORE_1_9B','High DevOpp Neighbors',IND_NAME),
                               IND_NAME_FCT = fct_reorder(factor(IND_NAME),GOTSCORE,sum,na.rm = TRUE)) %>% 
                        group_by(VAR_NAME,VAR_LVL,IND_NAME_FCT) %>% 
                        summarize(SCORE = sum(SCORE, na.rm = TRUE),
                                  N = paste("n =",n())) 
                
                gg1 <- ggplot(data = cp2, aes(x = VAR_LVL, y = SCORE, fill = IND_NAME_FCT))       
                gg1 <- gg1 + geom_bar(stat = "identity",position = position_fill(), color = "white")
                gg1 <- gg1 + geom_text(aes(x = VAR_LVL, y = .05, label = N), color = "white")
                gg1 <- gg1 + facet_wrap(~VAR_NAME,ncol = 2,scales = "free_x",strip.position = "right")
                gg1 <- gg1 + scale_y_continuous(labels = percent_format()) 
                gg1 <- gg1 + scale_fill_hue(h = c(0, 360) + 15, c = 100, l = 70, h.start = 180,
                                            direction = -1, na.value = "grey50")
                gg1 <- gg1 + theme_minimal()
                gg1 <- gg1 + theme(text=element_text(colour = "gray30")) 
                gg1 <- gg1 + theme(axis.ticks.y=element_blank())
                gg1 <- gg1 + theme(axis.title=element_blank())
                gg1 <- gg1 + theme(legend.title=element_blank())
                gg1 <- gg1 + theme(panel.grid=element_blank()) 
                gg1 <- gg1 + theme(legend.position = c(0.75, .25))
                gg1 
                
                
        }
        plot <- make_hcp_by_vars()
        rm(make_hcp_by_vars)
        plot
}

# Development Potential: Plot 1 ----

dev <-  devpresopp_df %>% filter(!is.na(INDEX_D_A))

# Check the levels of some variables
dev %>% select(PU_CAT_DES, LU,LU_DETAIL, ZONING,ZONE_CAT) %>% map_df(fct_infreq, ordered = TRUE) %>% map(table)

# Make Present Use chart


do_by_pu <- {
        make_do_by_pu <- function(){
                
                pu_lvls <- list("Housing/Mixed" = c("Multi-Family","Single Family","Mixed Use"),
                                "Retail" = "Retail/Service",
                                "Parking"= "Parking",
                                "Vacant" = "Vacant",
                                "Other"= "")
                
                
                dev <-  devpresopp_df %>% filter(!is.na(INDEX_D_A))
                
                do2 <- 
                        dev %>% 
                        select(P = PU_CAT_DES,matches("SCORE_\\dDA$|SCORE_D\\dDA")) %>% 
                        mutate(PU_CAT_DES_FCT = case_when(P %in% pu_lvls[["Housing/Mixed"]] ~ "Housing/Mixed",
                                                          P %in% pu_lvls[["Retail"]] ~ "Retail" ,
                                                          P %in% pu_lvls[["Parking"]] ~ "Parking",
                                                          P %in% pu_lvls[["Vacant"]] ~ "Vacant",
                                                          TRUE ~ "Other") %>% fct_relevel(names(pu_lvls))) %>% 
                        gather(IND, SCORE, SCORE_1DA:SCORE_D3DA) %>% 
                        mutate(GOTSCORE = if_else(!is.na(SCORE) & SCORE != 0,1,0)) %>% 
                        mutate(IND_NAME = case_when(IND %in% "SCORE_1DA" ~ "REC_LEVEL",
                                                    IND %in% "SCORE_2DA" ~ "RLTIONSHIP",
                                                    IND %in% "SCORE_3DA" ~ "WILL_SELL",
                                                    IND %in% "SCORE_D1DA" ~ "POT_UNIT52",
                                                    IND %in% "SCORE_D2DA" ~ "CST_UNIT_D",
                                                    IND %in% "SCORE_D3DA" ~ "NO_BLDG",
                                                    TRUE ~ NA_character_)) %>%  
                        mutate(IND_NAME_FCT = fct_reorder(f = IND_NAME,x = SCORE, sum,na.rm = TRUE)) %>% 
                        group_by(PU_CAT_DES_FCT,IND_NAME_FCT) %>% 
                        summarise(SCORE = sum(SCORE, na.rm = TRUE),
                                  N = paste("n =",n()))
                
                gg1 <- ggplot(data = do2, aes(x = PU_CAT_DES_FCT, y = SCORE, fill = IND_NAME_FCT))       
                gg1 <- gg1 + geom_bar(stat = "identity", color = "white")
                gg1 <- gg1 + geom_text(aes(x = PU_CAT_DES_FCT, y = 50, label = N), color = "white")
                gg1 <- gg1 + scale_fill_hue(h = c(0, 360) + 15, c = 100, l = 70, h.start = 270,
                                            direction = -1, na.value = "grey50")
                gg1 <- gg1 + theme_minimal()
                gg1 <- gg1 + theme(text=element_text(colour = "gray30")) 
                gg1 <- gg1 + theme(axis.ticks.y=element_blank())
                gg1 <- gg1 + theme(axis.title=element_blank())
                gg1 <- gg1 + theme(legend.title=element_blank())
                gg1 <- gg1 + theme(panel.grid=element_blank()) 
                gg1 
        }
        plot <- make_do_by_pu()
        rm(make_do_by_pu)
        plot        
}


# Development Potential: Plot 2 ----

do_by_vars <- {
        make_do_by_vars <- function(){
                
                pu_lvls <- list("Housing/\nMixed" = c("Multi-Family","Single Family","Mixed Use"),
                                "Retail" = "Retail/Service",
                                "Parking"= "Parking",
                                "Vacant" = "Vacant",
                                "Other"= "")
               
                pu_grps <- names(pu_lvls)
                
                zoning_grps <- dev %>% pull(ZONE_CAT) %>% fct_infreq() %>% levels %>% c("Other")
                
                var_grps <- unique(c(pu_grps,zoning_grps))
                
                var_lvls <- c("PU_CAT_DES_FCT", "ZONE_CAT_FCT")
                
                var_labels <- c("PRESENT USE", "ZONING")
                
                dev <-  devpresopp_df %>% filter(!is.na(INDEX_D_A))
                
                do2 <- 
                        dev %>% 
                        select(P = PU_CAT_DES, Z = ZONE_CAT,matches("SCORE_\\dDA$|SCORE_D\\dDA")) %>%  
                        mutate(PU_CAT_DES_FCT = case_when(P %in% pu_lvls[["Housing/\nMixed"]] ~ "Housing/\nMixed",
                                                          P %in% pu_lvls[["Retail"]] ~ "Retail" ,
                                                          P %in% pu_lvls[["Parking"]] ~ "Parking",
                                                          P %in% pu_lvls[["Vacant"]] ~ "Vacant",
                                                          TRUE ~ "Other") %>% fct_relevel(names(pu_lvls))) %>% 
                        mutate(ZONE_CAT_FCT = if_else(is.na(Z),"Other",Z) %>% fct_relevel(names(zoning_grps))) %>% 
                        gather(IND, SCORE, SCORE_1DA:SCORE_D3DA) %>% 
                        gather(VAR_NAME,VAR_LVL,PU_CAT_DES_FCT, ZONE_CAT_FCT) %>% 
                        mutate(VAR_NAME = fct_relevel(VAR_NAME, var_lvls) %>% factor(labels = var_labels),
                               VAR_LVL = fct_relevel(VAR_LVL,var_grps) %>% fct_relevel("Other", after = Inf)) %>% 
                        mutate(GOTSCORE = if_else(!is.na(SCORE) & SCORE != 0,1,0)) %>% 
                        mutate(IND_NAME = case_when(IND %in% "SCORE_1DA" ~ "REC_LEVEL",
                                                    IND %in% "SCORE_2DA" ~ "RLTIONSHIP",
                                                    IND %in% "SCORE_3DA" ~ "WILL_SELL",
                                                    IND %in% "SCORE_D1DA" ~ "POT_UNIT52",
                                                    IND %in% "SCORE_D2DA" ~ "CST_UNIT_D",
                                                    IND %in% "SCORE_D3DA" ~ "NO_BLDG",
                                                    TRUE ~ NA_character_)) %>%  
                        mutate(IND_NAME_FCT = fct_reorder(f = IND_NAME,x = SCORE, sum,na.rm = TRUE)) %>% 
                        group_by(VAR_NAME,VAR_LVL,IND_NAME_FCT) %>% 
                        summarise(SCORE = sum(SCORE, na.rm = TRUE),
                                  N = paste("n =",n()))
                
                
                gg1 <- ggplot(data = do2, aes(x = VAR_LVL, y = SCORE, fill = IND_NAME_FCT))       
                gg1 <- gg1 + geom_bar(stat = "identity", color = "white")
                gg1 <- gg1 + geom_text(aes(x = VAR_LVL, y = 100, label = N), color = "white")
                gg1 <- gg1 + facet_wrap(~VAR_NAME,ncol = 2,scales = "free_x",strip.position = "right")
                # gg1 <- gg1 + scale_y_continuous(labels = percent_format()) 
                gg1 <- gg1 + scale_fill_hue(h = c(0, 360) + 15, c = 100, l = 70, h.start = 180,
                                            direction = -1, na.value = "grey50")
                gg1 <- gg1 + theme_minimal()
                gg1 <- gg1 + theme(text=element_text(colour = "gray30")) 
                gg1 <- gg1 + theme(axis.ticks.y=element_blank())
                gg1 <- gg1 + theme(axis.title=element_blank())
                gg1 <- gg1 + theme(legend.title=element_blank())
                gg1 <- gg1 + theme(panel.grid=element_blank()) 
                gg1 <- gg1 + theme(legend.position = c(.9, .85))
                gg1 
                
                
        }
        plot <- make_do_by_vars()
        rm(make_do_by_vars)
        plot
        
}
