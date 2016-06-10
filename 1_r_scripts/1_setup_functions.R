# PROJECT TITLE HERE

# SETUP: README ----------------------------------------------------------------------------------

### This document loads the project's packages and creates any user-defined functions for this specific project.
### Note: some one-time-use "anonymous" functions may be included within other project documents.

# SETUP: LOAD PACKAGES AND PROJECT SETTINGS -------------------------------------------------------

# require(scales)         # for ggplot2 label formatting (e.g., 'dollar', 'percent', ect.)
# require(gplots)         # for converting colors to HEX strings
# require(grDevices)      # for color palettes
# require(rgdal)          # for readOGR and others
# require(sp)             # for spatial objects
# require(leaflet)        # for interactive maps (NOT leafletR here)
# require(plyr)           # for rounding (the `round_any` function)
# require(dplyr)          # for working with data frames
# require(ggplot2)        # for plotting
# require(tigris)
# require(acs)
# require(stringr)        # to pad fips codes
# require(purrr)
# require(magrittr)
# require(downloader)
# require(tmap)
# require(rgeos)
# require(operator.tools) # for the `notin` function
# require(tidyr)          # for the `spread` function
# require(acs)            # for loading US Census data
# require(readr)
# require(rapport)        # for creating a camel case string
# require(VGAM)           # for creating estimate median values for neighborhoods
# require(htmlwidgets)
# require(classInt)       # for setting breaks in graphs (http://bit.ly/1QexSEP)
# require(spdep)          # for identifying spatial neighbors
# require(maptools)       # for combining SpatialPolygonsDataFrames
# require(grid)
# require(gridExtra)
# require(useful)         # for "$150K labeling 
# require(readxl)         # for reading Excel documents
# require(stringr)        # for string extraction



options(scipen=999,stringsAsFactors = FALSE)

crs_proj <- CRS("+init=epsg:4326") # This project will use WGS 84 projected coordinate system
crs_geog <- CRS("+init=epsg:2285") # Washington State plane CRS

# SETUP: MY FUNCTIONS --------------------------------------------------------------------



