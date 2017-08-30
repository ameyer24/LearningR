###############################################################################
# Overview_____________________________________________________________________
###############################################################################

# This script looks at electronic resources usage and pricing information.
# It ingests DB1 and JR1 Counter Reports (Version 4)
# It allows libraries to import pricing information.
# It then calculates cost per use and other summaries.

###############################################################################
# Installing and Loading Packages _____________________________________________
###############################################################################
install.packages("tidyverse")
install.packages("readxl")
install.packages("xlsx")
install.packages("zoo")
install.packages("mosaic")
install.packages("scales")
library(mosaic)
library(xlsx)
library(tidyverse)
library(readxl)
library(zoo)
library(lubridate)
library(scales)

###############################################################################
# Reading the files and tidying the data ______________________________________
###############################################################################

# Setting up the folders
input.folder <- "C:/DataScience/inputs"
output.folder <- "C:/DataScience/outputs"
DB1folder <- "C:/DataScience/inputs/DB1Reports"
JR1folder <- "C:/DataScience/inputs/JR1Reports"