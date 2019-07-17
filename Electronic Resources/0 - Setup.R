###############################################################################
# Installing Packages _________________________________________________________
###############################################################################
install.packages("tidyverse")
install.packages("readxl")
install.packages("xlsx")
install.packages("zoo")
install.packages("mosaic")
install.packages("scales")

###############################################################################
# Loading Packages ____________________________________________________________
###############################################################################
library(tidyverse)
library(readxl)
library(lubridate)
library(mosaic)
library(zoo)
library(scales)
library(knitr)

###############################################################################
# Setting up the Working Directory ____________________________________________
###############################################################################

getwd()
DB1_folder <- "./DB1_reports"
JR1_folder <- "./JR1_reports"
output_folder <- "./outputs"
