###############################################################################
# Installing Packages _________________________________________________________
###############################################################################
install.packages("tidyverse")
# install.packages("readxl")
# install.packages("xlsx")
# install.packages("zoo")
# install.packages("mosaic")
# install.packages("scales")

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
# Reading the files and tidying the data ______________________________________
###############################################################################

# Setting up the folders
input.folder <- "C:/DataScience/inputs"
output.folder <- "C:/DataScience/outputs"
DB1folder <- "C:/DataScience/inputs/DB1Reports"
JR1folder <- "C:/DataScience/inputs/JR1Reports"
