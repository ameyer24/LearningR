## This script is designed to read JR1 (r4) COUNTER Reports
## I'm going to start with the outline from the DB1 script.

## Install packages.
install.packages("tidyverse")
install.packages("readxl")
install.packages("xlsx")
install.packages("zoo")
install.packages("mosaic")
## Load Packages.
library(mosaic)
library(xlsx)
library(tidyverse)
library(readxl)
library(zoo)
library(lubridate)

## READING THE FILES AND TIDYING THE DATA
## Setting up the folder pathway.
folder <- "C:/Users/ameyer/Desktop/CounterReportsJR1"
export_folder <- "C:/Users/ameyer/Desktop/CounterReportsReportsJR1"

## Define "Cleaner" functions.
## This function reads from CSV files.

JR1r4_CSV_Cleaner <- function(file){
  require(zoo)
  JR1_Import <- read_csv(file, skip=7, col_names = TRUE)
  JR1_Import_Cleaned <- JR1_Import %>%
    subset(select = -c(8,9,10)) %>%
    gather(Date, Usage, 8:ncol(JR1_Import_Cleaned)) %>%
    mutate(Date = as.yearmon(Date, "%b-%Y"))
}

JR1_Import <- read_csv("C:/Users/ameyer/Desktop/CounterReportsJR1/JR1_Article_Requests_by_Journal_1196819.CSV", skip=7, col_names = TRUE)

JR1_Import_Cleaned <- JR1_Import %>%
  subset(select = -c(8,9,10))%>%
  gather(Date, Usage, 8:ncol(JR1_Import_Cleaned)) %>%
  mutate(Date = as.yearmon(Date, "%b-%Y"))

test <- JR1r4_CSV_Cleaner("C:/Users/ameyer/Desktop/CounterReportsJR1/JR1_Article_Requests_by_Journal_1196819.CSV")

## This function reads from Excel files.

JR1r4_xl_Cleaner <- function(file){
  require(zoo)
  x <- read_excel(file, skip=7, col_names = TRUE, col_types = )
  x <- x[-2,]
  x <- subset(x, select = -c(8,9,10))
  x <- gather(x, Date, Usage, 8:ncol(x))
  x$Date <- as.yearmon(x$Date, "%b-%Y")
  return(x)
}


load_csv_data <- function(path) {
  files <- dir(path, pattern ="*.CSV", full.names = TRUE)
  tables <- lapply(files, JR1r4_CSV_Cleaner)
  do.call(rbind, tables)
}

load_xl_data <- function(path) {
  files <- dir(path, pattern ="*.xl*", full.names = TRUE)
  tables <- lapply(files, JR1r4_xl_Cleaner)
  do.call(rbind, tables)
}

Tidy_JR1_data <-rbind(load_xl_data(folder))
