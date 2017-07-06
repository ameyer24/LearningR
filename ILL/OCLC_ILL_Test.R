install.packages("tidyverse")
install.packages("data.table")
install.packages("zoo")
library(tidyverse)
library(data.table)
library(zoo)

## Starting to play around with OCLC ILL data.

oclc_borrowing_raw <- "C:/DataScience/inputs/ILL/Borrower Activity Overview Report.txt"

oclc_borrowing <- fread(oclc_borrowing_raw,skip=8, nrows=9)


## Gather up this data to make it tidy.
## Also renames the "month" column to something more meaningful.
oclc_borrowing_tidy <- oclc_borrowing %>%
  gather(key="Date", value="Number",-1) %>%
  setnames("Month","Measure") %>%
  mutate(Date = as.yearmon(Date, "%b-%y")) %>%
  rowwise() %>%
  mutate(Number = ifelse(grepl(":", Number),
                         round((as.numeric(unlist(strsplit(Number,":"))[1]))+(as.numeric(unlist(strsplit(Number,":"))[2])/24)+(as.numeric(unlist(strsplit(Number,":"))[3])/1440),4),
                         as.numeric(Number)
                         )
         )

Graph1 <- oclc_borrowing_tidy %>%
  filter(Measure == "Requests Filled" | Measure == "Average Turnaround Time For Filled Requests (dd:hh:mm)") %>%
  ggplot(aes(x= Date, y=Number, group=Measure)) +
  scale_x_yearmon() +
  geom_line() +
  geom_point() +
  geom_smooth()+
  facet_grid(Measure ~ ., scales="free")
Graph1

## Export
oclc_borrowing_export <- oclc_borrowing_tidy %>%
  spread(Date,Number,1) %>%
  write.csv("C:/DataScience/outputs/oclc_borrowing_export.csv")


