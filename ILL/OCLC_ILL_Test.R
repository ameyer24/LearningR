install.packages("tidyverse")
install.packages("data.table")
install.packages("zoo")
library(tidyverse)
library(data.table)
library(zoo)

## Starting to play around with OCLC ILL data.

WorldShare_ILL_Raw_Borrowing <- fread("C:/Users/ameyer/Desktop/OCLC_ILL.csv",skip=8, nrows=9)
WorldShare_ILL_Raw_Lending <- fread("C:/Users/ameyer/Desktop/OCLC_ILL_Lending.csv",skip=9, nrows=9)

## Gather up this data to make it tidy.
## Also renames the "month" column to something more meaningful.
WorldShare_ILL_Tidy <- WorldShare_ILL_Raw_Borrowing %>%
  gather(key="Date", value="Number",-1) %>%
  setnames("Month","Measure") %>%
  mutate(Date = as.yearmon(Date, "%b-%y")) %>%
  rowwise() %>%
  mutate(Number = ifelse(grepl(":", Number),
                         round((as.numeric(unlist(strsplit(Number,":"))[1]))+(as.numeric(unlist(strsplit(Number,":"))[2])/24)+(as.numeric(unlist(strsplit(Number,":"))[3])/1440),4),
                         as.numeric(Number)
                         )
         )

WorldShare_ILL_Tidy_Lending <- WorldShare_ILL_Raw_Lending %>%
  gather(key="Date", value="Number",-1) %>%
  setnames("Month","Measure") %>%
  mutate(Date = as.yearmon(Date, "%b-%y")) %>%
  rowwise() %>%
  mutate(Number = ifelse(grepl(":", Number),
                         round((as.numeric(unlist(strsplit(Number,":"))[1]))+(as.numeric(unlist(strsplit(Number,":"))[2])/24)+(as.numeric(unlist(strsplit(Number,":"))[3])/1440),4),
                         as.numeric(Number)
  )
  )




Graph1 <- WorldShare_ILL_Tidy %>%
  filter(Measure == "Requests Filled" |Measure == "Average Turnaround Time For Filled Requests (dd:hh:mm)") %>%
  ggplot(aes(x= Date, y=Number, group=Measure)) +
  scale_x_yearmon() +
  geom_line() +
  geom_point() +
  geom_smooth()+
  facet_grid(Measure ~ ., scales="free")
Graph1

Graph2 <- WorldShare_ILL_Tidy %>%
  filter(Measure == "Average Turnaround Time For Filled Requests (dd:hh:mm)") %>%
  ggplot(aes(x= Date, y=Number, group=Measure)) +
  scale_x_yearmon() +
  geom_line() +
  geom_point() +
  geom_smooth()
Graph2

Graph3 <- WorldShare_ILL_Tidy %>%
  filter(Measure == "Average Turnaround Time For Filled Requests (dd:hh:mm)") %>%
  ggplot(aes(x= Date, y=Number, group=Measure)) +
  scale_x_yearmon() +
  geom_line()+
  geom_smooth(span = 0.5) +
  geom_point()
Graph3

Graph4 <- WorldShare_ILL_Tidy_Lending %>%
  filter(Measure == "Average Turnaround Time For Filled Requests (dd:hh:mm)") %>%
  ggplot(aes(x= Date, y=Number, group=Measure)) +
  scale_x_yearmon() +
  geom_line()+
  geom_smooth(span = 0.5) +
  geom_point()
Graph4


# labs(title="Average Turnaround Times for Filled Requests", x="Month", y="Time (in days)")