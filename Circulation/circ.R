###############################################################################
# Set Up ______________________________________________________________________
###############################################################################
# Load Packages
library(tidyverse)
library(lubridate)
library(data.table)

# Set up Files
circ_raw <- "C:/DataScience/inputs/Circulation/circtransactions.csv"
# Set up the Column Names for the Import File
col_names = c("Circ_ID","Item_ID","Item_Type","Patron","Charge_Date","Due_Date")


###############################################################################
# Import and Tidy _____________________________________________________________
###############################################################################

# Importing the data.
circ_data <- read.csv(file = circ_raw, col.names = col_names)

# Cleaning up the date/time fields.
circ_data$Charge_Date <- parse_date_time(circ_data$Charge_Date,
                                         orders = "m/d/y H:M:S")
circ_data$Due_Date <- parse_date_time(circ_data$Due_Date,
                                         orders = "m/d/y H:M:S")

###############################################################################
# Transformations and Visualizations __________________________________________
###############################################################################

# Bar Plot of Circulation Transactions by Year
ggplot(data = circ_data, aes(x=year(Charge_Date))) +
  geom_bar()

# Bar Plot of Circulation Transactions by Year, Faceted by Patron Group
ggplot(data = circ_data, aes(x=year(Charge_Date))) +
  geom_bar() +
  facet_grid(Patron ~ .)

# Bar Plot of Circulation Transactions by Year, Faceted by Item Type
ggplot(data = circ_data, aes(x=year(Charge_Date))) +
  geom_bar() +
  facet_grid(Item_Type ~ .)

###############################################################################
# Transformations - Collapsing Factors ________________________________________
###############################################################################
# Raw data has 15 patron groups - I want to collapse them.
levels(circ_data$Patron)
# Create a new variable with simplified patron groups
circ_data$PatronSimple <- fct_collapse(circ_data$Patron,
             Student=c("Undergraduate",
                       "Graduate Student"),
             Staff=c("Faculty - Full time",
                     "Faculty - Part time",
                     "Librarian / Library Staff",
                     "Staff"),
             ILL=c("UBIN",
                   "UBLong",
                   "UBNonCirc",
                   "UBReg",
                   "Worldshare ILL" ),
             Other=c("ACTS/ATLA",
                     "Alumni",
                     "Clergy",
                     "Guests and Other")
)

# Bar Plot of Circulation Transactions by Year, Faceted by Simple Patron Group
# This looks much better.
ggplot(data = circ_data, aes(x=year(Charge_Date))) +
  geom_bar() +
  facet_grid(PatronSimple ~ .)


# Raw data has 19 Item Types - I want to collapse them.
levels(circ_data$Item_Type)
# Create new variable with simplified Item Types
circ_data$ItemSimple <- fct_collapse(circ_data$Item_Type,
                                       Archives=c("Arch-Covenant",
                                                  "Archives"),
                                       Books=c("Books",
                                               "Periodicals",
                                               "Reference",
                                               "Reserve--24-Hour",
                                               "Reserve--7-Day",
                                               "Reserve--LUO (2 hr)",
                                               "Theses"),
                                       AV=c("Circulating Video",
                                            "Sound Recording"),
                                       Equipment=c("EQ-1 day",
                                                   "EQ-3 day",
                                                   "EQ-6 hour",
                                                   "EQ-Kindle",
                                                   "EQ-Laptop"),
                                     Other=c("Games and Realia",
                                             "Lockers",
                                             "")
)
ggplot(data = circ_data, aes(x=year(Charge_Date))) +
  geom_bar() +
  facet_grid(ItemSimple ~ .)

ggplot(data = circ_data, aes(x=year(Charge_Date))) +
  geom_bar() +
  facet_grid(ItemSimple ~ PatronSimple)

###############################################################################
# Transformations - Calculate Loan Periods ____________________________________
###############################################################################
# System does not record what item was on reserve when...
# ...but R can calculate the duration of the loan at time it was charged.
circ_data$LoanPeriod <- as.numeric(difftime(circ_data$Due_Date, circ_data$Charge_Date, units="hours"))
# That gives raw values; let's categorize them.
circ_data$LoanCat <- cut(circ_data$LoanPeriod,
                         breaks = c(0,2,3,6,24,190,696,1388,2710,100000),
                         labels = c("0-2 Hour", "3 Hour","6 Hour","24 hour","1 Week","4 Week","8 Week","16 Week","More"))
# That gives raw values; let's categorize them.
# Let's make this a lot easier. Under a week or over a week. Reserve or not.
circ_data$LoanCatSimple <- cut(circ_data$LoanPeriod,
                         breaks = c(0,190,100000),
                         labels = c("Reserve","Regular"))


ggplot(data = circ_data, aes(x=year(Charge_Date))) +
  geom_bar() +
  facet_grid(ItemSimple ~ PatronSimple)

###############################################################################
# Transformations - Filter by Patron Group ____________________________________
###############################################################################

student_circ_data <- filter(circ_data,Patron=="Undergraduate")

###############################################################################
# Visualize - Busy at Circulation Desk ________________________________________
###############################################################################
# extra stuff we don't need
circ_data$ChargeDay <- weekdays(as.Date(circ_data$Charge_Date))
circ_data$ChargeHour <- hour(circ_data$Charge_Date)
# This isn't great...but it does something.
ggplot(data = circ_data) +
  geom_tile(aes(x=ChargeDay,
                  y=ChargeHour,
                  fill = Circ_ID))

###############################################################################
# Transformations - Filter by Item Type ____________________________________
###############################################################################
# Look at only the book data.
book_circ_data <- filter(circ_data,Item_Type=="Books")
# Graph and peak at circ loan periods.
# Filter by students
ggplot(data = filter(book_circ_data, PatronSimple == "ILL"),
                     aes(x=year(Charge_Date))) +
  geom_bar(aes(fill=LoanCatSimple))


eq_circ_data <- filter(circ_data,ItemSimple=="Equipment")
# Graph and peak at circ loan periods.
# Filter by students
ggplot(data = filter(eq_circ_data, PatronSimple == "Student"),
       aes(x=year(Charge_Date))) +
  geom_bar()


bookEQ_circ_data <- circ_data %>%
  filter(Item_Type %in% c("Books",
                          "EQ-1 day",
                          "EQ-3 day",
                          "EQ-6 hour",
                          "EQ-Kindle",
                          "EQ-Laptop"))

ggplot(data = filter(bookEQ_circ_data, PatronSimple == "Student"),
       aes(x=year(Charge_Date))) +
  geom_bar(aes(fill=Item_Type))
  
