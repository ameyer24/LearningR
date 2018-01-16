###############################################################################
# Transformations - Collapsing Factors ________________________________________
###############################################################################
# Raw data has 15 patron groups - I want to collapse them into fewer groups.
levels(circ_data$Patron)
# Create a new variable with simplified patron groups
circ_data$Patron_Group <- fct_collapse(circ_data$Patron,
                                       Student = c("Undergraduate",
                                                   "Graduate Student"),
                                       Staff   = c("Faculty - Full time",
                                                   "Faculty - Part time",
                                                   "Librarian / Library Staff",
                                                   "Staff"),
                                       ILL     = c("UBIN",
                                                   "UBLong",
                                                   "UBNonCirc",
                                                   "UBReg",
                                                   "Worldshare ILL" ),
                                       Other   = c("ACTS/ATLA",
                                                   "Alumni",
                                                   "Clergy",
                                                   "Guests and Other")
)

# Raw data has 19 Item Types - I want to collapse them.
levels(circ_data$Item_Type)
# Create new variable with simplified Item Types
circ_data$Item_Group <- fct_collapse(circ_data$Item_Type,
                                     Archives  = c("Arch-Covenant",
                                                   "Archives"),
                                     Books     = c("Books",
                                                   "Periodicals",
                                                   "Reference",
                                                   "Reserve--24-Hour",
                                                   "Reserve--7-Day",
                                                   "Reserve--LUO (2 hr)",
                                                   "Theses"),
                                     AV        = c("Circulating Video",
                                                   "Sound Recording"),
                                     Equipment = c("EQ-1 day",
                                                   "EQ-3 day",
                                                   "EQ-6 hour",
                                                   "EQ-Kindle"),
                                     Laptops   = c("EQ-Laptop"),
                                     Other     = c("Games and Realia",
                                                   "Lockers",
                                                   "")
)


###############################################################################
# Transformations - Calculate Loan Periods ____________________________________
###############################################################################
# System does not record what item was on reserve when...
# ...but R can calculate the duration of the loan at time it was charged.

circ_data$Loan_Period <- as.numeric(difftime(circ_data$Due_Date,
                                            circ_data$Charge_Date,
                                            units="hours"))

# Convert the raw difference into loan period categories.
# I've updated the parameters to include some absurb loan periods.
# Maybe I should just exclude those instead.
circ_data$Loan_Cat <- cut(circ_data$Loan_Period,
                         breaks = c(-1000000000000000,2,3,6,24,190,696,1388,2710,1000000000000000),
                         labels = c("0-2 Hour", "3 Hour","6 Hour","24 hour","1 Week","4 Week","8 Week","16 Week","More"))

# Let's make this a lot easier. Under a week or over a week. Reserve or not.
circ_data$Loan_Cat_Simple <- cut(circ_data$Loan_Period,
                               breaks = c(-1000000000000000,190,1000000000000000),
                               labels = c("Reserve","Regular"))


###############################################################################
# Transformations - Add Date Information ______________________________________
###############################################################################
# These don't need to be variables...but are here for reference.
circ_data$Charge_Year    <- year(as.Date(circ_data$Charge_Date))
circ_data$Charge_Month   <- month(as.Date(circ_data$Charge_Date))
circ_data$Charge_WeekDay <- weekdays(as.Date(circ_data$Charge_Date))
circ_data$Charge_Hour    <- hour(circ_data$Charge_Date)

# It would be nice to look at academic year (and not calendar year)
circ_data$Academic_Year  <- circ_data$Charge_Year + (circ_data$Charge_Month >= 7)
circ_data$Academic_Year  <- factor(paste(circ_data$Academic_Year - 1,
                                        "-",
                                       circ_data$Academic_Year))

###############################################################################
# Transformations - Sort Book Circulations ____________________________________
###############################################################################
# I want to sort book circulations into Reserve and Regular.
# Combine the LoanCatSimple and ItemType Simple for books
circ_data$Item_Circ_Group <- ifelse(circ_data$Item_Group == "Books",
                                ifelse(circ_data$Loan_Cat_Simple == "Reserve",
                                       paste("Book - Reserve"),
                                       paste("Book - Regular")),
                                paste(circ_data$Item_Group))
