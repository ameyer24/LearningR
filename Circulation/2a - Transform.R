###############################################################################
# Transformations - Collapsing Factors ________________________________________
###############################################################################
# Raw data has 15 patron groups - I want to collapse them into fewer groups.
levels(circ_data$Patron)
# Create a new variable with simplified patron groups
circ_data$PatronGroups <- fct_collapse(circ_data$Patron,
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
levels(circ_data$ItemType)
# Create new variable with simplified Item Types
circ_data$ItemGroups <- fct_collapse(circ_data$ItemType,
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

circ_data$LoanPeriod <- as.numeric(difftime(circ_data$DueDate,
                                            circ_data$ChargeDate,
                                            units="hours"))

# Convert the raw difference into loan period categories.
# I've updated the parameters to include some absurb loan periods.
# Maybe I should just exclude those instead.
circ_data$LoanCat <- cut(circ_data$LoanPeriod,
                         breaks = c(-1000000000000000,2,3,6,24,190,696,1388,2710,1000000000000000),
                         labels = c("0-2 Hour", "3 Hour","6 Hour","24 hour","1 Week","4 Week","8 Week","16 Week","More"))

# Let's make this a lot easier. Under a week or over a week. Reserve or not.
circ_data$LoanCatSimple <- cut(circ_data$LoanPeriod,
                               breaks = c(-1000000000000000,190,1000000000000000),
                               labels = c("Reserve","Regular"))


###############################################################################
# Transformations - Add Date Information ______________________________________
###############################################################################
# These don't need to be variables...but are here for reference.
circ_data$ChargeYear    <- year(as.Date(circ_data$ChargeDate))
circ_data$ChargeMonth   <- month(as.Date(circ_data$ChargeDate))
circ_data$ChargeWeekDay <- weekdays(as.Date(circ_data$ChargeDate))
circ_data$ChargeHour    <- hour(circ_data$ChargeDate)

# It would be nice to look at academic year (and not calendar year)
circ_data$AcademicYear  <- circ_data$ChargeYear + (circ_data$ChargeMonth >= 7)
circ_data$AcademicYear  <- factor(paste(circ_data$AcademicYear - 1,
                                        "-",
                                       circ_data$AcademicYear))

###############################################################################
# Transformations - Sort Book Circulations ____________________________________
###############################################################################
# I want to sort book circulations into Reserve and Regular.
# Combine the LoanCatSimple and ItemType Simple for books
circ_data$ItemGroup2 <- ifelse(circ_data$ItemGroups == "Books",
                                ifelse(circ_data$LoanCatSimple == "Reserve",
                                       paste("Books - Reserve"),
                                       paste("Books - Regular")),
                                paste(circ_data$ItemGroups))
