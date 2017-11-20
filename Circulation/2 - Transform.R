###############################################################################
# Transformations - Collapsing Factors ________________________________________
###############################################################################
# Raw data has 15 patron groups - I want to collapse them.
levels(circ.data$Patron)
# Create a new variable with simplified patron groups
circ.data$PatronSimple <- fct_collapse(circ.data$Patron,
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
levels(circ.data$ItemType)
# Create new variable with simplified Item Types
circ.data$ItemSimple <- fct_collapse(circ.data$ItemType,
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
                                                   "EQ-Kindle",
                                                   "EQ-Laptop"),
                                     Other     = c("Games and Realia",
                                                   "Lockers",
                                                   "")
)


###############################################################################
# Transformations - Calculate Loan Periods ____________________________________
###############################################################################
# System does not record what item was on reserve when...
# ...but R can calculate the duration of the loan at time it was charged.

circ.data$LoanPeriod <- as.numeric(difftime(circ.data$DueDate,
                                            circ.data$ChargeDate,
                                            units="hours"))

# Convert the raw difference into loan period categories.
circ.data$LoanCat <- cut(circ.data$LoanPeriod,
                         breaks = c(0,2,3,6,24,190,696,1388,2710,100000),
                         labels = c("0-2 Hour", "3 Hour","6 Hour","24 hour","1 Week","4 Week","8 Week","16 Week","More"))

# Let's make this a lot easier. Under a week or over a week. Reserve or not.
circ.data$LoanCatSimple <- cut(circ.data$LoanPeriod,
                               breaks = c(0,190,100000),
                               labels = c("Reserve","Regular"))


###############################################################################
# Transformations - Add Date Information ______________________________________
###############################################################################
# These don't need to be variables...but are here for reference.
circ.data$ChargeYear    <- year(as.Date(circ.data$ChargeDate))
circ.data$ChargeMonth   <- month(as.Date(circ.data$ChargeDate))
circ.data$ChargeWeekDay <- weekdays(as.Date(circ.data$ChargeDate))
circ.data$ChargeHour    <- hour(circ.data$ChargeDate)

# It would be nice to look at academic year (and not calendar year)
circ.data$AcademicYear  <- circ.data$ChargeYear + (circ.data$ChargeMonth >= 7)
circ.data$AcademicYear  <- factor(paste(circ.data$AcademicYear - 1,
                                        "-",
                                       circ.data$AcademicYear))

###############################################################################
# Transformations - Sort Book Circulations ____________________________________
###############################################################################
# I want to sort book circulations into Reserve and Regular.
# Combine the LoanCatSimple and ItemType Simple for books
circ.data$ItemSimple2 <- ifelse(circ.data$ItemSimple == "Books",
                                ifelse(circ.data$LoanCatSimple == "Reserve",
                                       paste("Books - Reserve"),
                                       paste("Books - Regular")),
                                paste(circ.data$ItemSimple))
