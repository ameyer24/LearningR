###############################################################################
# Import and Tidy Acq Information ____________________________________________
###############################################################################
# Path to the acq informaiton file
acq_file <- "C:/DataScience/inputs/Circulation/acqinformation.csv"
# Column names for acq data
acq_col_names = c("Line_Item_ID","Ledger","Fund","Create_Date","Price","Title","Item_ID")
# This is the acq information
acq_data <- read.csv(acq_file, col.names = acq_col_names)

# Subset the information for testing
history_acq <- filter(acq_data, Fund == "History")

# Sometimes there are multiple item records per line item
# One order, two books
# Can I discard those? Otherwise deal with them?
history_acq <- history_acq %>%
  distinct(Line_Item_ID, .keep_all = TRUE)

# Merge acq and circ information
acq_circ_data <- merge(x = history_acq,
                       y= circ_data,
                       by = "Item_ID",
                       all.x = TRUE)

# How many history books ordered per year?
history_acq %>%
  group_by(Ledger) %>%
  summarize(Distinct_Line_Items = n_distinct(Line_Item_ID))

# List of titles for year
history_books <- history_acq %>%
  filter(Ledger=="2017-2018") %>%
  arrange(Title)

  