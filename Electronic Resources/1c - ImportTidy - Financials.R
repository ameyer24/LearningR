###############################################################################
# Import Financial Information ________________________________________________
###############################################################################
# This creates a blank template to help with the import of financial data.
financial_template <- DB1 %>%
  mutate(Year = year(Date)) %>%
  distinct(Database, Publisher, Platform, Year) %>%
  mutate(Price ="",
         Notes="",
         Fund="",
         Order_Agent="",
         Order_Term="Fiscal Year") %>%
  spread(Year,Price) %>%
  write_csv(paste(output_folder, "financial_template.csv",sep="/"))

# This creates a path to the financial data file.
# I'm making this explicit (and not relative) because I want to keep this local.
financial_data_file <- "C:/DataScience/inputs/database.price.csv"

# Imports the pricing information file.
financial_data_raw <- read_csv(financial_data_file ,col_names = TRUE)

# Creates a tidy dataframe of just database pricing.
# Keeps only the notes and fund information.
# Excludes databases without pricing.
DB1_financial <- financial_data_raw %>%
  gather(key = Fiscal_Year, value = Cost, 8:16) %>%
  filter(!is.na(Cost)) %>%
  mutate(Cost = as.numeric(Cost))

# Create new variable listing all funds; used in later functions.
all_funds = unique(DB1_financial$Fund)

###############################################################################
# Remove Unneeded Information _________________________________________________
###############################################################################
rm(financial_template, financial_data_file, financial_data_raw)

