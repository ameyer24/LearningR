###############################################################################
# Import Pricing Information___________________________________________________
###############################################################################

# This creates a blank template to help with the import of pricing data.
db.price.template <- DB1 %>%
  mutate(Year = year(Date)) %>%
  distinct(Database, Publisher, Platform, Year) %>%
  mutate(Price ="",
         Notes="",
         Fund="",
         Order_Agent="",
         Order_Term="Fiscal Year") %>%
  spread(Year,Price) %>%
  write_csv(paste(output.folder, "database.price.template.csv",sep="/"))

# Imports the pricing information file.
db.prices.raw <- read_csv(paste(input.folder, "database.price.csv",sep="/"),col_names = TRUE)

# Creates a variable to describe the pricing data information. 
# This sets the number of descriptive columns at 7 (the rest are years)
db.prices.desc <- 7

# Creates a tidy dataframe of just database pricing.
# Keeps only the notes and fund information.
# Excludes databases without pricing.
DB1.fin <- db.prices.raw %>%
  gather(key = Fiscal_Year,
         value = Cost,
         8:16) %>% # Updated to make work; would be great to abstract this!!!
  filter(!is.na(Cost)) %>%
  mutate(Cost = as.numeric(Cost))
  #subset(select = -c(6:7))

# Create new variable listing all funds; used in later functions.
all.funds = unique(DB1.fin$Fund)

# Removes old data to keep everything nice and neat
rm(db.price.template, db.prices.desc, db.prices.raw)

