library(tidyverse)
###############################################################################
# Reading the files and tidying the data ______________________________________
###############################################################################

ishare_raw <- "C:/DataScience/inputs/ILL/IShareLendingBorrowing.csv"
ishare_col_names = c("Institution","Lending","Borrowing","Fiscal_Year")

# Importing the data.
ishare_data <- read.csv(file = ishare_raw, col.names = ishare_col_names)
ishare_data_tidy <- gather(ishare_data,2:3, key="Type", value="Value")

###############################################################################
# Exploring the Data___________________________________________________________
###############################################################################

Graph1 <- ishare_data_tidy %>%
  filter(Institution == "North Park University (NPU)") %>%
  ggplot() +
  geom_line(mapping = aes(x = Fiscal_Year, y = Value, color=Type))
Graph1
