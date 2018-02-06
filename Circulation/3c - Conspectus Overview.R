# How many items in each division?
item_count <- count(item_data, Division)
# How many items in each division and category?
item_count2 <- count(item_data, Division, Category)
# how does LC Class compare to division?
item_count3 <- count(item_data,LC_Letter ,Division)


# Detailed circulation report
# Look at every circulation transactions within a range.

conspectus_details <- function(category){
  circ_item_data %>%
    filter(Category == category) %>%
    arrange(Charge_Date)
}

test <- conspectus_details("Algebra")

# Summarized report
conspectus_details2 <- function(division){
  circ_item_data %>% 
    filter(Division == division) %>% 
    with(table(Category,Charge_Year))
}
conspectus_details2("Mathematics")
