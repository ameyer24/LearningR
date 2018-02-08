# Start with book data (all_book_data)
# How many books are there in a category?
div="Mathematics"
items_cat_fun1 <- function(division){
  all_book_data %>%
    filter(Division == division) %>%
    summarize(Item_Count = n_distinct(Item_ID))
}
items_cat_fun1(div)

# How many circulation transactions?
items_cat_fun2 <- function(division){
  all_book_data %>%
    filter(Division == division) %>%
    summarize(Circ_Count = n_distinct(Circ_ID))
}
items_cat_fun2(div)

# Calculate how many items have never circulated.
# Count number of Items with NA for Circ_ID?

###############################################################################
# Circulations by Loan_Cat and Patron Group____________________________________
###############################################################################
items_cat_fun3 <- function(division){
  all_book_data %>%
    filter(Division == division) %>%
    group_by(Loan_Cat_Simple, Patron_Group) %>% 
    summarize(Circ_Count = n_distinct(Circ_ID))
}
items_cat_fun3(div)

