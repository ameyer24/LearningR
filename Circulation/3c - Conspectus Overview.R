library(reshape2)
# How many items in each division?
item_count <- count(item_data, Division, sort=TRUE)

# How many items in each division and category?
item_count2 <- count(item_data, Division, Category, sort=TRUE)

# How many items in each division and item type?
item_count3 <- count(item_data, Division, Item_Type, sort=TRUE)
# This does the same thing but formats the data into a dataframe.
item_count4 <- dcast(item_data, Division ~ Item_Type)

# how does LC Class compare to division?
item_count3 <- count(item_data,LC_Letter ,Division)

###############################################################################
# Circulation Transactions - by Division _____________________________________
###############################################################################
# Item count within a Division
# filter - only books
div="Physical Education & Recreation"

book_circ_fun1 <- function(division){
  book_circ_data %>% 
  filter(Division == division) %>% 
  dcast(Category ~ Academic_Year)
}
test <- book_circ_fun1(div)

###############################################################################
# Circulation Transactions - by Division _____________________________________
###############################################################################
book_circ_fun2 <- function(division){
  book_circ_data %>% 
  filter(Division == division) %>%
  filter(Patron_Group == "Student") %>%
  #filter(Loan_Cat_Simple == "Regular") %>% 
  group_by(Division, Category, Charge_Year) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=Charge_Year,y=count)) +
  geom_line(aes(color=Category))
}
book_circ_fun2(div)
###############################################################################
# Circulation Transactions - Overview _________________________________________
###############################################################################
book_circ_fun3 <- function(){
  book_circ_data %>% 
    filter(Patron_Group == "Student") %>%
    filter(Loan_Cat_Simple == "Regular") %>% 
    group_by(Division, Charge_Year) %>%
    summarise(count=n()) %>%
    ggplot(aes(x=Charge_Year,y=count)) +
    geom_smooth(aes(color=Division)) +
    scale_y_log10()
}
book_circ_fun3()
