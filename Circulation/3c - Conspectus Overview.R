###############################################################################
# Collection Overview _________________________________________________________
###############################################################################
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
# Circulation Transactions - by Category _____________________________________
###############################################################################
book_circ_fun3 <- function(){
  book_circ_data %>% 
    filter(Patron_Group == "Student") %>%
    filter(Loan_Cat_Simple == "Regular") %>% 
    group_by(Division, Charge_Year) %>%
    summarise(count=n()) %>%
    ggplot(aes(x=Charge_Year,y=count)) +
    geom_smooth(aes(color=Division))
}
book_circ_fun3()


###############################################################################
# Circulation Transactions - by Division _____________________________________
###############################################################################
# Look at book circulation within a division

div="Mathematics"

book_circ_fun1 <- function(division){
  book_circ_data %>% 
    filter(Division == division) %>%
    group_by(Category, Academic_Year,Loan_Cat_Simple) %>%
    summarise("Count of Circulations"=n()) %>%
    spread(Academic_Year, "Count of Circulations", fill=0)
}

test <- book_circ_fun1(div)

# Simple line graph over time
book_circ_fun2 <- function(division){
  book_circ_data %>% 
  filter(Division == division) %>%
  filter(Patron_Group == "Student") %>%
  #filter(Loan_Cat_Simple == "Regular") %>% 
  group_by(Division, Category, Charge_Year,Loan_Cat_Simple) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=Charge_Year,y=count)) +
  geom_line(aes(color=Category)) +
  facet_grid(.~Loan_Cat_Simple)
}
book_circ_fun2(div)

# What are the most popular books in each loan category?
book_circ_fun5 <- function(division,loan_cat){
  book_circ_data %>% 
    filter(Division == division) %>%
    filter(Loan_Cat_Simple == loan_cat) %>% 
    group_by(Title, Category) %>%
    summarise(Circ_Count=n()) %>% 
    arrange(Category,desc(Circ_Count))
}
test <- book_circ_fun5(div,"Regular")
test <- book_circ_fun5(div,"Reserve")
