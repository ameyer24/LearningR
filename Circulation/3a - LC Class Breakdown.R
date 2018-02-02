class <- "QA"

###############################################################################
# Overview _______________________________________________________________
###############################################################################
# How many items are in that LC class?
sample1 <- item_data %>%
  filter(LC_Class == class)

# Graph of 
test1 <- function(LC_Range){
  item_data %>%
    filter(LC_Class==LC_Range) %>%
    ggplot(aes(x=Item_Type)) +
    geom_bar()
}

test1(class)

# Add circulation information from circ_data table.
sample2 <- merge(x=sample1, y=circ_data, by="Item_ID", all.x = TRUE)
# Items wiht 0 circs are NA, items with 5 circs are listed 5 times.




###############################################################################
# Book Circulation Data________________________________________________________
###############################################################################

# Book Circulation by LC_Class
book_circ_LC <- function(LC_Range){
  book_circ_data %>%
    filter(LC_Class == LC_Range) %>%
    filter(Item_Circ_Group == "Book - Regular") %>%
    filter(Patron_Group == "Student") %>%
    ggplot(aes(x=Academic_Year)) +
    geom_bar(aes(fill = Patron_Group))
}
book_circ_LC("DS")

book_circ_LC2 <- function(LC){
  book_circ_data %>%
    filter(LC_Letter == LC) %>%
    filter(Item_Circ_Group == "Book - Regular") %>%
    ggplot(aes(x=Academic_Year)) +
    geom_bar(aes(fill = Patron_Group))+
    facet_grid(LC_Class ~ Patron_Group)
}
book_circ_LC2("D")
book_circ_data %>% count(LC_Class)