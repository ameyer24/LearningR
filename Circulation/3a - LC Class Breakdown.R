class <- "QA"

###############################################################################
# Overview _______________________________________________________________
###############################################################################
# How many items are in that LC range?
sample1 <- item_data %>%
  filter(LC_Class == class)

# How many different circulation transactions?
sample2 <- book_circ_data %>%
  filter(LC_Class ==class)

# How many item have circulated?
# group by item ID, count Circ_ID?
sample3 <- sample2 %>%
  group_by(Item_ID) %>%
  count()

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