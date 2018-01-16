###############################################################################
# Visualize - Bar Plots _______________________________________________________
###############################################################################

# Simple Bar Plot with data filtered by factors
# Plotted by Calendar Year
circ_data %>%
  filter(Patron == "Undergraduate") %>%
  filter(Item_Group == "Laptops") %>%
  ggplot(aes(x=year(Charge_Date))) + 
    geom_bar(aes(fill = Item_Group))

# Bar Plot with facets  
circ_data %>%
ggplot(aes(x=year(Charge_Date))) +
  geom_bar() +
  facet_grid(Item_Group ~ Patron_Group)

# Bar Plot with filters
# X Axis is Academic Year (with some formatting!)
circ_data %>%
  filter(Patron_Group == "Student") %>%
  filter(Item_Group != "Archives") %>%
  ggplot(aes(x=Academic_Year)) + 
  geom_bar() +
  facet_grid(Item_Group ~ .) + 
  theme(axis.text.x = element_text(angle=90))


###############################################################################
# Summarize the Data __________________________________________________________
###############################################################################

circ_data %>%
  filter(Item_Circ_Group == "Book - Regular") %>%
  filter(Academic_Year == "2016 - 2017") %>%
  ggplot(aes(x=Patron_Group)) +
  geom_bar()

