###############################################################################
# Visualize - Bar Plots _______________________________________________________
###############################################################################


# Simple Bar Plot with data filtered by factors
# Plotted by Calendar Year
circ.data %>%
  filter(ItemSimple == "Books") %>%
  filter(PatronSimple == "Student") %>%
  ggplot(aes(x=year(ChargeDate))) + 
           geom_bar(aes(fill = ItemType))

# Simple Bar Plot with more complicated filtering         
circ.data %>%
  filter(ItemType %in% c("Books",
                          "EQ-1 day",
                          "EQ-3 day",
                          "EQ-6 hour",
                          "EQ-Kindle",
                          "EQ-Laptop")) %>%
ggplot(aes(x=year(ChargeDate))) +
  geom_bar(aes(fill=ItemType))

# Bar Plot with facets  
circ.data %>%
ggplot(aes(x=year(ChargeDate))) +
  geom_bar() +
  facet_grid(ItemSimple ~ PatronSimple)

# Bar Plot with filters
# X Axis is Academic Year (with some formatting!)
circ.data %>%
  filter(PatronSimple == "Student") %>%
  filter(ItemSimple2 != "Archives") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar() +
  facet_grid(ItemSimple2 ~ .) + 
  theme(axis.text.x = element_text(angle=90))

# Bar Plot with filters
# X Axis is Academic Year (with some formatting!)
circ.data %>%
  filter(ItemSimple2 != "Archives") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar() +
  facet_grid(ItemSimple2 ~ PatronSimple) + 
  theme(axis.text.x = element_text(angle=90))

# Bar Plot with filters
# X Axis is Academic Year (with some formatting!)
circ.data %>%
  filter(ItemSimple2 != "Archives") %>%
  filter(AcademicYear == "2016 - 2017") %>%
  ggplot(aes(x=ItemSimple2)) + 
  geom_bar(aes(fill = PatronSimple))+ 
  theme(axis.text.x = element_text(angle=90))

###############################################################################
# Summarize the Data __________________________________________________________
###############################################################################

circ.data %>%
  filter(ItemSimple2 == "Books - Regular") %>%
  filter(AcademicYear == "2016 - 2017") %>%
  ggplot(aes(x=PatronSimple)) +
  geom_bar()

  
circ.data %>%
  filter(PatronSimple == "Student") %>%
  filter(AcademicYear == "2016 - 2017") %>%
  ggplot(aes(x=ItemSimple2)) +
  geom_bar()

circ.data %>%
  filter(ItemSimple2 == "Books - Regular") %>%
  ggplot(aes(x=ChargeDate)) +
  geom_area(stat="count")
