###############################################################################
# Visualize - Bar Plots _______________________________________________________
###############################################################################

# Simple Bar Plot with data filtered by factors
# Plotted by Calendar Year
circ.data %>%
  filter(PatronSimple != "ILL") %>%
  ggplot(aes(x=year(ChargeDate))) + 
    geom_bar(aes(fill = ItemSimple2))

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
  facet_grid(ItemSimple2 ~ PatronSimple)

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

###############################################################################
# Book Circulation Visualization_______________________________________________
###############################################################################

# Bar Plot of Circulation by LCLetter
book.circ.data %>%
  ggplot(aes(x=LCLetter)) + 
  geom_bar(aes(fill=ItemSimple2)) +
  facet_grid(AcademicYear ~ .)

# Bar Plot showing Circulation by LCLetter on the acaedemic year.
book.circ.data %>%
  filter(LCLetter == "P") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar(aes(fill=ItemSimple2))

# Barplot showing circulation by LCLetter - just regular circulation.
book.circ.data %>%
  filter(LCLetter == "B") %>%
  filter(ItemSimple2 == "Books - Regular") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar(aes(fill=ItemSimple2))

# Barplot showing circulation by LCLetter - colored by Patron Group
book.circ.data %>%
  filter(LCLetter == "B") %>%
  filter(ItemSimple2 == "Books - Regular") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar(aes(fill=PatronSimple))

# Barplot showing circulation by LCLetter - colored by Patron Group - excluding ILL
book.circ.data %>%
  filter(LCLetter == "M") %>%
  filter(ItemSimple2 == "Books - Regular") %>%
  filter(PatronSimple != "ILL") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar(aes(fill=PatronSimple))
