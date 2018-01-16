###############################################################################
# Visualize - Bar Plots _______________________________________________________
###############################################################################

# Simple Bar Plot with data filtered by factors
# Plotted by Calendar Year
circ.data %>%
  filter(Patron == "Undergraduate") %>%
  filter(ItemGroup2 == "Laptops") %>%
  ggplot(aes(x=year(ChargeDate))) + 
    geom_bar(aes(fill = ItemGroup2))

# Bar Plot with facets  
circ.data %>%
ggplot(aes(x=year(ChargeDate))) +
  geom_bar() +
  facet_grid(ItemGroup2 ~ PatronGroups)

# Bar Plot with filters
# X Axis is Academic Year (with some formatting!)
circ.data %>%
  filter(PatronGroups == "Student") %>%
  filter(ItemGroup2 != "Archives") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar() +
  facet_grid(ItemGroup2 ~ .) + 
  theme(axis.text.x = element_text(angle=90))

# Bar Plot with filters
# X Axis is Academic Year (with some formatting!)
circ.data %>%
  filter(ItemGroup2 != "Archives") %>%
  filter(AcademicYear == "2016 - 2017") %>%
  ggplot(aes(x=ItemGroup2)) + 
  geom_bar(aes(fill = PatronGroups))+ 
  theme(axis.text.x = element_text(angle=90))

###############################################################################
# Summarize the Data __________________________________________________________
###############################################################################

circ.data %>%
  filter(ItemGroup2 == "Books - Regular") %>%
  filter(AcademicYear == "2016 - 2017") %>%
  ggplot(aes(x=PatronGroups)) +
  geom_bar()

  
circ.data %>%
  filter(PatronGroups == "Student") %>%
  filter(AcademicYear == "2016 - 2017") %>%
  ggplot(aes(x=ItemGroup2)) +
  geom_bar()

###############################################################################
# Book Circulation Visualization_______________________________________________
###############################################################################

# Bar Plot of Circulation by LCLetter
book.circ.data %>%
  ggplot(aes(x=LCLetter)) + 
  geom_bar(aes(fill=ItemGroup2)) +
  facet_grid(AcademicYear ~ .)

# Bar Plot showing Circulation by LCLetter on the acaedemic year.
book.circ.data %>%
  filter(LCLetter == "P") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar(aes(fill=ItemGroup2))

# Barplot showing circulation by LCLetter - just regular circulation.
book.circ.data %>%
  filter(LCLetter == "B") %>%
  filter(ItemGroup2 == "Books - Regular") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar(aes(fill=ItemGroup2))

# Barplot showing circulation by LCLetter - colored by Patron Group
book.circ.data %>%
  filter(LCLetter == "B") %>%
  filter(ItemGroup2 == "Books - Regular") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar(aes(fill=PatronGroups))

# Barplot showing circulation by LCLetter - colored by Patron Group - excluding ILL
book.circ.data %>%
  filter(LCLetter == "M") %>%
  filter(ItemGroup2 == "Books - Regular") %>%
  filter(PatronGroups != "ILL") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar(aes(fill=PatronGroups))
