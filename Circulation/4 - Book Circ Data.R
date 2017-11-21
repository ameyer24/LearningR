levels(book.circ.data$LCClass)
# This creates a factor with the first letter from the LCClass
book.circ.data$LCLetter <- as.factor(substring(book.circ.data$LCClass,1,1))

# Bar Plot of Circulation by LCLetter
book.circ.data %>%
  ggplot(aes(x=LCLetter)) + 
  geom_bar(aes(fill=ItemSimple2)) +
  facet_grid(AcademicYear ~ .)

# Bar Plot showing change in Circ in LCLetter over time.
book.circ.data %>%
  filter(LCLetter == "P") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar(aes(fill=ItemSimple2))

# Bar Plot showing change in Circ in LCLetter over time.
book.circ.data %>%
  filter(LCLetter == "B") %>%
  filter(ItemSimple2 == "Books - Regular") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar(aes(fill=ItemSimple2))


