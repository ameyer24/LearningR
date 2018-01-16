# Group and Sum Book Circ Data
summed_book_data <- book.circ.data %>%
  group_by(PatronSimple,ItemSimple2,AcademicYear) %>%
  summarize(CircCount = n())



