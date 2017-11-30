# Group and Sum Book Circ Data
summed_book_data <- book.circ.data %>%
  group_by(PatronSimple,ItemSimple2,LCLetter,AcademicYear,ChargeMonth) %>%
  summarize(CircCount = n())

# find NA ItemSimple values

circ.data[!complete.cases(circ.data),]
test <- filter(circ.data, circ.data$CircID =="19482")

