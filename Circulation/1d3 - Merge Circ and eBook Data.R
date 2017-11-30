# Tweak the circulation data to make it tidy by month
month.circ.data <- circ.data %>%
  group_by(ItemGroup2,ChargeYear,ChargeMonth) %>%
  summarize(Usage = n())

# Merge Year and Month for Circ Data

# Tweak eBook Usage
month.eBook.data <- BR1.usage.data
month.eBook.data$ItemGroup2 <- "eBook"
# Divide Year.Mon
month.eBook.data