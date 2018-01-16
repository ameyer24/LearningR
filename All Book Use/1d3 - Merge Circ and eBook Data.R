# Tweak the circulation data to make it tidy by month
# Rename columns
month.circ.data <- circ.data %>%
  group_by(ItemGroup2,ChargeYear,ChargeMonth) %>%
  rename("Year" = "ChargeYear") %>%
  rename("Month" = "ChargeMonth") %>%
  summarize(Usage = n())
# Filter; we only have ebook data from 2015 to present.
month.circ.data <- filter(month.circ.data, Year >= 2015)


# Tweak eBook Usage
month.eBook.data <- BR1.usage.data
month.eBook.data$ItemGroup2 <- "eBook"
# Divide Year.Mon
month.eBook.data$Year <- year(month.eBook.data$Date)
month.eBook.data$Month <- month(month.eBook.data$Date)
# Drop NA value - First Drop Book DOI
month.eBook.data$`Book DOI` <- NULL
month.eBook.data <- na.omit(month.eBook.data)

# Group and Sum
month.eBook.data <- month.eBook.data %>%
  group_by(Year,Month,ItemGroup2) %>%
  summarize(Usage = sum(Usage))

# Merge the two dataframes.
# This is the combined usage for print and ebooks (from EBSCO)
month.print.eBook.Usage <- rbind(month.circ.data,month.eBook.data)

# Visualize this.
ggplot(month.print.eBook.Usage, aes(x=Year, y=Usage, fill=ItemGroup2))+
  geom_bar(stat="identity")
