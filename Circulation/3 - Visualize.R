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

circ.data %>%
  filter(PatronSimple == "Student") %>%
  ggplot(aes(x=AcademicYear)) + 
  geom_bar() +
  facet_grid(ItemSimple2 ~ .) + 
  theme(axis.text.x = element_text(angle=90))


###############################################################################
# Visualize - Busy at Circulation Desk ________________________________________
###############################################################################

# This isn't great...but it does something.
circ.data %>%
ggplot() +
  geom_tile(aes(x=ChargeWeekDay,
                y=ChargeHour,
                fill = CircID))
