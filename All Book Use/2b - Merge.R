# Merge the circulation transactions data and the item data.
# Keeps all circ data; includes item data when it matchs by ItemID.

# Merge after transforming!
item.circ.data <- merge(x = circ.data, y= item.data, by = "ItemID", all.x = TRUE)

# Gets only book circulation data.
book.circ.data <- filter(item.circ.data, ItemType=="Books")
