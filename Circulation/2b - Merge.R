# Merge the circulation transactions data and the item data.
# Keeps all circ data; includes item data when it matchs by ItemID.

# Merge after transforming!
circ_item_data <- merge(x = circ_data, y= item_data, by = c("Item_ID","Item_Type"), all.x = TRUE)

# Gets only book circulation data.
book_circ_data <- filter(circ_item_data, Item_Type=="Books")


