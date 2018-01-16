# Path to the eBook Bibliographic file.
eBook_Bibliographic_Info_EBSCO <- "C:/DataScience/inputs/Circulation/eBook_Bibliographic/EBSCO_OwnedTitle_Report.xlsx"
EBSCO.eBook.bibliographic.data <- read_excel(eBook_Bibliographic_Info_EBSCO, col_names = TRUE)

# Merge usage and bibliographic data.
# Match on ProperityID?
# What the heck can I match on?!?!
# Should I pull data from our ILS?




