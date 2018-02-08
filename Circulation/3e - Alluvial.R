install.packages("alluvial")
library(alluvial)


all_book_data %>%
  filter(Division == div) %>%
  group_by(Loan_Cat_Simple, Patron_Group) %>% 
  summarize(Circ_Count = n_distinct(Circ_ID)) %>% 
  na.omit() -> all_fun_data2


alluvial(all_fun_data2[,1:2], freq=all_fun_data2$Circ_Count)

all_book_data %>%
  filter(Division == div) %>%
  group_by(Category, Loan_Cat_Simple,Patron_Group) %>% 
  summarize(Circ_Count = n_distinct(Circ_ID)) %>% 
  na.omit() -> all_fun_data3

alluvial(all_fun_data3[,1:3],
         freq=all_fun_data3$Circ_Count,
         col = ifelse(all_fun_data3$Patron_Group == "Student", "blue", "red"),
         alpha = 0.7)
