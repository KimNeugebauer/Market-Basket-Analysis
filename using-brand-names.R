### Connecting data base and loading libraries

library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(arules)



# 2. Connecting to db
db_user <- 'data_student_berlin'
db_password <- 'waai_has_shitty_internet'
db_name <- 'pricehub'
db_host <- '34.89.228.59' # for local access
db_port <- 3306
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)

# 3. Read data from db
db_table <- 'line_item'
s <- paste0("select * from ", db_table)
rs <- dbSendQuery(mydb, s)
line_item <-  fetch(rs, n = -1)

##Import df 'orders'
db_table <- 'orders'
s <- paste0("select * from ", db_table)
rs <- dbSendQuery(mydb, s)
orders <-  fetch(rs, n = -1)

##Import df 'products'
db_table <- 'products'
s <- paste0("select * from ", db_table)
rs <- dbSendQuery(mydb, s)
products <-  fetch(rs, n = -1)
on.exit(dbDisconnect(mydb))



## exploring tables

str(line_item)
str(products)
summary(orders)
summary(products)
summary(line_item)



## filtering out all not completed orders

orders %>% filter(orders$state == "Cancelled")
orders %>%  filter(orders$state != "Cancelled")

as.factor(orders$state)



## joining line item and orders

join1 <- inner_join(line_item, orders, by="id_order")

head(join1)


## joining previous with products

join_full <- inner_join(join1, products, by="sku")

head(join_full)



## filtering out non completed orders

join_full <- join_full %>%  
  filter(join_full$state == "Completed" | 
           join_full$state == "Pending" |
           join_full$state == "Place order")

summary(join_full)
dim(join_full)



## create column with unit price * quantity

join_full <- join_full %>% 
  mutate(Paid_per_product = unit_price * product_quantity)



## grouping by id_order
## calculating difference in the total amount paid
## keeping only orders with price diff/total paid < 0.2

diff_table <- join_full %>% 
  group_by(id_order) %>%
  summarise(paid_per_order = sum(Paid_per_product),
            total_paid = mean(total_paid)) %>% 
  mutate(diff_total_paid = abs(total_paid-paid_per_order),
         ratio = diff_total_paid/total_paid) %>% 
  filter(ratio < 0.3)



## creating out of line_item a table that contains only relevant rows for id_order

final_table <- line_item[line_item$id_order %in% diff_table$id_order, ]



## taking only id_order and sku into the transactional file

transactional_file <- final_table

transactional_file$product_quantity <- NULL
transactional_file$id <- NULL
transactional_file$unit_price <- NULL
transactional_file$date <- NULL

View(transactional_file)



## excluding transactions of size 1

not1 <- transactional_file %>% 
  group_by(id_order) %>% 
  count(id_order)


not1 <- not1 %>% filter(n != 1)

transactions_not1 <- 
      transactional_file[transactional_file$id_order %in% 
                           not1$id_order, ]


## joining with products table 

transactions_not1_brand <- transactions_not1 %>% 
  left_join(products, by ="sku")


## exchanging sku with brand-categories

transactions_not1_brand$brand_cat <- 
      paste(transactions_not1_brand$brand, 
            transactions_not1_brand$manual_categories)

transactions_not1_brand <- transactions_not1_brand %>% 
  select(id_order,brand_cat)



## deleting missing values

sum(is.na(transactions_not1_brand))
transactions_not1_brand <- na.omit(transactions_not1_brand)



## transforming transactional file using read.transactions

write.csv(transactions_not1_brand, 
          file = "transactions_not1_brand.csv",
          row.names=FALSE)


large_trans_not1_brand <- read.transactions(
  "transactions_not1_brand.csv",
  format = "single", header = TRUE,
  cols = c(1,2),
  sep = ","
)


## Getting to know the large transactions file

large_trans_not1_brand
inspect(head(large_trans_not1_brand))
LIST(head(large_trans_not1_brand))
size(head(large_trans_not1_brand, 300))


## Plotting and imaging the large transactions file

itemFrequencyPlot(large_trans_not1_brand)

itemFrequencyPlot(large_trans_not1_brand, 
                  topN = 15, 
                  type = c("absolute"), 
                  col = rainbow(20),
                  horiz = TRUE,
                  xlab = "Item Frequency, absolute"
)


image(large_trans_not1_brand)


## Cross Table of items purchased together (also testing Chi-Squared)

cross_table <- crossTable(large_trans_not1_brand, 
                          sort = TRUE)

View(cross_table)
cross_table[1:5,1:5]


cross_table_chi <- 
    crossTable(large_trans_not1_brand, 
               measure = "chi")["Apple accessories","Apple smartphone"]

cross_table_chi



## applying the apriori algorithm

rules <- apriori(large_trans_not1_brand, 
                 parameter = list(supp = 0.001,   # 17 rules
                                  conf = 0.6, 
                                  target = "rules"))

rules <- apriori(large_trans_not1_brand, 
                 parameter = list(supp = 0.0005,   # 57 rules
                                  conf = 0.5, 
                                  target = "rules"))

inspect(rules[1:10])

summary(rules)
str(rules)


## applying the apriori algorithm for rules that lead to buying apple accessories

rules_apple_acc <- apriori(large_trans_not1_brand, 
                          parameter = list(supp = 0.0005,   # 57 rules
                                  conf = 0.5, 
                                  target = "rules"),
                          appearance = list(rhs = "Apple accessories", 
                                   default = "lhs"))


inspect(rules_apple_acc)

summary(rules_apple_acc)
str(rules_apple_acc)


## frequent items

frequentItems <- eclat (large_trans_not1_brand, 
                        parameter = list(supp = 0.01, maxlen = 15))

inspect(sort(frequentItems, 
             decreasing = TRUE, 
             by = "support")[1:10])

View(frequentItems)



## checking confidence and lift

rules_conf <- sort(rules, by = "confidence", desc = TRUE)
inspect(rules_conf[1:10])

rules_lift <- sort(rules, by = "lift", desc = TRUE)
inspect(rules_lift[1:10])

