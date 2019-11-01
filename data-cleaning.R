### Connecting data base and checking data

library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)

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
View(join_full)


join_full <- join_full %>%  
              filter(join_full$state == "Completed")

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
  filter(ratio < 0.2)


## filtering out of line_item the relevant id_order

final_table <- line_item[line_item$id_order %in% diff_table$id_order, ]


kim[kim$a %in% sunny, ]



summary(diff_table$diff_total_paid)
str(diff_table)


## exploring difference in total paid

hist(diff_table$diff_total_paid, 
     main ="Histogram of the difference in total amount paid",
     breaks = 20, xlim = c(-1000,3000))

ggplot(join_full, mapping = aes(diff_table$diff_total_paid)) + 
        geom_histogram()

ggplot(join_full, mapping = aes(y=diff_table$diff_total_paid, x=total_paid)) + 
        geom_jitter()


## calculating difference in price

diff_table2 <- diff_table %>% 
  mutate(diff_price = abs(products$price-unit_price))

str(diff_table2)
summary(join_full)


## plots and histograms

hist(diff_table2$diff_price,
     main ="Histogram of the difference in price")

ggplot(diff_table2, mapping = aes(diff_price)) + 
  geom_histogram()

ggplot(diff_table2, mapping = aes(x=diff_price, y=diff_total_paid)) + 
  geom_jitter()


## excluding outliers

excl_outl <- diff_table2 %>% filter(diff_total_paid < 5000)

ggplot(excl_outl, mapping = aes(x=diff_price, y=diff_total_paid)) + 
  geom_jitter()

full_table <- diff_table2 %>% filter(diff_total_paid < 5000 & total_paid < 6000)
full_table <- full_table %>% filter(price_ratio < 20 & total_ratio < 0.2)


## calculating ratio of total paid

full_table <- full_table %>% mutate(total_ratio= 
                                    (diff_total_paid / total_paid)*100)

hist(full_table$total_ratio)


## comparing the ratios

summary(full_table$total_ratio)
ggplot(full_table, mapping = aes(x=price_ratio, y=total_ratio)) + geom_jitter()
ggplot(full_table2, mapping = aes(x=price_ratio, y=total_ratio)) + geom_jitter()


