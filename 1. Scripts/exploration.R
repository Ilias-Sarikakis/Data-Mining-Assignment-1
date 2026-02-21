library(tidyverse)

#---------------------------------------------------#
####             0. Data Exploration             ####
#---------------------------------------------------#

# Loading the data
aisles <- read_csv("0. Data/aisles.csv")
departments <- read_csv("0. Data/departments.csv")
orders <- read_csv("0. Data/orders.csv")
products <- read_csv("0. Data/products.csv")
order_products <- read_csv("0. Data/order_products.csv")

# Data Description
#------------------------#
# aisles:
# - aisle_id: unique identifier for each aisle
# - aisle: name of the aisle

# departments:
# - department_id: unique identifier for each department
# - department: name of the department

# orders:
# - order_id: unique identifier for each order
# - user_id: unique identifier for each user
# - order_number: the order of the user's orders (1 for the first order, 2 for the second, etc.)
# - order_dow: the day of the week the order was placed (0 for Sunday, 1 for Monday, etc.)
# - order_hour_of_day: the hour of the day the order was placed (0-23)
# - days_since_prior_order: the number of days since the user's last order (NA for the first order)

n_distinct(orders$user_id) # 206209

# products:
# - product_id: unique identifier for each product
# - product_name: name of the product
# - aisle_id: identifier for the aisle the product belongs to
# - department_id: identifier for the department the product belongs to

# order_products:
# - order_id: unique identifier for each order
# - product_id: unique identifier for each product in the order
# - add_to_cart_order: the order in which the product was added to the cart (1 for the first product, 2 for the second, etc.)
# - reordered: whether the product was reordered (1 for yes, 0 for no)
#------------------------#

# str(aisles)
# str(departments)
# str(orders)
# str(products)
# str(order_products)

# Check for any NAs in the data
sum(is.na(aisles)) # 0
sum(is.na(departments)) # 0
sum(is.na(orders)) # 206209 - This is expected since the first order for each user has NA for days_since_prior_order
sum(is.na(products)) # 0
sum(is.na(order_products)) # 0

# What is the reordering thing exactly?
# Seems to be a binary variable indicating whether a product was ordered for the first time
# or if it has been previously ordered
user_orders <- orders %>% filter(user_id == 112108)
user_ordered_products <- order_products %>% filter(order_id %in% user_orders$order_id)

# Easy way to validate this, just look for the first order per user
# and see if all products have a reorder value of 0
first_order <- orders %>% filter(order_number == 1)
first_order_products <- order_products %>% filter(order_id %in% first_order$order_id)
all(first_order_products$reordered == 0) # TRUE - So the data are consistent


# Proportion of reordered orders (this is inflated since a product can be reordered multiple times)
# 0        1 
# 40.99    59.01 
round(table(order_products$reordered) / nrow(order_products) * 100, 2)

# Let's join the user_id and check again
order_products_with_user <- order_products %>%
  left_join(orders %>% select(order_id, user_id), by = "order_id")

# Keeping the distinct user-product combinations to avoid inflation
# Also keeping the maximum reordered value for each user-product combination 
# to capture if the product was ever ordered by the user multiple times 
order_products_with_user <- order_products_with_user %>%
  distinct(user_id, product_id, reordered, .keep_all = TRUE) %>%
  group_by(user_id, product_id) %>%
  summarise(reordered = max(reordered), .groups = "drop") %>%
  ungroup()

# Now this is surprising, it shows that most items are one offs.
# 0        1 
# 59.86    40.14
round(table(order_products_with_user$reordered) / nrow(order_products_with_user) * 100, 2)

# Now let's check if there are any users with only 1 order to remove them, since I already know they have no reorders
user_orders <- orders %>%
  group_by(user_id) %>%
  summarise(orders = n()) %>%
  ungroup()

mean(user_orders$orders) # 16.59037

# So all users have multiple orders. That's very interesting
# 4 100
range(user_orders$orders) 

# What is the frequency of orders per user?
orders_per_user <- user_orders %>%
  group_by(orders) %>%
  summarise(count = n()) %>%
  ungroup()

mean(orders_per_user$count) # 2125.866

ggplot(orders_per_user, aes(x = orders, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Orders per User", x = "Number of Orders", y = "Count of Users") +
  theme_minimal()







