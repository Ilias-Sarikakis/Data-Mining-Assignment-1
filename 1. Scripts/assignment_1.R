library(tidyverse)
library(arules)
library(ggplot2)

options(scipen=999)

fig_dir <- "2. Report/figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)


# Loading the data
aisles <- read_csv("0. Data/aisles.csv")
departments <- read_csv("0. Data/departments.csv")
orders <- read_csv("0. Data/orders.csv")
products <- read_csv("0. Data/products.csv")
order_products <- read_csv("0. Data/order_products.csv")


# Functions
#--------------------------------------#
save_plot_png <- function(p, filename, width = 9, height = 5, dpi = 300) {
  out_path <- file.path(fig_dir, filename)
  ggsave(filename = out_path, plot = p, width = width, height = height, dpi = dpi, units = "in")
  invisible(out_path)
}
#--------------------------------------#


#---------------------------------------------------#
####             0. Data Exploration             ####
#---------------------------------------------------#

# Data Description
#------------------------#
# aisles:
# - aisle_id: unique identifier for each aisle
# - aisle: name of the aisle

n_distinct(aisles$aisle_id) # 134 unique aisles

# departments:
# - department_id: unique identifier for each department
# - department: name of the department

n_distinct(departments$department_id) # 21 unique departments

# orders:
# - order_id: unique identifier for each order
# - user_id: unique identifier for each user
# - order_number: the order of the user's orders (1 for the first order, 2 for the second, etc.)
# - order_dow: the day of the week the order was placed (0 for Sunday, 1 for Monday, etc.)
# - order_hour_of_day: the hour of the day the order was placed (0-23)
# - days_since_prior_order: the number of days since the user's last order (NA for the first order)

n_distinct(orders$order_id) # 3421083 unique orders
n_distinct(orders$user_id) # 206209 unique users

orders_per_user <- orders %>%
  group_by(user_id) %>%
  summarise(count = max(order_number)) %>%
  ungroup()
range(orders_per_user$count) # 4 - 100 orders per user
mean(orders_per_user$count) # 16.59037 average orders per user


orders_days <- as.data.frame(round(table(orders$order_dow) / nrow(orders) * 100, 2)) # 0-6 (Sunday-Saturday)
orders_days$Var1 <- factor(orders_days$Var1, levels = as.character(0:6))
p_orders_days <- ggplot(orders_days, aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  labs(
    title = "Orders by day of week",
    x = "Day of week (0=Sun ... 6=Sat)",
    y = "Percent of orders"
  ) +
  theme_minimal()
print(p_orders_days)


# orders_hours <- as.data.frame(round(table(orders$order_hour_of_day) / nrow(orders) * 100, 2)) # 0-6 (Sunday-Saturday)
orders_hours <- orders %>% group_by(order_hour_of_day) %>% summarise(count = n()) %>% ungroup() %>%
  rename(Var1 = order_hour_of_day, Freq = count)
orders_hours$Var1 <- factor(orders_hours$Var1, levels = as.character(0:23))
p_orders_hours <- ggplot(orders_hours, aes(x = Var1, y = Freq)) +
  geom_col(fill = "lightblue") +
  labs(
    title = "Orders by hour of day",
    x = "Hour of day (0-23)",
    y = "Total orders"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
print(p_orders_hours)

# products:
# - product_id: unique identifier for each product
# - product_name: name of the product
# - aisle_id: identifier for the aisle the product belongs to
# - department_id: identifier for the department the product belongs to

n_distinct(products$product_id) # 49688 unique products
n_distinct(products$aisle_id) # 134 unique aisles (matches aisles dataset)
n_distinct(products$department_id) # 21 unique departments (matches departments dataset)

# order_products:
# - order_id: unique identifier for each order
# - product_id: unique identifier for each product in the order
# - add_to_cart_order: the order in which the product was added to the cart (1 for the first product, 2 for the second, etc.)
# - reordered: whether the product was reordered (1 for yes, 0 for no)

n_distinct(order_products$order_id) # 3346083 unique orders (!! Less than orders dataset)
n_distinct(order_products$product_id) # 49688 unique products (matches products dataset)

products_per_order <- order_products %>%
  group_by(order_id) %>%
  summarise(count = max(add_to_cart_order)) %>%
  ungroup()

mean(products_per_order$count) # 10.10707 average products per order
range(products_per_order$count) # 1 - 145 products per order
#------------------------#

# Check for any NAs in the data #
#-------------------------------#
sum(is.na(aisles)) # 0
sum(is.na(departments)) # 0
sum(is.na(orders)) # 206209 - This is expected since the first order for each user has NA for days_since_prior_order
sum(is.na(products)) # 0
sum(is.na(order_products)) # 0

rm(orders_days, orders_hours, orders_per_user, products_per_order) 



#--------------------------------------------------#
####             1. Pruning Process             ####
#--------------------------------------------------#
set.seed(12345)

sample_size <- 200000


# Basket size of each order
basket_sizes <- order_products %>%
  group_by(order_id) %>%
  summarise(basket_size = n_distinct(product_id), .groups = "drop") %>%
  ungroup()

# Calculating the total number of orders for each basket size
# And plotting the distribution of orders by basket size
basket_sizes_orders <- basket_sizes %>%
  group_by(basket_size) %>%
  summarise(Orders = n()) %>%
  ungroup()

p_basket_sizes_full <- ggplot(basket_sizes_orders, aes(x = basket_size, y = Orders)) +
  geom_col(fill = "lightblue") +
  labs(
    title = "Total unique orders per basket size",
    x = "Basket size (# unique products)",
    y = "Total unique orders"
  ) +
  scale_x_continuous(breaks = seq(0, max(basket_sizes_orders$basket_size), by = 10)) +
  theme_minimal()
print(p_basket_sizes_full)
save_plot_png(p_basket_sizes_full, "basket_size_distribution_full.png", width = 6, height = 3)


# Removing orders of size 1 and stratifying the remaining orders into 4 strata: 2-4, 5-9, 10-14, 15+
basket_sizes_2plus <- basket_sizes %>%
  filter(basket_size >= 2) %>%
  mutate(
    stratum = case_when(
      basket_size <= 4 ~ "2-4",
      basket_size <= 9 ~ "5-9",
      basket_size <= 14 ~ "10-14",
      TRUE ~ "15+"
    ),
    stratum = factor(stratum, levels = c("2-4", "5-9", "10-14", "15+"))
  )

# 2-4    5-9    10-14   15+ 
# 20.10  34.72  21.76   23.42 
round(table(basket_sizes_2plus$stratum) / nrow(basket_sizes_2plus) * 100, 2)

# Stratification of baskets between 2-4, 5-9, 10-14, 15+
stratum_dist <- basket_sizes_2plus %>%
  count(stratum, name = "n_full") %>%
  mutate(
    prop_full = n_full / sum(n_full),
    n_target = as.integer(round(prop_full * sample_size))
  )

# Ensure the target sample size is exactly the sample size
diff_n <- sample_size - sum(stratum_dist$n_target)
idx_largest <- which.max(stratum_dist$n_full)
stratum_dist$n_target[idx_largest] <- stratum_dist$n_target[idx_largest] + diff_n

# Named lookup: stratum -> target sample size
n_by_stratum <- setNames(stratum_dist$n_target, as.character(stratum_dist$stratum))

# Randomly sample orders within strata based on the specified sample size
sampled_orders <- basket_sizes_2plus %>%
  group_split(stratum) %>%
  purrr::map_dfr(\(df) {
    s <- as.character(df$stratum[[1]])
    slice_sample(df, n = n_by_stratum[[s]])
  }) %>%
  select(order_id, basket_size, stratum)

# Keep all products within the sampled orders
orders_pruned <- order_products %>%
  filter(order_id %in% sampled_orders$order_id)


# -----------------------------#
# Validation based on the strata
# -----------------------------#
# Basket size of each order
basket_sizes_pruned <- orders_pruned %>%
  group_by(order_id) %>%
  summarise(basket_size = n_distinct(product_id), .groups = "drop") %>%
  ungroup()

# Calculating the total number of orders for each basket size
# And plotting the distribution of orders by basket size
basket_sizes_orders_pruned <- basket_sizes_pruned %>%
  group_by(basket_size) %>%
  summarise(Orders = n()) %>%
  ungroup()

p_basket_sizes_pruned <- ggplot(basket_sizes_orders_pruned, aes(x = basket_size, y = Orders)) +
  geom_col(fill = "lightblue") +
  labs(
    title = "Total unique orders per basket size after pruning",
    x = "Basket size (# unique products)",
    y = "Total unique orders (pruned)"
  ) +
  scale_x_continuous(breaks = seq(0, max(basket_sizes_orders_pruned$basket_size), by = 10)) +
  theme_minimal()
print(p_basket_sizes_pruned)
save_plot_png(p_basket_sizes_pruned, "basket_size_distribution_pruned.png", width = 6, height = 3)

# Removing orders of size 1 and stratifying the remaining orders into 4 strata: 2-4, 5-9, 10-14, 15+
basket_sizes_pruned <- basket_sizes_pruned %>%
  mutate(
    stratum = case_when(
      basket_size <= 4 ~ "2-4",
      basket_size <= 9 ~ "5-9",
      basket_size <= 14 ~ "10-14",
      TRUE ~ "15+"
    ),
    stratum = factor(stratum, levels = c("2-4", "5-9", "10-14", "15+"))
  )

# 2-4    5-9    10-14   15+ 
# 20.10  34.72  21.75   23.42 
round(table(basket_sizes_pruned$stratum) / nrow(basket_sizes_pruned) * 100, 2)



#------------------------------------------------------------#
####             2. Constructing Transactions             ####
#------------------------------------------------------------#
# Constructing transactions based on the aisle of each product
order_products_pruned <- orders_pruned %>%
  left_join(orders %>% select(order_id, order_hour_of_day), by = "order_id") %>%
  left_join(products %>% select(product_id, aisle_id), by = "product_id") %>%
  left_join(aisles %>% select(aisle_id, aisle), by = "aisle_id")

# Keeping the unique aisles per order
order_products_pruned <- order_products_pruned %>%
  distinct(order_id, aisle, .keep_all = TRUE)

# Creating categories for the time of day to indicate
# whether orders were placed in the morning, afternoon, evening or night
order_products_pruned <- order_products_pruned %>%
  select(-c(product_id, add_to_cart_order, reordered)) %>%
  mutate(
    time_of_day = case_when(
      order_hour_of_day >= 6 & order_hour_of_day <= 11 ~ "morning",
      order_hour_of_day >= 12 & order_hour_of_day <= 17 ~ "afternoon",
      order_hour_of_day >= 18 & order_hour_of_day <= 22 ~ "evening",
      TRUE ~ "night"
    ),
    time_item = paste0("TOD_", time_of_day))

# Squashing the data into a list where each entry contains
# the time of day category and the unique aisles of the products in the order 
timing_transactions_list <- order_products_pruned %>%
  group_by(order_id) %>%
  summarise(items = list(c(first(time_item), aisle)), .groups = "drop") %>%
  pull(items)

head(timing_transactions_list)

# Converting the transactions list to a transactions object for association rule mining
transactions_timing <- as(timing_transactions_list, "transactions")


#-----------------------------------------------------------#
####             3. Mining Association Rules             ####
#-----------------------------------------------------------#
# Mining rules associating time of day with products of certain aisles
rules_timing <- apriori(
  transactions_timing,
  parameter = list(supp = 0.01, conf = 0.01, minlen = 2, maxlen = 3)
)

rules_timing_df <- as(rules_timing, "data.frame")
rules_timing_df <- rules_timing_df %>%
  arrange(desc(confidence))

head(rules_timing_df)

#------------------------------------------------------------------#
####             3.1. Rules count by supp/conf grid             ####
#------------------------------------------------------------------#
support_values <- c(0.001, 0.01, 0.1)
confidence_values <- c(0.001, 0.01, 0.1)

rule_grid <- expand.grid(
  support = support_values,
  confidence = confidence_values
)

rules_count_table <- rule_grid %>%
  mutate(
    n_rules = purrr::pmap_int(
      list(support, confidence),
      \(support, confidence) {
        rules_tmp <- apriori(
          transactions_timing,
          parameter = list(supp = support, conf = confidence, minlen = 2, maxlen = 3)
        )
        length(rules_tmp)
      }
    )
  ) %>%
  arrange(desc(support), desc(confidence))

print(rules_count_table)

#     support    confidence   n_rules
# 1   0.100      0.100        81
# 2   0.100      0.010        81
# 3   0.100      0.001        81
# 4   0.010      0.100        6895
# 5   0.010      0.010        7881
# 6   0.010      0.001        7881
# 7   0.001      0.100        90201
# 8   0.001      0.010        120712
# 9   0.001      0.001        121374


#---------------------------------------------------#
####             3.2 Market Insights             ####
#---------------------------------------------------#
rules_without_fruits_vegetables <- rules_timing_df %>%
  filter(
      !grepl("fruits", rules_timing_df$rules) &
      !grepl("vegetables", rules_timing_df$rules)
  )

time_of_day_rules <- rules_timing_df %>%
  filter(grepl("^\\{TOD_", rules_timing_df$rules))

time_of_day_rules_no_fruits_veg <- rules_timing_df %>%
  filter(grepl("^\\{TOD_", rules_timing_df$rules) &
         !grepl("fruits", rules_timing_df$rules) &
         !grepl("vegetables", rules_timing_df$rules))












