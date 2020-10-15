library(readr)

orders = read_csv("orders-2020.csv", col_types = cols(Refunded = col_character(), Paid = col_character()))

library(tidyverse)

#Testing here to see if the items column contains any @'s. Since it doesn't, we will insert @'s at key locations to split the data later on
orders %>% filter(grepl('@', `Item(s)`))

orders$`Item(s)` = gsub('(?<=\\.[0-9][0-9])(?=[A-Za-z0-9])', ' @ ', orders$`Item(s)`, perl = TRUE)

#Trying to find out how many @'s there are in each row by making a new column that counts the number of @'s
library(stringr)

orders$item_count = str_count(orders$`Item(s)`, '@')+1 #Adding 1 here because each @ represents an EXTRA item, meaning it won't be counting the first item
unique(orders$item_count)

#looks like there are 1-15 items in each row

item_split = strsplit(orders$`Item(s)`, split = ' @ ')
better_item_df = data.frame(Order_ID = rep(orders$`Order ID`, sapply(item_split, length)), item = unlist(item_split))

#Lets split this new dataframe's item column into 4 columns. First we will split into 3 and then create the 4th at the end

even_better_item_df = better_item_df %>%
  separate(item,
           c('item_sku', 'item_name', 'quant_price'),
           ' / ')

almost_best_item_df = even_better_item_df %>%
  separate(quant_price,
           c('quantity', 'price'),
           ' × ')

#Now that that's done we're doing some general data cleanup

#Removing $ and converting from character to numeric

orders$Tax = gsub('[$]', '', orders$Tax)
orders$Tax = as.numeric(gsub(',', '', orders$Tax))

orders$Shipping = gsub('[$]', '', orders$Shipping)
orders$Shipping = as.numeric(gsub(',', '', orders$Shipping))

orders$Discount = gsub('[$]', '', orders$Discount)
orders$Discount = as.numeric(gsub(',', '', orders$Discount))

orders$Refunded = gsub('[$]', '', orders$Refunded)
orders$Refunded = as.numeric(gsub(',', '', orders$Refunded))

orders$Paid = gsub('[$]', '', orders$Paid)
orders$Paid = as.numeric(gsub(',', '', orders$Paid))

orders$`Total Invoiced` = gsub('[$]', '', orders$`Total Invoiced`)
orders$`Total Invoiced` = as.numeric(gsub(',', '', orders$`Total Invoiced`))

orders$`Grand Total` = gsub('[$]', '', orders$`Grand Total`)
orders$`Grand Total` = as.numeric(gsub(',', '', orders$`Grand Total`))

orders$`Gross Profit` = gsub('[$]', '', orders$`Gross Profit`)
orders$`Gross Profit` = as.numeric(gsub(',', '', orders$`Gross Profit`))

almost_best_item_df$price = gsub('[$]', '', almost_best_item_df$price)
almost_best_item_df$price = as.numeric(gsub(',', '', almost_best_item_df$price))

#Use janitor to clean column names
library(janitor)

orders = clean_names(orders)

#Let's create a tax rate for each order ID. Taxes are usually applied after discounts, but depending on the state it will apply either before or after shipping
#I am going to assume it applies before shipping, not after
orders$tax_rate = orders$tax/(orders$gross_profit + orders$discount)

#Let's also get a discount rate
orders$discount_rate = orders$discount/orders$gross_profit


#Create new column in almost_best_item_df that will have a new total_invoiced
almost_best_item_df$quantity = as.numeric(almost_best_item_df$quantity)

almost_best_item_df$gross_profit_2 = almost_best_item_df$quantity * almost_best_item_df$price

#If you run the following 2 lines of code you'll see that there are no differences at all between the paid, total_invoiced, and grand_total columns
#Let's get rid of 2 of the 3 since it's redundant information
which(orders$paid != orders$total_invoiced)
which(orders$paid != orders$grand_total)

orders = subset(orders, select = -c(paid, total_invoiced))

#Random but I forgot to drop the item column since we don't need it anymore so let's do that
orders = subset(orders, select = -item_s)

#Now, we've reached a point where we can use column multiplication to recreate all of the price related columns, with the exception of shipping and refunded
#Refunded isn't much of an issue since there are only 22 rows that have refunds. Additionally, since the refunds match the totals, this would imply that
#the entire order was refunded. So we can just match the order ID's to the refunds and recreate them by copying the price column
#Let's create a new refund column that contains TRUE or FALSE based on whether or not it was refunded
orders$refund_status = !is.na(orders$refunded)

#Shipping is somewhat more annoying, but for this let's go simple and evenly divide shipping amongst each item in the order
#We'll use the item_count column we made for this
orders$shipping_per_item = orders$shipping/orders$item_count

#Let's join the old table into our newer almost_best_item_df table. Only taking the columns we need, and recreating the rest with existing information

#Just made new data frame that contains the items split up into different rows. Going to join this with the old dataset using order id as key
joined_orders = almost_best_item_df %>%
  left_join(orders, by = c('Order_ID' = 'order_id'))

#Now that the tables have been joined, we want to remove the unwanted extra columns from the orders data frame that were added
joined_orders = joined_orders %>%
  select(-c(quantity_ordered, tax, shipping, discount, gross_profit, grand_total, refunded))

#Gross profit and shipping_per_item named incorrectly so rectifying that
joined_orders = joined_orders %>%
  rename(gross_profit = gross_profit_2,
         shipping = shipping_per_item)

#Now we need to recreate new tax, discount, grand_total, and refunded columns
joined_orders$discount = joined_orders$discount_rate*joined_orders$gross_profit
joined_orders$tax = joined_orders$tax_rate*(joined_orders$gross_profit + joined_orders$discount)
joined_orders$grand_total = joined_orders$tax + joined_orders$shipping + joined_orders$discount + joined_orders$gross_profit
joined_orders$refund = 0

for(i in 1:length(joined_orders$refund_status)){
  if(joined_orders$refund_status[i]){
    joined_orders$refund[i] = joined_orders$grand_total[i]
  }
}

#Columns are all there, final step is to rearrange columns for clarity
joined_orders = clean_names(joined_orders)

joined_orders = joined_orders %>%
  relocate(order_number,
           .before = order_id)

joined_orders = joined_orders %>%
  relocate(invoice_number:coupon,
           .after = order_id)

joined_orders = joined_orders %>%
  relocate(price,
           .before = quantity)

joined_orders = joined_orders %>%
  relocate(tax,
           .after = quantity)

joined_orders = joined_orders %>%
  relocate(shipping:grand_total,
           .after = tax)

joined_orders = joined_orders %>%
  relocate(refund,
           .after = grand_total)

#Item count, tax_rate, discount_rate, refund_status are all unnecessary at this point and I would consider removing them but they could be useful for some
#data visualization so why not keep them

#Behold joined_orders in all it's glory. This took roughly 4 hours.
write.csv(joined_orders, 'orders-2020-clean.csv')
