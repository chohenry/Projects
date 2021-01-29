packages <- c('tidyverse', 'ggridges')
lapply(packages, library, character.only = TRUE)

write.excel <- function (x,row.names=FALSE,col.names=TRUE,...) {
  
  write.table (x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
  
}


df_needs <- read_csv("./data/customer_needs.csv")

# The needs we meet weighted in order by ranking
df_needs %>%
  select(customer_need, recreation, speed, mountain) %>%
  group_by(customer_need) %>%
  summarize(score = sum(recreation * 0.35, speed * 0.40, mountain *0.25) /3) %>%
  arrange(desc(score)) ->temp1

df_segment <- read_csv("./data/segments_by_application.csv")

df_segment %>%
  select(segments, speed, recreation, mountain) %>%
  group_by(segments) %>%
  summarize(score = sum(recreation * 0.35, speed * 0.40, mountain *0.25) /3) %>%
  arrange(desc(score)) -> temp2

df_demand <- read_csv("./data/potential_demand.csv")

df_demand %>%
  select(potential_demand, speed, recreation, mountain) %>%
  head(12) %>%
  group_by(potential_demand) %>%
  summarize(score = sum(recreation * 0.35, speed * 0.40, mountain *0.25) /3) %>%
  arrange(desc(score)) %>%
  ungroup() %>%
  mutate(zscore = (score - mean(score))/ sd(score)) -> df1



df_price <- read_csv("./data/price_willing_to_pay.csv")

df_price %>%
  select(price_willing_to_pay, speed, recreation, mountain) %>%
  group_by(price_willing_to_pay) %>%
  summarize(score = sum(recreation * 0.35, speed * 0.40, mountain *0.25) /3) %>%
  arrange(desc(score)) %>%
  ungroup() %>%
  mutate(zscore = (score - mean(score))/ sd(score)) -> df2


df1 %>%
  left_join(df2, by = c("potential_demand" = "price_willing_to_pay")) %>%
  mutate(new_score = zscore.x + zscore.y) %>%
  arrange(desc(new_score)) -> df4


df_store_price <- read_csv("./data/store_price.csv")

df_store_price %>%
  mutate(zscore_setup = (`Setup/Close Cost` - mean(`Setup/Close Cost`)) / sd(`Setup/Close Cost`),
         zscore_qtupkeep = (`Quarterly Lease Cost` - mean(`Quarterly Lease Cost`)) / sd(`Quarterly Lease Cost`)) %>%
  mutate(total_zscore = zscore_setup + zscore_qtupkeep)-> df3

df3 %>%
  select(City, total_zscore) %>%
  arrange(total_zscore)


df1 %>%
  left_join(df2, by = c("potential_demand" = "price_willing_to_pay")) %>%
  mutate(new_score = zscore.x + zscore.y) %>%
  arrange(desc(new_score)) %>%
  left_join(df3, by = c("potential_demand" = "City")) %>%
  mutate(final_score = new_score + (total_zscore * -1)) %>%
  select(potential_demand, final_score) %>%
  arrange(desc(final_score)) -> df5


write.excel(temp1)
write.excel(temp2)
write.excel(df1)
write.excel(df2)
write.excel(df3)
write.excel(df4)
write.excel(df5)
