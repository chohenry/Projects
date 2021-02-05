packages <- c('tidyverse', 'ggridges')
lapply(packages, library, character.only = TRUE)

write.excel <- function (x,row.names=FALSE,col.names=TRUE,...) {
  write.table (x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


df_demand <- read_csv("./data/potential_demand.csv")

View(df_demand)

df_demand %>%
  gather(key = key, value = value, -region, -potential_demand) %>%
  mutate(value = round(value/4, 0)) %>%
  spread(key = key, value = value) %>%
  filter(potential_demand %in% c('Nantes', 'Melbourne')) %>%
  select(potential_demand, youth, work, recreation, speed, mountain) -> df1

df_pay <- read_csv("./data/price_willing_to_pay.csv")

df_pay %>%
  filter(price_willing_to_pay %in% c('Nantes', 'Melbourne')) %>%
  select(price_willing_to_pay, youth, work, recreation, speed, mountain) -> df2
  

df_segment <- read_csv("./data/segments_by_application.csv")

View(df_segment)

df1 %>%
  left_join(df2, by = c("potential_demand" = "price_willing_to_pay")) %>%
  mutate(recreation = recreation.x * recreation.y,
         speed = speed.x * speed.y,
         mountain = mountain.x * mountain.y) %>%
  select(potential_demand, recreation, speed, mountain) -> temp

write.excel(df2)
