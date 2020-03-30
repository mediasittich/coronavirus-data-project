library(coronavirus)
library(dplyr)


data("coronavirus")

head(coronavirus)
update_datasets()

coronavirus %>%
  filter(type == "confirmed") %>%
  group_by(Country.Region) %>%
  summarise(total = sum(cases)) %>%
  arrange(-total) %>%
  head(10)
