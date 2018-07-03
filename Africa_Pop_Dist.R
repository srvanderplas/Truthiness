africa_europe_popdist <- filter(location, simple%in% c("Africa", "Europe")) %>%
  left_join(population) %>%
  select(name, desc, label_lat, label_long,  age, Pct) %>%
  mutate(Pct = as.numeric(Pct)) %>%
  arrange(name, age)

europe_popdist <- filter(location, simple == "Europe") %>%
  left_join(population) %>%
  select(name, desc, label_lat, label_long,  age, Pct) %>%
  mutate(Pct = as.numeric(Pct)) %>%
  arrange(name, age)

africa_popdist <- filter(location, simple == "Africa") %>%
  left_join(population) %>%
  select(name, desc, label_lat, label_long,  age, Pct) %>%
  mutate(Pct = as.numeric(Pct)) %>%
  arrange(name, age)

ggplot(filter(africa_popdist, age == "0-14")) + 
  geom_bar(aes(x = name, y = Pct), fill = "grey", color = "black", stat = "identity") + 
  geom_text(aes(x = name, y = 0, label = name), hjust = 0, vjust = 0.5, nudge_y = 1, angle = 90) + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()) + 
  scale_y_continuous("Percent of the Population 0 - 14 years old") + 
  ggtitle("African Countries - Population Distribution")


ggplot(filter(europe_popdist, age == "0-14")) + 
  geom_bar(aes(x = name, y = Pct), fill = "grey", color = "black", stat = "identity") + 
  geom_text(aes(x = name, y = 0, label = name), hjust = 0, vjust = 0.5, nudge_y = 1, angle = 90) + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()) + 
  scale_y_continuous("Percent of the Population 0 - 14 years old") + 
  ggtitle("European Countries - Population Distribution")
