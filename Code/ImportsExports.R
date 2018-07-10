library(tidyverse)

# According to https://www.foreign-trade.com/reference/hscode.htm

hs_codes <- data_frame(
  ProductCode = sprintf("%02d", 1:97),
  ProductCategory = rep(c("Animal", "Vegetable", 
                          "Food", "Mineral", "Chemical", "Plastic", "Leather",
                          "Wood", "Textile", "Footwear", "Stone & Glass", 
                          "Metal", "Machinery", "Transportation", "Misc"), 
                        times = c(5, 10, 9, 3, 11, 2, 3, 6, 14, 4, 4, 12, 2, 4, 8))
)

brazil_exports <- read_csv("Data/BrazilExports2017.csv") %>%
  filter(PartnerName == "World") %>%
  set_names(make.names(names(.))) %>%
  left_join(hs_codes) %>%
  group_by(ProductCategory) %>%
  summarize(TotalTradeMil = sum(TradeValue.in.1000.USD)/1000) %>%
  arrange(desc(TotalTradeMil))
aus_exports <- read_csv("Data/AustraliaExports2017.csv") %>%
  filter(PartnerName == "World") %>%
  set_names(make.names(names(.))) %>%
  left_join(hs_codes) %>%
  group_by(ProductCategory) %>%
  summarize(TotalTradeMil = sum(TradeValue.in.1000.USD)/1000) %>%
  arrange(desc(TotalTradeMil))

japan_imports <- read_csv("Data/JapanImports2017.csv") %>%
  filter(PartnerName == "World") %>%
  set_names(make.names(names(.))) %>%
  left_join(hs_codes) %>%
  group_by(ProductCategory) %>%
  summarize(TotalTradeMil = sum(TradeValue.in.1000.USD)/1000) %>%
  arrange(desc(TotalTradeMil))
qatar_imports <- read_csv("Data/QatarImports2016.csv") %>%
  filter(PartnerName == "World") %>%
  set_names(make.names(names(.))) %>%
  left_join(hs_codes) %>%
  group_by(ProductCategory) %>%
  summarize(TotalTradeMil = sum(TradeValue.in.1000.USD)/1000) %>%
  arrange(desc(TotalTradeMil))

save(brazil_exports, aus_exports, file = "Data/BrazilAusExports.Rdata")
save(japan_imports, qatar_imports, file = "Data/JapanQatarImports.Rdata")
