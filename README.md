getwd

install.packages("tidyverse")
library(tidyverse)

store <- read.csv("store.csv")
test <- read.csv("test.csv")
train <- read.csv("train.csv")

#記述統計テーブル
summary(store)
summary(test)
summary(train)

#テーブルを繋げる
train_store <- train %>%
  left_join(store, by = "Store")
test_store <- test %>%
  left_join(store, by = "Store")

#NAを除く
train_store_nona <- train_store %>%
  mutate(
    CompetitionDistance = ifelse(is.na(CompetitionDistance), max(CompetitionDistance, na.rm = TRUE), CompetitionDistance),
    CompetitionOpenSinceYear = replace_na(CompetitionOpenSinceYear, 0),
    CompetitionOpenSinceMonth = replace_na(CompetitionOpenSinceMonth, 0),
    Promo2SinceWeek = replace_na(Promo2SinceWeek, 0),
    Promo2SinceYear = replace_na(Promo2SinceYear, 0),
    PromoInterval = replace_na(PromoInterval, "None")
  )
test_store_nona <- test_store %>%
  mutate(
    CompetitionDistance = ifelse(is.na(CompetitionDistance), max(CompetitionDistance, na.rm = TRUE), CompetitionDistance),
    CompetitionOpenSinceYear = replace_na(CompetitionOpenSinceYear, 0),
    CompetitionOpenSinceMonth = replace_na(CompetitionOpenSinceMonth, 0),
    Promo2SinceWeek = replace_na(Promo2SinceWeek, 0),
    Promo2SinceYear = replace_na(Promo2SinceYear, 0),
    PromoInterval = replace_na(PromoInterval, "None")
  )
test_store_full <- test_store_nona %>% mutate(Sales = NA, Customers = NA)  # test_storeの中にNAのSalesとCustomersをいれる
full <- bind_rows(train_store_nona, test_store_full)  #データをミックス

#histogramを作る
full_SalesCustomersNoNA <- full %>%
  filter(!is.na(Sales) & !is.na(Customers))#fullのなかのSalesとCustomersのNAを削除
ggplot(full_SalesCustomersNoNA, aes(x = Sales)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Sales Distribution", x = "Sales", y = "Count") +
  theme_minimal()#Salesのhistogram
ggplot(full_SalesCustomersNoNA, aes(x = Customers)) +
  geom_histogram(binwidth = 100, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Customers Distribution", x = "Customers", y = "Count") +
  theme_minimal()#Customersのhistogram
ggplot(full, aes(x = CompetitionDistance)) +
  geom_histogram(binwidth = 1000, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Competition Distance Distribution", x = "Competition Distance", y = "Count") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 30000))#CompetitionDistanceのhistogram

#Openの日のhistogramを作る
full_SalesCustomersNoNA_open <- full_SalesCustomersNoNA %>%
  filter(Open == 1 & !is.na(Sales) & !is.na(Customers))#fullのなかのSalesとCustomersのNAとOpen＝0の部分を削除
ggplot(full_SalesCustomersNoNA_open, aes(x = Sales)) +
  geom_histogram(binwidth = 1000, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Sales Distribution (Open Stores)", x = "Sales", y = "Count") +
  theme_minimal()# Sales　Openのhistogram
ggplot(full_SalesCustomersNoNA_open, aes(x = Customers)) +
  geom_histogram(binwidth = 100, fill = "darkgreen", color = "black", alpha = 0.7) +
  labs(title = "Customers Distribution (Open Stores)", x = "Customers", y = "Count") +
  theme_minimal()#Customers　Openのhistogram

#カーネル密度関数
sales_kernel <- ggplot(full_SalesCustomersNoNA_open, aes(Sales)) + geom_density(kernel = "gaussian",bw = 1000.0)
print(sales_kernel)#salesのカーネル密度関数図
customers_kernel <- ggplot(full_SalesCustomersNoNA_open, aes(Customers)) + geom_density(kernel = "gaussian")
print(customers_kernel)#customersのカーネル密度関数図
competitiondistance_kernel <- ggplot(full, aes(CompetitionDistance)) + geom_density(kernel = "gaussian",bw = 1000.0)
print(competitiondistance_kernel)#CompetitionDistanceのカーネル密度関数図

#散布図
sales_customers <- ggplot(full_SalesCustomersNoNA_open, aes(Sales, Customers)) + geom_point()#SalesとCustomersの間の散布図
ggsave("sales_customers.png",plot = sales_customers,width = 8,height = 6,dpi = 300)
sales_competitiondistance <- ggplot(full_SalesCustomersNoNA_open, aes(Sales, CompetitionDistance)) + geom_point()#SalesとCompetitionDistanceの間の散布図
ggsave("sales_competitiondistance.png",plot = sales_competitiondistance,width = 8,height = 6,dpi = 300)
competitiondistance_customers <- ggplot(full_SalesCustomersNoNA_open, aes(CompetitionDistance,Customers)) + geom_point()#CustomersとCompetitionDistanceの間の散布図
ggsave("competitiondistance_customers.png",plot = competitiondistance_customers,width = 8,height = 6,dpi = 300)

#箱ひげ図
sales_customers_box <- ggplot(full_SalesCustomersNoNA_open, aes(Sales, Customers)) + geom_boxplot()#SalesとCustomersの間の箱ひげ図
ggsave("sales_customers_box.png",plot = sales_customers_box,width = 8,height = 6,dpi = 300)
sales_competitiondistance_box <- ggplot(full_SalesCustomersNoNA_open, aes(Sales, CompetitionDistance)) + geom_boxplot()#SalesとCustomersの間の箱ひげ図
ggsave("sales_competitiondistance_box.png",plot = sales_competitiondistance_box,width = 8,height = 6,dpi = 300)
competitiondistance_customers_box <- ggplot(full_SalesCustomersNoNA_open, aes(CompetitionDistance,Customers)) + geom_boxplot()#SalesとCustomersの間の箱ひげ図
ggsave("competitiondistance_customers.png",plot = competitiondistance_customers,width = 8,height = 6,dpi = 300)

#回帰分析
model1_customers_sales <- lm(Sales ~ Customers,data = full_SalesCustomersNoNA_open)
res_1 <- resid(model1_customers_sales) 
plot(fitted(model1_customers_sales) ,res_1)
model2_customers_sales_cd <- lm(Sales ~ Customers + CompetitionDistance,data = full_SalesCustomersNoNA_open)
res_2 <- resid(model2_customers_sales_cd) 
plot(fitted(model2_customers_sales_cd), res_2)

#重回帰表
install.packages("stargazer")
library(stargazer)
stargazer(model1_customers_sales,model2_customers_sales_cd,  type = "text")
