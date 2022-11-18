library(tidyverse)
install.packages("stargazer")
library(stargazer)


dat <- readRDS("C:/Users/Caesar/Documents/GitHub/StatsI_Fall2022/tutorials/house_price_project/data/train.rds")

best_model <- lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving)
summary(best_model)
stargazer(best_model, type = "html")


best_model_zip <- lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving + dat$ZipCode)

best_model_resid <- resid(best_model)

best_model_zip_resid <- resid(best_model_zip) 

dat %>%
  group_by(ZipCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(as.factor(reorder(ZipCode, n)), n)) +
  geom_col() +
  coord_flip() +
  xlab("Zip Code")


best_model_resid %>%
  group_by(ZipCode)

zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(resids = (best_model_zip_resid)),
          count = n()) %>%
  arrange(resids) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))


dat %>%
  group_by(ZipGroup, ZipCode)%>%
  summarise(n = n())
  

best_model_zip_resid %>%
  group_by(ZipGroup, ZipCode)%>%
  summarise(n = n())


zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))
dat <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")

mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup, data = dat)

stargazer(mod4, type = "html") # work in Rmd

# if we want 
