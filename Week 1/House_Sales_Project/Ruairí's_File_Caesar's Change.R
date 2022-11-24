# Hallissey's Housing Price Project

## Data
dat <- read.table ("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv"
)


install.packages("dplR")
install.packages("stargazer")
install.packages("callr")
library(callr)
library(stargazer)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(car)

# Landvalue and bathrooms multivariate regression
lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$LandVal)
brlv.lm <- lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$LandVal)
summary(brlv.lm)

## Bathroom regression and plot
lm(dat$AdjSalePrice ~ dat$Bathrooms)
br.lm <- lm(dat$AdjSalePrice ~ dat$Bathrooms)
summary (br.lm)
plot(dat$Bathrooms, dat$AdjSalePrice)
abline(br.lm)

#Landvalue regression and plot r .65
lm(dat$AdjSalePrice ~ dat$LandVal)
lv.lm <- lm(dat$AdjSalePrice ~ dat$LandVal)
summary(lv.lm)
plot(dat$LandVal, dat$AdjSalePrice)
abline(lv.lm)

# Sqr foot to living regression and plot r .48
summary(lm(dat$AdjSalePrice ~ dat$SqFtTotLiving))
sqftol.lm <- lm(dat$AdjSalePrice ~ dat$SqFtTotLiving)
summary(sqftol.lm)
plot(dat$SqFtTotLiving, dat$AdjSalePrice)
abline(sqftol.lm)

# BldgGrade r.45
summary(lm(dat$AdjSalePrice ~ dat$BldgGrade))
bldg.lm <- lm(dat$AdjSalePrice ~ dat$BldgGrade)
summary(bldg.lm)
plot(dat$BldgGrade, dat$AdjSalePrice)
abline(bldg.lm)

# Impsval regression r. 69
summary(lm(dat$AdjSalePrice ~ dat$ImpsVal))
impval.lm <- lm(dat$AdjSalePrice ~ dat$ImpsVa)
plot(dat$AdjSalePrice ~ dat$ImpsVal)
abline(impval.lm)

# Multivariates
# r .53
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving))

# r .70
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving
     + dat$ImpsVal))

# r .86 (https://medias.spotern.com/spots/w640/65/65297-1533296705.jpg)
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving
     + dat$ImpsVal + dat$LandVal))
# we eliminate the IMP value becasue we calculate the land price including the IMP value,
# so it's self-explanation 

# r .86
summary(
  lm(dat$AdjSalePrice ~ dat$ImpsVal + dat$LandVal)) 
## the r^2 is high because we include land value, and it's a bit self-explanation,
## so we can't use this model.

### Bi and multivarte regressions for bathrooms, bldg grade, and sq ft living
## Bivariates
# Bathroom regression and plot r .28
lm(dat$AdjSalePrice ~ dat$Bathrooms)
br.lm <- lm(dat$AdjSalePrice ~ dat$Bathrooms)
summary (br.lm)
plot(dat$Bathrooms, dat$AdjSalePrice)
abline(br.lm)
# Sqr foot to living regression and plot r .48
summary(lm(dat$AdjSalePrice ~ dat$SqFtTotLiving))
sqftol.lm <- lm(dat$AdjSalePrice ~ dat$SqFtTotLiving)
summary(sqftol.lm)
plot(dat$SqFtTotLiving, dat$AdjSalePrice)
abline(sqftol.lm)
# BldgGrade r.45
summary(lm(dat$AdjSalePrice ~ dat$BldgGrade))
bldg.lm <- lm(dat$AdjSalePrice ~ dat$BldgGrade)
summary(bldg.lm)
plot(dat$BldgGrade, dat$AdjSalePrice)
abline(bldg.lm)

# Multivariate
# r .53
summary(
  lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade + dat$SqFtTotLiving))
brblsqrlm <- lm(dat$AdjSalePrice ~ dat$Bathrooms + dat$BldgGrade
                + dat$SqFtTotLiving)
mvlm.resid <- residuals(brblsqrlm)


## Code for model with zip_group
# data = d

dat$res <- mvlm.resid


## dat$Grant_Zip_Group <- cut(dat$ZipCode, breaks = 5)   ## doesn't work 
## barplot(table(dat$Grant_cut))

zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(resids = median(dat$res),
            count = n()) %>%
  arrange(resids) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))


dd <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")


lm.4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup + Bathrooms, data = dd)

summary(lm.4)

stargazer(lm.4, type = "html")
# we can say that lm.4 is the best model of prediction. 

scatter.smooth(dat$SqFtTotLiving, resid(lm.4), # plot a smooth line on the scatter plot
               lpars = list(col = "blue", lwd = 3, lty = 3), 
               main = "Residual Plot (Sale Price ~ Size)",
               xlab = "Total Living Area (sq.ft.)",
               ylab = "Residuals")
abline(h = 0, col = "red")

par(mfrow = c(1,1))
avPlot(lm.4, variable = "SqFtTotLiving")

avPlot(lm.4, variable = "BldgGrade")


scatter.smooth(dat$Bedrooms, resid(lm.4), # plot a smooth line on the scatter plot
               lpars = list(col = "blue", lwd = 3, lty = 3), 
               main = "Residual Plot (Sale Price ~ Size)",
               xlab = "Total Living Area (sq.ft.)",
               ylab = "Residuals")
abline(h = 0, col = "red")

terms <- predict(lm.4, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid <- resid(lm.4) + terms # add the individual regression terms to the residual for each observation

df <- data.frame(SqFtTotLiving = dat[, "SqFtTotLiving"], # create a new data.frame of these vals
                 Terms = terms[, "SqFtTotLiving"],
                 PartialResid = partial_resid[, "SqFtTotLiving"])

ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(SqFtTotLiving, Terms), colour = "red")

lm.5 <- lm(AdjSalePrice ~ Bathrooms + I(SqFtTotLiving^2) + SqFtTotLiving + BldgGrade + ZipGroup, data = dd)
summary(lm.5)

terms_poly <- predict(lm.5, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid_poly <- resid(lm.5) + terms_poly # add the individual regression terms to the residual for each observation

df_poly <- data.frame(SqFtTotLiving = dat[, "SqFtTotLiving"], # create a new data.frame of these vals
                      Terms = terms_poly[, "I(SqFtTotLiving^2)"],
                      PartialResid = partial_resid_poly[, "I(SqFtTotLiving^2)"])

ggplot(df_poly, aes(SqFtTotLiving, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(SqFtTotLiving, Terms), colour = "red")

par(mfrow = c(2, 2)) # we change the graphic device to show 4 plots at once
plot(mod4) # we supply our lm object to plot()

## final model 
mod7 <- lm(AdjSalePrice ~ Bathrooms + I(SqFtTotLiving^2) +
             SqFtTotLiving + BldgGrade + ZipGroup, + BldgGrade,
           I(BldgGrade^2), data = dat2)







########### 

mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + I(SqFtTotLiving^2) + SqFtLot + Bathrooms + Bedrooms + BldgGrade + PropertyType + as.factor(ZipGroup), data = dd)

terms_poly <- predict(mod4, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid_poly <- resid(mod4) + terms_poly # add the individual regression terms to the residual for each observation

df_poly <- data.frame(SqFtTotLiving = dat[, "SqFtTotLiving"], # create a new data.frame of these vals
                      Terms = terms_poly[, "I(SqFtTotLiving^2)"],
                      PartialResid = partial_resid_poly[, "I(SqFtTotLiving^2)"])

ggplot(df_poly, aes(SqFtTotLiving, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(SqFtTotLiving, Terms), colour = "red")




mod5 <- lm(AdjSalePrice ~ SqFtTotLiving + I(SqFtTotLiving^2) + SqFtLot + Bathrooms + (Bedrooms)^2 + BldgGrade + PropertyType + as.factor(ZipGroup), data = dd)

terms_poly <- predict(mod5, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid_poly <- resid(mod5) + terms_poly # add the individual regression terms to the residual for each observation

df_poly <- data.frame(SqFtTotLiving = dat[, "SqFtTotLiving"], # create a new data.frame of these vals
                      Terms = terms_poly[, "I(SqFtTotLiving^2)"],
                      PartialResid = partial_resid_poly[, "I(SqFtTotLiving^2)"])

ggplot(df_poly, aes(SqFtTotLiving, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(SqFtTotLiving, Terms), colour = "red")

###########

terms <- predict(mod2, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid <- resid(mod2) + terms # add the individual regression terms to the residual for each observation

df <- data.frame(SqFtTotLiving = dat[, "SqFtTotLiving"], # create a new data.frame of these vals
                 Terms = terms[, "SqFtTotLiving"],
                 PartialResid = partial_resid[, "SqFtTotLiving"])

ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(SqFtTotLiving, Terms), colour = "red")




mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + I(SqFtTotLiving^2) + SqFtLot + Bathrooms + Bedrooms + BldgGrade + PropertyType + as.factor(ZipGroup), data = dd)



stargazer(mod1, mod2, mod4, type = "html")




terms_poly <- predict(mod4, type = "terms") # extract the individual regression terms from our model for each observation

partial_resid_poly <- resid(mod4) + terms_poly # add the individual regression terms to the residual for each observation

df_poly <- data.frame(SqFtTotLiving = dat[, "SqFtTotLiving"], # create a new data.frame of these vals
                      Terms = terms_poly[, "I(SqFtTotLiving^2)"],
                      PartialResid = partial_resid_poly[, "I(SqFtTotLiving^2)"])

ggplot(df_poly, aes(SqFtTotLiving, PartialResid)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  geom_line(aes(SqFtTotLiving, Terms), colour = "red")


par(mfrow = c(2, 2)) # we change the graphic device to show 4 plots at once
plot(mod4) # we supply our lm object to plot()


sresid <- rstandard(mod4) # the rstandard() function extracts standardised residuals
index <- order(sresid) # make an index of standardised residuals
dat[index[1:5], c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", "Bedrooms", "BldgGrade")]





