# Packages
install.packages("tidyr")
library(tidyr)
install.packages("tidyverse")
library(tidyverse)# data manipulation and visualization
install.packages("modelr")
library(modelr)     # provides easy pipeline modeling functions
install.packages("broom")
library(broom)# helps to tidy up model outputs
install.packages("dplyr")
library(dplyr)
install.packages("corrplot")
library(corrplot)
advertising <- read.csv("F:\\eckovation.R\\Advertising.csv") %>% select(-X1)

advertising
head(advertising)
tail(advertising)

corr.data <- cor(advertising)
round(corr.data, 2)

#The function corrplot() take the correlation matrix as the frst argument
#The second argument (type="upper") is used to display only the upper triangular of the correlation matrix.

corrplot(corr.data, type ="upper", order="hclust",
          tl.col ="black", tl.srt = 45)


plot(advertising$Newspaper, advertising$Sales)
plot(advertising$Radio, advertising$Sales)
plot(advertising$TV, advertising$Sales)

#####-------------Applying Regression------------###########

set.seed(123) # create same values of random data.
sample <- sample(c(TRUE, FALSE), nrow(advertising), replace = T, prob = c(0.6,0.4))
sample
advertising[sample ,]
train <- advertising[sample, ]
test <- advertising[!sample, ]

plot(advertising)

# Model Building
# To build this model in R we use the formula notation of 

model1 <- lm(Sales ~ TV, data = train)
rsquare(model1, data=train)

glance(model1)
summary(model1)


# In other words, our intercept estimate is 6.76 so when the TV advertising budget is 
# zero we can expect sales to be 6,760 (remember we're operating in units of 1,000). 
# And for every $1,000 increase in the TV advertising budget we expect the average 
# increase in sales to be 50 units.


# Our results show us that our 95% confidence interval for ??1??1 (TV) is [.043, .057].
# Thus, since zero is not in this interval we can conclude that as the TV advertising
# budget increases by $1,000 we can expect the sales to increase by 43-57 units. 
# This is also supported by the t-statistic provided by our results, which are computed by
# t=??1???0SE(??1)(5)(5)t=??1???0SE(??1)


cor(train$TV, train$Sales)^2
ggplot(train, aes(TV, Sales)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(se = FALSE, color = "red")

#data model diagnostic to our training data

model1_results <-augment(model1, train)
model1_results

rmse(model1, train)
rmse(model1, test)
summary(model1)
#sales=B0+B1 (advertising)

#Making second model_2--------------######

model2 <-lm(Sales ~TV + Radio + Newspaper, data = train)
summary(model2)
tidy(model2)
confint(model2)
list(model1 = broom::glance(model1), model2 = broom::glance(model2))
rmse(model2, test)
summary(model2)

######------Making second model_3--------------######

model3 <-lm(Sales ~TV + Radio, data = train)
summary(model3)
tidy(model3)

rmse(model1, test)
rmse(model2, test)
rmse(model3, test)
summary(model3)
summary(model2)

# Making Predictions

(test <- test %>% 
    add_predictions(model1))


#The primary concern is to assess if the out-of-sample mean squared error (MSE), 
# also known as the mean squared prediction error, is substantially higher than the
# in-sample mean square error, as this is a sign of deficiency in the model. The MSE 
# is computed as
# MSE=1nn???i=1(yi???^yi)2(9)(9)MSE=1n???i=1n(yi???y^i)2

