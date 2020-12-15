library("car")
install.packages('caret')
library("caret")
install.packages('gvlma')
library("gvlma")
install.packages('predictmeans')
library("predictmeans")
install.packages('e1071')
library("e1071")
library('tidyverse')

height <- read.csv('heights.csv')
head(height)

#first changed the column names to something easy to type
colnames(height)
names(height)[names(height) == 'AM_Height'] <- 'am'
names(height)[names(height) == 'PM_Height'] <- 'pm'
colnames(height)


scatter.smooth(x=height$am, y=height$pm, main= "How much we shrink daily")
#clear linear relationship

cor(height$am, height$pm)
#correlation is almost 100%

mod <- lm(height$am ~ height$pm, data = height)
summary(mod)

par(mfrow=c(2,2))
plot(mod)
# it looks homoscedastic but still need to run more tests
lmtest::bptest(mod)
# our p value is greater than .05 which means it is not significant and proves that data is homoscedastic

car::ncvTest(mod)
# same as above, no corrections required. 
# our data also passes homogeneity test 


CookD(mod, group = NULL, plot = TRUE, idn = 3, newwd = TRUE)
# we see no outliers in x space

lev = hat(model.matrix(mod))
plot(lev)
# not necessary but we also did leverage check an no data value 0.15

car::outlierTest(mod)
#p value is less than .05 which means we have at least 1 value which is an outlier
# r student value is less than 2.5 so we can ignore this outlier.

summary(influence.measures(mod))
# no value in fist 3 column is above 1, again no influential outliers. 


summary(mod)
#F-statistic p value is less that .05, so it is significant.So null hypthesis is rejected. SHRINK IS REAL!
#And the ratio of shrinking for each student is almost same, that means one common cause results this shrink.

# Multiple R squared value shows that 
#estimate value is 1.009 and intercept value is -6.994
# our equation for instance height 1800 during sleep is y = 1800 - 6.9, y as am height will be = 1793.1 












