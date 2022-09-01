# Logistics-regression-in-R-programming-
install.packages("mlogit")  
library(mlogit)
pacman::p_load(pacman, tidyverse, mlogit, rio)
data <- import("philant.xlsx")
view(data)
attach(data)

## calling for the glm function 
logit <- glm(`Win/Loss (Win 1, Loss 0)`~ `Points Team Scored`, data = data, family = "binomial")
summary(logit)


data$predicted <- predict(logit, newdata = data, type = "response")

view(data)

data$predict_round <- ifelse(data$predicted > 0.5, yes = 1, no = 0)
view(data) 

## plotting the graph

plot(data$`Points Team Scored`, data$`Win/Loss (Win 1, Loss 0)`, col = "orange", pch = 20, 
     main = "Points Scored vs Outcome", xlab = "Points scored", ylab = "Outcome")
lines(data$`Points Team Scored`, data$predicted, col = "blue")

data <- data[order(data$`Points Team Scored`), ]


### another one with diffrerent variables
glimpse(data)
logit <- glm(`Win/Loss (Win 1, Loss 0)`~ `Rebounds`, data = data, family = "binomial")
summary(logit)


data$predicted <- predict(logit, newdata = data, type = "response")

view(data)

data$predict_round <- ifelse(data$predicted > 0.5, yes = 1, no = 0)
view(data) 

## plotting the graph
data <- data[order(data$`Rebounds`), ]
plot(data$`Rebounds`, data$`Win/Loss (Win 1, Loss 0)`, col = "orange", pch = 20, 
     main = "Rebounds vs Outcome", xlab = "Rebounds", ylab = "Outcome")
lines(data$`Rebounds`, data$predicted, col = "blue")

data <- data[order(data$`Points Team Scored`), ]



### Adding  more variable 
## calling for the glm function 
logit <- glm(`Win/Loss (Win 1, Loss 0)`~ `Points Team Scored` + `Home/Away (Home 1, Away 0)` + `Field Goal %`, data = data, family = "binomial")
summary(logit)


data$predicted <- predict(logit, newdata = data, type = "response")

view(data)

data$predict_round <- ifelse(data$predicted > 0.5, yes = 1, no = 0)
view(data) 

## plotting the graph

plot(data$`Points Team Scored`, data$`Win/Loss (Win 1, Loss 0)`, col = "orange", pch = 20, 
     main = "Points Scored vs Outcome", xlab = "Points scored", ylab = "Outcome")
lines(data$`Points Team Scored`, data$predicted, col = "blue")

data <- data[order(data$`Points Team Scored`), ]



### Tranforming the variables 

## calling for the glm function 
###Transforming the point scoredinto log transformation 
data$`Points Team Scored` <- log(data$`Points Team Scored`)
logit <- glm(`Win/Loss (Win 1, Loss 0)`~ `Points Team Scored`, data = data, family = "binomial")
summary(logit)


view(data) 

data$predicted <- predict(logit, newdata = data, type = "response")

view(data)

data$predict_round <- ifelse(data$predicted > 0.5, yes = 1, no = 0)
## plotting the graph

plot(data$`Points Team Scored`, data$`Win/Loss (Win 1, Loss 0)`, col = "orange", pch = 20, 
     main = "Points Scored vs Outcome", xlab = "Points scored", ylab = "Outcome")
lines(data$`Points Team Scored`, data$predicted, col = "blue")
d
data <- data[order(data$`Points Team Scored`), ]

### correlation data analysis 
### we try to remove variables that are closely correlated tp prevent multicoliearity problem 

cor(data[c("Points Team Scored", "Home/Away (Home 1, Away 0)", "Field Goal %")])

cor(data[c("Rebounds", "Offensive Rebounds", "Defensive Rebounds")])


### Using statistics 
data$`Points Team Scored` <- log(data$`Points Team Scored`)
logit <- glm(`Win/Loss (Win 1, Loss 0)`~ `Points Team Scored`, data = data, family = "binomial")
summary(logit)

data$predicted <- predict(logit, newdata = data, type = "response")
data$predict_round <- ifelse(data$predicted > 0.5, yes = 1, no = 0)

### confidence interval
confint(logit)

### confusion table
data_train <- import("philant_training.xlsx")
data_test <- import("philant_testing.xlsx")

view(data_train)
view(data_test)
attach(data_train)
attach(data_test)

## calling for the glm function 
logit <- glm(`Win/Loss (Win 1, Loss 0)`~ `Points Team Scored`, data = data, family = "binomial")
summary(logit)


data_test$predicted <- predict(logit, newdata = data_test, type = "response")

data_test$predict_round <- ifelse(data_test$predicted > 0.5, yes = 1, no = 0)
view(data_test)

### calculating the confusion matrix from the data 
table(data_test$`Win/Loss (Win 1, Loss 0)`, data_test$predict_round)

### proportion table
prop.table(table(data_test$`Win/Loss (Win 1, Loss 0)`, data_test$predict_round))





### Updating the calculations 
data_train <- import("philant_training.xlsx")
data_test <- import("philant_testing.xlsx")

view(data_train)
view(data_test)
attach(data_train)
attach(data_test)

## calling for the glm function 
logit <- glm(`Win/Loss (Win 1, Loss 0)`~ `Points Team Scored` + `Home/Away (Home 1, Away 0)`+ `Field Goal %` , data = data, family = "binomial")
summary(logit)


data_test$predicted <- predict(logit, newdata = data_test, type = "response")

data_test$predict_round <- ifelse(data_test$predicted > 0.5, yes = 1, no = 0)
view(data_test)

### calculating the confusion matrix from the data 
table(data_test$`Win/Loss (Win 1, Loss 0)`, data_test$predict_round)

### proportion table
prop.table(table(data_test$`Win/Loss (Win 1, Loss 0)`, data_test$predict_round))











