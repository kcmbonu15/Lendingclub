#### Lending club data Analysis
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(zoo)
library(lubridate)
library(ggplot2)
### reading the data 
hw <- read.csv("C:/Users/kaelo/Desktop/LoanStats3a.csv", skip = 1, header = TRUE, dec = ",", quote = "\"")
head(hw)
#annoying column; just get rid of it
hw[, 'desc'] <- NULL
summary(hw)
#almost all NA, so just get rid of it
hw[, 'mths_since_last_record'] <- NULL

#get rid of fields that are mainly NA
poor_coverage <- sapply(hw, function(x) {
        coverage <- 1 - sum(is.na(x)) / length(x)
        coverage < 0.8
})
hw <- hw[, poor_coverage == FALSE]

### Looking at the density plot for the dependents variables
ggplot(data = hw, aes(loan_status)) + geom_density(aes(fill = loan_status)) +
        theme(axis.text.x = element_blank()) +
        labs(list(title = "Loan amount by status", x = "Status", y = "Density"))


## Distribution of loan amounts by status
box_status <- ggplot(hw, aes(loan_status, loan_amnt))
box_status + geom_boxplot(aes(fill = loan_status)) +
        theme(axis.text.x = element_blank()) +
        labs(list(
                title = "Loan amount by status",
                x = "Status",
                y = "Amount"))

## Distribution of loan amounts by term
box_status <- ggplot(hw, aes(term, loan_amnt))
box_status + geom_boxplot(aes(fill = term)) +
        theme(axis.text.x = element_blank()) +
        labs(list(
                title = "Loan amount by term",
                x = "term",
                y = "Amount"))


bad_indicators <- c("Charged Off ",
                    "Default",
                    "Does not meet the credit policy. Status:Charged Off",
                    "Late (16-30 days)",
                    "Late (31-120 days)")


hw$is_bad <- ifelse(hw$loan_status %in% bad_indicators, 1,
                    ifelse(hw$loan_status == "", NA,
                           0))
table(hw$is_bad)

library(stringr)
hw$issue_d <- as.Date((as.yearmon(hw$issue_d, "%b-%Y")))

hw$earliest_cr_line <- as.Date((as.yearmon(hw$earliest_cr_line, "%b-%Y")))

hw$revol_util <- str_replace_all(hw$revol_util, "[%]", "")
hw$revol_util <- as.numeric(hw$revol_util)


outcomes <- ddply(hw, .(issue_d), function(x) {
        c("percent_bad" = sum(x$is_bad) / nrow(x),
          "n_loans" = nrow(x))
})

plot(outcomes$percent_bad, main = "Bad Rate",col = "blue")
outcomes

##Here’s how the issue date was growing as the years goes up:

dfm <- hw %>%
        select(issue_d, loan_amnt) %>%
        group_by(issue_d) %>%
        summarise(Amount = sum(loan_amnt))

ts_amnt <- ggplot(dfm,
                  aes(x = issue_d, y = Amount))
ts_amnt + geom_line() + xlab("Date issued")


##Here’s was playing with the loans, and how the grades where changing in years
df_grade <- hw %>%
        select(issue_d, loan_amnt, grade) %>%
        group_by(issue_d, grade) %>%
        summarise(Amount = sum(loan_amnt))

ts_amnt_grade <- ggplot(df_grade,
                        aes(x = issue_d, y = Amount))
ts_amnt_grade + geom_area(aes(fill = grade)) + xlab("Date issued")


#figure out which columns are numeirc (and hence we can look at the distribution)
numeric_cols <- sapply(hw, is.numeric)
#turn the data into long format (key->value esque)
hw.lng <- melt(hw[, numeric_cols], id = "is_bad")
head(hw.lng)

length(unique(hw[, "member_id"]))  ## we have 42536 customers
#plot the distribution for bads and goods for each variable

p <- ggplot(aes(x = value, group = is_bad, colour = factor(is_bad)), data = hw.lng)
#quick and dirty way to figure out if you have any good variables
p + geom_density() +
        facet_wrap( ~ variable, scales = "free")

#Specifies Variable 's Data Classes
sapply(hw[1,], class)
summary(factor(hw$is_bad))


names(hw[, !complete.cases(t(hw))])
sum(is.na(hw))
hw$is_rent <- hw$home_ownership == "RENT"
### selecting variables for the building of the model ########
int1 <- hw %>% select(loan_amnt, is_bad, delinq_2yrs, inq_last_6mths, open_acc, pub_rec,
                      revol_bal, revol_util, total_acc,
                      acc_now_delinq, delinq_amnt, pub_rec_bankruptcies,
                      tax_liens, is_rent,term)

summary(factor(int1$is_bad))
summary(int1)
sapply(int1[1,], class)
names(int1[, !complete.cases(t(int1))])
sum(is.na(int1))

#### imputation of missing with mean #########

int1$loan_amnt[is.na(int1$loan_amnt)] <- mean(int1$loan_amnt, na.rm = TRUE)
int1$delinq_2yrs[is.na(int1$delinq_2yrs)] <- mean(int1$delinq_2yrs, na.rm = TRUE)
int1$inq_last_6mths[is.na(int1$inq_last_6mths)] <- mean(int1$inq_last_6mths, na.rm = TRUE)
int1$pub_rec[is.na(int1$pub_rec)] <- mean(int1$pub_rec, na.rm = TRUE)
int1$delinq_amnt[is.na(int1$delinq_amnt)] <- mean(int1$delinq_amnt, na.rm = TRUE)
int1$open_acc[is.na(int1$open_acc)] <- mean(int1$open_acc, na.rm = TRUE)
int1$revol_bal[is.na(int1$revol_bal)] <- mean(int1$revol_bal, na.rm = TRUE)
int1$total_acc[is.na(int1$total_acc)] <- mean(int1$total_acc, na.rm = TRUE)
int1$pub_rec[is.na(int1$pub_rec)] <- mean(int1$pub_rec, na.rm = TRUE)
int1$tax_liens[is.na(int1$tax_liens)] <- mean(int1$tax_liens, na.rm = TRUE)
int1$revol_util[is.na(int1$revol_util)] <- mean(int1$revol_util, na.rm = TRUE)
int1$pub_rec_bankruptcies[is.na(int1$pub_rec_bankruptcies)] <- mean(int1$pub_rec_bankruptcies, na.rm = TRUE)
int1$acc_now_delinq[is.na(int1$acc_now_delinq)] <- mean(int1$acc_now_delinq, na.rm = TRUE)

sum(is.na(int1))
summary(int1)
newdata <- na.omit(int1)

### Building the models
library(caret)
set.seed(3277)
summary(factor(newdata$is_bad))
trainingIndices <- createDataPartition(newdata$is_bad, p = 0.75,
                                       list = FALSE)
loan_Training <- newdata[trainingIndices,]
loan_Testing <- newdata[ - trainingIndices,]
dim(loan_Training)
dim(loan_Testing)

### Using Logistic regressions
set.seed(2345)
modelFit2 <- train(factor(is_bad) ~ ., data = loan_Training, method= "glm")
modelFit2                           

#### loan data predictions
predictions<- predict(modelFit2, newdata = loan_Testing)
predictions
### calculating the confusion matrix functions
###Fraction correct or Accuracy is 98%
confusionMatrix(predictions,loan_Testing$is_bad)

### K folds cross validation (loan data)
set.seed(34353)
folds<- createFolds(newdata$is_bad, k =10,
                    list = TRUE, returnTrain = TRUE)
sapply(folds, length)
folds[[1]][1:10]

#### Loan data k folds returned  test
set.seed(34353)
folds<- createFolds(newdata$is_bad, k =10,
                    list = TRUE, returnTrain = FALSE)
sapply(folds, length)
folds[[1]][1:10]

#########using Linear discrimant Analysis#######
set.seed(1234)
modelFit3 <- train(factor(is_bad) ~ ., data = loan_Training, method= "lda")
modelFit3

predictions1<- predict(modelFit3, newdata = loan_Testing)
predictions1

#### Fraction correct we have 97 % accuracy
confusionMatrix(predictions1,loan_Testing$is_bad)

##### using Gradient boosting model
set.seed(789987)

modelFit4 <- train(factor(is_bad) ~ ., data = loan_Training, method= "gbm")
modelFit4

predictions2<- predict(modelFit4, newdata = loan_Testing)
predictions2

#### Fraction correct we have 98 % accuracy
confusionMatrix(predictions2,loan_Testing$is_bad)

####This a demostration of different Machine learning Techiques 
###logistic regression -LOGIT
## Linear discrimant analysis- LDA
###Gradient boosting  -G BM
#  My recommendation for this model building process
## I will recommend logistic and gradient boosting they both have better Accuracy
# and the selected variables used as a characteristics to use to class a "bad" 
## Customer to a "good" customers. And this shows we have alot of bad customers
