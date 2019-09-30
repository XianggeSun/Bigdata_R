library(readxl)
library(ggplot2)

# Load Mocked_Customer_Data_With_Missing.xlsx into R and save it as a data frame
rawdata <- read_excel("D:/9733/Week4_Mocked_Customer_Data_With_Missing.xlsx")
df <- data.frame(rawdata)

columns=colnames(df)

# Check the data completeness
for (i in columns){
  if (sum(is.na(df[i])) != 0)
    {
    print(paste('Data of', i, ' is not complete, it has null value.', sep = ''))
    }
  else{
    print('Data is complete.')
  }}

# Identify numeric vs categorical variables
# ?????????????????? home_owner??? num_cont?
split(names(df),sapply(df, function(x) paste(class(x), collapse=" ")))
df$churn_flag = as.factor(df$churn_flag)
df$gender = as.factor(df$gender)
df$marrital = as.factor(df$marrital)
df$fortune = as.factor((df$fortune))

# Plot histograms of “age” group by “churn”
#x11(width=6, height=5)
#????? title?

par(mfrow=c(1,2))
hist(df$age[df$churn_flag==0], main = 'churn = 0', xlab = 'Age',  ylab = 'Frequency')
hist(df$age[df$churn_flag==1], main = 'churn = 1', xlab = 'Age',  ylab = 'Frequency')
mtext("Plots of Different Functions", side = 3, line = 0, outer = T, cex=2,font=2)


# Conduct a t-test of “tot_bill” regarding “churn”
t.test(df$tot_bill~df$churn_flag)

# Fill out missing values if any
# Replace missing value with mean in column i if it's a numerical variable
# Replace missing value with mode in column i if it's a categorical variable
# ????????? columns 5, 8, 11 have missing value
FindMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df[is.na(df[, 5]), 5] <- mean(df[, 5], na.rm = TRUE)
df[is.na(df[, 8]), 8] <- mean(df[, 8], na.rm = TRUE)
df[is.na(df[, 11]), 11] <- FindMode(df[,11])

# Randomly split the data frame into train and test sets:

## 75% of the sample size
smp_size <- floor(0.75 * nrow(df))

## set the seed to make your partition reproducible
set.seed(127)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ][,-1]
test <- df[-train_ind, ][,-1]


# Use the training set build a logistic regression model to predict “churn”
model <- glm(churn_flag ~ .-churn_flag,
             family=binomial(link='logit'),data=train)


#model2=glm.fit(train, train$churn_flag)

# Interpret the result
summary(model)


# Calculate precision, recall, F1-Score, ROC 
y <- test['churn_flag']
y_pred <- predict(model, test[,-1], type='response')
precision <-performance(pred,"ppv")
plot(precision)

recall <- performance(pred,"rec")
plot(recall)

F1 <- performance(pred,"f")
plot(F1)

# Plot the ROC and precision-recall curves
## ROC curve
ROC.perf <- performance(pred, "tpr", "fpr")
plot (ROC.perf)

auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)

# precision-recall curves
pred <- prediction(y_pred, test['churn_flag'])
RP.perf <- performance(pred, "prec", "rec")
plot (RP.perf)

colnames(df)
##Improvements
##First we try to standarize the data
num_namelist<- c( "tot_bill", "tot_bal","tot_bytes","tot_air","google_bytes", "google_air",   "yahoo_bytes",  "yahoo_air")
df2<-data.frame(df)
for (i in num_namelist){
  print(i)
  df2[i] =scale(df[i],center=T,scale=T)
  }
smp_size <- floor(0.75 * nrow(df2))

## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(df2)), size = smp_size)

train <- df2[train_ind, ][,-1]
test <- df2[-train_ind, ][,-1]


# Use the training set build a logistic regression model to predict “churn”
model <- glm(churn_flag ~ .-churn_flag,
             family=binomial(link='logit'),data=train)


#model2=glm.fit(train, train$churn_flag)

# Interpret the result
summary(model)

# Calculate precision, recall, F1-Score, ROC 
y <- test['churn_flag']
y_pred <- predict(model, test[,-1], type='response')
precision2 <-performance(pred,"ppv")
plot(precision)

recall2 <- performance(pred,"rec")
plot(recall)

F1_2 <- performance(pred,"f")
plot(F1)

# Plot the ROC and precision-recall curves
## ROC curve
ROC.perf <- performance(pred, "tpr", "fpr")
plot (ROC.perf)

auc.tmp <- performance(pred,"auc")
auc2 <- as.numeric(auc.tmp@y.values)

# precision-recall curves
pred <- prediction(y_pred, test['churn_flag'])
RP.perf <- performance(pred, "prec", "rec")
plot (RP.perf)
#We can see that there are no change using standarization

#Then we consider stepwise regression
model2<-step(object = model,trace = 0)
summary(model2)
# Calculate precision, recall, F1-Score, ROC 
y <- test['churn_flag']
y_pred <- predict(model2, test[,-1], type='response')
precision3 <-performance(pred,"ppv")
plot(precision)

recall3 <- performance(pred,"rec")
plot(recall)

F1_3 <- performance(pred,"f")
plot(F1)

# Plot the ROC and precision-recall curves
## ROC curve
ROC.perf <- performance(pred, "tpr", "fpr")
plot (ROC.perf)

auc.tmp <- performance(pred,"auc")
auc3 <- as.numeric(auc.tmp@y.values)

# precision-recall curves
pred <- prediction(y_pred, test['churn_flag'])
RP.perf <- performance(pred, "prec", "rec")
plot (RP.perf)


summary(model)
summary(model2)