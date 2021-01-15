################################################################################
######################  Visualizing Full-Time Results   ########################
################################################################################

#Libraries needed
library(ggplot2)
library(tidyverse)
library(scales)

#Counting Home wins, Away wins and draws
tab <- finalDataClean %>%
  group_by(FTR) %>%
  summarize("Frequency" = n())

#Adding percentages
df <- data.frame(
  Result = tab$FTR,
  n = tab$Frequency,
  prop = round(tab$Frequency/nrow(finalDataClean), 2) #380 is the number of matches in a season
)
#Adding label position
df <- df %>%
  arrange(desc(Result)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
df
#Setting colors
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF")
#Pie chart
pie <- ggplot(df, aes(x = "Summary of Full Time Results", y = prop, fill = Result)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  theme_void()
pie <- pie  + 
  scale_fill_discrete(name = "Full-Time Result", labels = c("Away win", "Draw", "Home win")) +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 10))


################################################################################
#####################     Data Partitioning     ################################
################################################################################
library(caret)
set.seed(1234)
trainIndex <- createDataPartition(finalDataClean$FTR, p = .8, 
                                  list = FALSE, 
                                  times = 1)
Train <- finalDataClean[ trainIndex,]
Test  <- finalDataClean[-trainIndex,]

################################################################################
#####################     Algorithm Selection    ###############################
################################################################################

# Prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the Base Support Vector Machine model
set.seed(7)
modelSvm <- train(FTR~., data=Train, method="svmRadialWeights", trControl=control, )
# train the Base Random Forest model
set.seed(7)
modelRf <- train(FTR~., data=Train, method="rf", trControl=control)
# train the Base Extreme Gradient Boosting model
set.seed(7)
modelXgb <- train(FTR~., data=Train, method="xgbTree", trControl=control, verbose=FALSE)
# train the Base Stochastic Gradient Boosting model
set.seed(7)
modelGbm <- train(FTR~., data=Train, method="gbm", trControl=control, verbose=FALSE)
# train the Base Neural Network model
set.seed(7)
modelNn <- train(FTR~., data=Train, method="nnet", trControl=control, linear.output = FALSE)

# collect resamples
results <- resamples(list(RandomForest = modelRf,
                          eXtremeGradientBoosting = modelXgb,
                          StochasticGradientBoosting = modelGbm,
                          NeuralNetwork = modelNn,
                          SupportVectorMachine = modelSvm))

# summarize the distributions
summary(results)
#Boxplots of results
bwplot(results)

################################################################################
#####################        XGBoost tuning         ############################
################################################################################
#The tuning process occurs in steps
#First, the learning rate will be tuned
#The second step includes finding the optimal max tree depth and min child ewight
#Next, we will look at tuning row and column sampling
#furthermore, we will look at tuning gamma
#The last step involves trying out even lower values for the learning rate

#At each step, the accuracy of the best tune will be checked
#We will also use the helper function to be able to plot all steps in the tuning process

#Functions 
# helper function for plotting the tuning process
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$Accuracy, probs = probs), min(x$results$Accuracy))) +
    theme_bw()
}

#Control with 10-fold Cross validation, repeated 3 times
control <- trainControl(method="repeatedcv", number=10, repeats=3)

#Baseline
grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)
set.seed(7)
modelXgbBase <- train(FTR~., data=Train, 
                      method="xgbTree", 
                      trControl=control,
                      tuneGrid = grid_default,
                      verbose=FALSE)
modelXgbBase$results$Accuracy


#Step 1: Tuning learning rate first
nrounds = 1000
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)
set.seed(7)
modelXgb <- train(FTR~., data=Train, 
                  method="xgbTree", 
                  trControl=control, 
                  tuneGrid = tune_grid,
                  verbose=FALSE)


tuneplot(modelXgb)

#Checking out-of-sample Accurcy with the best tune for step 1
modelXgb$bestTune

tune_gridTest1 <- expand.grid(
  nrounds = 200,
  eta = 0.1,
  max_depth = 2,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)
set.seed(7)
test1 <- train(FTR~., data=Train, 
               method="xgbTree", 
               trControl=control, 
               tuneGrid = tune_gridTest1,
               verbose=FALSE)
test1$results$Accuracy


#Step 2: Tuning max depth and min child weight
nrounds = 1000
tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = 0.1,
  max_depth = c(2,3),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)
set.seed(7)
modelXgb2 <- train(FTR~., data=Train, 
                   method="xgbTree", 
                   trControl=control, 
                   tuneGrid = tune_grid2,
                   verbose=FALSE)

tuneplot(modelXgb2)

#Checking out-of-sample Accurcy with the best tune for step 2
modelXgb2$bestTune

tune_gridTest2 <- expand.grid(
  nrounds = 50,
  eta = 0.1,
  max_depth = 2,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)
set.seed(7)
test2 <- train(FTR~., data=Train, 
               method="xgbTree", 
               trControl=control, 
               tuneGrid = tune_gridTest2,
               verbose=FALSE)
test2$results$Accuracy

#Step 3: Tuning row and column sampling 
tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = 0.1,
  max_depth = modelXgb2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = modelXgb2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)
set.seed(7)
modelXgb3 <- train(FTR~., data=Train, 
                   method="xgbTree", 
                   trControl=control, 
                   tuneGrid = tune_grid3,
                   verbose=FALSE)
tuneplot(modelXgb3)
modelXgb3$bestTune

#Checking out-of-sample Accurcy with the best tune for step 3
modelXgb3$bestTune

tune_gridTest3 <- expand.grid(
  nrounds = 50,
  eta = 0.1,
  max_depth = 2,
  gamma = 0,
  colsample_bytree = 0.6,
  min_child_weight = 3,
  subsample = 1
)
set.seed(7)
test3 <- train(FTR~., data=Train, 
               method="xgbTree", 
               trControl=control, 
               tuneGrid = tune_gridTest3,
               verbose=FALSE)
test3$results$Accuracy

#Step 4: Tuning gamma
tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = 0.1,
  max_depth = modelXgb2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = modelXgb3$bestTune$colsample_bytree,
  min_child_weight = modelXgb2$bestTune$min_child_weight,
  subsample = modelXgb3$bestTune$subsample
)
set.seed(7)
modelXgb4 <- train(FTR~., data=Train, 
                   method="xgbTree", 
                   trControl=control, 
                   tuneGrid = tune_grid4,
                   verbose=FALSE)
modelXgb4$bestTune
tuneplot(modelXgb4)
#Checking out-of-sample Accurcy with the best tune for step 4
modelXgb4$bestTune

tune_gridTest4 <- expand.grid(
  nrounds = 50,
  eta = 0.1,
  max_depth = 2,
  gamma = 0.5,
  colsample_bytree = 0.6,
  min_child_weight = 1,
  subsample = 1
)
set.seed(7)
test4 <- train(FTR~., data=Train, 
               method="xgbTree", 
               trControl=control, 
               tuneGrid = tune_gridTest4,
               verbose=FALSE)
test4$results$Accuracy

#Step 5: Learning rate again
tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 5000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = modelXgb2$bestTune$max_depth,
  gamma = modelXgb4$bestTune$gamma,
  colsample_bytree = modelXgb3$bestTune$colsample_bytree,
  min_child_weight = modelXgb2$bestTune$min_child_weight,
  subsample = modelXgb3$bestTune$subsample
)
set.seed(7)
modelXgb5 <- train(FTR~., data=Train, 
                   method="xgbTree", 
                   trControl=control, 
                   tuneGrid = tune_grid5,
                   verbose=FALSE)
tuneplot(modelXgb5)
modelXgb5$bestTune


################################################################################
#####################         Best model            ############################
################################################################################
#The final model is not made using a tuning grid and the Caret package, 
#because the results need to be in a specific format to construct feature importance

#Getting data in the right format
library(Matrix)
trainm <- sparse.model.matrix(FTR ~ ., data = Train)
train_label <- Train[, "FTR"]
train_label <- ifelse(train_label == "A", 0, ifelse(train_label == "D", 1, 2 )) #changing labels to numeric
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

#Training model with tuned parameters
mbst <- xgboost(data = train_matrix,
                nrounds = modelXgb5$bestTune$nrounds,
                eta = modelXgb5$bestTune$eta,
                max.depth = modelXgb2$bestTune$max_depth,
                gamma = modelXgb4$bestTune$gamma,
                colsample_bytree = modelXgb3$bestTune$colsample_bytree,
                min_child_weight = modelXgb2$bestTune$min_child_weight,
                subsample = modelXgb3$bestTune$subsample,
                objective = "multi:softprob", 
                num_class = 3)

#Making Predictions
testm <- sparse.model.matrix(FTR ~ ., data = Test)
test_label <- Test[, "FTR"]
test_label <- ifelse(test_label == "A", 0, ifelse(test_label == "D", 1, 2 )) #changing labels to numeric
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

#Confusion matrix and accuracy
p <- predict(mbst, newdata = test_matrix)
pred <- matrix(p, nrow = 3, ncol = length(p)/3) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(., "last")-1)
table(Prediction = pred$max_prob, Actual = pred$label)
accuracy <- (23+1+81)/200

# Variable Importance
imp <- xgb.importance(model = mbst)
xgb.plot.importance(imp, top_n = 25)
