# FootballPrediction
This analysis shows the predictive power of a classification model predicting the full-time result of football matches with the use of historical match and performance data as well as the betting odds. A real betting strategy would not only consider the predicted class with the highest predicted probability, but would rather look at the chances of all three outcomes and the potential payoffs. So, building a classifier model is the first step in creating a betting strategy. 

As quantifying the quality and play style of a football team is a crucial but complex task, this analysis tried to solve this problem with the use of historical performance data to improve the predictive power of a classification model. The dimensionality of the peformance statistics data set is reduced with the use of Principal Component Analysis. This method allows adding all performance statistics of the home and away team for each match with just a couple principle components (PC’s). This provides a good indication of the quality and play style of a team. 

The data wrangling part can not be run in one go. The addition of some features needs to be done per individual season. The added comments in the script should provide enough guidance to perform all steps. 

As this model is just build as a hobby (for now) I would NOT advice to use the outcomes in a real bettting strategy. I am currently working on finding more appropiate data to update the model and increase the accuracy. 
