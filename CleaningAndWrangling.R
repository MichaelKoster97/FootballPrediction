#Packages
library(readxl) #to open a excel document
library(lattice) #to plot component loadings in PCA
library(ggplot2) #for visualization
library(scales) #for visualization
library(xgboost) #for xgboost algorithm
library(caret) #for model tuning and comparison
library(Matrix) #to get the data in the right format for xgboost

#Helper function to set NA values to zero
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

#Loading the data
#Match Statistics
data1920 <- read.csv("C:\\Users\\micha\\Desktop\\Master\\seminar\\1920.csv")
data1819 <- read.csv("C:\\Users\\micha\\Desktop\\Master\\seminar\\1819.csv")
data1718 <- read.csv("C:\\Users\\micha\\Desktop\\Master\\seminar\\1718.csv")

#Subsetting match statistics datasets to have only columns that exist in all three data frames
data1920 <- data1920[, c(4:8, 12, 25:106)] 
labels <- vector()
labels1718 <- colnames(data1718)
labels1819 <- colnames(data1819)
labels1920 <- colnames(data1920)

k <- 1
for(i in 1:length(labels1920)) {
  #We only want features that are in all data sets
  if(labels1920[i] %in% labels1819 & labels1920[i] %in% labels1718) { 
    labels[k] <- labels1920[i]
    k <- k + 1
  }
}

#New dataframes with only the colums that exist in all data frames
data1920 <- data1920[, labels]
data1819 <- data1819[, labels]
data1718 <- data1718[, labels]

#Performance Statistics Data
HomePerformance <- read_excel("C:\\Users\\micha\\Desktop\\Master\\seminar\\HomeStatistics.xlsx")
HomePerformance <- HomePerformance[, -82]

AwayPerformance <- read_excel("C:\\Users\\micha\\Desktop\\Master\\seminar\\AwayStatistics.xlsx")
AwayPerformance <- AwayPerformance[, -82]


################################################################################
#################          Performance/Quality        #########################
###############################################################################

#Performance/Quality Measurements HOME
HomePerformance <- HomePerformance[, c(1:16,20:24,28,30,31,36:40,42,44,45,50:77,83)]
pca <- prcomp(HomePerformance[,-1], scale. = TRUE, center = TRUE)
screeplot(pca, main = "", las = 1)

LoadingsHome <- data.frame(pca$rotation[,c(1:7)])     #Component Loadings
print(summary(pca))     #Importance of Components
pca$x[,c(1:7)]    #Component scores

HomePerformance <- cbind(pca$x[,c(1:7)], HomePerformance[,1]) #Adding PC's to dataframe


#Levelplot Home
PCS <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
pcaTableData <- pca$rotation
pcaTableData <- pcaTableData[,PCS]
pcaTableData <- t(pcaTableData)
levelplot(pcaTableData, 
          xlab=list("Principal Components", fontface="bold"),
          ylab=list("Performance Variables", fontface='bold')
)

levelHome <- levelplot(pcaTableData, col.regions = gray(0:100/100))


#Performance/Quality Measurements AWAY
AwayPerformance <- AwayPerformance[, c(1:16,20:24,28,30,31,36:40,42,44,45,50:77,83)]
pca <- prcomp(AwayPerformance[, -1], scale. = TRUE, center = TRUE)
screeplot(pca, main = "", las = 1)

LoadingsAway <- data.frame(pca$rotation[,c(1:7)])     #Component Loadings
print(summary(pca))     #Importance of Components
pca$x[,c(1:7)]    #Component scores
AwayPerformance <- cbind(pca$x[,c(1:7)], AwayPerformance[,1]) #Adding PC's to dataframe

#Levelplot Away
PCS <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
pcaTableData <- pca$rotation
pcaTableData <- pcaTableData[,PCS]
pcaTableData <- t(pcaTableData)
levelplot(pcaTableData, 
          xlab=list("Principal Components", fontface="bold"),
          ylab=list("Performance Variables", fontface='bold')
)

levelAway <- levelplot(pcaTableData, col.regions = gray(0:100/100))





################################################################################
#################          Data Transformations        #########################
###############################################################################
data <- data1718        #Change to the data frame you want to transform, all seasons are added together at the end

#Making columns to store data transformations
data$HomeTeam <- factor(data$HomeTeam)
data$AwayTeam <- factor(data$AwayTeam)
teams <- levels(data$HomeTeam)

data$MatchWeek <- NA

data$CumPointsHomeTeam <- NA
data$AvgPointsHomeTeam <- NA
data$AvgPoints3HomeTeam <- NA

data$CumPointsAwayTeam <- NA
data$AvgPointsAwayTeam <- NA
data$AvgPoints3AwayTeam <- NA

data$CumGoalsScoredHome <- NA
data$CumGoalsConcededHome <- NA
data$AvgGoalsScoredHome <- NA
data$AvgGoalsConcededHome <- NA
data$AvgGoalsScored3Home <- NA
data$AvgGoalsConceded3Home <- NA

data$CumGoalsScoredAway <- NA
data$CumGoalsConcededAway <- NA
data$AvgGoalsScoredAway <- NA
data$AvgGoalsConcededAway <- NA
data$AvgGoalsScored3Away <- NA
data$AvgGoalsConceded3Away <- NA

data$HFA <- NA
data$AFD <- NA

#Loop (from line 155 to 330) to do all transformations 
#Run this loop only once for one dataset of one season
#then change the data in line 117 if a new season needs to be transformed

for(a in 1:length(teams)) {
  club <- teams[a]
  
  #Calculating Points for each game
  Points <- vector()
  k <- 1
  for (b in 1:380) {
    if (data$HomeTeam[b] == club) {
      if (data$FTR[b] == "H") {
        Points[k] <- 3
      } else if (data$FTR[b] == "D") {
        Points[k] <- 1
      } else if (data$FTR[b] == "A") {
        Points[k] <- 0
      }
      k <- k + 1
    }
    else if (data$AwayTeam[b] == club) {
      if (data$FTR[b] == "H") {
        Points[k] <- 0
      } else if (data$FTR[b] == "D") {
        Points[k] <- 1
      } else if (data$FTR[b] == "A") {
        Points[k] <- 3
      }
      k <- k + 1
    }
  }
  
  #Calculating cummulative and Average points at each gameweek
  CummalativePoints <- vector()
  AveragePoints <- vector()
  
  for(c in 2:38) {
    k <- c-1
    CummalativePoints[c] <- sum(Points[1:k])
    AveragePoints[c] <- CummalativePoints[c]/(k)
  }
  
  #Calculating Avg points last three games
  AveragePoints3 <- vector()
  for(d in 4:38) {
    k <- d-3
    l <- d-1
    AveragePoints3[d] <- sum(Points[k:l])/3
  }
  
  #Adding Points to dataframe
  k <- 1
  for (e in 1:380) {
    if(data$HomeTeam[e] == club) {
      data$CumPointsHomeTeam[e] <- CummalativePoints[k]
      data$AvgPointsHomeTeam[e] <- AveragePoints[k]
      data$AvgPoints3HomeTeam[e] <- AveragePoints3[k]
      
      data$MatchWeek[e] <- k
      
      k <- k + 1
    }
    else if(data$AwayTeam[e] == club) {
      data$CumPointsAwayTeam[e] <- CummalativePoints[k]
      data$AvgPointsAwayTeam[e] <- AveragePoints[k]
      data$AvgPoints3AwayTeam[e] <- AveragePoints3[k]
      
      data$MatchWeek[e] <- k
      
      k <- k + 1
    }
  }
  
  #Caclulating Total goals scored and conceded
  TotalGoalsScored <- vector()
  TotalGoalsConceded <- vector()
  
  HomeGoalsFor <- vector()
  HomeGoalsAgainst <- vector()
  AwayGoalsFor <- vector()
  AwayGoalsAgainst <- vector()
  
  k <- 1
  for(f in 1:380) {
    if (data$HomeTeam[f] == club) {
      TotalGoalsScored[k] <- data$FTHG[f]
      TotalGoalsConceded[k] <- data$FTAG[f]
      
      HomeGoalsFor[k] <- data$FTHG[f]
      HomeGoalsAgainst[k] <- data$FTAG[f]
      
      k <- k + 1
    }
    else if (data$AwayTeam[f] == club) {
      TotalGoalsScored[k] <- data$FTAG[f]
      TotalGoalsConceded[k] <- data$FTHG[f]
      
      AwayGoalsFor[k] <- data$FTAG[f]
      AwayGoalsAgainst[k] <- data$FTHG[f]
      
      k <- k + 1
    }
  }
  
  #Cummulative and Avg Goals for and against
  CumGoalsScored <- vector()
  CumGoalsConceded <- vector()
  
  AvgGoalsScored <- vector()
  AvgGoalsConceded <- vector()
  
  for(l in 2:38) {
    k <- l-1
    CumGoalsScored[l] <- sum(TotalGoalsScored[1:k])
    CumGoalsConceded[l] <- sum(TotalGoalsConceded[1:k])
    
    AvgGoalsScored[l] <- CumGoalsScored[l]/(k)
    AvgGoalsConceded[l] <- CumGoalsConceded[l]/(k)
  }
  
  #Avg Goals Scored and Conceded last Three Matches
  AvgGoalsScored3 <- vector()
  AvgGoalsConceded3 <- vector()
  
  for(m in 4:38) {
    k <- m-3
    l <- m-1
    AvgGoalsScored3[m] <- sum(TotalGoalsScored[k:l])/3
    AvgGoalsConceded3[m] <- sum(TotalGoalsConceded[k:l])/3
  }
  
  #Home Field Advantage and Away Field Disadvantage
  HomeGoalsFor <- na.zero(HomeGoalsFor)
  HomeGoalsAgainst <- na.zero(HomeGoalsAgainst)
  AwayGoalsFor <- na.zero(AwayGoalsFor)
  AwayGoalsAgainst <- na.zero(AwayGoalsAgainst)
  
  HFA <- vector()
  AFD <- vector()
  for (n in 2:38) {
    k <- n-1
    HFA[n] <- (sum(HomeGoalsFor[1:k])-sum(HomeGoalsAgainst[1:k]))/length(which(HomeGoalsFor[1:k] != 0))
    AFD[n] <- (sum(AwayGoalsFor[1:k])-sum(AwayGoalsAgainst[1:k]))/length(which(AwayGoalsFor[1:k] != 0))
  }
  
  #Adding Goals to dataframe
  k <- 1
  for (o in 1:380) {
    if (data$HomeTeam[o] == club) {
      data$CumGoalsScoredHome[o] <- CumGoalsScored[k]
      data$CumGoalsConcededHome[o] <- CumGoalsConceded[k]
      
      data$AvgGoalsScoredHome[o] <- AvgGoalsScored[k]
      data$AvgGoalsConcededHome[o] <- AvgGoalsConceded[k]
      
      data$AvgGoalsScored3Home[o] <- AvgGoalsScored3[k]
      data$AvgGoalsConceded3Home[o] <- AvgGoalsConceded3[k]
      
      data$HFA[o] <- HFA[k]
      
      k <- k + 1
    }
    else if (data$AwayTeam[o] == club) {
      data$CumGoalsScoredAway[o] <- CumGoalsScored[k]
      data$CumGoalsConcededAway[o] <- CumGoalsConceded[k]
      
      data$AvgGoalsScoredAway[o] <- AvgGoalsScored[k]
      data$AvgGoalsConcededAway[o] <- AvgGoalsConceded[k]
      
      data$AvgGoalsScored3Away[o] <- AvgGoalsScored3[k]
      data$AvgGoalsConceded3Away[o] <- AvgGoalsConceded3[k]
      
      data$AFD[o] <- AFD[k]
      
      k <- k + 1
    }
  }
  
}
#Adding Goal Differences
data$GoalDiffHome <- data$CumGoalsScoredHome - data$CumGoalsConcededHome
data$GoalDiffAway <- data$CumGoalsScoredAway - data$CumGoalsConcededAway
data$AvgGoalDiffHome <- data$AvgGoalsScoredHome - data$AvgGoalsConcededHome
data$AvgGoalDiffAway <- data$AvgGoalsScoredAway - data$AvgGoalsConcededAway
data$AvgGoalDiff3Home <- data$AvgGoalsScored3Home - data$AvgGoalsConceded3Home
data$AvgGoalDiff3Away <- data$AvgGoalsScored3Away - data$AvgGoalsConceded3Away

#Adding Point differences
data$PointDiff <- data$CumPointsHomeTeam - data$CumPointsAwayTeam
data$AvgPointsDiff <- data$AvgPointsHomeTeam - data$AvgPointsAwayTeam 
data$AvgPointsDiff3 <- data$AvgPoints3HomeTeam - data$AvgPoints3AwayTeam

#Adding Principal Components for Home and Away team for all Matches
data$HomePC1 <- NA
data$HomePC2 <- NA
data$HomePC3 <- NA
data$HomePC4 <- NA
data$HomePC5 <- NA
data$HomePC6 <- NA
data$HomePC7 <- NA

data$AwayPC1 <- NA
data$AwayPC2 <- NA
data$AwayPC3 <- NA
data$AwayPC4 <- NA
data$AwayPC5 <- NA
data$AwayPC6 <- NA
data$AwayPC7 <- NA

#Making PC's for teams that were not in the premier league the previous season 
#For these promoted the averages are taken of the three teams that finished at the bottom of the league

#Season1819
RelegatedHome <- HomePerformance[c(17,19,20), ] #Make a df with all relegated teams
RelegatedAway <- AwayPerformance[c(15,19,20), ]
RelegatedHome1819 <- c(mean(RelegatedHome$PC1),
                       mean(RelegatedHome$PC2),
                       mean(RelegatedHome$PC3),
                       mean(RelegatedHome$PC4),
                       mean(RelegatedHome$PC5),
                       mean(RelegatedHome$PC6),
                       mean(RelegatedHome$PC7),
                       "RelegatedHome1819")

RelegatedAway1819 <- c(mean(RelegatedAway$PC1),
                       mean(RelegatedAway$PC2),
                       mean(RelegatedAway$PC3),
                       mean(RelegatedAway$PC4),
                       mean(RelegatedAway$PC5),
                       mean(RelegatedAway$PC6),
                       mean(RelegatedAway$PC7),
                       "RelegatedAway1819")

#Season1718
RelegatedHome2 <- HomePerformance[c(34,38,39), ] #Make a df with all relegated teams
RelegatedAway2 <- AwayPerformance[c(29, 37, 40), ]
RelegatedHome1718 <- c(mean(RelegatedHome2$PC1),
                       mean(RelegatedHome2$PC2),
                       mean(RelegatedHome2$PC3),
                       mean(RelegatedHome2$PC4),
                       mean(RelegatedHome2$PC5),
                       mean(RelegatedHome2$PC6),
                       mean(RelegatedHome2$PC7),
                       "RelegatedHome1718")

RelegatedAway1718 <- c(mean(RelegatedAway2$PC1),
                       mean(RelegatedAway2$PC2),
                       mean(RelegatedAway2$PC3),
                       mean(RelegatedAway2$PC4),
                       mean(RelegatedAway2$PC5),
                       mean(RelegatedAway2$PC6),
                       mean(RelegatedAway2$PC7),
                       "RelegatedAway1718")

#Season1617
RelegatedHome3 <- HomePerformance[c(53,59,60), ] #Make a df with all relegated teams
RelegatedAway3 <- AwayPerformance[c(52,57,60), ]
RelegatedHome1617 <- c(mean(RelegatedHome3$PC1),
                       mean(RelegatedHome3$PC2),
                       mean(RelegatedHome3$PC3),
                       mean(RelegatedHome3$PC4),
                       mean(RelegatedHome3$PC5),
                       mean(RelegatedHome3$PC6),
                       mean(RelegatedHome3$PC7),
                       "RelegatedHome1617")

RelegatedAway1617 <- c(mean(RelegatedAway3$PC1),
                       mean(RelegatedAway3$PC2),
                       mean(RelegatedAway3$PC3),
                       mean(RelegatedAway3$PC4),
                       mean(RelegatedAway3$PC5),
                       mean(RelegatedAway3$PC6),
                       mean(RelegatedAway3$PC7),
                       "RelegatedAway1617")

#Adding PC's for newly promoted teams to the performance statistics dataframes
HomePerformance <- rbind(HomePerformance, RelegatedHome1617, RelegatedHome1718, RelegatedHome1819)
AwayPerformance <- rbind(AwayPerformance, RelegatedAway1617, RelegatedAway1718, RelegatedAway1819)

#Adding all PC's
#The following section needs to be adjusted each time the PC's for a team are  being added
levels(data$HomeTeam) #Helpful to look at the exact names of the teams in the dataframa
HomePerformance$Team <- factor(HomePerformance$Team)
levels(HomePerformance$Team) #Same again as before, also, some names are different in this dataset

club <- "West Ham"  #Change this to the name of the team in the match statistics dataset
temp <- "West Ham1617"    #Change this to the name of the team in the  home performance statistics dataset
temp2 <- "West Ham1617"   #Change this to the name of the team in the  away performance statistics dataset   
Home <- subset(HomePerformance, HomePerformance$Team == temp) #Subsetting to easily get the PC's
Away <- subset(AwayPerformance, AwayPerformance$Team == temp2)

#Adding the right PC's for each match for one season, run once to add PC's of one team
for (i in 1:380) {
  if (data$HomeTeam[i] == club) {
    data$HomePC1[i] <- Home$PC1
    data$HomePC2[i] <- Home$PC2
    data$HomePC3[i] <- Home$PC3
    data$HomePC4[i] <- Home$PC4
    data$HomePC5[i] <- Home$PC5
    data$HomePC6[i] <- Home$PC6
    data$HomePC7[i] <- Home$PC7
  }
  else if (data$AwayTeam[i] == club) {
    data$AwayPC1[i] <- Away$PC1
    data$AwayPC2[i] <- Away$PC2
    data$AwayPC3[i] <- Away$PC3
    data$AwayPC4[i] <- Away$PC4
    data$AwayPC5[i] <- Away$PC5
    data$AwayPC6[i] <- Away$PC6
    data$AwayPC7[i] <- Away$PC7
  }
}

#Adding to final dataframe
#Because we did the previous steps seperate for all three season, the following code is used to add everything together
finalData <- rbind(data, finalData)


#Saving to excel file (just to be sure :) )
library("writexl") #package for Writing a data frame to an excel file
write_xlsx(finalData,"C:\\Users\\micha\\Desktop\\Master\\seminar\\finalData.xlsx") #Saving to excel file (just to be sure :) )




################################################################################
#######################   Cleaning Final Data    ###############################
################################################################################
finalData[sapply(finalData, is.infinite)] <- NA     #Some values had an infinite value
finalData <- na.omit(finalData)                     #Omitting NA values
finalData <- finalData[, c(1:2,5:72)]               #Some columns needed to go

#Factor of categorical variables
finalData$AwayTeam <- factor(finalData$AwayTeam)
finalData$HomeTeam <- factor(finalData$HomeTeam)
finalData$Season <- factor(finalData$Season)
finalData$FTR <- factor(finalData$FTR)
finalData$Referee <- factor(finalData$Referee)

#Changing columns to numeric if needed
library( taRifx )
temp <- japply( finalData[, c(5:69)], which(sapply(finalData[, c(5:69)], class)=="character"), as.numeric )

#FinalData Clean
finalDataClean <- cbind(finalData[, c(1:4)], temp)