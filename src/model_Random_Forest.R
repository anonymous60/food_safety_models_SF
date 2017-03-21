# 2017-03-14 Random Forest
#'
# -------------------------- model_rf_mTry() -------------------------- #
#' Trains a random forest model
#' @description Build random forest models with different values for m, use validation set approach to  
#' compute miss-classification rate, true positive rate and false positive rate for different m values
#' @param LastTrainDate An string of "YYYY-MM-DD" that indicates the split of training data and test data.
#' The taining data set is up till(include) to this date. The default value is "2015-12-31" if not indicated
#' @return A data frame that contains miss-classification rate, true positive rate and false positive rate for different m values
#' @seealso \code{\link{model_rf_mTry_plot}}, \code{\link{model_rf_fit}}, 
#' \code{\link{model_plot_scores_vs_critical}},  \code{\link{model_order_businesses_by_score}}
#' @examples
#' # Example 
#' # Specify the LastTrainDate to split the training data and test data
#' output = model_rf_mTry("2015-12-01")
#' # Visualize this output data frame:
#' # Plot the miss-classification rate, true positive rate and
#' # false positive rate against different m values
#' model_rf_mTry_plot()
#' @import randomForest randomForest
#' @export
#'
#' @author
model_rf_mTry <- function (LastTrainDate = "2015-12-31") {
  # validate inputs
  LastTrainDate <- as.Date(LastTrainDate)
  if (!is.atomic(LastTrainDate) || is.na(LastTrainDate))
    stop("LastTrainDate = ", LastTrainDate, " is invalid")
    
  print("Trains random forest against all possible m values(the mumber of variables randomly sampled as candidates at each split), this may take a while...")

  # Columns choosing from ABT
  C1 <- c("inspection_id", "inspection_date","critical_found",
          "maxtemp_F", 
          "complaint", "followup", "newownerconst", 
          "prior_highrisk_viols", "prior_critical_found", 
          "days_since_last_insp","burgsPast90d")
  #rawTable <- readRDS("data_source/FoodInspectionABTv2.Rds")
  #load( "Model_ABT.rda" )
  BigTable <- Model_ABT[,C1]
  BigTable <- stats::na.omit(BigTable[,C1])
  
  # Prepare data for glmnet model:
  lm.dat <- BigTable[,C1]
  lm.dat$critical_found <- factor(lm.dat$critical_found)
  lm.dat$complaint <- factor(lm.dat$complaint)
  lm.dat$followup <- factor(lm.dat$followup)
  lm.dat$newownerconst <- factor(lm.dat$newownerconst)
  lm.dat$prior_highrisk_viols <-pmin(lm.dat$prior_highrisk_viols, 1)
  lm.dat$prior_critical_found <- factor(lm.dat$prior_critical_found)
  lm.dat$days_since_last_insp <- ifelse(lm.dat$days_since_last_insp > 360, 1L, 0L)
  lm.dat$burgsPast90d <- pmin(lm.dat$burgsPast90d, 70)
  
  # Divide test data set and training data set by LastTrainDate
  train <- lm.dat[which(lm.dat$inspection_date <= LastTrainDate), ]
  test <- lm.dat[which(lm.dat$inspection_date > LastTrainDate), ]
  
  # Create data matrix for glmnet
  trainX <- data.matrix(train[,4:11])
  trainY <- train$critical_found
  testX <- data.matrix(test[,4:11])
  testY <- test$critical_found
  myX <- data.matrix(lm.dat[,4:11])
  myY <- lm.dat$critical_found
  
  MSE <- rep(0, ncol(trainX))
  TPR <- rep(0, ncol(trainX))
  FPR <- rep(0, ncol(trainX))
  set.seed(3)
  for (i in 1:ncol(trainX)){
    set.seed(7)
    rf.rel = randomForest(critical_found~.,data = train[,3:11], mtry = i,
                          importance= TRUE, corr.bias = TRUE, cutoff = c(0.9,0.1) )
    yhat.rf = predict(rf.rel ,newdata= test[,3:11], type = "response")
    MSE[i] = mean(yhat.rf != testY)
    ct = table(yhat.rf,testY)
    TPR[i] <- ct[2,2]/sum(ct[,2])
    FPR[i] <- ct[2,1]/sum(ct[,1])
  }
  Model_randomForest_mTry <- as.data.frame(rbind(MSE, TPR, FPR))
  rownames(Model_randomForest_mTry) <- c("MSE","TPR","FPR")
  colnames(Model_randomForest_mTry) <- c("m=1","m=2","m=3","m=4","m=5","m=6","m=7","m=8")
  save(Model_randomForest_mTry, file = "Model_randomForest_mTry.rda")
  return(Model_randomForest_mTry)
}


# -------------------------- model_rf_mTry_plot() -------------------------- #
#' Plots a random forest model
#' @description Visualize the output of function model_rf_mTry().
#' Plot the miss-classification rate, true positive rate and false positive rate for different m values.
#' The ideal pick of m should yeilds high true positive rate and relatively low false positive rate and low miss-classification rate.
#' @param trained A logical variable to indicate if function model_rf_mTry() has been run,
#'                the default value is TRUE.
#' If it's FALSE, the function will call model_rf_mTry() first 
#' @return A plot of the data frame
#' @seealso \code{\link{model_rf_mTry}}, \code{\link{model_rf_fit}}, 
#' \code{\link{model_plot_scores_vs_critical}},  \code{\link{model_order_businesses_by_score}}
#' @importFrom graphics plot
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @export
#' 
#' @author
model_rf_mTry_plot <- function (trained = TRUE) {
  if (trained == TRUE) {
    # trained, then load the randomForest_m data frame
    #rf_m <- load("Model_randomForest_mTry.rda")
  }else{
    # Un-trained, Run model_rf_mTry using default settings.
    Model_randomForest_mTry <- model_rf_mTry()
  }
  # Plot the miss-classification rate, true positive rate and false positive rate in one plot
  plot(as.numeric(Model_randomForest_mTry[1,]), type = 'b', col = "red",
       ylab = "", xlab = "m",
       ylim=c(0,1), xlim = c(1,ncol(Model_randomForest_mTry)))
  lines(as.numeric(Model_randomForest_mTry[2,]), type = 'b',col = "blue")
  lines(as.numeric(Model_randomForest_mTry[3,]), type = 'b', col = "green")
  legend('topright', rownames(Model_randomForest_mTry) , 
         lty=1, col=c('red', 'blue', 'green'), bty='n', cex=0.75)
}

#
# -------------------------- model_rf_fit() -------------------------- #
#' Fits a random forest model
#' @description Fit the Random Forest model, specify the value of mTry and the LastTrainDate :  
#' @param mTry Number of variables randomly sampled as candidates at each split. 
#' The default value is 6.
#' @param LastTrainDate The last date of the training data set. After spliting the entir data set into training data and test data,
#' The taining data set is up till(include) to this date. The default is "2015-12-31" if not indicated
#' @return A list of 3:
#' $ randomForest_out    : The output model
#' $ variableImportance  : The variable importance matrix
#' $ predictions: List of 5: The prediction results of the model on test data set
#'                scoresTable: The data frome containing "business_name", "business_id", "inspection_id", 
#'                             "original_critical"(original labels of critical or non-critical), 
#'                             "scores"(probabilities output from the model), 
#'                             "predictions" (binary output from the classifier)
#'                confusionTable  : The confusion table computed from predictions and original labels
#'                CorrectPercent  : The Percentage of the correct preditions
#'                ErrorPercent    : The miss classification rate
#'                TruePositiveRate: The true positive rate computed from the confustion table
#'
#' @seealso \code{\link{model_rf_mTry}}, \code{\link{model_rf_mTry_plot}},
#' \code{\link{model_plot_scores_vs_critical}},  \code{\link{model_order_businesses_by_score}}
#' @examples
#' # Example 1
#' #
#' # Using the default settings to fit the Random Forest model
#' # 
#' output = model_rf_fit( )
#' # Print the variables importance of the fitted model
#' print(output$variableImportance)
#' # View the variables importance
#' require("randomForest")
#' randomForest::varImpPlot(output$randomForest_out)
#' #
#' # Print the top 10 businesses (names and business IDs) that most likely have critical 
#' # safety issue according to the Random Forest model's prediction
#' stable = output$predictions$scoresTable
#' print(head(model_order_businesses_by_score(stable),10))
#' #
#' # View the scores of likelyhood in boxplot:
#' # View the scores of restruarnts that are predicted to be at critical risk versus
#' # restaurants that are predicted to be at non-critical risk in a box plot
#' model_plot_scores_vs_critical(stable)
#' #
#' # Print the confusion table
#' c = output$predictions$confusionTable
#' print(c)
#' #
#' # True Positive rate.
#' print(output$predictions$TruePositiveRate)
#' #
#' # Miss Classification rate
#' print(output$predictions$ErrorPercent)
#' #
#' #' # Example 2
#' #
#' # Choose m=6 , and LastTrainDate= "2015-12-01" to fit the Random Forest model
#' output2 = model_rf_fit ( 6, "2015-12-01" )
#' # Print the variable importance of the fitted model:
#' print(output2$variableImportance)
#' require("randomForest")
#' randomForest::varImpPlot(output2$randomForest_out)
#' #
#' @importFrom randomForest randomForest
#' @importFrom stats predict
#' @export
#' 
#' @author
model_rf_fit <- function( mTry = NA, LastTrainDate = "2015-12-31" ){
  if (is.na(mTry)){
    #Using defaute mtry settting for Random Forest classification
    mTry = 6
  }
  # otherwise, using input mTry
  
  # Columns choosing from ABT
  C0 <- c("business_name", "business_id", 
          "inspection_id", "inspection_date","critical_found",
          "maxtemp_F", 
          "complaint", "followup", "newownerconst", 
          "prior_highrisk_viols", "prior_critical_found", 
          "days_since_last_insp","burgsPast90d")
  C1 <- c("inspection_id", "inspection_date","critical_found",
          "maxtemp_F", 
          "complaint", "followup", "newownerconst", 
          "prior_highrisk_viols", "prior_critical_found", 
          "days_since_last_insp","burgsPast90d")
  #rawTable <- readRDS("data_source/FoodInspectionABTv2.Rds")
  #load( "Model_ABT.rda" )
  BigTable <- Model_ABT[,C0]
  BigTable<- BigTable[stats::complete.cases(BigTable[,C1]),]
  
  # Prepare data for glmnet model:
  lm.dat <- BigTable[,C1]
  lm.dat$critical_found <- factor(lm.dat$critical_found)
  lm.dat$complaint <- factor(lm.dat$complaint)
  lm.dat$followup <- factor(lm.dat$followup)
  lm.dat$newownerconst <- factor(lm.dat$newownerconst)
  lm.dat$prior_highrisk_viols <-pmin(lm.dat$prior_highrisk_viols, 1)
  lm.dat$prior_critical_found <- factor(lm.dat$prior_critical_found)
  lm.dat$days_since_last_insp <- ifelse(lm.dat$days_since_last_insp > 360, 1L, 0L)
  lm.dat$burgsPast90d <- base::pmin(lm.dat$burgsPast90d, 70)
  
  # Divide test data set and training data set by LastTrainDate
  train <- lm.dat[which(lm.dat$inspection_date <= LastTrainDate), ]
  test <- lm.dat[which(lm.dat$inspection_date > LastTrainDate), ]
  #trainBusiness <- BigTable[which(lm.dat$inspection_date <= LastTrainDate),c("business_name", "business_id")]
  testBusiness <- BigTable[which(lm.dat$inspection_date > LastTrainDate),c("business_name", "business_id")]
  
  
  # Create data matrix for glmnet
  trainX <- data.matrix(train[,4:11])
  trainY <- train$critical_found
  testX <- data.matrix(test[,4:11])
  testY <- test$critical_found
  myX <- data.matrix(lm.dat[,4:11])
  myY <- lm.dat$critical_found

  set.seed(3)
  # Random Forest Model
  rf.rel = randomForest(critical_found~.,data = train[,3:11], mtry=mTry,
                          importance= TRUE, cutoff = c(0.9,0.1))
  # Testing the model on the test data
  yprob.rf <- predict(rf.rel ,newdata= test[,3:11],type="prob")[ , 2]
  yhat.rf <- predict(rf.rel ,newdata= test[,3:11])

  confusionTable <- table(yhat.rf,testY)
  TruePositiveRate <- confusionTable[2,2]/sum(confusionTable[,2])
  FalsePositiveRate <- confusionTable[2,1]/sum(confusionTable[,1])
  # Rate of correct prediction 
  CorrectPercent <- mean(yhat.rf == testY)
  ErrorPercent <- mean(yhat.rf != testY)
  
  # Create the output model, train on full data
  rf.rel1 <- randomForest(x = myX, y = myY, mtry = mTry,
                         importance= TRUE)
  varImp <- varImpPlot(rf.rel1)
  
  # Creare the output like
  rf_out <- as.data.frame(cbind(testBusiness,test$inspection_id,testY,yprob.rf,yhat.rf))
  colnames(rf_out) <- c("business_name", "business_id", "inspection_id", "original_critical",
                        "scores", "predictions")
  rf_output <- list()
  rf_output[[1]] <- rf.rel1
  rf_output[[2]] <- varImp
  rf_output[[3]] <- list() 
  rf_output[[3]][[1]] <- rf_out
  rf_output[[3]][[2]] <- confusionTable
  rf_output[[3]][[3]] <- CorrectPercent
  rf_output[[3]][[4]] <- ErrorPercent
  rf_output[[3]][[5]] <- TruePositiveRate
  rf_output[[3]] <- stats::setNames(rf_output[[3]],
                                    c("scoresTable", "confusionTable",
                                      "CorrectPercent", "ErrorPercent", "TruePositiveRate"))
  rf_output <- stats::setNames(rf_output,
                               c("randomForest_out", "variableImportance", "predictions"))
  save(rf_output, file = "Model_randomForest_output.rda")
  return(rf_output)
  
}
#
# -------------------------- model_plot_scores_vs_critical() -------------------------- #
#' Plots scores vs critical
#' @description View the scores of likelyhood in boxplot, i.e. scores of restaurants that are predicted to be 
#' at critical risk versus restaurants that are predicted to be at non-critical risk in a boxplot
#' @param stable An data frame from the output by runing model_rf_fit() or model_cv_glm_lasso()
#' @return boxplot of the data
#' @seealso \code{\link{model_rf_mTry}},\code{\link{model_rf_mTry_plot}},\code{\link{model_rf_fit}}, 
#' \code{\link{model_order_businesses_by_score}}
#' @examples
#' # Example 1
#' # Get the predicted scores of restaurants by Logistic Regression model with the Lasso
#' # View scores of restruarnts that are predicted to be at critical risk versus
#' # restaurants that are predicted to be at non-critical risk in a box plot
#' library("sffoodinspectr")
#' output = model_cv_glm_lasso( )
#' stable = output$predictions$scoresTable
#' model_plot_scores_vs_critical(stable)
#' #
#' #' # Example 2
#' # Get the predicted scores of restaurants by Random Forest model
#' # View scores of restruarnts that are predicted to be at critical risk versus
#' # restaurants that are predicted to be at non-critical risk in a box plot
#' output = model_rf_fit( )
#' stable = output$predictions$scoresTable
#' model_plot_scores_vs_critical(stable)
#' @importFrom graphics boxplot
#' @importFrom graphics legend
#' @export
#' 
#' @author
model_plot_scores_vs_critical <- function (stable){
  if (sum(c("scores","original_critical") %in% colnames(stable)) ==2){
    # View the scores of restaurants that are predicted to be at critical risk Versus
    # restaurants that are predicted to be at non-critical risk
    boxplot(stable$scores ~ stable$original_critical, 
            col = c("green","red") , ylab = "scores", xlab = "non-critical V.S. critical")
    legend('topleft', c("non-critical", "critical") , 
           lty=1, lwd = 7, col=c('green', 'red'), bty='n', cex=0.7)
  } else {
    print("scores table not found or content is not complete!")
  }
}
#
# -------------------------- model_order_businesses_by_score() -------------------------- #
#' Returns businesses ordered by scores
#' @description Order the restaurant (business names and business IDs) by scores in the decreasing order.
#' Scores are produced by prediction model, the higher score indicates higher probability 
#' that the business would have critical safety issue.
#' @param stable A data frame from the output by runing model_rf_fit() or model_cv_glm_lasso()
#' @return An data frame of businesses order by "scores" and "predictions" 
#' @seealso \code{\link{model_rf_mTry}}, \code{\link{model_rf_mTry_plot}},\code{\link{model_rf_fit}},
#' \code{\link{model_plot_scores_vs_critical}}
#' @examples
#' # Example 1
#' # Print the top 10 businesses (names and business IDs) that most likely have critical 
#' # safety issue according to the Random Forest model's prediction
#' output = model_rf_fit( )
#' stable = output$predictions$scoresTable
#' print(head(model_order_businesses_by_score(stable),10))
#' 
#' # Example 2
#' # Print the top 10 businesses (names and business IDs) that most likely have critical 
#' # safety issue according to the Logistic Regression(w/ Lasso) Model's prediction
#' output = model_cv_glm_lasso ( )
#' stable = output$predictions$scoresTable
#' print(head(model_order_businesses_by_score(stable),10))
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr collect
#' @importFrom magrittr %>%
#' @export
#' 
#' @author
model_order_businesses_by_score <- function (stable) {
  if (sum(c("business_name","business_id","scores","predictions") %in% colnames(stable)) == 4){
    predictions <- scores <- NULL
    tmp = stable[,c("business_name","business_id","scores","predictions")]
    # Order the table by scores
    ordered_table <- tmp %>%
      arrange(desc(predictions), desc(scores)) %>%
      collect()
    return(ordered_table)
  } else {
    print("scores table not found or content is not complete!")
    return(NULL)
  }
}

