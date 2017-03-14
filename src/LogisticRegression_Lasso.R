# 2017-03-13 Logistic Regression w/ Lasso fit
#'
# -------------------------- Logistic Regression with the Lasso -------------------------- #
#' 
#' Fits a Logistic Regression model, with response variable "critical_found", and a set of predictors.
#' Using the Lasso for model selection
#' 
#' @param k An integer indicating the number of folds for cross-validation. The default value is 10
#' @param LastTrainDate An string of "YYYY-MM-DD" that indicates the split of training data and test data.
#' The taining data set is up till(include) to this date. The default is "2015-12-31" if not indicated
#' @return A list of 5:
#' $ bestlam    : Best lambda for the Lasso
#' $ roc        : List of 3: TruePosRate(True Positive rate matrix) and FalsePosRate(False Positive rate matrix),
#'                and Threshold (probability threshold to determing weather the prediction to be 1-critical or 0-non critical)
#' $ lasso.coef : coefficients of the best model chosen from cross-validation
#' $ glm_out    : The glmnet object of output model
#' $ testResults: List of 7: results of testing the model on test data set
#'                Y : matrix of inspection_IDs and original labels  
#'                probabilities : output vector of probabilities from glm model with the Lasso.
#'                predictions : output vector of predictions from glm model with the Lasso.
#'                confusionTable  : The confusion table computed from predictions and original labels
#'                CorrectPercent  : The Percentage of the correct preditions
#'                ErrorPercent    : The miss classification rate
#'                TruePositiveRate: The true positive rate computed from the confustion table
#' 
#' 
#' @seealso \code{\link{predict}} for prediction, \code{\link{cv.glmnet}} for cross-validation
#' 
#' @examples
#' # Example 1
#' #
#' # Using the default settings to 
#' # build Logistic Regression model with the Lasso as model selection method,
#' # Get coefficients and output model:
#' output = glm_Lasso ( )
#' print(output$bestlam)
#' print(output$lasso.coef)
#' plot(output$glm_out)
#' #
#' # Print the top 10 high risk inspection_IDs amoung test data set
#' # and their probabilities to be identified as high-risk
#' probs = as.data.frame(cbind(output$testResults$Y[,1], output$testResults$probabilities))
#' names(probs) = c("Insp_ID","Probability")
#' ordered = probs[order(probs[,2], decreasing = TRUE), ]
#' print(head(ordered,10))
#' #
#' # Print the confusion table
#' c = output$testResults$confusionTable
#' print(c)
#' #
#' # True Positive rate.
#' print(output$testResults$TruePositiveRate)
#' #
#' # Miss Classification rate
#' print(output$testResults$ErrorPercent)
#' #
#' # Make ROC curve evaluation, and mark the probability threshold in red:
#' FPR = output$roc$FalsePosRate
#' TPR = output$roc$TruePosRate
#' X1 = output$roc$Threshold[3]
#' Y1 = output$roc$Threshold[2]
#' plot(FPR, TPR, points(X1, Y1, col = "red",bg=2, pch=23))
#' 
#' # Example 2
#' # Specify the number of fold for cross-validation,
#' # and date to split the training data and test data
#' output = glm_Lasso(5, "2015-12-01")
#' #
#' # Make ROC curve evaluation, and mark the probability threshold in red:
#' FPR = output$roc$FalsePosRate
#' TPR = output$roc$TruePosRate
#' X1 = output$roc$Threshold[3]
#' Y1 = output$roc$Threshold[2]
#' plot(FPR, TPR, points(X1, Y1, col = "red",bg=2, pch=23))
#' 
#' @import glmnet
#' @import boot
#' @import dplyr
#' 
#' @export

glm_Lasso <- function (K = 10, LastTrainDate = "2015-12-31"){
  # Columns choosing from ABT
  C1 <- c("inspection_id", "inspection_date","critical_found",
          "maxtemp_F", "meanwindspdm", "maxhumidity", 
          "complaint", "followup", "newownerconst",#"first_inspection", 
          "prior_inspections", "prior_violations", 
          "prior_complaints", "prior_followups", "prior_highrisk_viols",  
          "prior_highrisk_ratio", "prior_critical_found", "prior_noncritical_found", 
          "days_since_first_insp", "days_since_last_insp","burgsPast90d","burgsPastWeek")
  BigTable2 <- readRDS("data_source/FoodInspectionABTv2.Rds")
  BigTable2 <- na.omit(FoodInsp1[,C1])
  
  # Prepare data for glmnet model:
  lm.dat <- BigTable2
  lm.dat$critical_found <- factor(lm.dat$critical_found)
  lm.dat$complaint <- factor(lm.dat$complaint)
  lm.dat$followup <- factor(lm.dat$followup)
  lm.dat$newownerconst <- factor(lm.dat$newownerconst)
  # Divide test data set and training data set by LastTrainDate
  train <- lm.dat[which(lm.dat$inspection_date <= LastTrainDate), ]
  test <- lm.dat[which(lm.dat$inspection_date > LastTrainDate), ]
  
  # Create data matrix for glmnet
  trainX <- data.matrix(train[,4:21])
  trainY <- data.matrix(train$critical_found)
  testX <- data.matrix(test[,4:21])
  testY <- data.matrix(test$critical_found)
  myX <- data.matrix(lm.dat[,4:21])
  myY <- data.matrix(lm.dat$critical_found)
  
  # Range of lambda for the Lasso
  grid <- 10^seq(2,-3,length=100)
  lasso.mod <- glmnet(trainX, trainY, alpha = 1, lambda = grid, family = "binomial")
  # Run 10-fold Cross-validation, choose the best lambda
  set.seed(3)
  cv.out <- cv.glmnet(trainX, trainY, alpha = 1,family = "binomial", k=K)
  bestlam <- cv.out$lambda.min
  
  # Test the model on test data set
  lasso.prob <- predict(lasso.mod,s=bestlam ,newx=testX, type ="response" )
  # Compute the threshold of porbability for critical.
  count = 0
  Tv = seq(0,1,0.05)
  p_fp = rep(0,length(Tv))
  p_tp = rep(0, length(Tv))
  ratio_tp_fp = rep(0, length(Tv))
  for (t in Tv){
    count  = count +1
    # percentage of 1 observations in the validation set,
    p = length(which(testY==1))/length(testY)
    
    # probability of the model predicting 1 while the true value of the observation is 0, 
    p_01 = sum(1*(lasso.prob>=t & testY==0))/dim(testX)[1] 
    
    # probability of the model predicting 1 when the true value of the observation is 1, 
    p_11 = sum(1*(lasso.prob>=t & testY==1))/dim(testX)[1]
    
    # probability of false-positive, 
    p_fp[count] = p_01/(1-p)
    
    # probability of true-positive, 
    p_tp[count] = p_11/p
    ratio_tp_fp[count] = p_tp[count]/p_fp[count]
  }
  cutoff <- 0.25
  tmp <- c(cutoff, p_tp[which(Tv==cutoff)], p_fp[which(Tv==cutoff)])
  names(tmp) <- c("threshold", "TPR","FPR")
  # Collect the ROC information, 
  roc = list()
  roc[[1]] <- as.matrix(tmp)
  roc[[2]] <- as.matrix(p_tp)
  roc[[3]] <- as.matrix(p_fp)
  #roc[[4]] <- plot(p_fp, p_tp, points(tmp[3], tmp[2], col = "red",bg=2, pch=23))
  roc <- setNames(roc, c("Threshold","TruePosRate","FalsePosRate"))
  
  # Preidiction on test data 
  lasso.pred <- rep(0, nrow(testY))
  lasso.pred[lasso.prob>cutoff] = 1
  # Compute the confusion table
  confusionTable <- table(lasso.pred, testY)
  TruePositiveRate <- confusionTable[2,2]/sum(confusionTable[,2])
  FalsePositiveRate <- confusionTable[2,1]/sum(confusionTable[,1])
  # Rate of correct prediction on test: around 0.8165205
  CorrectPercent <- mean(lasso.pred == testY)
  # Predition error rate on test data:0.1834795
  ErrorPercent <- 1- mean(lasso.pred == testY)
  
  # Generate the output coefficients using all of the data:
  glm_out <- glmnet(myX,myY,alpha=1,lambda=grid, family = "binomial")
  lasso.coef <- predict(glm_out,type="coefficients",s=bestlam)
  output <- list()
  output[[1]] <- bestlam
  output[[2]] <- list()
  output[[2]] <- roc
  output[[3]] <- lasso.coef
  output[[4]] <- glm_out
  output[[5]] <- list()
  output[[5]][[1]] <- cbind(test$inspection_id, testY)
  output[[5]][[2]] <- lasso.prob
  output[[5]][[3]] <- as.matrix(lasso.pred)
  output[[5]][[4]] <- confusionTable
  output[[5]][[5]] <- CorrectPercent
  output[[5]][[6]] <- ErrorPercent
  output[[5]][[7]] <- TruePositiveRate
  output[[5]] <- setNames(output[[5]], c("Y","probabilities","predictions","confusionTable", 
                                         "CorrectPercent","ErrorPercent","TruePositiveRate"))
  output <- setNames(output, c("bestlam","roc","lasso.coef","glm_out", "testResults"))
}
