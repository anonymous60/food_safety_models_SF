# 2017-03-13 Logistic Regression w/ the Lasso
#'
# -------------------------- Logistic Regression with the Lasso -------------------------- #
#' 
#' Fits a Logistic Regression model, with response variable "critical_found", and a set of predictors.
#' Using the Lasso for model selection
#' 
#' @param K An integer indicating the number of folds for cross-validation. The default value is 10
#' @param LastTrainDate An string of "YYYY-MM-DD" that indicates the split of training data and test data.
#' The training data set is up till(include) to this date. The default is "2015-12-31" if not indicated
#' @param savedata Optionally save the output to the file
#'                 "model_cv_glm_lasso.rda"
#' @return A list of 5:
#' $ bestlam    : Best lambda for the Lasso
#' $ roc        : List of 3: TruePosRate(True Positive rate matrix) and FalsePosRate(False Positive rate matrix),
#'                and Threshold (probability threshold to determing weather the prediction to be 1-critical or 0-non critical)
#' $ lasso.coef : coefficients of the best model chosen from cross-validation
#' $ glm_out    : The glmnet object of output model
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
#' 
#' @seealso \code{\link{predict}} for prediction, \code{\link{cv.glmnet}} for cross-validation
#' 
#' @examples
#' # Example 1
#' #
#' # Using the default settings to
#' # build Logistic Regression model with the Lasso as model selection method,
#' # Get coefficients and output model:
#' output = model_cv_glm_lasso()
#' print(output$bestlam)
#' print(output$lasso.coef)
#' plot(output$glm_out)
#' #
#' # Print the top 10 businesses (names and business IDs) that most likely have critical 
#' # safety issue according to the Logistic Regression(w/ Lasso) Model's prediction
#' stable = output$predictions$scoresTable
#' print(head(model_order_businesses_by_score(stable),10))
#' #
#' # View scores of restruarnts that are predicted to be at critical risk versus
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
#' output = model_cv_glm_lasso(5, "2015-12-01")
#' #
#' # Make ROC curve evaluation, and mark the probability threshold in red:
#' FPR = output$roc$FalsePosRate
#' TPR = output$roc$TruePosRate
#' X1 = output$roc$Threshold[3]
#' Y1 = output$roc$Threshold[2]
#' plot(FPR, TPR, points(X1, Y1, col = "red",bg=2, pch=23))
#'
#' @export
#' @author 

model_cv_glm_lasso <- function (K = 10,
                                LastTrainDate = "2015-12-31",
                                savedata = FALSE) {
  # validate inputs
  K <- as.integer(K)
  if (!is.atomic(K) || is.na(K)) stop("K = ", K, " is invalid")
  LastTrainDate <- as.Date(LastTrainDate)
  if (!is.atomic(LastTrainDate) || is.na(LastTrainDate))
    stop("LastTrainDate = ", LastTrainDate, " is invalid")
    
  print("Trains Logistic Regression with the Lasso using different lambda values, cross-validatig to select the best lambda , this may take a while...")

  # Columns choosing from ABT
  C0 <- c("business_name", "business_id", 
          "inspection_id", "inspection_date","critical_found",
          "maxtemp_F", "meanwindspdm", "maxhumidity", 
          "complaint", "followup", "newownerconst",#"first_inspection", 
          "prior_inspections", "prior_violations", 
          "prior_complaints", "prior_followups", "prior_highrisk_viols",  
          "prior_highrisk_ratio", "prior_critical_found", "prior_noncritical_found", 
          "days_since_first_insp", "days_since_last_insp","burgsPast90d","burgsPastWeek")
  C1 <- c("inspection_id", "inspection_date","critical_found",
          "maxtemp_F", "meanwindspdm", "maxhumidity", 
          "complaint", "followup", "newownerconst",#"first_inspection", 
          "prior_inspections", "prior_violations", 
          "prior_complaints", "prior_followups", "prior_highrisk_viols",  
          "prior_highrisk_ratio", "prior_critical_found", "prior_noncritical_found", 
          "days_since_first_insp", "days_since_last_insp","burgsPast90d","burgsPastWeek")
  BigTable2 <- Model_ABT[,C0]
  BigTable2<- BigTable2[stats::complete.cases(BigTable2[,C1]),]

  # Prepare data for glmnet model:
  lm.dat <- BigTable2[,C1]
  lm.dat$critical_found <- factor(lm.dat$critical_found)
  lm.dat$complaint <- factor(lm.dat$complaint)
  lm.dat$followup <- factor(lm.dat$followup)
  lm.dat$newownerconst <- factor(lm.dat$newownerconst)
  # Divide test data set and training data set by LastTrainDate
  train <- lm.dat[which(lm.dat$inspection_date <= LastTrainDate), ]
  test <- lm.dat[which(lm.dat$inspection_date > LastTrainDate), ]
  testBusiness <- BigTable2[which(lm.dat$inspection_date > LastTrainDate),
                            c("business_name", "business_id")]
  
  # Create data matrix for glmnet
  trainX <- data.matrix(train[,4:21])
  trainY <- data.matrix(train$critical_found)
  testX <- data.matrix(test[,4:21])
  testY <- data.matrix(test$critical_found)
  myX <- data.matrix(lm.dat[,4:21])
  myY <- data.matrix(lm.dat$critical_found)
  
  # Range of lambda for the Lasso
  grid <- 10^seq(2,-3,length=100)
  lasso.mod <- glmnet::glmnet(trainX, trainY, alpha = 1, lambda = grid, family = "binomial")
  # Run 10-fold Cross-validation, choose the best lambda
  set.seed(3)
  cv.out <- glmnet::cv.glmnet(trainX, trainY, alpha = 1,family = "binomial", nfolds = K)
  bestlam <- cv.out$lambda.min
  
  # Test the model on test data set
  lasso.prob <- predict(lasso.mod,s=bestlam ,newx=testX, type ="response" )
  # Compute the threshold of porbability for critical.
  count <- 0
  Tv <- seq(0,1,0.05)
  p_fp <- rep(0,length(Tv))
  p_tp <- rep(0, length(Tv))
  ratio_tp_fp <- rep(0, length(Tv))
  for (t in Tv){
    count <- count +1
    # percentage of 1 observations in the validation set,
    p <- length(which(testY==1))/length(testY)
    # probability of the model predicting 1 while the true value of the observation is 0, 
    p_01 <- sum(1*(lasso.prob>=t & testY==0))/dim(testX)[1] 
    # probability of the model predicting 1 when the true value of the observation is 1, 
    p_11 <- sum(1*(lasso.prob>=t & testY==1))/dim(testX)[1]
    # probability of false-positive, 
    p_fp[count] <- p_01/(1-p)
    # probability of true-positive, 
    p_tp[count] <- p_11/p
    ratio_tp_fp[count] <- p_tp[count]/p_fp[count]
  }
  cutoff <- 0.25
  tmp <- c(cutoff, p_tp[which(Tv==cutoff)], p_fp[which(Tv==cutoff)])
  names(tmp) <- c("threshold", "TPR","FPR")
  # Collect the ROC information, 
  roc <- list()
  roc[[1]] <- as.matrix(tmp)
  roc[[2]] <- as.matrix(p_tp)
  roc[[3]] <- as.matrix(p_fp)
  roc <- stats::setNames(roc, c("Threshold","TruePosRate","FalsePosRate"))
  
  # Preidiction on test data 
  lasso.pred <- rep(0, nrow(testY))
  lasso.pred[lasso.prob>cutoff] <- 1
  # Compute the confusion table
  confusionTable <- table(lasso.pred, testY)
  TruePositiveRate <- confusionTable[2,2]/sum(confusionTable[,2])
  FalsePositiveRate <- confusionTable[2,1]/sum(confusionTable[,1])
  # Rate of correct prediction on test: around 0.8165205
  CorrectPercent <- mean(lasso.pred == testY)
  # Predition error rate on test data:0.1834795
  ErrorPercent <- 1- mean(lasso.pred == testY)
  
  # Generate the output coefficients using all of the data:
  glm_out <- glmnet::glmnet(myX,myY,alpha=1,lambda=grid, family = "binomial")
  lasso.coef <- predict(glm_out,type="coefficients",s=bestlam)
  
  # Collect the output
  rf_out <- as.data.frame(cbind(testBusiness,test$inspection_id,testY,lasso.prob,lasso.pred))
  colnames(rf_out) <- c("business_name", "business_id", "inspection_id", "original_critical",
                        "scores", "predictions")
  glm_output <- list()
  glm_output[[1]] <- bestlam
  glm_output[[2]] <- list()
  glm_output[[2]] <- roc
  glm_output[[3]] <- lasso.coef
  glm_output[[4]] <- glm_out
  glm_output[[5]] <- list()
  glm_output[[5]][[1]] <- rf_out
  glm_output[[5]][[2]] <- confusionTable
  glm_output[[5]][[3]] <- CorrectPercent
  glm_output[[5]][[4]] <- ErrorPercent
  glm_output[[5]][[5]] <- TruePositiveRate
  glm_output[[5]] <- stats::setNames(glm_output[[5]],
                                     c("scoresTable",
                                       "confusionTable",
                                       "CorrectPercent",
                                       "ErrorPercent",
                                       "TruePositiveRate"))
  glm_output <- stats::setNames(glm_output,
                                c("bestlam",
                                  "roc",
                                  "lasso.coef",
                                  "glm_out",
                                  "predictions"))
  if (savedata)
    save(glm_output, file = "model_cv_glm_lasso.rda")
  return(glm_output)
}
