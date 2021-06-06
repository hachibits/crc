library(tidyverse)
library(sparsediscrim)
library(randomForest)
library(e1071)
library(limma)
library(dplyr)

shen_data <- read.csv("../data/cleaned/shen_data.csv")
filbin_data <- read.csv("../data/cleaned/filbin_data.csv")

# Omit non-COVID-19 instances ----
plabel <- as.data.frame(filbin_data$group)
ID <- plabel != "non-COVID-19"
filbin_data <- filbin_data[ID, ]

# Intersect structure of dataframes ----
shen_numeric <- shen_data %>%
  dplyr::select(which(colnames(shen_data) %in% colnames(filbin_data))) %>%
  dplyr::select(where(is.numeric))
filbin_numeric <- filbin_data %>%
  dplyr::select(which(colnames(shen_data) %in% colnames(filbin_data))) %>%
  dplyr::select(where(is.numeric))

# Normalise `filbin` and `shen` dataframes ----
normalise = function(df, n) {
  #df_numeric = df[, !names(df) %in% c("group")]
  # Log-transformation and remove non-informative proteins
  log2(df[rowSums(is.na(df)) < n*0.5,])
  # Median normalisation
  pmedian = apply(df, 2, median, na.rm = TRUE)
  adj = pmedian - median(pmedian)
  df = sweep(df, 2, adj, FUN = "-")
  return(df)
}

filbin_numeric <- normalise(filbin_numeric, nrow(filbin_numeric))
shen_numeric <- normalise(shen_numeric, nrow(shen_numeric))

# k-nearest neighbours ----
#covid_id <- !(filbin_data$group == "non-COVID-19")
X <- as.matrix(filbin_numeric)

rownames(X) <- paste("sample", seq_len(nrow(X)))
y <- filbin_data$group

# cvK <- 5  # number of CV folds
# cv_50acc_knn <- cv_acc <- c()
# for (i in 1:50) {
#   cvSets <- cvTools::cvFolds(nrow(X), cvK)  # permute all the data, into 3 folds
#   cv_acc <- NA  # initialise results vector
#   for (j in 1:cvK) {
#     test_id <- cvSets$subsets[cvSets$which == j]
#     X_test <- X[test_id, ]
#     X_train <- X[-test_id, ]
#     y_test <- y[test_id]
#     y_train <- y[-test_id]
#     knn5 <- class::knn(train = X_train, test = X_test, cl = y_train, k = 10)
#     cv_acc[j] <- table(knn5, y_test) %>% diag %>% sum %>% `/`(length(y_test))
#   }
#   cv_50acc_knn <- append(cv_50acc_knn, mean(cv_acc))
# }

# Support vector machine ----
cvK <- 5  # number of CV folds
cv_acc_svm <- pred_svm <- c()

cvSets <- cvTools::cvFolds(nrow(X), cvK)  # permute all the data, into 5 folds

for (j in 1:cvK) {
  test_id <- cvSets$subsets[cvSets$which == j]
  X_test <- X[test_id, ]
  X_train <- X[-test_id, ]
  y_test <- y[test_id]
  y_train <- y[-test_id]
  
  design <- model.matrix(~ y_train)
  
  # Fit the limma model
  fit <- lmFit(t(X_train), design)
  fit2 <- eBayes(fit)
  tT <- topTable(fit2, coef = 2, number = Inf, sort.by ="t")
  selected_features <<- rownames(tT)[1:200]
  
  ##SVM
  trained_svm <<- svm(X_train[, selected_features],
                     factor(y_train), type = "C")
  predicted_svm <- predict(trained_svm, X_test[, selected_features])
  names(predicted_svm) <- names(y_test)
  pred_svm <- append(pred_svm, predicted_svm)
  cv_acc_svm[j] <- mean(predicted_svm == y_test)

}

# for (i in 1:50) {
#   cvSets <- cvTools::cvFolds(nrow(X), cvK)  # permute all the data, into 5 folds
#   
#   cv_acc_svm <- NA
#   for (j in 1:cvK) {
#     test_id <- cvSets$subsets[cvSets$which == j]
#     X_test <- X[test_id, ]
#     X_train <- X[-test_id, ]
#     y_test <- y[test_id]
#     y_train <- y[-test_id]
#     
#     design <- model.matrix(~ y_train)
#     
#     # Fit the limma model
#     fit <- lmFit(t(X_train), design)
#     fit2 <- eBayes(fit)
#     tT <- topTable(fit2, coef = 2, number = Inf, sort.by ="t")
#     selected_features <<- rownames(tT)[1:200]
#     
#     ##SVM
#     trained_svm <<- svm(X_train[, selected_features],
#                        factor(y_train), type = "C")
#     predicted_svm <- predict(trained_svm, X_test[, selected_features])
#     names(predicted_svm) <- names(y_test)
#     pred_svm <- append(pred_svm, predicted_svm)
#     cv_acc_svm[j] <- mean(predicted_svm == y_test)
#   
#   }
#   cv_50acc_svm  <- append(cv_50acc_svm , mean(cv_acc_svm))
# }

svm_acc <<- cv_acc_svm;

# ----
# df <- read.csv("./scaffold.csv")
# predicted_svm <- predict(trained_svm, df[, selected_features])