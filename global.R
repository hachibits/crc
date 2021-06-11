library(tidyverse)
library(sparsediscrim)
library(randomForest)
library(e1071)
library(limma)
library(dplyr)

if (file.exists("./data/cleaned/") && !file.exists("./data/proteomics.RData")) {
  filbin_data <- read.csv("./data/cleaned/filbin_data.csv",
                          stringsAsFactors = FALSE,
                          check.names = FALSE)
  shen_data <- read.csv("./data/cleaned/filbin_data.csv",
                        stringsAsFactors = FALSE,
                        check.names = FALSE)
  
  save.image(file="./data/proteomics.RData")
}

load("./data/proteomics.RData")


plabel <- ifelse(filbin_data$group %in% c("Severe", "non-Severe"), "COVID-19", "non-COVID-19")

pdata <- filbin_data %>%
  dplyr::select(where(is.numeric))

normalise = function(df, n) {
  log2(df[rowSums(is.na(df)) < n*0.5,])
  
  df <- limma::normalizeBetweenArrays(df, method = "scale")
  
  pmedian = apply(df, 2, median, na.rm = TRUE)
  adj = pmedian - median(pmedian)
  df = sweep(df, 2, adj, FUN = "-")
  
  return(df)
}

pdata <- normalise(pdata, nrow(pdata)) %>%
  cbind(as.data.frame(plabel))

model.svm <- svm(as.factor(plabel) ~ ., data=pdata, probability=TRUE)

pred.svm <- predict(model.svm, pdata, probability=TRUE)
pred.svm.prob <- attr(pred.svm, 'probabilities')[,1]


load("./data/proteomics.RData")

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

shen_numeric <- normalise(shen_numeric, nrow(shen_numeric))
filbin_numeric <- normalise(filbin_numeric, nrow(filbin_numeric))


# Support vector machine ----
X <- as.matrix(filbin_numeric)

rownames(X) <- paste("sample", seq_len(nrow(X)))
y <- filbin_data$group

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
  
  # SVM
  trained_svm <<- svm(X_train[, selected_features],
                      factor(y_train), type = "C")
  predicted_svm <- predict(trained_svm, X_test[, selected_features])
  names(predicted_svm) <- names(y_test)
  pred_svm <- append(pred_svm, predicted_svm)
  cv_acc_svm[j] <- mean(predicted_svm == y_test)
  
}

# ----
linebreaks <- function(n) {
  HTML(strrep(br(), n))
}