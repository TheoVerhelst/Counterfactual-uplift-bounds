# Code for the project Machu-Picchu written by Théo Verhelst
# Supervisors at Orange: Denis Mercier, Jeevan Shrestha
# Academic supervision: Gianluca Bontempi (ULB MLG)

library(caret) # createFolds
source("utils.R")
source("uplift_models.R")
source("easy_ensemble.R")
source("estimate_cf.R")

data <- read.csv("orange_churn.csv")
data <- list(
    x = remove_columns(data, c("y", "t")),
    y = data$y,
    t = data$t
)

compute_bounds <- function(data, pred, use_churn_convention = TRUE) {
    k <- 5
    folds <- createFolds(data$y, k = k)
    
    results <- lapply(1:k, function(i) {
        message(as.character(Sys.time()), ": fold ", i)
        data_train <- subset_data(data, -folds[[i]])
        data_test <- subset_data(data, folds[[i]])

        models <- train_uplift_rf(data_train, ntree = 100, split_method = "ED")
        pred <- predict_cf(models, data_test)
        pred$index <- folds[[i]]
        pred$y <- data_test$y
        pred$t <- data_test$t

        return(list(
            pred = pred,
            models = models
        ))
    })
    
    pred <- do.call(rbind, lapply(results, function(result) result$pred))
    models <- lapply(results, function(result) result$models)
    
    pred$S_0 <- calibrate_score(pred$S_0, mean(pred$y[pred$t == "0"] == "1"))
    pred$S_1 <- calibrate_score(pred$S_1, mean(pred$y[pred$t == "1"] == "1"))

    if (use_churn_convention) {
        pred$uplift <- pred$S_0 - pred$S_1
    } else {
        pred$uplift <- pred$S_1 - pred$S_0
    }

    stats <- as.data.frame(estimate_from_empirical(pred, use_churn_convention))
    stats$number_customers <- nrow(data$x)
    return(list(pred = pred, models = models, stats = stats))
}


base_path <- "runs/"
message(as.character(Sys.time()), ": start training")
results <- compute_bounds(data, pred, use_churn_convention=TRUE)
saveRDS(object = results, file = paste0(base_path, "results_uplift_rf.rds"))
message(as.character(Sys.time()), ": done")
