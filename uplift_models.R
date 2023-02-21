# Code for the project Machu-Picchu written by Théo Verhelst
# Supervisors at Orange: Denis Mercier, Jeevan Shrestha
# Academic supervision: Gianluca Bontempi (ULB MLG)

# Collection of uplift models. Some are implemented manually, other come from
# libraries. All models using a base learner (meta-models) use a random forest
# as base learner.
#
# Important: all uplift models use the "sales" convention, meaning that Y = 1
# is the outcome to be maximized (so Y = 1 means a sale, or no churn). Uplift
# is defined as P(Y_1 = 1 | x) - P(Y_0 = 1 | x). If you need the other
# convention, simply take the opposite of the predictions of the uplift model.

library(uplift)
source("utils.R")
suppressMessages(library(randomForest))

predict_rf <- function(model, x) predict(model, x, type = "prob")[, 2]

rf_param_space <- list(
    "ntree" = list(
        "type" = "set",
        "values" = 100
    )
)

# Generic function to predict uplift
predict_uplift <- function(model, ...) {
    UseMethod("predict_uplift", model)
}

# Base implementation that predicts uplift in terms of counterfactuals
# This is used by many models
predict_uplift_generic <- function(model, data) {
    pred <- predict_cf(model, data)
    return(pred$S_1 - pred$S_0)
}

# Generic function to predict counterfactuals
predict_cf <- function (model, ...) {
    UseMethod("predict_cf", model)
}

# T-feature model

train_tfeature <- function(data, ...) {
    x <- data$x
    x$treatment <- as.factor(as.character(data$t))
    return(structure(list(model = randomForest(x, data$y, ...)), class="tfeature"))
}

predict_cf.tfeature <- function(model, data) {
    x_t0 <- data$x
    x_t1 <- data$x
    x_t0$treatment <- factor("0", levels = c("0", "1"))
    x_t1$treatment <- factor("1", levels = c("0", "1"))
    return(data.frame(S_0 = predict_rf(model$model, x_t0), S_1 = predict_rf(model$model, x_t1)))
}

predict_uplift.tfeature <- predict_uplift_generic

param_space_tfeature <- rf_param_space


# Two-models approach

train_twomodels <- function(data, ...) {
    x_0 = data$x[data$t == "0",]
    y_0 = data$y[data$t == "0"]
    x_1 = data$x[data$t == "1",]
    y_1 = data$y[data$t == "1"]
    models <- list(
        model_0 = randomForest(x_0, y_0, ...),
        model_1 = randomForest(x_1, y_1, ...)
    )
    return(structure(models, class="twomodels"))
}

predict_cf.twomodels <- function(model, data) data.frame(
    S_0 = predict_rf(model$model_0, data$x),
    S_1 = predict_rf(model$model_1, data$x)
)

predict_uplift.twomodels <- predict_uplift_generic

param_space_twomodels <- rf_param_space


# Uplift-RF model

train_uplift_rf <- function(data, ...) {
    return(structure(list(
        model = upliftRF(
            x = data$x,
            y = as.numeric(as.character(data$y)),
            ct = as.numeric(as.character(data$t)),
            ...
        )),
        class="uplift_rf"
    ))
}

predict_cf.uplift_rf <- function(model, data) {
    res <- predict(model$model, data$x)
    print(names(res))
    return(data.frame(S_0 = res[,2], S_1 = res[,1]))
}

predict_uplift.uplift_rf <- predict_uplift_generic

param_space_uplift_rf <- list(
    "ntree" = list(
        "type" = "set",
        "values" = 100
    ),
    "split_method" = list(
        "type" = "set",
        "values" = "ED"
    )
)

all_model_names <- sapply(.S3methods(predict_uplift), function(method_name) {
    strsplit(method_name, "\\.")[[1]][2]
}, USE.NAMES = FALSE)

train_functions <- napply(all_model_names, function(model_name) get(paste0("train_", model_name)))

param_spaces <- napply(all_model_names, function(model_name) get(paste0("param_space_", model_name)))
