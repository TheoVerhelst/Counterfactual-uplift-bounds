# Code for the project Machu-Picchu written by Théo Verhelst
# Supervisors at Orange: Denis Mercier, Jeevan Shrestha
# Academic supervision: Gianluca Bontempi (ULB MLG)

library(parallel)
source("utils.R")

train_easy_ensemble <- function(data, train_function, params, nb_parts, n_cores, verbose = FALSE) {
    if (!(is.factor(data$y) && is.factor(data$t)))
        stop("The variables y and t must be factors")

    y_values <- table(data$y)

    if (length(y_values) != 2)
        stop("The variables y must have two distinct values: '0' or '1'.")

    if (!all(sort(names(y_values)) == c("0", "1")))
        stop("The variables y must have only distinct two values: '0' or '1'.")

    if (y_values["1"] > y_values["0"])
        stop("The positive class '1' must be the minority class: '1': ",
                 y_values["1"], " obs. and '0': ", y_values["0"], " obs.")

    if (y_values["1"] * nb_parts > y_values["0"])
        stop("Wrong ratio of positive / negative observations",
                 y_values["1"], " pos. obs. * ", nb_parts, "parts > ", y_values["0"], " neg. obs.,",
                 "reduce nb_parts or add negative observations")

    if (any((sapply(data$x, class) == "character")))
        stop("Some of the input variables are 'character'. Please convert them to 'factor'.")


    idx_pos <- which(data$y == "1")
    idx_neg <- which(data$y == "0")
    idx_neg <- shuffle(idx_neg)
    # The number of elements if the majority class was evenly divided in nb_parts parts
    # This is actually greater than the subsample we will take
    idx_neg_split_size <- floor(length(idx_neg) / nb_parts)
    stopifnot(idx_neg_split_size > length(idx_pos))
    # create list of vectors containing the subsamples we will take from the negative class
    idx_neg_split <- lapply(0:(nb_parts - 1), function(part_i)
        idx_neg[part_i * idx_neg_split_size + (1:length(idx_pos))]
    )

    # Add to each training sets the positive observations (minority class)
    idx_all_split <- lapply(idx_neg_split, function(idx_neg_part) shuffle(c(idx_neg_part, idx_pos)))

    if (verbose) {
        message("easy_ensemble -- number of observations from the original training set:")
        message("    Total: ", length(data$y))
        message("    Positive (minority) class: ", length(idx_pos))
        message("    Negative (majority) class: ", length(idx_neg))

        message("easy_ensemble -- number of observations after splitting:")
        message("    Total: ")
        message("        Avg: ", mean(sapply(idx_all_split, length)))
        message("    Positive (minority) class: ")
        message("        Avg: ", mean(sapply(idx_all_split, function(split) sum(data$y[split] == "1"))))
        message("    Negative (majority) class: ")
        message("        Avg: ", mean(sapply(idx_all_split, function(split) sum(data$y[split] == "0"))))

    }
    
    base_fn <- function(split) {
        if (verbose)
            message(as.character(Sys.time()), ": easy_ensemble split")
        call_expand_args(
            train_function,
            args = params,
            list(
                x = data$x[split,],
                y = data$y[split],
                t = data$t[split],
                r = data$r[split]
            )
        )
    }

    if (!is.null(n_cores)) {
        cl <- makeCluster(n_cores, type="FORK")
        res <- parLapply(cl, idx_all_split, base_fn)
        stopCluster(cl)
        return(res)
    } else {
        return(lapply(idx_all_split, base_fn))
    }
}


predict_easy_ensemble_uplift <- function(models, data, n_cores) {
    if (!is.null(n_cores)) {
        cl <- makeCluster(n_cores, type="FORK")
        pred <- parLapply(cl, models, function(model) predict_uplift(model, data))
        stopCluster(cl)
    } else {
        pred <- lapply(models, function(model) predict_uplift(model, data))
    }
    # List of vectors to data frame
    pred <- as.data.frame(do.call(cbind, pred))
    return(rowMeans(pred))
}

predict_easy_ensemble_cf <- function(models, data, n_cores) {
    if (!is.null(n_cores)) {
        cl <- makeCluster(n_cores, type="FORK")
        pred <- parLapply(cl, models, function(model) predict_cf(model, data))
        stopCluster(cl)
    } else {
        pred <- lapply(models, function(model) predict_cf(model, data))
    }

    return(data.frame(
        risk_t0 = rowMeans(as.data.frame(do.call(cbind, lapply(pred, function(p) p$risk_t0)))),
        risk_t1 = rowMeans(as.data.frame(do.call(cbind, lapply(pred, function(p) p$risk_t1))))
    ))
}
