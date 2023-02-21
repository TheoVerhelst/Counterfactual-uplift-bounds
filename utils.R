# Code for the project Machu-Picchu written by Théo Verhelst
# Supervisors at Orange: Denis Mercier, Jeevan Shrestha
# Academic supervision: Gianluca Bontempi (ULB MLG)

# Calibration of posterior probabilities as shown in
# Dal Pozzolo, Andrea, et al. "Calibrating probability
# with undersampling for unbalanced classification."
# 2015 IEEE Symposium Series on Computational Intelligence.
# IEEE, 2015.
calibrate_score <- function(score, p) {
    p_s <- mean(score)
    ratio <- p * (p_s - 1) / (p_s * (p - 1))
    return(ratio * score / (ratio * score - score + 1))
}

RMSE <- function(y, y_hat) sqrt(mean((y - y_hat)^2, na.rm=TRUE))

# Returns a data frame where a number of columns have been removed
remove_columns <- function(data, colums) data[, setdiff(names(data), colums), drop = FALSE]

# Returns a shuffled version of the data frame or vector
shuffle <- function(data) {
  if (is.data.frame(data))
     data[sample(1:nrow(data)),]
  else
    data[sample(1:length(data))]
}

subset_data <- function(data, mask) {
    return(napply(names(data), function(n) {
        if(is.data.frame(data[[n]])) {
            return(data[[n]][mask,])
        } else {
            return(data[[n]][mask])
        }
    }))
}

train_test_split <- function(N, split_ratio) sample(1:N)[1:(N * split_ratio)]

# Calls the function fn with both (possibly named) arguments in args and other arguments in ...
# This is useful when dealing programmatically with named arguments. Note that arguments
# from args are put after those in ... in the call to fn
#
# Example: call_expand_args(runif, c(min=3, max=4), n=50)
# is equivalent to runif(n=50, min=3, max=4)
call_expand_args <- function(fn, ..., args) {
  do.call(fn, append(list(...), args))
}

# An middle ground between sapply and lapply: use names (hence napply) but keep a list output.
# I need this all the time!
napply <- function(X, FUN, ...) sapply(X, FUN, ..., simplify = FALSE, USE.NAMES = TRUE)


# Assuming the input x if a binary factor (levels = c(0, 1)), this
# function returns the opposite of the factor.
factor_opposite <- function(x) as.factor(1 - as.numeric(as.character(x)))

all_same_length <- function(...) sd(sapply(list(...), length)) == 0

# Create N samples from a Dirichlet distribution with parameter vector alpha.
sample_dirichlet <- function(N, alpha) {
    stopifnot(all(!is.na(alpha) & alpha > 0))
    row_sum <- 0
    while (any(row_sum == 0)) {
        gamma_sample <- sapply(
            alpha, function(alpha_i) rgamma(N, shape = alpha_i, rate = 1)
        )
        # Avoid simplification to 1D array when N == 1
        if (N == 1) {
            gamma_sample <- t(as.matrix(gamma_sample))
        }
        row_sum <- rowSums(gamma_sample)
    }
    # The result has N rows and length(alpha) columns
    return(gamma_sample[1:N,] / row_sum)
}
