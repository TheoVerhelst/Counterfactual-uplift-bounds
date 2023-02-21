# Code for the project Machu-Picchu written by Théo Verhelst
# Supervisors at Orange: Denis Mercier, Jeevan Shrestha
# Academic supervision: Gianluca Bontempi (ULB MLG)

frechet_bounds <- function(S_0, S_1) {
    return(list(
        alpha = list(min = max(0, 1 - S_0 - S_1), max = min(1 - S_0, 1 - S_1)),
        beta  = list(min = max(0, S_0 - S_1),     max = min(S_0, 1 - S_1)),
        gamma = list(min = max(0, S_1 - S_0),     max = min(1 - S_0, S_1)),
        delta = list(min = max(0, S_0 + S_1 - 1), max = min(S_0, S_1)),
        phi   = list(min = max(0, S_0 + S_1 - 1) - S_0 * S_1, max = min(S_0, S_1) - S_0 * S_1)
    ))
}


estimate_from_empirical <- function(data, use_churn_convention = FALSE) {
    S_0 <- mean(data$y[data$t == "0"] == "1")
    S_1 <- mean(data$y[data$t == "1"] == "1")
    U <- ifelse(use_churn_convention, S_0 - S_1, S_1 - S_0)
    return(list(
        alpha_ind      = mean((1 - data$S_0) * (1 - data$S_1)),
        alpha_pop      = (1 - S_0) * (1 - S_1),
        alpha_lb_s_ind = mean(pmax(0,  1 - data$S_0 - data$S_1)),
        alpha_ub_s_ind = mean(pmin(1 - data$S_0, 1 - data$S_1)),
        alpha_lb_s_pop = max(0, 1 - S_0 - S_1),
        alpha_ub_s_pop = min(1 - S_0, 1 - S_1),

        beta_ind       = mean(data$S_0 * (1 - data$S_1)),
        beta_pop       = S_0 * (1 - S_1),
        beta_lb_s_ind  = mean(pmax(0, data$S_0 - data$S_1)),
        beta_ub_s_ind  = mean(pmin(data$S_0, 1 - data$S_1)),
        beta_lb_s_pop  = max(0, S_0 - S_1),
        beta_ub_s_pop  = min(S_0, 1 - S_1),


        gamma_ind      = mean((1 - data$S_0) * data$S_1),
        gamma_pop      = (1 - S_0) * S_1,
        gamma_lb_s_ind = mean(pmax(0, data$S_1 - data$S_0)),
        gamma_ub_s_ind = mean(pmin(1 - data$S_0, data$S_1)),
        gamma_lb_s_pop = max(0, S_1 - S_0),
        gamma_ub_s_pop = min(1 - S_0, S_1),


        delta_ind      = mean(data$S_0 * data$S_1),
        delta_pop      = S_0 * S_1,
        delta_lb_s_ind = mean(pmax(0, data$S_0 + data$S_1 - 1)),
        delta_ub_s_ind = mean(pmin(data$S_0, data$S_1)),
        delta_lb_s_pop = max(0, S_0 + S_1 - 1),
        delta_ub_s_pop = min(S_0, S_1),

        S_0 = S_0,
        S_1 = S_1,
        uplift = U
    ))
}

entropy_dirichlet <- function(a) {
    A <- sum(a)
    return(digamma(A + 1) - sum(a * digamma(a + 1)) / A)
}

estimate_from_simulation <- function(data) {
    U <- mean(data$uplift)
    S_0 <- mean(data$S_0)
    S_1 <- mean(data$S_1)
    U_hat <- mean(data$uplift_hat)
    S_0_hat <- mean(data$S_0_hat)
    S_1_hat <- mean(data$S_1_hat)
    phi <- mean(data$phi)
    H_Y0_Y1_X <- -mean(
        data$alpha * log(data$alpha) +
        data$beta  * log(data$beta) +
        data$gamma * log(data$gamma) +
        data$delta * log(data$delta)
    )
    return(list(
        alpha          = mean(data$alpha),
        alpha_ind      = mean((1 - data$S_0_hat) * (1 - data$S_1_hat)),
        alpha_pop      = (1 - S_0_hat) * (1 - S_1_hat),
        alpha_lb_s_ind = mean(pmax(0, 1 - data$S_0_hat - data$S_1_hat)),
        alpha_ub_s_ind = mean(pmin(1 - data$S_0_hat, 1 - data$S_1_hat)),
        alpha_lb_s_pop = max(0, 1 - S_0_hat - S_1_hat),
        alpha_ub_s_pop = min(1 - S_0_hat, 1 - S_1_hat),

        beta          = mean(data$beta),
        beta_ind      = mean(data$S_0_hat * (1 - data$S_1_hat)),
        beta_pop      = S_0_hat * (1 - S_1_hat),
        beta_lb_s_ind = mean(pmax(0, data$S_0_hat - data$S_1_hat)),
        beta_ub_s_ind = mean(pmin(data$S_0_hat, 1 - data$S_1_hat)),
        beta_lb_s_pop = max(0, S_0_hat - S_1_hat),
        beta_ub_s_pop = min(S_0_hat, 1 - S_1_hat),

        gamma          = mean(data$gamma),
        gamma_ind      = mean((1 - data$S_0_hat) * data$S_1_hat),
        gamma_pop      = (1 - S_0_hat) * S_1_hat,
        gamma_lb_s_ind = mean(pmax(0, data$S_1_hat - data$S_0_hat)),
        gamma_ub_s_ind = mean(pmin(1 - data$S_0_hat, data$S_1_hat)),
        gamma_lb_s_pop = max(0, S_1_hat - S_0_hat),
        gamma_ub_s_pop = min(1 - S_0_hat, S_1_hat),

        delta          = mean(data$delta),
        delta_ind      = mean(data$S_0_hat * data$S_1_hat),
        delta_pop      = S_0_hat * S_1_hat,
        delta_lb_s_ind = mean(pmax(0, data$S_0_hat + data$S_1_hat - 1)),
        delta_ub_s_ind = mean(pmin(data$S_0_hat, data$S_1_hat)),
        delta_lb_s_pop = max(0, S_0_hat + S_1_hat - 1),
        delta_ub_s_pop = min(S_0_hat, S_1_hat),

        uplift = U,
        S_0 = S_0,
        S_1 = S_1,
        S_0_hat = S_0_hat,
        S_1_hat = S_1_hat,
        phi = phi,
        H_Y0_Y1_X = H_Y0_Y1_X
    ))
}