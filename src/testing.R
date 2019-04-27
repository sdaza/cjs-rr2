library(powerlmm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(parallel)
library(lavaan)
library(AER)
library(brms)

data_generator <- function(pp, confounder, label, beta_s, c_scale, hidden_confounder_sd = 0, measure_error_sd = 0, collider = FALSE) {
    function() {
        d <- simulate_data(pp) # powerlmm::simulate_data

        dpp <- d %>%
            filter(time %in% c(0, 10),
                   treatment == 0) %>%
            mutate(time = factor(time, labels = c("pre", "post")),
                   treatment = rep(0:1, each = pp$n2)) %>%
            select(-y_c) %>%
            spread(time, y) %>%
            mutate(hidden_confounder = rnorm(pp$n2, sd = hidden_confounder_sd),
                   yc = post + hidden_confounder)

        if(collider) {
            dpp <- add_collider(dpp, confounder = confounder, pp$n2, beta_s, c_scale)
        } else {
            dpp <- add_confounding(dpp, confounder = confounder, pp$n2, beta_s, c_scale)

        }
        dpp <- dpp %>%
            mutate(pre = intercept_subject + rnorm(pp$n2, sd = measure_error_sd),
                   label = label)
        dpp
    }
}


#' Extract results from lm and ivreg
#' @param x object
#' @param model character; name of model
get_res <- function(x, model) {
    est <-  coef(x)
    ind <- names(est) == "session"
    est <- est[ind]
    se <- sqrt(diag(vcov(x)))[ind]
    data.frame(model = model,
               para = names(est),
               est = est,
               se = se,
               lwr = est - 1.96 * se,
               upr = est + 1.96 * se)
}

#' brms: fit and extract results
#' @param dpp new pre-post data to fit
#' @param fit_brm brms-object to update
get_brm <- function(dpp, fit_brm) {
    fit <- suppressMessages(update(fit_brm, newdata = dpp, iter = 2000))
    est <- fixef(fit)
    est <- est[rownames(est) == "y_session", ]
    data.frame(model ="IV (brms)",
               para = "session",
               est = est[1],
               se = est[2],
               lwr = est[3],
               upr = est[4])

}
#' lavaan: creates function that fit and extract results
#' @param model character; lavaan model syntax
#' @return function; accepts new data via "dpp"
make_lavaan_func <- function(model) {
    function(dpp) {
        fit_lavaan <- tryCatch({sem(model, data=dpp)},
                               error = function(e) {NA},
                               warning = function(w) {NA})
        if(isS4(fit_lavaan)) {
            se <- sqrt(diag(vcov(fit_lavaan)))
            ind <- names(se) == "y~session"
            est <- coef(fit_lavaan)
            est <- est[ind]
            se <- se[ind]
        } else if (is.na(fit_lavaan)) {
            se <- NA
            est <- NA
        }
        data.frame(model = "IV (lavaan)",
                   para = "session",
                   est = est,
                   se = se,
                   lwr = est - 1.96 * se,
                   upr = est + 1.96 * se)
    }
}

#' add confounding to pre-post data
#' @param x prepost data
#' @param confounder charater; name of column in x to use as confounder
#' @param n2 number of total subjects
#' @beta_s dose-response parameter
#' @c_scale scale confounding distribution
add_confounding <- function(x, confounder, n2, beta_s, c_scale) {
    x$confounder <- x[, confounder]
    x <- x %>%
        mutate(s =  (7 - (confounder - mean(confounder))/sd(confounder) * c_scale) - hidden_confounder*0.5 + rnorm(n2, sd = 1),
               s = ifelse(s < 1, 1, s) * treatment,
               session = s + rnorm(n2, sd = 2),
               session = ifelse(session < 1, 1, session) * treatment,
               yt = yc - beta_s * s,
               y = yt * treatment + yc * (1-treatment),
               id = 1:n2)

    x
}
add_collider <- function(x, confounder, n2, beta_s, c_scale) {
    x$confounder <- x[, confounder]
    x <- x %>%
        mutate(s =  (7 - (confounder - mean(confounder))/sd(confounder) * c_scale) + rnorm(n2, sd = 1),
               s = ifelse(s < 1, 1, s) * treatment,
               session = s + rnorm(n2, sd = 0),
               session = ifelse(session < 1, 1, session) * treatment,
               cred = (session + hidden_confounder) * treatment,
               yt = yc - beta_s * s,
               y = yt * treatment + yc * (1-treatment),
               id = 1:n2)

    x
}



#' Create data generator
#'
#' @param pp powerlmm::study_parameters-object
#' @param confounder character; passed to 'add_confounding'
#' @param beta_s passed to 'add_confounding'
#' @param c_scale passed to 'add_confounding'
#'
#' @return function with no arguments that generate data.frame
data_generator <- function(pp, confounder, label, beta_s, c_scale, hidden_confounder_sd = 0, measure_error_sd = 0, collider = FALSE) {
    function() {
        d <- simulate_data(pp) # powerlmm::simulate_data

        dpp <- d %>%
            filter(time %in% c(0, 10),
                   treatment == 0) %>%
            mutate(time = factor(time, labels = c("pre", "post")),
                   treatment = rep(0:1, each = pp$n2)) %>%
            select(-y_c) %>%
            spread(time, y) %>%
            mutate(hidden_confounder = rnorm(pp$n2, sd = hidden_confounder_sd),
                   yc = post + hidden_confounder)

        if(collider) {
            dpp <- add_collider(dpp, confounder = confounder, pp$n2, beta_s, c_scale)
        } else {
            dpp <- add_confounding(dpp, confounder = confounder, pp$n2, beta_s, c_scale)

        }
        dpp <- dpp %>%
            mutate(pre = intercept_subject + rnorm(pp$n2, sd = measure_error_sd),
                   label = label)
        dpp
    }
}


#' Simulation function
#'
#' @param i i:th simulation
#' @param data_gen data_generator
#' @param fit_brm prefit brms-object

sim <- function(i, data_gen, fit_brm) {
    dpp <- data_gen()
    d_t <- dpp %>%
        filter(treatment == 1)

    OLS <- lm(y ~ session, data = dpp)
    OLS_pre <- lm(y ~ pre + session, data = dpp)
    OLS_tx <- lm(y ~ session, data = d_t)
    OLS_pre_tx <- lm(y ~ pre + session, data = d_t)
    IV_ivreg <- ivreg(y ~ session + pre | treatment + treatment:pre, data = dpp)
    IV_brm <- get_brm(dpp, fit_brm)
    IV_lavaan <- get_lavaan(dpp)
    res <- mapply(get_res,
                  list(OLS, OLS_pre, OLS_tx, OLS_pre_tx, IV_ivreg),
                  model = c("OLS s", "OLS s + pre", "OLS s (tx)", "OLS s + pre (tx)", "IV (ivreg)"),
                  SIMPLIFY = FALSE)
    res <- c(res, list(IV_brm), list(IV_lavaan))
    res <- do.call(rbind, res)
    res$sim <- i

    res
}
sim_collider <- function(i, data_gen, fit_brm) {
    dpp <- data_gen()
    d_t <- dpp %>%
        filter(treatment == 1)

    OLS <- lm(y ~ session, data = dpp)
    OLS_pre <- lm(y ~ pre + session, data = dpp)
    OLS_pre_cred <- lm(y ~ pre + session + cred, data = dpp)
    OLS_tx <- lm(y ~ session, data = d_t)
    OLS_pre_tx <- lm(y ~ pre + session, data = d_t)
    OLS_pre_tx_cred <- lm(y ~ pre + session + cred, data = d_t)
    IV_ivreg <- ivreg(y ~ session + pre | treatment + treatment:pre, data = dpp)
    IV_brm <- get_brm(dpp, fit_brm)
    IV_lavaan <- get_lavaan(dpp)
    res <- mapply(get_res,
                  list(OLS, OLS_pre, OLS_pre_cred, OLS_tx, OLS_pre_tx, OLS_pre_tx_cred, IV_ivreg),
                  model = c("OLS s", "OLS s + pre","OLS s + pre + collider",
                            "OLS s (tx)", "OLS s + pre (tx)", "OLS s + pre + collider (tx)",
                            "IV (ivreg)"),
                  SIMPLIFY = FALSE)
    res <- c(res, list(IV_brm), list(IV_lavaan))
    res <- do.call(rbind, res)
    res$sim <- i

    res
}
#' Summarise simulation results
#'
#' @param x Sim results from sim()
#' @param type character; label to use for type of confounding
#' @param theta true value of session dose-response coef
summarise_sim <- function(x, type, theta = -1) {
    x <- x %>% group_by(model) %>%
        summarise(est_M = mean(est, na.rm = TRUE),
                  est_SE = mean(se, na.rm = TRUE),
                  est_SD = sd(est, na.rm = TRUE),
                  cover = mean(lwr < theta & theta < upr, na.rm = TRUE)) %>%
        mutate(est_RB = ((est_M - theta)/theta) * 100,
               se_RB = (est_SE - est_SD)/est_SD)

    x$type <- type

    x
}

# Skeleton to base data on
sp <- study_parameters(n1 = 11,
                      n2 = 100,
                      T_end = 10,
                      fixed_intercept = 10,
                      fixed_slope = -.5,
                      icc_pre_subject = 0.8,
                      sigma_error = 3,
                      cohend = 0)

## Some fake data, code at the end of post
dpp <- data_generator(update(sp, n2 = 5000),
                      confounder = "intercept_subject",
                      label = "example",
                      beta_s = 1,
                      c_scale = 3,
                      hidden_confounder_sd = 0,
                      measure_error_sd = 0)()


  # testing analysis

head(dpp)

# with interaction?

f1 <- bf(session ~ treatment + treatment:pre)
f2 <- bf(y ~ session + pre)
IV_brm <- brm(f1 + f2, data = dpp, cores = 4)

summary(IV_brm)

# without interaction?

f1 <- bf(session ~ treatment)
f2 <- bf(y ~ session)
IV_brm <- brm(f1 + f2, data = dpp, cores = 4)

summary(IV_brm)
