# R-code for reproducing the simulations and empirical results in ?The locally
# Gaussian partial correlation?

# Packages needed
library(lg)          # <-- The main package, see documentation for details.
library(mvtnorm)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyr)
library(readxl)
library(tseries)

# Load the necessary packages and set random seed for full reproducibility
SEED <- 1
set.seed(SEED)

# EXAMPLE: THE MULTIVARIATE GAUSSIAN DISTRIBUTION
# -----------------------------------------------

# We generate samples from three-variate Gaussian distributions with different
# sample sizes. We then calculate the partial correlations between X1 and X2
# given X3 and save the results.

# Estimation parameters
n_gaussian                           <- c(100, 500, 2000)       # The sample sizes
level                                <- .95                     # Conf. level for asymptotic conf. int.
grid_size                            <- 7                       # No. of grid points in each direction
bw_select                            <- "plugin"                # Bandwidth selection method
plugin_constant                      <- 4                       # Proportionality constant for plugin bandwidth selector
est_method                           <- "1par"                  # Estimation method
transform_to_marginal_normality      <- TRUE                    # Transform to marginal normality

# Create the grid
grid <- expand.grid(X1 = seq(-3.5, 3.5, length.out = grid_size),
                    X2 = seq(-3.5, 3.5, length.out = grid_size))

# This will be a list of plots that we print out later
gaussian_example <- list()

# One set of plots for each sample size
for(i in seq_along(n_gaussian)) {
    
    # Generate data
    x <- rmvnorm(n_gaussian[i], sigma = diag(3))
    
    # Estimate the local correlation
    lg_object <- lg_main(x, 
                         bw_method = bw_select,
                         plugin_constant_joint = plugin_constant,
                         est_method = "1par", 
                         transform_to_marginal_normality = TRUE)
    
    partial_correlations <- partial_cor(lg_object, grid = grid, condition = 0)
    
    cplot <- corplot(partial_correlations,
                     plot_obs = FALSE, 
                     alpha_point = 0.1,
                     plot_thres = 0.005,
                     alpha_tile = .001,
                     main = "",
                     xlab = "",
                     ylab = "",
                     label_size = 4)
    
    gaussian_example[[i]] <- cplot + theme_classic() + theme(legend.position = "none")
    ggsave(paste0("figures/gaussian-example-plots-", i, ".pdf"), width = 4, height = 3)
}

# EXAMPLE: THE STRUCTURAL EXAMPLE
# -------------------------------

n                                    <- 500                     # The sample sizes
level                                <- .95                     # Conf. level for asymptotic conf. int.
a                                    <- 1
grid_size_biv                        <- 100                     # No. of grid points in each direction for bivariate plots
grid_size_uni                        <- 400                     # No. of grid points for the univariate plot
bw_select                            <- "plugin"                # Bandwidth selection method
plugin_constant                      <- 4                       # Proportionality constant for choosing bandwidth
est_method                           <- "1par"                  # Estimation method
transform_to_marginal_normality      <- TRUE                    # Transform to marginal normality

# Create the bivariate grid
grid_biv <- expand.grid(X1 = seq(-4, 4, length.out = grid_size_biv),
                        X2 = seq(-5, 10, length.out = grid_size_biv))

# Create the univariate grid
x0 <- seq(from = -3.5, 3.5, length.out = grid_size_uni)
grid_uni <- cbind(x0, x0^2)

# Generate data
temp <- rmvnorm(n, sigma = diag(2))
x <- cbind(temp[,1], a*temp[,1]^2 + temp[,2], temp[,2])

# Estimate the local correlation
lg_object <- lg_main(x, 
                     bw_method = bw_select,
                     plugin_constant_joint = plugin_constant, 
                     est_method = "1par", 
                     transform_to_marginal_normality = TRUE)

# For the bivariate plot
partial_correlations_biv <- partial_cor(lg_object, grid = grid_biv, condition = 0, level = level)

# For the univariate plot
partial_correlations_uni <- partial_cor(lg_object, grid = grid_uni, condition = 0, level = level)

# We then generate the plots
cplot_biv <- corplot(partial_correlations_biv,
                     plot_obs = TRUE, 
                     alpha_point = 1,
                     point_size = 0.7,
                     plot_thres = 0.005,
                     main = "",
                     xlab = "",
                     ylab = "",
                     label_size = 4,
                     plot_labels = FALSE)

uni_grid <- data.frame(partial_correlations_uni$grid)
colnames(uni_grid) <- c("x", "y")

# Add the univariate grid to the bivariate plot.
cplot_biv <- cplot_biv + ggplot2::geom_line(ggplot2::aes(x = x, y = y), data = uni_grid, lty = 2) +
    theme_classic() + theme(legend.position = "none")

# The univariate plot
uni <- data_frame(x = partial_correlations_uni$grid[,1],
                  pc = partial_correlations_uni$partial_correlations,
                  lower = partial_correlations_uni$partial_correlations - 1.96*partial_correlations_uni$partial_correlations_sd,
                  upper = partial_correlations_uni$partial_correlations + 1.96*partial_correlations_uni$partial_correlations_sd)

uniplot <- uni %>% ggplot() + geom_line(mapping = aes(x  = x, y = pc), size = 1.3) +
    xlab("") + ylab("") +
    geom_ribbon(aes(x = x, ymin = lower, ymax = upper), alpha = 0.3) +
    labs(title = "") +
    theme_classic() + 
    theme(legend.position = "none")

# Calculate the ordinary Gaussian partial correlation in the structural example
partial_cor <- -cov2cor(solve(cor(x)))[1,2]

# EXAMPLE: we calculate the LGPC for the case with bivariate normals and stochastic correlation. 
# -----------------------------------------------------------------------------------

set.seed(SEED)

# Sample size
n_simex2 <- 1000

# This is the function that we use to generate data:
gen <- function(n) {
    
    out <- matrix(NA, nrow = n, ncol = 3)
    colnames(out) <- c("x1", "x2", "rho")
    
    for(i in 1:n) {
        rho <- runif(1, min = -.95, max = .95)
        x <- rmvnorm(1, mean = c(0,0), sigma = matrix(c(1, rho, rho, 1), nrow = 2))
        
        out[i, ] <- c(x, rho)
    }
    
    as_tibble(out)
}

# Generate the data
x <- gen(n_simex2)

c     <-  2       # Smoothing
thres <- .02      # Plotting threshold

low_val  <- -.9
mid_val  <- 0
high_val <- .9

plot_dim <- 5

# Make a plot of just the observations:
simex2_points <- x %>% 
    ggplot(aes(x = x1, y = x2)) +
    xlab("") + 
    ylab("") +
    geom_point() +
    theme_classic() 
ggsave("figures/simex2-plots-1.pdf", height = plot_dim, width = plot_dim)

# Create partial correlation maps for low, mid and high values of X3:
lg_object <- lg_main(x %>% select(x1, x2, rho), 
                     plugin_constant_joint = c, 
                     est_method = "trivariate")

# LOW
grid_low <- expand.grid(x1 = seq(-3.5, 3.5, length.out = 10),
                        x2 = seq(-3.5, 3.5, length.out = 10)) %>% 
    as_tibble %>% 
    mutate(dens = dmvnorm(cbind(.$x1, .$x2), 
                          sigma = matrix(c(1, low_val, low_val, 1), ncol = 2))) %>% 
    mutate(plot = dens > thres*max(dens)) %>% 
    filter(plot) %>% 
    select(x1, x2)

partial_correlations_low <- partial_cor(lg_object, 
                                        grid = grid_low, 
                                        condition = low_val, 
                                        level = NULL)

simex2_low <- corplot(partial_correlations_low,
                      label_size = 4,
                      main = "",
                      xlab = "",
                      ylab = "",
                      alpha_tile = 0.001) +
    geom_point(aes(x = x1, y = x2), 
               alpha = .1,
               data = x) +
    theme_classic() +
    theme(legend.position = "none")
ggsave("figures/simex2-plots-2.pdf", height = plot_dim, width = plot_dim)

# MID
grid_mid <- expand.grid(x1 = seq(-3.5, 3.5, length.out = 10),
                        x2 = seq(-3.5, 3.5, length.out = 10)) %>% 
    as_tibble %>% 
    mutate(dens = dmvnorm(cbind(.$x1, .$x2), 
                          sigma = matrix(c(1, mid_val, mid_val, 1), ncol = 2))) %>% 
    mutate(plot = dens > thres*max(dens)) %>% 
    filter(plot) %>% 
    select(x1, x2)

partial_correlations_mid <- partial_cor(lg_object, 
                                        grid = grid_mid, 
                                        condition = mid_val, 
                                        level = NULL)

simex2_mid <- corplot(partial_correlations_mid,
                      label_size = 4,
                      main = "",
                      xlab = "",
                      ylab = "",
                      alpha_tile = 0.001) +
    geom_point(aes(x = x1, y = x2), 
               alpha = .1,
               data = x) +
    theme_classic() +
    theme(legend.position = "none")
ggsave("figures/simex2-plots-3.pdf", height = plot_dim, width = plot_dim)

# HIGH
grid_high <- expand.grid(x1 = seq(-3.5, 3.5, length.out = 10),
                         x2 = seq(-3.5, 3.5, length.out = 10)) %>% 
    as_tibble %>% 
    mutate(dens = dmvnorm(cbind(.$x1, .$x2), 
                          sigma = matrix(c(1, high_val, high_val, 1), ncol = 2))) %>% 
    mutate(plot = dens > thres*max(dens)) %>% 
    filter(plot) %>% 
    select(x1, x2)

partial_correlations_high <- partial_cor(lg_object, 
                                         grid = grid_high, 
                                         condition = high_val, 
                                         level = NULL)

simex2_high <- corplot(partial_correlations_high,
                       label_size = 4,
                       main = "",
                       xlab = "",
                       ylab = "",
                       alpha_tile = 0.001) +
    geom_point(aes(x = x1, y = x2), 
               alpha = .1,
               data = x) +
    theme_classic() +
    theme(legend.position = "none")
ggsave("figures/simex2-plots-4.pdf", height = plot_dim, width = plot_dim)

# EXAMPLE: REAL DATA: -------------------
#
# We look at the S&P500 example from Cheng & Huang (2012). Make plots and run
# our CI-test on the same data.

RERUN_CI_TESTS_REAL <- FALSE     # Run the CI tests for the real data example in Section 3

# Parameters --------
c_plotting         <- 3.5       # Bandwidth proportionality constant for making plots
c_test             <- 1.75      # Bandwidth proportionality constant for CI test
grid_size_plotting <- 9         # Grid points in each direction for plotting
plot_thres         <- .00001    # Density threshold for plots
alpha_point        <- .05       # Transparency when plotting data points
label_size         <- 4.5       # Label size in plots
plot_obs           <- TRUE      # Plot the observations at all
extend             <- 1.2       # Extend xlim,ylim beyond tiles for plot
n_rep              <- 10        # The number of bootstrap samples for the CI test

# Plotting range
r_range <- c(-.06, .06)
v_range <- c(-1, 1)

# Load the S&P500 data, clean it up, and make log-returns. --------
sp500 <- read_csv("data/sp500.csv") %>% 
    mutate(ret = c(NA, diff(log(Close))),
           vol = c(NA, diff(log(Volume)))) %>% 
    mutate(ret_lagged = c(NA, ret[-nrow(.)]),
           vol_lagged = c(NA, vol[-nrow(.)])) %>% 
    select(Date, ret, vol, ret_lagged, vol_lagged)

# Calculate the LGPC in the case where we investigate whether returns Granger causes volume

# Create the data set
sp500_ret_to_vol <- sp500 %>%  filter(!is.na(vol_lagged)) %>% 
    select(vol, ret_lagged, vol_lagged)

# Create lg-object for plotting
ret_to_vol_lg_object <- lg_main(sp500_ret_to_vol,
                                plugin_constant_joint = c_plotting, 
                                est_method = "trivariate")

# Define a grid for plotting
grid <- expand.grid(x = seq(v_range[1], v_range[2], length.out = grid_size_plotting),
                    y = seq(r_range[1], r_range[2], length.out = grid_size_plotting))

# Estimate the LGPC
ret_to_vol_partial_cor <- partial_cor(ret_to_vol_lg_object, 
                                      grid = grid,
                                      condition = 0)

# Create the plot
ret_to_vol_plot <- corplot(ret_to_vol_partial_cor,
                           main = "",
                           xlab = "",
                           ylab = "",
                           xlim = v_range*extend,
                           ylim = r_range*extend,
                           plot_thres =plot_thres,
                           plot_obs = plot_obs,
                           alpha_point = alpha_point,
                           label_size = label_size) +
    theme_classic() +
    theme(legend.position = "none")

# Calculate the LGPC in the case where we investigate whether volume Granger causes returns

# The data set
sp500_vol_to_ret <- sp500 %>%  filter(!is.na(vol_lagged)) %>% 
    select(ret, vol_lagged, ret_lagged)

# Create lg-object for plotting
vol_to_ret_lg_object <- lg_main(sp500_vol_to_ret,
                                plugin_constant_joint = c_plotting, 
                                est_method = "trivariate")

# Define a grid for plotting
grid <- expand.grid(x = seq(r_range[1], r_range[2], length.out = grid_size_plotting),
                    y = seq(v_range[1], v_range[2], length.out = grid_size_plotting))

# Estimate the LGPC
vol_to_ret_partial_cor <- partial_cor(vol_to_ret_lg_object, 
                                      grid = grid,
                                      condition = 0)
# Create the plot
vol_to_ret_plot <- corplot(vol_to_ret_partial_cor,
                           main = "",
                           xlab = "",
                           ylab = "",
                           xlim = r_range*extend,
                           ylim = v_range*extend,
                           plot_thres = plot_thres,
                           plot_obs = plot_obs,
                           alpha_point = alpha_point,
                           label_size = label_size) +
    theme_classic() +
    theme(legend.position = "none")

# Create the LOW and HIGH plots as well
vol_to_ret_partial_cor_low <- partial_cor(vol_to_ret_lg_object, 
                                          grid = grid,
                                          condition = -0.05)

vol_to_ret_plot_low <- corplot(vol_to_ret_partial_cor_low,
                               main = "",
                               xlab = "",
                               ylab = "",
                               xlim = r_range*extend,
                               ylim = v_range*extend,
                               plot_thres = plot_thres,
                               plot_obs = plot_obs,
                               alpha_point = alpha_point,
                               label_size = label_size) +
    theme_classic() +
    theme(legend.position = "none")

vol_to_ret_partial_cor_high <- partial_cor(vol_to_ret_lg_object, 
                                           grid = grid,
                                           condition = 0.05)

vol_to_ret_plot_high <- corplot(vol_to_ret_partial_cor_high,
                                main = "",
                                xlab = "",
                                ylab = "",
                                xlim = r_range*extend,
                                ylim = v_range*extend,
                                plot_thres = plot_thres,
                                plot_obs = plot_obs,
                                alpha_point = alpha_point,
                                label_size = label_size) +
    theme_classic() +
    theme(legend.position = "none")

# Do the nonlinear Granger causality test for returns to volume

# Create a new lg object, because we use a different bandwidth proportionality
# constant when testing as opposed to plotting
ret_to_vol_lg_object_test <- lg_main(sp500_ret_to_vol,         
                                     plugin_constant_joint = c_test)

# Run the test
ret_to_vol <- ci_test(ret_to_vol_lg_object_test)

# Do the nonlinear Granger causality test for volum to returns

# Create a new lg object, because we use a different bandwidth proportionality
# constant when testing as opposed to plotting
vol_to_ret_lg_object_test <- lg_main(sp500_vol_to_ret,
                                     plugin_constant_joint = c_test)

# Run the test
vol_to_ret <- ci_test(vol_to_ret_lg_object_test, n_rep = n_rep)

# EXAMPLE: Granger causality from large to small firms
# -----------------------------------------------------

prices <- read_delim("data/prices1.csv", delim = ";") %>% 
    bind_rows(read_delim("data/prices2.csv", delim = ";")) %>% 
    bind_rows(read_delim("data/prices3.csv", delim = ";")) %>% 
    bind_rows(read_delim("data/prices4.csv", delim = ";")) %>% 
    select(-Last, -IsStock, -Market, -SharesIssued) %>% 
    rename(date = TradeDate, ticker = Symbol,
           name = SecurityName, return = LogReturnLast) %>% 
    mutate(date = dmy_hm(date)) %>% 
    mutate(return = replace_na(return, 0)) %>% 
    arrange(ticker, date)

zeros <- function(x) sum(x == 0)

counts <- prices %>%  
    group_by(ticker) %>% 
    summarise(n = n(),
              zeros = zeros(return))

# We then have some data on the market values of these companies as they were on
# 27 APR 2020. We will use this to split the companies into partitions based on
# that. The idea is then to determine Granger causality between small and large
# companies, as done in the paper "Is there a symmetric nonlinear causal
# relationship between large and small firms?"

# About half of the stocks have not been listed during the whole observation
# period or was not traded on many days, especially the small ones. We take out
# those, with a threshold of max_zeros.
n_partitions <- 5
n_trading_days <- max(counts$n)
max_zeros <- 150

all_companies <- 
    read_excel("data/oslo.xlsx") %>% 
    select(Ticker, Navn, `Markedsverdi (MNOK)`) %>% 
    rename(ticker = Ticker,
           name = Navn,
           market_value = `Markedsverdi (MNOK)`) %>% 
    mutate(market_value = gsub(",", ".", .$market_value)) %>% 
    mutate(market_value = as.numeric(gsub("[[:space:]]", "", .$market_value))) %>% 
    arrange(desc(market_value)) %>% 
    full_join(counts) %>% 
    mutate(include = (n == n_trading_days) & (zeros < max_zeros)) %>% 
    filter(include) %>% 
    mutate(partition =  (rep(1:n_partitions, (nrow(.) %/% n_partitions + 1)) %>% 
                             sort)[1:nrow(.)])

# First calculate the total market value of the partitions
value <- all_companies %>% 
    group_by(partition) %>% 
    summarise(total_value = sum(market_value, na.rm = TRUE))

# Make the portfolios
portfolios <- 
    all_companies %>% 
    select(ticker, partition, market_value) %>% 
    full_join(prices) %>% 
    filter(partition %in% c(1,5)) %>% 
    full_join(value) %>% 
    mutate(partition = as.character(partition)) %>% 
    mutate(partition = recode(.$partition,
                              "1" = "top",
                              "5" = "bottom")) %>% 
    mutate(weight = market_value/total_value) %>% 
    mutate(weighted_return = weight*exp(return)) %>% 
    group_by(date, partition) %>% 
    summarise(portfolio_return = log(sum(weighted_return, na.rm = TRUE)))

# Make the bootstrap replicates
set.seed(1)
B <- 500
block_length <- 8
smoothing = 3
conditions = c(-.02, 0, .02)

wide_data <-
    portfolios %>%
    ungroup %>%
    mutate(partition = as.character(partition)) %>%
    mutate(partition = recode(partition,
                              bottom = "lead_series",
                              top = "lag_series")) %>%
    pivot_wider(id_cols = date, names_from = partition, values_from = portfolio_return) %>%
    select(date, lead_series, lag_series) %>%
    drop_na()

indices <- 1:nrow(wide_data)
bootstrap_replicates <- tsbootstrap(indices, nb = B, b = block_length, type = "stationary")

make_order1_data <- function(orig_data, order) {
    orig_data[order, ] %>%
        ungroup %>%
        mutate(lead_l1 = lag(lead_series),
               lag_l1 = lag(lag_series)) %>%
        select(lag_series, lead_l1, lag_l1) %>%
        drop_na()
}

# Make the first plot with the first order. We want to make a plot containing
# both the trivariate and bivariate estimation methods, with bootstrapped
# confidence bands.

lgpc_order1 <- tibble(
    x = numeric(),
    estimate = numeric(),
    method = character(),
    condition = numeric(),
    sd = numeric(),
    lower = numeric(),
    upper = numeric()
)

for(j in conditions) {
    
    print(j)
    order1_biv <- make_order1_data(wide_data, 1:nrow(wide_data))
    lg_object <- lg_main(order1_biv, est_method = "1par", plugin_constant_joint = smoothing)
    grid <- cbind(seq(-.04, .04, length.out = 150),
                  seq(-.04, .04, length.out = 150))
    biv_lgpc <- partial_cor(lg_object, grid = grid, condition = j)
    bootstrapped_lgpc_biv <- matrix(NA, nrow = nrow(grid), ncol = B)
    
    for(i in 1:B) {
        order1 <- make_order1_data(wide_data, bootstrap_replicates[,i])
        lg_object <- lg_main(order1, est_method = "1par", plugin_constant_joint = smoothing)
        biv_lgpc_rep <- partial_cor(lg_object, grid = grid, condition = j)
        bootstrapped_lgpc_biv[,i] <- biv_lgpc_rep$partial_correlations
    }
    
    lgpc_order1 <- lgpc_order1 %>%
        bind_rows(
            tibble(x = grid[,1],
                   estimate = biv_lgpc$partial_correlations[,1],
                   method = "bivariate",
                   condition = j,
                   sd = apply(bootstrapped_lgpc_biv, 1, sd, na.rm = TRUE),
                   lower = apply(bootstrapped_lgpc_biv, 1, quantile, 0.025, na.rm = TRUE),
                   upper = apply(bootstrapped_lgpc_biv, 1, quantile, 0.975, na.rm = TRUE)))
    
}

lgpc_order1 %>%
    ggplot(aes(x = x)) +
    geom_line(aes(y = estimate, linetype = as.factor(condition))) +
    geom_ribbon(aes(min = lower, max = upper, group = as.factor(condition)),
                alpha = .1) +
    geom_hline(yintercept = 0, linetype = 3) +
    ylim(c(-1, 1)) +
    xlab("Diagonal") +
    ylab("LGPC") +
    labs(linetype = "Conditioning variable") +
    # ggtitle("LGPC along diagonal, First order Granger causality, big firms -> small firms") +
    theme_minimal()

ggsave("figures/granger-order1.pdf", width = 6, height = 3)

# Second order Granger causality -----

make_order2_data <- function(orig_data, order) {
    orig_data[order, ] %>%
        ungroup %>%
        mutate(lead_l1 = lag(lead_series),
               lead_l2 = lag(lead_series, n = 2),
               lag_l1 = lag(lag_series),
               lag_l2 = lag(lag_series, n = 2)) %>%
        select(lag_series, lead_l1, lead_l2, lag_l1, lag_l2) %>%
        drop_na()
}
# Make the second plot with the second order, with bootstrapped confidence
# bands.

lgpc_order2 <- tibble(
    x = numeric(),
    estimate = numeric(),
    method = character(),
    component = numeric(),
    condition = numeric(),
    sd = numeric(),
    lower = numeric(),
    upper = numeric()
)

for(j in conditions) {
    
    print(j)
    order2_biv <- make_order2_data(wide_data, 1:nrow(wide_data))
    lg_object <- lg_main(order2_biv, est_method = "1par", plugin_constant_joint = smoothing)
    grid <- cbind(seq(-.03, .03, length.out = 150),
                  seq(-.03, .03, length.out = 150),
                  seq(-.03, .03, length.out = 150))
    biv_lgpc <- partial_cor(lg_object, grid = grid, condition = rep(j,2))
    bootstrapped_lgpc_biv1 <- matrix(NA, nrow = nrow(grid), ncol = B)
    bootstrapped_lgpc_biv2 <- matrix(NA, nrow = nrow(grid), ncol = B)
    bootstrapped_lgpc_biv3 <- matrix(NA, nrow = nrow(grid), ncol = B)
    for(i in 1:B) {
        order2 <- make_order2_data(wide_data, bootstrap_replicates[,i])
        lg_object <- lg_main(order2, est_method = "1par", plugin_constant_joint = smoothing)
        biv_lgpc_rep <- partial_cor(lg_object, grid = grid, condition = rep(j,2))
        bootstrapped_lgpc_biv1[,i] <- biv_lgpc_rep$partial_correlations[,1]
        bootstrapped_lgpc_biv2[,i] <- biv_lgpc_rep$partial_correlations[,2]
        bootstrapped_lgpc_biv3[,i] <- biv_lgpc_rep$partial_correlations[,3]
    }
    
    lgpc_order2 <- lgpc_order2 %>%
        bind_rows(
            tibble(x = grid[,1],
                   estimate = biv_lgpc$partial_correlations[,1],
                   method = "bivariate",
                   component = 1,
                   condition = j,
                   sd = apply(bootstrapped_lgpc_biv1, 1, sd, na.rm = TRUE),
                   lower = apply(bootstrapped_lgpc_biv1, 1, quantile, 0.025, na.rm = TRUE),
                   upper = apply(bootstrapped_lgpc_biv1, 1, quantile, 0.975, na.rm = TRUE))) %>%
        bind_rows(
            tibble(x = grid[,1],
                   estimate = biv_lgpc$partial_correlations[,2],
                   method = "bivariate",
                   component = 2,
                   condition = j,
                   sd = apply(bootstrapped_lgpc_biv2, 1, sd, na.rm = TRUE),
                   lower = apply(bootstrapped_lgpc_biv2, 1, quantile, 0.025, na.rm = TRUE),
                   upper = apply(bootstrapped_lgpc_biv2, 1, quantile, 0.975, na.rm = TRUE))) %>%
        bind_rows(
            tibble(x = grid[,1],
                   estimate = biv_lgpc$partial_correlations[,3],
                   method = "bivariate",
                   component = 3,
                   condition = j,
                   sd = apply(bootstrapped_lgpc_biv3, 1, sd, na.rm = TRUE),
                   lower = apply(bootstrapped_lgpc_biv3, 1, quantile, 0.025, na.rm = TRUE),
                   upper = apply(bootstrapped_lgpc_biv3, 1, quantile, 0.975, na.rm = TRUE)))
}

lgpc_order2 %>%
    mutate(component = as.character(component)) %>% 
    mutate(component = recode(.$component, 
                              "1" = "(1,2)",
                              "2" = "(1,3)",
                              "3" = "(2,3)")) %>% 
    ggplot(aes(x = x)) +
    geom_line(aes(y = estimate, linetype = as.factor(condition))) +
    facet_wrap( ~ component) +
    geom_ribbon(aes(min = lower, max = upper, group = as.factor(condition)),
                alpha = .1) +
    geom_hline(yintercept = 0, linetype = 3) +
    ylim(c(-1, 1)) +
    xlab("Diagonal") +
    ylab("LGPC") +
    labs(linetype = "Conditioning variable") +
    theme_minimal() +
    theme(legend.position = "none")

ggsave("figures/granger-order2.pdf", width = 6, height = 3)
